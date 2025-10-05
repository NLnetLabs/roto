//! Machine code generation via cranelift
//!
//! This module takes Roto IR and translates that to cranelift IR. Cranelift
//! then does the rest.

use std::{
    any::{type_name, Any, TypeId},
    collections::HashMap,
    marker::PhantomData,
    mem::ManuallyDrop,
    sync::Arc,
};

use crate::{
    ast::Identifier,
    ice,
    label::{LabelRef, LabelStore},
    lir::{
        self, value::IrType, FloatCmp, IntCmp, IrValue, Operand, Var, VarKind,
    },
    runtime::{
        context::ContextDescription, ty::Reflect, ConstantValue,
        RuntimeConstant, RuntimeFunctionRef,
    },
    typechecker::{
        info::TypeInfo,
        scope::{ResolvedName, ScopeRef},
        types,
    },
    Runtime,
};
use check::{
    check_roto_type_reflect, FunctionRetrievalError, RotoFunc, TypeMismatch,
};
use cranelift::{
    codegen::{
        entity::EntityRef,
        ir::{
            condcodes::IntCC, types::*, AbiParam, Block, InstBuilder,
            MemFlags, StackSlotData, StackSlotKind, Value,
        },
        isa::TargetIsa,
        settings::{self, Configurable as _},
    },
    frontend::{
        FuncInstBuilder, FunctionBuilder, FunctionBuilderContext, Switch,
        Variable,
    },
    jit::{JITBuilder, JITModule},
    module::{DataDescription, FuncId, Linkage, Module as _},
    prelude::{FloatCC, Signature},
};
use cranelift_codegen::ir::{SigRef, StackSlot};
use log::info;

pub mod check;
pub mod testing;
#[cfg(test)]
mod tests;

struct ModuleData {
    cranelift_jit: ManuallyDrop<JITModule>,

    /// The functions in this module can reference constants. The values of
    /// these constants are stored in this HashMap. So, as long as the function
    /// are around, we have to keep these constants around. That is why they
    /// need to be stored in this struct, even though this field is unused.
    _constants: HashMap<ResolvedName, ConstantValue>,

    /// The functions in this module can reference registered function that
    /// might contain data (i.e. closures). We need to properly drop these.
    _registered_fns: Vec<Arc<Box<dyn Any>>>,
}

impl ModuleData {
    fn new(
        cranelift_jit: JITModule,
        constants: HashMap<ResolvedName, ConstantValue>,
        registered_fns: Vec<Arc<Box<dyn Any>>>,
    ) -> Self {
        Self {
            cranelift_jit: ManuallyDrop::new(cranelift_jit),
            _constants: constants,
            _registered_fns: registered_fns,
        }
    }
}

impl Drop for ModuleData {
    fn drop(&mut self) {
        // SAFETY: We only give out functions that hold a SharedModuleData and
        // therefore an Arc to this module. This ensures that this drop method
        // is only called after all functions have been dropped. Therefore,
        // freeing this memory is ok.
        unsafe {
            let cranelift_jit = ManuallyDrop::take(&mut self.cranelift_jit);
            cranelift_jit.free_memory();
        }
    }
}

/// A wrapper around a cranelift [`JITModule`] that cleans up after itself
///
/// This is achieved by wrapping the module in an [`Arc`].
#[derive(Clone)]
pub struct SharedModuleData(Arc<ModuleData>);

impl SharedModuleData {
    fn new(
        cranelift_jit: JITModule,
        constants: HashMap<ResolvedName, ConstantValue>,
        registered_fns: Vec<Arc<Box<dyn Any>>>,
    ) -> Self {
        Self(Arc::new(ModuleData::new(
            cranelift_jit,
            constants,
            registered_fns,
        )))
    }
}

// Just a simple debug to print _something_.
impl std::fmt::Debug for SharedModuleData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("SharedModuleData").finish()
    }
}

unsafe impl Send for ModuleData {}
unsafe impl Sync for ModuleData {}

/// A compiled, ready-to-run Roto module
pub struct Module {
    /// The set of public functions and their signatures.
    functions: HashMap<String, FunctionInfo>,

    context_description: ContextDescription,

    /// The inner cranelift module
    inner: SharedModuleData,

    /// Info from the typechecker for checking types against Rust types
    type_info: TypeInfo,
}

/// A function extracted from Roto
///
/// A [`TypedFunc`] can be retrieved from a compiled script using
/// [`Package::get_function`](crate::Package::get_function).
///
/// The function can be called with one of the [`TypedFunc::call`] functions.
#[derive(Clone, Debug)]
pub struct TypedFunc<Ctx, F> {
    func: *const u8,
    return_by_ref: bool,

    // The module holds the data for this function, that's why we need
    // to ensure that it doesn't get dropped. This field is ESSENTIAL
    // for the safety of calling this function. Without it, the data that
    // the `func` pointer points to might have been dropped.
    _module: SharedModuleData,
    _ty: PhantomData<(Ctx, F)>,
}

/// SAFETY: These implementations are safe because we don't modify anything
/// the pointer points to and the pointer will stay valid as long as we hold
/// on to the `ModuleData`, which is stored in the `TypedFunc`.
unsafe impl<Ctx, F> Send for TypedFunc<Ctx, F> {}
unsafe impl<Ctx, F> Sync for TypedFunc<Ctx, F> {}

impl<Ctx: 'static, F: RotoFunc> TypedFunc<Ctx, F> {
    pub fn call_tuple(&self, ctx: &mut Ctx, args: F::Args) -> F::Return {
        unsafe { F::invoke::<Ctx>(ctx, args, self.func, self.return_by_ref) }
    }
}

macro_rules! call_impl {
    ($($ty:ident),*) => {
        impl<Ctx: 'static, $($ty,)* Return> TypedFunc<Ctx, fn($($ty,)*) -> Return>
        where
            $($ty: Reflect,)*
            Return: Reflect
        {
            #[allow(non_snake_case)]
            #[allow(clippy::too_many_arguments)]
            pub fn call(&self, ctx: &mut Ctx, $($ty: $ty,)*) -> Return {
                self.call_tuple(ctx, ($($ty,)*))
            }

            #[allow(non_snake_case)]
            pub fn into_func(self) -> impl Fn(&mut Ctx, $($ty,)*) -> Return {
                move |ctx, $($ty,)*| self.call(ctx, $($ty,)*)
            }
        }
    }
}

call_impl!();
call_impl!(A);
call_impl!(A, B);
call_impl!(A, B, C);
call_impl!(A, B, C, D);
call_impl!(A, B, C, D, E);
call_impl!(A, B, C, D, E, F);
call_impl!(A, B, C, D, E, F, G);

pub struct FunctionInfo {
    id: FuncId,
    signature: types::Signature,
    return_by_ref: bool,
}

struct ModuleBuilder {
    constants: HashMap<ResolvedName, ConstantValue>,

    registered_fns: Vec<Arc<Box<dyn Any>>>,

    /// The set of public functions and their signatures.
    functions: HashMap<String, FunctionInfo>,

    /// External functions
    runtime_functions: HashMap<RuntimeFunctionRef, (*const u8, FuncId)>,

    /// The inner cranelift module
    inner: JITModule,

    /// Map of cranelift variables and their types
    ///
    /// This is necessary because cranelift does not seem to allow us to
    /// query it.
    variable_map: HashMap<Var, (Variable, Type)>,

    /// Instruction set architecture
    isa: Arc<dyn TargetIsa>,

    /// To print labels for debugging.
    #[allow(unused)]
    label_store: LabelStore,

    /// The information generated by the type checker
    type_info: TypeInfo,

    /// Signature to use for calls to `clone`
    clone_signature: Signature,

    /// Signature to use for calls to `drop`
    drop_signature: Signature,

    /// Signature to use for calls to `init_string`
    init_string_signature: Signature,

    context_description: ContextDescription,
}

struct FuncGen<'c> {
    module: &'c mut ModuleBuilder,

    /// The cranelift function builder
    builder: FunctionBuilder<'c>,

    /// Scope of the function
    scope: ScopeRef,

    /// Blocks of the function
    block_map: HashMap<LabelRef, Block>,

    /// Signature to use for calls to `clone`
    clone_signature: SigRef,

    /// Signature to use for calls to `drop`
    drop_signature: SigRef,

    /// Signature to use for calls to `init_string`
    init_string_signature: SigRef,
}

// We use `with_aligned` to make sure that we notice if anything is
// unaligned. It does add additional checks, so should be disabled at some
// point, or at least be configurable.
const MEMFLAGS: MemFlags = MemFlags::new().with_aligned();

pub fn codegen(
    runtime: &Runtime,
    ir: &[lir::Function],
    runtime_functions: &HashMap<RuntimeFunctionRef, lir::Signature>,
    label_store: LabelStore,
    type_info: TypeInfo,
    context_description: ContextDescription,
) -> Module {
    // The ISA is the Instruction Set Architecture. We always compile for
    // the system we run on, so we use `cranelift_native` to get the ISA
    // for the current system. We enable building for speed only. Size is
    // not super important at the moment.
    let mut settings = settings::builder();
    settings.set("opt_level", "speed").unwrap();
    let flags = settings::Flags::new(settings);
    let isa = cranelift::native::builder().unwrap().finish(flags).unwrap();

    let mut builder = JITBuilder::with_isa(
        isa.to_owned(),
        cranelift::module::default_libcall_names(),
    );

    for func_ref in runtime_functions.keys() {
        let f = runtime.get_function(*func_ref);
        builder.symbol(
            format!("runtime_function_trampoline_{}", f.id),
            f.func.trampoline(),
        );
    }

    let jit = JITModule::new(builder);

    let mut drop_signature = jit.make_signature();
    drop_signature
        .params
        .push(AbiParam::new(isa.pointer_type()));

    let mut clone_signature = jit.make_signature();
    clone_signature
        .params
        .push(AbiParam::new(isa.pointer_type()));
    clone_signature
        .params
        .push(AbiParam::new(isa.pointer_type()));

    let mut init_string_signature = jit.make_signature();
    init_string_signature
        .params
        .push(AbiParam::new(isa.pointer_type()));
    init_string_signature
        .params
        .push(AbiParam::new(isa.pointer_type()));
    init_string_signature
        .params
        .push(AbiParam::new(cranelift::codegen::ir::types::I32));

    let mut module = ModuleBuilder {
        constants: HashMap::new(),
        functions: HashMap::new(),
        registered_fns: Vec::new(),
        runtime_functions: HashMap::new(),
        inner: jit,
        isa,
        variable_map: HashMap::new(),
        label_store,
        type_info,
        drop_signature,
        clone_signature,
        init_string_signature,
        context_description,
    };

    for constant in runtime.constants().values() {
        module.declare_constant(constant);
    }

    for (func_ref, ir_sig) in runtime_functions {
        let mut sig = module.inner.make_signature();

        // This function is the trampoline, so we need to pass the pointer
        // to the actual function.
        sig.params.push(AbiParam::new(module.isa.pointer_type()));

        for (_, ty) in &ir_sig.parameters {
            sig.params.push(AbiParam::new(module.cranelift_type(ty)));
        }
        if let Some(ty) = &ir_sig.return_type {
            sig.returns.push(AbiParam::new(module.cranelift_type(ty)));
        }
        let f = runtime.get_function(*func_ref);
        let Ok(func_id) = module.inner.declare_function(
            &format!("runtime_function_trampoline_{}", f.id),
            Linkage::Import,
            &sig,
        ) else {
            panic!()
        };

        let arc_box = f.func.pointer();
        let ptr = &raw const **arc_box as *const u8;
        module.registered_fns.push(arc_box);
        module.runtime_functions.insert(*func_ref, (ptr, func_id));
    }

    // Our functions might call each other, so we declare them before we
    // define them. This is also when we start building the function
    // hashmap.
    for func in ir {
        module.declare_function(func);
    }

    let mut builder_context = FunctionBuilderContext::new();
    for func in ir {
        module.define_function(func, &mut builder_context);
    }

    module.finalize()
}

impl ModuleBuilder {
    fn declare_constant(&mut self, constant: &RuntimeConstant) {
        // Every constant needs to live as long as the functions and therefore
        // module that references them. However, the runtime might be dropped
        // before we call a Roto function. Therefore we clone the constants into
        // this hashmap which we pass to the ModuleData so that they will be
        // kept around.
        self.constants.insert(constant.name, constant.value.clone());
    }

    /// Declare a function and its signature (without the body)
    fn declare_function(&mut self, func: &lir::Function) {
        let lir::Function {
            name,
            ir_signature,
            signature,
            public,
            ..
        } = func;

        let mut sig = self.inner.make_signature();

        if ir_signature.return_ptr {
            sig.params
                .push(AbiParam::new(self.cranelift_type(&IrType::Pointer)));
        }

        // This is the parameter for the context
        sig.params
            .push(AbiParam::new(self.cranelift_type(&IrType::Pointer)));

        for (_, ty) in &ir_signature.parameters {
            sig.params.push(AbiParam::new(self.cranelift_type(ty)));
        }

        sig.returns = match &ir_signature.return_type {
            Some(ty) => vec![AbiParam::new(self.cranelift_type(ty))],
            None => Vec::new(),
        };

        let func_id = self
            .inner
            .declare_function(
                name.as_str(),
                if *public {
                    Linkage::Export
                } else {
                    Linkage::Local
                },
                &sig,
            )
            .unwrap();

        self.functions.insert(
            name.to_string(),
            FunctionInfo {
                id: func_id,
                return_by_ref: ir_signature.return_ptr,
                signature: signature.clone(),
            },
        );
    }

    /// Define a function body
    ///
    /// The function must be declared first.
    fn define_function(
        &mut self,
        func: &lir::Function,
        builder_context: &mut FunctionBuilderContext,
    ) {
        let lir::Function {
            name,
            blocks,
            ir_signature,
            scope,
            variables,
            ..
        } = func;
        let func_id = self.functions[name.as_str()].id;

        let mut ctx = self.inner.make_context();
        let mut sig = self.inner.make_signature();

        if ir_signature.return_ptr {
            sig.params
                .push(AbiParam::new(self.cranelift_type(&IrType::Pointer)));
        }

        // This is the context
        sig.params
            .push(AbiParam::new(self.cranelift_type(&IrType::Pointer)));

        for (_, ty) in &ir_signature.parameters {
            sig.params.push(AbiParam::new(self.cranelift_type(ty)));
        }

        if let Some(ty) = &ir_signature.return_type {
            sig.returns.push(AbiParam::new(self.cranelift_type(ty)));
        }

        ctx.func.signature = sig;
        ctx.set_disasm(true);

        let mut builder =
            FunctionBuilder::new(&mut ctx.func, builder_context);

        let mut stack_slots = Vec::new();
        for (v, t) in variables {
            let idx = self.variable_map.len();
            let var = Variable::new(idx);

            let ir_ty = match t {
                lir::ValueOrSlot::Val(ir_ty) => *ir_ty,
                lir::ValueOrSlot::StackSlot(layout) => {
                    let slot =
                        builder.create_sized_stack_slot(StackSlotData::new(
                            StackSlotKind::ExplicitSlot,
                            layout.size() as u32,
                            layout.align_shift() as u8,
                        ));

                    stack_slots.push((v.clone(), slot));
                    IrType::Pointer
                }
            };

            let ty = self.cranelift_type(&ir_ty);
            self.variable_map.insert(v.clone(), (var, ty));
            builder.declare_var(var, ty);
        }

        let mut func_gen = FuncGen {
            drop_signature: builder
                .import_signature(self.drop_signature.clone()),
            clone_signature: builder
                .import_signature(self.clone_signature.clone()),
            init_string_signature: builder
                .import_signature(self.init_string_signature.clone()),
            module: self,
            builder,
            scope: *scope,
            block_map: HashMap::new(),
        };

        func_gen.entry_block(
            &blocks[0],
            &ir_signature.parameters,
            stack_slots,
            ir_signature.return_ptr,
        );

        for block in &blocks[1..] {
            func_gen.block(block);
        }

        func_gen.finalize();

        self.inner.define_function(func_id, &mut ctx).unwrap();

        let capstone = self.isa.to_capstone().unwrap();
        info!(
            "\n{}",
            ctx.compiled_code()
                .unwrap()
                .disassemble(None, &capstone)
                .unwrap()
        );
        self.inner.clear_context(&mut ctx);
    }

    fn finalize(mut self) -> Module {
        self.inner.finalize_definitions().unwrap();
        Module {
            functions: self.functions,
            inner: SharedModuleData::new(
                self.inner,
                self.constants,
                self.registered_fns,
            ),
            type_info: self.type_info,
            context_description: self.context_description,
        }
    }

    /// Get the corresponding Cranelift type for a Roto type
    fn cranelift_type(&mut self, ty: &IrType) -> Type {
        match ty {
            IrType::Bool | IrType::U8 | IrType::I8 => I8,
            IrType::U16 | IrType::I16 => I16,
            IrType::U32 | IrType::I32 | IrType::Asn => I32,
            IrType::U64 | IrType::I64 => I64,
            IrType::F32 => F32,
            IrType::F64 => F64,
            IrType::Pointer => self.isa.pointer_type(),
        }
    }
}

impl<'c> FuncGen<'c> {
    fn finalize(mut self) {
        self.builder.seal_all_blocks();
        self.builder.finalize()
    }

    /// Set up the entry block for the function
    fn entry_block(
        &mut self,
        block: &lir::Block,
        parameters: &[(Identifier, IrType)],
        stack_slots: Vec<(Var, StackSlot)>,
        return_ptr: bool,
    ) {
        let entry_block = self.get_block(block.label);
        self.builder.switch_to_block(entry_block);

        let ty = self.module.cranelift_type(&IrType::Pointer);
        self.variable(
            &Var {
                scope: self.scope,
                kind: VarKind::Context,
            },
            ty,
        );

        if return_ptr {
            let ty = self.module.cranelift_type(&IrType::Pointer);
            self.variable(
                &Var {
                    scope: self.scope,
                    kind: VarKind::Return,
                },
                ty,
            );
        }

        for (x, ty) in parameters {
            let ty = self.module.cranelift_type(ty);
            let _ = self.variable(
                &Var {
                    scope: self.scope,
                    kind: VarKind::Explicit(*x),
                },
                ty,
            );
        }

        self.builder
            .append_block_params_for_function_params(entry_block);

        let args = self.builder.block_params(entry_block).to_owned();
        let mut args = args.into_iter();

        if return_ptr {
            self.def(
                self.module.variable_map[&Var {
                    scope: self.scope,
                    kind: VarKind::Return,
                }]
                    .0,
                args.next().unwrap(),
            )
        }

        self.def(
            self.module.variable_map[&Var {
                scope: self.scope,
                kind: VarKind::Context,
            }]
                .0,
            args.next().unwrap(),
        );

        for ((x, _), val) in parameters.iter().zip(args) {
            self.def(
                self.module.variable_map[&Var {
                    scope: self.scope,
                    kind: VarKind::Explicit(*x),
                }]
                    .0,
                val,
            );
        }

        for (v, slot) in stack_slots {
            let pointer_ty = self.module.isa.pointer_type();
            let p = self.ins().stack_addr(pointer_ty, slot, 0);
            self.def(self.module.variable_map[&v].0, p);
        }

        for instruction in &block.instructions {
            self.instruction(instruction);
        }
    }

    /// Translate an IR block to a Cranelift block
    fn block(&mut self, block: &lir::Block) {
        let b = self.get_block(block.label);
        self.builder.switch_to_block(b);

        for instruction in &block.instructions {
            self.instruction(instruction);
        }
    }

    /// Translate an IR instruction to cranelift instructions which are
    /// added to the current block
    fn instruction(&mut self, instruction: &lir::Instruction) {
        match instruction {
            lir::Instruction::Jump(label) => {
                let block = self.get_block(*label);
                self.ins().jump(block, &[]);
            }
            lir::Instruction::Switch {
                examinee,
                branches,
                default,
            } => {
                let mut switch = Switch::new();

                for (idx, label) in branches {
                    let block = self.get_block(*label);
                    switch.set_entry(*idx as u128, block);
                }

                let otherwise = self.get_block(*default);

                let (val, _) = self.operand(examinee);
                switch.emit(&mut self.builder, val, otherwise);
            }
            lir::Instruction::Assign { to, val, ty } => {
                let ty = self.module.cranelift_type(ty);
                let var = self.variable(to, ty);
                let (val, _) = self.operand(val);
                self.def(var, val)
            }
            lir::Instruction::Call {
                to,
                ctx,
                func,
                args,
                return_ptr,
            } => {
                let func = func.as_str();
                let func_id = self.module.functions[func].id;
                let func_ref = self
                    .module
                    .inner
                    .declare_func_in_func(func_id, self.builder.func);

                let mut new_args = Vec::new();

                if let Some(return_ptr) = return_ptr {
                    new_args.push(self.operand(&return_ptr.clone().into()).0);
                }

                new_args.push(self.operand(ctx).0);

                for arg in args {
                    new_args.push(self.operand(arg).0);
                }

                let inst = self.ins().call(func_ref, &new_args);

                if let Some((to, ty)) = to {
                    let ty = self.module.cranelift_type(ty);
                    let var = self.variable(to, ty);
                    self.def(var, self.builder.inst_results(inst)[0]);
                }
            }
            lir::Instruction::CallRuntime { func, args } => {
                let (ptr, trampoline_func_id) =
                    self.module.runtime_functions[func];
                let func_ref = self.module.inner.declare_func_in_func(
                    trampoline_func_id,
                    self.builder.func,
                );

                let ptr_type = self.module.isa.pointer_type();
                let ptr = self.ins().iconst(ptr_type, ptr as usize as i64);

                let mut new_args = Vec::new();
                new_args.push(ptr);
                new_args.extend(args.iter().map(|op| self.operand(op).0));

                self.ins().call(func_ref, &new_args);
            }
            lir::Instruction::Return(Some(v)) => {
                let (val, _) = self.operand(v);
                self.ins().return_(&[val]);
            }
            lir::Instruction::Return(None) => {
                self.ins().return_(&[]);
            }
            lir::Instruction::IntCmp {
                to,
                cmp,
                left,
                right,
            } => {
                let (l, _) = self.operand(left);
                let (r, _) = self.operand(right);
                let var = self.variable(to, I8);
                let val = self.int_cmp(l, r, cmp);
                self.def(var, val);
            }
            lir::Instruction::FloatCmp {
                to,
                cmp,
                left,
                right,
            } => {
                let (l, _) = self.operand(left);
                let (r, _) = self.operand(right);
                let var = self.variable(to, I8);
                let val = self.float_cmp(l, r, cmp);
                self.def(var, val);
            }
            lir::Instruction::Not { to, val } => {
                let (val, _) = self.operand(val);
                let var = self.variable(to, I8);
                let val = self.ins().icmp_imm(IntCC::Equal, val, 0);
                self.def(var, val);
            }
            lir::Instruction::Negate { to, val } => {
                let (val, val_ty) = self.operand(val);
                let var = self.variable(to, val_ty);

                let val = if let F32 | F64 = val_ty {
                    self.ins().fneg(val)
                } else {
                    self.ins().ineg(val)
                };

                self.def(var, val)
            }
            lir::Instruction::Add { to, left, right } => {
                let (l, left_ty) = self.operand(left);
                let (r, _) = self.operand(right);

                let var = self.variable(to, left_ty);
                // Possibly interesting note for later: this is wrapping
                // addition
                let val = if let F32 | F64 = left_ty {
                    self.ins().fadd(l, r)
                } else {
                    self.ins().iadd(l, r)
                };
                self.def(var, val)
            }
            lir::Instruction::Sub { to, left, right } => {
                let (l, left_ty) = self.operand(left);
                let (r, _) = self.operand(right);

                let var = self.variable(to, left_ty);
                // Possibly interesting note for later: this is wrapping
                // subtraction
                let val = if let F32 | F64 = left_ty {
                    self.ins().fsub(l, r)
                } else {
                    self.ins().isub(l, r)
                };
                self.def(var, val)
            }
            lir::Instruction::Mul { to, left, right } => {
                let (l, left_ty) = self.operand(left);
                let (r, _) = self.operand(right);

                let var = self.variable(to, left_ty);
                // Possibly interesting note for later: this is wrapping
                // multiplication
                let val = if let F32 | F64 = left_ty {
                    self.ins().fmul(l, r)
                } else {
                    self.ins().imul(l, r)
                };
                self.def(var, val)
            }
            lir::Instruction::Div {
                to,
                signed,
                left,
                right,
            } => {
                let (l, left_ty) = self.operand(left);
                let (r, _) = self.operand(right);

                let var = self.variable(to, left_ty);

                let val = match signed {
                    true => self.ins().sdiv(l, r),
                    false => self.ins().udiv(l, r),
                };
                self.def(var, val)
            }
            lir::Instruction::FDiv { to, left, right } => {
                let (l, left_ty) = self.operand(left);
                let (r, _) = self.operand(right);

                let var = self.variable(to, left_ty);

                let val = self.ins().fdiv(l, r);
                self.def(var, val)
            }
            lir::Instruction::Initialize { to, bytes, layout } => {
                let pointer_ty = self.module.isa.pointer_type();
                let slot =
                    self.builder.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        layout.size() as u32,
                        layout.align_shift() as u8,
                    ));

                let data_id = self
                    .module
                    .inner
                    .declare_anonymous_data(false, false)
                    .unwrap();
                let mut data_description = DataDescription::new();
                data_description.define(bytes.clone().into_boxed_slice());
                self.module
                    .inner
                    .define_data(data_id, &data_description)
                    .unwrap();
                let global_value = self
                    .module
                    .inner
                    .declare_data_in_func(data_id, self.builder.func);
                let value = self.ins().global_value(pointer_ty, global_value);

                let var = self.variable(to, pointer_ty);
                let p = self.ins().stack_addr(pointer_ty, slot, 0);
                self.builder.emit_small_memory_copy(
                    self.module.isa.frontend_config(),
                    p,
                    value,
                    bytes.len() as u64,
                    0,
                    0,
                    true,
                    MEMFLAGS,
                );
                self.def(var, p);
            }
            lir::Instruction::Write { to, val } => {
                let (x, _) = self.operand(val);
                let (to, _) = self.operand(to);
                self.ins().store(MEMFLAGS, x, to, 0);
            }
            lir::Instruction::Read { to, from, ty } => {
                let c_ty = self.module.cranelift_type(ty);
                let (from, _) = self.operand(from);
                let res = self.ins().load(c_ty, MEMFLAGS, from, 0);
                let to = self.variable(to, c_ty);
                self.def(to, res);
            }
            lir::Instruction::Offset { to, from, offset } => {
                let (from, _) = self.operand(from);
                let tmp = self.ins().iadd_imm(from, *offset as i64);
                let to = self.variable(to, self.module.isa.pointer_type());
                self.def(to, tmp)
            }
            lir::Instruction::Copy { to, from, size } => {
                let (dest, _) = self.operand(to);
                let (src, _) = self.operand(from);

                self.builder.emit_small_memory_copy(
                    self.module.isa.frontend_config(),
                    dest,
                    src,
                    *size as u64,
                    0,
                    0,
                    true,
                    MEMFLAGS,
                )
            }
            lir::Instruction::Clone { to, from, clone_fn } => {
                let (dest, _) = self.operand(to);
                let (src, _) = self.operand(from);
                let pointer_ty = self.module.isa.pointer_type();
                let clone = self
                    .ins()
                    .iconst(pointer_ty, *clone_fn as *mut u8 as usize as i64);
                self.builder.ins().call_indirect(
                    self.clone_signature,
                    clone,
                    &[src, dest],
                );
            }
            lir::Instruction::Drop { var, drop } => {
                if let Some(drop) = drop {
                    let (var, _) = self.operand(var);
                    let pointer_ty = self.module.isa.pointer_type();
                    let drop = self
                        .ins()
                        .iconst(pointer_ty, *drop as *mut u8 as usize as i64);
                    self.builder.ins().call_indirect(
                        self.drop_signature,
                        drop,
                        &[var],
                    );
                }
            }
            lir::Instruction::MemCmp {
                to,
                size,
                left,
                right,
            } => {
                let (left, _) = self.operand(left);
                let (right, _) = self.operand(right);
                let (size, _) = self.operand(size);

                // We could pass more precise alignment to cranelift, but
                // values of 1 should just work.
                let val = self.builder.call_memcmp(
                    self.module.isa.frontend_config(),
                    left,
                    right,
                    size,
                );

                let var = self.variable(to, I32);
                self.def(var, val);
            }
            lir::Instruction::ConstantAddress { to, name } => {
                let ty = self.module.cranelift_type(&IrType::Pointer);
                let const_ptr = self.module.constants.get(name).unwrap();
                let ptr = const_ptr.ptr() as usize;
                let val = self.ins().iconst(ty, ptr as i64);
                let to = self.variable(to, ty);
                self.def(to, val);
            }
            lir::Instruction::InitString {
                to,
                string,
                init_func,
            } => {
                let data_id = self
                    .module
                    .inner
                    .declare_anonymous_data(false, false)
                    .unwrap();

                let mut description = DataDescription::new();
                description.define(string.clone().into_bytes().into());
                self.module
                    .inner
                    .define_data(data_id, &description)
                    .unwrap();

                let global_value = self
                    .module
                    .inner
                    .declare_data_in_func(data_id, self.builder.func);

                let pointer_ty = self.module.isa.pointer_type();
                let init_func = self.ins().iconst(
                    pointer_ty,
                    *init_func as *mut u8 as usize as i64,
                );
                let data = self.ins().global_value(pointer_ty, global_value);
                let len = self.ins().iconst(I32, string.len() as u64 as i64);

                let (to, _) = self.operand(&Operand::Place(to.clone()));
                self.builder.ins().call_indirect(
                    self.init_string_signature,
                    init_func,
                    &[to, data, len],
                );
            }
        }
    }

    /// Get the block for the given label or create it if it doesn't exist
    fn get_block(&mut self, label: LabelRef) -> Block {
        *self
            .block_map
            .entry(label)
            .or_insert_with(|| self.builder.create_block())
    }

    /// Return the [`FuncInstBuilder`] for the function builder
    fn ins<'short>(&'short mut self) -> FuncInstBuilder<'short, 'c> {
        self.builder.ins()
    }

    /// Define a variable with a value
    fn def(&mut self, var: Variable, val: Value) {
        self.builder.def_var(var, val);
    }

    fn operand(&mut self, val: &Operand) -> (Value, Type) {
        match val {
            lir::Operand::Place(p) => {
                let (var, ty) = self.module.variable_map.get(p).map_or_else(
                    || {
                        ice!(
                            "did not find {:?} in {:#?}",
                            p,
                            self.module.variable_map,
                        )
                    },
                    |x| x,
                );
                (self.builder.use_var(*var), *ty)
            }
            lir::Operand::Value(v) => {
                if let Some((ty, val)) = self.integer_operand(v) {
                    (self.ins().iconst(ty, val), ty)
                } else if let Some((ty, val)) = self.float_operand(v) {
                    if ty == F32 {
                        (self.ins().f32const(val as f32), ty)
                    } else if ty == F64 {
                        (self.ins().f64const(val), ty)
                    } else {
                        ice!()
                    }
                } else {
                    ice!()
                }
            }
        }
    }

    fn integer_operand(&self, val: &IrValue) -> Option<(Type, i64)> {
        let pointer_ty = self.module.isa.pointer_type();
        Some(match val {
            IrValue::Bool(x) => (I8, *x as i64),
            IrValue::U8(x) => (I8, *x as i64),
            IrValue::U16(x) => (I16, *x as i64),
            IrValue::U32(x) => (I32, *x as i64),
            IrValue::U64(x) => (I64, *x as i64),
            IrValue::I8(x) => (I8, *x as i64),
            IrValue::I16(x) => (I16, *x as i64),
            IrValue::I32(x) => (I32, *x as i64),
            IrValue::I64(x) => (I64, *x),
            IrValue::Asn(x) => (I32, x.into_u32() as i64),
            IrValue::Pointer(x) => (pointer_ty, *x as i64),
            _ => return None,
        })
    }

    fn float_operand(&self, val: &IrValue) -> Option<(Type, f64)> {
        Some(match val {
            IrValue::F32(x) => (F32, *x as f64),
            IrValue::F64(x) => (F64, *x),
            _ => return None,
        })
    }

    fn variable(&mut self, var: &Var, ty: Type) -> Variable {
        let len = self.module.variable_map.len();
        let (var, _ty) =
            *self.module.variable_map.entry(var.clone()).or_insert_with(
                || {
                    let var = Variable::new(len);
                    self.builder.declare_var(var, ty);
                    (var, ty)
                },
            );
        var
    }

    fn int_cmp(&mut self, left: Value, right: Value, op: &IntCmp) -> Value {
        let cc = match op {
            IntCmp::Eq => IntCC::Equal,
            IntCmp::Ne => IntCC::NotEqual,
            IntCmp::ULt => IntCC::UnsignedLessThan,
            IntCmp::ULe => IntCC::UnsignedLessThanOrEqual,
            IntCmp::UGt => IntCC::UnsignedGreaterThan,
            IntCmp::UGe => IntCC::UnsignedGreaterThanOrEqual,
            IntCmp::SLt => IntCC::SignedLessThan,
            IntCmp::SLe => IntCC::SignedLessThanOrEqual,
            IntCmp::SGt => IntCC::SignedGreaterThan,
            IntCmp::SGe => IntCC::SignedGreaterThanOrEqual,
        };
        self.ins().icmp(cc, left, right)
    }

    fn float_cmp(
        &mut self,
        left: Value,
        right: Value,
        op: &FloatCmp,
    ) -> Value {
        let cc = match op {
            FloatCmp::Eq => FloatCC::Equal,
            FloatCmp::Ne => FloatCC::NotEqual,
            FloatCmp::Lt => FloatCC::LessThan,
            FloatCmp::Le => FloatCC::LessThanOrEqual,
            FloatCmp::Gt => FloatCC::GreaterThan,
            FloatCmp::Ge => FloatCC::GreaterThanOrEqual,
        };
        self.ins().fcmp(cc, left, right)
    }
}

impl Module {
    pub fn get_function<Ctx: 'static, F: RotoFunc>(
        &mut self,
        name: &str,
    ) -> Result<TypedFunc<Ctx, F>, FunctionRetrievalError> {
        let name = format!("pkg.{name}");
        let function_info = self.functions.get(&name).ok_or_else(|| {
            FunctionRetrievalError::DoesNotExist {
                name: name.to_string(),
                existing: self.functions.keys().cloned().collect(),
            }
        })?;

        let sig = &function_info.signature;
        let id = function_info.id;

        if self.context_description.type_id != TypeId::of::<Ctx>() {
            return Err(FunctionRetrievalError::TypeMismatch(
                "context type".into(),
                TypeMismatch {
                    rust_ty: type_name::<Ctx>().into(),
                    roto_ty: self.context_description.type_name.into(),
                },
            ));
        }

        F::check_args(&mut self.type_info, &sig.parameter_types)?;

        check_roto_type_reflect::<F::Return>(
            &mut self.type_info,
            &sig.return_type,
        )
        .map_err(|e| {
            FunctionRetrievalError::TypeMismatch(
                "the return value".to_string(),
                e,
            )
        })?;

        let func_ptr = self.inner.0.cranelift_jit.get_finalized_function(id);
        Ok(TypedFunc {
            func: func_ptr,
            return_by_ref: function_info.return_by_ref,
            _module: self.inner.clone(),
            _ty: PhantomData,
        })
    }
}
