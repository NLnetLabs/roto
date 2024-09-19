//! Machine code generation via cranelift

use std::{
    any::TypeId, collections::HashMap, marker::PhantomData, sync::Arc,
};

use crate::{
    ast::Identifier,
    lower::{
        ir::{self, IntCmp, Operand, Var, VarKind},
        label::{LabelRef, LabelStore},
        value::IrType,
        IrFunction,
    },
    runtime::ty::{Reflect, TypeRegistry},
    typechecker::{info::TypeInfo, scope::ScopeRef, types},
    IrValue,
};
use check::{
    check_roto_type_reflect, return_type_by_ref, FunctionRetrievalError,
    RotoParams,
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
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, FuncId, Linkage, Module as _};
use log::info;
use string_interner::{backend::StringBackend, StringInterner};

pub mod check;
#[cfg(test)]
mod tests;

/// A compiled, ready-to-run Roto module
pub struct Module {
    /// The set of public functions and their signatures.
    functions: HashMap<String, FunctionInfo>,

    /// The inner cranelift module
    inner: JITModule,

    /// Info from the typechecker for checking types against Rust types
    type_info: TypeInfo,
}

pub struct TypedFunc<'module, Params, Return> {
    func: *const u8,
    return_by_ref: bool,
    _ty: PhantomData<&'module (Params, Return)>,
}

unsafe impl<Params, Return> Send for TypedFunc<'_, Params, Return> {}
unsafe impl<Params, Return> Sync for TypedFunc<'_, Params, Return> {}

impl<'module, Params: RotoParams, Return: Reflect>
    TypedFunc<'module, Params, Return>
{
    pub fn call_tuple(&self, params: Params) -> Return {
        unsafe { params.invoke::<Return>(self.func, self.return_by_ref) }
    }
}

macro_rules! call_impl {
    ($($ty:ident),*) => {
        impl<'module, $($ty,)* Return: Reflect> TypedFunc<'module, ($($ty,)*), Return>
        where
            ($($ty,)*): RotoParams,
        {
            #[allow(non_snake_case)]
            #[allow(clippy::too_many_arguments)]
            pub fn call(&self, $($ty: $ty,)*) -> Return {
                self.call_tuple(($($ty,)*))
            }

            #[allow(non_snake_case)]
            pub fn into_func(self) -> impl Fn($($ty,)*) -> Return + 'module {
                move |$($ty,)*| self.call($($ty,)*)
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
}

struct ModuleBuilder<'a> {
    /// The set of public functions and their signatures.
    functions: HashMap<String, FunctionInfo>,

    /// External functions
    runtime_functions: HashMap<usize, FuncId>,

    /// The inner cranelift module
    inner: JITModule,

    /// Map of cranelift variables and their types
    ///
    /// This is necessary because cranelift does not seem to allow us to
    /// query it.
    variable_map: HashMap<Var, (Variable, Type)>,

    /// Instruction set architecture
    isa: Arc<dyn TargetIsa>,

    /// Identifiers are used for debugging and resolving function names.
    identifiers: &'a StringInterner<StringBackend>,

    /// To print labels for debugging.
    #[allow(unused)]
    label_store: LabelStore,

    type_info: TypeInfo,
}

struct FuncGen<'a, 'c> {
    module: &'c mut ModuleBuilder<'a>,

    /// The cranelift function builder
    builder: FunctionBuilder<'c>,

    /// Scope of the function
    scope: ScopeRef,

    /// Blocks of the function
    block_map: HashMap<LabelRef, Block>,
}

// We use `with_aligned` to make sure that we notice if anything is
// unaligned. It does add additional checks, so should be disabled at some
// point, or at least be configurable.
const MEMFLAGS: MemFlags = MemFlags::new().with_aligned();

pub fn codegen(
    ir: &[ir::Function],
    runtime_functions: &HashMap<usize, IrFunction>,
    identifiers: &StringInterner<StringBackend>,
    label_store: LabelStore,
    type_info: TypeInfo,
) -> Module {
    // The ISA is the Instruction Set Architecture. We always compile for
    // the system we run on, so we use `cranelift_native` to get the ISA
    // for the current system. We enable building for speed only. Size is
    // not super important at the moment.
    let mut settings = settings::builder();
    settings.set("opt_level", "speed").unwrap();
    let flags = settings::Flags::new(settings);
    let isa = cranelift_native::builder().unwrap().finish(flags).unwrap();

    let mut builder = JITBuilder::with_isa(
        isa.to_owned(),
        cranelift_module::default_libcall_names(),
    );

    for (name, func) in runtime_functions {
        builder.symbol(format!("runtime_function_{name}"), func.ptr);
    }

    let jit = JITModule::new(builder);

    let mut module = ModuleBuilder {
        functions: HashMap::new(),
        runtime_functions: HashMap::new(),
        inner: jit,
        isa,
        variable_map: HashMap::new(),
        identifiers,
        label_store,
        type_info,
    };

    for (roto_func_id, func) in runtime_functions {
        let mut sig = module.inner.make_signature();
        for ty in &func.params {
            sig.params.push(AbiParam::new(module.cranelift_type(ty)));
        }
        if let Some(ty) = &func.ret {
            sig.returns.push(AbiParam::new(module.cranelift_type(ty)));
        }
        let Ok(func_id) = module.inner.declare_function(
            &format!("runtime_function_{roto_func_id}"),
            Linkage::Import,
            &sig,
        ) else {
            panic!()
        };
        module.runtime_functions.insert(*roto_func_id, func_id);
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

impl ModuleBuilder<'_> {
    /// Declare a function and its signature (without the body)
    fn declare_function(&mut self, func: &ir::Function) {
        let ir::Function {
            name,
            ir_signature,
            signature,
            public,
            ..
        } = func;

        let mut sig = self.inner.make_signature();
        for (_, ty) in &ir_signature.parameters {
            sig.params.push(AbiParam::new(self.cranelift_type(ty)));
        }

        sig.returns = match &ir_signature.return_type {
            Some(ty) => vec![AbiParam::new(self.cranelift_type(ty))],
            None => Vec::new(),
        };

        let name = self.identifiers.resolve(name.0).unwrap();
        let func_id = self
            .inner
            .declare_function(
                name,
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
                signature: signature.clone(),
            },
        );
    }

    /// Define a function body
    ///
    /// The function must be declared first.
    fn define_function(
        &mut self,
        func: &ir::Function,
        builder_context: &mut FunctionBuilderContext,
    ) {
        let ir::Function {
            name,
            blocks,
            ir_signature,
            scope,
            ..
        } = func;
        let name = self.identifiers.resolve(name.0).unwrap();
        let func_id = self.functions[name].id;

        let mut ctx = self.inner.make_context();
        let mut sig = self.inner.make_signature();

        if ir_signature.return_ptr {
            sig.params
                .push(AbiParam::new(self.cranelift_type(&IrType::Pointer)));
        }

        for (_, ty) in &ir_signature.parameters {
            sig.params.push(AbiParam::new(self.cranelift_type(ty)));
        }

        if let Some(ty) = &ir_signature.return_type {
            sig.returns.push(AbiParam::new(self.cranelift_type(ty)));
        }

        ctx.func.signature = sig;
        ctx.set_disasm(true);

        let builder = FunctionBuilder::new(&mut ctx.func, builder_context);

        let mut func_gen = FuncGen {
            module: self,
            builder,
            scope: *scope,
            block_map: HashMap::new(),
        };

        func_gen.entry_block(
            &blocks[0],
            &ir_signature.parameters,
            ir_signature.return_ptr,
        );

        for block in blocks {
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
            inner: self.inner,
            type_info: self.type_info,
        }
    }

    /// Get the corresponding Cranelift type for a Roto type
    fn cranelift_type(&mut self, ty: &IrType) -> Type {
        match ty {
            IrType::Bool | IrType::U8 | IrType::I8 => I8,
            IrType::U16 | IrType::I16 => I16,
            IrType::U32 | IrType::I32 | IrType::Asn => I32,
            IrType::U64 | IrType::I64 => I64,
            IrType::Pointer | IrType::ExtPointer => self.isa.pointer_type(),
            IrType::ExtValue => todo!(),
        }
    }
}

impl<'a, 'c> FuncGen<'a, 'c> {
    fn finalize(self) {
        self.builder.finalize()
    }

    /// Set up the entry block for the function
    fn entry_block(
        &mut self,
        block: &ir::Block,
        parameters: &[(Identifier, IrType)],
        return_ptr: bool,
    ) {
        let entry_block = self.get_block(block.label);
        self.builder.switch_to_block(entry_block);

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
    }

    /// Translate an IR block to a Cranelift block
    fn block(&mut self, block: &ir::Block) {
        let b = self.get_block(block.label);
        self.builder.switch_to_block(b);
        self.builder.seal_block(b);

        for instruction in &block.instructions {
            self.instruction(instruction);
        }
    }

    /// Translate an IR instruction to cranelift instructions which are
    /// added to the current block
    fn instruction(&mut self, instruction: &ir::Instruction) {
        match instruction {
            ir::Instruction::Jump(label) => {
                let block = self.get_block(*label);
                self.ins().jump(block, &[]);
            }
            ir::Instruction::Switch {
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
            ir::Instruction::Assign { to, val, ty } => {
                let ty = self.module.cranelift_type(ty);
                let var = self.variable(to, ty);
                let (val, _) = self.operand(val);
                self.def(var, val)
            }
            ir::Instruction::Call { to, func, args } => {
                let func = self.module.identifiers.resolve(func.0).unwrap();
                let func_id = self.module.functions[func].id;
                let func_ref = self
                    .module
                    .inner
                    .declare_func_in_func(func_id, self.builder.func);

                let args: Vec<_> =
                    args.iter().map(|(_, a)| self.operand(a).0).collect();

                let inst = self.ins().call(func_ref, &args);

                if let Some((to, ty)) = to {
                    let ty = self.module.cranelift_type(ty);
                    let var = self.variable(to, ty);
                    self.def(var, self.builder.inst_results(inst)[0]);
                }
            }
            ir::Instruction::CallRuntime { to, func, args } => {
                let func_id = self.module.runtime_functions[&func.id];
                let func_ref = self
                    .module
                    .inner
                    .declare_func_in_func(func_id, self.builder.func);

                let args: Vec<_> =
                    args.iter().map(|op| self.operand(op).0).collect();
                let inst = self.ins().call(func_ref, &args);

                if let Some((to, ty)) = to {
                    let ty = self.module.cranelift_type(ty);
                    let var = self.variable(to, ty);
                    self.def(var, self.builder.inst_results(inst)[0]);
                }
            }
            ir::Instruction::Return(Some(v)) => {
                let (val, _) = self.operand(v);
                self.ins().return_(&[val]);
            }
            ir::Instruction::Return(None) => {
                self.ins().return_(&[]);
            }
            ir::Instruction::Cmp {
                to,
                cmp,
                left,
                right,
            } => {
                let (l, _) = self.operand(left);
                let (r, _) = self.operand(right);
                let var = self.variable(to, I8);
                let val = self.binop(l, r, cmp);
                self.def(var, val);
            }
            ir::Instruction::Not { to, val } => {
                let (val, _) = self.operand(val);
                let var = self.variable(to, I8);
                let val = self.ins().icmp_imm(IntCC::Equal, val, 0);
                self.def(var, val);
            }
            ir::Instruction::And { to, left, right } => {
                let (l, _) = self.operand(left);
                let (r, _) = self.operand(right);
                let var = self.variable(to, I8);
                let val = self.ins().band(l, r);
                self.def(var, val);
            }
            ir::Instruction::Or { to, left, right } => {
                let (l, _) = self.operand(left);
                let (r, _) = self.operand(right);
                let var = self.variable(to, I8);
                let val = self.ins().bor(l, r);
                self.def(var, val);
            }
            ir::Instruction::Add { to, left, right } => {
                let (l, left_ty) = self.operand(left);
                let (r, _) = self.operand(right);

                let var = self.variable(to, left_ty);
                // Possibly interesting note for later: this is wrapping
                // addition
                let val = self.ins().iadd(l, r);
                self.def(var, val)
            }
            ir::Instruction::Sub { to, left, right } => {
                let (l, left_ty) = self.operand(left);
                let (r, _) = self.operand(right);

                let var = self.variable(to, left_ty);
                // Possibly interesting note for later: this is wrapping
                // subtraction
                let val = self.ins().isub(l, r);
                self.def(var, val)
            }
            ir::Instruction::Mul { to, left, right } => {
                let (l, left_ty) = self.operand(left);
                let (r, _) = self.operand(right);

                let var = self.variable(to, left_ty);
                // Possibly interesting note for later: this is wrapping
                // multiplication
                let val = self.ins().imul(l, r);
                self.def(var, val)
            }
            ir::Instruction::Div {
                to,
                ty,
                left,
                right,
            } => {
                let (l, left_ty) = self.operand(left);
                let (r, _) = self.operand(right);

                let var = self.variable(to, left_ty);

                let val = match ty {
                    IrType::I8 | IrType::I16 | IrType::I32 | IrType::I64 => {
                        self.ins().sdiv(l, r)
                    }
                    IrType::U8 | IrType::U16 | IrType::U32 | IrType::U64 => {
                        self.ins().udiv(l, r)
                    }
                    _ => panic!(),
                };
                self.def(var, val)
            }
            ir::Instruction::Extend { to, ty, from } => {
                let ty = self.module.cranelift_type(ty);
                let (from, _) = self.operand(from);
                let val = self.ins().uextend(ty, from);

                let var = self.variable(to, ty);
                self.def(var, val)
            }
            ir::Instruction::Eq { .. } => todo!(),
            ir::Instruction::Alloc {
                to,
                size,
                align_shift,
            } => {
                let slot =
                    self.builder.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        *size,
                        *align_shift,
                    ));

                let pointer_ty = self.module.isa.pointer_type();
                let var = self.variable(to, pointer_ty);
                let p = self.ins().stack_addr(pointer_ty, slot, 0);
                self.def(var, p);
            }
            ir::Instruction::Initialize {
                to,
                bytes,
                align_shift,
            } => {
                let pointer_ty = self.module.isa.pointer_type();
                let slot =
                    self.builder.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        bytes.len() as u32,
                        *align_shift,
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
            ir::Instruction::Write { to, val } => {
                let (x, _) = self.operand(val);
                let (to, _) = self.operand(to);
                self.ins().store(MEMFLAGS, x, to, 0);
            }
            ir::Instruction::Read { to, from, ty } => {
                let c_ty = self.module.cranelift_type(ty);
                let (from, _) = self.operand(from);
                let res = self.ins().load(c_ty, MEMFLAGS, from, 0);
                let to = self.variable(to, c_ty);
                self.def(to, res);
            }
            ir::Instruction::Offset { to, from, offset } => {
                let (from, _) = self.operand(from);
                let tmp = self.ins().iadd_imm(from, *offset as i64);
                let to = self.variable(to, self.module.isa.pointer_type());
                self.def(to, tmp)
            }
            ir::Instruction::Copy { to, from, size } => {
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
            ir::Instruction::MemCmp {
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
        let pointer_ty = self.module.isa.pointer_type();
        match val {
            ir::Operand::Place(p) => {
                let (var, ty) = self.module.variable_map.get(p).map_or_else(
                    || {
                        panic!(
                            "did not find {:?} in {:#?}",
                            p, self.module.variable_map,
                        )
                    },
                    |x| x,
                );
                (self.builder.use_var(*var), *ty)
            }
            ir::Operand::Value(v) => {
                let (ty, val) = match v {
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
                    _ => todo!(),
                };
                (self.ins().iconst(ty, val), ty)
            }
        }
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

    fn binop(&mut self, left: Value, right: Value, op: &IntCmp) -> Value {
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
}

impl Module {
    pub fn get_function<'module, Params: RotoParams, Return: Reflect>(
        &'module mut self,
        type_registry: &mut TypeRegistry,
        identifiers: &StringInterner<StringBackend>,
        name: &str,
    ) -> Result<TypedFunc<'module, Params, Return>, FunctionRetrievalError>
    {
        let function_info = self.functions.get(name).ok_or_else(|| {
            FunctionRetrievalError::DoesNotExist {
                name: name.to_string(),
                existing: self.functions.keys().cloned().collect(),
            }
        })?;

        let sig = &function_info.signature;
        let id = function_info.id;

        Params::check(
            type_registry,
            &mut self.type_info,
            identifiers,
            &sig.parameter_types,
        )?;

        check_roto_type_reflect::<Return>(
            type_registry,
            &mut self.type_info,
            identifiers,
            &sig.return_type,
        )
        .map_err(|e| {
            FunctionRetrievalError::TypeMismatch(
                "the return value".to_string(),
                e,
            )
        })?;

        let return_by_ref =
            return_type_by_ref(type_registry, TypeId::of::<Return>());

        let func_ptr = self.inner.get_finalized_function(id);
        Ok(TypedFunc {
            func: func_ptr,
            return_by_ref,
            _ty: PhantomData,
        })
    }
}
