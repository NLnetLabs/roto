//! Machine code generation via cranelift

use std::{
    collections::HashMap, marker::PhantomData, num::NonZeroU8, sync::Arc,
};

use crate::{
    lower::{
        ir::{self, IntCmp, Operand},
        value::IrType,
    },
    IrValue, Runtime,
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
use cranelift_module::{FuncId, Linkage, Module as _};
use log::info;

#[cfg(test)]
mod tests;

/// A compiled, ready-to-run Roto module
pub struct Module {
    /// The set of public functions and their signatures.
    functions: HashMap<String, (FuncId, ir::Signature)>,
    /// The inner cranelift module
    inner: JITModule,
}

pub struct TypedFunc<Params, Return> {
    func: *const u8,
    _ty: PhantomData<(Params, Return)>,
}

impl<Params: RotoParams, Return: RotoType> TypedFunc<Params, Return> {
    pub fn call(&self, params: Params) -> Return {
        unsafe { Params::invoke::<Return>(self.func, params) }
    }
}

struct ModuleBuilder<'a> {
    /// The set of public functions and their signatures.
    functions: HashMap<String, (FuncId, ir::Signature)>,
    /// The inner cranelift module
    inner: JITModule,
    variable_map: HashMap<&'a str, (Variable, Type)>,
    isa: Arc<dyn TargetIsa>,
}

struct FuncGen<'a, 'c> {
    module: &'c mut ModuleBuilder<'a>,
    builder: FunctionBuilder<'c>,
    block_map: HashMap<&'a str, Block>,
}

// We use `with_aligned` to make sure that we notice if anything is
// unaligned. It does add additional checks, so should be disabled at some
// point, or at least be configurable.
const MEMFLAGS: MemFlags = MemFlags::new().with_aligned();

pub fn codegen(ir: &[ir::Function], _runtime: &Runtime) -> Module {
    // The ISA is the Instruction Set Architecture. We always compile for
    // the system we run on, so we use `cranelift_native` to get the ISA
    // for the current system. We enable building for speed only. Size is
    // not super important at the moment.
    let mut settings = settings::builder();
    settings.set("opt_level", "speed").unwrap();
    let flags = settings::Flags::new(settings);
    let isa = cranelift_native::builder().unwrap().finish(flags).unwrap();

    let builder = JITBuilder::with_isa(
        isa.to_owned(),
        cranelift_module::default_libcall_names(),
    );

    // TODO: Declare runtime items here.

    let jit = JITModule::new(builder);

    let mut module = ModuleBuilder {
        functions: HashMap::new(),
        inner: jit,
        isa,
        variable_map: HashMap::new(),
    };

    // Our functions might call each other, so we declare them before we
    // define them. This is also when we start building the function
    // hashmap.
    for func in ir {
        module.declare_function(func);
    }

    let mut builder_context = FunctionBuilderContext::new();
    for func in ir {
        module.define_function(func, &mut builder_context);
        info!("\n{}", func);
    }

    module.finalize()
}

impl<'a> ModuleBuilder<'a> {
    /// Declare a function and its signature (without the body)
    fn declare_function(&mut self, func: &ir::Function) {
        let ir::Function {
            name,
            signature,
            public,
            ..
        } = func;

        let mut sig = self.inner.make_signature();
        for (_, ty) in &signature.parameters {
            sig.params.push(AbiParam::new(self.cranelift_type(ty)));
        }

        sig.returns = match &signature.return_type {
            Some(ty) => vec![AbiParam::new(self.cranelift_type(ty))],
            None => Vec::new(),
        };

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

        self.functions.insert(name.clone(), (func_id, signature.clone()));
    }

    /// Define a function body
    ///
    /// The function must be declared first.
    fn define_function(
        &mut self,
        func: &'a ir::Function,
        builder_context: &mut FunctionBuilderContext,
    ) {
        let ir::Function {
            name,
            blocks,
            signature,
            ..
        } = func;
        let (func_id, _) = self.functions[name].clone();

        let mut ctx = self.inner.make_context();
        let mut sig = self.inner.make_signature();

        for (_, ty) in &signature.parameters {
            sig.params.push(AbiParam::new(self.cranelift_type(ty)));
        }

        if let Some(ty) = &signature.return_type {
            sig.returns.push(AbiParam::new(self.cranelift_type(ty)));
        }

        ctx.func.signature = sig;
        ctx.set_disasm(true);

        let builder = FunctionBuilder::new(&mut ctx.func, builder_context);

        let mut func_gen = FuncGen {
            module: self,
            builder,
            block_map: HashMap::new(),
        };

        func_gen.entry_block(&blocks[0], &signature.parameters);

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
        }
    }

    /// Get the corresponding Cranelift type for a Roto type
    fn cranelift_type(&mut self, ty: &IrType) -> Type {
        match ty {
            IrType::Bool | IrType::U8 | IrType::I8 => I8,
            IrType::U16 | IrType::I16 => I16,
            IrType::U32 | IrType::I32 => I32,
            IrType::U64 | IrType::I64 => I64,
            IrType::IpAddr => I32,
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
        block: &'a ir::Block,
        parameters: &'a [(String, IrType)],
    ) {
        let entry_block = self.get_block(&block.label);
        self.builder.switch_to_block(entry_block);

        for (x, ty) in parameters {
            let ty = self.module.cranelift_type(ty);
            let _ = self.variable(x, ty);
        }

        self.builder
            .append_block_params_for_function_params(entry_block);

        let args = self.builder.block_params(entry_block).to_owned();
        for ((x, _), val) in parameters.iter().zip(args) {
            self.def(self.module.variable_map[&x.as_ref()].0, val);
        }
    }

    /// Translate an IR block to a Cranelift block
    fn block(&mut self, block: &'a ir::Block) {
        let b = self.get_block(&block.label);
        self.builder.switch_to_block(b);
        self.builder.seal_block(b);

        for instruction in &block.instructions {
            self.instruction(instruction);
        }
    }

    /// Translate an IR instruction to cranelift instructions which are
    /// added to the current block
    fn instruction(&mut self, instruction: &'a ir::Instruction) {
        match instruction {
            ir::Instruction::Jump(label) => {
                let block = self.get_block(label);
                self.ins().jump(block, &[]);
            }
            ir::Instruction::Switch {
                examinee,
                branches,
                default,
            } => {
                let mut switch = Switch::new();

                for (idx, label) in branches {
                    let block = self.get_block(label);
                    switch.set_entry(*idx as u128, block);
                }

                let otherwise = self.get_block(default);

                let (val, _) = self.operand(examinee);
                switch.emit(&mut self.builder, val, otherwise);
            }
            ir::Instruction::Assign { to, val, ty } => {
                let ty = self.module.cranelift_type(ty);
                let var = self.variable(&to.var, ty);
                let (val, _) = self.operand(val);
                self.def(var, val)
            }
            ir::Instruction::Call { to, func, args } => {
                let (func_id, _) = self.module.functions[func];
                let func_ref = self
                    .module
                    .inner
                    .declare_func_in_func(func_id, self.builder.func);

                let args: Vec<_> =
                    args.iter().map(|(_, a)| self.operand(a).0).collect();
                let inst = self.ins().call(func_ref, &args);

                if let Some((to, ty)) = to {
                    let ty = self.module.cranelift_type(ty);
                    let var = self.variable(&to.var, ty);
                    self.def(var, self.builder.inst_results(inst)[0]);
                }
            }
            ir::Instruction::CallRuntime { to, func, args } => todo!(),
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
                let var = self.variable(&to.var, I8);
                let val = self.binop(l, r, cmp);
                self.def(var, val);
            }
            ir::Instruction::Not { to, val } => {
                let (val, _) = self.operand(val);
                let var = self.variable(&to.var, I8);
                let val = self.ins().icmp_imm(IntCC::Equal, val, 0);
                self.def(var, val);
            }
            ir::Instruction::And { to, left, right } => {
                let (l, _) = self.operand(left);
                let (r, _) = self.operand(right);
                let var = self.variable(&to.var, I8);
                let val = self.ins().band(l, r);
                self.def(var, val);
            }
            ir::Instruction::Or { to, left, right } => {
                let (l, _) = self.operand(left);
                let (r, _) = self.operand(right);
                let var = self.variable(&to.var, I8);
                let val = self.ins().bor(l, r);
                self.def(var, val);
            }
            ir::Instruction::Add { to, left, right } => {
                let (l, left_ty) = self.operand(left);
                let (r, _) = self.operand(right);

                let var = self.variable(&to.var, left_ty);
                // Possibly interesting note for later: this is wrapping
                // addition
                let val = self.ins().iadd(l, r);
                self.def(var, val)
            }
            ir::Instruction::Sub { to, left, right } => {
                let (l, left_ty) = self.operand(left);
                let (r, _) = self.operand(right);

                let var = self.variable(&to.var, left_ty);
                // Possibly interesting note for later: this is wrapping
                // subtraction
                let val = self.ins().isub(l, r);
                self.def(var, val)
            }
            ir::Instruction::Mul { to, left, right } => {
                let (l, left_ty) = self.operand(left);
                let (r, _) = self.operand(right);

                let var = self.variable(&to.var, left_ty);
                // Possibly interesting note for later: this is wrapping
                // multiplication
                let val = self.ins().imul(l, r);
                self.def(var, val)
            }
            ir::Instruction::Div { to, ty, left, right } => {
                let (l, left_ty) = self.operand(left);
                let (r, _) = self.operand(right);

                let var = self.variable(&to.var, left_ty);

                let val = match ty {
                    IrType::I8 | IrType::I16 | IrType::I32 | IrType::I64 => {
                        self.ins().sdiv(l, r)
                    }
                    IrType::U8 | IrType::U16 | IrType::U32 | IrType::U64 => {
                        self.ins().udiv(l, r)
                    }
                    _ => panic!()
                };
                self.def(var, val)
            }
            ir::Instruction::Eq { .. } => todo!(),
            ir::Instruction::Alloc { to, size } => {
                let slot = self.builder.create_sized_stack_slot(
                    StackSlotData::new(StackSlotKind::ExplicitSlot, *size),
                );

                let pointer_ty = self.module.isa.pointer_type();
                let var = self.variable(&to.var, pointer_ty);
                let p = self.ins().stack_addr(pointer_ty, slot, 0);
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
                let to = self.variable(&to.var, c_ty);
                self.def(to, res);
            }
            ir::Instruction::Offset { to, from, offset } => {
                let (from, _) = self.operand(from);
                let tmp = self.ins().iadd_imm(from, *offset as i64);
                let to =
                    self.variable(&to.var, self.module.isa.pointer_type());
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

                // We could pass more precise alignment to cranelift, but
                // values of 1 should just work.
                let val = self.builder.emit_small_memory_compare(
                    self.module.isa.frontend_config(),
                    IntCC::Equal,
                    left,
                    right,
                    *size as u64,
                    NonZeroU8::new(1).unwrap(),
                    NonZeroU8::new(1).unwrap(),
                    MEMFLAGS,
                );
                let var = self.variable(&to.var, I8);
                self.def(var, val);
            }
        }
    }

    /// Get the block for the given label or create it if it doesn't exist
    fn get_block(&mut self, label: &'a str) -> Block {
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

    fn operand(&mut self, val: &'a Operand) -> (Value, Type) {
        let pointer_ty = self.module.isa.pointer_type();
        match val {
            ir::Operand::Place(p) => {
                let a: &'a str = &p.var;
                let (var, ty) = self.module.variable_map[a];
                (self.builder.use_var(var), ty)
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
                    IrValue::Pointer(x) => (pointer_ty, *x as i64),
                    _ => todo!(),
                };
                (self.ins().iconst(ty, val), ty)
            }
        }
    }

    fn variable(&mut self, var: &'a str, ty: Type) -> Variable {
        let len = self.module.variable_map.len();
        let (var, _ty) =
            *self.module.variable_map.entry(var).or_insert_with(|| {
                let var = Variable::new(len);
                self.builder.declare_var(var, ty);
                (var, ty)
            });
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
    pub fn get_function<Params: RotoParams, Return: RotoType>(
        &self,
        name: &str,
    ) -> Option<TypedFunc<Params, Return>> {
        let (id, sig) = self.functions.get(name)?;
        let correct_params = Params::check(
            &sig.parameters.iter().map(|p| p.1).collect::<Vec<_>>(),
        );
        let correct_return =
            Return::check(sig.return_type);
        if !correct_params || !correct_return {
            return None;
        }

        let func_ptr = self.inner.get_finalized_function(*id);
        Some(TypedFunc {
            func: func_ptr,
            _ty: PhantomData,
        })
    }
}

/// A type that is compatible with Roto
/// 
/// Such a type needs to have a corresponding roto representation.
/// It also needs to have a size and be convertible into a slice(?).
/// 
/// We need to do several things with this:
///  - Do runtime type checking before handing out a roto function.
///  - Do runtime type checking 
pub trait RotoType {
    fn check(ty: Option<IrType>) -> bool;
}

impl RotoType for bool {
    fn check(ty: Option<IrType>) -> bool {
        ty == Some(IrType::Bool)
    }
}

impl RotoType for i8 {
    fn check(ty: Option<IrType>) -> bool {
        ty == Some(IrType::I8)
    }
}

impl RotoType for u8 {
    fn check(ty: Option<IrType>) -> bool {
        ty == Some(IrType::U8)
    }
}

impl RotoType for i16 {
    fn check(ty: Option<IrType>) -> bool {
        ty == Some(IrType::I16)
    }
}

impl RotoType for u16 {
    fn check(ty: Option<IrType>) -> bool {
        ty == Some(IrType::U16)
    }
}

impl RotoType for i32 {
    fn check(ty: Option<IrType>) -> bool {
        ty == Some(IrType::I32)
    }
}

impl RotoType for u32 {
    fn check(ty: Option<IrType>) -> bool {
        ty == Some(IrType::U32)
    }
}

impl RotoType for () {
    fn check(ty: Option<IrType>) -> bool {
        ty.is_none()
    }
}

impl<T> RotoType for *mut T {
    fn check(ty: Option<IrType>) -> bool {
        ty == Some(IrType::Pointer)
    }
}

pub trait RotoParams {
    fn check(ty: &[IrType]) -> bool {
        ty.is_empty()
    }

    unsafe fn invoke<R>(func_ptr: *const u8, params: Self) -> R;
}

impl RotoParams for () {
    fn check(ty: &[IrType]) -> bool {
        ty.is_empty()
    }

    unsafe fn invoke<R>(func_ptr: *const u8, (): Self) -> R {
        let func_ptr =
            unsafe { std::mem::transmute::<_, fn() -> R>(func_ptr) };
        func_ptr()
    }
}

impl<A1> RotoParams for (A1,)
where
    A1: RotoType,
{
    fn check(ty: &[IrType]) -> bool {
        let &[ty] = ty else {
            return false;
        };
        A1::check(Some(ty))
    }

    unsafe fn invoke<R>(func_ptr: *const u8, (a1,): Self) -> R {
        let func_ptr =
            unsafe { std::mem::transmute::<_, fn(A1) -> R>(func_ptr) };
        func_ptr(a1)
    }
}

impl<A1, A2> RotoParams for (A1, A2)
where
    A1: RotoType,
    A2: RotoType,
{
    fn check(ty: &[IrType]) -> bool {
        let &[ty1, ty2] = ty else {
            return false;
        };
        A1::check(Some(ty1)) && A2::check(Some(ty2))
    }

    unsafe fn invoke<R>(func_ptr: *const u8, (a1, a2): Self) -> R {
        let func_ptr =
            unsafe { std::mem::transmute::<_, fn(A1, A2) -> R>(func_ptr) };
        func_ptr(a1, a2)
    }
}
