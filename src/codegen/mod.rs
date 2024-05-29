//! Machine code generation via cranelift

use std::{collections::HashMap, marker::PhantomData, rc::Rc, sync::Arc};

use crate::{
    lower::ir::{self, IntCmp, Operand},
    typechecker::{
        types::{Primitive, Type as RotoType},
        TypeInfo,
    },
    Runtime, SafeValue,
};
use cranelift::{
    codegen::{
        entity::EntityRef,
        ir::{
            condcodes::IntCC, types::*, AbiParam, Block, InstBuilder,
            MemFlags, Signature, StackSlot, StackSlotData, StackSlotKind,
            Value,
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
    functions: HashMap<String, (FuncId, Signature)>,
    /// The inner cranelift module
    inner: JITModule,
}

pub struct TypedFunc<Params, Return> {
    func: *const u8,
    _ty: PhantomData<(Params, Return)>,
}

impl<Params: RotoParams, Return: IsRotoType> TypedFunc<Params, Return> {
    pub fn call(&self, params: Params) -> Return {
        unsafe { Params::invoke::<Return>(self.func, params) }
    }
}

struct ModuleBuilder<'r, 'a> {
    /// The set of public functions and their signatures.
    functions: HashMap<String, (FuncId, Signature)>,
    /// The inner cranelift module
    inner: JITModule,
    type_info: &'r mut TypeInfo,
    variable_map: HashMap<&'a str, Variable>,
    isa: Arc<dyn TargetIsa>,
}

struct FuncGen<'r, 'a, 'c> {
    module: &'c mut ModuleBuilder<'r, 'a>,
    builder: FunctionBuilder<'c>,
    block_map: HashMap<&'a str, Block>,
}

// We use `with_aligned` to make sure that we notice if anything is
// unaligned. It does add additional checks, so should be disabled at some
// point, or at least be configurable.
const MEMFLAGS: MemFlags = MemFlags::new().with_aligned();

pub fn codegen(
    ir: &[ir::Function],
    type_info: &mut TypeInfo,
    _runtime: &Runtime,
) -> Module {
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
        type_info,
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

impl<'a> ModuleBuilder<'_, 'a> {
    /// Declare a function and its signature (without the body)
    fn declare_function(&mut self, func: &ir::Function) {
        let ir::Function {
            name,
            parameters,
            return_type,
            public,
            ..
        } = func;

        let mut sig = self.inner.make_signature();
        for (_, t) in parameters {
            sig.params.push(AbiParam::new(self.cranelift_type(t)));
        }
        sig.returns = vec![AbiParam::new(self.cranelift_type(return_type))];

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

        self.functions.insert(name.clone(), (func_id, sig));
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
            parameters,
            ..
        } = func;
        let (func_id, sig) = self.functions[name].clone();

        let mut ctx = self.inner.make_context();
        ctx.set_disasm(true);
        ctx.func.signature = sig.clone();

        let builder = FunctionBuilder::new(&mut ctx.func, builder_context);

        let mut func_gen = FuncGen {
            module: self,
            builder,
            block_map: HashMap::new(),
        };

        func_gen.entry_block(&blocks[0], parameters);

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
    fn cranelift_type(&mut self, ty: &RotoType) -> Type {
        let ty = self.type_info.resolve(ty);
        match ty {
            RotoType::Primitive(p) => match p {
                Primitive::U32 => I32,
                Primitive::U16 => I16,
                Primitive::U8 => I8,
                Primitive::I32 => I32,
                Primitive::I16 => I16,
                Primitive::I8 => I8,
                Primitive::Bool => I8,
                // todo!(): this is temporary: units need to be 0-sized and
                // compiled away
                Primitive::Unit => I8,
                Primitive::String => todo!(),
            },
            RotoType::IntVar(_) => I32,
            // TODO: The associated data needs to be fixed of course
            RotoType::Verdict(_, _) => I8,
            RotoType::Record(_)
            | RotoType::RecordVar(_, _)
            | RotoType::NamedRecord(_, _)
            | RotoType::Enum(_, _) => self.isa.pointer_type(),
            RotoType::Var(_) => todo!(),
            RotoType::ExplicitVar(_) => todo!(),
            RotoType::Never => todo!(),
            RotoType::BuiltIn(_, _) => todo!(),
            RotoType::List(_) => todo!(),
            RotoType::Table(_) => todo!(),
            RotoType::OutputStream(_) => todo!(),
            RotoType::Rib(_) => todo!(),
            RotoType::Term(_) => todo!(),
            RotoType::Action(_) => todo!(),
            RotoType::Filter(_) => todo!(),
            RotoType::FilterMap(_) => todo!(),
            RotoType::Name(_) => todo!(),
        }
    }
}

impl<'r, 'a, 'c> FuncGen<'r, 'a, 'c> {
    fn finalize(self) {
        self.builder.finalize()
    }

    /// Set up the entry block for the function
    fn entry_block(
        &mut self,
        block: &'a ir::Block,
        parameters: &'a [(String, RotoType)],
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
            self.def(self.module.variable_map[&x.as_ref()], val);
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

                let val = self.operand(examinee);
                switch.emit(&mut self.builder, val, otherwise);
            }
            ir::Instruction::Assign { to, val, ty } => {
                if self.size_of(ty) == 0 {
                    return;
                }
                let ty = self.module.cranelift_type(ty);
                let var = self.variable(&to.var, ty);
                let val = self.operand(val);
                self.def(var, val)
            }
            ir::Instruction::Call { to, ty, func, args } => {
                let (func_id, _) = self.module.functions[func];
                let func_ref = self
                    .module
                    .inner
                    .declare_func_in_func(func_id, self.builder.func);

                let args: Vec<_> =
                    args.iter().map(|(_, a)| self.operand(a)).collect();
                let inst = self.ins().call(func_ref, &args);

                let ty = self.module.cranelift_type(ty);
                let var = self.variable(&to.var, ty);
                self.def(var, self.builder.inst_results(inst)[0]);
            }
            ir::Instruction::CallExternal { .. } => todo!(),
            ir::Instruction::Return(v) => {
                let val = self.operand(v);
                self.ins().return_(&[val]);
            }
            ir::Instruction::Exit(v, _) => {
                // For now, exit is the same as return, because we can't
                // exit without a value
                let val = self.ins().iconst(I8, *v as i64);
                self.ins().return_(&[val]);
            }
            ir::Instruction::Cmp {
                to,
                cmp,
                left,
                right,
            } => {
                let l = self.operand(left);
                let r = self.operand(right);
                let var = self.variable(&to.var, I8);
                let val = self.binop(l, r, cmp);
                self.def(var, val);
            }
            ir::Instruction::Not { to, val } => {
                let val = self.operand(val);
                let var = self.variable(&to.var, I8);
                let val = self.ins().icmp_imm(IntCC::Equal, val, 0);
                self.def(var, val);
            }
            ir::Instruction::And { to, left, right } => {
                let l = self.operand(left);
                let r = self.operand(right);
                let var = self.variable(&to.var, I8);
                let val = self.ins().band(l, r);
                self.def(var, val);
            }
            ir::Instruction::Or { to, left, right } => {
                let l = self.operand(left);
                let r = self.operand(right);
                let var = self.variable(&to.var, I8);
                let val = self.ins().bor(l, r);
                self.def(var, val);
            }
            ir::Instruction::Eq { .. } => todo!(),
            ir::Instruction::AccessRecord {
                to,
                record,
                field,
                record_ty,
            } => {
                let (ty, offset) = self.offset_of(record_ty, field);
                let c_ty = self.module.cranelift_type(&ty);
                let var = self.variable(&to.var, c_ty);
                let record = self.operand(record);
                let val = self.read_field(record, &ty, offset as i32);
                self.def(var, val);
            }
            ir::Instruction::CreateRecord { to, fields, ty } => {
                let size = self.size_of(ty);
                let slot = self.builder.create_sized_stack_slot(
                    StackSlotData::new(StackSlotKind::ExplicitSlot, size),
                );

                for (field_name, field_operand) in fields {
                    let (ty, offset) = self.offset_of(ty, field_name);
                    self.write_field(slot, field_operand, &ty, offset as i32);
                }

                let pointer_ty = self.module.isa.pointer_type();
                let var = self.variable(&to.var, pointer_ty);

                // Records are represented by the address to their stack
                // slot. Hence, that's what we put in the variable.
                let p = self.ins().stack_addr(pointer_ty, slot, 0);
                self.def(var, p);
            }
            ir::Instruction::CreateEnum {
                to,
                variant,
                data,
                ty,
            } => {
                let size = self.size_of(ty);
                let slot = self.builder.create_sized_stack_slot(
                    StackSlotData::new(StackSlotKind::ExplicitSlot, size),
                );

                let variant_val = self.ins().iconst(I8, *variant as i64);
                self.ins().stack_store(variant_val, slot, 0);

                let RotoType::Enum(_, variants) = ty else {
                    panic!()
                };

                if let Some(data) = data {
                    let field_ty =
                        &variants[*variant as usize].1.as_ref().unwrap();
                    let offset = 1 + self.padding_of(field_ty, 1);

                    self.write_field(slot, data, field_ty, offset as i32);
                }

                let pointer_ty = self.module.isa.pointer_type();
                let var = self.variable(&to.var, pointer_ty);

                // Enums are represented by the address to their stack
                // slot. Hence, that's what we put in the variable.
                let p = self.ins().stack_addr(pointer_ty, slot, 0);
                self.def(var, p);
            }
            ir::Instruction::EnumDiscriminant { to, from } => {
                // The discriminant is the first byte of the enum, so offset
                // 0 and of type I8.
                let p = self.operand(from);
                let p = self.ins().load(I8, MEMFLAGS, p, 0);
                let var = self.variable(&to.var, I8);
                self.def(var, p);
            }
            ir::Instruction::AccessEnum { to, from, field_ty } => {
                let enum_val = self.operand(from);

                // The offset of the field is (at least) 1 because of the
                // discriminant.
                let offset = 1 + self.padding_of(field_ty, 1);
                let val = self.read_field(enum_val, field_ty, offset as i32);

                let ty = self.module.cranelift_type(field_ty);
                let var = self.variable(&to.var, ty);
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

    /// Write to a field of a compound data type
    fn write_field(
        &mut self,
        slot: StackSlot,
        op: &'a Operand,
        ty: &RotoType,
        offset: i32,
    ) {
        let op = self.operand(op);

        if is_reference_type(ty) {
            let pointer_ty = self.module.isa.pointer_type();
            let size = self.size_of(ty);

            let dest = self.ins().stack_addr(pointer_ty, slot, offset);

            self.builder.emit_small_memory_copy(
                self.module.isa.frontend_config(),
                dest,
                op,
                size as u64,
                0,
                0,
                true,
                MEMFLAGS,
            )
        } else {
            self.ins().stack_store(op, slot, offset);
        }
    }

    /// Read a field of a compound data type
    fn read_field(
        &mut self,
        addr: Value,
        ty: &RotoType,
        offset: i32,
    ) -> Value {
        if is_reference_type(ty) {
            self.ins().iadd_imm(addr, offset as i64)
        } else {
            let ty = self.module.cranelift_type(ty);
            self.ins().load(ty, MEMFLAGS, addr, offset)
        }
    }

    /// Return the [`FuncInstBuilder`] for the function builder
    fn ins<'short>(&'short mut self) -> FuncInstBuilder<'short, 'c> {
        self.builder.ins()
    }

    /// Define a variable with a value
    fn def(&mut self, var: Variable, val: Value) {
        self.builder.def_var(var, val);
    }

    fn operand(&mut self, val: &'a Operand) -> Value {
        let pointer_ty = self.module.isa.pointer_type();
        match val {
            ir::Operand::Place(p) => {
                let var = self.variable(&p.var, I8);
                self.builder.use_var(var)
            }
            ir::Operand::Value(v) => match v {
                SafeValue::Bool(x) => self.ins().iconst(I8, *x as i64),
                SafeValue::U8(x) => self.ins().iconst(I8, *x as i64),
                SafeValue::U16(x) => self.ins().iconst(I16, *x as i64),
                SafeValue::U32(x) => self.ins().iconst(I32, *x as i64),
                SafeValue::I8(x) => self.ins().iconst(I8, *x as i64),
                SafeValue::I16(x) => self.ins().iconst(I16, *x as i64),
                SafeValue::I32(x) => self.ins().iconst(I32, *x as i64),
                SafeValue::Runtime(x) => self
                    .ins()
                    .iconst(pointer_ty, Rc::as_ptr(x) as *const () as i64),
                SafeValue::Unit => todo!(),
                SafeValue::Verdict(_) => todo!(),
                SafeValue::Record(_) => todo!(),
                SafeValue::Enum(_, _) => todo!(),
            },
        }
    }

    fn variable(&mut self, var: &'a str, ty: Type) -> Variable {
        let len = self.module.variable_map.len();
        *self.module.variable_map.entry(var).or_insert_with(|| {
            let var = Variable::new(len);
            self.builder.declare_var(var, ty);
            var
        })
    }

    fn size_of(&mut self, t: &RotoType) -> u32 {
        let pointer_bytes = self.module.isa.pointer_bytes();
        self.module.type_info.size_of(t, pointer_bytes as u32)
    }

    fn padding_of(&mut self, t: &RotoType, offset: u32) -> u32 {
        let pointer_bytes = self.module.isa.pointer_bytes();
        self.module
            .type_info
            .padding_of(t, offset, pointer_bytes as u32)
    }

    fn offset_of(
        &mut self,
        record: &RotoType,
        field: &str,
    ) -> (RotoType, u32) {
        let pointer_bytes = self.module.isa.pointer_bytes();
        self.module
            .type_info
            .offset_of(record, field, pointer_bytes as u32)
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
    pub fn get_function<Params: RotoParams, Return: IsRotoType>(
        &self,
        name: &str,
    ) -> Option<TypedFunc<Params, Return>> {
        let (id, sig) = self.functions.get(name)?;
        let correct_params = Params::check(
            &sig.params.iter().map(|p| p.value_type).collect::<Vec<_>>(),
        );
        let correct_return = Return::check(sig.returns[0].value_type);
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

/// Record and enums are passed by reference
fn is_reference_type(t: &RotoType) -> bool {
    matches!(
        t,
        RotoType::Record(..)
            | RotoType::RecordVar(..)
            | RotoType::NamedRecord(..)
            | RotoType::Enum(..)
    )
}

pub trait IsRotoType {
    fn check(ty: Type) -> bool {
        ty == I8
    }
}

impl IsRotoType for i8 {
    fn check(ty: Type) -> bool {
        ty == I8
    }
}

impl IsRotoType for u8 {
    fn check(ty: Type) -> bool {
        ty == I8
    }
}

impl IsRotoType for i16 {
    fn check(ty: Type) -> bool {
        ty == I16
    }
}

impl IsRotoType for u16 {
    fn check(ty: Type) -> bool {
        ty == I16
    }
}

impl IsRotoType for i32 {
    fn check(ty: Type) -> bool {
        ty == I32
    }
}

impl IsRotoType for u32 {
    fn check(ty: Type) -> bool {
        ty == I32
    }
}

pub trait RotoParams {
    fn check(ty: &[Type]) -> bool {
        ty.is_empty()
    }

    unsafe fn invoke<R>(func_ptr: *const u8, params: Self) -> R;
}

impl RotoParams for () {
    fn check(ty: &[Type]) -> bool {
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
    A1: IsRotoType,
{
    fn check(ty: &[Type]) -> bool {
        let &[ty] = ty else {
            return false;
        };
        A1::check(ty)
    }

    unsafe fn invoke<R>(func_ptr: *const u8, (a1,): Self) -> R {
        let func_ptr =
            unsafe { std::mem::transmute::<_, fn(A1) -> R>(func_ptr) };
        func_ptr(a1)
    }
}
