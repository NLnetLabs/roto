//! Machine code generation via cranelift

use std::{collections::HashMap, marker::PhantomData, sync::Arc};

use cranelift::{
    codegen::{
        entity::EntityRef,
        ir::{
            condcodes::IntCC, types::*, AbiParam, Block, InstBuilder,
            Signature, Value,
        },
        settings::{self, Configurable as _},
    },
    frontend::{FunctionBuilder, FunctionBuilderContext, Switch, Variable},
};
use cranelift_codegen::{
    ir::{MemFlags, StackSlotData, StackSlotKind},
    isa::TargetIsa,
    trace,
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module as _};

use crate::{
    lower::ir::{self, IntCmp, Operand},
    typechecker::{
        types::{Primitive, Type as RotoType},
        TypeInfo,
    },
    Runtime, SafeValue,
};

#[cfg(test)]
mod tests;

/// A compiled, ready-to-run Roto module
pub struct Module {
    /// The set of public functions and their signatures.
    functions: HashMap<String, (FuncId, Signature)>,
    /// The inner cranelift module
    inner: JITModule,
}

pub struct TypedFunc<P, R> {
    func: *const u8,
    _ty: PhantomData<(P, R)>,
}

impl<P: RotoParams, R: IsRotoType> TypedFunc<P, R> {
    pub fn call(&self, params: P) -> R {
        unsafe { P::invoke::<R>(self.func, params) }
    }
}

struct ModuleBuilder<'r> {
    /// The set of public functions and their signatures.
    functions: HashMap<String, (FuncId, Signature)>,
    /// The inner cranelift module
    inner: JITModule,
    type_info: &'r mut TypeInfo,
    isa: Arc<dyn TargetIsa>,
}

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

    // Our functions might call each other, so we declare them before we
    // define them. This is also when we start building the function
    // hashmap.
    let mut module = ModuleBuilder {
        functions: HashMap::new(),
        inner: jit,
        type_info,
        isa,
    };

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

    fn define_function(
        &mut self,
        func: &ir::Function,
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

        let mut builder =
            FunctionBuilder::new(&mut ctx.func, builder_context);

        let mut block_map = HashMap::new();
        let mut variable_map = HashMap::<&str, _>::new();

        let entry_block = block_map
            .entry(blocks[0].label.as_ref())
            .or_insert_with(|| builder.create_block());

        builder.switch_to_block(*entry_block);

        for (x, t) in parameters.iter() {
            let len = variable_map.len();
            let _ = *variable_map.entry(x.as_ref()).or_insert_with(|| {
                let var = Variable::new(len);
                builder.declare_var(var, self.cranelift_type(t));
                var
            });
        }

        builder.append_block_params_for_function_params(*entry_block);

        let params = builder.block_params(*entry_block).to_owned();
        for ((x, _), p) in parameters.iter().zip(params) {
            builder.def_var(variable_map[&x.as_ref()], p);
        }

        for block in blocks {
            let b = block_map
                .entry(block.label.as_ref())
                .or_insert_with(|| builder.create_block());
            builder.switch_to_block(*b);
            builder.seal_block(*b);
            self.compile_block(
                &mut block_map,
                &mut variable_map,
                &mut builder,
                block,
            );
        }

        builder.finalize();

        self.inner.define_function(func_id, &mut ctx).unwrap();

        let capstone = self.isa.to_capstone().unwrap();
        trace!(
            "{}",
            ctx.compiled_code()
                .unwrap()
                .disassemble(None, &capstone)
                .unwrap()
        );
        self.inner.clear_context(&mut ctx);
    }

    fn compile_block<'a>(
        &mut self,
        block_map: &mut HashMap<&'a str, Block>,
        variable_map: &mut HashMap<&'a str, Variable>,
        builder: &mut FunctionBuilder,
        block: &'a ir::Block,
    ) {
        for instruction in &block.instructions {
            match instruction {
                ir::Instruction::Jump(label) => {
                    let block = block_map
                        .entry(label)
                        .or_insert_with(|| builder.create_block());
                    builder.ins().jump(*block, &[]);
                }
                ir::Instruction::Switch {
                    examinee,
                    branches,
                    default,
                } => {
                    let mut switch = Switch::new();

                    for (idx, label) in branches {
                        let block = block_map
                            .entry(label)
                            .or_insert_with(|| builder.create_block());
                        switch.set_entry(*idx as u128, *block);
                    }

                    let otherwise = block_map
                        .entry(default)
                        .or_insert_with(|| builder.create_block());

                    let val =
                        self.compile_operand(variable_map, builder, examinee);
                    switch.emit(builder, val, *otherwise);
                }
                ir::Instruction::Assign { to, val, ty } => {
                    let len = variable_map.len();
                    let var =
                        *variable_map.entry(&to.var).or_insert_with(|| {
                            let var = Variable::new(len);
                            builder.declare_var(var, self.cranelift_type(ty));
                            var
                        });
                    let val =
                        self.compile_operand(variable_map, builder, val);
                    builder.def_var(var, val)
                }
                ir::Instruction::Call { to, ty, func, args } => {
                    let (func_id, _) = self.functions[func];
                    let func_ref = self
                        .inner
                        .declare_func_in_func(func_id, builder.func);

                    let args: Vec<_> = args
                        .iter()
                        .map(|(_, a)| {
                            self.compile_operand(variable_map, builder, a)
                        })
                        .collect();
                    let inst = builder.ins().call(func_ref, &args);

                    let len = variable_map.len();
                    let var =
                        *variable_map.entry(&to.var).or_insert_with(|| {
                            let var = Variable::new(len);
                            builder.declare_var(var, self.cranelift_type(ty));
                            var
                        });
                    builder.def_var(var, builder.inst_results(inst)[0]);
                }
                ir::Instruction::CallExternal { .. } => todo!(),
                ir::Instruction::Return(v) => {
                    let val = self.compile_operand(variable_map, builder, v);
                    builder.ins().return_(&[val]);
                }
                ir::Instruction::Exit(v, _) => {
                    let val = builder.ins().iconst(I8, *v as i64);
                    builder.ins().return_(&[val]);
                }
                ir::Instruction::Cmp {
                    to,
                    cmp,
                    left,
                    right,
                } => {
                    let l = self.compile_operand(variable_map, builder, left);
                    let r =
                        self.compile_operand(variable_map, builder, right);
                    let len = variable_map.len();
                    let var =
                        *variable_map.entry(&to.var).or_insert_with(|| {
                            let var = Variable::new(len);
                            builder.declare_var(var, I8);
                            var
                        });
                    let val = compile_binop(builder, l, r, cmp);
                    builder.def_var(var, val);
                }
                ir::Instruction::Not { to, val } => {
                    let val =
                        self.compile_operand(variable_map, builder, val);
                    let len = variable_map.len();
                    let var =
                        *variable_map.entry(&to.var).or_insert_with(|| {
                            let var = Variable::new(len);
                            builder.declare_var(var, I8);
                            var
                        });
                    let val = builder.ins().icmp_imm(IntCC::Equal, val, 0);
                    builder.def_var(var, val);
                }
                ir::Instruction::And { to, left, right } => {
                    let l = self.compile_operand(variable_map, builder, left);
                    let r =
                        self.compile_operand(variable_map, builder, right);
                    let len = variable_map.len();
                    let var =
                        *variable_map.entry(&to.var).or_insert_with(|| {
                            let var = Variable::new(len);
                            builder.declare_var(var, I8);
                            var
                        });
                    let val = builder.ins().band(l, r);
                    builder.def_var(var, val);
                }
                ir::Instruction::Or { to, left, right } => {
                    let l = self.compile_operand(variable_map, builder, left);
                    let r =
                        self.compile_operand(variable_map, builder, right);
                    let len = variable_map.len();
                    let var =
                        *variable_map.entry(&to.var).or_insert_with(|| {
                            let var = Variable::new(len);
                            builder.declare_var(var, I8);
                            var
                        });
                    let val = builder.ins().bor(l, r);
                    builder.def_var(var, val);
                }
                ir::Instruction::Eq { .. } => todo!(),
                ir::Instruction::AccessRecord {
                    to,
                    record,
                    field,
                    record_ty,
                } => {
                    let pointer_bytes = self.isa.pointer_bytes() as u32;

                    let (ty, offset) = self.type_info.offset_of(
                        record_ty,
                        field,
                        pointer_bytes,
                    );

                    let len = variable_map.len();
                    let var =
                        *variable_map.entry(&to.var).or_insert_with(|| {
                            let var = Variable::new(len);
                            builder
                                .declare_var(var, self.cranelift_type(&ty));
                            var
                        });

                    let record =
                        self.compile_operand(variable_map, builder, record);

                    let val = if let RotoType::Record(..)
                    | RotoType::NamedRecord(..)
                    | RotoType::RecordVar(..) = ty
                    {
                        builder.ins().iadd_imm(record, offset as i64)
                    } else {
                        builder.ins().load(
                            self.cranelift_type(&ty),
                            MemFlags::new().with_aligned(),
                            record,
                            offset as i32,
                        )
                    };

                    builder.def_var(var, val);
                }
                ir::Instruction::CreateRecord { to, fields, ty } => {
                    let pointer_bytes = self.isa.pointer_bytes() as u32;
                    let pointer_ty = self.isa.pointer_type();
                    let size = self.type_info.size_of(ty, pointer_bytes);
                    let slot_data =
                        StackSlotData::new(StackSlotKind::ExplicitSlot, size);
                    let slot = builder.create_sized_stack_slot(slot_data);

                    for (field_name, field_operand) in fields {
                        let (ty, offset) = self.type_info.offset_of(
                            ty,
                            field_name,
                            pointer_bytes,
                        );

                        let op = self.compile_operand(
                            variable_map,
                            builder,
                            field_operand,
                        );

                        if let RotoType::Record(..)
                        | RotoType::NamedRecord(..)
                        | RotoType::RecordVar(..) = ty
                        {
                            let size =
                                self.type_info.size_of(&ty, pointer_bytes);

                            let dest = builder.ins().stack_addr(
                                pointer_ty,
                                slot,
                                offset as i32,
                            );

                            builder.emit_small_memory_copy(
                                self.isa.frontend_config(),
                                dest,
                                op,
                                size as u64,
                                0,
                                0,
                                true,
                                MemFlags::new().with_aligned(),
                            )
                        } else {
                            builder.ins().stack_store(
                                op,
                                slot,
                                offset as i32,
                            );
                        }
                    }

                    let len = variable_map.len();
                    let var =
                        *variable_map.entry(&to.var).or_insert_with(|| {
                            let var = Variable::new(len);
                            builder.declare_var(var, pointer_ty);
                            var
                        });

                    let p = builder.ins().stack_addr(pointer_ty, slot, 0);
                    builder.def_var(var, p);
                }
                ir::Instruction::CreateEnum { .. } => todo!(),
                ir::Instruction::AccessEnum { .. } => todo!(),
            }
        }
    }

    fn compile_operand<'a>(
        &self,
        variable_map: &mut HashMap<&'a str, Variable>,
        builder: &mut FunctionBuilder,
        val: &'a Operand,
    ) -> Value {
        match val {
            ir::Operand::Place(p) => {
                let len = variable_map.len();
                let var = *variable_map.entry(&p.var).or_insert_with(|| {
                    let var = Variable::new(len);
                    builder.declare_var(var, I8);
                    var
                });
                builder.use_var(var)
            }
            ir::Operand::Value(v) => match v {
                SafeValue::Unit => todo!(),
                SafeValue::Bool(x) => builder.ins().iconst(I8, *x as i64),
                SafeValue::U8(x) => builder.ins().iconst(I8, *x as i64),
                SafeValue::U16(x) => builder.ins().iconst(I16, *x as i64),
                SafeValue::U32(x) => builder.ins().iconst(I32, *x as i64),
                SafeValue::I8(x) => builder.ins().iconst(I8, *x as i64),
                SafeValue::I16(x) => builder.ins().iconst(I16, *x as i64),
                SafeValue::I32(x) => builder.ins().iconst(I32, *x as i64),
                SafeValue::Verdict(_) => todo!(),
                SafeValue::Record(_) => todo!(),
                SafeValue::Enum(_, _) => todo!(),
                SafeValue::Runtime(_) => todo!(),
            },
        }
    }

    fn finalize(mut self) -> Module {
        self.inner.finalize_definitions().unwrap();
        Module {
            functions: self.functions,
            inner: self.inner,
        }
    }

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
            // TODO: The associated data needs to be fixed of course
            RotoType::Verdict(_, _) => I8,
            RotoType::Var(_) => todo!(),
            RotoType::ExplicitVar(_) => todo!(),
            RotoType::IntVar(_) => todo!(),
            RotoType::RecordVar(_, _) => todo!(),
            RotoType::Never => todo!(),
            RotoType::BuiltIn(_, _) => todo!(),
            RotoType::List(_) => todo!(),
            RotoType::Table(_) => todo!(),
            RotoType::OutputStream(_) => todo!(),
            RotoType::Rib(_) => todo!(),
            RotoType::Record(_) => todo!(),
            RotoType::NamedRecord(_, _) => self.isa.pointer_type(),
            RotoType::Enum(_, _) => todo!(),
            RotoType::Term(_) => todo!(),
            RotoType::Action(_) => todo!(),
            RotoType::Filter(_) => todo!(),
            RotoType::FilterMap(_) => todo!(),
            RotoType::Name(_) => todo!(),
        }
    }
}

impl Module {
    pub fn get_function<P: RotoParams, R: IsRotoType>(
        &self,
        name: &str,
    ) -> Option<TypedFunc<P, R>> {
        let (id, sig) = self.functions.get(name)?;
        let correct_params = P::check(
            &sig.params.iter().map(|p| p.value_type).collect::<Vec<_>>(),
        );
        let correct_return = R::check(sig.returns[0].value_type);
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

fn compile_binop(
    builder: &mut FunctionBuilder,
    left_val: Value,
    right_val: Value,
    op: &IntCmp,
) -> Value {
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
    builder.ins().icmp(cc, left_val, right_val)
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
