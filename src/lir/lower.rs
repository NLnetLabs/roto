mod clone_drop;

use std::collections::HashMap;

use crate::{
    ast::{self, Identifier, Literal},
    ice,
    label::{LabelRef, LabelStore},
    mir,
    runtime::{
        init_string,
        layout::{Layout, LayoutBuilder},
        RuntimeFunctionRef,
    },
    typechecker::{
        info::TypeInfo,
        scope::{ResolvedName, ScopeRef},
        scoped_display::TypeDisplay,
        types::{
            self, EnumVariant, FloatSize, IntKind, IntSize, Primitive, Type,
            TypeDefinition,
        },
    },
    IrValue, Runtime,
};

use super::{
    value::IrType, Block, FloatCmp, Function, Instruction, IntCmp, Lir,
    Operand, Signature, ValueOrSlot, Var, VarKind,
};

pub fn lower_to_lir(ctx: &mut LowerCtx<'_>, mir: mir::Mir) -> Lir {
    Lowerer::program(ctx, mir)
}

pub struct LowerCtx<'c> {
    pub runtime: &'c Runtime,
    pub type_info: &'c mut TypeInfo,
    pub label_store: &'c mut LabelStore,
    pub runtime_functions: &'c mut HashMap<RuntimeFunctionRef, Signature>,
}

struct Lowerer<'c, 'r> {
    ctx: &'c mut LowerCtx<'r>,
    blocks: Vec<Block>,
    function_scope: ScopeRef,
    tmp_idx: usize,
    return_type: Type,
    variables: Vec<(Var, ValueOrSlot)>,
}

/// A location is a lot like a [`mir::Place`], but it makes the distinction
/// that a reference type (which is placed in a stack slot) always gets the
/// `Pointer` variant. If the `mir::Place` has any projections, the location
/// is also a `Pointer`, because it then represents some offset in a stack
/// slot.
///
/// So we get the following mapping:
///
/// - a variable of a value type becomes `Location::Var`.
/// - a variable of a reference type becomes `Location::Pointer` with offset 0
/// - a variable of any type with some projection becomes a `Location::Pointer`
///   with some offset.
enum Location {
    Var(Var),
    Pointer { base: Var, offset: usize },
}

/// # Lower MIR constructs
impl Lowerer<'_, '_> {
    fn program(ctx: &mut LowerCtx<'_>, mir: mir::Mir) -> Lir {
        let mut functions = Vec::new();

        for function in mir.functions {
            functions.push(Self::function(ctx, function));
        }

        Lir { functions }
    }

    fn function(ctx: &mut LowerCtx<'_>, function: mir::Function) -> Function {
        let return_type = function.signature.return_type.clone();
        let mut lowerer = Lowerer {
            ctx,
            tmp_idx: function.tmp_idx,
            function_scope: function.scope,
            return_type: return_type.clone(),
            blocks: Vec::new(),
            variables: Vec::new(),
        };
        let name = function.name;
        let signature = function.signature;

        lowerer.variables = function
            .variables
            .iter()
            .filter_map(|(v, ty)| {
                // The MIR can emit unresolved types in places where the never
                // type is used. The instructions for those are eliminated by
                // dead code elimination.
                let ty = lowerer.ctx.type_info.resolve(ty);
                if matches!(&ty, Type::Var(_)) {
                    return None;
                }

                let lir_v = lowerer.var(v.clone());
                let Some(is_reference_type) = lowerer.is_reference_type(&ty)
                else {
                    ice!("Need an inhabited type");
                };
                let var_type = if is_reference_type {
                    // Parameters don't need a slot because they already
                    // live somewhere.
                    if function.parameters.contains(v) {
                        ValueOrSlot::Val(IrType::Pointer)
                    } else {
                        let Some(layout) = lowerer
                            .ctx
                            .type_info
                            .layout_of(&ty, lowerer.ctx.runtime)
                        else {
                            ice!("Need an inhabited type");
                        };
                        ValueOrSlot::StackSlot(layout)
                    }
                } else {
                    ValueOrSlot::Val(lowerer.lower_type(&ty)?)
                };
                Some((lir_v, var_type))
            })
            .collect::<Vec<_>>();

        for block in function.blocks {
            lowerer.block(block);
        }

        let entry_block = lowerer.blocks[0].label;

        let (return_ir_type, return_ptr) =
            match lowerer.is_reference_type(&return_type) {
                Some(true) => (None, true),
                Some(false) => (lowerer.lower_type(&return_type), false),
                None => (None, false),
            };

        let ir_signature = Signature {
            parameters: function
                .parameters
                .iter()
                .zip(&signature.parameter_types)
                .filter_map(|(def, ty)| {
                    let ty = lowerer.lower_type(ty)?;
                    let mir::VarKind::Explicit(x) = def.kind else {
                        ice!()
                    };
                    Some((x, ty))
                })
                .collect(),
            return_ptr,
            return_type: return_ir_type,
        };

        Function {
            name,
            blocks: lowerer.blocks,
            variables: lowerer.variables,
            signature,
            scope: function.scope,
            ir_signature,
            entry_block,
            public: true,
        }
    }

    fn block(&mut self, block: mir::Block) {
        self.blocks.push(Block {
            label: block.label,
            instructions: Vec::new(),
        });

        for instruction in block.instructions {
            self.instruction(instruction)
        }
    }

    fn instruction(&mut self, instruction: mir::Instruction) {
        match instruction {
            mir::Instruction::Assign { to, ty, value } => {
                self.assign(to, ty, value)
            }
            mir::Instruction::Jump(lbl) => {
                self.emit_jump(lbl);
            }
            mir::Instruction::Switch {
                examinee,
                branches,
                default,
            } => self.switch(examinee, branches, default),
            mir::Instruction::SetDiscriminant { to, ty, variant } => {
                self.set_discriminant(to, &ty, &variant)
            }
            mir::Instruction::Return { var } => self.r#return(var),
            mir::Instruction::Drop { val, ty } => self.drop(val, ty),
        }
    }

    fn assign(&mut self, to: mir::Place, ty: Type, value: mir::Value) {
        let to = self.location(to, ty.clone());
        let op = match value {
            mir::Value::Const(lit, ty) => {
                let Some(op) = self.literal(&lit, &ty) else {
                    return;
                };
                op
            }
            mir::Value::Constant(name, ty) => {
                let ptr_var = self.new_tmp(IrType::Pointer);
                self.emit_constant_address(ptr_var.clone(), name);

                if let Some(to) = to {
                    self.clone_place(
                        to,
                        Location::Pointer {
                            base: ptr_var,
                            offset: 0,
                        },
                        &ty,
                    );
                }
                return;
            }
            mir::Value::Context(x) => {
                let from = Location::Pointer {
                    base: Var {
                        scope: self.function_scope,
                        kind: VarKind::Context,
                    },
                    offset: x,
                };
                if let Some(to) = to {
                    self.clone_place(to, from, &ty);
                }
                return;
            }
            mir::Value::Discriminant(v) => self.get_discriminant(v),
            mir::Value::Not(var) => {
                let val = self.var(var);
                let tmp = self.new_tmp(IrType::Bool);
                self.emit_not(tmp.clone(), val.into());
                tmp.into()
            }
            mir::Value::Negate(var, ty) => {
                let val = self.var(var);
                let ty = self.lower_type(&ty).unwrap();
                let tmp = self.new_tmp(ty);
                self.emit_negate(tmp.clone(), val.into());
                tmp.into()
            }
            mir::Value::Move(var) => self.var(var).into(),
            mir::Value::Clone(place) => {
                let from = self.location(place, ty.clone());
                if let (Some(to), Some(from)) = (to, from) {
                    self.clone_place(to, from, &ty);
                }
                return;
            }
            mir::Value::BinOp {
                left,
                binop,
                ty,
                right,
            } => self.binop(left, binop, ty, right),
            mir::Value::Call { func, args } => {
                let Some(op) = self.call(func, args, &ty) else {
                    return;
                };
                op
            }
            mir::Value::CallRuntime { func_ref, args } => {
                let Some(op) = self.call_runtime(func_ref, args, &ty) else {
                    return;
                };
                op
            }
        };

        // There are valid assignments in MIR that have the never type. For
        // example, a function call that returns the never type. So we cannot
        // unwrap `to` here, but we will simply only assign the value if it is
        // inhabited.
        if let Some(to) = to {
            self.move_val(to, op, &ty);
        }
    }

    fn call(
        &mut self,
        func: ResolvedName,
        args: Vec<mir::Var>,
        return_type: &Type,
    ) -> Option<Operand> {
        let reference_return = self.is_reference_type(return_type);
        let (to, out_ptr) = match reference_return {
            Some(true) => {
                let layout = self
                    .ctx
                    .type_info
                    .layout_of(return_type, self.ctx.runtime)
                    .unwrap();
                let out_ptr = self.new_stack_slot(layout);
                (None, Some(out_ptr))
            }
            Some(false) => {
                let to = self
                    .lower_type(return_type)
                    .map(|ty| (self.new_tmp(ty), ty));
                (to, None)
            }
            None => (None, None),
        };

        let ctx = Var {
            scope: self.function_scope,
            kind: VarKind::Context,
        };

        let func = self.ctx.type_info.full_name(&func);

        let args = args.into_iter().map(|v| self.var(v).into()).collect();

        self.emit(Instruction::Call {
            to: to.clone(),
            ctx: ctx.into(),
            func,
            args,
            return_ptr: out_ptr.clone(),
        });

        if let Some(out_ptr) = out_ptr {
            Some(out_ptr.into())
        } else {
            to.map(|(to, _ty)| to.into())
        }
    }

    fn call_runtime(
        &mut self,
        func_ref: RuntimeFunctionRef,
        args: Vec<mir::Var>,
        return_type: &Type,
    ) -> Option<Operand> {
        let layout = self
            .ctx
            .type_info
            .layout_of(return_type, self.ctx.runtime)
            .unwrap_or_else(|| Layout::new(0, 1));
        let out_ptr = self.new_stack_slot(layout);

        let mut parameters = Vec::new();
        parameters.push(("ret".into(), IrType::Pointer));

        let sig = self.ctx.type_info.runtime_function_signature(func_ref);
        parameters.extend(sig.parameter_types.iter().enumerate().filter_map(
            |(i, ty)| Some((i.to_string().into(), self.lower_type(ty)?)),
        ));

        let ir_signature = Signature {
            parameters,
            return_ptr: true,
            return_type: None,
        };

        let args = std::iter::once(Operand::Place(out_ptr.clone()))
            .chain(args.iter().map(|a| self.var(a.clone()).into()))
            .collect();

        self.ctx.runtime_functions.insert(func_ref, ir_signature);

        self.emit(Instruction::CallRuntime {
            func: func_ref,
            args,
        });

        if self.is_reference_type(return_type)? {
            Some(out_ptr.into())
        } else {
            let ty = self.lower_type(return_type)?;
            let tmp = self.new_tmp(ty);
            self.emit_read(tmp.clone(), out_ptr.into(), ty);
            Some(tmp.into())
        }
    }

    fn move_val(&mut self, to: Location, val: Operand, ty: &Type) {
        let Some(ir_ty) = self.lower_type(ty) else {
            return;
        };

        match to {
            Location::Var(to) => {
                self.emit_assign(to, val, ir_ty);
            }
            Location::Pointer { base, offset } => {
                let to = self.offset(base.into(), offset as u32);

                match self.is_reference_type(ty) {
                    Some(true) => {
                        let size = self
                            .ctx
                            .type_info
                            .layout_of(ty, self.ctx.runtime)
                            .unwrap()
                            .size();
                        self.emit_memcpy(to, val, size as u32);
                    }
                    Some(false) => self.emit_write(to, val),
                    None => {}
                }
            }
        }
    }

    fn clone_place(&mut self, to: Location, from: Location, ty: &Type) {
        match (to, from) {
            // This is a not-by-reference type so we'll just assign it.
            (Location::Var(to), Location::Var(from)) => {
                let Some(ty) = self.lower_type(ty) else {
                    return;
                };
                self.emit(Instruction::Assign {
                    to,
                    val: from.into(),
                    ty,
                })
            }
            // We read a not-by-reference type from a field
            (Location::Var(to), Location::Pointer { base, offset }) => {
                let Some(ty) = self.lower_type(ty) else {
                    return;
                };
                let from = self.offset(base.into(), offset as u32);
                self.emit_read(to, from, ty)
            }
            // We write a not-by-reference type to a field
            (Location::Pointer { base, offset }, Location::Var(from)) => {
                let Some(_ty) = self.lower_type(ty) else {
                    return;
                };
                let to = self.offset(base.into(), offset as u32);
                self.emit_write(to, from.into());
            }
            (
                Location::Pointer {
                    base: base_to,
                    offset: offset_to,
                },
                Location::Pointer {
                    base: base_from,
                    offset: offset_from,
                },
            ) => {
                let from = self.offset(base_from.into(), offset_from as u32);
                let to = self.offset(base_to.into(), offset_to as u32);
                self.clone_type(from, to, ty);
            }
        }
    }

    /// Returns `None` if the type uninhabited
    fn location(&mut self, place: mir::Place, ty: Type) -> Option<Location> {
        let root_ty = place.root_ty;
        if place.projection.is_empty() {
            if self
                .ctx
                .type_info
                .is_reference_type(&ty, self.ctx.runtime)?
            {
                Some(Location::Pointer {
                    base: self.var(place.var),
                    offset: 0,
                })
            } else {
                Some(Location::Var(self.var(place.var)))
            }
        } else {
            let base = self.var(place.var);

            let mut offset = 0;
            let mut ty = root_ty;
            for p in place.projection {
                match p {
                    mir::Projection::Field(ident) => {
                        let (new_offset, new_ty) = self.get_field(&ty, ident);
                        ty = new_ty;
                        offset += new_offset;
                    }
                    mir::Projection::VariantField(variant_name, n) => {
                        let Type::Name(type_name) = &ty else { ice!() };
                        let TypeDefinition::Enum(type_constructor, variants) =
                            self.ctx.type_info.resolve_type_name(type_name)
                        else {
                            ice!()
                        };

                        let subs: Vec<_> = type_constructor
                            .arguments
                            .iter()
                            .zip(&type_name.arguments)
                            .collect();

                        let mut builder = LayoutBuilder::new();
                        builder.add(&Layout::of::<u8>());

                        let variant = variants
                            .iter()
                            .find(|v| v.name == variant_name)
                            .unwrap();

                        let mut last_ty = None;
                        let mut new_offset = 0;
                        for field_ty in variant.fields.iter().take(n + 1) {
                            let field_ty = field_ty.substitute_many(&subs);
                            new_offset =
                                builder.add(&self.ctx.type_info.layout_of(
                                    &field_ty,
                                    self.ctx.runtime,
                                )?);
                            last_ty = Some(field_ty);
                        }

                        ty = last_ty.unwrap();
                        offset += new_offset;
                    }
                }
            }
            Some(Location::Pointer { base, offset })
        }
    }

    fn get_field(&mut self, ty: &Type, ident: Identifier) -> (usize, Type) {
        let res_ty = self.ctx.type_info.resolve(ty);

        let fields = match res_ty {
            Type::Name(name) => {
                let TypeDefinition::Record(_, fields) =
                    self.ctx.type_info.resolve_type_name(&name)
                else {
                    ice!()
                };
                fields
            }
            Type::Record(fields) | Type::RecordVar(_, fields) => fields,
            _ => {
                ice!(
                    "Cannot get field {ident} of type {}",
                    ty.display(self.ctx.type_info)
                )
            }
        };

        let mut builder = LayoutBuilder::new();
        for (field, new_ty) in fields {
            let offset = builder.add(
                &self
                    .ctx
                    .type_info
                    .layout_of(&new_ty, self.ctx.runtime)
                    .unwrap(),
            );
            if *field == ident {
                return (offset, new_ty);
            }
        }

        ice!("Field not found: {ident}!")
    }

    fn switch(
        &mut self,
        examinee: mir::Var,
        mut branches: Vec<(usize, LabelRef)>,
        default: Option<LabelRef>,
    ) {
        let examinee = self.var(examinee);
        // TODO: Switch should probably have a var as argument
        if let Some(default) = default {
            self.emit_switch(examinee.into(), branches, default);
        } else {
            let (_, last) = branches.pop().unwrap();
            self.emit_switch(examinee.into(), branches, last);
        }
    }

    fn get_discriminant(&mut self, from: mir::Var) -> Operand {
        let from = self.var(from);
        let tmp = self.new_tmp(IrType::U8);
        self.emit_read(tmp.clone(), from.into(), IrType::U8);
        tmp.into()
    }

    fn set_discriminant(
        &mut self,
        to: mir::Var,
        ty: &Type,
        variant: &EnumVariant,
    ) {
        let to = self.var(to);
        let Type::Name(type_name) = ty else { ice!() };
        let TypeDefinition::Enum(_, variants) =
            self.ctx.type_info.resolve_type_name(type_name)
        else {
            ice!()
        };
        let idx = variants
            .iter()
            .position(|v| v.name == variant.name)
            .unwrap();
        self.emit_write(to.into(), Operand::Value(IrValue::U8(idx as u8)));
    }

    fn r#return(&mut self, var: mir::Var) {
        let var = self.var(var);

        let layout = self
            .ctx
            .type_info
            .layout_of(&self.return_type, self.ctx.runtime);

        if layout.as_ref().map_or(true, |l| l.size() == 0) {
            self.emit_return(None);
            return;
        }

        if self
            .ctx
            .type_info
            .is_reference_type(&self.return_type, self.ctx.runtime)
            .unwrap()
        {
            self.emit_memcpy(
                Var {
                    scope: self.function_scope,
                    kind: VarKind::Return,
                }
                .into(),
                var.into(),
                layout.unwrap().size() as u32,
            );
            self.emit_return(None);
        } else {
            self.emit_return(Some(var.into()));
        }
    }

    fn drop(&mut self, val: mir::Place, ty: Type) {
        let Some(var) = self.location(val, ty.clone()) else {
            return;
        };

        // Any reference type (and therefore any type that needs drop)
        // has a pointer location, we can ignore the rest.
        match var {
            Location::Var(_var) => {}
            Location::Pointer { base, offset } => {
                let op = self.offset(base.into(), offset as u32);
                self.drop_type(op, ty);
            }
        };
    }

    fn var(&self, to: mir::Var) -> Var {
        Var {
            scope: to.scope,
            kind: match to.kind {
                mir::VarKind::Explicit(identifier) => {
                    VarKind::Explicit(identifier)
                }
                mir::VarKind::Tmp(x) => VarKind::Tmp(x),
            },
        }
    }

    /// Lower a literal
    fn literal(&mut self, lit: &Literal, ty: &Type) -> Option<Operand> {
        Some(match &lit {
            Literal::String(s) => {
                let layout = Primitive::String.layout();
                let to = self.new_stack_slot(layout);
                self.emit(Instruction::InitString {
                    to: to.clone(),
                    string: s.clone(),
                    init_func: init_string,
                });

                to.into()
            }
            Literal::Asn(n) => IrValue::Asn(*n).into(),
            Literal::IpAddress(addr) => {
                const LAYOUT: Layout = Primitive::IpAddr.layout();
                let to = self.new_stack_slot(LAYOUT);

                const SIZE: usize = LAYOUT.size();
                let x: [u8; SIZE] = unsafe { std::mem::transmute_copy(addr) };

                self.emit(Instruction::Initialize {
                    to: to.clone(),
                    bytes: x.into(),
                    layout: Primitive::IpAddr.layout(),
                });
                to.into()
            }
            Literal::Integer(x) => {
                match ty {
                    Type::IntVar(_, _) => {
                        return Some(IrValue::I32(*x as i32).into())
                    }
                    Type::Name(type_name) => {
                        if let TypeDefinition::Primitive(Primitive::Int(
                            k,
                            s,
                        )) =
                            self.ctx.type_info.resolve_type_name(type_name)
                        {
                            use types::IntKind::*;
                            use types::IntSize::*;
                            return Some(
                                match (k, s) {
                                    (Unsigned, I8) => IrValue::U8(*x as _),
                                    (Unsigned, I16) => IrValue::U16(*x as _),
                                    (Unsigned, I32) => IrValue::U32(*x as _),
                                    (Unsigned, I64) => IrValue::U64(*x as _),
                                    (Signed, I8) => IrValue::I8(*x as _),
                                    (Signed, I16) => IrValue::I16(*x as _),
                                    (Signed, I32) => IrValue::I32(*x as _),
                                    (Signed, I64) => IrValue::I64(*x as _),
                                }
                                .into(),
                            );
                        }
                    }
                    _ => {}
                }
                ice!("should be a type error");
            }
            Literal::Float(x) => {
                match ty {
                    Type::FloatVar(_) => {
                        return Some(IrValue::F64(*x).into())
                    }
                    Type::Name(type_name) => {
                        if let TypeDefinition::Primitive(Primitive::Float(
                            s,
                        )) =
                            self.ctx.type_info.resolve_type_name(type_name)
                        {
                            return Some(
                                match s {
                                    FloatSize::F32 => IrValue::F32(*x as f32),
                                    FloatSize::F64 => IrValue::F64(*x),
                                }
                                .into(),
                            );
                        }
                    }
                    _ => {}
                }
                ice!("should be a type error");
            }
            Literal::Bool(x) => IrValue::Bool(*x).into(),
            Literal::Unit => return None,
        })
    }

    fn binop(
        &mut self,
        left: mir::Var,
        binop: ast::BinOp,
        ty: Type,
        right: mir::Var,
    ) -> Operand {
        let left = self.var(left).into();
        let right = self.var(right).into();

        if let Some((kind, _size)) = self.ctx.type_info.get_int_type(&ty) {
            if let Some(cmp) = binop_to_int_cmp(&binop, kind) {
                let to = self.new_tmp(IrType::Bool);
                self.emit(Instruction::IntCmp {
                    to: to.clone(),
                    cmp,
                    left,
                    right,
                });
                return to.into();
            }

            let ty = self.lower_type(&ty).unwrap();
            let to = self.new_tmp(ty);
            match binop {
                ast::BinOp::Add => self.emit(Instruction::Add {
                    to: to.clone(),
                    left,
                    right,
                }),
                ast::BinOp::Sub => self.emit(Instruction::Sub {
                    to: to.clone(),
                    left,
                    right,
                }),
                ast::BinOp::Mul => self.emit(Instruction::Mul {
                    to: to.clone(),
                    left,
                    right,
                }),
                ast::BinOp::Div => self.emit(Instruction::Div {
                    to: to.clone(),
                    left,
                    right,
                    signed: kind == IntKind::Signed,
                }),
                _ => ice!(),
            }

            return to.into();
        }

        if self.ctx.type_info.is_float_type(&ty) {
            if let Some(cmp) = binop_to_float_cmp(&binop) {
                let to = self.new_tmp(IrType::Bool);
                self.emit(Instruction::FloatCmp {
                    to: to.clone(),
                    cmp,
                    left,
                    right,
                });
                return to.into();
            }

            let ty = self.lower_type(&ty).unwrap();
            let to = self.new_tmp(ty);
            match binop {
                ast::BinOp::Add => self.emit(Instruction::Add {
                    to: to.clone(),
                    left,
                    right,
                }),
                ast::BinOp::Sub => self.emit(Instruction::Sub {
                    to: to.clone(),
                    left,
                    right,
                }),
                ast::BinOp::Mul => self.emit(Instruction::Mul {
                    to: to.clone(),
                    left,
                    right,
                }),
                ast::BinOp::Div => self.emit(Instruction::FDiv {
                    to: to.clone(),
                    left,
                    right,
                }),
                _ => ice!(),
            }

            return to.into();
        }

        if self.ctx.type_info.is_asn_type(&ty) {
            let Some(cmp) = binop_to_int_cmp(&binop, IntKind::Unsigned)
            else {
                ice!();
            };
            let to = self.new_tmp(IrType::Bool);
            self.emit(Instruction::IntCmp {
                to: to.clone(),
                cmp,
                left,
                right,
            });
            return to.into();
        }

        if binop == ast::BinOp::Eq {
            let size = self
                .ctx
                .type_info
                .layout_of(&ty, self.ctx.runtime)
                .map_or(0, |l| l.size());

            if size == 0 {
                return Operand::Value(IrValue::Bool(true));
            }

            let tmp = self.new_tmp(IrType::Pointer);
            let out = self.new_tmp(IrType::Bool);
            self.emit(Instruction::MemCmp {
                to: tmp.clone(),
                size: IrValue::Pointer(size).into(),
                left: left.clone(),
                right: right.clone(),
            });
            self.emit(Instruction::IntCmp {
                to: out.clone(),
                cmp: IntCmp::Eq,
                left: tmp.into(),
                right: IrValue::Pointer(0).into(),
            });
            return out.into();
        }

        ice!("Could not lower binop")
    }
}

/// # Emit instructions
impl Lowerer<'_, '_> {
    fn emit(&mut self, instruction: Instruction) {
        self.blocks
            .last_mut()
            .unwrap()
            .instructions
            .push(instruction)
    }

    fn emit_assign(&mut self, to: Var, val: Operand, ty: IrType) {
        self.emit(Instruction::Assign { to, val, ty })
    }

    fn emit_not(&mut self, to: Var, val: Operand) {
        self.emit(Instruction::Not { to, val })
    }

    fn emit_negate(&mut self, to: Var, val: Operand) {
        self.emit(Instruction::Negate { to, val })
    }

    fn emit_jump(&mut self, lbl: LabelRef) {
        self.emit(Instruction::Jump(lbl))
    }

    fn emit_read(&mut self, to: Var, from: Operand, ty: IrType) {
        self.emit(Instruction::Read { to, ty, from })
    }

    fn emit_write(&mut self, to: Operand, val: Operand) {
        self.emit(Instruction::Write { to, val })
    }

    fn emit_clone(
        &mut self,
        to: Operand,
        from: Operand,
        clone_fn: unsafe extern "C" fn(*const (), *mut ()),
    ) {
        self.emit(Instruction::Clone { to, from, clone_fn })
    }

    fn emit_memcpy(&mut self, to: Operand, from: Operand, size: u32) {
        self.emit(Instruction::Copy { to, from, size })
    }

    fn emit_switch(
        &mut self,
        examinee: Operand,
        branches: Vec<(usize, LabelRef)>,
        default: LabelRef,
    ) {
        self.emit(Instruction::Switch {
            examinee,
            branches,
            default,
        })
    }

    fn emit_return(&mut self, var: Option<Operand>) {
        self.emit(Instruction::Return(var))
    }

    fn emit_constant_address(&mut self, to: Var, name: Identifier) {
        self.emit(Instruction::ConstantAddress { to, name })
    }
}

/// # Helper methods
impl Lowerer<'_, '_> {
    fn current_label(&self) -> LabelRef {
        self.blocks.last().unwrap().label
    }

    fn offset(&mut self, var: Operand, offset: u32) -> Operand {
        if offset == 0 {
            var
        } else {
            let new = self.new_tmp(IrType::Pointer);
            self.emit(Instruction::Offset {
                to: new.clone(),
                from: var,
                offset,
            });
            new.into()
        }
    }

    fn new_tmp(&mut self, ty: IrType) -> Var {
        let var = Var {
            scope: self.function_scope,
            kind: VarKind::Tmp(self.tmp_idx),
        };
        self.tmp_idx += 1;
        self.variables.push((var.clone(), ValueOrSlot::Val(ty)));
        var
    }

    fn new_stack_slot(&mut self, layout: Layout) -> Var {
        let var = Var {
            scope: self.function_scope,
            kind: VarKind::Tmp(self.tmp_idx),
        };
        self.tmp_idx += 1;
        self.variables
            .push((var.clone(), ValueOrSlot::StackSlot(layout)));
        var
    }

    fn new_block(&mut self, label: LabelRef) {
        self.blocks.push(Block {
            label,
            instructions: Vec::new(),
        })
    }

    fn lower_type(&mut self, ty: &Type) -> Option<IrType> {
        let ty = self.ctx.type_info.resolve(ty);
        if self
            .ctx
            .type_info
            .layout_of(&ty, self.ctx.runtime)
            .map_or(false, |l| l.size() == 0)
        {
            return None;
        }

        if let Type::Name(type_name) = &ty {
            let type_def = self.ctx.type_info.resolve_type_name(type_name);
            if let TypeDefinition::Primitive(p) = type_def {
                use FloatSize::*;
                use IntKind::*;
                use IntSize::*;
                'prim: {
                    return Some(match p {
                        Primitive::Int(Unsigned, I8) => IrType::U8,
                        Primitive::Int(Unsigned, I16) => IrType::U16,
                        Primitive::Int(Unsigned, I32) => IrType::U32,
                        Primitive::Int(Unsigned, I64) => IrType::U64,
                        Primitive::Int(Signed, I8) => IrType::I8,
                        Primitive::Int(Signed, I16) => IrType::I16,
                        Primitive::Int(Signed, I32) => IrType::I32,
                        Primitive::Int(Signed, I64) => IrType::I64,
                        Primitive::Float(F32) => IrType::F32,
                        Primitive::Float(F64) => IrType::F64,
                        Primitive::Asn => IrType::U32,
                        Primitive::Bool => IrType::Bool,
                        _ => break 'prim,
                    });
                }
            }
            if let TypeDefinition::Runtime(_, _) = type_def {
                return Some(IrType::Pointer);
            }
        }

        Some(match ty {
            Type::IntVar(_, _) => IrType::I32,
            Type::FloatVar(_) => IrType::F64,
            x if self.is_reference_type(&x)? => IrType::Pointer,
            _ => ice!("could not lower: {ty:?}"),
        })
    }

    fn is_reference_type(&mut self, ty: &Type) -> Option<bool> {
        self.ctx.type_info.is_reference_type(ty, self.ctx.runtime)
    }
}

fn binop_to_int_cmp(op: &ast::BinOp, kind: IntKind) -> Option<IntCmp> {
    let signed = kind == IntKind::Signed;

    Some(match op {
        ast::BinOp::Eq => IntCmp::Eq,
        ast::BinOp::Ne => IntCmp::Ne,
        ast::BinOp::Lt if signed => IntCmp::SLt,
        ast::BinOp::Le if signed => IntCmp::SLe,
        ast::BinOp::Gt if signed => IntCmp::SGt,
        ast::BinOp::Ge if signed => IntCmp::SGe,
        ast::BinOp::Lt => IntCmp::ULt,
        ast::BinOp::Le => IntCmp::ULe,
        ast::BinOp::Gt => IntCmp::UGt,
        ast::BinOp::Ge => IntCmp::UGe,
        _ => return None,
    })
}

fn binop_to_float_cmp(op: &ast::BinOp) -> Option<FloatCmp> {
    Some(match op {
        ast::BinOp::Eq => FloatCmp::Eq,
        ast::BinOp::Ne => FloatCmp::Ne,
        ast::BinOp::Lt => FloatCmp::Lt,
        ast::BinOp::Le => FloatCmp::Le,
        ast::BinOp::Gt => FloatCmp::Gt,
        ast::BinOp::Ge => FloatCmp::Ge,
        _ => return None,
    })
}
