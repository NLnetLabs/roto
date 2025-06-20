mod clone_drop;

use crate::{
    ast::Literal,
    ice,
    label::{LabelRef, LabelStore},
    mir,
    runtime::layout::{Layout, LayoutBuilder},
    typechecker::{
        info::TypeInfo,
        scope::ScopeRef,
        types::{EnumVariant, FloatSize, Primitive, Type, TypeDefinition},
    },
    IrValue, Runtime,
};

use super::{
    value::IrType, Block, Function, Instruction, Lir, Operand, Var, VarKind,
};

pub fn lower_to_lir(
    runtime: &Runtime,
    type_info: &mut TypeInfo,
    label_store: &mut LabelStore,
    mir: mir::Mir,
) -> Lir {
    Lowerer::program(runtime, type_info, label_store, mir)
}

struct Lowerer<'r> {
    blocks: Vec<Block>,
    function_scope: ScopeRef,
    type_info: &'r mut TypeInfo,
    label_store: &'r mut LabelStore,
    tmp_idx: usize,
    runtime: &'r Runtime,
    return_type: Type,
}

enum Location {
    Var(Var),
    Pointer { base: Var, offset: usize },
}

/// # Lower MIR constructs
impl Lowerer<'_> {
    fn program(
        runtime: &Runtime,
        type_info: &mut TypeInfo,
        label_store: &mut LabelStore,
        mir: mir::Mir,
    ) -> Lir {
        let mut functions = Vec::new();

        for function in mir.functions {
            functions.push(Self::function(
                runtime,
                type_info,
                label_store,
                function,
            ));
        }

        Lir { functions }
    }

    fn function(
        runtime: &Runtime,
        type_info: &mut TypeInfo,
        label_store: &mut LabelStore,
        function: mir::Function,
    ) -> Function {
        let mut lowerer = Lowerer {
            runtime,
            type_info,
            label_store,
            tmp_idx: function.tmp_idx,
            function_scope: function.scope,
            return_type: function.signature.return_type.clone(),
            blocks: Vec::new(),
        };
        let name = function.name;
        let signature = function.signature;

        for block in function.blocks {
            lowerer.block(block);
        }

        Function {
            name,
            blocks: lowerer.blocks,
            signature,
            scope: todo!(),
            ir_signature: todo!(),
            entry_block: todo!(),
            public: todo!(),
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
            mir::Instruction::Assign {
                to,
                root_ty,
                ty,
                value,
            } => self.assign(to, root_ty, ty, value),
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
            mir::Instruction::Alloc { to, ty } => self.alloc(to, &ty),
            mir::Instruction::Return { var } => self.r#return(var),
            mir::Instruction::Drop { var, ty } => self.drop(var, ty),
        }
    }

    fn assign(
        &mut self,
        to: mir::Place,
        root_ty: Type,
        ty: Type,
        value: mir::Value,
    ) {
        // A call value is the only value that can have a side effect.
        // We can ignore the others if the return type is zero-sized.
        if self.type_info.layout_of(&ty, self.runtime).size() == 0 {
            match value {
                mir::Value::Call { func, args } => {
                    todo!()
                }
                _ => {} // no nothing!
            }
            return;
        }

        let location = if to.projection.is_empty() {
            if self.type_info.is_reference_type(&ty, self.runtime) {
                Location::Pointer {
                    base: self.var(to.var),
                    offset: 0,
                }
            } else {
                Location::Var(self.var(to.var))
            }
        } else {
            let base = self.var(to.var);

            let mut offset = 0;
            let mut ty = root_ty;
            for p in to.projection {
                match p {
                    mir::Projection::Field(ident) => {
                        let Type::Name(name) = &ty else { ice!() };
                        let TypeDefinition::Record(_, fields) =
                            self.type_info.resolve_type_name(&name)
                        else {
                            ice!()
                        };

                        let mut new_offset = 0;
                        let mut builder = LayoutBuilder::new();
                        for (field, new_ty) in fields {
                            new_offset = builder.add(
                                &self.type_info.layout_of(&ty, self.runtime),
                            );
                            if *field == ident {
                                ty = new_ty;
                                break;
                            }
                        }

                        offset += new_offset;
                    }
                    mir::Projection::VariantField(variant, n) => {
                        let mut builder = LayoutBuilder::new();
                        builder.add(
                            &self
                                .type_info
                                .layout_of(&Type::u8(), self.runtime),
                        );

                        let mut new_offset = 0;
                        for ty in variant.fields.iter().take(n) {
                            new_offset = builder.add(
                                &self.type_info.layout_of(&ty, self.runtime),
                            );
                        }

                        offset = new_offset;
                    }
                }
            }

            Location::Pointer { base, offset }
        };

        let op = match value {
            mir::Value::Const(lit, ty) => self.literal(&lit, &ty),
            mir::Value::Move(var) => self.var(var).into(),
            mir::Value::Discriminant(v) => self.get_discriminant(v),
            mir::Value::Not(..) => todo!(),
            mir::Value::Clone(..) => todo!(),
            mir::Value::BinOp { .. } => todo!(),
            mir::Value::Call { .. } => todo!(),
            mir::Value::CallRuntime { .. } => todo!(),
        };
    }

    fn switch(
        &mut self,
        examinee: mir::Var,
        branches: Vec<(usize, LabelRef)>,
        default: LabelRef,
    ) {
        let examinee = self.var(examinee);
        // TODO: Switch should probably have a var as argument
        self.emit_switch(examinee.into(), branches, default);
    }

    fn alloc(&mut self, to: mir::Var, ty: &Type) {
        let layout = self.type_info.layout_of(&ty, self.runtime);
        let to = self.var(to);
        self.emit_alloc(to, layout);
    }

    fn get_discriminant(&mut self, from: mir::Var) -> Operand {
        let from = self.var(from);
        let tmp = self.new_tmp();
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
            self.type_info.resolve_type_name(type_name)
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

        let layout =
            self.type_info.layout_of(&self.return_type, self.runtime);

        if layout.size() == 0 {
            self.emit_return(None);
            return;
        }

        if self
            .type_info
            .is_reference_type(&self.return_type, self.runtime)
        {
            self.emit_memcpy(
                Var {
                    scope: self.function_scope,
                    kind: VarKind::Return,
                }
                .into(),
                var.into(),
                layout.size() as u32,
            );
            self.emit_return(None);
        } else {
            self.emit_return(Some(var.into()));
        }
    }

    fn drop(&mut self, var: mir::Var, ty: Type) {
        let var = self.var(var);
        self.drop_with_type(var.into(), ty)
    }

    fn var(&self, to: mir::Var) -> Var {
        Var {
            scope: to.scope,
            kind: match to.kind {
                mir::VarKind::Explicit(identifier) => {
                    VarKind::Explicit(identifier)
                }
                mir::VarKind::Tmp(x) => VarKind::Tmp(x),
                mir::VarKind::NamedTmp(identifier, x) => {
                    VarKind::NamedTmp(identifier, x)
                }
            },
        }
    }

    /// Lower a literal
    fn literal(&mut self, lit: &Literal, ty: &Type) -> Operand {
        match &lit {
            Literal::String(s) => {
                let to = self.new_tmp();
                let layout =
                    self.type_info.layout_of(&Type::string(), self.runtime);
                self.emit_alloc(to.clone(), layout);
                self.emit(Instruction::InitString {
                    to: to.clone(),
                    string: s.clone(),
                    init_func: self.runtime.string_init_function,
                });

                to.into()
            }
            Literal::Asn(n) => IrValue::Asn(*n).into(),
            Literal::IpAddress(addr) => {
                let to = self.new_tmp();

                const SIZE: usize = Primitive::IpAddr.layout().size();
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
                    Type::IntVar(_) => return IrValue::I32(*x as i32).into(),
                    Type::Name(type_name) => {
                        if let TypeDefinition::Primitive(Primitive::Int(
                            k,
                            s,
                        )) = self.type_info.resolve_type_name(&type_name)
                        {
                            use super::IntKind::*;
                            use super::IntSize::*;
                            return match (k, s) {
                                (Unsigned, I8) => IrValue::U8(*x as _),
                                (Unsigned, I16) => IrValue::U16(*x as _),
                                (Unsigned, I32) => IrValue::U32(*x as _),
                                (Unsigned, I64) => IrValue::U64(*x as _),
                                (Signed, I8) => IrValue::I8(*x as _),
                                (Signed, I16) => IrValue::I16(*x as _),
                                (Signed, I32) => IrValue::I32(*x as _),
                                (Signed, I64) => IrValue::I64(*x as _),
                            }
                            .into();
                        }
                    }
                    _ => {}
                }
                ice!("should be a type error");
            }
            Literal::Float(x) => {
                match ty {
                    Type::FloatVar(_) => return IrValue::F64(*x).into(),
                    Type::Name(type_name) => {
                        if let TypeDefinition::Primitive(Primitive::Float(
                            s,
                        )) = self.type_info.resolve_type_name(&type_name)
                        {
                            return match s {
                                FloatSize::F32 => IrValue::F32(*x as f32),
                                FloatSize::F64 => IrValue::F64(*x),
                            }
                            .into();
                        }
                    }
                    _ => {}
                }
                ice!("should be a type error");
            }
            Literal::Bool(x) => IrValue::Bool(*x).into(),
            Literal::Unit => todo!(),
        }
    }
}

/// # Emit instructions
impl Lowerer<'_> {
    fn emit(&mut self, instruction: Instruction) {
        self.blocks
            .last_mut()
            .unwrap()
            .instructions
            .push(instruction)
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

    fn emit_memcpy(&mut self, to: Operand, from: Operand, size: u32) {
        self.emit(Instruction::Copy {
            to,
            from,
            size,
            clone: None,
        })
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

    fn emit_alloc(&mut self, to: Var, layout: Layout) {
        self.emit(Instruction::Alloc {
            to: to.clone(),
            layout,
        });
    }

    fn emit_return(&mut self, var: Option<Operand>) {
        self.emit(Instruction::Return(var))
    }
}

/// # Helper methods
impl Lowerer<'_> {
    fn current_label(&self) -> LabelRef {
        self.blocks.last().unwrap().label
    }

    fn offset(&mut self, var: Operand, offset: u32) -> Operand {
        if offset == 0 {
            var
        } else {
            let new = self.new_tmp();
            self.emit(Instruction::Offset {
                to: new.clone(),
                from: var,
                offset,
            });
            new.into()
        }
    }

    fn new_tmp(&mut self) -> Var {
        let var = Var {
            scope: self.function_scope,
            kind: VarKind::Tmp(self.tmp_idx),
        };
        self.tmp_idx += 1;
        var
    }

    fn new_block(&mut self, label: LabelRef) {
        self.blocks.push(Block {
            label,
            instructions: Vec::new(),
        })
    }
}
