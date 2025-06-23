use std::{any::TypeId, sync::Arc};

use crate::{
    ast::Identifier,
    lir::{value::IrType, Instruction, Operand},
    runtime::{
        layout::{Layout, LayoutBuilder},
        CloneDrop, Movability,
    },
    typechecker::types::{Primitive, Type, TypeDefinition},
};

use super::Lowerer;

impl Lowerer<'_, '_> {
    pub fn drop_with_type(&mut self, var: Operand, ty: Type) {
        let new_var = var.clone();
        let f = move |lowerer: &mut Self, offset, ty| {
            lowerer.drop_leaf_type(&new_var, offset, ty)
        };
        self.traverse_type(&var.clone(), 0, ty.clone(), "drop".into(), &f);
    }

    fn drop_leaf_type(&mut self, var: &Operand, offset: usize, ty: Type) {
        if let Some(&CloneDrop { drop, .. }) = self.get_leaf_clone_drop(&ty) {
            let var = self.offset(var.clone(), offset as u32);
            self.emit(Instruction::Drop {
                var,
                drop: Some(drop),
            })
        }
    }

    pub fn clone_type(&mut self, from: Operand, to: Operand, ty: &Type) {
        let new_from = from.clone();
        let new_to = to.clone();
        let f = move |lowerer: &mut Self, offset, ty| {
            lowerer.clone_leaf_type(&new_from, &new_to, offset, ty)
        };
        self.traverse_type(&from, 0, ty.clone(), "clone".into(), &f);
    }

    fn clone_leaf_type(
        &mut self,
        from: &Operand,
        to: &Operand,
        offset: usize,
        ty: Type,
    ) {
        if let Some(&CloneDrop { clone, .. }) = self.get_leaf_clone_drop(&ty)
        {
            let from = self.offset(from.clone(), offset as u32);
            let to = self.offset(to.clone(), offset as u32);
            self.emit_clone(to, from, clone);
            return;
        }

        let size = match ty {
            // These are invalid at this point
            Type::Var(_) | Type::ExplicitVar(_) => {
                panic!()
            }
            // These aren't copied, since they aren't leafs or zero sized
            Type::Never
            | Type::Record(_)
            | Type::RecordVar(_, _)
            | Type::Function(_, _) => return,
            Type::Name(type_name) => {
                let type_def =
                    self.ctx.type_info.resolve_type_name(&type_name);

                match type_def {
                    // For enums we will do most of the work in the traversal
                    // but the discriminant should be done here.
                    TypeDefinition::Enum(_, _) => 1,
                    TypeDefinition::Primitive(p) => p.layout().size(),
                    TypeDefinition::Runtime(_, id) => self
                        .ctx
                        .runtime
                        .get_runtime_type(id)
                        .unwrap()
                        .layout()
                        .size(),
                    _ => return,
                }
            }
            Type::IntVar(_) => Primitive::i32().layout().size(),
            Type::FloatVar(_) => Primitive::f64().layout().size(),
        };

        let from = self.offset(from.clone(), offset as u32);
        let to = self.offset(to.clone(), offset as u32);
        self.emit(Instruction::Copy {
            to,
            from,
            size: size as u32,
        })
    }

    fn get_leaf_clone_drop(&mut self, ty: &Type) -> Option<&CloneDrop> {
        let id = match ty {
            Type::Name(type_name) => {
                let type_def =
                    self.ctx.type_info.resolve_type_name(type_name);
                match type_def {
                    TypeDefinition::Runtime(_, id) => Some(id),
                    TypeDefinition::Primitive(Primitive::String) => {
                        Some(TypeId::of::<Arc<str>>())
                    }
                    _ => None,
                }
            }
            _ => None,
        };

        let id = id?;

        let ty = self.ctx.runtime.get_runtime_type(id).unwrap();

        if let Movability::CloneDrop(clone_drop) = ty.movability() {
            Some(clone_drop)
        } else {
            None
        }
    }

    fn traverse_type(
        &mut self,
        var: &Operand,
        offset: usize,
        ty: Type,
        label: Identifier,
        f: &impl Fn(&mut Self, usize, Type),
    ) {
        let ty = self.ctx.type_info.resolve(&ty);
        f(self, offset, ty.clone());
        match ty {
            Type::RecordVar(_, fields) | Type::Record(fields) => {
                let mut builder = LayoutBuilder::new();
                for (_, ty) in fields {
                    let new_offset = builder.add(
                        &self.ctx.type_info.layout_of(&ty, self.ctx.runtime),
                    );
                    self.traverse_type(
                        var,
                        offset + new_offset,
                        ty,
                        label,
                        f,
                    );
                }
            }
            Type::Name(type_name) => {
                let type_def =
                    self.ctx.type_info.resolve_type_name(&type_name);
                match type_def {
                    TypeDefinition::Runtime(_, _) => {}
                    TypeDefinition::Primitive(_) => {}
                    TypeDefinition::Enum(type_constructor, variants) => {
                        let subs: Vec<_> = type_constructor
                            .arguments
                            .iter()
                            .zip(&type_name.arguments)
                            .collect();

                        let current_label = self.current_label();
                        let lbl_prefix = self
                            .ctx
                            .label_store
                            .wrap_internal(current_label, label);
                        let continue_lbl =
                            self.ctx.label_store.next(current_label);

                        let branches: Vec<_> = (0..variants.len())
                            .map(|i| {
                                let ident =
                                    Identifier::from(&format!("variant_{i}"));
                                let lbl = self
                                    .ctx
                                    .label_store
                                    .wrap_internal(lbl_prefix, ident);
                                (i, lbl)
                            })
                            .collect();

                        let offset_var =
                            self.offset(var.clone(), offset as u32);

                        let discriminant = self.new_tmp();
                        self.emit(Instruction::Read {
                            to: discriminant.clone(),
                            from: offset_var,
                            ty: IrType::U8,
                        });
                        self.emit(Instruction::Switch {
                            examinee: discriminant.into(),
                            branches: branches.clone(),
                            default: continue_lbl,
                        });

                        for (idx, lbl) in branches {
                            self.new_block(lbl);
                            let variant = &variants[idx];

                            let mut builder = LayoutBuilder::new();
                            builder.add(&Layout::of::<u8>());
                            for ty in &variant.fields {
                                let ty = ty.substitute_many(&subs);
                                let new_offset = builder.add(
                                    &self
                                        .ctx
                                        .type_info
                                        .layout_of(&ty, self.ctx.runtime),
                                );
                                self.traverse_type(
                                    var,
                                    offset + new_offset,
                                    ty,
                                    label,
                                    f,
                                );
                            }
                            self.emit(Instruction::Jump(continue_lbl))
                        }

                        self.new_block(continue_lbl);
                    }
                    TypeDefinition::Record(type_constructor, fields) => {
                        let subs: Vec<_> = type_constructor
                            .arguments
                            .iter()
                            .zip(&type_name.arguments)
                            .collect();

                        let mut builder = LayoutBuilder::new();
                        for (_, ty) in &fields {
                            let ty = ty.substitute_many(&subs);
                            let new_offset = builder.add(
                                &self
                                    .ctx
                                    .type_info
                                    .layout_of(&ty, self.ctx.runtime),
                            );
                            self.traverse_type(
                                var,
                                offset + new_offset,
                                ty,
                                label,
                                f,
                            );
                        }
                    }
                }
            }
            Type::Never | Type::IntVar(_) | Type::FloatVar(_) => {}
            Type::Var(_) | Type::Function(_, _) | Type::ExplicitVar(_) => {
                panic!("Can't traverse: {ty:?}")
            }
        }
    }
}
