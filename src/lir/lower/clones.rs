use std::{any::TypeId, collections::HashSet, sync::Arc};

use crate::{
    ast::Identifier,
    ice,
    lir::{
        Block, Function, Instruction, Signature, Var, VarKind,
        lower::{Location, LowerCtx},
        value::IrType,
    },
    parser::meta::Meta,
    runtime::{
        Movability,
        layout::{Layout, LayoutBuilder},
    },
    typechecker::{
        scope::{ScopeRef, ScopeType},
        scoped_display::TypeDisplay,
        types::{self, EnumVariant, Primitive, Type, TypeDefinition},
    },
    value::ErasedList,
};

use super::Lowerer;

impl Lowerer<'_, '_> {
    /// Call the clone function (if any) for a given type
    ///
    /// This means:
    ///
    ///  - Do nothing for simple primitives.
    ///  - Call `::generated_clone_{type_id}` for complex types.
    ///  - Call the runtime drop function for registered types.
    ///
    /// This function also checks whether the value needs to be dropped in the
    /// first place.
    pub fn call_clone_of(&mut self, to: Location, from: Location, ty: &Type) {
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
                let from = self.offset(base, offset as u32);
                self.emit_read(to, from.into(), ty)
            }
            // We write a not-by-reference type to a field
            (Location::Pointer { base, offset }, Location::Var(from)) => {
                let Some(_ty) = self.lower_type(ty) else {
                    return;
                };
                let to = self.offset(base, offset as u32);
                self.emit_write(to.into(), from.into());
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
                let from = self.offset(base_from, offset_from as u32);
                let to = self.offset(base_to, offset_to as u32);
                self.call_clone_function(from, to, ty);
            }
        }
    }

    /// Call the clone function (if any) for a given type
    ///
    /// This means:
    ///
    ///  - Do a memcpy for simple primitives or types that do not need custom
    ///    clones.
    ///  - Call `::generated_clone_{type_id}` for complex types.
    ///  - Call the runtime drop function for registered types.
    ///
    /// This function also checks whether the value needs to be dropped in the
    /// first place.
    pub fn call_clone_function(&mut self, from: Var, to: Var, ty: &Type) {
        // The easy case, we don't need to clone this.
        if !self.needs_clone(ty) {
            let size = self
                .ctx
                .type_info
                .layout_of(ty, self.ctx.runtime)
                .unwrap()
                .size() as u32;
            self.emit_memcpy(to.into(), from.into(), size);
            return;
        }

        // In this case we have to call a runtime function
        if let Some(clone_fn) = self.get_runtime_clone(ty) {
            self.emit_clone(to.into(), from.into(), clone_fn);
            return;
        }

        // Finally, this might be a complex Roto type for which we generate a
        // drop function
        let type_id = self.ctx.type_info.type_id(ty);
        self.emit(Instruction::Call {
            to: None,
            ctx: None,
            func: format!("::generated::clone_{type_id}").into(),
            args: vec![from.clone().into()],
            return_ptr: Some(to),
        });

        // When that happens we also need to make sure that drop function will
        // be generated.
        self.ctx.clones_to_generate.push_back(ty.clone());
    }

    pub fn needs_clone(&mut self, ty: &Type) -> bool {
        let ty = self.ctx.type_info.resolve(ty);

        match &ty {
            Type::Record(items) | Type::RecordVar(_, items) => {
                items.iter().any(|i| self.needs_clone(&i.1))
            }
            Type::IntVar(_, _)
            | Type::FloatVar(_)
            | Type::Unit
            | Type::Var(_) // assumed to be Unit
            | Type::Never => false,
            Type::Name(type_name) => {
                let type_def =
                    self.ctx.type_info.resolve_type_name(type_name);

                if let Some(items) =
                    type_def.record_fields(&type_name.arguments)
                {
                    items.iter().any(|i| self.needs_clone(&i.1))
                } else if let Some(variants) =
                    type_def.match_patterns(&type_name.arguments)
                {
                    variants
                        .iter()
                        .flat_map(|v| &v.fields)
                        .any(|i| self.needs_clone(i))
                } else {
                    self.get_runtime_clone(&ty).is_some()
                }
            }
            Type::Function(_, _) | Type::ExplicitVar(_) => {
                ice!(
                    "Can't check whether {} needs clone",
                    ty.display(self.ctx.type_info)
                )
            }
        }
    }

    pub fn generate_clones(ctx: &mut LowerCtx<'_>) -> Vec<Function> {
        // We collect all the generated functions in here.
        let mut functions = Vec::new();

        // To ensure that we don't generate any function twice, we keep track
        // of all the type_ids we have generated a clone function for.
        let mut generated = HashSet::new();

        // We don't use a for loop because we will add more types to the end
        // of the vecdeque while processing it.
        while let Some(ty) = ctx.clones_to_generate.pop_front() {
            let type_id = ctx.type_info.type_id(&ty);

            // HashMap::insert returns false when the value is not new, that is,
            // when we have already generated this clone function.
            if !generated.insert(type_id) {
                continue;
            }

            let function = Self::generate_clone(ctx, &ty);
            functions.push(function);
        }

        functions
    }

    fn generate_clone(ctx: &mut LowerCtx<'_>, ty: &Type) -> Function {
        let type_id = ctx.type_info.type_id(ty);

        let ident = format!("::generated::clone_{type_id}").into();
        let root_scope = ctx.type_info.scope_graph.root();
        let scope = ctx
            .type_info
            .scope_graph
            .wrap(root_scope, ScopeType::Function(ident));

        let mut lowerer = Lowerer {
            ctx,
            tmp_idx: 0,
            // The clone fn doesn't need to access anything, so the
            // scope doesn't really matter.
            function_scope: scope,
            return_type: Type::Unit,
            blocks: Vec::new(),
            variables: Vec::new(),
        };

        lowerer.generate_clone_body(ident, scope, ty);

        let signature = types::Signature {
            types: Vec::new(),
            parameter_types: vec![ty.clone()],
            return_type: ty.clone(),
        };

        // We need a unified signature for all types, so we always expect pointers
        let ir_signature = Signature {
            parameters: vec![("val".into(), IrType::Pointer)],
            context: false,
            return_ptr: true,
            return_type: None,
        };

        let entry_block = lowerer.blocks[0].label;

        Function {
            name: ident,
            scope,
            signature,
            ir_signature,
            entry_block,
            variables: lowerer.variables,
            blocks: lowerer.blocks,
            public: false,
        }
    }

    fn generate_clone_body(
        &mut self,
        ident: Identifier,
        scope: ScopeRef,
        ty: &Type,
    ) {
        self.blocks.push(Block {
            label: self.ctx.label_store.new_label(ident),
            instructions: Vec::new(),
        });

        // This is the variable referring to the value being cloned.
        let root_var = Var {
            scope,
            kind: VarKind::Explicit("val".into()),
        };

        let return_var = Var {
            scope,
            kind: VarKind::Return,
        };

        let ty = self.ctx.type_info.resolve(ty);

        // This takes care of String and Clone registered types, so we don't
        // have to consider those later.
        if let Some(clone_fn) = self.get_runtime_clone(&ty) {
            self.emit(Instruction::Clone {
                to: return_var.into(),
                from: root_var.into(),
                clone_fn,
            });
            self.emit_return(None);
            return;
        }

        match &ty {
            Type::ExplicitVar(_) => {
                ice!("Can't generate clone of explicit type variable")
            }
            // These are all simple types. In practice, we should never generate
            // these functions, but if we get here anyway, we can simply generate
            // a function that just returns.
            Type::Unit
            | Type::Never
            | Type::IntVar(_, _)
            | Type::FloatVar(_)
            | Type::Function(_, _)
            | Type::Var(_) => {
                self.emit_return(None);
            }
            Type::RecordVar(_, fields) | Type::Record(fields) => {
                self.generate_clone_body_record(return_var, root_var, fields)
            }
            Type::Name(type_name) => {
                let type_def =
                    self.ctx.type_info.resolve_type_name(type_name);

                if let Some(fields) =
                    type_def.record_fields(&type_name.arguments)
                {
                    self.generate_clone_body_record(
                        return_var, root_var, &fields,
                    );
                    return;
                }

                if let Some(variants) =
                    type_def.match_patterns(&type_name.arguments)
                {
                    self.generate_clone_body_enum(
                        return_var, root_var, &variants,
                    );
                    return;
                }

                match type_def {
                    // We handled String above, the other primitives don't need to be
                    // dropped.
                    TypeDefinition::Primitive(_) => {
                        self.emit_return(None);
                    }
                    // If we get here with a runtime type, it implements Copy, so
                    // doesn't need to be dropped.
                    TypeDefinition::Runtime(_, _) => {
                        let size = self
                            .ctx
                            .type_info
                            .layout_of(&ty, self.ctx.runtime)
                            .unwrap()
                            .size() as u32;

                        self.emit_memcpy(
                            return_var.into(),
                            root_var.into(),
                            size,
                        );

                        self.emit_return(None);
                    }
                    TypeDefinition::List(_) => {
                        ice!("list clone should have been handled above");
                    }
                    TypeDefinition::Enum(_, _) => {
                        ice!("enum clone should have been handled above");
                    }
                    TypeDefinition::Record(_, _) => {
                        ice!("record clone should have been handled above");
                    }
                }
            }
        }
    }

    fn generate_clone_body_record(
        &mut self,
        return_var: Var,
        root_var: Var,
        fields: &[(Meta<Identifier>, Type)],
    ) {
        let mut builder = LayoutBuilder::new();
        for (_, ty) in fields {
            let Some(layout) =
                self.ctx.type_info.layout_of(ty, self.ctx.runtime)
            else {
                continue;
            };

            let new_offset = builder.add(&layout);

            if !self.needs_clone(ty) {
                continue;
            }

            let to = Location::Pointer {
                base: return_var.clone(),
                offset: new_offset,
            };

            let from = Location::Pointer {
                base: root_var.clone(),
                offset: new_offset,
            };

            self.call_clone_of(to, from, ty);
        }

        self.emit_return(None);
    }

    fn generate_clone_body_enum(
        &mut self,
        return_var: Var,
        root_var: Var,
        variants: &[EnumVariant],
    ) {
        let current_label = self.current_label();
        let lbl_prefix = self
            .ctx
            .label_store
            .wrap_internal(current_label, "clone".into());

        let branches: Vec<_> = (0..variants.len())
            .map(|i| {
                let ident = Identifier::from(&format!("variant_{i}"));
                let lbl =
                    self.ctx.label_store.wrap_internal(lbl_prefix, ident);
                (i, lbl)
            })
            .collect();

        let offset_var = self.offset(root_var.clone(), 0);

        let discriminant = self.new_tmp(IrType::U8);
        self.emit(Instruction::Read {
            to: discriminant.clone(),
            from: offset_var.into(),
            ty: IrType::U8,
        });

        // We branch on the discriminant AND we have to write it to the
        // destination.
        self.emit_write(
            return_var.clone().into(),
            discriminant.clone().into(),
        );

        let mut lbls = branches.clone();
        let default_lbl = lbls.pop().unwrap().1;

        self.emit(Instruction::Switch {
            examinee: discriminant.into(),
            branches: lbls,
            default: default_lbl,
        });

        for (idx, lbl) in branches {
            self.new_block(lbl);
            let variant = &variants[idx];

            let Some(layouts) = variant
                .fields
                .iter()
                .map(|ty| {
                    let layout =
                        self.ctx.type_info.layout_of(ty, self.ctx.runtime)?;
                    Some((ty, layout))
                })
                .collect::<Option<Vec<_>>>()
            else {
                // If one of the items is uninhabited then we don't have to do anything here
                self.emit(Instruction::Return(None));
                continue;
            };

            let mut builder = LayoutBuilder::new();
            builder.add(&Layout::of::<u8>());
            for (ty, layout) in layouts {
                let new_offset = builder.add(&layout);

                let from = Location::Pointer {
                    base: return_var.clone(),
                    offset: new_offset,
                };

                let to = Location::Pointer {
                    base: root_var.clone(),
                    offset: new_offset,
                };

                self.call_clone_of(from, to, ty);
            }
            self.emit(Instruction::Return(None));
        }
    }

    /// Returns the clone function of a registered type
    fn get_runtime_clone(
        &mut self,
        ty: &Type,
    ) -> Option<unsafe extern "C" fn(*const (), *mut ())> {
        let id = match ty {
            Type::Name(type_name) => {
                let type_def =
                    self.ctx.type_info.resolve_type_name(type_name);
                match type_def {
                    TypeDefinition::Runtime(_, id) => Some(id),
                    TypeDefinition::Primitive(Primitive::String) => {
                        Some(TypeId::of::<crate::String>())
                    }
                    TypeDefinition::List(_) => {
                        Some(TypeId::of::<ErasedList>())
                    }
                    _ => None,
                }
            }
            _ => None,
        };

        let id = id?;

        let ty = self.ctx.runtime.get_runtime_type(id).unwrap();

        if let Movability::CloneDrop(clone_drop) = ty.movability() {
            Some(clone_drop.clone)
        } else {
            None
        }
    }
}
