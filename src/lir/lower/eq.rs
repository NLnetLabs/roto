use std::{any::TypeId, collections::HashSet, net::IpAddr};

use inetnum::addr::Prefix;

use crate::{
    RotoString,
    ast::Identifier,
    label::LabelRef,
    lir::{
        Block, FloatCmp, Instruction, IntCmp, IrType, IrValue, Item,
        ItemKind, Operand, Signature, Var, VarKind,
    },
    mir::{Ty, TyRef},
    runtime::layout::{Layout, LayoutBuilder},
    typechecker::{
        scope::{ScopeRef, ScopeType},
        types::Primitive,
    },
    value::{EqFn, ErasedList},
};

use super::{LowerCtx, Lowerer};

impl Lowerer<'_, '_> {
    pub fn call_eq_of(
        &mut self,
        negated: bool,
        left: Operand,
        right: Operand,
        ty: TyRef,
    ) -> Operand {
        let Some(ir_ty) = self.lower_type(ty) else {
            return IrValue::Bool(true).into();
        };
        match ir_ty {
            IrType::Bool
            | IrType::U8
            | IrType::U16
            | IrType::U32
            | IrType::U64
            | IrType::I8
            | IrType::I16
            | IrType::I32
            | IrType::I64
            | IrType::Char
            | IrType::Asn => {
                let to = self.new_tmp(IrType::Bool);
                self.emit(Instruction::IntCmp {
                    to: to.clone(),
                    cmp: if negated { IntCmp::Ne } else { IntCmp::Eq },
                    left,
                    right,
                });
                return to.into();
            }
            IrType::F32 | IrType::F64 => {
                let to = self.new_tmp(IrType::Bool);
                self.emit(Instruction::FloatCmp {
                    to: to.clone(),
                    cmp: if negated { FloatCmp::Ne } else { FloatCmp::Eq },
                    left,
                    right,
                });
                return to.into();
            }
            IrType::Pointer => {} // continue
        };

        let to = self.new_tmp(IrType::Bool);

        if let Some(eq_fn) = self.get_runtime_eq(ty) {
            let layout = self.layout_of(TyRef::BOOL).unwrap();
            let out_ptr = self.new_stack_slot(layout);
            self.emit_eq(out_ptr.clone().into(), left, right, eq_fn);
            self.emit_read(to.clone(), out_ptr.into(), IrType::Bool);
            if negated {
                self.emit_not(to.clone(), to.clone().into());
            }
            return to.into();
        }

        let layout = self.layout_of(TyRef::BOOL).unwrap();
        let out_ptr = self.new_stack_slot(layout);

        let type_id = ty.type_id();
        self.emit(Instruction::Call {
            to: None,
            ctx: None,
            func: format!("::generated::eq_{type_id}").into(),
            args: vec![out_ptr.clone().into(), left.clone(), right.clone()],
            return_ptr: None,
        });

        self.emit_read(to.clone(), out_ptr.into(), IrType::Bool);

        // When that happens we also need to make sure that the clone function
        // will be generated.
        self.ctx.eq_to_generate.push_back(ty);

        if negated {
            self.emit_not(to.clone(), to.clone().into());
        }

        to.into()
    }

    pub fn call_eq_by_ptr(
        &mut self,
        left_ptr: Var,
        right_ptr: Var,
        ty: TyRef,
    ) -> Operand {
        let Some(is_ref) = self.is_reference_type(ty) else {
            return IrValue::Bool(true).into();
        };

        if is_ref {
            self.call_eq_of(false, left_ptr.into(), right_ptr.into(), ty)
        } else {
            let ir_ty = self.lower_type(ty).unwrap();

            let left = self.new_tmp(ir_ty);
            self.emit_read(left.clone(), left_ptr.into(), ir_ty);

            let right = self.new_tmp(ir_ty);
            self.emit_read(right.clone(), right_ptr.into(), ir_ty);

            self.call_eq_of(false, left.into(), right.into(), ty)
        }
    }

    pub fn generate_eqs(ctx: &mut LowerCtx<'_>) -> Vec<Item> {
        // We collect all the generated functions in here.
        let mut functions = Vec::new();

        // To ensure that we don't generate any function twice, we keep track
        // of all the type_ids we have generated a clone function for.
        let mut generated = HashSet::new();

        // We don't use a for loop because we will add more types to the end
        // of the vecdeque while processing it.
        while let Some(ty) = ctx.eq_to_generate.pop_front() {
            let type_id = ty.type_id();

            // HashMap::insert returns false when the value is not new, that is,
            // when we have already generated this clone function.
            if !generated.insert(type_id) {
                continue;
            }

            let function = Self::generate_eq(ctx, ty);
            functions.push(function);
        }

        functions
    }

    fn generate_eq(ctx: &mut LowerCtx<'_>, ty: TyRef) -> Item {
        let type_id = ty.type_id();

        let ident = format!("::generated::eq_{type_id}").into();
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
            return_type: TyRef::UNIT,
            blocks: Vec::new(),
            variables: Vec::new(),
        };

        lowerer.generate_eq_body(ident, scope, ty);

        // We need a unified signature for all types, so we always expect pointers
        let ir_signature = Signature {
            parameters: vec![
                ("left".into(), IrType::Pointer),
                ("right".into(), IrType::Pointer),
            ],
            context: false,
        };

        let entry_block = lowerer.blocks[0].label;

        Item {
            name: ident,
            scope,
            kind: ItemKind::Function {
                signature: None,
                ir_signature,
            },
            entry_block,
            variables: lowerer.variables,
            blocks: lowerer.blocks,
            public: false,
        }
    }

    fn generate_eq_body(
        &mut self,
        ident: Identifier,
        scope: ScopeRef,
        ty: TyRef,
    ) {
        self.blocks.push(Block {
            label: self.ctx.label_store.new_label(ident),
            instructions: Vec::new(),
        });

        let left_ptr = Var {
            scope,
            kind: VarKind::Explicit("left".into()),
        };

        let right_ptr = Var {
            scope,
            kind: VarKind::Explicit("right".into()),
        };

        match self.ctx.type_info.ty_pool.get(ty) {
            Ty::Unit | Ty::Never => {
                self.emit_write(
                    Var {
                        scope: self.function_scope,
                        kind: VarKind::Return,
                    }
                    .into(),
                    IrValue::Bool(true).into(),
                );
                self.emit_return();
            }
            Ty::Record(fields) => {
                let fields = fields.clone();
                self.generate_eq_body_record(left_ptr, right_ptr, &fields)
            }
            Ty::Enum(variants) => {
                let variants = variants.clone();
                self.generate_eq_body_enum(left_ptr, right_ptr, &variants)
            }
            Ty::Runtime(_) => {
                self.generate_eq_runtime(left_ptr, right_ptr, ty);
            }
            Ty::Primitive(primitive) => match primitive {
                Primitive::Char
                | Primitive::Bool
                | Primitive::Asn
                | Primitive::Int(_, _) => {
                    self.generate_int_eq(left_ptr, right_ptr, ty)
                }
                Primitive::IpAddr | Primitive::Prefix => {
                    self.generate_eq_runtime(left_ptr, right_ptr, ty)
                }
                Primitive::Float(_) => {
                    self.generate_float_eq(left_ptr, right_ptr, ty)
                }
                Primitive::String => {
                    self.generate_eq_runtime(left_ptr, right_ptr, ty)
                }
            },
            Ty::List(_) => self.generate_eq_runtime(left_ptr, right_ptr, ty),
        }
    }

    fn generate_int_eq(&mut self, left_ptr: Var, right_ptr: Var, ty: TyRef) {
        let ir_ty = self.lower_type(ty).unwrap();

        let left = self.new_tmp(ir_ty);
        self.emit_read(left.clone(), left_ptr.into(), ir_ty);

        let right = self.new_tmp(ir_ty);
        self.emit_read(right.clone(), right_ptr.into(), ir_ty);

        let to = self.new_tmp(IrType::Bool);
        self.emit(Instruction::IntCmp {
            to: to.clone(),
            cmp: IntCmp::Eq,
            left: left.into(),
            right: right.into(),
        });

        self.emit_write(
            Var {
                scope: self.function_scope,
                kind: VarKind::Return,
            }
            .into(),
            to.into(),
        );
        self.emit_return();
    }

    fn generate_float_eq(
        &mut self,
        left_ptr: Var,
        right_ptr: Var,
        ty: TyRef,
    ) {
        let ir_ty = self.lower_type(ty).unwrap();

        let left = self.new_tmp(ir_ty);
        self.emit_read(left.clone(), left_ptr.into(), ir_ty);

        let right = self.new_tmp(ir_ty);
        self.emit_read(right.clone(), right_ptr.into(), ir_ty);

        let to = self.new_tmp(IrType::Bool);
        self.emit(Instruction::FloatCmp {
            to: to.clone(),
            cmp: FloatCmp::Eq,
            left: left.into(),
            right: right.into(),
        });

        self.emit_write(
            Var {
                scope: self.function_scope,
                kind: VarKind::Return,
            }
            .into(),
            to.into(),
        );
        self.emit_return();
    }

    fn generate_eq_body_record(
        &mut self,
        left_base: Var,
        right_base: Var,
        fields: &[(Identifier, TyRef)],
    ) {
        let mut builder = LayoutBuilder::new();

        let current_label = self.current_label();
        let lbl_prefix = self
            .ctx
            .label_store
            .wrap_internal(current_label, "eq".into());

        let lbls: Vec<LabelRef> = (0..fields.len() + 1)
            .map(|i| {
                let ident = Identifier::from(&format!("field_{i}"));
                self.ctx.label_store.wrap_internal(lbl_prefix, ident)
            })
            .collect();

        let ident = Identifier::from("false");
        let false_lbl = self.ctx.label_store.wrap_internal(lbl_prefix, ident);

        self.emit_jump(lbls[0]);

        for (i, (_, ty)) in fields.iter().enumerate() {
            self.new_block(lbls[i]);
            let Some(layout) = self.layout_of(*ty) else {
                // This type is uninhabited, which we could check up front
                // but it's not important.
                self.emit_jump(lbls[i + 1]);
                continue;
            };

            let offset = builder.add(&layout);

            let left = self.offset(left_base.clone(), offset as u32);
            let right = self.offset(right_base.clone(), offset as u32);

            let out = self.call_eq_by_ptr(left, right, *ty);
            self.emit_switch(out, vec![(1, lbls[i + 1])], false_lbl);
        }

        self.new_block(*lbls.last().unwrap());
        self.emit_write(
            Var {
                scope: self.function_scope,
                kind: VarKind::Return,
            }
            .into(),
            IrValue::Bool(true).into(),
        );
        self.emit_return();

        self.new_block(false_lbl);
        self.emit_write(
            Var {
                scope: self.function_scope,
                kind: VarKind::Return,
            }
            .into(),
            IrValue::Bool(false).into(),
        );
        self.emit_return();
    }

    fn generate_eq_body_enum(
        &mut self,
        left_base: Var,
        right_base: Var,
        variants: &[(Identifier, Vec<TyRef>)],
    ) {
        let current_label = self.current_label();
        let lbl_prefix = self
            .ctx
            .label_store
            .wrap_internal(current_label, "eq".into());

        let ident = Identifier::from("match");
        let match_lbl = self.ctx.label_store.wrap_internal(lbl_prefix, ident);

        let ident = Identifier::from("false");
        let false_lbl = self.ctx.label_store.wrap_internal(lbl_prefix, ident);

        let variant_lbls: Vec<LabelRef> = (0..variants.len())
            .map(|i| {
                let ident = Identifier::from(&format!("variant_{i}"));
                self.ctx.label_store.wrap_internal(lbl_prefix, ident)
            })
            .collect();

        // Read the left discriminant
        let left_discriminant = self.new_tmp(IrType::U8);
        self.emit(Instruction::Read {
            to: left_discriminant.clone(),
            from: left_base.clone().into(),
            ty: IrType::U8,
        });

        // Read the right discriminant
        let right_discriminant = self.new_tmp(IrType::U8);
        self.emit(Instruction::Read {
            to: right_discriminant.clone(),
            from: right_base.clone().into(),
            ty: IrType::U8,
        });

        // If the discriminants are equal, we have to look at the fields,
        // otherwise we immediately return false.
        let discriminants_equal = self.new_tmp(IrType::U8);
        self.emit(Instruction::IntCmp {
            to: discriminants_equal.clone(),
            cmp: IntCmp::Eq,
            left: left_discriminant.clone().into(),
            right: right_discriminant.into(),
        });

        self.emit_switch(
            discriminants_equal.into(),
            vec![(1, match_lbl)],
            false_lbl,
        );

        // This block matches on the left discriminant.
        self.new_block(match_lbl);

        let branches: Vec<(usize, LabelRef)> =
            variant_lbls.iter().copied().enumerate().collect();
        self.emit_switch(
            left_discriminant.into(),
            branches.clone(),
            false_lbl,
        );

        for (idx, variant_lbl) in branches {
            let variant = &variants[idx];

            let lbls: Vec<LabelRef> = (0..=variant.1.len())
                .map(|i| {
                    let ident = Identifier::from(&format!("field_{i}"));
                    self.ctx.label_store.wrap_internal(variant_lbl, ident)
                })
                .collect();

            let Some(layouts) = variant
                .1
                .iter()
                .map(|ty| {
                    let layout = self.layout_of(*ty)?;
                    Some((ty, layout))
                })
                .collect::<Option<Vec<_>>>()
            else {
                // If one of the items is uninhabited then we don't have to do anything here
                self.emit_write(
                    Var {
                        scope: self.function_scope,
                        kind: VarKind::Return,
                    }
                    .into(),
                    IrValue::Bool(true).into(),
                );
                self.emit_return();
                continue;
            };

            self.new_block(variant_lbl);
            self.emit_jump(lbls[0]);

            let mut builder = LayoutBuilder::new();
            builder.add(&Layout::of::<u8>());
            for (i, (ty, layout)) in layouts.iter().enumerate() {
                self.new_block(lbls[i]);
                let offset = builder.add(layout);

                let left = self.offset(left_base.clone(), offset as u32);
                let right = self.offset(right_base.clone(), offset as u32);

                let out = self.call_eq_by_ptr(left, right, **ty);
                self.emit_switch(out, vec![(1, lbls[i + 1])], false_lbl);
            }

            self.new_block(*lbls.last().unwrap());
            self.emit_write(
                Var {
                    scope: self.function_scope,
                    kind: VarKind::Return,
                }
                .into(),
                IrValue::Bool(true).into(),
            );
            self.emit_return();
        }

        self.new_block(false_lbl);
        self.emit_write(
            Var {
                scope: self.function_scope,
                kind: VarKind::Return,
            }
            .into(),
            IrValue::Bool(false).into(),
        );
        self.emit_return();
    }

    fn generate_eq_runtime(
        &mut self,
        left_ptr: Var,
        right_ptr: Var,
        ty: TyRef,
    ) {
        let eq_fn = self.get_runtime_eq(ty).unwrap();
        let to = Var {
            scope: self.function_scope,
            kind: VarKind::Return,
        };
        self.emit(Instruction::Eq {
            to: to.into(),
            left: left_ptr.into(),
            right: right_ptr.into(),
            eq_fn,
        });
        self.emit_return();
    }

    fn get_runtime_eq(&mut self, ty: TyRef) -> Option<EqFn> {
        let ty = self.ctx.type_info.ty_pool.get(ty);
        let id = match ty {
            Ty::Runtime(id) => Some(*id),
            Ty::Primitive(Primitive::String) => {
                Some(TypeId::of::<RotoString>())
            }
            Ty::Primitive(Primitive::IpAddr) => Some(TypeId::of::<IpAddr>()),
            Ty::Primitive(Primitive::Prefix) => Some(TypeId::of::<Prefix>()),
            Ty::List(_) => Some(TypeId::of::<ErasedList>()),
            _ => None,
        };

        let id = id?;

        let ty = self.ctx.runtime.get_runtime_type(id).unwrap();

        Some(ty.eq_fn())
    }
}
