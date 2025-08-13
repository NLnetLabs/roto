//! Lowering a match expression

use std::collections::{HashMap, HashSet};

use crate::{
    ast::{self, Identifier, Match, Pattern},
    ice,
    label::LabelRef,
    mir::{Place, Projection, Value},
    parser::meta::{Meta, MetaId},
    typechecker::types::{EnumVariant, Type},
};

use super::{
    super::{Var, VarKind},
    Lowerer,
};

impl Lowerer<'_> {
    /// Lower a match expression
    ///
    /// Lowering a match expression is quite tricky. Here's how we do it at
    /// the moment. Take this match expression:
    ///
    /// ```roto
    /// match x {
    ///     A(y) -> {}
    ///     B -> {}
    /// }
    /// ```
    ///
    /// The easiest thing to do is to treat it as one big if-else chain
    /// where we check whether each variant matches, but that is not
    /// particularly efficient. Instead, we should switch directly on the
    /// discriminant.
    ///
    /// First we evaluate `x` and then we go to a `switch` instruction,
    /// which has a label for each pattern. Simple enough. However, there
    /// are 2 things that make it more complicated: default patterns and
    /// guards.
    ///
    /// A guard requires us to do checks after switching on the
    /// discriminant. This expression for example:
    ///
    /// ```roto
    /// match x {
    ///     A(y) | y == 1 -> b1,
    ///     A(y) | y == 2 -> b2,
    ///     B -> b3,
    ///     A(y) -> b4,
    /// }
    /// ```
    ///
    /// Can be compiled like this expression:
    ///
    /// ```roto
    /// match x {
    ///     A(y) -> {
    ///         if y == 1 {
    ///             b1
    ///         } else if y == 2 {
    ///             b2
    ///         } else {
    ///             b4
    ///         }
    ///     }
    ///     B -> b3,
    /// }
    /// ```
    ///
    /// That means that we have to collect all the branches for variant `A`,
    /// to lower them together.
    ///
    /// Default patterns have to be added to each discriminant too. Here's
    /// a particularly interesting case:
    ///
    /// ```roto
    /// match x {
    ///     A(y) | c1 -> b1,
    ///     _ | c2 -> b2,
    ///     B | c3 -> b3,
    ///     _ -> b4,
    /// }
    /// ```
    ///
    /// This should be compiled equivalently to:
    ///
    /// ```roto
    /// match x {
    ///     A(y) -> {
    ///         if c1 { b1 }
    ///         else if c2 { b2 }
    ///         else { b4 }
    ///     }
    ///     B -> {
    ///         if c2 { b2 }
    ///         else if c3 { b3 }
    ///         else { b4 }
    ///     }
    /// }
    /// ```
    ///
    /// Note how the default patterns are added to the if-else chains for
    /// all possible discriminants.
    ///
    /// We do this by checking which variants occur in patterns and then
    /// making those chains for all branches that match that discriminant or
    /// are `_`.
    pub fn r#match(&mut self, id: MetaId, m: &Meta<Match>) -> Value {
        let ast::Match { expr, arms } = &m.node;

        let ty = self.type_info.type_of(expr);

        let Type::Name(type_name) = ty else {
            ice!("can only match on enums")
        };
        let type_def = self.type_info.resolve_type_name(&type_name);
        let variants = type_def.match_patterns(&type_name.arguments).unwrap();

        let current_label = self.current_label();
        let lbl_prefix = self
            .label_store
            .wrap_internal(current_label, Identifier::from("match"));

        let default_lbl = self
            .label_store
            .wrap_internal(lbl_prefix, Identifier::from("default"));
        let continue_lbl = self.label_store.next(current_label);

        // First collect all the information needed to create the switches
        // to arrive at the right arm
        let branches: Vec<_> = arms
            .iter()
            .enumerate()
            .map(|(i, arm)| {
                let discriminant = match &arm.pattern.node {
                    Pattern::EnumVariant { variant, .. } => Some(
                        variants
                            .iter()
                            .position(|s| s.name == variant.node)
                            .unwrap(),
                    ),
                    Pattern::Underscore => None,
                };
                (discriminant, arm, i)
            })
            .collect();

        // Get everything we need to match on in the outer layer
        // regardless of guards.
        let all_discriminants: HashSet<_> =
            branches.iter().filter_map(|(d, _, _)| *d).collect();

        let all_discriminants: HashMap<_, _> = all_discriminants
            .into_iter()
            .map(|d| {
                let ident = Identifier::from(&format!("case_{d}"));
                let lbl = self.label_store.wrap_internal(lbl_prefix, ident);
                (d, lbl)
            })
            .collect();

        let switch_branches = all_discriminants
            .iter()
            .map(|(d, lbl)| (*d, *lbl))
            .collect();

        // We need to know for the switch whether there are any default
        // branches. So start with this check
        let default_branches: Vec<_> =
            branches.iter().filter(|(d, _, _)| d.is_none()).collect();

        let examinee = self.expr(expr);
        let examinee_ty = self.type_info.type_of(expr);
        let examinee = self.assign_to_var(examinee, examinee_ty.clone());
        let discriminant = self.undropped_tmp();
        self.emit_assign(
            Place::new(discriminant.clone(), Type::u8()),
            Type::u8(),
            Value::Discriminant(examinee.clone()),
        );
        let default_branch = if !default_branches.is_empty() {
            Some(default_lbl)
        } else {
            None
        };
        self.emit_switch(discriminant, switch_branches, default_branch);

        let arm_labels: HashMap<_, _> = branches
            .iter()
            .map(|(_, _, idx)| {
                let s = format!("arm_{idx}");
                let ident = Identifier::from(s);
                (*idx, self.label_store.wrap_internal(lbl_prefix, ident))
            })
            .collect();

        for (discriminant, lbl) in all_discriminants {
            // Each discriminant gets the branches for itself and `_`.
            // See doc comment on this function for more information.
            let branches: Vec<_> = branches
                .iter()
                .filter(|(d, _, _)| *d == Some(discriminant) || d.is_none())
                .collect();
            self.match_case(
                examinee.clone(),
                &examinee_ty,
                Some(&variants[discriminant]),
                lbl,
                &branches,
                &arm_labels,
            );
        }

        if !default_branches.is_empty() {
            self.match_case(
                examinee,
                &examinee_ty,
                None,
                default_lbl,
                &default_branches,
                &arm_labels,
            );
        }

        // Here we finally create all the blocks for the expression of each
        // arm.
        let ty = self.type_info.type_of(id);
        let out = self.undropped_tmp();
        for (_, arm, arm_index) in branches {
            self.stack_slots.push(Vec::new());

            // Re-add the bindings generated by this pattern to the set of live
            // variables, so we drop them properly.
            if let Pattern::EnumVariant {
                variant: _,
                fields: Some(fields),
            } = &arm.pattern.node
            {
                for field_binding in fields.iter() {
                    let name = self.type_info.resolved_name(field_binding);
                    let ty = self.type_info.type_of(field_binding);
                    let var = Var {
                        scope: name.scope,
                        kind: VarKind::Explicit(name.ident),
                    };
                    self.stack_slots.last_mut().unwrap().push((var, ty));
                }
            }

            self.new_block(arm_labels[&arm_index]);
            let val = self.block(&arm.body);
            self.emit_assign(
                Place::new(out.clone(), ty.clone()),
                ty.clone(),
                val,
            );

            let to_drop = self.stack_slots.pop().unwrap();
            for (var, ty) in to_drop.into_iter().rev() {
                self.emit_drop(Place::new(var, ty.clone()), ty);
            }
            self.emit_jump(continue_lbl);
        }

        self.new_block(continue_lbl);
        self.add_live_variable(out.clone(), ty);

        Value::Move(out)
    }

    fn match_case(
        &mut self,
        examinee: Var,
        examinee_ty: &Type,
        variant: Option<&EnumVariant>,
        lbl: LabelRef,
        branches: &[&(Option<usize>, &ast::MatchArm, usize)],
        arm_labels: &HashMap<usize, LabelRef>,
    ) {
        self.new_block(lbl);

        let ident = Identifier::from(format!("guard_{}", 0));
        let guard_lbl = self.label_store.wrap_internal(lbl, ident);
        self.emit_jump(guard_lbl);

        let mut next_lbl = guard_lbl;

        for (i, (_, arm, arm_index)) in branches.iter().enumerate() {
            let guard_lbl = next_lbl;
            self.new_block(guard_lbl);
            self.stack_slots.push(Vec::new());

            // We can only extract the fields at each arm, we cannot combine
            // them unfortunately, since we cannot combine these patterns:
            //   Some(x) | Some(y) | Some(_) | _
            // with 1 extraction.
            //
            // The fields we extract get dropped in two places:
            //  - At the end of an arm that uses them.
            //  - When a guard expression evaluates to false.
            //
            // Here, that means that we drop when the guard evaluates to false
            // and otherwise just "forget".
            if let Pattern::EnumVariant {
                fields: Some(fields),
                variant: _,
            } = &arm.pattern.node
            {
                let variant = variant.unwrap();
                for (i, field_binding) in fields.iter().enumerate() {
                    let name = self.type_info.resolved_name(field_binding);
                    let ty = self.type_info.type_of(field_binding);
                    let var = Var {
                        scope: name.scope,
                        kind: VarKind::Explicit(name.ident),
                    };
                    self.add_live_variable(var.clone(), ty.clone());
                    self.do_assign(
                        Place::new(var, ty.clone()),
                        ty,
                        Value::Clone(Place {
                            var: examinee.clone(),
                            root_ty: examinee_ty.clone(),
                            projection: vec![Projection::VariantField(
                                variant.name,
                                i,
                            )],
                        }),
                    );
                }
            }

            let ident = Identifier::from(format!("guard_{}", i + 1));
            next_lbl = self.label_store.wrap_internal(lbl, ident);

            let arm_lbl = arm_labels[arm_index];

            // Even if we "forget" to drop the values, we still need to pop
            // them from the stack.
            let to_drop = self.stack_slots.pop().unwrap();

            if let Some(guard) = &arm.guard {
                let op = self.expr(guard);
                let op = self.assign_to_var(op, Type::bool());

                let ident = Identifier::from(format!("guard_{}_drop", i));
                let intermediate_lbl =
                    self.label_store.wrap_internal(lbl, ident);

                self.emit_switch(
                    op,
                    vec![(1, arm_lbl)],
                    Some(intermediate_lbl),
                );

                self.new_block(intermediate_lbl);

                for (var, ty) in to_drop.into_iter().rev() {
                    self.emit_drop(Place::new(var, ty.clone()), ty);
                }

                self.emit_jump(next_lbl);
            } else {
                self.emit_jump(arm_lbl);
            }
        }
    }
}
