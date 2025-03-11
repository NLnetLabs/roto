//! Lowering a match expression

use std::collections::{HashMap, HashSet};

use crate::{
    ast::{self, Identifier, Match, Pattern},
    parser::meta::Meta,
    runtime::layout::{Layout, LayoutBuilder},
    typechecker::types::Type,
};

use super::{
    ir::{Instruction, Operand, Var, VarKind},
    label::LabelRef,
    value::IrType,
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
    pub fn match_expr(&mut self, m: &Meta<Match>) -> Option<Operand> {
        let ast::Match { expr, arms } = &m.node;

        let ty = self.type_info.type_of(expr);

        let Type::Name(type_name) = ty else { panic!() };
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
        let all_discriminants: HashSet<_> = branches
            .iter()
            .filter_map(|(d, _, _)| *d)
            .map(|d| {
                let ident = Identifier::from(&format!("case_{d}"));
                let lbl = self.label_store.wrap_internal(lbl_prefix, ident);
                (d, lbl)
            })
            .collect();

        let switch_branches = all_discriminants.iter().cloned().collect();

        // We need to know for the switch whether there are any default
        // branches. So start with this check
        let default_branches: Vec<_> =
            branches.iter().filter(|(d, _, _)| d.is_none()).collect();

        // We need some expression, otherwise typechecking would have failed.
        let Some(Operand::Place(op)) = self.expr(expr) else {
            panic!();
        };
        let discriminant = self.new_tmp();
        self.add(Instruction::Read {
            to: discriminant.clone(),
            from: op.clone().into(),
            ty: IrType::U8,
        });
        self.add(Instruction::Switch {
            examinee: discriminant.into(),
            branches: switch_branches,
            default: if default_branches.is_empty() {
                continue_lbl
            } else {
                default_lbl
            },
        });

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
            self.match_case(op.clone(), lbl, &branches, &arm_labels);
        }

        if !default_branches.is_empty() {
            self.match_case(op, default_lbl, &default_branches, &arm_labels);
        }

        // Here we finally create all the blocks for the expression of each
        // arm.
        let out = self.new_tmp();
        let mut any_assigned = false;
        for (_, arm, arm_index) in branches {
            self.new_block(arm_labels[&arm_index]);
            let ty = self.type_info.type_of(&arm.body);
            let val = self.block(&arm.body);
            if let Some(val) = val {
                if let Some(ty) = self.lower_type(&ty) {
                    self.add(Instruction::Assign {
                        to: out.clone(),
                        val,
                        ty,
                    });
                }
                any_assigned = true;
            }
            self.add(Instruction::Jump(continue_lbl));
        }

        self.new_block(continue_lbl);

        if any_assigned {
            Some(out.into())
        } else {
            None
        }
    }

    fn match_case(
        &mut self,
        examinee: Var,
        lbl: LabelRef,
        branches: &[&(Option<usize>, &ast::MatchArm, usize)],
        arm_labels: &HashMap<usize, LabelRef>,
    ) {
        self.new_block(lbl);

        let ident = Identifier::from("guard_0");
        let guard_lbl = self.label_store.wrap_internal(lbl, ident);
        self.add(Instruction::Jump(guard_lbl));

        let mut next_lbl = guard_lbl;

        for (i, (_, arm, arm_index)) in branches.iter().enumerate() {
            let guard_lbl = next_lbl;
            self.new_block(guard_lbl);

            if let Pattern::EnumVariant {
                fields: Some(fields),
                variant: _,
            } = &arm.pattern.node
            {
                let mut b = LayoutBuilder::new();
                b.add(&Layout::of::<u8>());
                for var in &**fields {
                    let ty = self.type_info.type_of(var);
                    let name = self.type_info.resolved_name(var);

                    // The offset of the field is (at least) 1 because of the
                    // discriminant.
                    let offset =
                        b.add(&self.type_info.layout_of(&ty, self.runtime));

                    let val = self.read_field(
                        examinee.clone().into(),
                        offset as u32,
                        &ty,
                    );

                    if let Some(val) = val {
                        let ty = self.lower_type(&ty).unwrap();
                        self.add(Instruction::Assign {
                            to: Var {
                                scope: name.scope,
                                kind: VarKind::Explicit(name.ident),
                            },
                            val,
                            ty,
                        });
                    }
                }
            }

            let ident = Identifier::from(format!("guard_{}", i + 1));
            next_lbl = self.label_store.wrap_internal(lbl, ident);

            let arm_label = arm_labels[arm_index];
            if let Some(guard) = &arm.guard {
                let op = self.expr(guard).unwrap();

                self.add(Instruction::Switch {
                    examinee: op,
                    branches: vec![(1, arm_label)],
                    default: next_lbl,
                });
            } else {
                self.add(Instruction::Jump(arm_label));
            }
        }
    }
}
