use std::collections::HashSet;

use crate::{
    ast::{self, Match},
    parser::meta::Meta,
    typechecker::types::Type,
};

use super::{
    ir::{Instruction, Operand, Var},
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
    pub fn match_expr(&mut self, m: &Meta<Match>) -> Operand {
        let ast::Match { expr, arms } = &m.node;

        let ty = self.type_info.type_of(expr);
        let Type::Enum(_, variants) = ty else {
            panic!("Should have been caught in typechecking")
        };

        let lbl_prefix = self.new_unique_block_name("$match");

        let default_lbl = format!("{lbl_prefix}_default");
        let continue_lbl = format!("{lbl_prefix}_continue");

        // First collect all the information needed to create the switches
        // to arrive at the right arm
        let branches: Vec<_> = arms
            .iter()
            .enumerate()
            .map(|(i, arm)| {
                let variant = &arm.variant_id.0;
                let discriminant = if variant == "_" {
                    None
                } else {
                    Some(
                        variants
                            .iter()
                            .position(|(s, _)| s == variant)
                            .unwrap(),
                    )
                };
                (discriminant, arm, i)
            })
            .collect();

        // Get everything we need to match on in the outer layer
        // regardless of guards.
        let all_discriminants: HashSet<_> = branches
            .iter()
            .filter_map(|(d, _, _)| *d)
            .map(|d| (d, format!("{lbl_prefix}_case_{d}")))
            .collect();

        let switch_branches = all_discriminants.iter().cloned().collect();

        // We need to know for the switch whether there are any default
        // branches. So start with this check
        let default_branches: Vec<_> =
            branches.iter().filter(|(d, _, _)| d.is_none()).collect();

        let op = self.expr(expr);
        self.add(Instruction::Switch {
            examinee: op.clone(),
            branches: switch_branches,
            default: if default_branches.is_empty() {
                continue_lbl.clone()
            } else {
                default_lbl.clone()
            },
        });

        for (discriminant, lbl) in all_discriminants {
            // Each discriminant gets the branches for itself and `_`.
            // See doc comment on this function for more information.
            let branches: Vec<_> = branches
                .iter()
                .filter(|(d, _, _)| *d == Some(discriminant) || d.is_none())
                .collect();
            self.match_case(op.clone(), &lbl_prefix, lbl, &branches);
        }

        if !default_branches.is_empty() {
            self.match_case(op, &lbl_prefix, default_lbl, &default_branches);
        }

        // Here we finally create all the blocks for the expression of each
        // arm.
        let out = self.new_tmp();
        for (_, arm, arm_index) in branches {
            self.new_block(&format!("{lbl_prefix}_arm_{arm_index}"));
            let val = self.block(&arm.body);
            if let Some(val) = val {
                self.add(Instruction::Assign {
                    to: out.clone(),
                    val,
                });
            }
            self.add(Instruction::Jump(continue_lbl.clone()));
        }

        self.new_block(&continue_lbl);
        out.into()
    }

    fn match_case(
        &mut self,
        examinee: Operand,
        lbl_prefix: &str,
        lbl: String,
        branches: &[&(Option<usize>, &ast::MatchArm, usize)],
    ) {
        self.new_block(&lbl);
        self.add(Instruction::Jump(format!("{lbl}_guard_0")));
        for (i, (_, arm, arm_index)) in branches.iter().enumerate() {
            self.new_block(&format!("{lbl}_guard_{i}"));

            if let Some(var) = &arm.data_field {
                let var = self.type_info.full_name(var);
                self.add(Instruction::AccessEnum {
                    to: Var { var },
                    from: examinee.clone(),
                })
            }

            if let Some(guard) = &arm.guard {
                let op = self.expr(guard);
                self.add(Instruction::Switch {
                    examinee: op,
                    branches: vec![(
                        1,
                        format!("{lbl_prefix}_arm_{arm_index}"),
                    )],
                    default: format!("{lbl}_guard_{}", i + 1),
                });
            } else {
                self.add(Instruction::Jump(format!(
                    "{lbl_prefix}_arm_{arm_index}"
                )));
            }
        }
    }
}
