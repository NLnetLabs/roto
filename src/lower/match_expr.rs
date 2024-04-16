use std::collections::HashMap;

use crate::{ast::{self, Match}, parser::meta::Meta, typechecker::types::Type};

use super::{ir::{Instruction, Operand, Var}, value::SafeValue, Lowerer};

impl Lowerer {
    pub fn match_expr(&mut self, m: &Meta<Match>) -> Operand<Var, SafeValue> {
        let ast::Match { expr, arms } = &m.node;

        let ty = self.type_info.type_of(expr);
        let Type::Enum(_, variants) = ty else {
            panic!()
        };

        let mut branches = HashMap::new();
        let mut default_arms = Vec::new();

        for arm in arms {
            if arm.variant_id.0 == "_" {
                default_arms.push(arm);
                continue;
            }

            let discriminant = variants
                .iter()
                .position(|(s, _)| s == &arm.variant_id.0)
                .unwrap();

            let (_, arms) = branches
                .entry(discriminant)
                .or_insert((&arm.variant_id.0, Vec::new()));
            arms.push(arm);
        }

        let mut branches: Vec<_> = branches.into_iter().collect();
        branches.sort_by(|a, b| a.0.cmp(&b.0));

        let switch_branches: Vec<_> = branches
            .iter()
            .map(|(d, (l, _))| (d.clone(), (*l).clone()))
            .collect();

        let default_lbl = self.new_unique_block_name("match::$default");
        let continue_lbl = self.new_unique_block_name("match::$continue");

        let op = self.expr(expr);
        self.add(Instruction::Switch {
            examinee: op.clone(),
            branches: switch_branches,
            default: if default_arms.is_empty() {
                continue_lbl.clone()
            } else {
                default_lbl.clone()
            },
        });

        let out = self.new_tmp();

        for (_, (lbl, arms)) in branches {
            self.new_block(&lbl);
            self.add(Instruction::Jump(format!("{lbl}_0")));
            for (i, arm) in arms.iter().enumerate() {
                self.new_block(&format!("{lbl}_{i}"));
                if let Some(var) = &arm.data_field {
                    let var = self.type_info.full_name(var);
                    self.add(Instruction::AccessEnum {
                        to: Var { var },
                        from: op.clone(),
                    })
                }
                let accept = format!("{lbl}_{i}_accept");
                if let Some(guard) = &arm.guard {
                    let op = self.expr(&guard);
                    let next_lbl = if i == arms.len() - 1 {
                        default_lbl.clone()
                    } else {
                        format!("{lbl}_{}", i + 1)
                    };
                    self.add(Instruction::Switch {
                        examinee: op,
                        branches: vec![(1, accept.clone())],
                        default: next_lbl,
                    });
                    self.new_block(&accept);
                }
                let op = self.block(&arm.body);
                if let Some(op) = op {
                    self.add(Instruction::Assign {
                        to: out.clone(),
                        val: op,
                    });
                }
                self.add(Instruction::Jump(continue_lbl.clone()));
            }
        }

        // TODO: guards on default
        if let Some(default_arm) = default_arms.first() {
            self.new_block(&default_lbl);
            let op = self.block(&default_arm.body);
            if let Some(op) = op {
                self.add(Instruction::Assign {
                    to: out.clone(),
                    val: op,
                });
            }
            self.add(Instruction::Jump(continue_lbl.clone()));
        }

        self.new_block(&continue_lbl);
        out.into()
    }
}
