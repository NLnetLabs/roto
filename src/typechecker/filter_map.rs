use crate::ast;

use super::{scope::Scope, types::Type, TypeChecker, TypeError, TypeResult};

impl TypeChecker<'_> {
    pub fn filter_map(
        &mut self,
        scope: &Scope,
        filter_map: &ast::FilterMap,
    ) -> TypeResult<Type> {
        let ast::FilterMap {
            ty,
            ident: _,
            for_ident: _,
            with_kv,
            body:
                ast::FilterMapBody {
                    define,
                    expressions,
                    apply,
                },
        } = filter_map;

        let mut scope = scope.wrap();

        let args = self.with_clause(&mut scope, &with_kv)?;

        self.define_section(&mut scope, define)?;

        for expression in expressions {
            let (v, t) = match expression {
                ast::FilterMapExpr::Term(term_section) => {
                    self.term_section(&scope, term_section)?
                }
                ast::FilterMapExpr::Action(action_section) => {
                    self.action_section(&scope, action_section)?
                }
            };
            scope.insert_var(v, t)?;
        }

        if let Some(apply_section) = apply {
            self.apply_section(&scope, apply_section)?;
        }

        Ok(match ty {
            ast::FilterType::FilterMap => Type::FilterMap(args),
            ast::FilterType::Filter => Type::Filter(args),
        })
    }

    fn define_section(
        &mut self,
        scope: &mut Scope,
        define: &ast::Define,
    ) -> TypeResult<()> {
        let ast::Define {
            for_kv: _,
            with_kv,
            body:
                ast::DefineBody {
                    rx_tx_type,
                    use_ext_data: _,
                    assignments,
                },
        } = define;

        self.with_clause(scope, &with_kv)?;

        match rx_tx_type {
            ast::RxTxType::RxOnly(ast::TypeIdentField { field_name, ty }) => {
                let Some(ty) = self.get_type(ty) else {
                    return Err(TypeError::Simple {
                        description: "type for rx is not defined".into(),
                    });
                };
                scope.insert_var(field_name, ty)?;
            }
            ast::RxTxType::Split(rx, tx) => {
                let Some(ty) = self.get_type(&rx.ty) else {
                    return Err(TypeError::Simple {
                        description: "type for rx is not defined".into(),
                    });
                };
                scope.insert_var(&rx.field_name, ty)?;

                let Some(ty) = self.get_type(&tx.ty) else {
                    return Err(TypeError::Simple {
                        description: "type for tx is not defined".into(),
                    });
                };
                scope.insert_var(&tx.field_name, ty)?;
            }
            ast::RxTxType::PassThrough(ast::TypeIdentField {
                field_name,
                ty,
            }) => {
                let Some(ty) = self.get_type(ty) else {
                    return Err(TypeError::Simple {
                        description: "type for rx_tx is not defined".into(),
                    });
                };
                scope.insert_var(field_name, ty)?;
            }
        }

        for (ident, expr) in assignments {
            let t = self.expr(&scope, expr)?;
            scope.insert_var(ident, t)?;
        }

        Ok(())
    }

    fn term_section(
        &mut self,
        scope: &Scope,
        term_section: &ast::TermSection,
    ) -> TypeResult<(String, Type)> {
        let ast::TermSection {
            ident,
            for_kv: _,
            with_kv,
            body: ast::TermBody { scopes },
        } = term_section;

        let mut scope = scope.wrap();

        let args = self.with_clause(&mut scope, &with_kv)?;

        for ast::TermScope {
            scope: _,
            operator,
            match_arms,
        } in scopes
        {
            match operator {
                ast::MatchOperator::Match => {
                    for (pattern, exprs) in match_arms {
                        assert!(pattern.is_none(), "ICE");
                        for expr in exprs {
                            // We ignore the result because it
                            // must be boolean.
                            self.logical_expr(&scope, expr)?;
                        }
                    }
                }
                ast::MatchOperator::MatchValueWith(ast::Identifier {
                    ident,
                }) => {
                    let x = scope.get_var(ident)?;
                    let Type::Enum(_, variants) = self.resolve_type(x) else {
                        return Err(TypeError::Simple {
                            description: format!(
                                "Cannot match on the type '{x:?}', \
                                because only matching on enums is supported."
                            ),
                        });
                    };

                    // We'll keep track of used variants to do some basic
                    // exhaustiveness checking. Only a warning for now.
                    let mut used_variants = Vec::<&str>::new();

                    for (pattern, exprs) in match_arms {
                        let ast::TermPatternMatchArm {
                            variant_id,
                            data_field,
                        } = pattern.as_ref().unwrap();

                        let variant_id = &variant_id.ident;

                        let Some(idx) = variants.iter().position(|(v, _)| {
                            v.as_str() == variant_id.as_str()
                        }) else {
                            return Err(TypeError::Simple {
                                description: format!("The variant {variant_id} does not exist on {x:?}")
                            });
                        };

                        if used_variants.contains(&variant_id.as_str()) {
                            println!("WARNING: Variant '{variant_id} occurs multiple times");
                        }

                        used_variants.push(variant_id.as_str());

                        let ty = &variants[idx].1;

                        let mut inner_scope = scope.wrap();

                        match (ty, data_field) {
                            (None, None) => {
                                // ok!
                            }
                            (Some(t), Some(id)) => {
                                inner_scope.insert_var(id, t)?;
                            }
                            (None, Some(_)) => return Err(
                                TypeError::Simple {
                                    description: "Got field for variant that doesn't have one"
                                    .into(),
                                }
                            ),
                            (Some(_), None) => {
                                return Err(TypeError::Simple {
                                    description: "Pattern should have a field".into()
                                })
                            }
                        }

                        for expr in exprs {
                            // We ignore the result because it
                            // must be boolean.
                            self.logical_expr(&inner_scope, expr)?;
                        }
                    }
                }
                _ => unreachable!("The grammar should have forbidden this."),
            }
        }

        Ok((ident.to_string(), Type::Term(args)))
    }

    fn action_section(
        &mut self,
        scope: &Scope,
        action_section: &ast::ActionSection,
    ) -> TypeResult<(String, Type)> {
        let ast::ActionSection {
            ident,
            with_kv,
            body: ast::ActionSectionBody { expressions },
        } = action_section;

        let mut inner_scope = scope.wrap();

        let args = self.with_clause(&mut inner_scope, &with_kv)?;

        for expr in expressions {
            let _t = self.compute_expr(&inner_scope, expr)?;
        }

        Ok((ident.to_string(), Type::Action(args)))
    }

    fn apply_section(
        &mut self,
        scope: &Scope,
        apply_section: &ast::ApplySection,
    ) -> TypeResult<()> {
        let ast::ApplySection {
            for_kv: _,
            with_kv: _,
            body:
                ast::ApplyBody {
                    scopes,
                    accept_reject: _,
                },
        } = apply_section;

        for ast::ApplyScope {
            scope: apply_scope,
            match_action,
        } in scopes
        {
            assert!(apply_scope.is_none(), "not implemented yet");
            match match_action {
                ast::MatchActionExpr::FilterMatchAction(
                    ast::FilterMatchActionExpr {
                        operator: _,
                        filter_ident,
                        negate: _,
                        actions,
                    },
                ) => {
                    let ty = self.expr(&scope, filter_ident)?;
                    self.unify(&ty, &Type::Term(vec![]))?;
                    for action in actions {
                        match action {
                            (None, None) | (Some(_), Some(_)) => {
                                unreachable!(
                                    "The grammar should have forbidden this."
                                )
                            }
                            (None, Some(_)) => {
                                // do nothing
                            }
                            (Some(expr), None) => {
                                self.expr(&scope, expr)?;
                            }
                        }
                    }
                }
                ast::MatchActionExpr::PatternMatchAction(
                    ast::PatternMatchActionExpr {
                        operator,
                        match_arms,
                    },
                ) => {
                    let ast::MatchOperator::MatchValueWith(x) = operator
                    else {
                        unreachable!(
                            "The grammar should have forbidden this."
                        )
                    };
                    let x = scope.get_var(x)?;
                    let Type::Enum(_, variants) =
                        self.resolve_type(x).clone()
                    else {
                        return Err(TypeError::Simple {
                            description: format!("Cannot match on the type '{x:?}', because only matching on enums is supported.")
                        });
                    };

                    // We'll keep track of used variants to do some basic
                    // exhaustiveness checking. Only a warning for now.
                    let mut used_variants = Vec::<&str>::new();

                    for ast::PatternMatchActionArm {
                        variant_id,
                        data_field,
                        guard,
                        actions,
                    } in match_arms
                    {
                        let variant_id = &variant_id.ident;
                        let Some(idx) = variants.iter().position(|(v, _)| {
                            v.as_str() == variant_id.as_str()
                        }) else {
                            return Err(TypeError::Simple {
                                description: format!("The variant {variant_id} does not exist on {x:?}")
                            });
                        };

                        if used_variants.contains(&variant_id.as_str()) {
                            println!("WARNING: variant {variant_id} appears multiple times.");
                        }

                        let ty = &variants[idx].1;

                        let mut inner_scope = scope.wrap();

                        match (ty, data_field) {
                            (None, None) => {
                                // ok!
                            }
                            (Some(t), Some(id)) => {
                                inner_scope.insert_var(id, t)?;
                            }
                            (None, Some(_)) => return Err(
                                TypeError::Simple {
                                    description: "Got field for variant that doesn't have one"
                                    .into(),
                                }
                            ),
                            (Some(_), None) => {
                                return Err(TypeError::Simple {
                                    description: "Pattern should have a field".into()
                                })
                            }
                        }

                        if let Some(guard) = guard {
                            let ast::TermCallExpr { term_id, args } = guard;
                            let Type::Term(term_params) =
                                scope.get_var(term_id)?
                            else {
                                return Err(TypeError::Simple {
                                    description: "Should be a term".into(),
                                });
                            };

                            match args {
                                Some(ast::ArgExprList { args }) => {
                                    if args.len() != term_params.len() {
                                        return Err(TypeError::Simple {
                                            description: "Number of arguments do not match".into(),
                                        });
                                    }
                                    for (arg, (_, param)) in
                                        args.into_iter().zip(term_params)
                                    {
                                        let ty =
                                            self.expr(&inner_scope, arg)?;
                                        self.unify(&ty, param)?;
                                    }
                                }
                                None => {
                                    if !term_params.is_empty() {
                                        return Err(TypeError::Simple {
                                            description: "Term takes params but none were given.".into()
                                        });
                                    }
                                }
                            }
                        } else {
                            // If there is a guard we don't remove the variant
                            // because it might appear again.
                            used_variants.push(variant_id.as_str());
                        }

                        for action in actions {
                            match action {
                                (Some(_), Some(_)) | (None, None) => {
                                    unreachable!("The grammar should have forbidden this.")
                                }
                                (
                                    Some(ast::ActionCallExpr {
                                        action_id,
                                        args,
                                    }),
                                    None,
                                ) => {
                                    let Type::Action(term_params) =
                                        scope.get_var(action_id)?
                                    else {
                                        return Err(TypeError::Simple {
                                            description:
                                                "Should be an action".into(),
                                        });
                                    };

                                    match args {
                                        Some(ast::ArgExprList { args }) => {
                                            if args.len() != term_params.len()
                                            {
                                                return Err(TypeError::Simple {
                                                    description: "Number of arguments do not match".into(),
                                                }
                                                );
                                            }
                                            for (arg, (_, param)) in args
                                                .into_iter()
                                                .zip(term_params)
                                            {
                                                let ty = self.expr(
                                                    &inner_scope,
                                                    arg,
                                                )?;
                                                self.unify(&ty, param)?;
                                            }
                                        }
                                        None => {
                                            if !term_params.is_empty() {
                                                return Err(TypeError::Simple {
                                                    description: "Term takes params but none were given.".into()
                                                });
                                            }
                                        }
                                    }
                                }
                                (None, Some(_accept_reject)) => {
                                    // always ok (for now)
                                }
                            }
                        }
                    }

                    for (v, _) in variants {
                        if !used_variants.contains(&v.as_str()) {
                            println!(
                                "WARNING: Variant {} is not covered in enum.",
                                v.as_str()
                            );
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn with_clause(
        &mut self,
        scope: &mut Scope,
        args: &[ast::TypeIdentField],
    ) -> TypeResult<Vec<(String, Type)>> {
        args.into_iter()
            .map(|ast::TypeIdentField { field_name, ty }| {
                let Some(ty) = self.get_type(ty) else {
                    return Err(TypeError::Simple {
                        description: "type does not exist".to_string()
                    });
                };

                scope.insert_var(&field_name, ty)?;
                Ok((field_name.ident.to_string(), ty.clone()))
            })
            .collect()
    }
}
