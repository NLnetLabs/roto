use crate::ast::{self, AccessExpr, Define, DefineBody};

use super::{scope::Scope, ty::Type, typed, TypeChecker, TypeResult};

impl TypeChecker {
    pub fn filter_map(
        &mut self,
        scope: &Scope,
        filter_map: ast::FilterMap,
    ) -> TypeResult<typed::FilterMap> {
        let ast::FilterMap {
            ty: _,
            ident: _,
            for_ident: _,
            with_kv: _,
            body:
                ast::FilterMapBody {
                    define,
                    expressions,
                    apply: _,
                },
        } = filter_map;

        let mut scope = scope.wrap();

        // TODO: with_kv

        let Define {
            for_kv: _,
            with_kv: _,
            body:
                DefineBody {
                    rx_tx_type: _,
                    use_ext_data: _,
                    assignments,
                },
        } = define;

        for (ident, expr) in assignments {
            let t = self.expr(&scope, expr)?;
            scope.insert_var(ident.ident.to_string(), t.clone())?;
        }

        for expression in expressions {
            match expression {
                ast::FilterMapExpr::Term(ast::TermSection {
                    ident: _,
                    for_kv: _,
                    with_kv: _,
                    body: ast::TermBody { scopes },
                }) => {
                    for ast::TermScope {
                        scope: _,
                        operator,
                        match_arms,
                    } in scopes
                    {
                        use ast::MatchOperator::*;
                        match operator {
                            Match => {
                                for (_, exprs) in match_arms {
                                    for expr in exprs {
                                        // We ignore the result because it
                                        // must be boolean.
                                        let _ =
                                            self.logical_expr(&scope, expr)?;
                                    }
                                }
                            }
                            MatchValueWith(_) => todo!(),
                            Some | ExactlyOne | All => todo!(),
                        }
                    }
                }
                ast::FilterMapExpr::Action(ast::ActionSection {
                    ident: _,
                    with_kv: _,
                    body: ast::ActionSectionBody { expressions },
                }) => {
                    for expr in expressions {
                        let _t = self.compute_expr(&scope, expr)?;
                    }
                }
            }
        }

        // apply section

        Ok(typed::FilterMap {})
    }

    fn logical_expr(
        &mut self,
        scope: &Scope,
        expr: ast::LogicalExpr,
    ) -> TypeResult<Type> {
        match expr {
            ast::LogicalExpr::OrExpr(ast::OrExpr { left, right })
            | ast::LogicalExpr::AndExpr(ast::AndExpr { left, right }) => {
                self.boolean_expr(scope, left)?;
                self.boolean_expr(scope, right)?;
                Ok(Type::Bool)
            }
            ast::LogicalExpr::NotExpr(ast::NotExpr { expr })
            | ast::LogicalExpr::BooleanExpr(expr) => {
                self.boolean_expr(scope, expr)
            }
        }
    }

    fn boolean_expr(
        &mut self,
        scope: &Scope,
        expr: ast::BooleanExpr,
    ) -> TypeResult<Type> {
        match expr {
            ast::BooleanExpr::GroupedLogicalExpr(
                ast::GroupedLogicalExpr { expr },
            ) => {
                self.logical_expr(scope, *expr)?;
            }
            ast::BooleanExpr::CompareExpr(expr) => {
                let ast::CompareExpr { left, right, op: _ } = *expr;
                let t_left = self.compare_arg(scope, left)?;
                let t_right = self.compare_arg(scope, right)?;
                self.unify(&t_left, &t_right)?;
            }
            ast::BooleanExpr::ComputeExpr(expr) => {
                let ty = self.compute_expr(scope, expr)?;
                self.unify(&Type::Bool, &ty)?;
            }
            ast::BooleanExpr::LiteralAccessExpr(expr) => {
                let ty = self.literal_access(scope, expr)?;
                self.unify(&Type::Bool, &ty)?;
            }
            ast::BooleanExpr::ListCompareExpr(expr) => {
                let ast::ListCompareExpr { left, op: _, right } = *expr;
                let t_left = self.expr(scope, left)?;
                let t_right = self.expr(scope, right)?;
                self.unify(&Type::List(Box::new(t_left)), &t_right)?;
            }
            ast::BooleanExpr::PrefixMatchExpr(_)
            | ast::BooleanExpr::BooleanLiteral(_) => (),
        };
        Ok(Type::Bool)
    }

    fn compare_arg(
        &mut self,
        scope: &Scope,
        expr: ast::CompareArg,
    ) -> TypeResult<Type> {
        match expr {
            ast::CompareArg::ValueExpr(expr) => self.expr(scope, expr),
            ast::CompareArg::GroupedLogicalExpr(
                ast::GroupedLogicalExpr { expr },
            ) => self.logical_expr(scope, *expr),
        }
    }

    fn expr(
        &mut self,
        scope: &Scope,
        expr: ast::ValueExpr,
    ) -> TypeResult<Type> {
        use ast::ValueExpr::*;
        match expr {
            LiteralAccessExpr(x) => self.literal_access(scope, x),
            PrefixMatchExpr(_) => todo!(),
            ComputeExpr(x) => self.compute_expr(scope, x),
            RootMethodCallExpr(_) => todo!(),
            AnonymousRecordExpr(ast::AnonymousRecordValueExpr {
                key_values,
            }) => Ok(Type::Record(self.record_type(scope, key_values)?)),
            TypedRecordExpr(ast::TypedRecordValueExpr {
                type_id,
                key_values,
            }) => {
                // We first retrieve the type we expect
                let (record_name, mut record_type) = match self
                    .types
                    .get(&type_id.ident.to_string())
                {
                    Some(Type::NamedRecord(n, t)) => (n.clone(), t.clone()),
                    Some(_) => {
                        return Err(format!(
                            "{type_id} does not refer to a named record type"
                        ))
                    }
                    None => {
                        return Err(format!(
                            "The type {type_id} does not exist"
                        ))
                    }
                };

                // Infer the type based on the given expression
                let inferred_type = self.record_type(scope, key_values)?;

                for (name, inferred_type) in inferred_type {
                    let Some(idx) =
                        record_type.iter().position(|(n, _)| n == &name)
                    else {
                        return Err(format!("Type {record_name} does not have a field {name}."));
                    };
                    let (_, ty) = record_type.remove(idx);
                    self.unify(&inferred_type, &ty)?;
                }

                let missing: Vec<_> =
                    record_type.into_iter().map(|(s, _)| s).collect();
                if !missing.is_empty() {
                    return Err(format!(
                        "Missing fields on {record_name}: {}",
                        missing.join(", ")
                    ));
                }

                Ok(Type::Name(record_name.clone()))
            }
            ListExpr(_) => todo!(),
        }
    }

    fn access(
        &mut self,
        scope: &Scope,
        receiver: Type,
        access: Vec<AccessExpr>,
    ) -> TypeResult<Type> {
        let mut last = receiver;
        for a in access {
            match a {
                AccessExpr::MethodComputeExpr(ast::MethodComputeExpr {
                    ident,
                    args: ast::ArgExprList { args },
                }) => {
                    let Some(arrow) =
                        self.methods.clone().iter().find_map(|m| {
                            if ident != m.name {
                                return None;
                            }
                            let arrow = self.instantiate_method(m);
                            if self.subtype_of(&arrow.rec, &last) {
                                Some(arrow)
                            } else {
                                None
                            }
                        })
                    else {
                        return Err(format!("Method not found"));
                    };

                    if args.len() != arrow.args.len() {
                        return Err(format!(
                            "Number of arguments don't match"
                        ));
                    }

                    self.unify(&arrow.rec, &last)?;

                    for (arg, ty) in args.iter().zip(&arrow.args) {
                        let arg_ty = self.expr(scope, arg.clone())?;
                        self.unify(&arg_ty, &ty)?;
                    }
                    last = self.resolve_type(&arrow.ret).clone();
                }
                AccessExpr::FieldAccessExpr(ast::FieldAccessExpr {
                    field_names,
                }) => {
                    for field in field_names {
                        if let Type::Record(fields)
                        | Type::NamedRecord(_, fields) =
                            self.resolve_type(&last)
                        {
                            if let Some((_, t)) = fields
                                .iter()
                                .find(|(s, _)| s == field.ident.as_str())
                            {
                                last = t.clone();
                                continue;
                            };
                        }
                        return Err(format!("No such field"));
                    }
                }
            }
        }
        Ok(last)
    }

    fn literal_access(
        &mut self,
        scope: &Scope,
        expr: ast::LiteralAccessExpr,
    ) -> TypeResult<Type> {
        let ast::LiteralAccessExpr {
            literal,
            access_expr,
        } = expr;
        let literal = self.literal(literal)?;
        self.access(scope, literal, access_expr)
    }

    fn literal(&mut self, literal: ast::LiteralExpr) -> TypeResult<Type> {
        use ast::LiteralExpr::*;
        Ok(match literal {
            StringLiteral(_) => Type::String,
            PrefixLiteral(_) => Type::Prefix,
            PrefixLengthLiteral(_) => Type::PrefixLength,
            AsnLiteral(_) => Type::AsNumber,
            IpAddressLiteral(_) => Type::IpAddress,
            ExtendedCommunityLiteral(_)
            | StandardCommunityLiteral(_)
            | LargeCommunityLiteral(_) => todo!(),
            IntegerLiteral(_) | HexLiteral(_) => self.fresh_int(),
            BooleanLiteral(_) => Type::Bool,
        })
    }

    fn compute_expr(
        &mut self,
        scope: &Scope,
        expr: ast::ComputeExpr,
    ) -> TypeResult<Type> {
        let ast::ComputeExpr {
            receiver,
            access_expr,
        } = expr;
        let receiver_type = match receiver {
            ast::AccessReceiver::Ident(x) => {
                scope.get_var(&x.ident.to_string())?
            }
            ast::AccessReceiver::GlobalScope => todo!(),
        };
        self.access(scope, receiver_type.clone(), access_expr)
    }

    fn record_type(
        &mut self,
        scope: &Scope,
        expr: Vec<(ast::Identifier, ast::ValueExpr)>,
    ) -> TypeResult<Vec<(String, Type)>> {
        Ok(expr
            .into_iter()
            .map(|(k, v)| {
                self.expr(scope, v).map(|v| (k.ident.to_string(), v))
            })
            .collect::<Result<_, _>>()?)
    }
}
