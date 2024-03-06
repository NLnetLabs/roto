use crate::ast::{self, Define, DefineBody};

use super::{scope::Scope, typed, Type, TypeChecker, TypeResult};

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
                    for scope in scopes {
                        let ast::TermScope {
                            scope: _,
                            operator,
                            match_arms: _,
                        } = scope;

                        use ast::MatchOperator::*;
                        match operator {
                            Match => todo!(),
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

    fn expr(&self, scope: &Scope, expr: ast::ValueExpr) -> TypeResult<Type> {
        use ast::ValueExpr::*;
        match expr {
            LiteralAccessExpr(x) => self.literal_access(x),
            PrefixMatchExpr(_) => todo!(),
            ComputeExpr(x) => self.compute_expr(scope, x),
            RootMethodCallExpr(_) => todo!(),
            AnonymousRecordExpr(_) => todo!(),
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
                    if !self.types_equal(&inferred_type, &ty) {
                        return Err(format!("Types for field {name} of {record_name} don't match."));
                    }
                }

                let missing: Vec<_> = record_type.into_iter().map(|(s,_)| s).collect();
                if !missing.is_empty() {
                    return Err(format!("Missing fields on {record_name}: {}", missing.join(", ")))
                }

                Ok(Type::Name(record_name.clone()))
            }
            ListExpr(_) => todo!(),
        }
    }

    fn literal_access(
        &self,
        expr: ast::LiteralAccessExpr,
    ) -> TypeResult<Type> {
        let ast::LiteralAccessExpr {
            literal,
            access_expr,
        } = expr;
        let lit_type = self.literal(literal);
        if !access_expr.is_empty() {
            todo!()
        }
        lit_type
    }

    fn literal(&self, literal: ast::LiteralExpr) -> TypeResult<Type> {
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
            IntegerLiteral(_) | HexLiteral(_) => todo!(),
            BooleanLiteral(_) => Type::Bool,
        })
    }

    fn compute_expr(
        &self,
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
        if !access_expr.is_empty() {
            todo!()
        }
        Ok(receiver_type.clone())
    }

    fn record_type(
        &self,
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
