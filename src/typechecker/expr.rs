use std::ops::Deref;

use crate::{
    ast,
    parser::span::{Spanned, WithSpan},
    typechecker::error,
};

use super::{
    scope::Scope,
    types::{Arrow, Method, Primitive, Type},
    TypeChecker, TypeResult,
};

impl TypeChecker<'_> {
    pub fn logical_expr(
        &mut self,
        scope: &Scope,
        expr: &ast::LogicalExpr,
    ) -> TypeResult<Type> {
        match expr {
            ast::LogicalExpr::OrExpr(ast::OrExpr { left, right })
            | ast::LogicalExpr::AndExpr(ast::AndExpr { left, right }) => {
                self.boolean_expr(scope, left)?;
                self.boolean_expr(scope, right)?;
                Ok(Type::Primitive(Primitive::Bool))
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
        expr: &ast::BooleanExpr,
    ) -> TypeResult<Type> {
        match expr {
            ast::BooleanExpr::GroupedLogicalExpr(
                ast::GroupedLogicalExpr { expr },
            ) => {
                self.logical_expr(scope, expr)?;
            }
            ast::BooleanExpr::CompareExpr(expr) => {
                let t_left = self.compare_arg(scope, &expr.left)?;
                let t_right = self.compare_arg(scope, &expr.right)?;
                self.unify(&t_left, &t_right, expr.right.span, Some(expr.left.span))?;
            }
            ast::BooleanExpr::ComputeExpr(expr) => {
                let ty = self.compute_expr(scope, expr)?;
                self.unify(
                    &Type::Primitive(Primitive::Bool),
                    &ty,
                    expr.span,
                    None,
                )?;
            }
            ast::BooleanExpr::LiteralAccessExpr(expr) => {
                let ty = self.literal_access(scope, expr)?;
                self.unify(
                    &Type::Primitive(Primitive::Bool),
                    &ty,
                    expr.span,
                    None,
                )?;
            }
            ast::BooleanExpr::ListCompareExpr(expr) => {
                let t_left = self.expr(scope, &expr.left)?;
                let t_right = self.expr(scope, &expr.right)?;
                self.unify(
                    &Type::List(Box::new(t_left)),
                    &t_right,
                    expr.right.span,
                    Some(expr.left.span),
                )?;
            }
            ast::BooleanExpr::PrefixMatchExpr(_)
            | ast::BooleanExpr::BooleanLiteral(_) => (),
        };
        Ok(Type::Primitive(Primitive::Bool))
    }

    fn compare_arg(
        &mut self,
        scope: &Scope,
        expr: &ast::CompareArg,
    ) -> TypeResult<Type> {
        match expr {
            ast::CompareArg::ValueExpr(expr) => self.expr(scope, expr),
            ast::CompareArg::GroupedLogicalExpr(expr) => {
                self.logical_expr(scope, &expr.expr)
            }
        }
    }

    pub fn expr(
        &mut self,
        scope: &Scope,
        expr: &ast::ValueExpr,
    ) -> TypeResult<Type> {
        use ast::ValueExpr::*;
        match expr {
            LiteralAccessExpr(x) => self.literal_access(scope, x),
            PrefixMatchExpr(_) => todo!(),
            ComputeExpr(x) => self.compute_expr(scope, x),
            RootMethodCallExpr(_) => todo!(),
            AnonymousRecordExpr(ast::AnonymousRecordValueExpr {
                key_values,
            }) => {
                let fields = self.record_type(scope, &key_values)?;
                Ok(self.fresh_record(fields))
            }
            TypedRecordExpr(record_expr) => {
                let record_span = record_expr.span;
                let ast::TypedRecordValueExpr {
                    type_id,
                    key_values,
                } = &record_expr.inner;

                // We first retrieve the type we expect
                let (record_name, mut record_type) = match self
                    .types
                    .get(&type_id.ident.to_string())
                {
                    Some(Type::NamedRecord(n, t)) => (n.clone(), t.clone()),
                    Some(_) => {
                        return Err(error::simple(
                            &format!(
                            "Expected a named record type, but found `{type_id}`",
                        ),
                            "not a named record type",
                            type_id.span,
                        ))
                    }
                    None => {
                        return Err(error::undeclared_type(type_id))
                    }
                };

                // Infer the type based on the given expression
                let inferred_type = self.record_type(scope, &key_values)?;

                for (name, inferred_type) in inferred_type {
                    let Some(idx) = record_type
                        .iter()
                        .position(|(n, _)| n == &name.inner)
                    else {
                        return Err(error::simple(
                            &format!("record `{record_name}` does not have a field `{name}`."),
                            &format!("`{record_name}` does not have this field"),
                            name.span
                        ));
                    };
                    let (_, ty) = record_type.remove(idx);
                    self.unify(&inferred_type, &ty, name.span, None)?;
                }

                let missing: Vec<_> =
                    record_type.into_iter().map(|(s, _)| s).collect();
                if !missing.is_empty() {
                    return Err(error::missing_fields(
                        &missing,
                        type_id,
                        type_id.span.merge(record_span),
                    ));
                }

                Ok(Type::Name(record_name.clone()))
            }
            ListExpr(ast::ListValueExpr { values }) => {
                let ret = self.fresh_var();
                for v in values.iter() {
                    let t = self.expr(scope, v)?;
                    self.unify(&ret, &t, v.span, None)?;
                }
                Ok(Type::List(Box::new(self.resolve_type(&ret).clone())))
            }
        }
    }

    fn access(
        &mut self,
        scope: &Scope,
        receiver: Type,
        access: &[impl Deref<Target = ast::AccessExpr>],
    ) -> TypeResult<Type> {
        let mut last = receiver;
        for a in access {
            match a.deref() {
                ast::AccessExpr::MethodComputeExpr(
                    ast::MethodComputeExpr {
                        ident,
                        args: ast::ArgExprList { args },
                    },
                ) => {
                    let Some(arrow) =
                        self.find_method(self.methods, &last, ident.as_ref())
                    else {
                        return Err(error::simple(
                            &format!(
                                "method `{ident}` not found on `{last}`",
                            ),
                            &format!("method not found for `{last}`"),
                            ident.span,
                        ));
                    };

                    if args.len() != arrow.args.len() {
                        return Err(error::number_of_arguments_dont_match(
                            "method",
                            &ident,
                            arrow.args.len(),
                            args.len(),
                        ));
                    }

                    self.unify(&arrow.rec, &last, ident.span, None)?;

                    for (arg, ty) in args.iter().zip(&arrow.args) {
                        let arg_ty = self.expr(scope, arg)?;
                        self.unify(&arg_ty, &ty, arg.span, None)?;
                    }
                    last = self.resolve_type(&arrow.ret).clone();
                }
                ast::AccessExpr::FieldAccessExpr(ast::FieldAccessExpr {
                    field_names,
                }) => {
                    for field in field_names {
                        if let Type::Record(fields)
                        | Type::NamedRecord(_, fields)
                        | Type::RecordVar(_, fields) =
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
                        return Err(error::simple(
                            &format!("no field `{field}` on type `{last}`",),
                            &format!("unknown field `{field}`"),
                            field.span,
                        ));
                    }
                }
            }
        }
        Ok(last)
    }

    fn find_method(
        &mut self,
        methods: &[Method],
        ty: &Type,
        name: &str,
    ) -> Option<Arrow> {
        methods.iter().find_map(|m| {
            if name != m.name {
                return None;
            }
            let arrow = self.instantiate_method(m);
            if self.subtype_of(&arrow.rec, ty) {
                Some(arrow)
            } else {
                None
            }
        })
    }

    fn literal_access(
        &mut self,
        scope: &Scope,
        expr: &ast::LiteralAccessExpr,
    ) -> TypeResult<Type> {
        let ast::LiteralAccessExpr {
            literal,
            access_expr,
        } = expr;
        let literal = self.literal(literal)?;
        self.access(scope, literal, access_expr)
    }

    fn literal(&mut self, literal: &ast::LiteralExpr) -> TypeResult<Type> {
        use ast::LiteralExpr::*;
        Ok(Type::Primitive(match literal {
            StringLiteral(_) => Primitive::String,
            PrefixLiteral(_) => Primitive::Prefix,
            PrefixLengthLiteral(_) => Primitive::PrefixLength,
            AsnLiteral(_) => Primitive::AsNumber,
            IpAddressLiteral(_) => Primitive::IpAddress,
            ExtendedCommunityLiteral(_)
            | StandardCommunityLiteral(_)
            | LargeCommunityLiteral(_) => Primitive::Community,
            BooleanLiteral(_) => Primitive::Bool,
            IntegerLiteral(_) | HexLiteral(_) => return Ok(self.fresh_int()),
        }))
    }

    pub fn compute_expr(
        &mut self,
        scope: &Scope,
        expr: &ast::ComputeExpr,
    ) -> TypeResult<Type> {
        let ast::ComputeExpr {
            receiver,
            access_expr,
        } = expr;
        match receiver {
            ast::AccessReceiver::Ident(x) => {
                // It might be a static method
                // TODO: This should be cleaned up
                if let Some(ty) = self.get_type(&x) {
                    let mut access_expr = access_expr.clone();
                    if access_expr.is_empty() {
                        return Err(error::simple(
                            "a type cannot appear on its own and must be followed by a method",
                            "must be followed by a method",
                            x.span,
                        ));
                    }
                    let m = match access_expr.remove(0).inner {
                        ast::AccessExpr::MethodComputeExpr(m) => m,
                        ast::AccessExpr::FieldAccessExpr(f) => {
                            return Err(error::simple(
                                &format!("`{x}` is a type and does not have any fields"),
                                "no field access possible on this type",
                                f.field_names[0].span,
                            ))
                        },
                    };
                    let receiver_type =
                        self.static_method_call(scope, ty.clone(), m)?;
                    self.access(scope, receiver_type, &access_expr)
                } else {
                    let receiver_type = scope.get_var(x)?.clone();
                    self.access(scope, receiver_type, access_expr)
                }
            }
            ast::AccessReceiver::GlobalScope => todo!(),
        }
    }

    fn static_method_call(
        &mut self,
        scope: &Scope,
        ty: Type,
        m: ast::MethodComputeExpr,
    ) -> TypeResult<Type> {
        let ast::MethodComputeExpr {
            ident,
            args: ast::ArgExprList { args },
        } = m;
        let Some(arrow) =
            self.find_method(self.static_methods, &ty, ident.as_ref())
        else {
            return Err(error::simple(
                &format!("no static method `{ident}` found for `{ty}`",),
                &format!("static method not found for `{ty}`"),
                ident.span,
            ));
        };

        if args.len() != arrow.args.len() {
            return Err(error::number_of_arguments_dont_match(
                "static method",
                &ident,
                arrow.args.len(),
                args.len(),
            ));
        }

        self.unify(&arrow.rec, &ty, ident.span, None)?;

        for (arg, ty) in args.iter().zip(&arrow.args) {
            let arg_ty = self.expr(scope, arg)?;
            self.unify(&arg_ty, &ty, arg.span, None)?;
        }
        Ok(self.resolve_type(&arrow.ret).clone())
    }

    fn record_type(
        &mut self,
        scope: &Scope,
        expr: &[(Spanned<ast::Identifier>, Spanned<ast::ValueExpr>)],
    ) -> TypeResult<Vec<(Spanned<String>, Type)>> {
        Ok(expr
            .into_iter()
            .map(|(k, v)| {
                self.expr(scope, v)
                    .map(|v| (k.ident.to_string().with_span(k.span), v))
            })
            .collect::<Result<_, _>>()?)
    }
}
