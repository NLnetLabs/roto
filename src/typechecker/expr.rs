use std::collections::HashSet;

use crate::{
    ast::{self, Identifier},
    parser::span::{Span, Spanned},
    typechecker::error,
};

use super::{
    scope::Scope,
    types::{Arrow, Method, Primitive, Type},
    TypeChecker, TypeResult,
};

#[derive(Clone)]
pub struct Context {
    pub expected_type: Type,
    pub function_return_type: Option<Type>,
}

impl TypeChecker<'_> {
    pub fn block(
        &mut self,
        scope: &Scope,
        ctx: &Context,
        block: &ast::Block,
    ) -> TypeResult<bool> {
        let mut diverged = false;

        for expr in &block.exprs {
            if diverged {
                return Err(error::unreachable_expression(expr));
            }
            let ctx = Context {
                expected_type: Type::Primitive(Primitive::Unit),
                ..ctx.clone()
            };

            diverged |= self.expr(scope, &ctx, expr)?;
        }

        // TODO: check return type
        let Some(expr) = &block.last else {
            return Ok(diverged);
        };

        if diverged {
            return Err(error::unreachable_expression(expr));
        }
        diverged |= self.expr(scope, ctx, expr)?;
        Ok(diverged)
    }

    pub fn expr(
        &mut self,
        scope: &Scope,
        ctx: &Context,
        expr: &Spanned<ast::Expr>,
    ) -> TypeResult<bool> {
        use ast::Expr::*;
        let span = expr.span;
        match &expr.inner {
            Literal(l) => self.literal(ctx, l),
            Match(m) => self.match_expr(scope, ctx, m),
            PrefixMatch(_) => {
                self.unify(
                    &Type::Primitive(Primitive::Bool),
                    &ctx.expected_type,
                    span,
                    None,
                )?;
                Ok(false)
            }
            FunctionCall(name, args) => {
                let t = scope.get_var(name)?;
                let t = self.resolve_type(t);

                let (call_type, params, ret) = match t {
                    Type::Term(params) => ("term", params, Type::Primitive(Primitive::Bool)),
                    Type::Action(params) => ("action", params, Type::Primitive(Primitive::Unit)),
                    t => {
                        return Err(error::simple(
                            format!("the variable `{name}` is not callable, but has type `{t}`"),
                            "not a term or an action",
                            name.span,
                        ))
                    }
                };

                let params: Vec<_> =
                    params.into_iter().map(|(_, t)| t).collect();
                let diverges = self.check_arguments(
                    scope, ctx, call_type, name, &params, args,
                )?;

                self.unify(&ctx.expected_type, &ret, span, None)?;
                Ok(diverges)
            }
            MethodCall(receiver, name, args) => {
                if let ast::Expr::Var(x) = &receiver.inner {
                    if let Some(ty) = self.get_type(x) {
                        let ty = ty.clone();
                        return self
                            .static_method_call(scope, ctx, &ty, name, args);
                    }
                }
                self.method_call(scope, ctx, receiver, name, args)
            }
            Access(e, x) => {
                let t = self.fresh_var();
                let diverges = self.expr(
                    scope,
                    &Context {
                        expected_type: t.clone(),
                        ..ctx.clone()
                    },
                    e,
                )?;
                let t = self.resolve_type(&t);
                if let Type::Record(fields)
                | Type::NamedRecord(_, fields)
                | Type::RecordVar(_, fields) = &t
                {
                    if let Some((_, t)) =
                        fields.iter().find(|(s, _)| s == x.0.as_str())
                    {
                        self.unify(&ctx.expected_type, t, x.span, None)?;
                        return Ok(diverges);
                    };
                }
                return Err(error::simple(
                    format!("no field `{x}` on type `{t}`",),
                    format!("unknown field `{x}`"),
                    x.span,
                ));
            }
            Var(x) => {
                let t = scope.get_var(x)?;
                self.unify(&ctx.expected_type, &t, x.span, None)?;
                Ok(false)
            }
            Record(record) => {
                let field_types: Vec<_> = record
                    .fields
                    .iter()
                    .map(|(s, _)| (s, self.fresh_var()))
                    .collect();
                let rec = self.fresh_record(field_types.clone());
                self.unify(&ctx.expected_type, &rec, span, None)?;

                self.record_fields(scope, ctx, field_types, record, span)
            }
            TypedRecord(name, record) => {
                // We first retrieve the type we expect
                let Some(ty) = self.types.get(&name.0.to_string()) else {
                    return Err(error::undeclared_type(name));
                };
                let ty = ty.clone();
                self.unify(&ctx.expected_type, &ty, span, None)?;

                // This check is redundant, but might give better error messages.
                let Type::NamedRecord(record_name, record_fields) = ty else {
                    return Err(error::simple(
                        format!("Expected a named record type, but found `{name}`"),
                        "not a named record type",
                        name.span,
                    ));
                };

                let diverges = self.record_fields(
                    scope,
                    ctx,
                    record_fields.clone(),
                    record,
                    span,
                )?;

                // Infer the type based on the given expression
                let field_types: Vec<_> = record
                    .fields
                    .iter()
                    .map(|(s, _)| (s, self.fresh_var()))
                    .collect();
                let rec = self.fresh_record(field_types.clone());
                self.unify(&ctx.expected_type, &rec, span, None)?;

                Ok(diverges)
            }
            List(es) => {
                let var = self.fresh_var();
                let ty = Type::List(Box::new(var.clone()));
                self.unify(&ctx.expected_type, &ty, span, None)?;

                let mut diverges = false;
                let ctx = Context {
                    expected_type: var,
                    ..ctx.clone()
                };

                for e in es {
                    diverges |= self.expr(scope, &ctx, e)?;
                }

                Ok(diverges)
            }
            Not(e) => {
                self.unify(
                    &ctx.expected_type,
                    &Type::Primitive(Primitive::Bool),
                    span,
                    None,
                )?;
                self.expr(
                    scope,
                    &Context {
                        expected_type: Type::Primitive(Primitive::Bool),
                        ..ctx.clone()
                    },
                    e,
                )
            }
            BinOp(left, op, right) => self.binop(scope, ctx, op, left, right),
            Return(_) => todo!(),
            IfElse(c, t, e) => {
                self.expr(
                    scope,
                    &Context {
                        expected_type: Type::Primitive(Primitive::Bool),
                        ..ctx.clone()
                    },
                    c,
                )?;

                if let Some(e) = e {
                    let var = self.fresh_var();
                    let ctx = Context {
                        expected_type: var,
                        ..ctx.clone()
                    };

                    let mut diverges = false;
                    diverges |= self.block(scope, &ctx, t)?;
                    diverges |= self.block(scope, &ctx, e)?;
                    Ok(diverges)
                } else {
                    self.unify(
                        &ctx.expected_type,
                        &Type::Primitive(Primitive::Unit),
                        span,
                        None,
                    )?;

                    // An if without else does not always diverge, because
                    // the condition could be false
                    let _ = self.block(
                        scope,
                        &Context {
                            expected_type: Type::Primitive(Primitive::Unit),
                            ..ctx.clone()
                        },
                        t,
                    );
                    Ok(false)
                }
            }
        }
    }

    fn literal(
        &mut self,
        ctx: &Context,
        lit: &Spanned<ast::Literal>,
    ) -> TypeResult<bool> {
        use ast::Literal::*;
        let span = lit.span;

        let t = match lit.inner {
            Accept | Reject => Type::Primitive(Primitive::Verdict),
            String(_) => Type::Primitive(Primitive::String),
            Prefix(_) => Type::Primitive(Primitive::Prefix),
            PrefixLength(_) => Type::Primitive(Primitive::PrefixLength),
            Asn(_) => Type::Primitive(Primitive::AsNumber),
            IpAddress(_) => Type::Primitive(Primitive::IpAddress),
            ExtendedCommunity(_) | StandardCommunity(_)
            | LargeCommunity(_) => Type::Primitive(Primitive::Community),
            Bool(_) => Type::Primitive(Primitive::Bool),
            Integer(_) => self.fresh_int(),
        };

        self.unify(&ctx.expected_type, &t, span, None)?;
        Ok(false)
    }

    fn match_expr(
        &mut self,
        scope: &Scope,
        ctx: &Context,
        mat: &Spanned<ast::Match>,
    ) -> TypeResult<bool> {
        let span = mat.span;
        let ast::Match { expr, arms } = &mat.inner;

        let diverges;

        let t_expr = {
            let examinee_type = self.fresh_var();
            let ctx = Context {
                expected_type: examinee_type.clone(),
                ..ctx.clone()
            };
            diverges = self.expr(scope, &ctx, expr)?;
            self.resolve_type(&examinee_type)
        };

        if diverges {
            todo!("make a pretty error")
        }

        let Type::Enum(_, variants) = &t_expr else {
            return Err(error::can_only_match_on_enum(&t_expr, expr.span));
        };

        // We'll keep track of used variants to do some basic
        // exhaustiveness checking.
        let mut used_variants = Vec::<&str>::new();

        // Match diverges if all its branches diverge
        let mut arms_diverge = true;

        for ast::MatchArm {
            variant_id,
            data_field,
            guard,
            body,
        } in arms
        {
            let variant_str = variant_id.0.as_str();
            let Some(idx) =
                variants.iter().position(|(v, _)| v.as_str() == variant_str)
            else {
                return Err(error::variant_does_not_exist(
                    variant_id, &t_expr,
                ));
            };

            let variant_already_used = used_variants.contains(&variant_str);
            if variant_already_used {
                println!("WARNING: Variant occurs multiple times in match! This arm is unreachable")
            }

            let ty = &variants[idx].1;
            let mut arm_scope = scope.wrap();

            match (ty, data_field) {
                (None, None) => {} // ok!
                (Some(t), Some(id)) => {
                    arm_scope.insert_var(id, t)?;
                }
                (None, Some(data_field)) => {
                    return Err(error::variant_does_not_have_field(
                        data_field, &t_expr,
                    ))
                }
                (Some(_), None) => {
                    return Err(error::need_data_field_on_pattern(
                        variant_id, &t_expr,
                    ));
                }
            }

            if let Some(guard) = guard {
                let ctx = Context {
                    expected_type: Type::Primitive(Primitive::Bool),
                    ..ctx.clone()
                };
                let _ = self.expr(&arm_scope, &ctx, guard)?;
            } else if !variant_already_used {
                // If there is a guard we don't mark the variant as used,
                // because the guard could evaluate to false and hence the
                // variant _should_ actually be used again.
                used_variants.push(variant_str);
            }

            arms_diverge &= self.block(&arm_scope, ctx, body)?;
        }

        if used_variants.len() < variants.len() {
            let mut missing_variants = Vec::new();
            for v in variants {
                let v = v.0.as_str();
                if !used_variants.contains(&v) {
                    missing_variants.push(v);
                }
            }
            return Err(error::nonexhaustive_match(span, &missing_variants));
        }

        Ok(arms_diverge)
    }

    fn binop(
        &mut self,
        scope: &Scope,
        ctx: &Context,
        op: &ast::BinOp,
        left: &Spanned<ast::Expr>,
        right: &Spanned<ast::Expr>,
    ) -> TypeResult<bool> {
        use ast::BinOp::*;

        self.unify(
            &Type::Primitive(Primitive::Bool),
            &ctx.expected_type,
            left.span.merge(right.span),
            None,
        )?;

        match op {
            And | Or => {
                let ctx = Context {
                    expected_type: Type::Primitive(Primitive::Bool),
                    ..ctx.clone()
                };

                let mut diverges = false;
                diverges |= self.expr(scope, &ctx, left)?;
                diverges |= self.expr(scope, &ctx, right)?;
                Ok(diverges)
            }
            Lt | Le | Gt | Ge => {
                let ctx = Context {
                    expected_type: self.fresh_int(),
                    ..ctx.clone()
                };

                let mut diverges = false;
                diverges |= self.expr(scope, &ctx, left)?;
                diverges |= self.expr(scope, &ctx, right)?;
                Ok(diverges)
            }
            Eq | Ne => {
                let ctx = Context {
                    expected_type: self.fresh_var(),
                    ..ctx.clone()
                };

                let mut diverges = false;
                diverges |= self.expr(scope, &ctx, left)?;
                diverges |= self.expr(scope, &ctx, right)?;
                Ok(diverges)
            }
            In | NotIn => {
                let ty = self.fresh_var();

                let mut diverges = false;
                diverges |= self.expr(
                    scope,
                    &Context {
                        expected_type: ty.clone(),
                        ..ctx.clone()
                    },
                    left,
                )?;
                diverges |= self.expr(
                    scope,
                    &Context {
                        expected_type: Type::List(Box::new(ty.clone())),
                        ..ctx.clone()
                    },
                    left,
                )?;

                Ok(diverges)
            }
        }
    }

    fn method_call(
        &mut self,
        scope: &Scope,
        ctx: &Context,
        receiver: &Spanned<ast::Expr>,
        name: &Spanned<Identifier>,
        args: &[Spanned<ast::Expr>],
    ) -> TypeResult<bool> {
        let t = self.fresh_var();
        let mut diverges = self.expr(
            scope,
            &Context {
                expected_type: t.clone(),
                ..ctx.clone()
            },
            receiver,
        )?;

        let Some(arrow) = self.find_method(self.methods, &t, name.as_ref())
        else {
            return Err(error::simple(
                format!("method `{name}` not found on `{t}`",),
                format!("method not found for `{t}`"),
                name.span,
            ));
        };

        self.unify(&arrow.rec, &t, name.span, None)?;

        diverges |= self.check_arguments(
            scope,
            ctx,
            "method",
            name,
            &arrow.args,
            args,
        )?;

        Ok(diverges)
    }

    fn static_method_call(
        &mut self,
        scope: &Scope,
        ctx: &Context,
        ty: &Type,
        name: &Spanned<Identifier>,
        args: &[Spanned<ast::Expr>],
    ) -> TypeResult<bool> {
        let Some(arrow) =
            self.find_method(self.static_methods, ty, name.as_ref())
        else {
            return Err(error::simple(
                format!("static method `{name}` not found on `{ty}`",),
                format!("static method not found on `{ty}`"),
                name.span,
            ));
        };

        self.unify(&ctx.expected_type, &arrow.ret, name.span, None)?;

        let diverges = self.check_arguments(
            scope,
            ctx,
            "static method",
            name,
            &arrow.args,
            args,
        )?;

        Ok(diverges)
    }

    fn check_arguments(
        &mut self,
        scope: &Scope,
        ctx: &Context,
        call_type: &str,
        name: &Spanned<Identifier>,
        params: &[Type],
        args: &[Spanned<ast::Expr>],
    ) -> TypeResult<bool> {
        if args.len() != params.len() {
            return Err(error::number_of_arguments_dont_match(
                call_type,
                &name,
                params.len(),
                args.len(),
            ));
        }

        let mut diverges = false;
        for (arg, ty) in args.iter().zip(params) {
            diverges |= self.expr(
                scope,
                &Context {
                    expected_type: ty.clone(),
                    ..ctx.clone()
                },
                arg,
            )?;
        }

        Ok(diverges)
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

    fn record_fields(
        &mut self,
        scope: &Scope,
        ctx: &Context,
        field_types: Vec<(impl AsRef<str>, Type)>,
        record: &ast::Record,
        span: Span,
    ) -> TypeResult<bool> {
        let mut used_fields = HashSet::<&str>::new();
        let mut missing_fields: HashSet<_> = field_types.iter().map(|x| x.0.as_ref()).collect();
        let mut invalid_fields = Vec::new();
        let mut duplicate_fields = Vec::new();
        
        for (ident, _) in &record.fields {
            if used_fields.contains(ident.as_ref()) {
                duplicate_fields.push(ident);
            } else if missing_fields.contains(ident.as_ref()) {
                missing_fields.remove(ident.as_ref());
                used_fields.insert(ident.as_ref());
            } else {
                invalid_fields.push(ident);
            }
        }

        if !invalid_fields.is_empty() || !duplicate_fields.is_empty() || !missing_fields.is_empty() {
            return Err(error::field_mismatch(span, invalid_fields, duplicate_fields, missing_fields));
        }

        let mut diverges = false;
        for (ident, expr) in &record.fields {
            let expected_type = field_types
                .iter()
                .find_map(|(s, t)| (s.as_ref() == ident.0).then_some(t))
                .unwrap()
                .clone();

            diverges |= self.expr(
                scope,
                &Context {
                    expected_type,
                    ..ctx.clone()
                },
                expr,
            )?;
        }
        Ok(diverges)
    }
}
