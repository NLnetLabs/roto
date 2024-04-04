use crate::{
    ast::{self, Identifier},
    parser::span::{Spanned, WithSpan},
    typechecker::error,
};

use super::{
    scope::Scope,
    types::{Arrow, Method, Primitive, Type},
    TypeChecker, TypeResult,
};

impl TypeChecker<'_> {
    pub fn block(
        &mut self,
        scope: &Scope,
        block: &ast::Block,
    ) -> TypeResult<Type> {
        for expr in &block.exprs {
            self.expr(scope, expr)?;
        }
        if let Some(expr) = &block.last {
            self.expr(scope, expr)
        } else {
            Ok(Type::Primitive(Primitive::Unit))
        }
    }

    pub fn expr(
        &mut self,
        scope: &Scope,
        expr: &Spanned<ast::Expr>,
    ) -> TypeResult<Type> {
        use ast::Expr::*;
        let span = expr.span;
        match &expr.inner {
            Literal(l) => Ok(self.literal(l)),
            Match(m) => self.match_expr(scope, m),
            PrefixMatch(_) => Ok(Type::Primitive(Primitive::Bool)),
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

                let params: Vec<_> = params.into_iter().map(|(_, t)| t).collect();
                self.check_arguments(scope, call_type, name, &params, args)?;

                Ok(ret)
            }
            MethodCall(receiver, name, args) => {
                if let ast::Expr::Var(x) = &receiver.inner {
                    if let Some(ty) = self.get_type(x) {
                        let ty = ty.clone();
                        return self
                            .static_method_call(scope, &ty, name, args);
                    }
                }
                self.method_call(scope, receiver, name, args)
            }
            Access(e, x) => {
                let t = self.expr(scope, e)?;
                let t = self.resolve_type(&t);
                if let Type::Record(fields)
                | Type::NamedRecord(_, fields)
                | Type::RecordVar(_, fields) = &t
                {
                    if let Some((_, t)) =
                        fields.iter().find(|(s, _)| s == x.0.as_str())
                    {
                        return Ok(t.clone());
                    };
                }
                return Err(error::simple(
                    format!("no field `{x}` on type `{t}`",),
                    format!("unknown field `{x}`"),
                    x.span,
                ));
            }
            Var(x) => scope.get_var(x).map(Clone::clone),
            Record(fields) => {
                let fields = self.record_type(scope, &fields)?;
                Ok(self.fresh_record(fields))
            }
            TypedRecord(name, fields) => {
                // We first retrieve the type we expect
                let (record_name, mut record_type) = match self
                    .types
                    .get(&name.0.to_string())
                {
                    Some(Type::NamedRecord(n, t)) => (n.clone(), t.clone()),
                    Some(_) => {
                        return Err(error::simple(
                            &format!(
                            "Expected a named record type, but found `{name}`",
                        ),
                            "not a named record type",
                            name.span,
                        ))
                    }
                    None => {
                        return Err(error::undeclared_type(name))
                    }
                };

                // Infer the type based on the given expression
                let inferred_type = self.record_type(scope, &fields)?;

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
                    return Err(error::missing_fields(&missing, name, span));
                }

                Ok(Type::Name(record_name.clone()))
            }
            List(es) => {
                let var = self.fresh_var();
                for e in es {
                    let t = self.expr(scope, e)?;
                    self.unify(&var, &t, span, None)?;
                }
                Ok(self.resolve_type(&var))
            }
            Not(e) => {
                let t = self.expr(scope, e)?;
                self.unify(
                    &Type::Primitive(Primitive::Bool),
                    &t,
                    span,
                    None,
                )?;
                Ok(Type::Primitive(Primitive::Bool))
            }
            BinOp(left, op, right) => self.binop(scope, op, left, right),
            Return(_) => todo!(),
            IfElse(c, t, e) => {
                let c_ty = self.expr(scope, c)?;
                self.unify(
                    &Type::Primitive(Primitive::Bool),
                    &c_ty,
                    span,
                    None,
                )?;

                if let Some(e) = e {
                    let var = self.fresh_var();

                    let t_ty = self.block(scope, t)?;
                    self.unify(&var, &t_ty, span, None)?;

                    let e_ty = self.block(scope, e)?;
                    self.unify(&var, &e_ty, span, None)
                } else {
                    let t_ty = self.block(scope, t)?;
                    self.unify(
                        &Type::Primitive(Primitive::Unit),
                        &t_ty,
                        span,
                        None,
                    )?;
                    Ok(Type::Primitive(Primitive::Unit))
                }
            }
        }
    }

    fn literal(&mut self, lit: &ast::Literal) -> Type {
        use ast::Literal::*;
        Type::Primitive(match lit {
            Accept | Reject => Primitive::Verdict,
            String(_) => Primitive::String,
            Prefix(_) => Primitive::Prefix,
            PrefixLength(_) => Primitive::PrefixLength,
            Asn(_) => Primitive::AsNumber,
            IpAddress(_) => Primitive::IpAddress,
            ExtendedCommunity(_) | StandardCommunity(_)
            | LargeCommunity(_) => Primitive::Community,
            Bool(_) => Primitive::Bool,
            Integer(_) => return self.fresh_int(),
        })
    }

    fn match_expr(
        &mut self,
        scope: &Scope,
        mat: &Spanned<ast::Match>,
    ) -> TypeResult<Type> {
        let span = mat.span;
        let ast::Match { expr, arms } = &mat.inner;
        let t_expr = self.expr(scope, expr)?;
        let t_expr = self.resolve_type(&t_expr);

        let Type::Enum(_, variants) = &t_expr else {
            return Err(error::can_only_match_on_enum(&t_expr, expr.span));
        };

        // We'll keep track of used variants to do some basic
        // exhaustiveness checking.
        let mut used_variants = Vec::<&str>::new();

        let t_return = self.fresh_var();

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
                let t_guard = self.expr(&arm_scope, guard)?;
                self.unify(
                    &Type::Primitive(Primitive::Bool),
                    &t_guard,
                    guard.span,
                    None,
                )?;
            } else if !variant_already_used {
                // If there is a guard we don't mark the variant as used, because the guard
                // could evaluate to false and hence the variant _should_ actually be used
                // again.
                used_variants.push(variant_str);
            }

            let t_body = self.block(&arm_scope, body)?;
            self.unify(&t_return, &t_body, body.span, None)?;
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

        Ok(self.resolve_type(&t_return))
    }

    fn binop(
        &mut self,
        scope: &Scope,
        op: &ast::BinOp,
        left: &Spanned<ast::Expr>,
        right: &Spanned<ast::Expr>,
    ) -> TypeResult<Type> {
        use ast::BinOp::*;

        let t_left = self.expr(scope, left)?;
        let t_right = self.expr(scope, right)?;

        match op {
            And | Or => {
                self.unify(
                    &Type::Primitive(Primitive::Bool),
                    &t_left,
                    left.span,
                    None,
                )?;

                self.unify(
                    &Type::Primitive(Primitive::Bool),
                    &t_right,
                    right.span,
                    None,
                )?;

                Ok(Type::Primitive(Primitive::Bool))
            }
            Eq | Ne | Lt | Le | Gt | Ge => {
                self.unify(&t_left, &t_right, right.span, Some(left.span))?;
                Ok(Type::Primitive(Primitive::Bool))
            }
            In | NotIn => {
                let Type::List(t_right) = &t_right else {
                    return Err(error::expected_list(right.span, &t_right));
                };

                self.unify(&t_left, &t_right, left.span, None)?;
                Ok(Type::Primitive(Primitive::Bool))
            }
        }
    }

    fn method_call(
        &mut self,
        scope: &Scope,
        receiver: &Spanned<ast::Expr>,
        name: &Spanned<Identifier>,
        args: &[Spanned<ast::Expr>],
    ) -> TypeResult<Type> {
        let t = self.expr(scope, receiver)?;
        let Some(arrow) = self.find_method(self.methods, &t, name.as_ref())
        else {
            return Err(error::simple(
                format!("method `{name}` not found on `{t}`",),
                format!("method not found for `{t}`"),
                name.span,
            ));
        };

        self.unify(&arrow.rec, &t, name.span, None)?;

        self.check_arguments(scope, "method", name, &arrow.args, args)?;

        Ok(self.resolve_type(&arrow.ret).clone())
    }

    fn static_method_call(
        &mut self,
        scope: &Scope,
        ty: &Type,
        name: &Spanned<Identifier>,
        args: &[Spanned<ast::Expr>],
    ) -> TypeResult<Type> {
        let Some(arrow) =
            self.find_method(self.static_methods, ty, name.as_ref())
        else {
            return Err(error::simple(
                format!("static method `{name}` not found on `{ty}`",),
                format!("static method not found on `{ty}`"),
                name.span,
            ));
        };

        self.check_arguments(
            scope,
            "static method",
            name,
            &arrow.args,
            args,
        )?;

        Ok(self.resolve_type(&arrow.ret).clone())
    }

    fn check_arguments(
        &mut self,
        scope: &Scope,
        call_type: &str,
        name: &Spanned<Identifier>,
        params: &[Type],
        args: &[Spanned<ast::Expr>],
    ) -> TypeResult<()> {
        if args.len() != params.len() {
            return Err(error::number_of_arguments_dont_match(
                call_type,
                &name,
                params.len(),
                args.len(),
            ));
        }

        for (arg, ty) in args.iter().zip(params) {
            let arg_ty = self.expr(scope, arg)?;
            self.unify(&arg_ty, &ty, arg.span, None)?;
        }

        Ok(())
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

    fn record_type(
        &mut self,
        scope: &Scope,
        expr: &ast::Record,
    ) -> TypeResult<Vec<(Spanned<String>, Type)>> {
        Ok(expr
            .fields
            .iter()
            .map(|(k, v)| {
                self.expr(scope, v)
                    .map(|v| (k.0.to_string().with_span(k.span), v))
            })
            .collect::<Result<_, _>>()?)
    }
}
