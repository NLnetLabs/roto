use std::{borrow::Borrow, collections::HashSet};

use crate::{
    ast::{self, Identifier},
    parser::meta::{Meta, MetaId},
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

impl Context {
    fn with_type(&self, t: impl Borrow<Type>) -> Self {
        Context {
            expected_type: t.borrow().clone(),
            ..self.clone()
        }
    }
}

impl TypeChecker<'_> {
    pub fn block(
        &mut self,
        scope: &Scope,
        ctx: &Context,
        block: &Meta<ast::Block>,
    ) -> TypeResult<bool> {
        let mut diverged = false;

        for expr in &block.exprs {
            if diverged {
                return Err(error::unreachable_expression(expr));
            }

            let ctx = ctx.with_type(Type::Primitive(Primitive::Unit));
            diverged |= self.expr(scope, &ctx, expr)?;
        }

        let Some(expr) = &block.last else {
            if !diverged {
                self.unify(
                    &ctx.expected_type,
                    &Type::Primitive(Primitive::Unit),
                    block.id,
                    None,
                )?;
            }
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
        expr: &Meta<ast::Expr>,
    ) -> TypeResult<bool> {
        use ast::Expr::*;
        let id = expr.id;

        // Store the type for use in the lowering step
        self.type_info
            .expr_types
            .insert(id, ctx.expected_type.clone());

        match &expr.node {
            Accept | Reject => {
                let Some(ret) = &ctx.function_return_type else {
                    let s = if let Accept = expr.node {
                        "accept"
                    } else {
                        "reject"
                    };
                    return Err(error::cannot_diverge_here(s, expr));
                };
                self.unify(
                    ret,
                    &Type::Primitive(Primitive::Verdict),
                    id,
                    None,
                )?;
                Ok(true)
            }
            Literal(l) => self.literal(ctx, l),
            Match(m) => self.match_expr(scope, ctx, m),
            PrefixMatch(_) => {
                self.unify(
                    &Type::Primitive(Primitive::Bool),
                    &ctx.expected_type,
                    id,
                    None,
                )?;
                Ok(false)
            }
            FunctionCall(name, args) => {
                let t = self.get_var(scope, name)?;
                let t = self.resolve_type(t);

                let (call_type, params, ret) = match t {
                    Type::Term(params) => ("term", params, Type::Primitive(Primitive::Bool)),
                    Type::Action(params) => ("action", params, Type::Primitive(Primitive::Unit)),
                    t => {
                        return Err(error::simple(
                            format!("the variable `{name}` is not callable, but has type `{t}`"),
                            "not a term or an action",
                            name.id,
                        ))
                    }
                };

                let params: Vec<_> =
                    params.into_iter().map(|(_, t)| t).collect();
                let diverges = self.check_arguments(
                    scope, ctx, call_type, name, &params, args,
                )?;

                self.unify(&ctx.expected_type, &ret, id, None)?;
                Ok(diverges)
            }
            MethodCall(receiver, name, args) => {
                // This could be an enum variant constructor, so we check that too
                if let Some((type_name, _, data)) =
                    self.find_enum_variant(ctx, id, receiver, name)?
                {
                    let Some(data) = data else {
                        return Err(error::simple(
                            format!("variant {name} of {type_name} does not have data"),
                            "does not have data",
                            name.id,
                        ));
                    };

                    let [arg] = &args.node[..] else {
                        return Err(error::simple(
                            format!("enum constructor must have exactly 1 argument"),
                            "must have exactly 1 argument",
                            args.id,
                        ));
                    };

                    let ctx = &Context {
                        expected_type: data.clone(),
                        ..ctx.clone()
                    };
                    return self.expr(scope, ctx, arg);
                }

                if let ast::Expr::Var(x) = &receiver.node {
                    if let Some(ty) = self.get_type(x) {
                        let ty = ty.clone();
                        return self
                            .static_method_call(scope, ctx, &ty, name, args);
                    }
                }
                self.method_call(scope, ctx, receiver, name, args)
            }
            Access(e, x) => {
                // This could be an enum variant constructor, so we check that too
                if let Some((name, _, data)) =
                    self.find_enum_variant(ctx, id, e, x)?
                {
                    if data.is_some() {
                        return Err(error::simple(
                            format!("variant {x} of {name} requires data"),
                            "requires data",
                            x.id,
                        ));
                    }

                    return Ok(false);
                }

                let t = self.fresh_var();
                let diverges = self.expr(scope, &ctx.with_type(&t), e)?;
                let t = self.resolve_type(&t);

                if let Type::Record(fields)
                | Type::NamedRecord(_, fields)
                | Type::RecordVar(_, fields) = &t
                {
                    if let Some((_, t)) =
                        fields.iter().find(|(s, _)| s == x.0.as_str())
                    {
                        self.unify(&ctx.expected_type, t, x.id, None)?;
                        return Ok(diverges);
                    };
                }

                Err(error::simple(
                    format!("no field `{x}` on type `{t}`",),
                    format!("unknown field `{x}`"),
                    x.id,
                ))
            }
            Var(x) => {
                let t = self.get_var(scope, x)?;
                self.unify(&ctx.expected_type, t, x.id, None)?;
                Ok(false)
            }
            Record(record) => {
                let field_types: Vec<_> = record
                    .fields
                    .iter()
                    .map(|(s, _)| (s, self.fresh_var()))
                    .collect();
                let rec = self.fresh_record(field_types.clone());
                self.unify(&ctx.expected_type, &rec, id, None)?;

                self.record_fields(scope, ctx, field_types, record, id)
            }
            TypedRecord(name, record) => {
                // We first retrieve the type we expect
                let Some(ty) = self.type_info.types.get(&name.0.to_string())
                else {
                    return Err(error::undeclared_type(name));
                };
                let ty = ty.clone();
                self.unify(&ctx.expected_type, &ty, id, None)?;

                let Type::NamedRecord(_, record_fields) = ty else {
                    return Err(error::simple(
                        format!("Expected a named record type, but found `{name}`"),
                        "not a named record type",
                        name.id,
                    ));
                };

                let diverges = self.record_fields(
                    scope,
                    ctx,
                    record_fields.clone(),
                    record,
                    id,
                )?;

                // Infer the type based on the given expression
                let field_types: Vec<_> = record
                    .fields
                    .iter()
                    .map(|(s, _)| (s, self.fresh_var()))
                    .collect();
                let rec = self.fresh_record(field_types.clone());
                self.unify(&ctx.expected_type, &rec, id, None)?;

                Ok(diverges)
            }
            List(es) => {
                let var = self.fresh_var();
                let ty = Type::List(Box::new(var.clone()));
                self.unify(&ctx.expected_type, &ty, id, None)?;

                let mut diverges = false;
                let ctx = ctx.with_type(var);

                for e in es {
                    diverges |= self.expr(scope, &ctx, e)?;
                }

                Ok(diverges)
            }
            Not(e) => {
                self.unify(
                    &ctx.expected_type,
                    &Type::Primitive(Primitive::Bool),
                    id,
                    None,
                )?;
                self.expr(
                    scope,
                    &ctx.with_type(Type::Primitive(Primitive::Bool)),
                    e,
                )
            }
            BinOp(left, op, right) => {
                self.binop(scope, ctx, op, id, left, right)
            }
            Return(e) => {
                let Some(ret) = &ctx.function_return_type else {
                    return Err(error::cannot_diverge_here("return", expr));
                };
                self.unify(&ctx.expected_type, &Type::Never, id, None)?;
                self.expr(scope, &ctx.with_type(ret), e)?;
                Ok(true)
            }
            IfElse(c, t, e) => {
                self.expr(
                    scope,
                    &ctx.with_type(Type::Primitive(Primitive::Bool)),
                    c,
                )?;

                if let Some(e) = e {
                    let var = self.fresh_var();
                    let ctx = ctx.with_type(var);

                    let mut diverges = false;
                    diverges |= self.block(scope, &ctx, t)?;
                    diverges |= self.block(scope, &ctx, e)?;
                    Ok(diverges)
                } else {
                    self.unify(
                        &ctx.expected_type,
                        &Type::Primitive(Primitive::Unit),
                        id,
                        None,
                    )?;

                    // An if without else does not always diverge, because
                    // the condition could be false
                    let _ = self.block(
                        scope,
                        &ctx.with_type(Type::Primitive(Primitive::Unit)),
                        t,
                    );
                    Ok(false)
                }
            }
        }
    }

    fn find_enum_variant(
        &mut self,
        ctx: &Context,
        id: MetaId,
        e: &Meta<ast::Expr>,
        x: &Meta<Identifier>,
    ) -> TypeResult<Option<(String, String, Option<Type>)>> {
        let ast::Expr::Var(ident) = &e.node else {
            return Ok(None);
        };

        let Some(t) = self.get_type(ident) else {
            return Ok(None);
        };

        let t = t.clone();

        let Type::Enum(name, variants) = &t else {
            return Ok(None);
        };

        let Some((variant, data)) =
            variants.iter().find(|(v, _)| v == &x.node.0)
        else {
            return Err(error::simple(
                format!("no variant {x} on enum {name}"),
                format!("variant not found"),
                x.id,
            ));
        };

        self.unify(&ctx.expected_type, &t, x.id, None)?;
        self.type_info
            .enum_variant_constructors
            .insert(id, t.clone());

        Ok(Some((name.clone(), variant.clone(), data.clone())))
    }

    fn literal(
        &mut self,
        ctx: &Context,
        lit: &Meta<ast::Literal>,
    ) -> TypeResult<bool> {
        use ast::Literal::*;
        let span = lit.id;

        let t = match lit.node {
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
        mat: &Meta<ast::Match>,
    ) -> TypeResult<bool> {
        let span = mat.id;
        let ast::Match { expr, arms } = &mat.node;

        let diverges;

        let t_expr = {
            let examinee_type = self.fresh_var();
            let ctx = ctx.with_type(&examinee_type);
            diverges = self.expr(scope, &ctx, expr)?;
            self.resolve_type(&examinee_type)
        };

        if diverges {
            todo!("make a pretty error")
        }

        let Type::Enum(_, variants) = &t_expr else {
            return Err(error::can_only_match_on_enum(&t_expr, expr.id));
        };

        // We'll keep track of used variants to do some basic
        // exhaustiveness checking.
        let mut used_variants = Vec::<&str>::new();

        // Match diverges if all its branches diverge
        let mut arms_diverge = true;

        // Whether there is a default arm present (with '_')
        let mut default_arm = false;

        for ast::MatchArm {
            variant_id,
            data_field,
            guard,
            body,
        } in arms
        {
            let variant_str = variant_id.0.as_str();
            
            // Anything after default is unreachable
            if default_arm {
                todo!("error")
            }

            if variant_str == "_" {
                if data_field.is_some() {
                    todo!("error")
                }

                let arm_scope = scope.wrap(&format!("$arm_default"));
                if let Some(guard) = guard {
                    let ctx = ctx.with_type(Type::Primitive(Primitive::Bool));
                    let _ = self.expr(&arm_scope, &ctx, guard)?;
                } else {
                    default_arm = true;
                }

                arms_diverge &= self.block(&arm_scope, ctx, body)?;
                continue;
            }

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
            let mut arm_scope = scope.wrap(&format!("$arm_{idx}"));

            match (ty, data_field) {
                (None, None) => {} // ok!
                (Some(t), Some(id)) => {
                    self.insert_var(&mut arm_scope, id, t)?;
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
                let ctx = ctx.with_type(Type::Primitive(Primitive::Bool));
                let _ = self.expr(&arm_scope, &ctx, guard)?;
            } else if !variant_already_used {
                // If there is a guard we don't mark the variant as used,
                // because the guard could evaluate to false and hence the
                // variant _should_ actually be used again.
                used_variants.push(variant_str);
            }

            arms_diverge &= self.block(&arm_scope, ctx, body)?;
        }

        if !default_arm && used_variants.len() < variants.len() {
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
        span: MetaId,
        left: &Meta<ast::Expr>,
        right: &Meta<ast::Expr>,
    ) -> TypeResult<bool> {
        use ast::BinOp::*;

        self.unify(
            &Type::Primitive(Primitive::Bool),
            &ctx.expected_type,
            span,
            None,
        )?;

        match op {
            And | Or => {
                let ctx = ctx.with_type(Type::Primitive(Primitive::Bool));

                let mut diverges = false;
                diverges |= self.expr(scope, &ctx, left)?;
                diverges |= self.expr(scope, &ctx, right)?;
                Ok(diverges)
            }
            Lt | Le | Gt | Ge => {
                let ctx = ctx.with_type(self.fresh_int());

                let mut diverges = false;
                diverges |= self.expr(scope, &ctx, left)?;
                diverges |= self.expr(scope, &ctx, right)?;
                Ok(diverges)
            }
            Eq | Ne => {
                let ctx = ctx.with_type(self.fresh_var());

                let mut diverges = false;
                diverges |= self.expr(scope, &ctx, left)?;
                diverges |= self.expr(scope, &ctx, right)?;
                Ok(diverges)
            }
            In | NotIn => {
                let ty = self.fresh_var();

                let mut diverges = false;
                diverges |= self.expr(scope, &ctx.with_type(&ty), left)?;
                diverges |= self.expr(
                    scope,
                    &ctx.with_type(Type::List(Box::new(ty))),
                    right,
                )?;

                Ok(diverges)
            }
        }
    }

    fn method_call(
        &mut self,
        scope: &Scope,
        ctx: &Context,
        receiver: &Meta<ast::Expr>,
        name: &Meta<Identifier>,
        args: &[Meta<ast::Expr>],
    ) -> TypeResult<bool> {
        let t = self.fresh_var();
        let mut diverges = self.expr(scope, &ctx.with_type(&t), receiver)?;

        let Some(arrow) = self.find_method(self.methods, &t, name.as_ref())
        else {
            return Err(error::simple(
                format!("method `{name}` not found on `{t}`",),
                format!("method not found for `{t}`"),
                name.id,
            ));
        };

        self.unify(&arrow.rec, &t, name.id, None)?;

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
        name: &Meta<Identifier>,
        args: &[Meta<ast::Expr>],
    ) -> TypeResult<bool> {
        let Some(arrow) =
            self.find_method(self.static_methods, ty, name.as_ref())
        else {
            return Err(error::simple(
                format!("static method `{name}` not found on `{ty}`",),
                format!("static method not found on `{ty}`"),
                name.id,
            ));
        };

        self.unify(&ctx.expected_type, &arrow.ret, name.id, None)?;

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
        name: &Meta<Identifier>,
        params: &[Type],
        args: &[Meta<ast::Expr>],
    ) -> TypeResult<bool> {
        if args.len() != params.len() {
            return Err(error::number_of_arguments_dont_match(
                call_type,
                name,
                params.len(),
                args.len(),
            ));
        }

        let mut diverges = false;
        for (arg, ty) in args.iter().zip(params) {
            diverges |= self.expr(scope, &ctx.with_type(ty), arg)?;
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
        span: MetaId,
    ) -> TypeResult<bool> {
        let mut used_fields = HashSet::<&str>::new();
        let mut missing_fields: HashSet<_> =
            field_types.iter().map(|x| x.0.as_ref()).collect();
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

        if !invalid_fields.is_empty()
            || !duplicate_fields.is_empty()
            || !missing_fields.is_empty()
        {
            return Err(error::field_mismatch(
                span,
                invalid_fields,
                duplicate_fields,
                missing_fields,
            ));
        }

        let mut diverges = false;
        for (ident, expr) in &record.fields {
            let expected_type = field_types
                .iter()
                .find_map(|(s, t)| (s.as_ref() == ident.0).then_some(t))
                .unwrap()
                .clone();

            diverges |=
                self.expr(scope, &ctx.with_type(expected_type), expr)?;
        }
        Ok(diverges)
    }
}
