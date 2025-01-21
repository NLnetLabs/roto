//! Type checking of expressions

use std::{borrow::Borrow, collections::HashSet};

use crate::{
    ast::{self, Identifier, Pattern},
    parser::meta::{Meta, MetaId},
    typechecker::types::FunctionDefinition,
};

use super::{
    scope::{LocalScopeRef, ScopeType},
    types::{Function, FunctionKind, Primitive, Signature, Type},
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
        scope: LocalScopeRef,
        ctx: &Context,
        block: &Meta<ast::Block>,
    ) -> TypeResult<bool> {
        let mut diverged = false;

        for stmt in &block.stmts {
            if diverged {
                return Err(self.error_unreachable_expression(stmt));
            }
            diverged |= self.stmt(scope, ctx, stmt)?;
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
            self.type_info
                .expr_types
                .insert(block.id, Type::Primitive(Primitive::Unit));
            self.type_info.diverges.insert(block.id, diverged);
            return Ok(diverged);
        };

        if diverged {
            return Err(self.error_unreachable_expression(expr));
        }
        diverged |= self.expr(scope, ctx, expr)?;

        // Store the same type info on the block as on the expression
        let ty = &self.type_info.expr_types[&expr.id];
        self.type_info.expr_types.insert(block.id, ty.clone());
        self.type_info.diverges.insert(block.id, diverged);

        Ok(diverged)
    }

    pub fn stmt(
        &mut self,
        scope: LocalScopeRef,
        ctx: &Context,
        stmt: &Meta<ast::Stmt>,
    ) -> TypeResult<bool> {
        let var = self.fresh_var();
        let ctx = ctx.with_type(&var);
        match &stmt.node {
            ast::Stmt::Let(ident, expr) => {
                let diverges = self.expr(scope, &ctx, expr)?;
                let ty = self.resolve_type(&var);
                self.insert_var(scope, ident.clone(), ty)?;
                Ok(diverges)
            }
            ast::Stmt::Expr(expr) => self.expr(scope, &ctx, expr),
        }
    }

    pub fn expr(
        &mut self,
        scope: LocalScopeRef,
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
            Return(kind, e) => {
                let Some(ret) = &ctx.function_return_type else {
                    return Err(
                        self.error_cannot_diverge_here(kind.str(), expr)
                    );
                };

                self.unify(&ctx.expected_type, &Type::Never, id, None)?;

                let expected_type = match kind {
                    ast::ReturnKind::Return => ret.clone(),
                    ast::ReturnKind::Accept => {
                        let a_ty = self.fresh_var();
                        let b_ty = self.fresh_var();
                        let ty = Type::Verdict(
                            Box::new(a_ty.clone()),
                            Box::new(b_ty),
                        );
                        self.unify(ret, &ty, id, None)?;
                        self.resolve_type(&a_ty)
                    }
                    ast::ReturnKind::Reject => {
                        let a_ty = self.fresh_var();
                        let r_ty = self.fresh_var();
                        let ty = Type::Verdict(
                            Box::new(a_ty),
                            Box::new(r_ty.clone()),
                        );
                        self.unify(ret, &ty, id, None)?;
                        self.resolve_type(&r_ty)
                    }
                };

                if let Some(e) = e {
                    self.expr(
                        scope,
                        &ctx.with_type(expected_type.clone()),
                        e,
                    )?;
                } else {
                    self.unify(
                        &expected_type,
                        &Type::Primitive(Primitive::Unit),
                        id,
                        None,
                    )?;
                }

                self.type_info.return_types.insert(
                    id,
                    ctx.function_return_type
                        .as_ref()
                        .unwrap_or(&Type::Primitive(Primitive::Unit))
                        .clone(),
                );

                Ok(true)
            }
            Literal(l) => self.literal(ctx, l),
            Match(m) => self.match_expr(scope, ctx, m),
            FunctionCall(name, args) => {
                // A function call can either be an internal function or an
                // external function. Internal functions have priority over
                // external functions.
                if let Ok(ty) = self.get_var(scope, name) {
                    let ty = ty.clone();
                    let ty = self.resolve_type(&ty);

                    let Type::Function(params, ret) = ty else {
                        return Err(self.error_simple(
                            format!("the variable `{name}` is not callable, but has type `{ty}`"),
                            "not a function",
                            name.id,
                        ));
                    };

                    // Tell the lower stage about the kind of function this
                    // is. The most important part here is the
                    // FunctionDefinition::Roto bit.
                    self.type_info.function_calls.insert(
                        name.id,
                        Function {
                            signature: Signature {
                                kind: FunctionKind::Free,
                                parameter_types: params
                                    .iter()
                                    .map(|p| p.1.clone())
                                    .collect(),
                                return_type: *ret.clone(),
                            },
                            name: name.node,
                            vars: Vec::new(),
                            definition: FunctionDefinition::Roto,
                        },
                    );

                    let params: Vec<_> =
                        params.into_iter().map(|(_, t)| t).collect();
                    let diverges = self.check_arguments(
                        scope, ctx, "function", name, &params, args,
                    )?;

                    self.unify(&ctx.expected_type, &ret, id, None)?;
                    Ok(diverges)
                } else {
                    // It's not found in scope, so it should be a runtime
                    // function, otherwise we really can't find it.
                    let Some((function, signature)) =
                        self.find_function(&FunctionKind::Free, **name)
                    else {
                        let n = name.as_str();
                        return Err(self.error_simple(
                            format!("function `{n}` not found`",),
                            "function not found",
                            name.id,
                        ));
                    };

                    let function = function.clone();

                    self.type_info.function_calls.insert(name.id, function);

                    self.unify(
                        &ctx.expected_type,
                        &signature.return_type,
                        name.id,
                        None,
                    )?;

                    let diverges = self.check_arguments(
                        scope,
                        ctx,
                        "runtime function",
                        name,
                        &signature.parameter_types,
                        args,
                    )?;

                    Ok(diverges)
                }
            }
            MethodCall(receiver, name, args) => {
                // This could be an enum variant constructor, so we check that too
                if let Some((type_name, _, data)) =
                    self.find_enum_variant(ctx, id, receiver, name)?
                {
                    let Some(data) = data else {
                        return Err(self.error_simple(
                            format!("variant {name} of {type_name} does not have data"),
                            "does not have data",
                            name.id,
                        ));
                    };

                    let [arg] = &args.node[..] else {
                        return Err(self.error_simple(
                            "enum constructor must have exactly 1 argument",
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
                    if let Some(ty) = self.get_type(x.node) {
                        let ty = ty.clone();
                        return self
                            .static_method_call(scope, ctx, &ty, name, args);
                    }
                }
                self.method_call(id, scope, ctx, receiver, name, args)
            }
            Access(e, x) => {
                // This could be an enum variant constructor, so we check that too
                if let Some((name, _, data)) =
                    self.find_enum_variant(ctx, id, e, x)?
                {
                    if data.is_some() {
                        return Err(self.error_simple(
                            format!("variant {x} of {name} requires data"),
                            "requires data",
                            x.id,
                        ));
                    }

                    return Ok(false);
                }

                let ty = self.fresh_var();
                let diverges = self.expr(scope, &ctx.with_type(&ty), e)?;
                let ty = self.resolve_type(&ty);

                if let Type::Record(fields)
                | Type::NamedRecord(_, fields)
                | Type::RecordVar(_, fields) = &ty
                {
                    if let Some((_, t)) =
                        fields.iter().find(|(s, _)| s.node == x.node)
                    {
                        self.unify(&ctx.expected_type, t, x.id, None)?;
                        return Ok(diverges);
                    };
                }

                Err(self.error_simple(
                    format!("no field `{x}` on type `{ty}`",),
                    format!("unknown field `{x}`"),
                    x.id,
                ))
            }
            Var(x) => {
                let t = self.get_var(scope, x)?.clone();
                self.unify(&ctx.expected_type, &t, x.id, None)?;
                Ok(false)
            }
            Record(record) => {
                let field_types: Vec<_> = record
                    .fields
                    .iter()
                    .map(|(s, _)| (s.clone(), self.fresh_var()))
                    .collect();
                let rec = self.fresh_record(field_types.clone());
                self.unify(&ctx.expected_type, &rec, id, None)?;

                self.record_fields(scope, ctx, field_types, record, id)
            }
            TypedRecord(name, record) => {
                // We first retrieve the type we expect
                let Some(ty) = self.type_info.types.get(name) else {
                    return Err(self.error_undeclared_type(name));
                };
                let ty = ty.clone();
                self.unify(&ctx.expected_type, &ty, id, None)?;

                let Type::NamedRecord(_, record_fields) = ty else {
                    return Err(self.error_simple(
                        format!(
                            "Expected a named record type, but found `{name}`"
                        ),
                        "not a named record type",
                        name.id,
                    ));
                };

                let diverges = self.record_fields(
                    scope,
                    ctx,
                    record_fields,
                    record,
                    id,
                )?;

                // Infer the type based on the given expression
                let field_types: Vec<_> = record
                    .fields
                    .iter()
                    .map(|(s, _)| (s.clone(), self.fresh_var()))
                    .collect();
                let rec = self.fresh_record(field_types);
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
            IfElse(c, t, e) => {
                self.expr(
                    scope,
                    &ctx.with_type(Type::Primitive(Primitive::Bool)),
                    c,
                )?;

                let idx = self.if_else_counter;
                self.if_else_counter += 1;

                if let Some(e) = e {
                    let mut diverges = false;
                    let then_scope =
                        self.scope_graph.wrap(scope, ScopeType::Then(idx));
                    diverges |= self.block(then_scope, ctx, t)?;
                    let else_scope =
                        self.scope_graph.wrap(scope, ScopeType::Else(idx));
                    diverges |= self.block(else_scope, ctx, e)?;

                    // Record divergence so that we can omit the
                    // block after the if-else while lowering
                    self.type_info.diverges.insert(id, diverges);
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
                    let then_scope =
                        self.scope_graph.wrap(scope, ScopeType::Then(idx));
                    let _ = self.block(
                        then_scope,
                        &ctx.with_type(Type::Primitive(Primitive::Unit)),
                        t,
                    )?;
                    self.type_info.diverges.insert(id, false);
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
    ) -> TypeResult<Option<(Identifier, Identifier, Option<Type>)>> {
        let ast::Expr::Var(ident) = &e.node else {
            return Ok(None);
        };

        let Some(t) = self.get_type(ident.node) else {
            return Ok(None);
        };

        let t = t.clone();

        let Type::Enum(name, variants) = &t else {
            return Ok(None);
        };

        let Some((variant, data)) =
            variants.iter().find(|(v, _)| v == &x.node)
        else {
            return Err(self.error_simple(
                format!("no variant {x} on enum {name}"),
                "variant not found",
                x.id,
            ));
        };

        self.unify(&ctx.expected_type, &t, x.id, None)?;
        self.type_info
            .enum_variant_constructors
            .insert(id, t.clone());

        Ok(Some((*name, *variant, data.clone())))
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
            Asn(_) => Type::Primitive(Primitive::Asn),
            IpAddress(_) => Type::Primitive(Primitive::IpAddr),
            Bool(_) => Type::Primitive(Primitive::Bool),
            Integer(_) => self.fresh_int(),
        };

        self.unify(&ctx.expected_type, &t, span, None)?;
        Ok(false)
    }

    fn match_expr(
        &mut self,
        scope: LocalScopeRef,
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
            return Err(self.error_can_only_match_on_enum(&t_expr, expr.id));
        };

        // We'll keep track of used variants to do some basic
        // exhaustiveness checking.
        let mut used_variants = Vec::new();

        // Match diverges if all its branches diverge
        let mut arms_diverge = true;

        // Whether there is a default arm present (with '_')
        let mut default_arm = false;

        let match_id = self.match_counter;
        self.match_counter += 1;

        for ast::MatchArm {
            pattern,
            guard,
            body,
        } in arms
        {
            // Anything after default is unreachable
            if default_arm {
                todo!("error")
            }

            match &pattern.node {
                Pattern::Underscore => {
                    let arm_scope = self
                        .scope_graph
                        .wrap(scope, ScopeType::MatchArm(match_id, None));

                    if let Some(guard) = guard {
                        let ctx =
                            ctx.with_type(Type::Primitive(Primitive::Bool));
                        let _ = self.expr(arm_scope, &ctx, guard)?;
                    } else {
                        default_arm = true;
                    }

                    arms_diverge &= self.block(arm_scope, ctx, body)?;
                    continue;
                }
                Pattern::EnumVariant {
                    variant,
                    data_field,
                } => {
                    let Some(idx) =
                        variants.iter().position(|(v, _)| v == &variant.node)
                    else {
                        return Err(self
                            .error_variant_does_not_exist(variant, &t_expr));
                    };

                    let variant_already_used =
                        used_variants.contains(&variant.node);
                    if variant_already_used {
                        println!("WARNING: Variant occurs multiple times in match! This arm is unreachable")
                    }

                    let ty = &variants[idx].1;
                    let arm_scope = self.scope_graph.wrap(
                        scope,
                        ScopeType::MatchArm(match_id, Some(idx)),
                    );

                    match (ty, data_field) {
                        (None, None) => {} // ok!
                        (Some(t), Some(id)) => {
                            self.insert_var(arm_scope, id.clone(), t)?;
                        }
                        (None, Some(data_field)) => {
                            return Err(self
                                .error_variant_does_not_have_field(
                                    data_field, &t_expr,
                                ))
                        }
                        (Some(_), None) => {
                            return Err(self
                                .error_need_data_field_on_pattern(
                                    variant, &t_expr,
                                ));
                        }
                    }

                    if let Some(guard) = guard {
                        let ctx =
                            ctx.with_type(Type::Primitive(Primitive::Bool));
                        let _ = self.expr(arm_scope, &ctx, guard)?;
                    } else if !variant_already_used {
                        // If there is a guard we don't mark the variant as used,
                        // because the guard could evaluate to false and hence the
                        // variant _should_ actually be used again.
                        used_variants.push(variant.node);
                    }

                    arms_diverge &= self.block(arm_scope, ctx, body)?;
                }
            }
        }

        if !default_arm && used_variants.len() < variants.len() {
            let mut missing_variants = Vec::new();
            for v in variants {
                let v = v.0;
                if !used_variants.contains(&v) {
                    missing_variants.push(v);
                }
            }
            return Err(
                self.error_nonexhaustive_match(span, &missing_variants)
            );
        }

        Ok(arms_diverge)
    }

    fn binop(
        &mut self,
        scope: LocalScopeRef,
        ctx: &Context,
        op: &ast::BinOp,
        span: MetaId,
        left: &Meta<ast::Expr>,
        right: &Meta<ast::Expr>,
    ) -> TypeResult<bool> {
        use ast::BinOp::*;

        // There's a special case: constructing prefixes with `/`
        // We do a conservative check on the left hand side to see if it
        // could be an ip address. This (hopefully) does not conflict with the
        // integer implementation later.
        if let Div = op {
            let var = self.fresh_var();
            let ctx_left = ctx.with_type(var.clone());

            let mut diverges = false;
            diverges |= self.expr(scope, &ctx_left, left)?;

            let resolved = self.resolve_type(&var);

            if let Type::Primitive(Primitive::IpAddr) = resolved {
                let ctx_right = ctx.with_type(Type::Primitive(Primitive::U8));
                diverges |= self.expr(scope, &ctx_right, right)?;

                self.unify(
                    &ctx.expected_type,
                    &Type::Primitive(Primitive::Prefix),
                    span,
                    None,
                )?;

                let name = Identifier::from("new");
                let (function, _sig) = self
                    .find_function(
                        &FunctionKind::StaticMethod(Type::Primitive(
                            Primitive::Prefix,
                        )),
                        name,
                    )
                    .unwrap();
                let function = function.clone();
                self.type_info.function_calls.insert(span, function);
                return Ok(diverges);
            }
        };

        if let Add = op {
            let var = self.fresh_var();
            let ctx_new = ctx.with_type(var.clone());

            let mut diverges = false;
            diverges |= self.expr(scope, &ctx_new, left)?;

            let resolved = self.resolve_type(&var);

            if let Type::Primitive(Primitive::String) = resolved {
                diverges |= self.expr(scope, &ctx_new, right)?;

                self.unify(
                    &ctx.expected_type,
                    &Type::Primitive(Primitive::String),
                    span,
                    None,
                )?;

                let name = Identifier::from("append");
                let (function, _sig) = self
                    .find_function(
                        &FunctionKind::Method(Type::Primitive(
                            Primitive::String,
                        )),
                        name,
                    )
                    .unwrap();
                let function = function.clone();
                self.type_info.function_calls.insert(span, function);
                return Ok(diverges);
            }
        }

        match op {
            And | Or => {
                self.unify(
                    &ctx.expected_type,
                    &Type::Primitive(Primitive::Bool),
                    span,
                    None,
                )?;

                let ctx = ctx.with_type(Type::Primitive(Primitive::Bool));

                let mut diverges = false;
                diverges |= self.expr(scope, &ctx, left)?;
                diverges |= self.expr(scope, &ctx, right)?;
                Ok(diverges)
            }
            Lt | Le | Gt | Ge => {
                self.unify(
                    &ctx.expected_type,
                    &Type::Primitive(Primitive::Bool),
                    span,
                    None,
                )?;

                let ctx = ctx.with_type(self.fresh_int());

                let mut diverges = false;
                diverges |= self.expr(scope, &ctx, left)?;
                diverges |= self.expr(scope, &ctx, right)?;
                Ok(diverges)
            }
            Eq | Ne => {
                self.unify(
                    &Type::Primitive(Primitive::Bool),
                    &ctx.expected_type,
                    span,
                    None,
                )?;
                let ctx = ctx.with_type(self.fresh_var());

                let mut diverges = false;
                diverges |= self.expr(scope, &ctx, left)?;
                diverges |= self.expr(scope, &ctx, right)?;

                let ty = self.resolve_type(&ctx.expected_type);
                match ty {
                    Type::IntVar(_)
                    | Type::Never
                    | Type::Primitive(_)
                    | Type::Enum(_, _)
                    | Type::Record(..)
                    | Type::RecordVar(..)
                    | Type::NamedRecord(..) => (),
                    _ => {
                        return Err(self.error_simple(
                            "type cannot be compared",
                            "cannot be compared",
                            span,
                        ))
                    }
                }

                Ok(diverges)
            }
            Add | Sub | Mul | Div => {
                let operand_ty = self.fresh_int();
                let new_ctx = ctx.with_type(operand_ty.clone());

                let mut diverges = false;
                diverges |= self.expr(scope, &new_ctx, left)?;
                diverges |= self.expr(scope, &new_ctx, right)?;

                self.unify(&ctx.expected_type, &operand_ty, span, None)?;
                Ok(diverges)
            }
            In | NotIn => {
                self.unify(
                    &Type::Primitive(Primitive::Bool),
                    &ctx.expected_type,
                    span,
                    None,
                )?;

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
        _meta_id: MetaId,
        scope: LocalScopeRef,
        ctx: &Context,
        receiver: &Meta<ast::Expr>,
        name: &Meta<Identifier>,
        args: &[Meta<ast::Expr>],
    ) -> TypeResult<bool> {
        let ty = self.fresh_var();
        let mut diverges = self.expr(scope, &ctx.with_type(&ty), receiver)?;

        let Some((function, signature)) =
            self.find_function(&FunctionKind::Method(ty.clone()), name.node)
        else {
            let ty = self.resolve_type(&ty);
            return Err(self.error_simple(
                format!("method `{name}` not found on `{ty}`",),
                format!("method not found for `{ty}`"),
                name.id,
            ));
        };

        let function = function.clone();
        self.type_info.function_calls.insert(name.id, function);

        self.unify(
            &ctx.expected_type,
            &signature.return_type,
            name.id,
            None,
        )?;

        let mut all_args = vec![receiver.clone()];
        all_args.extend_from_slice(args);

        diverges |= self.check_arguments(
            scope,
            ctx,
            "method",
            name,
            &signature.parameter_types,
            &all_args,
        )?;

        Ok(diverges)
    }

    fn static_method_call(
        &mut self,
        scope: LocalScopeRef,
        ctx: &Context,
        ty: &Type,
        name: &Meta<Identifier>,
        args: &[Meta<ast::Expr>],
    ) -> TypeResult<bool> {
        let Some((function, signature)) = self.find_function(
            &FunctionKind::StaticMethod(ty.clone()),
            name.node,
        ) else {
            let ty = self.resolve_type(ty);
            return Err(self.error_simple(
                format!("static method `{name}` not found on `{ty}`",),
                format!("static method not found on `{ty}`"),
                name.id,
            ));
        };

        let function = function.clone();
        self.type_info.function_calls.insert(name.id, function);

        self.unify(
            &ctx.expected_type,
            &signature.return_type,
            name.id,
            None,
        )?;

        let diverges = self.check_arguments(
            scope,
            ctx,
            "static method",
            name,
            &signature.parameter_types,
            args,
        )?;

        Ok(diverges)
    }

    fn check_arguments(
        &mut self,
        scope: LocalScopeRef,
        ctx: &Context,
        call_type: &str,
        name: &Meta<Identifier>,
        params: &[Type],
        args: &[Meta<ast::Expr>],
    ) -> TypeResult<bool> {
        if args.len() != params.len() {
            return Err(self.error_number_of_arguments_dont_match(
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

    fn find_function(
        &mut self,
        kind: &FunctionKind,
        name: Identifier,
    ) -> Option<(Function, Signature)> {
        let funcs = self.functions.clone();
        for f in funcs {
            if f.name != name {
                continue;
            }
            let signature = self.instantiate_method(&f);
            let is_match = match (&signature.kind, kind) {
                (FunctionKind::Free, FunctionKind::Free) => true,
                (FunctionKind::Method(ty1), FunctionKind::Method(ty2)) => {
                    self.subtype_of(ty1, ty2)
                }
                (
                    FunctionKind::StaticMethod(ty1),
                    FunctionKind::StaticMethod(ty2),
                ) => self.subtype_of(ty1, ty2),
                _ => false,
            };
            if is_match {
                return Some((f, signature));
            }
        }
        None
    }

    fn record_fields(
        &mut self,
        scope: LocalScopeRef,
        ctx: &Context,
        field_types: Vec<(Meta<Identifier>, Type)>,
        record: &ast::Record,
        span: MetaId,
    ) -> TypeResult<bool> {
        let mut used_fields = HashSet::new();
        let mut missing_fields: HashSet<_> =
            field_types.iter().map(|x| x.0.node).collect();
        let mut invalid_fields = Vec::new();
        let mut duplicate_fields = Vec::new();

        for (ident, _) in &record.fields {
            if used_fields.contains(&ident.node) {
                duplicate_fields.push(ident);
            } else if missing_fields.remove(&ident.node) {
                used_fields.insert(ident.node);
            } else {
                invalid_fields.push(ident);
            }
        }

        if !invalid_fields.is_empty()
            || !duplicate_fields.is_empty()
            || !missing_fields.is_empty()
        {
            return Err(self.error_field_mismatch(
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
                .find_map(|(s, t)| (s.node == ident.node).then_some(t))
                .unwrap()
                .clone();

            diverges |=
                self.expr(scope, &ctx.with_type(expected_type), expr)?;
        }
        Ok(diverges)
    }
}
