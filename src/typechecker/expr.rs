//! Type checking of expressions

use std::{borrow::Borrow, collections::HashSet};

use crate::{
    ast::{self, Identifier, Pattern},
    parser::meta::{Meta, MetaId},
    typechecker::scope::DeclarationKind,
};

use super::{
    scope::{Declaration, ResolvedName, ScopeRef, ScopeType, ValueKind},
    types::{
        Function, FunctionDefinition, FunctionKind, Primitive, Signature,
        Type,
    },
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

#[derive(Clone)]
pub enum ResolvedPath {
    Function {
        name: ResolvedName,
        definition: FunctionDefinition,
        signature: Signature,
    },
    Method {
        value: PathValue,
        name: ResolvedName,
        definition: FunctionDefinition,
        signature: Signature,
    },
    Value(PathValue),
    StaticMethod {
        name: ResolvedName,
        definition: FunctionDefinition,
        signature: Signature,
    },
    EnumConstructor {
        ty: Type,
        variant: Identifier,
        data: Option<Type>,
    },
}

#[derive(Clone)]
pub struct PathValue {
    pub name: ResolvedName,
    pub kind: ValueKind,
    pub ty: Type,
    pub fields: Vec<(Identifier, Type)>,
}

impl TypeChecker {
    pub fn block(
        &mut self,
        scope: ScopeRef,
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
        scope: ScopeRef,
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
        scope: ScopeRef,
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
            FunctionCall(e, args) => match &e.node {
                ast::Expr::Path(p) => {
                    self.path_function_call(scope, ctx, id, p, args)
                }
                ast::Expr::Access(e, name) => {
                    let ty = self.fresh_var();
                    let mut diverges =
                        self.expr(scope, &ctx.with_type(&ty), e)?;
                    diverges |=
                        self.method_call(scope, ctx, id, ty, name, args)?;
                    Ok(diverges)
                }
                _ => Err(self.error_simple(
                    "arbitrary function expressions are not supported yet"
                        .to_string(),
                    "cannot be called".to_string(),
                    e.id,
                )),
            },
            Access(e, field) => {
                let ty = self.fresh_var();
                let diverges = self.expr(scope, &ctx.with_type(&ty), e)?;
                let ty = self.resolve_type(&ty);
                let ty = self.access_field(&ty, field)?;
                self.unify(&ctx.expected_type, &ty, field.id, None)?;
                Ok(diverges)
            }
            Path(p) => {
                let last_ident = p.idents.last().unwrap();
                let resolved_path = self.resolve_expression_path(scope, p)?;
                self.type_info
                    .path_kinds
                    .insert(p.id, resolved_path.clone());

                let ResolvedPath::Value(PathValue {
                    name: _,
                    kind: _,
                    ty,
                    fields,
                }) = resolved_path
                else {
                    todo!("error and enum constructor")
                };

                let ty = if let Some(f) = fields.last() {
                    f.1.clone()
                } else {
                    ty
                };

                self.type_info.expr_types.insert(p.id, ty.clone());
                self.unify(&ctx.expected_type, &ty, last_ident.id, None)?;
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
            TypedRecord(path, record) => {
                let last_ident = path.idents.last().unwrap();
                let ty = self.resolve_type_path(scope, path)?;

                let Type::NamedRecord(_, record_fields) = &ty else {
                    return Err(self.error_simple(
                        format!(
                            "Expected a named record type, but found `{last_ident}`"
                        ),
                        "not a named record type",
                        last_ident.id,
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
                    .map(|(s, _)| (s.clone(), self.fresh_var()))
                    .collect();
                let rec = self.fresh_record(field_types);
                self.unify(&ctx.expected_type, &rec, id, None)?;
                self.unify(&ctx.expected_type, &ty, id, None)?;

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
                    let then_scope = self
                        .type_info
                        .scope_graph
                        .wrap(scope, ScopeType::Then(idx));
                    diverges |= self.block(then_scope, ctx, t)?;
                    let else_scope = self
                        .type_info
                        .scope_graph
                        .wrap(scope, ScopeType::Else(idx));
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
                    let then_scope = self
                        .type_info
                        .scope_graph
                        .wrap(scope, ScopeType::Then(idx));
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
        scope: ScopeRef,
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
                        .type_info
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
                    let arm_scope = self.type_info.scope_graph.wrap(
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
        scope: ScopeRef,
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
                    .find_method(
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
                    .find_method(
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

    fn check_arguments(
        &mut self,
        scope: ScopeRef,
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

    fn find_method(
        &mut self,
        kind: &FunctionKind,
        name: Identifier,
    ) -> Option<(Function, Signature)> {
        // Free functions are part of the normal scope graph and should not be
        // handled here.
        assert!(!matches!(kind, FunctionKind::Free));

        let funcs = self.functions.clone();
        for f in funcs {
            if f.name.ident != name {
                continue;
            }
            let signature = self.instantiate_method(&f);
            let is_match = match (&signature.kind, kind) {
                (FunctionKind::Free, FunctionKind::Free) => unreachable!(),
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
        scope: ScopeRef,
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

    /// Resolve the module part of a path
    ///
    /// In a path, we might start with a path of modules and at some point, we
    /// transition into other items. This function resolves that first part.
    fn resolve_module_part_of_path<'a>(
        &mut self,
        mut scope: ScopeRef,
        is_absolute: bool,
        idents: impl Iterator<Item = &'a Meta<Identifier>>,
    ) -> TypeResult<(&'a Meta<Identifier>, Declaration)> {
        let mut idents = idents.peekable();

        if is_absolute || idents.peek().unwrap().node == "lib".into() {
            scope = ScopeRef::GLOBAL;
        }

        let mut ident = idents.next().unwrap();

        // Keep checking modules until we find something that isn't a module
        loop {
            let dec = if ident.node == "super".into() {
                let Some(dec) =
                    self.type_info.scope_graph.parent_module(scope)
                else {
                    todo!("error");
                };
                dec.clone()
            } else {
                let Some(stub) =
                    self.type_info.scope_graph.resolve_name(scope, ident)
                else {
                    return Err(self.error_not_defined(ident));
                };
                self.type_info.scope_graph.get_declaration(stub.name)
            };

            let DeclarationKind::Module(s) = &dec.kind else {
                return Ok((ident, dec));
            };
            scope = *s;

            let Some(tmp_ident) = idents.next() else {
                return Ok((ident, dec));
            };

            ident = tmp_ident;
        }
    }

    /// Resolve a path of identifiers into a value
    fn resolve_expression_path(
        &mut self,
        scope: ScopeRef,
        ast::Path {
            idents,
            is_absolute,
        }: &ast::Path,
    ) -> TypeResult<ResolvedPath> {
        let mut idents = idents.iter();
        let (ident, dec) = self.resolve_module_part_of_path(
            scope,
            *is_absolute,
            &mut idents,
        )?;

        match &dec.kind {
            // We have reached the end of the iterator, but are still a module even though we
            // should be an expression. Time to error!
            DeclarationKind::Module(_) => {
                Err(self.error_expected_value(ident, &dec))
            }
            // We ended on a type, which means there can only be 1 ident left to make this
            // an enum variant constructor or the name of a static method.
            DeclarationKind::Type(ty) => {
                let Some(next_ident) = idents.next() else {
                    return Err(self.error_expected_value(ident, &dec));
                };

                if let Some((function, signature)) = self.find_method(
                    &FunctionKind::StaticMethod(ty.clone()),
                    **next_ident,
                ) {
                    if let Some(field) = idents.next() {
                        return Err(self.error_no_field_on_type(ty, field));
                    }
                    return Ok(ResolvedPath::StaticMethod {
                        name: dec.name,
                        definition: function.definition.clone(),
                        signature,
                    });
                }

                let Type::Enum(_, variants) = &ty else {
                    return Err(self.error_expected_value(ident, &dec));
                };

                let Some((variant, data)) =
                    variants.iter().find(|(v, _)| *v == **next_ident)
                else {
                    return Err(self.error_no_field_on_type(ty, next_ident));
                };

                if let Some(field) = idents.next() {
                    return Err(self.error_no_field_on_type(ty, field));
                }

                Ok(ResolvedPath::EnumConstructor {
                    ty: ty.clone(),
                    variant: *variant,
                    data: data.clone(),
                })
            }
            // We ended on a function, which means there can be no identifiers left
            DeclarationKind::Function(definition, ref ty) => {
                if let Some(field) = idents.next() {
                    return Err(self.error_no_field_on_type(ty, field));
                }
                let Type::Function(parameter_types, return_type) = ty else {
                    panic!()
                };
                let signature = Signature {
                    kind: FunctionKind::Free,
                    parameter_types: parameter_types.clone(),
                    return_type: (**return_type).clone(),
                };
                Ok(ResolvedPath::Function {
                    name: dec.name,
                    definition: definition.clone(),
                    signature,
                })
            }
            // We have a value, so the rest of the idents should be field accesses
            // optionally ending with a method.
            DeclarationKind::Value(kind, ref root_ty) => {
                let mut fields = Vec::new();
                let mut ty = root_ty.clone();

                // We loop until we either find the last field or a method.
                // The method must be the last identifier.
                while let Some(field) = idents.next() {
                    if let Some((function, signature)) = self.find_method(
                        &FunctionKind::Method(ty.clone()),
                        **field,
                    ) {
                        if let Some(field) = idents.next() {
                            return Err(
                                self.error_no_field_on_type(&ty, field)
                            );
                        }
                        return Ok(ResolvedPath::Method {
                            value: PathValue {
                                name: dec.name,
                                kind: kind.clone(),
                                ty: root_ty.clone(),
                                fields,
                            },
                            name: function.name,
                            definition: function.definition,
                            signature,
                        });
                    }

                    // The field is not a method, so we try a field access.
                    ty = self.access_field(&ty, field)?;
                    fields.push((field.node, ty.clone()));
                }

                Ok(ResolvedPath::Value(PathValue {
                    name: dec.name,
                    kind: kind.clone(),
                    ty: root_ty.clone(),
                    fields,
                }))
            }
        }
    }

    fn resolve_type_path(
        &mut self,
        scope: ScopeRef,
        ast::Path {
            idents,
            is_absolute,
        }: &ast::Path,
    ) -> TypeResult<Type> {
        let mut idents = idents.iter();
        let (ident, dec) = self.resolve_module_part_of_path(
            scope,
            *is_absolute,
            &mut idents,
        )?;

        match dec.kind {
            DeclarationKind::Value(_, _)
            | DeclarationKind::Function(_, _)
            | DeclarationKind::Module(_) => {
                Err(self.error_expected_type(ident, dec.to_stub()))
            }
            DeclarationKind::Type(ty) => Ok(ty),
        }
    }

    fn access_field(
        &mut self,
        ty: &Type,
        field: &Meta<Identifier>,
    ) -> TypeResult<Type> {
        let ty = self.type_info.resolve(ty);
        if let Type::Record(fields)
        | Type::NamedRecord(_, fields)
        | Type::RecordVar(_, fields) = &ty
        {
            if let Some((_, t)) =
                fields.iter().find(|(s, _)| s.node == field.node)
            {
                return Ok(t.clone());
            };
        }

        Err(self.error_no_field_on_type(&ty, field))
    }

    fn path_function_call(
        &mut self,
        scope: ScopeRef,
        ctx: &Context,
        id: MetaId,
        p: &Meta<ast::Path>,
        args: &Meta<Vec<Meta<ast::Expr>>>,
    ) -> TypeResult<bool> {
        let resolved_path = self.resolve_expression_path(scope, p)?;

        let (name, definition, signature, description) = match &resolved_path
        {
            ResolvedPath::Function {
                name,
                definition,
                signature,
            } => (name, definition, signature, "function"),
            ResolvedPath::StaticMethod {
                name,
                definition,
                signature,
            } => (name, definition, signature, "static method"),
            ResolvedPath::Method {
                value: _,
                definition,
                name,
                signature,
            } => (name, definition, signature, "method"),
            _ => {
                todo!("error")
            }
        };

        // Tell the lower stage about the kind of function this is.
        self.type_info
            .path_kinds
            .insert(p.id, resolved_path.clone());
        self.type_info.function_calls.insert(
            id,
            Function {
                signature: signature.clone(),
                name: *name,
                vars: Vec::new(),
                definition: definition.clone(),
            },
        );

        let last_ident = p.idents.last().unwrap();

        // Skip the first parameter if we are checking a method
        let params = if let ResolvedPath::Method { .. } = resolved_path {
            &signature.parameter_types[1..]
        } else {
            &signature.parameter_types
        };

        let diverges = self.check_arguments(
            scope,
            ctx,
            description,
            last_ident,
            params,
            args,
        )?;

        self.unify(&ctx.expected_type, &signature.return_type, id, None)?;
        Ok(diverges)
    }

    fn method_call(
        &mut self,
        scope: ScopeRef,
        ctx: &Context,
        id: MetaId,
        ty: Type,
        field: &Meta<Identifier>,
        args: &Meta<Vec<Meta<ast::Expr>>>,
    ) -> TypeResult<bool> {
        let Some((function, signature)) =
            self.find_method(&FunctionKind::Method(ty.clone()), **field)
        else {
            return Err(self.error_no_field_on_type(&ty, field));
        };

        self.type_info.function_calls.insert(
            id,
            Function {
                signature: signature.clone(),
                name: function.name,
                vars: Vec::new(),
                definition: function.definition.clone(),
            },
        );

        let params = &signature.parameter_types[1..];
        let diverges =
            self.check_arguments(scope, ctx, "method", field, params, args)?;
        self.unify(&ctx.expected_type, &signature.return_type, id, None)?;
        Ok(diverges)
    }
}
