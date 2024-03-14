use crate::{
    ast::{
        self, AccessExpr, Define, DefineBody, MethodComputeExpr,
        TypeIdentField,
    },
    typechecker::types::Primitive,
};

use super::{
    scope::Scope,
    types::{Arrow, Method, Type},
    TypeChecker, TypeResult,
};

impl TypeChecker {
    pub fn filter_map(
        &mut self,
        scope: &Scope,
        filter_map: ast::FilterMap,
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

        let is_filter_map = ty == ast::FilterType::FilterMap;

        let args = self.with_clause(&mut scope, &with_kv)?;

        let Define {
            for_kv: _,
            with_kv,
            body:
                DefineBody {
                    rx_tx_type,
                    use_ext_data: _,
                    assignments,
                },
        } = define;

        self.with_clause(&mut scope, &with_kv)?;

        match rx_tx_type {
            ast::RxTxType::RxOnly(TypeIdentField { field_name, ty }) => {
                let Some(ty) = self.types.get(&ty.ident.to_string()) else {
                    return Err("type for rx is not defined".into());
                };
                scope.insert_var(field_name.ident.to_string(), ty.clone())?;
            }
            ast::RxTxType::Split(rx, tx) => {
                let Some(ty) = self.types.get(&rx.ty.ident.to_string())
                else {
                    return Err("type for rx is not defined".into());
                };
                scope.insert_var(
                    rx.field_name.ident.to_string(),
                    ty.clone(),
                )?;

                let Some(ty) = self.types.get(&tx.ty.ident.to_string())
                else {
                    return Err("type for tx is not defined".into());
                };
                scope.insert_var(
                    tx.field_name.ident.to_string(),
                    ty.clone(),
                )?;
            }
            ast::RxTxType::PassThrough(TypeIdentField { field_name, ty }) => {
                let Some(ty) = self.types.get(&ty.ident.to_string()) else {
                    return Err("type for rx_tx is not defined".into());
                };
                scope.insert_var(field_name.ident.to_string(), ty.clone())?;
            }
        }

        for (ident, expr) in assignments {
            let t = self.expr(&scope, expr)?;
            scope.insert_var(ident.ident.to_string(), t.clone())?;
        }

        for expression in expressions {
            match expression {
                ast::FilterMapExpr::Term(ast::TermSection {
                    ident,
                    for_kv: _,
                    with_kv,
                    body: ast::TermBody { scopes },
                }) => {
                    let mut inner_scope = scope.wrap();

                    let args =
                        self.with_clause(&mut inner_scope, &with_kv)?;

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
                                        self.logical_expr(
                                            &inner_scope,
                                            expr,
                                        )?;
                                    }
                                }
                            }
                            ast::MatchOperator::MatchValueWith(
                                ast::Identifier { ident },
                            ) => {
                                let x = scope.get_var(&ident.to_string())?;
                                let Type::Enum(_, variants) =
                                    self.resolve_type(x).clone()
                                else {
                                    todo!()
                                };

                                // We'll keep track of used variants to do some basic
                                // exhaustiveness checking. Only a warning for now.
                                let arms = &match_arms;
                                let mut used_variants = Vec::<&str>::new();

                                for (pattern, exprs) in arms {
                                    let ast::TermPatternMatchArm {
                                        variant_id,
                                        data_field,
                                    } = pattern.as_ref().unwrap();

                                    let variant_id = &variant_id.ident;
                                    let Some(idx) =
                                        variants.iter().position(|(v, _)| {
                                            v.as_str() == variant_id.as_str()
                                        })
                                    else {
                                        return Err(format!("The variant {variant_id} does not exist on {x:?}"));
                                    };

                                    if used_variants
                                        .contains(&variant_id.as_str())
                                    {
                                        println!("WARNING: Variant '{variant_id} occurs multiple times");
                                    }

                                    used_variants.push(variant_id.as_str());

                                    let ty = &variants[idx].1;

                                    let mut inner_scope = inner_scope.wrap();

                                    match (ty, data_field) {
                                        (None, None) => {
                                            // ok!
                                        }
                                        (Some(t), Some(id)) => {
                                            inner_scope.insert_var(
                                                id.ident.to_string(),
                                                t.clone(),
                                            )?;
                                        }
                                        (None, Some(_)) => {
                                            return Err("Got field for variant that doesn't have one".into())
                                        },
                                        (Some(_), None) => {
                                            return Err("Pattern should have a field".into())
                                        },
                                    }

                                    for expr in exprs {
                                        // We ignore the result because it
                                        // must be boolean.
                                        self.logical_expr(
                                            &inner_scope,
                                            expr.clone(),
                                        )?;
                                    }
                                }
                            }
                            _ => unreachable!(),
                        }
                    }

                    scope.insert_var(
                        ident.ident.to_string(),
                        Type::Term(args),
                    )?;
                }
                ast::FilterMapExpr::Action(ast::ActionSection {
                    ident,
                    with_kv,
                    body: ast::ActionSectionBody { expressions },
                }) => {
                    let mut inner_scope = scope.wrap();

                    let args =
                        self.with_clause(&mut inner_scope, &with_kv)?;

                    for expr in expressions {
                        let _t = self.compute_expr(&inner_scope, expr)?;
                    }

                    scope.insert_var(
                        ident.ident.to_string(),
                        Type::Action(args),
                    )?;
                }
            }
        }

        if let Some(ast::ApplySection {
            body:
                ast::ApplyBody {
                    scopes,
                    accept_reject: _,
                },
            for_kv: _,
            with_kv: _,
        }) = apply
        {
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
                                    unreachable!()
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
                            unreachable!()
                        };
                        let x = scope.get_var(&x.ident.to_string())?;
                        let Type::Enum(_, variants) =
                            self.resolve_type(x).clone()
                        else {
                            todo!()
                        };

                        // We'll keep track of used variants to do some basic
                        // exhaustiveness checking. Only a warning for now.
                        let arms = &match_arms;
                        let mut used_variants = Vec::<&str>::new();

                        for ast::PatternMatchActionArm {
                            variant_id,
                            data_field,
                            guard,
                            actions,
                        } in arms
                        {
                            let variant_id = &variant_id.ident;
                            let Some(idx) =
                                variants.iter().position(|(v, _)| {
                                    v.as_str() == variant_id.as_str()
                                })
                            else {
                                return Err(format!("The variant {variant_id} does not exist on {x:?}"));
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
                                    inner_scope.insert_var(
                                        id.ident.to_string(),
                                        t.clone(),
                                    )?;
                                }
                                (None, Some(_)) => {
                                    return Err("Got field for variant that doesn't have one".into())
                                },
                                (Some(_), None) => {
                                    return Err("Pattern should have a field".into())
                                },
                            }

                            if let Some(guard) = guard {
                                let ast::TermCallExpr { term_id, args } =
                                    guard;
                                let Type::Term(term_params) = scope
                                    .get_var(&term_id.ident.to_string())?
                                else {
                                    return Err("Should be a term".into());
                                };

                                match args {
                                    Some(ast::ArgExprList { args }) => {
                                        if args.len() != term_params.len() {
                                            return Err(
                                                "Number of arguments do not match".into(),
                                            );
                                        }
                                        for (arg, (_, param)) in
                                            args.into_iter().zip(term_params)
                                        {
                                            let ty = self.expr(
                                                &inner_scope,
                                                arg.clone(),
                                            )?;
                                            self.unify(&ty, param)?;
                                        }
                                    }
                                    None => {
                                        if !term_params.is_empty() {
                                            return Err("Term takes params but none were given.".into());
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
                                        unreachable!()
                                    }
                                    (
                                        Some(ast::ActionCallExpr {
                                            action_id,
                                            args,
                                        }),
                                        None,
                                    ) => {
                                        let Type::Action(term_params) = scope
                                            .get_var(
                                                &action_id.ident.to_string(),
                                            )?
                                        else {
                                            return Err(
                                                "Should be a action".into()
                                            );
                                        };

                                        match args {
                                            Some(ast::ArgExprList {
                                                args,
                                            }) => {
                                                if args.len()
                                                    != term_params.len()
                                                {
                                                    return Err(
                                                        "Number of arguments do not match".into(),
                                                    );
                                                }
                                                for (arg, (_, param)) in args
                                                    .into_iter()
                                                    .zip(term_params)
                                                {
                                                    let ty = self.expr(
                                                        &inner_scope,
                                                        arg.clone(),
                                                    )?;
                                                    self.unify(&ty, param)?;
                                                }
                                            }
                                            None => {
                                                if !term_params.is_empty() {
                                                    return Err("Term takes params but none were given.".into());
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
                                println!("WARNING: Variant {} is not covered in enum.", v.as_str());
                            }
                        }
                    }
                }
            }
        }

        Ok(if is_filter_map {
            Type::FilterMap(args)
        } else {
            Type::Filter(args)
        })
    }

    fn with_clause(
        &mut self,
        scope: &mut Scope,
        args: &[TypeIdentField],
    ) -> TypeResult<Vec<(String, Type)>> {
        args.into_iter()
            .map(|TypeIdentField { field_name, ty }| {
                let Some(ty) = self.types.get(&ty.ident.to_string()) else {
                    return Err("type does not exist".to_string());
                };

                let field_name = field_name.ident.to_string();
                scope.insert_var(field_name.clone(), ty.clone())?;
                Ok((field_name.clone(), ty.clone()))
            })
            .collect()
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
                self.unify(&Type::Primitive(Primitive::Bool), &ty)?;
            }
            ast::BooleanExpr::LiteralAccessExpr(expr) => {
                let ty = self.literal_access(scope, expr)?;
                self.unify(&Type::Primitive(Primitive::Bool), &ty)?;
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
        Ok(Type::Primitive(Primitive::Bool))
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
            }) => Ok(Type::RecordVar(
                self.fresh_record(),
                self.record_type(scope, key_values)?,
            )),
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
            ListExpr(ast::ListValueExpr { values }) => {
                let ret = self.fresh_var();
                for v in values {
                    let t = self.expr(scope, v)?;
                    self.unify(&ret, &t)?;
                }
                Ok(Type::List(Box::new(self.resolve_type(&ret).clone())))
            }
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
                    let Some(arrow) = self.find_method(
                        self.methods.clone(),
                        &last,
                        &ident.ident.to_string(),
                    ) else {
                        return Err(format!(
                            "Method '{ident}' not found on {last:?}"
                        ));
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
                        return Err(format!(
                            "No field '{field}' on {last:?}"
                        ));
                    }
                }
            }
        }
        Ok(last)
    }

    fn find_method(
        &mut self,
        methods: Vec<Method>,
        ty: &Type,
        name: &str,
    ) -> Option<Arrow> {
        methods.iter().find_map(|m| {
            if name != m.name {
                return None;
            }
            let arrow = self.instantiate_method(m);
            dbg!(&arrow, ty);
            if dbg!(self.subtype_of(&arrow.rec, ty)) {
                Some(arrow)
            } else {
                None
            }
        })
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
        dbg!();
        let literal = self.literal(literal)?;
        self.access(scope, literal, access_expr)
    }

    fn literal(&mut self, literal: ast::LiteralExpr) -> TypeResult<Type> {
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

    fn compute_expr(
        &mut self,
        scope: &Scope,
        expr: ast::ComputeExpr,
    ) -> TypeResult<Type> {
        let ast::ComputeExpr {
            receiver,
            access_expr,
        } = expr;
        match receiver {
            ast::AccessReceiver::Ident(x) => {
                // It might be a static method
                // TODO: This should be cleaned up
                if let Some(ty) = self.types.get(&x.ident.to_string()) {
                    let mut access_expr = access_expr.clone();
                    if access_expr.is_empty() {
                        return Err(
                            "Type should be followed by a method".into()
                        );
                    }
                    let AccessExpr::MethodComputeExpr(m) =
                        access_expr.remove(0)
                    else {
                        return Err(
                            "First access on a type should be a method call"
                                .into(),
                        );
                    };
                    let receiver_type =
                        self.static_method_call(scope, ty.clone(), m)?;
                    self.access(scope, receiver_type, access_expr)
                } else {
                    let receiver_type =
                        scope.get_var(&x.ident.to_string())?.clone();
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
        m: MethodComputeExpr,
    ) -> TypeResult<Type> {
        let MethodComputeExpr {
            ident,
            args: ast::ArgExprList { args },
        } = m;
        let Some(arrow) =
            self.find_method(self.static_methods.clone(), &ty, &ident.ident)
        else {
            return Err(format!(
                "No static method '{}' found for '{:?}'",
                &ident, ty
            ));
        };

        if args.len() != arrow.args.len() {
            return Err(format!("Number of arguments don't match"));
        }

        self.unify(&arrow.rec, &ty)?;

        for (arg, ty) in args.iter().zip(&arrow.args) {
            let arg_ty = self.expr(scope, arg.clone())?;
            self.unify(&arg_ty, &ty)?;
        }
        Ok(self.resolve_type(&arrow.ret).clone())
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
