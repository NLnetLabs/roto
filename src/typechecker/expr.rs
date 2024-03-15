use crate::ast;

use super::{
    scope::Scope,
    types::{Arrow, Method, Primitive, Type},
    TypeChecker, TypeResult,
};

impl TypeChecker {
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
                let t_left = self.expr(scope, &expr.left)?;
                let t_right = self.expr(scope, &expr.right)?;
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
        expr: &ast::CompareArg,
    ) -> TypeResult<Type> {
        match expr {
            ast::CompareArg::ValueExpr(expr) => self.expr(scope, expr),
            ast::CompareArg::GroupedLogicalExpr(
                ast::GroupedLogicalExpr { expr },
            ) => self.logical_expr(scope, expr),
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
        access: &[ast::AccessExpr],
    ) -> TypeResult<Type> {
        let mut last = receiver;
        for a in access {
            match a {
                ast::AccessExpr::MethodComputeExpr(
                    ast::MethodComputeExpr {
                        ident,
                        args: ast::ArgExprList { args },
                    },
                ) => {
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
                        let arg_ty = self.expr(scope, arg)?;
                        self.unify(&arg_ty, &ty)?;
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
        expr: &ast::LiteralAccessExpr,
    ) -> TypeResult<Type> {
        let ast::LiteralAccessExpr {
            literal,
            access_expr,
        } = expr;
        dbg!();
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
                if let Some(ty) = self.types.get(&x.ident.to_string()) {
                    let mut access_expr = access_expr.clone();
                    if access_expr.is_empty() {
                        return Err(
                            "Type should be followed by a method".into()
                        );
                    }
                    let ast::AccessExpr::MethodComputeExpr(m) =
                        access_expr.remove(0)
                    else {
                        return Err(
                            "First access on a type should be a method call"
                                .into(),
                        );
                    };
                    let receiver_type =
                        self.static_method_call(scope, ty.clone(), m)?;
                    self.access(scope, receiver_type, &access_expr)
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
        m: ast::MethodComputeExpr,
    ) -> TypeResult<Type> {
        let ast::MethodComputeExpr {
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
            let arg_ty = self.expr(scope, arg)?;
            self.unify(&arg_ty, &ty)?;
        }
        Ok(self.resolve_type(&arrow.ret).clone())
    }

    fn record_type(
        &mut self,
        scope: &Scope,
        expr: &[(ast::Identifier, ast::ValueExpr)],
    ) -> TypeResult<Vec<(String, Type)>> {
        Ok(expr
            .into_iter()
            .map(|(k, v)| {
                self.expr(scope, v).map(|v| (k.ident.to_string(), v))
            })
            .collect::<Result<_, _>>()?)
    }
}
