//! Type checking function-like items

use crate::{
    ast::{self, Expr, Identifier, Stmt},
    ice,
    parser::meta::Meta,
    typechecker::{
        error::{Label, TypeError},
        scope::{DeclarationKind, TypeOrStub},
        types::Signature,
    },
    yang::parser::{
        Keyword,
        ast::{Argument, YangStmtSeq},
    },
};

use super::{
    TypeChecker, TypeResult,
    expr::Context,
    scope::{ResolvedName, ScopeRef, ScopeType},
    types::Type,
};

impl TypeChecker {
    /// Type check a filter map
    pub fn filter_map(
        &mut self,
        outer_scope: ScopeRef,
        filter_map: &ast::FilterMap,
    ) -> TypeResult<()> {
        let ast::FilterMap {
            filter_type: _,
            ident,
            params,
            body,
        } = filter_map;

        let signature = self.type_info.function_signature(ident);
        let return_type = signature.return_type;

        let scope = self
            .type_info
            .scope_graph
            .wrap(outer_scope, ScopeType::Function(ident.node));
        self.type_info.function_scopes.insert(ident.id, scope);

        let params = self.params(scope, params)?;
        for (v, t) in &params {
            self.insert_var(scope, v.clone(), t)?;
        }

        let ctx = Context {
            expected_type: return_type.clone(),
            function_return_type: Some(return_type.clone()),
            item: ResolvedName {
                scope: outer_scope,
                ident: **ident,
            },
        };

        self.references.add_node(ctx.item);
        self.block(scope, &ctx, body)?;
        self.resolve_obligations()?;

        Ok(())
    }

    pub fn function(
        &mut self,
        outer_scope: ScopeRef,
        function: &ast::FunctionDeclaration,
    ) -> TypeResult<()> {
        let ast::FunctionDeclaration {
            ident,
            params,
            body,
            ret,
        } = function;

        let scope = self
            .type_info
            .scope_graph
            .wrap(outer_scope, ScopeType::Function(ident.node));

        self.type_info.function_scopes.insert(ident.id, scope);

        let params = self.params(scope, params)?;
        for (v, t) in &params {
            self.insert_var(scope, v.clone(), t)?;
        }

        let ret = if let Some(ret) = ret {
            self.evaluate_type_expr(scope, ret)?
        } else {
            Type::unit()
        };

        let ctx = Context {
            expected_type: ret.clone(),
            function_return_type: Some(ret),
            item: ResolvedName {
                scope: outer_scope,
                ident: **ident,
            },
        };

        self.references.add_node(ctx.item);
        self.block(scope, &ctx, body)?;
        self.resolve_obligations()?;

        Ok(())
    }

    pub fn constant(
        &mut self,
        outer_scope: ScopeRef,
        constant: &ast::ConstantDeclaration,
    ) -> TypeResult<()> {
        let ast::ConstantDeclaration { ident, ty, expr } = constant;

        let scope = self
            .type_info
            .scope_graph
            .wrap(outer_scope, ScopeType::Function(ident.node));

        self.type_info.function_scopes.insert(ident.id, scope);

        let ty = self.evaluate_type_expr(outer_scope, ty)?;
        let ctx = Context {
            expected_type: ty,
            function_return_type: None,
            item: ResolvedName {
                scope: outer_scope,
                ident: **ident,
            },
        };

        self.references.add_node(ctx.item);

        self.expr(scope, &ctx, expr)?;
        self.resolve_obligations()?;

        Ok(())
    }

    pub fn test(
        &mut self,
        outer_scope: ScopeRef,
        test: &ast::Test,
    ) -> TypeResult<()> {
        let ast::Test { ident, body } = test;

        let name = Identifier::from(format!("test#{ident}"));
        let name = Meta {
            id: ident.id,
            node: name,
        };
        self.insert_function(
            outer_scope,
            name.clone(),
            super::types::FunctionDefinition::Roto,
            Vec::new(),
            String::new(),
            Signature {
                types: Vec::new(),
                parameter_types: Vec::new(),
                return_type: Type::verdict(Type::unit(), Type::unit()),
            },
        )?;

        let scope = self
            .type_info
            .scope_graph
            .wrap(outer_scope, ScopeType::Function(name.node));

        self.type_info.function_scopes.insert(name.id, scope);

        let ret = Type::verdict(Type::unit(), Type::unit());
        let ctx = Context {
            expected_type: ret.clone(),
            function_return_type: Some(ret),
            item: ResolvedName {
                scope: outer_scope,
                ident: *name,
            },
        };
        self.references.add_node(ctx.item);
        self.block(scope, &ctx, body)?;
        self.resolve_obligations()?;
        Ok(())
    }

    pub fn function_type(
        &mut self,
        scope: ScopeRef,
        dec: &ast::FunctionDeclaration,
    ) -> TypeResult<Signature> {
        let return_type = if let Some(ret) = &dec.ret {
            self.evaluate_type_expr(scope, ret)?
        } else {
            Type::unit()
        };
        let parameter_types = self
            .params(scope, &dec.params)?
            .into_iter()
            .map(|(_, t)| t)
            .collect();

        Ok(Signature {
            types: Vec::new(),
            parameter_types,
            return_type,
        })
    }

    pub fn filter_map_type(
        &mut self,
        scope: ScopeRef,
        dec: &ast::FilterMap,
    ) -> TypeResult<Signature> {
        let accept = self.fresh_var();
        let reject = self.fresh_var();

        let parameter_types = self
            .params(scope, &dec.params)?
            .into_iter()
            .map(|(_, t)| t)
            .collect();

        Ok(Signature {
            types: Vec::new(),
            parameter_types,
            return_type: Type::verdict(accept, reject),
        })
    }

    fn params(
        &mut self,
        scope: ScopeRef,
        args: &ast::Params,
    ) -> TypeResult<Vec<(Meta<Identifier>, Type)>> {
        args.0
            .iter()
            .map(|(field_name, ty)| {
                let ty = self.evaluate_type_expr(scope, ty)?;
                Ok((field_name.clone(), ty))
            })
            .collect()
    }
}

// These are YANG statements that can take `type` as sub-statement, meaning \
// that these statements have
impl TypeChecker {
    fn leaf(&mut self, scope: ScopeRef, dec: &YangStmtSeq) -> TypeResult<()> {
        todo!()
    }

    fn leaf_list(
        &mut self,
        scope: ScopeRef,
        dec: &YangStmtSeq,
    ) -> TypeResult<()> {
        todo!()
    }

    fn base_ty(
        &mut self,
        scope: ScopeRef,
        dec: &YangStmtSeq,
    ) -> TypeResult<()> {
        todo!()
    }

    fn deviate(
        &mut self,
        scope: ScopeRef,
        dec: &YangStmtSeq,
    ) -> TypeResult<()> {
        todo!()
    }

    pub(crate) fn type_def(
        &mut self,
        scope: ScopeRef,
        stmt: &Meta<Stmt>,
        kw: &Meta<Keyword>,
        sub_stmts: &Meta<Expr>,
    ) -> TypeResult<()> {
        {
            // the name of the defined type
            let Some(ty) = stmt.node.argument() else {
                ice!("missing type name in module {:?}", stmt.node.as_ident())
            };

            let Some(ty_ident) = ty.as_ident() else {
                ice!("type name {ty} is invalid");
            };

            // println!("seq {:#?}", st);
            println!("[declare_types] declare type `{}`", ty.node);

            // what is the type the current typedef is based on?
            let base_ty = match sub_stmts.find_attr("type") {
                Some(Meta {
                    id,
                    node: Argument::Ident(b_ty),
                }) => Meta {
                    id: *id,
                    node: *b_ty,
                },
                Some(Meta {
                    id,
                    node: Argument::PrefixIdent((p, b_ty)),
                }) => {
                    // the prefix should already exist as an imported
                    // module here
                    println!(
                        "[declare_types] type {} w/ module prefix `{}`",
                        b_ty, p
                    );
                    self.type_info
                        .scope_graph
                        .resolve_name(
                            scope,
                            &Meta { id: *id, node: *p },
                            true,
                        )
                        .ok_or(TypeError {
                            description: format!(
                                "Cannot find module with prefix \
                                        `{p}` in type declaration for \
                                        `{ty_ident}`"
                            ),
                            location: kw.id,
                            labels: vec![
                                Label::error(
                                    "in this type declaration..",
                                    ty.id,
                                ),
                                Label::info(
                                    "module prefix and type name cannot be \
                                     found",
                                    *id,
                                ),
                            ],
                            notes: Vec::new(),
                        })?;
                    Meta {
                        id: *id,
                        node: *b_ty,
                    }
                }
                _ => {
                    ice!("type name {ty} is not an identifier");
                }
            };

            println!(
                "[declare_types_in_modules] derived-from-type `{}`",
                base_ty.node
            );

            let kind = if let Some(builtin_ty) = self
                .type_info
                .scope_graph
                .resolve_name(ScopeRef::GLOBAL, &base_ty, true)
            {
                builtin_ty.kind
            } else {
                println!(
                    "[declare_types_in_modules] found non-builtin type `{}`",
                    base_ty.as_str()
                );
                DeclarationKind::Type(TypeOrStub::Stub { num_params: 0 })
            };

            let res = self.type_info.scope_graph.insert_declaration(
                scope,
                &Meta {
                    id: kw.id,
                    node: ty_ident,
                },
                kind,
                stmt.node
                    .description()
                    .map(|d| d.to_string())
                    .unwrap_or_default(),
                |_| false,
            );

            if let Err(e) = res {
                return Err(TypeError {
                    description: format!("{:?}", stmt.node),
                    location: kw.id,
                    labels: vec![
                        Label::error(
                            format!("`{ty_ident}` redefined here"),
                            kw.id,
                        ),
                        Label::info(
                            format!("`{ty_ident}` previously declared here"),
                            e,
                        ),
                    ],
                    notes: Vec::new(),
                });
            }

            Ok(())
        }
    }
}
