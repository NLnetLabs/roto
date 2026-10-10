//! Type checking function-like items

use crate::{
    ast::{self, Expr, Identifier, Stmt},
    ice,
    parser::meta::Meta,
    typechecker::{
        error::{Label, TypeError},
        scope::{DeclarationKind, ModuleScope, TypeOrStub},
        types::Signature,
    },
    yang::parser::{
        Keyword, YangStmt,
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

// These are YANG statements that can take `type` as sub-statement, or that
// take sub-statements that can take them ('container' basically), meaning
// that they can represent types themselves.
//
// Module and submodule do not appear here, since they can only live at the root of an AST: yang does not allow nested modules.
impl TypeChecker {
    pub(crate) fn body<'a>(
        &'a mut self,
        scope: ScopeRef,
        iter: impl Iterator<Item = &'a Meta<Stmt>>,
    ) -> TypeResult<()> {
        for stmt in iter {
            println!(
                "[declare_types_in_module] in body: {:?} ({:?})",
                stmt.as_ident(),
                scope
            );
            let Stmt::YangStmtSeq(YangStmtSeq {
                stmt: YangStmt::Stmt(stmt_kw),
                ..
            }) = &stmt.node
            else {
                return Ok(());
            };

            match &stmt.node {
                // an actual type definition, it will have to have a type
                // sub-statement, that defines its base type
                Stmt::YangStmtSeq(YangStmtSeq {
                    stmt:
                        YangStmt::Stmt(Meta {
                            node: crate::yang::parser::Keyword::TypeDef,
                            ..
                        }),
                    sub_stmts,
                    ..
                }) => self.type_def(scope, stmt, stmt_kw, sub_stmts),
                // a type statement will have a type sub-statement if it
                // does not refer to a builtin type, it then refers to a base
                // type (which doesn't have to be a builtin itself, so it
                // can recurse)
                Stmt::YangStmtSeq(YangStmtSeq {
                    stmt:
                        YangStmt::Stmt(Meta {
                            node: crate::yang::parser::Keyword::Type,
                            ..
                        }),
                    sub_stmts,
                    ..
                }) => self.base_type(scope, stmt, stmt_kw, sub_stmts),
                // a leaf has to have a type statement, that represents the
                // type of the value the leaf holds.
                Stmt::YangStmtSeq(YangStmtSeq {
                    stmt:
                        YangStmt::Stmt(Meta {
                            node: crate::yang::parser::Keyword::Leaf,
                            ..
                        }),
                    sub_stmts,
                    ..
                }) => self.leaf(scope, stmt, sub_stmts),
                // recurse into a container's body
                Stmt::YangStmtSeq(YangStmtSeq {
                    stmt:
                        YangStmt::Stmt(Meta {
                            node: crate::yang::parser::Keyword::Container,
                            ..
                        }),
                    sub_stmts,
                    ..
                }) => self.body(scope, sub_stmts.iter_stmt()),
                _ => Ok(()),
            }?;
        }

        Ok(())
    }

    // check the type statement in a leaf
    pub(crate) fn leaf(
        &mut self,
        scope: ScopeRef,
        stmt: &Meta<Stmt>,
        sub_stmts: &Meta<Expr>,
    ) -> TypeResult<()> {
        match sub_stmts.find_attr("type") {
            Some(Meta {
                id,
                node: Argument::Ident(b_ty),
            }) => {
                println!("[leaf] type {} in scope {:?}", b_ty, scope);
                self.type_info
                    .scope_graph
                    .resolve_name(
                        scope,
                        &Meta {
                            id: *id,
                            node: *b_ty,
                        },
                        true,
                    )
                    .ok_or(TypeError {
                        description: format!(
                            "the type mentioned in this {} is unknown",
                            stmt.node.as_ident().unwrap()
                        ),
                        location: stmt.id,
                        labels: vec![
                            Label::error("in this leaf", stmt.id),
                            Label::info("this type is unknown", *id),
                        ],
                        notes: Vec::new(),
                    })?;

                Ok(())
            }
            // This is prefixed by what MUST be a module prefix
            Some(Meta {
                id,
                node: Argument::PrefixIdent((p, b_ty)),
            }) => {
                self.type_info
                    .scope_graph
                    .validate_prefixed_type(scope, stmt, b_ty, p, id)?;

                Ok(())
            }
            _ => {
                ice!(
                    "The statement {} is missing sub-statements. This should \
                     have been caught earlier.",
                    stmt.as_ident().unwrap()
                );
            }
        }
    }

    pub(crate) fn leaf_list(
        &mut self,
        scope: ScopeRef,
        stmt: &Meta<Stmt>,
        kw: &Meta<Keyword>,
        sub_stms: &Meta<Expr>,
    ) -> TypeResult<()> {
        todo!()
    }

    pub(crate) fn base_type(
        &mut self,
        scope: ScopeRef,
        stmt: &Meta<Stmt>,
        kw: &Meta<Keyword>,
        sub_stms: &Meta<Expr>,
    ) -> TypeResult<()> {
        todo!()
    }

    pub(crate) fn deviate(
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
                }) => {
                    println!("[leaf] type {} in scope {:?}", b_ty, scope);
                    self.type_info
                        .scope_graph
                        .resolve_name(
                            scope,
                            &Meta {
                                id: *id,
                                node: *b_ty,
                            },
                            true,
                        )
                        .ok_or(TypeError {
                            description: format!(
                                "the type mentioned in this {} is unknown",
                                stmt.node.as_ident().unwrap()
                            ),
                            location: stmt.id,
                            labels: vec![
                                Label::error("in this leaf", stmt.id),
                                Label::info("this type is unknown", *id),
                            ],
                            notes: Vec::new(),
                        })?;

                    Meta {
                        id: *id,
                        node: *b_ty,
                    }
                }
                Some(Meta {
                    id,
                    node: Argument::PrefixIdent((p, b_ty)),
                }) => {
                    self.type_info
                        .scope_graph
                        .validate_prefixed_type(scope, stmt, b_ty, p, id)?;

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
                "[declare_types_in_modules] {}: derived-from-type `{}`",
                ty.node, base_ty.node
            );

            let kind = if let Some(builtin_ty) = self
                .type_info
                .scope_graph
                .resolve_name(ScopeRef::GLOBAL, &base_ty, true)
            {
                println!(
                    "[declare_types_in_module] {}: base type is builtin `{}`",
                    ty_ident, base_ty.node
                );
                builtin_ty.kind
            } else {
                println!(
                    "[declare_types_in_modules] {}: base type is NOT a \
                     builtin type `{}`",
                    ty.node,
                    base_ty.as_str()
                );
                DeclarationKind::Type(TypeOrStub::Stub { num_params: 0 })
            };

            println!(
                "[declare_types_in_modules] insert declaration {}: scope \
                {:?}, base type `{}`",
                ty_ident, scope, base_ty.node
            );

            // let new_scope = self
            //     .type_info
            //     .scope_graph
            //     .wrap(scope, ScopeType::Type(ty_ident));
            let dec = self.type_info.scope_graph.insert_declaration(
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

            if let Err(e) = dec {
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
