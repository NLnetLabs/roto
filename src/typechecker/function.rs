//! Type checking function-like items

use crate::{
    ast::{self, Expr, Identifier, Stmt},
    ice,
    parser::meta::Meta,
    typechecker::{
        error::{Label, TypeError},
        scope::{DeclarationKind, TypeOrStub},
        types::{Signature, TypeDefinition, TypeName},
    },
    yang::{
        parser::{
            Keyword, YangStmt,
            ast::{Argument, YangStmtSeq},
        },
        refine::{
            EnumDefinition, RangeDefinition, Ranges, RefineAttrs, Refinement,
            YangStatus, allowed_restriction_for, required_specifications,
        },
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
                }) => {
                    // containers have a scope of their own
                    let idx = self.block_counter;
                    self.block_counter += 1;
                    let block_scope = self
                        .type_info
                        .scope_graph
                        .wrap(scope, ScopeType::Block(idx));

                    self.body(block_scope, sub_stmts.iter_stmt())
                }
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
        let Some(leaf_name) = stmt.arg_as_ident() else {
            ice!(
                "leaf name `{:?}` is invalid. This should have been caught \
                by the parser",
                stmt.node
            );
        };

        let doc = stmt
            .node
            .description()
            .map(|d| d.to_string())
            .unwrap_or_default();

        match sub_stmts.find_attr_argument("type") {
            Some(Meta {
                id,
                node: Argument::Ident(b_ty),
            }) => {
                println!("[leaf] type {} in scope {:?}", b_ty, scope);
                let decla = self
                    .type_info
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

                let DeclarationKind::Type(TypeOrStub::Type(ty)) = decla.kind
                else {
                    println!("kind {:?}", decla.kind);
                    ice!(
                        "the type `{}` mentioned in this {:?} is not fully \
                         resolved",
                        b_ty,
                        stmt.node.as_ident().unwrap().as_str()
                    )
                };

                self.type_info
                    .scope_graph
                    .insert_declaration(
                        scope,
                        &Meta {
                            node: leaf_name,
                            id: *id,
                        },
                        DeclarationKind::Leaf(leaf_name, ty),
                        doc,
                        |_| false,
                    )
                    .unwrap_or_else(|_| {
                        ice!(
                            "declaration for `{leaf_name}` \
                         in scope {scope:?} already exists"
                        )
                    });
                Ok(())
            }
            // This is prefixed by what MUST be a module prefix
            Some(Meta {
                id,
                node: Argument::PrefixIdent((p, b_ty)),
            }) => {
                let kind =
                    self.evaluate_prefixed_type(scope, stmt, b_ty, p, id)?;

                let DeclarationKind::Type(TypeOrStub::Type(ty)) = kind else {
                    ice!("this type is not fully resolved")
                };

                self.type_info
                    .scope_graph
                    .insert_declaration(
                        scope,
                        &Meta {
                            node: leaf_name,
                            id: *id,
                        },
                        DeclarationKind::Leaf(leaf_name, ty),
                        doc,
                        |_| false,
                    )
                    .unwrap_or_else(|_| {
                        ice!(
                            "declaration for `{leaf_name}` \
                         in scope {scope:?} already exists"
                        )
                    });
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

            println!("[type_def] find type '{:?}'", ty);

            let Some(ty_ident) = ty.as_ident() else {
                ice!("type name {ty} is invalid");
            };

            // shadowing type definitions is NOT allowed in yang modules, so
            // let's check if a type with the same name was already declared
            // in this scope, or a parent scope
            let mut p_scope = Some(scope);
            while p_scope.is_some() {
                if let Some(ty_decla) =
                    self.type_info.scope_graph.declarations.get(
                        &ResolvedName {
                            scope: p_scope.unwrap(),
                            ident: ty_ident,
                        },
                    )
                {
                    return Err(TypeError {
                        description: format!(
                            "the type `{}` has already been \
                         defined in this scope",
                            ty_ident
                        ),
                        location: stmt.id,
                        labels: vec![
                            Label::error("this type", stmt.id),
                            Label::info(
                                "is already defined here",
                                ty_decla.id,
                            ),
                        ],
                        notes: Vec::new(),
                    });
                }
                p_scope = self.type_info.scope_graph.parent(p_scope.unwrap());
            }

            // what is the type the current typedef is based on?
            let (derived_from_ty, base_kind) = match sub_stmts
                .find_attr_argument("type")
            {
                // this is a single identifier for a type, meaning it can be a
                // built-in, or declared in this module, in this scope or in a
                // parent scope.
                Some(Meta {
                    id,
                    node: Argument::Ident(b_ty),
                }) => {
                    println!("[type_def] type {} in scope {:?}", b_ty, scope);
                    let decla = self
                        .type_info
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
                                Label::error(
                                    format!(
                                        "in this {}",
                                        stmt.node.as_ident().unwrap()
                                    ),
                                    stmt.id,
                                ),
                                Label::info("this type is unknown", *id),
                            ],
                            notes: Vec::new(),
                        })?;

                    let ty_sub_stmts = sub_stmts
                        .node
                        .find_attr("type")
                        .map(|s| s.sub_stmts())
                        .into_iter()
                        .flatten();

                    // we have the type definition of the base type, but this
                    // may be a refinement of that base type
                    let refines = self.refinements(
                        &Meta {
                            id: *id,
                            node: *b_ty,
                        },
                        ty_sub_stmts,
                    )?;

                    let kind = DeclarationKind::Type(TypeOrStub::Type(
                        TypeDefinition::YangEnum(
                            TypeName {
                                name: decla.name,
                                arguments: vec![],
                            },
                            refines,
                        ),
                    ));

                    (
                        Meta {
                            id: *id,
                            node: *b_ty,
                        },
                        kind,
                    )
                }
                // This is a type that has a prefix (`module:type`), the
                // prefix must be the prefix of an `import` statement in the
                // module root
                Some(Meta {
                    id,
                    node: Argument::PrefixIdent((p, b_ty)),
                }) => {
                    let kind = self
                        .evaluate_prefixed_type(scope, stmt, b_ty, p, id)?;

                    (
                        Meta {
                            id: *id,
                            node: *b_ty,
                        },
                        kind,
                    )
                }
                _ => {
                    ice!("type name {ty} is not an identifier");
                }
            };

            println!(
                "[declare_types_in_modules] insert typedef for '{}': scope \
                {:?}, base type `{}`",
                ty_ident, scope, derived_from_ty.node
            );

            let dec = self.type_info.scope_graph.insert_declaration(
                scope,
                &Meta {
                    id: kw.id,
                    node: ty_ident,
                },
                base_kind,
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

    fn refinements<'a>(
        &'a self,
        derived_from_ty: &'a Meta<Identifier>,
        stmts: impl Iterator<Item = &'a Meta<Stmt>>,
    ) -> TypeResult<Vec<Refinement>> {
        let mut refines = vec![];

        for y_stmt in stmts {
            let Stmt::YangStmtSeq(YangStmtSeq {
                stmt,
                arg,
                sub_stmts,
            }) = &y_stmt.node
            else {
                ice!(
                    "{y_stmt:?} is not a valid yang statement sequence. This \
                     shouldn't have been parsed."
                )
            };

            let Some(keyword) = stmt.as_keyword() else {
                ice!(
                    "{stmt} is not a keyword. This shouldn't have been parsed."
                );
            };

            let err = TypeError {
                description: format!(
                    "`{:?}` statement is not allowed as a sub-statement \
                        of a `{}` statement",
                    keyword, derived_from_ty.node
                ),
                location: y_stmt.id,
                labels: vec![
                    Label::info(
                        "this statement defined here, has a sub-statement...",
                        derived_from_ty.id,
                    ),
                    Label::error(
                        "defined here, but that's not allowed",
                        y_stmt.id,
                    ),
                ],
                notes: vec![],
            };

            match (
                required_specifications(derived_from_ty.node, keyword),
                allowed_restriction_for(derived_from_ty.node, keyword),
            ) {
                (Some(Keyword::Base), None) => {
                    let Some(arg) = arg else {
                        return Err(err);
                    };

                    let def =
                        crate::yang::refine::BaseDefinition(arg.to_string());
                    refines.push(Refinement::Base(def));
                }
                (Some(Keyword::Enum), None) => {
                    let Some(arg) = arg else {
                        return Err(err);
                    };

                    refines.push(Refinement::Enum(
                        self.enum_definition(
                            arg.as_str().as_str(),
                            sub_stmts,
                        ),
                    ));
                }
                (None, Some(Keyword::Range)) => {
                    println!("range arg {:?}", arg);
                    println!("sub {:#?}", (*sub_stmts).as_str());

                    // this only works for refinements without sub-statments
                    let Some(range_str) = sub_stmts
                        .as_str()
                        .or_else(|| arg.clone().map(|a| a.as_str()))
                    else {
                        return Err(err);
                    };

                    let ranges =
                        Ranges::parse(range_str.as_str()).map_err(|e| {
                            println!("error {:?}", e.0);
                            let mut err_msg = err.clone();
                            err_msg.description = e.0;
                            err_msg.labels[0].message =
                                "this type definition here has a sub-\
                                 statement"
                                    .to_string();
                            err_msg.labels[1].message =
                                "this range is invalid".to_string();
                            err_msg
                        })?;

                    refines.push(Refinement::Range(RangeDefinition {
                        ranges,
                        attrs: RefineAttrs::from(sub_stmts),
                    }));
                }
                (None, Some(refine)) => {
                    ice!(
                        "The refinement `{refine:?}` is currently \
                         unimplemented"
                    );
                }
                (Some(spec), None) => {
                    ice!(
                        "The specification `{spec:?}` is currently \
                         unimplemented for the statement `{:?}` ",
                        y_stmt
                    );
                }
                _ => {
                    ice!(
                        "The refinement `{keyword:?}` is unknown for the \
                        type `{:?}`",
                        derived_from_ty
                    );
                }
            };
        }

        Ok(refines)
    }

    fn enum_definition<'a>(
        &'a self,
        enumeration: &'a str,
        sub_stmts: &'a Meta<Expr>,
    ) -> EnumDefinition {
        let mut def = EnumDefinition::new(enumeration.to_string());

        for enu in sub_stmts.iter_stmt() {
            if let Stmt::YangStmtSeq(seq) = &enu.node {
                match seq.stmt.as_keyword() {
                    Some(Keyword::Description) => {
                        def.description =
                            seq.argument().map(|a| a.node.as_str())
                    }
                    Some(Keyword::Status) => {
                        if let Some(s) = seq.argument().map(|a| {
                            YangStatus::from(a.node.as_str().as_str())
                        }) {
                            def.status = s;
                        }
                    }
                    Some(Keyword::Reference) => {
                        def.reference = seq.argument_as_str();
                    }
                    Some(Keyword::Value) => {
                        def.value = seq
                            .argument_as_str()
                            .and_then(|a| a.parse::<i32>().ok());
                    }
                    Some(kw) => {
                        ice!(
                            "unknown statement `{:?}` in `enum` statement \
                            `{}`",
                            kw,
                            &enumeration
                        );
                    }
                    _ => {
                        ice!(
                            "cannot parse `enum` statement `{}`",
                            enumeration
                        );
                    }
                }
            }
        }

        def
    }
}
