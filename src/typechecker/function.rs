//! Type checking function-like items

use crate::{
    ast::{self, Identifier},
    parser::meta::Meta,
    typechecker::types::Signature,
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
