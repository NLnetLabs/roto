//! Type checking function-like items

use crate::{
    ast::{self, Identifier},
    ice,
    parser::meta::Meta,
};

use super::{
    expr::Context,
    scope::{ScopeRef, ScopeType},
    types::Type,
    TypeChecker, TypeResult,
};

impl TypeChecker {
    /// Type check a filter map
    pub fn filter_map(
        &mut self,
        scope: ScopeRef,
        filter_map: &ast::FilterMap,
    ) -> TypeResult<Type> {
        let ast::FilterMap {
            filter_type: _,
            ident,
            params,
            body,
        } = filter_map;

        let ty = self.type_info.type_of(ident);
        let Type::Function(_, return_type) = &ty else {
            ice!()
        };

        let scope = self
            .type_info
            .scope_graph
            .wrap(scope, ScopeType::Function(ident.node));
        self.type_info.function_scopes.insert(ident.id, scope);

        let params = self.params(scope, params)?;
        for (v, t) in &params {
            self.insert_var(scope, v.clone(), t)?;
        }

        let ctx = Context {
            expected_type: *return_type.clone(),
            function_return_type: Some(*return_type.clone()),
        };

        self.block(scope, &ctx, body)?;
        self.resolve_obligations()?;

        let param_types = params.into_iter().map(|(_, t)| t).collect();

        Ok(Type::Function(param_types, Box::new(ty)))
    }

    pub fn function(
        &mut self,
        scope: ScopeRef,
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
            .wrap(scope, ScopeType::Function(ident.node));

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
        };

        self.block(scope, &ctx, body)?;
        self.resolve_obligations()?;

        Ok(())
    }

    pub fn test(
        &mut self,
        scope: ScopeRef,
        test: &ast::Test,
    ) -> TypeResult<()> {
        let ast::Test { ident, body } = test;

        let name = Identifier::from(format!("test#{ident}"));
        let name = Meta {
            id: ident.id,
            node: name,
        };
        self.insert_function(
            scope,
            name.clone(),
            super::types::FunctionDefinition::Roto,
            Vec::new(),
            String::new(),
            Type::Function(
                Vec::new(),
                Box::new(Type::verdict(Type::unit(), Type::unit())),
            ),
        )?;

        let scope = self
            .type_info
            .scope_graph
            .wrap(scope, ScopeType::Function(name.node));

        self.type_info.function_scopes.insert(name.id, scope);

        let ret = Type::verdict(Type::unit(), Type::unit());
        let ctx = Context {
            expected_type: ret.clone(),
            function_return_type: Some(ret),
        };
        self.block(scope, &ctx, body)?;
        self.resolve_obligations()?;
        Ok(())
    }

    pub fn function_type(
        &mut self,
        scope: ScopeRef,
        dec: &ast::FunctionDeclaration,
    ) -> TypeResult<Type> {
        let ret = if let Some(ret) = &dec.ret {
            self.evaluate_type_expr(scope, ret)?
        } else {
            Type::unit()
        };
        let param_types = self
            .params(scope, &dec.params)?
            .into_iter()
            .map(|(_, t)| t)
            .collect();
        Ok(Type::Function(param_types, Box::new(ret)))
    }

    pub fn filter_map_type(
        &mut self,
        scope: ScopeRef,
        dec: &ast::FilterMap,
    ) -> TypeResult<Type> {
        let accept = self.fresh_var();
        let reject = self.fresh_var();
        let param_types = self
            .params(scope, &dec.params)?
            .into_iter()
            .map(|(_, t)| t)
            .collect();
        Ok(Type::Function(
            param_types,
            Box::new(Type::verdict(accept, reject)),
        ))
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
