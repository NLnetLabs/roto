use crate::{
    ast::{self, Identifier},
    parser::meta::Meta,
};

use super::{
    expr::Context,
    scope::{ScopeRef, ScopeType},
    types::{Primitive, Type},
    TypeChecker, TypeResult,
};

impl TypeChecker {
    pub fn filter_map(
        &mut self,
        scope: ScopeRef,
        filter_map: &ast::FilterMap,
    ) -> TypeResult<Type> {
        let ast::FilterMap {
            filter_type,
            ident,
            params,
            body,
        } = filter_map;

        let scope = self
            .type_info
            .scope_graph
            .wrap(scope, ScopeType::Function(ident.node));
        self.type_info.function_scopes.insert(ident.id, scope);

        let params = self.params(scope, params)?;
        for (v, t) in &params {
            self.insert_var(scope, v.clone(), t)?;
        }

        let a = self.fresh_var();
        let r = self.fresh_var();
        let ty = Type::Verdict(Box::new(a.clone()), Box::new(r.clone()));

        let ctx = Context {
            expected_type: ty.clone(),
            function_return_type: Some(ty.clone()),
        };

        self.block(scope, &ctx, body)?;

        if let Type::Var(x) = self.resolve_type(&a) {
            self.unify(
                &Type::Var(x),
                &Type::Primitive(Primitive::Unit),
                filter_map.ident.id,
                None,
            )
            .unwrap();
        }
        if let Type::Var(x) = self.resolve_type(&r) {
            self.unify(
                &Type::Var(x),
                &Type::Primitive(Primitive::Unit),
                filter_map.ident.id,
                None,
            )
            .unwrap();
        }

        let param_types = params.into_iter().map(|(_, t)| t).collect();
        Ok(match filter_type {
            ast::FilterType::FilterMap => Type::FilterMap(param_types),
            ast::FilterType::Filter => Type::Filter(param_types),
        })
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
            self.resolve_type_path(scope, ret)?
        } else {
            Type::Primitive(Primitive::Unit)
        };

        let ctx = Context {
            expected_type: ret.clone(),
            function_return_type: Some(ret),
        };

        self.block(scope, &ctx, body)?;
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
            Type::Function(
                Vec::new(),
                Box::new(Type::Primitive(Primitive::Unit)),
            ),
        )?;

        let scope = self
            .type_info
            .scope_graph
            .wrap(scope, ScopeType::Function(name.node));

        self.type_info.function_scopes.insert(name.id, scope);

        let unit = Box::new(Type::Primitive(Primitive::Unit));
        let ret = Type::Verdict(unit.clone(), unit);
        let ctx = Context {
            expected_type: ret.clone(),
            function_return_type: Some(ret),
        };
        self.block(scope, &ctx, body)?;
        Ok(())
    }

    pub fn function_type(
        &mut self,
        scope: ScopeRef,
        dec: &ast::FunctionDeclaration,
    ) -> TypeResult<Type> {
        let ret = if let Some(ret) = &dec.ret {
            self.resolve_type_path(scope, ret)?
        } else {
            Type::Primitive(Primitive::Unit)
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
        let accept = Box::new(self.fresh_var());
        let reject = Box::new(self.fresh_var());
        let param_types = self
            .params(scope, &dec.params)?
            .into_iter()
            .map(|(_, t)| t)
            .collect();
        Ok(Type::Function(
            param_types,
            Box::new(Type::Verdict(accept, reject)),
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
                let ty = self.resolve_type_path(scope, ty)?;
                Ok((field_name.clone(), ty))
            })
            .collect()
    }
}
