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

impl TypeChecker<'_> {
    pub fn filter_map(
        &mut self,
        scope: ScopeRef,
        filter_map: &ast::FilterMap,
    ) -> TypeResult<Type> {
        let ast::FilterMap {
            filter_type,
            ident,
            params,
            body: ast::FilterMapBody { define, apply },
        } = filter_map;

        let scope = self
            .scope_graph
            .wrap(scope, ScopeType::Function(ident.node));
        self.type_info.function_scopes.insert(ident.id, scope);

        let params = self.params(params)?;
        for (v, t) in &params {
            self.insert_var(scope, v.clone(), t)?;
        }

        self.define_section(scope, define)?;

        let a = self.fresh_var();
        let r = self.fresh_var();
        let ty = Type::Verdict(Box::new(a.clone()), Box::new(r.clone()));

        let ctx = Context {
            expected_type: ty.clone(),
            function_return_type: Some(ty.clone()),
        };

        self.block(scope, &ctx, apply)?;

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

        Ok(match filter_type {
            ast::FilterType::FilterMap => Type::FilterMap(params),
            ast::FilterType::Filter => Type::Filter(params),
        })
    }

    fn define_section(
        &mut self,
        scope: ScopeRef,
        define: &[(Meta<Identifier>, Meta<ast::Expr>)],
    ) -> TypeResult<()> {
        for (ident, expr) in define {
            let var = self.fresh_var();
            let ctx = Context {
                expected_type: var.clone(),
                function_return_type: None,
            };
            let diverges = self.expr(scope, &ctx, expr)?;
            if diverges {
                unreachable!(
                    "Something has gone wrong in the type checker. \
                    Divergence should have prohibited elsewhere."
                );
            }
            let ty = self.resolve_type(&var);
            self.insert_var(scope, ident.clone(), ty)?;

            // We want the fully qualified name to be stored, so we do a lookup
            // This won't fail because we just added it.
            self.get_var(scope, ident).unwrap();
        }

        Ok(())
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
            .scope_graph
            .wrap(scope, ScopeType::Function(ident.node));

        self.type_info.function_scopes.insert(ident.id, scope);

        let params = self.params(params)?;
        for (v, t) in &params {
            self.insert_var(scope, v.clone(), t)?;
        }

        let ret = if let Some(ret) = ret {
            let Some(ty) = self.get_type(ret.node) else {
                return Err(self.error_undeclared_type(ret));
            };
            ty.clone()
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

    pub fn function_type(
        &mut self,
        dec: &ast::FunctionDeclaration,
    ) -> TypeResult<Type> {
        let ret = if let Some(ret) = &dec.ret {
            let Some(ty) = self.get_type(ret.node) else {
                return Err(self.error_undeclared_type(ret));
            };
            ty.clone()
        } else {
            Type::Primitive(Primitive::Unit)
        };
        Ok(Type::Function(self.params(&dec.params)?, Box::new(ret)))
    }

    fn params(
        &mut self,
        args: &ast::Params,
    ) -> TypeResult<Vec<(Meta<Identifier>, Type)>> {
        args.0
            .iter()
            .map(|(field_name, ty)| {
                let Some(ty) = self.get_type(ty.node) else {
                    return Err(self.error_undeclared_type(ty));
                };

                let ty = ty.clone();
                Ok((field_name.clone(), ty))
            })
            .collect()
    }
}
