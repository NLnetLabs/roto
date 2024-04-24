use crate::{
    ast::{self, Identifier},
    parser::meta::Meta,
    typechecker::error,
};

use super::{
    expr::Context,
    scope::Scope,
    types::{Primitive, Type},
    TypeChecker, TypeResult,
};

impl TypeChecker<'_, '_> {
    pub fn filter_map(
        &mut self,
        scope: &Scope,
        filter_map: &ast::FilterMap,
    ) -> TypeResult<Type> {
        let ast::FilterMap {
            filter_type,
            ident,
            params,
            body: ast::FilterMapBody { define, apply },
        } = filter_map;
        let mut scope = scope.wrap(&ident.0);

        let params = self.params(params)?;
        for (v, t) in &params {
            self.insert_var(&mut scope, v, t)?;
        }

        self.define_section(&mut scope, define)?;

        let a = self.fresh_var();
        let r = self.fresh_var();
        let ty = Type::Verdict(Box::new(a), Box::new(r));

        let ctx = Context {
            expected_type: ty.clone(),
            function_return_type: Some(ty.clone()),
        };

        self.block(&scope, &ctx, apply)?;

        Ok(match filter_type {
            ast::FilterType::FilterMap => Type::FilterMap(params),
            ast::FilterType::Filter => Type::Filter(params),
        })
    }

    fn define_section(
        &mut self,
        scope: &mut Scope,
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
            self.insert_var(scope, ident, ty)?;

            // We want the fully qualified name to be stored, so we do a lookup
            // This won't fail because we just added it.
            self.get_var(scope, ident).unwrap();
        }

        Ok(())
    }

    pub fn term(
        &mut self,
        scope: &Scope,
        term_section: &ast::TermDeclaration,
    ) -> TypeResult<()> {
        let ast::TermDeclaration {
            ident,
            params,
            body,
        } = term_section;

        let mut scope = scope.wrap(&ident.0);

        let params = self.params(params)?;
        for (v, t) in &params {
            self.insert_var(&mut scope, v, t)?;
        }

        let ctx = Context {
            expected_type: Type::Primitive(Primitive::Bool),
            function_return_type: Some(Type::Primitive(Primitive::Bool)),
        };

        self.block(&scope, &ctx, body)?;
        Ok(())
    }

    pub fn term_type(
        &mut self,
        dec: &ast::TermDeclaration,
    ) -> TypeResult<Type> {
        Ok(Type::Term(self.params(&dec.params)?))
    }

    pub fn action(
        &mut self,
        scope: &Scope,
        action_section: &ast::ActionDeclaration,
    ) -> TypeResult<()> {
        let ast::ActionDeclaration {
            ident,
            params,
            body,
        } = action_section;

        let mut scope = scope.wrap(&ident.0);

        // Insert params into the scope
        let params = self.params(params)?;
        for (v, t) in &params {
            self.insert_var(&mut scope, v, t)?;
        }

        let ctx = Context {
            expected_type: Type::Primitive(Primitive::Unit),
            function_return_type: Some(Type::Primitive(Primitive::Unit)),
        };

        self.block(&scope, &ctx, body)?;

        Ok(())
    }

    pub fn action_type(
        &mut self,
        dec: &ast::ActionDeclaration,
    ) -> TypeResult<Type> {
        Ok(Type::Action(self.params(&dec.params)?))
    }

    fn params(
        &mut self,
        args: &ast::Params,
    ) -> TypeResult<Vec<(Meta<Identifier>, Type)>> {
        args.0
            .iter()
            .map(|(field_name, ty)| {
                let Some(ty) = self.get_type(ty) else {
                    return Err(error::undeclared_type(ty));
                };

                let ty = ty.clone();
                Ok((field_name.clone(), ty))
            })
            .collect()
    }
}
