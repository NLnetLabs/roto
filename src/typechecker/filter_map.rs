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

impl TypeChecker<'_> {
    pub fn filter_map(
        &mut self,
        scope: &Scope,
        filter_map: &ast::FilterMap,
    ) -> TypeResult<Type> {
        let ast::FilterMap {
            filter_type: ty,
            ident,
            params,
            body:
                ast::FilterMapBody {
                    define,
                    expressions,
                    apply,
                },
        } = filter_map;
        let mut scope = scope.wrap(&ident.0);

        let args = self.params(&mut scope, params)?;

        self.define_section(&mut scope, define)?;

        for expression in expressions {
            let (v, t) = match expression {
                ast::FilterMapExpr::Term(term_section) => {
                    self.term(&scope, term_section)?
                }
                ast::FilterMapExpr::Action(action_section) => {
                    self.action(&scope, action_section)?
                }
            };
            self.insert_var(&mut scope, &v, t)?;
        }

        let ctx = Context {
            expected_type: Type::Primitive(Primitive::Verdict),
            function_return_type: Some(Type::Primitive(Primitive::Verdict)),
        };

        self.block(&scope, &ctx, apply)?;

        Ok(match ty {
            ast::FilterType::FilterMap => Type::FilterMap(args),
            ast::FilterType::Filter => Type::Filter(args),
        })
    }

    fn define_section(
        &mut self,
        scope: &mut Scope,
        define: &ast::Define,
    ) -> TypeResult<()> {
        let ast::Define {
            body:
                ast::DefineBody {
                    rx_tx_type,
                    assignments,
                },
        } = define;

        match rx_tx_type {
            ast::RxTxType::RxOnly(field_name, ty) => {
                let Some(ty) = self.get_type(ty) else {
                    return Err(error::undeclared_type(ty));
                };
                self.insert_var(scope, field_name, ty.clone())?;
            }
            ast::RxTxType::Split { rx, tx } => {
                let Some(ty) = self.get_type(&rx.1) else {
                    return Err(error::undeclared_type(&rx.1));
                };
                self.insert_var(scope, &rx.0, ty.clone())?;

                let Some(ty) = self.get_type(&tx.1) else {
                    return Err(error::undeclared_type(&tx.1));
                };
                self.insert_var(scope, &tx.0, ty.clone())?;
            }
        }

        for (ident, expr) in assignments {
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
            // This won't fail because we just addded it.
            self.get_var(scope, ident).unwrap();
        }

        Ok(())
    }

    fn term(
        &mut self,
        scope: &Scope,
        term_section: &ast::TermDeclaration,
    ) -> TypeResult<(Meta<Identifier>, Type)> {
        let ast::TermDeclaration {
            ident,
            params,
            body,
        } = term_section;

        let mut scope = scope.wrap(&ident.0);

        let args = self.params(&mut scope, params)?;

        let ctx = Context {
            expected_type: Type::Primitive(Primitive::Bool),
            function_return_type: Some(Type::Primitive(Primitive::Bool)),
        };

        self.block(&scope, &ctx, body)?;

        Ok((ident.clone(), Type::Term(args)))
    }

    fn action(
        &mut self,
        scope: &Scope,
        action_section: &ast::ActionDeclaration,
    ) -> TypeResult<(Meta<Identifier>, Type)> {
        let ast::ActionDeclaration {
            ident,
            params,
            body,
        } = action_section;

        let mut scope = scope.wrap(&ident.0);

        let args = self.params(&mut scope, params)?;

        let ctx = Context {
            expected_type: Type::Primitive(Primitive::Unit),
            function_return_type: Some(Type::Primitive(Primitive::Unit)),
        };

        self.block(&scope, &ctx, body)?;

        Ok((ident.clone(), Type::Action(args)))
    }

    fn params(
        &mut self,
        scope: &mut Scope,
        args: &[(Meta<Identifier>, Meta<Identifier>)],
    ) -> TypeResult<Vec<(String, Type)>> {
        args.iter()
            .map(|(field_name, ty)| {
                let Some(ty) = self.get_type(ty) else {
                    return Err(error::undeclared_type(ty));
                };

                let ty = ty.clone();
                self.insert_var(scope, field_name, &ty)?;
                Ok((field_name.0.to_string(), ty))
            })
            .collect()
    }
}
