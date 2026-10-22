//! This module implements the simplification of match expressions.
//!
//! That simplification is done in the type checker because that allows us to
//! emit error messages when either types do not match or when the patterns are
//! not exhaustive.
//!
//! There is extensive literature on this subject, but this implementation is
//! mostly based on the work by Yorick Peterse and Jules Jacobs. We take
//! additional inspiration from the Gleam compiler.
//!
//! - Yorick's implementation: <https://github.com/yorickpeterse/pattern-matching-in-rust>
//! - Jules's paper: <https://julesjacobs.com/notes/patternmatching/patternmatching.pdf>
//!
//! Now let's explain what that entails. We need to simplify each match expression
//! to a simpler shape of the following form:
//!
//! ```roto
//! match x {
//!     c(a1, .., aN) => {}
//!     _ => {}
//! }
//! ```
//!
//! Where `c` is some constructor and `a1..aN` are variables. The tree we get by
//! composing these is our decision tree. This is encoded in the [`Decision`] type.

use crate::{
    ast::{self, Identifier},
    parser::meta::Meta,
    typechecker::{
        TypeChecker, TypeResult, expr::Context, scope::ScopeRef, types::Type,
    },
};

struct Variable;

enum Decision {
    /// We unconditionally accept and run the body.
    Success(ast::Block),

    /// A pattern is missing.
    Failure,

    /// Check whether the guard returns true.
    Guard {
        condition: ast::Expr,
        body: ast::Block,
        fallback: Box<Decision>,
    },

    /// Switch on a given variable with a list of cases.
    Switch {
        variable: Variable,
        cases: Vec<Case>,
        fallback: Option<Box<Decision>>,
    },
}

pub struct Case {
    /// The constructor to test against an input variable.
    constructor: Constructor,

    /// Variables to introduce to the body of this case.
    ///
    /// In a real compiler you'd assign these variables in your IR first, then
    /// generate the code for the sub tree.
    arguments: Vec<Variable>,

    /// The sub tree of this case.
    body: Decision,
}

/// A simplified pattern enum derived from the AST.
enum Pat {
    Wildcard,
    Unit,
    Int(i64),
    Float(f64),
    RangeInt(i64, i64),
    RangeFloat(f64, f64),
    Bool(bool),
    String(String),
    Char(char),
    Or(Vec<Pat>),
    Record {
        fields: Vec<(Identifier, Pat)>,
        rest: bool,
    },
}

/// A type constructor.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Constructor {
    True,
    False,
    Int(i64),
    Variant(Type, usize),
    Range(i64, i64),
}

impl TypeChecker {
    fn match_expr(
        &mut self,
        scope: ScopeRef,
        ctx: &Context,
        mat: &Meta<ast::Match>,
    ) -> TypeResult<()> {
        let span = mat.id;
        let ast::Match { expr, arms } = &mat.node;

        // Before we can do any clever compilation, we need to figure out the
        // types involved. So we'll do as much type checking as we can before
        // doing any transformations.
        //
        // This means that we do a couple of things:
        //  - Unify the type of the examinee with the patterns.
        //  - Check that each "or" pattern has the same variables it introduces.
        //  - Check each body with those variables.

        // First determine the type of the expression.
        let t_expr = self.fresh_var();
        let expr_ctx = ctx.with_type(&t_expr);
        let diverges = self.expr(scope, &expr_ctx, expr)?;

        // Then unify that with all the patterns. The reason is that the
        // patterns might have more information about the type. For example,
        // here the pattern tells us that the type should be `u8`:
        //
        // ```
        // match 10 {
        //    10u8..20 => {},
        //    _ => {}
        // }
        // ```
        //
        // Doing this also makes sure that all the patterns are consistent and
        // correct.
        for arm in &arms.node {
            self.unify_with_pattern(scope, &t_expr, &arm.pattern)?;
        }

        todo!()
    }

    fn unify_with_pattern(
        &mut self,
        scope: ScopeRef,
        ty: &Type,
        pattern: &Meta<ast::Pattern>,
    ) -> TypeResult<()> {
        let span = pattern.id;
        match &**pattern {
            ast::Pattern::Underscore => {}
            ast::Pattern::Parens(inner) => {
                self.unify_with_pattern(scope, ty, inner)?;
            }
            ast::Pattern::Int(_, Some(int_type))
            | ast::Pattern::RangeInt(_, _, Some(int_type)) => {
                let ty_pat = Type::from_int_type(*int_type);
                self.unify(ty, &ty_pat, span, None)?;
            }
            ast::Pattern::Int(_, None)
            | ast::Pattern::RangeInt(_, _, None) => {
                let ty_pat = self.fresh_int();
                self.unify(ty, &ty_pat, span, None)?;
            }
            ast::Pattern::RangeFloat(_, _, Some(float_type)) => {
                let ty_pat = Type::from_float_type(*float_type);
                self.unify(ty, &ty_pat, span, None)?;
            }
            ast::Pattern::RangeFloat(_, _, None) => {
                let ty_pat = self.fresh_float();
                self.unify(ty, &ty_pat, span, None)?;
            }
            ast::Pattern::Char(_) => {
                self.unify(ty, &Type::char(), span, None)?;
            }
            ast::Pattern::String(_) => {
                self.unify(ty, &Type::string(), span, None)?;
            }
            ast::Pattern::Bool(_) => {
                self.unify(ty, &Type::bool(), span, None)?;
            }
            ast::Pattern::Unit => {
                self.unify(ty, &Type::unit(), span, None)?;
            }
            ast::Pattern::Path(path) => {
                let idents = &path.idents;
                let mut idents = idents.iter();
                let (ident, dec) =
                    self.resolve_module_part_of_path(scope, &mut idents)?;

                match dec.kind {
                    super::scope::DeclarationKind::Enum(Some((
                        ty_def,
                        variant,
                    ))) => {
                        if let Some(_field) = idents.next() {
                            todo!(
                                "make a nice error for variant cannot have field"
                            )
                        }
                        todo!()
                    }
                    _ => {
                        todo!("error")
                    }
                }
            }
            ast::Pattern::EnumConstructor { path, arguments } => todo!(),
            ast::Pattern::RecordConstructor { path, fields } => todo!(),
            ast::Pattern::Or(patterns) => {
                for pattern in patterns {
                    self.unify_with_pattern(scope, ty, pattern)?;
                }
            }
        }
        Ok(())
    }
}
