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

use std::collections::{HashMap, hash_map::Entry};

use crate::{
    ast::{self, ArgPattern, Identifier},
    parser::meta::{Meta, MetaId},
    typechecker::{
        TypeChecker, TypeResult,
        error::{Label, TypeError},
        expr::Context,
        scope::ScopeRef,
        types::{Type, TypeDefinition},
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
    pub fn match_expr(
        &mut self,
        scope: ScopeRef,
        ctx: &Context,
        mat: &Meta<ast::Match>,
    ) -> TypeResult<bool> {
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

        if diverges {
            return Err(self.error_simple(
                "condition in `match` is not allowed to diverge",
                "never evaluates to a value",
                expr.id,
            ));
        }

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
        // have the correct types.
        for arm in &arms.node {
            self.unify_with_pattern(scope, &t_expr, &arm.pattern)?;
        }

        Ok(diverges)
    }

    fn unify_with_pattern(
        &mut self,
        scope: ScopeRef,
        ty: &Type,
        pattern: &Meta<ast::Pattern>,
    ) -> TypeResult<VariableSet> {
        let span = pattern.id;
        match &**pattern {
            ast::Pattern::Underscore => Ok(VariableSet::default()),
            ast::Pattern::Parens(inner) => {
                self.unify_with_pattern(scope, ty, inner)?;
                Ok(VariableSet::default())
            }
            ast::Pattern::Int(_, Some(int_type))
            | ast::Pattern::RangeInt(_, _, Some(int_type)) => {
                let ty_pat = Type::from_int_type(*int_type);
                self.unify(ty, &ty_pat, span, None)?;
                Ok(VariableSet::default())
            }
            ast::Pattern::Int(_, None)
            | ast::Pattern::RangeInt(_, _, None) => {
                let ty_pat = self.fresh_int();
                self.unify(ty, &ty_pat, span, None)?;
                Ok(VariableSet::default())
            }
            ast::Pattern::RangeFloat(_, _, Some(float_type)) => {
                let ty_pat = Type::from_float_type(*float_type);
                self.unify(ty, &ty_pat, span, None)?;
                Ok(VariableSet::default())
            }
            ast::Pattern::RangeFloat(_, _, None) => {
                let ty_pat = self.fresh_float();
                self.unify(ty, &ty_pat, span, None)?;
                Ok(VariableSet::default())
            }
            ast::Pattern::Char(_) => {
                self.unify(ty, &Type::char(), span, None)?;
                Ok(VariableSet::default())
            }
            ast::Pattern::String(_) => {
                self.unify(ty, &Type::string(), span, None)?;
                Ok(VariableSet::default())
            }
            ast::Pattern::Bool(_) => {
                self.unify(ty, &Type::bool(), span, None)?;
                Ok(VariableSet::default())
            }
            ast::Pattern::Unit => {
                self.unify(ty, &Type::unit(), span, None)?;
                Ok(VariableSet::default())
            }
            ast::Pattern::Path(path) => {
                let idents = &path.idents;

                let mut idents = idents.iter();
                let (ident, dec) =
                    self.resolve_module_part_of_path(scope, &mut idents)?;

                match dec.kind {
                    super::scope::DeclarationKind::Enum(Some((
                        super::types::TypeDefinition::Enum(enum_ty, _),
                        variant,
                    ))) => {
                        if let Some(field) = idents.next() {
                            return Err(self.error_invalid_path_in_pattern(
                                &enum_ty, field,
                            ));
                        }

                        let expected = variant.fields.len();
                        let got = 0;
                        if expected != got {
                            return Err(self.error_simple(
                                format!("pattern has {got} fields, but the corresponding enum variant has {expected} fields"),
                                format!("expected {expected} fields, got {got}", ),
                                span,
                            ));
                        }
                        self.unify(ty, &Type::Name(enum_ty), span, None)?;
                        Ok(VariableSet::default())
                    }
                    _ if let [ident] = &path.idents[..] => {
                        let mut set = VariableSet::default();
                        set.add(ident.clone(), ty.clone())?;
                        Ok(set)
                    }
                    _ => Err(self.error_expected_enum_variant(ident, dec)),
                }
            }
            ast::Pattern::EnumConstructor { path, arguments } => {
                let idents = &path.idents;
                let mut idents = idents.iter();
                let (ident, dec) =
                    self.resolve_module_part_of_path(scope, &mut idents)?;

                match dec.kind {
                    super::scope::DeclarationKind::Enum(Some((
                        super::types::TypeDefinition::Enum(enum_ty, _),
                        variant,
                    ))) => {
                        if let Some(field) = idents.next() {
                            return Err(self.error_invalid_path_in_pattern(
                                &enum_ty, field,
                            ));
                        }

                        let expected = variant.fields.len();
                        let got = arguments.len();
                        if expected != got {
                            return Err(self.error_simple(
                                format!("pattern has {got} fields, but the corresponding enum variant has {expected} fields"),
                                format!("expected {expected} fields, got {got}", ),
                                span,
                            ));
                        }

                        let mut vars = VariableSet::default();
                        for (field_ty, arg_pattern) in
                            variant.fields.iter().zip(arguments.iter())
                        {
                            let ArgPattern::Pattern(p) = &arg_pattern.node
                            else {
                                return Err(self.error_simple(
                                    "rest patterns in enum variants are not supported yet",
                                    "not supported yet",
                                    arg_pattern.id,
                                ));
                            };
                            let new_vars =
                                self.unify_with_pattern(scope, field_ty, p)?;
                            vars.extend(new_vars)?;
                        }
                        self.unify(ty, &Type::Name(enum_ty), span, None)?;
                        Ok(vars)
                    }
                    _ => Err(self.error_expected_enum_variant(ident, dec)),
                }
            }
            ast::Pattern::RecordConstructor {
                path,
                fields: pattern_fields,
            } => {
                let idents = &path.idents;
                let mut idents = idents.iter();
                let (ident, dec) =
                    self.resolve_module_part_of_path(scope, &mut idents)?;

                match dec.kind {
                    super::scope::DeclarationKind::Type(
                        super::scope::TypeOrStub::Type(
                            TypeDefinition::Record(record_ty, record_fields),
                        ),
                    ) => {
                        self.unify(
                            ty,
                            &Type::Name(record_ty.clone()),
                            span,
                            None,
                        )?;

                        let mut seen_fields =
                            HashMap::<Identifier, MetaId>::new();

                        let mut found_rest = false;
                        let mut vars = VariableSet::default();
                        for field in pattern_fields.iter() {
                            match &**field {
                                ast::FieldPattern::Rest => {
                                    if found_rest {
                                        return Err(self.error_simple(
                                            "cannot use rest pattern `..` multiple times in a pattern",
                                            "can only be used once",
                                            field.id,
                                        ));
                                    }
                                    found_rest = true;
                                }
                                ast::FieldPattern::Ident(ident) => {
                                    if seen_fields.contains_key(ident) {
                                        let old_id =
                                            seen_fields.get(ident).unwrap();
                                        return Err(self
                                            .error_field_used_multiple_times(
                                                ident, *old_id,
                                            ));
                                    }
                                    seen_fields.insert(ident.node, ident.id);
                                    if let Some((_, ty)) = record_fields
                                        .iter()
                                        .find(|(k, _)| k.node == ident.node)
                                    {
                                        vars.add(ident.clone(), ty.clone())?;
                                    } else {
                                        return Err(self
                                            .error_no_field_on_type(
                                                &record_ty, ident,
                                            ));
                                    }
                                }
                                ast::FieldPattern::Pattern(
                                    ident,
                                    field_pattern,
                                ) => {
                                    if seen_fields.contains_key(ident) {
                                        let old_id =
                                            seen_fields.get(ident).unwrap();
                                        return Err(self
                                            .error_field_used_multiple_times(
                                                ident, *old_id,
                                            ));
                                    }
                                    seen_fields.insert(ident.node, ident.id);
                                    if let Some((_, field_ty)) = record_fields
                                        .iter()
                                        .find(|(k, _)| k == ident)
                                    {
                                        self.unify_with_pattern(
                                            scope,
                                            field_ty,
                                            field_pattern,
                                        )?;
                                    } else {
                                        return Err(self
                                            .error_no_field_on_type(
                                                &record_ty, ident,
                                            ));
                                    }
                                }
                            }
                        }
                        Ok(vars)
                    }
                    _ => {
                        return Err(self.error_expected_record(ident, dec));
                    }
                }
            }
            ast::Pattern::Or(patterns) => {
                let mut sets = Vec::new();
                for pattern in patterns {
                    let set = self.unify_with_pattern(scope, ty, pattern)?;
                    sets.push(Meta {
                        id: pattern.id,
                        node: set,
                    });
                }
                self.combine_variable_sets(span, sets)
            }
        }
    }

    fn combine_variable_sets(
        &mut self,
        span: MetaId,
        mut sets: Vec<Meta<VariableSet>>,
    ) -> TypeResult<VariableSet> {
        let mut out = VariableSet::default();
        while let Some((key, value)) =
            sets.iter().flat_map(|s| s.vars.iter()).next()
        {
            let key = *key;
            let value = value.clone();
            out.vars.insert(key, value.clone());

            let mut results = Vec::new();
            for secondary_set in &mut sets {
                let Some(secondary_value) = secondary_set.vars.remove(&key)
                else {
                    results.push(Err(secondary_set.id));
                    continue;
                };
                self.unify(
                    &value.1,
                    &secondary_value.1,
                    secondary_value.0,
                    None,
                )?;
                results.push(Ok(secondary_value.0));
            }

            if results.iter().any(|r| r.is_err()) {
                let labels = results
                    .iter()
                    .map(|r| match r {
                        Ok(id) => Label::info("variable bound here", *id),
                        Err(id) => Label::error(
                            "variable missing in this pattern",
                            *id,
                        ),
                    })
                    .collect();
                return Err(TypeError {
                    description: format!(
                        "variable `{key}` is not bound in all patterns"
                    ),
                    location: span,
                    labels,
                    notes: Vec::new(),
                });
            }
        }

        Ok(out)
    }
}

#[derive(Default)]
struct VariableSet {
    vars: HashMap<Identifier, (MetaId, Type)>,
}

impl VariableSet {
    fn add(&mut self, ident: Meta<Identifier>, ty: Type) -> TypeResult<()> {
        match self.vars.entry(ident.node) {
            Entry::Occupied(entry) => Err(TypeError {
                description: "variable is bound multiple times in pattern"
                    .into(),
                location: ident.id,
                labels: vec![
                    Label::error("defined again here", ident.id),
                    Label::info("first defined here", entry.get().0),
                ],
                notes: Vec::new(),
            }),
            Entry::Vacant(entry) => {
                entry.insert((ident.id, ty));
                Ok(())
            }
        }
    }

    fn extend(&mut self, other: Self) -> TypeResult<()> {
        for (key, value) in other.vars {
            self.add(
                Meta {
                    id: value.0,
                    node: key,
                },
                value.1,
            )?;
        }
        Ok(())
    }
}
