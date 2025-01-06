//! Type errors

use std::fmt::Display;

use crate::{
    ast::{Expr, Identifier},
    parser::meta::{Meta, MetaId},
};

use super::{types::Type, TypeChecker};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Level {
    Error,
    Info,
}

/// A label is a bit of text attached to a span
#[derive(Clone, Debug)]
pub struct Label {
    pub level: Level,
    pub id: MetaId,
    pub message: String,
}

impl Label {
    /// Create an error label
    fn error(msg: impl Display, id: MetaId) -> Self {
        Label {
            level: Level::Error,
            id,
            message: msg.to_string(),
        }
    }

    /// Create an info label
    fn info(msg: impl Display, id: MetaId) -> Self {
        Label {
            level: Level::Info,
            id,
            message: msg.to_string(),
        }
    }
}

/// A type error displayed to the user
#[derive(Clone, Debug)]
pub struct TypeError {
    pub description: String,
    pub location: MetaId,
    pub labels: Vec<Label>,
}

impl TypeChecker<'_> {
    /// Catch all error with a basic format with just one label
    pub fn error_simple(
        &self,
        description: impl Display,
        msg: impl Display,
        span: MetaId,
    ) -> TypeError {
        TypeError {
            description: description.to_string(),
            location: span,
            labels: vec![Label::error(msg, span)],
        }
    }

    pub fn error_duplicate_fields(
        &self,
        field_name: &str,
        locations: &[MetaId],
    ) -> TypeError {
        TypeError {
            description: format!(
                "field `{field_name}` appears multiple times in the same record"
            ),
            location: locations[0],
            labels: locations
                .iter()
                .map(|&span| {
                    Label::error(
                        format!("field `{field_name}` declared here"),
                        span,
                    )
                })
                .collect(),
        }
    }

    pub fn error_undeclared_type(&self, ty: &Meta<Identifier>) -> TypeError {
        TypeError {
            description: format!("cannot find type `{ty}`",),
            location: ty.id,
            labels: vec![Label::error("not found", ty.id)],
        }
    }

    pub fn error_field_mismatch<'a>(
        &self,
        span: MetaId,
        invalid: impl IntoIterator<Item = &'a Meta<Identifier>>,
        duplicate: impl IntoIterator<Item = &'a Meta<Identifier>>,
        missing: impl IntoIterator<Item = Identifier>,
    ) -> TypeError {
        let missing: Vec<_> =
            missing.into_iter().map(|m| m.as_str()).collect();

        let description = if missing.len() > 1 {
            let fields = join_quoted(missing);
            format!(
                "field mismatch: missing fields {fields} in record literal"
            )
        } else if let [m] = &missing[..] {
            format!(
                "field: mismatch: missing field `{}` in record literal",
                m
            )
        } else {
            "field mismatch".into()
        };

        let mut labels = vec![Label::error(&description, span)];

        for field in invalid {
            labels.push(Label::info("invalid field", field.id));
        }

        for field in duplicate {
            labels.push(Label::info("duplicate field", field.id));
        }

        TypeError {
            description,
            location: span,
            labels,
        }
    }

    pub fn error_tried_to_overwrite_builtin(
        &self,
        type_name: &Meta<Identifier>,
    ) -> TypeError {
        TypeError {
            description: format!(
                "type `{type_name}` is a built-in type and cannot be overwritten"
            ),
            location: type_name.id,
            labels: vec![Label::error("declared here", type_name.id)],
        }
    }

    pub fn error_declared_twice(
        &self,
        new_declaration: &Meta<Identifier>,
        old_declaration: MetaId,
    ) -> TypeError {
        TypeError {
            description: format!(
                "type `{new_declaration}` is declared multiple times"
            ),
            location: new_declaration.id,
            labels: vec![
                Label::error("cannot overwrite type", new_declaration.id),
                Label::info("previously declared here", old_declaration),
            ],
        }
    }

    pub fn error_not_defined(&self, ident: &Meta<Identifier>) -> TypeError {
        TypeError {
            description: format!("cannot find value `{ident}` in this scope"),
            location: ident.id,
            labels: vec![Label::error("not found in this scope", ident.id)],
        }
    }

    pub fn error_number_of_arguments_dont_match(
        &self,
        call_type: &str,
        method_name: &Meta<Identifier>,
        takes: usize,
        given: usize,
    ) -> TypeError {
        TypeError {
            description: format!(
                "{call_type} `{method_name}` takes {takes} arguments but {given} arguments were given"
            ),
            location: method_name.id,
            labels: vec![Label::error(
                format!("takes {takes} arguments but {given} arguments were given"),
                method_name.id)
            ],
        }
    }

    pub fn error_can_only_match_on_enum(
        &self,
        ty: &Type,
        span: MetaId,
    ) -> TypeError {
        TypeError {
            description: format!(
                "cannot match on the type `{ty}`, \
                because only matching on enums is supported.",
            ),
            location: span,
            labels: vec![Label::error(
                format!("cannot match on type `{ty}`"),
                span,
            )],
        }
    }

    pub fn error_variant_does_not_have_field(
        &self,
        variant: &Meta<Identifier>,
        ty: &Type,
    ) -> TypeError {
        TypeError {
            description: format!("pattern has a data field, but the variant `{variant}` of `{ty}` doesn't have one"),
            location: variant.id,
            labels: vec![Label::error("unexpected data field", variant.id)],
        }
    }

    pub fn error_need_data_field_on_pattern(
        &self,
        variant: &Meta<Identifier>,
        ty: &Type,
    ) -> TypeError {
        TypeError {
            description: format!("pattern has no data field, but variant `{variant}` of `{ty}` does have a data field"),
            location: variant.id,
            labels: vec![Label::error("missing data field", variant.id)],
        }
    }

    pub fn error_variant_does_not_exist(
        &self,
        variant: &Meta<Identifier>,
        ty: &Type,
    ) -> TypeError {
        TypeError {
            description: format!(
                "the variant `{variant}` does not exist on `{ty}`"
            ),
            location: variant.id,
            labels: vec![Label::error(
                format!("variant does not exist on `{ty}`"),
                variant.id,
            )],
        }
    }

    pub fn error_mismatched_types(
        &self,
        expected: &Type,
        got: &Type,
        span: MetaId,
        cause: Option<MetaId>,
    ) -> TypeError {
        let mut labels = vec![Label::error(
            format!("expected `{expected}`, found `{got}`"),
            span,
        )];
        if let Some(span) = cause {
            labels.push(Label::info(
                format!("expected because this is `{expected}`"),
                span,
            ));
        }

        TypeError {
            description: "mismatched types".into(),
            location: span,
            labels,
        }
    }

    pub fn error_nonexhaustive_match(
        &self,
        span: MetaId,
        missing_variants: &[Identifier],
    ) -> TypeError {
        let missing_variants: Vec<_> =
            missing_variants.iter().map(|s| s.as_str()).collect();

        TypeError {
            description: format!(
                "match expression is not exhaustive, missing variants {}",
                join_quoted(&missing_variants)
            ),
            location: span,
            labels: vec![Label::error(
                format!("missing variants {}", join_quoted(missing_variants)),
                span,
            )],
        }
    }

    pub fn error_unreachable_expression<T>(
        &self,
        expr: &Meta<T>,
    ) -> TypeError {
        TypeError {
            description: "expression is unreachable".into(),
            location: expr.id,
            labels: vec![Label::error("unreachable", expr.id)],
        }
    }

    pub fn error_cannot_diverge_here(
        &self,
        divergence_type: &str,
        expr: &Meta<Expr>,
    ) -> TypeError {
        TypeError {
            description: format!("cannot `{divergence_type}` here"),
            location: expr.id,
            labels: vec![Label::error("not allowed", expr.id)],
        }
    }
}

fn join_quoted<T: std::fmt::Display>(
    list: impl IntoIterator<Item = T>,
) -> String {
    let mut list: Vec<_> =
        list.into_iter().map(|s| format!("`{s}`")).collect();
    let last = list.pop().unwrap();
    if list.is_empty() {
        return last;
    }
    format!("{} and {last}", list.join(", "))
}
