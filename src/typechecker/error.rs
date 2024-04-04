use std::fmt::Display;

use crate::{
    ast::Identifier,
    parser::span::{Span, Spanned},
};

use super::types::Type;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Level {
    Error,
    Info,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Label {
    pub level: Level,
    pub span: Span,
    pub message: String,
}

impl Label {
    fn error(msg: impl Display, span: Span) -> Self {
        Label {
            level: Level::Error,
            span,
            message: msg.to_string(),
        }
    }

    fn info(msg: impl Display, span: Span) -> Self {
        Label {
            level: Level::Info,
            span,
            message: msg.to_string(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeError {
    pub description: String,
    pub location: Span,
    pub labels: Vec<Label>,
}

pub fn simple(
    description: impl Display,
    msg: impl Display,
    span: Span,
) -> TypeError {
    TypeError {
        description: description.to_string(),
        location: span,
        labels: vec![Label::error(msg, span)],
    }
}

pub fn duplicate_fields(field_name: &str, locations: &[Span]) -> TypeError {
    TypeError {
        description: format!(
            "field `{field_name}` appears multiple times in the same record"
        ),
        location: locations[0].clone(),
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

pub fn undeclared_type(ty: &Spanned<Identifier>) -> TypeError {
    TypeError {
        description: format!("cannot find type `{ty}`"),
        location: ty.span,
        labels: vec![Label::error("not found", ty.span)],
    }
}

pub fn missing_fields(
    fields: &[String],
    type_name: &Spanned<Identifier>,
    span: Span,
) -> TypeError {
    let description = if fields.len() > 1 {
        let fields = join_quoted(fields);
        format!("missing fields {fields} in record literal for `{type_name}`")
    } else {
        format!(
            "missing field `{}` in record literal for `{type_name}`",
            fields[0]
        )
    };
    TypeError {
        description,
        location: span,
        labels: vec![Label::error(
            format!("missing {}", join_quoted(fields)),
            span,
        )],
    }
}

pub fn tried_to_overwrite_builtin(
    type_name: &Spanned<Identifier>,
) -> TypeError {
    TypeError {
        description: format!(
            "type `{type_name}` is a built-in type and cannot be overwritten"
        ),
        location: type_name.span,
        labels: vec![Label::error("declared here", type_name.span)],
    }
}

pub fn declared_twice(
    new_declaration: &Spanned<Identifier>,
    old_declaration: Span,
) -> TypeError {
    TypeError {
        description: format!(
            "type `{new_declaration}` is declared multiple times"
        ),
        location: new_declaration.span,
        labels: vec![
            Label::error("cannot overwrite type", new_declaration.span),
            Label::info("previously declared here", old_declaration),
        ],
    }
}

pub fn number_of_arguments_dont_match(
    call_type: &str,
    method_name: &Spanned<Identifier>,
    takes: usize,
    given: usize,
) -> TypeError {
    TypeError {
        description: format!(
            "{call_type} `{method_name}` takes {takes} arguments but {given} arguments were given"
        ),
        location: method_name.span,
        labels: vec![Label::error(
            format!("takes {takes} arguments but {given} arguments were given"),
            method_name.span)
        ],
    }
}

pub fn can_only_match_on_enum(ty: &Type, span: Span) -> TypeError {
    TypeError {
        description: format!(
            "cannot match on the type `{ty}`, \
            because only matching on enums is supported."
        ),
        location: span,
        labels: vec![Label::error(
            format!("cannot match on type `{ty}`"),
            span,
        )],
    }
}

pub fn variant_does_not_have_field(
    variant: &Spanned<Identifier>,
    ty: &Type,
) -> TypeError {
    TypeError {
        description: format!("pattern has a data field, but the variant `{variant}` of `{ty}` doesn't have one"),
        location: variant.span,
        labels: vec![Label::error("unexpected data field", variant.span)],
    }
}

pub fn need_data_field_on_pattern(
    variant: &Spanned<Identifier>,
    ty: &Type,
) -> TypeError {
    TypeError {
        description: format!("pattern has no data field, but variant `{variant}` of `{ty}` does have a data field"),
        location: variant.span,
        labels: vec![Label::error("missing data field", variant.span)],
    }
}

pub fn variant_does_not_exist(
    variant: &Spanned<Identifier>,
    ty: &Type,
) -> TypeError {
    TypeError {
        description: format!(
            "the variant `{variant}` does not exist on `{ty}`"
        ),
        location: variant.span,
        labels: vec![Label::error(
            format!("variant does not exist on `{ty}`"),
            variant.span,
        )],
    }
}

pub fn mismatched_types(
    expected: Type,
    got: Type,
    span: Span,
    cause: Option<Span>,
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

pub fn nonexhaustive_match(
    span: Span,
    missing_variants: &[impl Display],
) -> TypeError {
    TypeError {
        description: format!(
            "match expression is not exhaustive, missing variants {}",
            join_quoted(missing_variants)
        ),
        location: span,
        labels: vec![Label::error(
            format!("missing variants {}", join_quoted(missing_variants)),
            span,
        )],
    }
}

pub fn expected_list(span: Span, ty: &Type) -> TypeError {
    TypeError {
        description: format!("expected a list but got `{ty}`"),
        location: span,
        labels: vec![Label::error("not a list", span)],
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
