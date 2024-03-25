use crate::{
    ast::{Identifier, TypeIdentifier},
    parser::span::{Span, Spanned},
};

use super::types::Type;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Label {
    pub span: Span,
    pub message: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeError {
    pub description: String,
    pub location: Span,
    pub labels: Vec<Label>,
}

pub fn simple(description: &str, label: &str, span: Span) -> TypeError {
    TypeError {
        description: description.into(),
        location: span,
        labels: vec![Label {
            span,
            message: label.into(),
        }],
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
            .map(|&span| Label {
                span,
                message: format!("field `{field_name}` declared here"),
            })
            .collect(),
    }
}

pub fn undeclared_type(ty: &Spanned<TypeIdentifier>) -> TypeError {
    TypeError {
        description: format!("cannot find type `{ty}`"),
        location: ty.span,
        labels: vec![Label {
            span: ty.span,
            message: "not found".into(),
        }],
    }
}

pub fn missing_fields(
    fields: &[String],
    type_name: &Spanned<TypeIdentifier>,
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
        labels: vec![Label {
            span,
            message: format!("missing {}", join_quoted(fields)),
        }],
    }
}

pub fn tried_to_overwrite_builtin(
    type_name: &Spanned<TypeIdentifier>,
) -> TypeError {
    TypeError {
        description: format!(
            "type `{type_name}` is a built-in type and cannot be overwritten"
        ),
        location: type_name.span,
        labels: vec![Label {
            span: type_name.span,
            message: "declared here".into(),
        }],
    }
}

pub fn declared_twice(
    new_declaration: &Spanned<TypeIdentifier>,
    old_declaration: Span,
) -> TypeError {
    TypeError {
        description: format!(
            "type `{new_declaration}` is declared multiple times"
        ),
        location: new_declaration.span,
        labels: vec![
            Label {
                span: new_declaration.span,
                message: "cannot overwrite type".into(),
            },
            Label {
                span: old_declaration,
                message: "previously declared here".into(),
            },
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
        labels: vec![Label { span: method_name.span, message: format!("takes {takes} arguments but {given} arguments were given")}],
    }
}

pub fn can_only_match_on_enum(ty: &Type, span: Span) -> TypeError {
    TypeError {
        description: format!(
            "cannot match on the type `{ty}`, \
            because only matching on enums is supported."
        ),
        location: span,
        labels: vec![Label {
            span,
            message: format!("cannot match on type `{ty}`"),
        }],
    }
}

pub fn variant_does_not_have_field(
    variant: &Spanned<Identifier>,
    ty: &Type,
) -> TypeError {
    TypeError {
        description: format!("pattern has a data field, but the variant `{variant}` of `{ty}` doesn't have one"),
        location: variant.span,
        labels: vec![Label { span: variant.span, message: "unexpected data field".into()}],
    }
}

pub fn need_data_field_on_pattern(
    variant: &Spanned<Identifier>,
    ty: &Type,
) -> TypeError {
    TypeError {
        description: format!("pattern has no data field, but variant `{variant}` of `{ty}` does have a data field"),
        location: variant.span,
        labels: vec![Label { span: variant.span, message: format!("missing data field")}],
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
        labels: vec![Label {
            span: variant.span,
            message: format!("variant does not exist on `{ty}`"),
        }],
    }
}

pub fn mismatched_types(
    expected: Type,
    got: Type,
    span: Span,
    cause: Option<Span>,
) -> TypeError {
    let mut labels = vec![Label {
        span,
        message: format!("expected `{expected}`, found `{got}`"),
    }];
    if let Some(span) = cause {
        labels.push(Label {
            span,
            message: format!("expected because this is `{expected}`"),
        });
    }

    TypeError {
        description: "mismatched types".into(),
        location: span,
        labels: vec![],
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
