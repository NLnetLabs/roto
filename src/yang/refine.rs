#![allow(missing_docs)]

use rust_decimal::Decimal;

use crate::ast::{Expr, Identifier, Stmt};
use crate::parser::ParseError;
use crate::parser::meta::Meta;
use crate::yang::parser::YangStmt;
use crate::yang::parser::ast::YangStmtSeq;
use crate::yang::parser::token::Keyword;

// Restrictions
//
// types              | refinements
// --------------------------------------------
// all integer types  | range
// decimal64          | range, fraction-digits
// string             | length
// enumeration        | enum
// bits               | bit
// binary             | length
// leafref            | require-instance
// instance-identifier| require-instance

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Refinement {
    Range(RangeDefinition),
    Length(LengthDefinition),
    FractionDigits(FractionDigitsDefinition),
    Pattern(PatternDefinition),
    Base(BaseDefinition),
    Enum(EnumDefinition),
    InstanceIndentifier(RequireInstanceDefinition),
}

impl Refinement {
    pub fn validate(&self, value: impl Length) -> bool {
        match self {
            Refinement::Range(def) => def.validate(&value),
            Refinement::Length(def) => def.validate(&value),
            Refinement::FractionDigits(def) => def.validate(&value),
            Refinement::Pattern(def) => def.validate(&value),
            Refinement::Base(_def) => true,
            Refinement::InstanceIndentifier(def) => def.validate(value),
            Refinement::Enum(enum_definition) => {
                enum_definition.validate(value)
            }
        }
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq, Hash)]
pub struct RefineAttrs {
    description: Option<String>,
    error_app_tag: Option<String>,
    error_message: Option<String>,
    reference: Option<String>,
}

impl From<&Meta<Expr>> for RefineAttrs {
    fn from(value: &Meta<Expr>) -> Self {
        let mut attrs = RefineAttrs::default();
        for stmt in value.iter_stmt() {
            if let Stmt::YangStmtSeq(YangStmtSeq {
                stmt, sub_stmts, ..
            }) = &stmt.node
            {
                // None have these can have sub-statements, so the argument is
                // in the first sub-statement
                match stmt.as_keyword() {
                    Some(Keyword::Description) => {
                        attrs.description = sub_stmts.as_str()
                    }
                    Some(Keyword::ErrorAppTag) => {
                        attrs.error_app_tag = sub_stmts.as_str()
                    }
                    Some(Keyword::ErrorMessage) => {
                        attrs.error_message = sub_stmts.as_str()
                    }
                    Some(Keyword::Reference) => {
                        attrs.reference = sub_stmts.as_str()
                    }
                    _ => {}
                }
            }
        }

        attrs
    }
}

pub struct RefineError(pub String);

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FractionDigitsDefinition {
    len: usize,
    attrs: RefineAttrs,
}

impl FractionDigitsDefinition {
    fn validate(&self, _value: &impl Length) -> bool {
        todo!()
    }
}

pub trait Length {
    fn len(&self) -> isize;
    fn is_empty(&self) -> bool;
    fn to_string(&self) -> String;
}

impl Length for &String {
    fn len(&self) -> isize {
        String::len(self) as isize
    }

    fn is_empty(&self) -> bool {
        String::is_empty(self)
    }

    fn to_string(&self) -> String {
        String::to_string(self)
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct RangeDefinition {
    pub ranges: Ranges,
    pub attrs: RefineAttrs,
}

impl RangeDefinition {
    fn validate(&self, value: &impl Length) -> bool {
        let mut res = false;
        let len = value.len();
        for range in self.ranges.0.iter() {
            if range.0 <= len && range.1 >= len {
                res = true;
            } else {
                return false;
            }
        }

        res
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct LengthDefinition {
    pub ranges: Ranges,
    pub attrs: RefineAttrs,
}

impl LengthDefinition {
    fn validate(&self, value: &impl Length) -> bool {
        let mut res = false;
        let len = value.len();
        for range in self.ranges.0.iter() {
            if range.0 <= len && range.1 >= len {
                res = true;
            } else {
                return false;
            }
        }

        res
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Ranges(Vec<(isize, isize)>);

impl Ranges {
    pub fn parse(value: &str) -> Result<Self, RefineError> {
        let mut ranges = vec![];
        for range in value.split('|') {
            let mut start_stop = range.split("..");
            let start = start_stop
                .next()
                .and_then(|s| s.split_whitespace().next())
                .ok_or(RefineError(format!(
                    "range(s) `{}` are invalid",
                    range
                )))?;
            let start = start
                .parse::<isize>()
                .ok()
                .or_else(|| {
                    if start.contains("min") {
                        Some(isize::MIN)
                    } else {
                        None
                    }
                })
                .ok_or(RefineError(format!(
                    "the lower boundary `{}` in range `{}` is invalid",
                    start, range
                )))?;

            let end = start_stop
                .next()
                .and_then(|s| s.split_whitespace().next())
                .ok_or(RefineError(format!(
                    "range(s) `{}` are invalid",
                    range
                )))?;

            let end = end
                .parse::<isize>()
                .ok()
                .or_else(|| {
                    if end.contains("max") {
                        Some(isize::MAX)
                    } else {
                        None
                    }
                })
                .ok_or(RefineError(format!(
                    "the upper boundary `{}` in range `{}` is invalid",
                    end, range
                )))?;

            if start_stop.next().is_some() {
                return Err(RefineError(format!(
                    "There are trailing characters `{}` that cannot be \
                    parsed",
                    start_stop.next().unwrap()
                )));
            }

            ranges.push((start, end));
        }

        ranges.sort_by_key(|a| a.0);
        let _ = ranges.iter().try_fold(
            isize::MIN,
            |f_end, &(n_start, n_end)| {
                if n_start <= f_end {
                    return Err(RefineError(format!(
                        "The range `{}-{}` overlaps one of the other ranges. \
                         This is not allowed.",
                        n_start, n_end
                    )));
                }

                if n_end <= n_start {
                    return Err(RefineError(format!(
                        "The lower boundary `{}` \
                         is bigger than the upper boundary `{}`",
                        n_start, f_end
                    )));
                }

                Ok(n_end)
            },
        )?;

        Ok(Ranges(ranges))
    }
}

//  Bit substatements
// +--------------+---------+-------------+
// | substatement | section | cardinality |
// +--------------+---------+-------------+
// | description  | 7.21.3  | 0..1        |
// | if-feature   | 7.20.2  | 0..n        |
// | position     | 9.7.4.2 | 0..1        |
// | reference    | 7.21.4  | 0..1        |
// | status       | 7.21.2  | 0..1        |
// +--------------+---------+-------------+

pub struct BitDefinition {
    description: Option<String>,
    // if-feature-expr-str = < a string that matches the rule >
    //                       < if-feature-expr >
    // if-feature-expr     = if-feature-term
    //                         [sep or-keyword sep if-feature-expr]
    // if-feature-term     = if-feature-factor
    //                         [sep and-keyword sep if-feature-term]
    // if-feature-factor   = not-keyword sep if-feature-factor /
    //                       "(" optsep if-feature-expr optsep ")" /
    //                       identifier-ref-arg
    if_feature: Vec<String>,
    position: Option<u8>,
    reference: Option<String>,
    // The "status" statement takes as an argument one of the strings
    // "current", "deprecated", or "obsolete".
    //
    // you love to hate it, even if yang has enumerations, this has to be
    // a string.
    status: YangStatus,
}

#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Hash)]
pub enum YangStatus {
    #[default]
    Current,
    Deprecated,
    Obsolete,
}

impl From<&str> for YangStatus {
    fn from(value: &str) -> Self {
        match value {
            "deprecated" => YangStatus::Deprecated,
            "obsolete" => YangStatus::Obsolete,
            _ => YangStatus::Current,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct PatternDefinition {
    pub pattern: String,
    pub modifier: PatternModifier,
    pub attrs: RefineAttrs,
}

impl PatternDefinition {
    fn validate(&self, _value: &impl Length) -> bool {
        todo!()
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum PatternModifier {
    InvertMatch,
    None,
}

impl From<&Meta<Expr>> for PatternModifier {
    fn from(sub_stmts: &Meta<Expr>) -> Self {
        for stmt in sub_stmts.iter_stmt() {
            if let Stmt::YangStmtSeq(YangStmtSeq { stmt, .. }) = &stmt.node
                && let Some(Keyword::Modifier) = stmt.as_keyword()
            {
                return PatternModifier::InvertMatch;
            }
        }

        PatternModifier::None
    }
}

// #[derive(Debug)]
// pub struct EnumDefinition(pub Vec<VariantDefinition>);

impl EnumDefinition {
    fn validate(&self, value: impl Length) -> bool {
        true
    }
}

#[derive(Clone, Default, Debug, Eq, PartialEq, Hash)]
pub struct EnumDefinition {
    pub name: String,
    pub status: YangStatus,
    pub description: Option<String>,
    pub if_feature: Option<String>,
    pub reference: Option<String>,
    pub value: Option<i32>,
}

impl EnumDefinition {
    pub fn new(name: String) -> Self {
        EnumDefinition {
            name,
            ..Default::default()
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct BaseDefinition(pub String);

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct RequireInstanceDefinition(bool);

impl RequireInstanceDefinition {
    fn validate(&self, value: impl Length) -> bool {
        match value.to_string().as_str() {
            "true" => true,
            "false" => false,
            _ => false,
        }
    }
}

pub trait RefinedValue
where
    Self: Sized,
{
    type Input;
    fn new(
        def: &RefinedTypes,
        id: TypeId,
        value: &Self::Input,
    ) -> Option<Self>;
    fn convert(id: TypeId, v: &Self::Input) -> Self;
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct TypeId(u32);

impl From<u32> for TypeId {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

#[derive(Copy, Clone, std::fmt::Debug)]
pub struct RDecimal64 {
    id: TypeId,
    value: Decimal,
}

impl RefinedValue for RDecimal64 {
    type Input = String;

    fn new(
        type_refs: &RefinedTypes,
        id: TypeId,
        value: &Self::Input,
    ) -> Option<Self> {
        let v = Self::convert(id, value);
        let def = type_refs.0.get(id.0 as usize)?;
        if def.validate(v) {
            return Some(v);
        }
        None
    }

    fn convert(id: TypeId, v: &Self::Input) -> Self {
        RDecimal64 {
            id,
            value: Decimal::from_str_exact(v).unwrap(),
        }
    }
}

impl Length for RDecimal64 {
    fn len(&self) -> isize {
        self.value.as_f64().floor() as isize
    }

    fn is_empty(&self) -> bool {
        false
    }

    fn to_string(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug, Clone)]
pub struct RString {
    id: TypeId,
    value: String,
}

impl RefinedValue for RString {
    type Input = String;

    fn new(
        type_refs: &RefinedTypes,
        id: TypeId,
        value: &Self::Input,
    ) -> Option<Self> {
        let def = type_refs.get(id)?;
        if def.validate(value) {
            Some(Self::convert(id, value))
        } else {
            None
        }
    }

    fn convert(id: TypeId, v: &Self::Input) -> Self {
        Self {
            id,
            value: v.clone(),
        }
    }
}

#[derive(Debug, Default)]
pub struct RefinedTypes(Vec<Refinement>);

impl RefinedTypes {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn insert(&mut self, def: Refinement) -> TypeId {
        let id = self.0.len();
        self.0.push(def);
        TypeId(id as u32)
    }

    pub fn get(&self, id: TypeId) -> Option<&Refinement> {
        self.0.get(id.0 as usize)
    }
}

// Type Restrictions
//
// types              | refinements
// --------------------------------------------
// all integer types  | range
// decimal64          | range, fraction-digits
// string             | length
//
// Type Specifications
//
// enumeration        | enum
// bits               | bit
// binary             | length
// identityref        | base
// leafref            | require-instance
// instance-identifier| require-instance

pub fn required_specifications(
    ty: Identifier,
    stmt: Keyword,
) -> Option<Keyword> {
    let mut kw = None;
    match ty.as_str() {
        "identityref" if Keyword::Base == stmt => kw = Some(Keyword::Base),
        "binary" if Keyword::Length == stmt => kw = Some(Keyword::Length),
        "bits" if Keyword::Bit == stmt => kw = Some(Keyword::Bit),
        "decimal64" if Keyword::FractionDigits == stmt => {
            kw = Some(Keyword::FractionDigits)
        }
        "enumeration" if stmt == Keyword::Enum => kw = Some(Keyword::Enum),
        // [Keyword::Value, Keyword::Status, Keyword::Reference]
        //     .contains(&stmt)
        "instance-identifier" if Keyword::RequireInstance == stmt => {
            kw = Some(Keyword::RequireInstance)
        }
        _ => {}
    };

    kw
}

pub fn allowed_restriction_for(
    ty: Identifier,
    stmt: Keyword,
) -> Option<Keyword> {
    let mut kw = None;
    match ty.as_str() {
        "int8" | "int16" | "int32" | "int64" | "uint8" | "uint16"
        | "uint32" | "uint64" | "decimal64"
            if stmt == Keyword::Range =>
        {
            kw = Some(Keyword::Range)
        }
        "string" if stmt == Keyword::Length => kw = Some(Keyword::Length),
        "string" if stmt == Keyword::Pattern => kw = Some(Keyword::Pattern),
        "identityref" if stmt == Keyword::Base => kw = Some(Keyword::Base),
        _ => {}
    };

    kw
}

//----------- playground -----------------------------------------------------

pub fn main() {
    let mut type_defs = RefinedTypes::new();

    let length_def = LengthDefinition {
        ranges: Ranges(vec![(1, 32)]),
        attrs: RefineAttrs::default(),
    };

    let id = type_defs.insert(Refinement::Length(length_def));
    let dec1 = RDecimal64::new(&type_defs, id, &"1.0".to_string()).unwrap();

    println!("val#1 {dec1:?}",);
    println!(
        "val#2 {:?}",
        RDecimal64::new(&type_defs, id, &"64.0".to_string())
    );
    println!(
        "val#3 {:?}",
        RDecimal64::new(&type_defs, id, &"20.0".to_string())
    );

    let string_length_def = LengthDefinition {
        ranges: Ranges(vec![(1_isize, 10)]),
        attrs: RefineAttrs::default(),
    };
    let id = type_defs.insert(Refinement::Length(string_length_def));

    let val_s = RString::new(&type_defs, id, &"someshit".to_string());
    println!("string#1 {val_s:?}");

    let val_s =
        RString::new(&type_defs, id, &"someshitistoolong".to_string());
    println!("string#1 {val_s:?}");

    println!("refinements {:#?}", type_defs);
}
