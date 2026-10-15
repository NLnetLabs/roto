#![allow(missing_docs)]

use std::{collections::HashMap, marker::PhantomData};

use rust_decimal::Decimal;

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
// instance-identifier| requre-instance

#[derive(Default)]
pub struct RefineAttrs {
    description: Option<String>,
    error_app_tag: Option<String>,
    error_message: Option<String>,
    reference: Option<String>,
}

pub struct RangeDefinition<T, L: PartialEq + PartialOrd + Ord> {
    ranges: Ranges<L>,
    attrs: RefineAttrs,
    _t: PhantomData<T>,
}

pub trait Length<T> {
    fn len(&self) -> T;
    fn is_empty(&self) -> bool;
}

impl Length<usize> for String {
    fn len(&self) -> usize {
        self.len()
    }

    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

impl<T, L: PartialEq + PartialOrd + Ord + Copy + std::fmt::Debug>
    RangeDefinition<T, L>
{
    fn validate(&self) -> impl Fn(T) -> Option<T>
    where
        L: Sized,
        T: Length<L>,
    {
        |value| {
            let mut res = false;
            let len = value.len();
            for range in self.ranges.0.iter() {
                if range.0 <= len && range.1 >= len {
                    res = true;
                } else {
                    return None;
                }
            }

            if res { Some(value) } else { None }
        }
    }
}

pub type LengthDefinition<T, L> = RangeDefinition<T, L>;

pub struct Ranges<T: PartialEq>(Vec<(T, T)>);

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

pub enum YangStatus {
    Current,
    Deprecated,
    Obsolete,
}

pub struct PatternDefinition {
    modifier: PatternModifier,
    attrs: RefineAttrs,
}

pub enum PatternModifier {
    InvertMatch,
}

pub trait RefinedValue
where
    Self: Sized,
{
    type Input;
    fn validate(
        apply_fn: impl Fn(Self) -> Option<Self>,
        value: Self::Input,
    ) -> Option<Self>;
    fn convert() -> impl Fn(Self::Input) -> Self;
}

#[derive(Copy, Clone, std::fmt::Debug)]
pub struct Decimal64 {
    value: Decimal,
}

impl RefinedValue for Decimal64 {
    type Input = String;

    fn validate(
        apply_fn: impl Fn(Self) -> Option<Self>,
        value: Self::Input,
    ) -> Option<Self> {
        (apply_fn)((Self::convert)()(value))
    }

    fn convert() -> impl Fn(Self::Input) -> Self {
        |v| Decimal64 {
            value: Decimal::from_str_exact(&v).unwrap(),
        }
    }
}

impl Length<Decimal> for Decimal64 {
    fn len(&self) -> Decimal {
        self.value
    }

    fn is_empty(&self) -> bool {
        false
    }
}

impl RefinedValue for String {
    type Input = String;

    fn validate(
        apply_fn: impl Fn(Self) -> Option<Self>,
        value: Self::Input,
    ) -> Option<Self> {
        (apply_fn)(value)
    }

    fn convert() -> impl Fn(Self::Input) -> Self {
        |v| -> Self { v }
    }
}

//----------- playground -------------------------------------------
pub fn main() {
    // let type_defs = HashMap::<u32, LengthDefinition>::new();

    let length_def = LengthDefinition {
        ranges: Ranges(vec![(Decimal::new(1, 0), Decimal::new(32, 0))]),
        attrs: RefineAttrs::default(),
        _t: PhantomData,
    };

    let dec1 = Decimal64::validate(length_def.validate(), "1.0".to_string())
        .unwrap();
    println!("val#1 {dec1:?}",);

    println!(
        "val#2 {:?}",
        Decimal64::validate(length_def.validate(), "64.0".to_string())
    );

    println!(
        "val#3 {:?}",
        Decimal64::validate(length_def.validate(), "20.0".to_string())
    );

    let string_length_def = LengthDefinition {
        ranges: Ranges(vec![(1_usize, 10)]),
        attrs: RefineAttrs::default(),
        _t: PhantomData,
    };

    let val_s = String::validate(
        string_length_def.validate(),
        "someshit".to_string(),
    );
    println!("string#1 {val_s:?}");

    let val_s = String::validate(
        string_length_def.validate(),
        "someshitistoolong".to_string(),
    );
    println!("string#1 {val_s:?}");
}
