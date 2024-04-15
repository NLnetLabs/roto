use std::fmt::Display;

use super::ir::Operand;

pub trait Value: Eq {
    fn as_unit(&self) {}
    fn as_bool(&self) -> bool;
    fn as_u32(&self) -> u32;
    fn switch_on(&self) -> u32;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SafeValue {
    Unit,
    Bool(bool),
    U8(u8),
    U16(u16),
    U32(u32),
    Record(Vec<(String, SafeValue)>),
    Enum(u32, Box<SafeValue>),
}

impl Value for SafeValue {
    fn as_bool(&self) -> bool {
        match self {
            SafeValue::Bool(x) => *x,
            _ => panic!("Invalid value!"),
        }
    }

    fn as_u32(&self) -> u32 {
        match self {
            SafeValue::U8(x) => *x as u32,
            SafeValue::U16(x) => *x as u32,
            SafeValue::U32(x) => *x as u32,
            _ => panic!("Invalid value!"),
        }
    }

    fn switch_on(&self) -> u32 {
        match self {
            SafeValue::Bool(b) => *b as u32,
            SafeValue::U8(x) => *x as u32,
            SafeValue::U16(x) => *x as u32,
            SafeValue::U32(x) => *x,
            SafeValue::Enum(x, _) => *x,
            SafeValue::Unit | SafeValue::Record(_) => {
                panic!("Can't switch on this value")
            }
        }
    }
}

impl<P> From<SafeValue> for Operand<P, SafeValue> {
    fn from(value: SafeValue) -> Self {
        Operand::Value(value)
    }
}

impl Display for SafeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SafeValue::Unit => write!(f, "Unit"),
            SafeValue::Bool(x) => write!(f, "Bool({x})"),
            SafeValue::U8(x) => write!(f, "U8({x})"),
            SafeValue::U16(x) => write!(f, "U16({x})"),
            SafeValue::U32(x) => write!(f, "U32({x})"),
            SafeValue::Record(fields) => write!(
                f,
                "{{{}}}",
                fields
                    .iter()
                    .map(|(f, v)| format!("{f}: {v}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            SafeValue::Enum(variant, v) => {
                write!(f, "Enum({variant}, {v})")
            }
        }
    }
}
