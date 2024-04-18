use std::fmt::Debug;
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
    BuiltIn(BuiltIn),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BuiltIn {
    IpAddress(std::net::IpAddr),
    Prefix(routecore::addr::Prefix),
    AsPath(routecore::bgp::aspath::AsPath<Vec<u8>>),
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
            _ => todo!(),
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
            _ => todo!(),
        }
    }
}

impl From<routecore::addr::Prefix> for SafeValue {
    fn from(value: routecore::addr::Prefix) -> Self {
        SafeValue::BuiltIn(BuiltIn::Prefix(value))
    }
}

impl TryFrom<&SafeValue> for routecore::addr::Prefix {
    type Error = ();

    fn try_from(value: &SafeValue) -> Result<Self, Self::Error> {
        match value {
            SafeValue::BuiltIn(BuiltIn::Prefix(x)) => Ok(*x),
            _ => Err(()),
        }
    }
}

impl From<std::net::IpAddr> for SafeValue {
    fn from(value: std::net::IpAddr) -> Self {
        SafeValue::BuiltIn(BuiltIn::IpAddress(value))
    }
}

impl TryFrom<&SafeValue> for std::net::IpAddr {
    type Error = ();

    fn try_from(value: &SafeValue) -> Result<Self, Self::Error> {
        match value {
            SafeValue::BuiltIn(BuiltIn::IpAddress(x)) => Ok(*x),
            _ => Err(()),
        }
    }
}

impl From<routecore::bgp::aspath::AsPath<Vec<u8>>> for SafeValue {
    fn from(value: routecore::bgp::aspath::AsPath<Vec<u8>>) -> Self {
        SafeValue::BuiltIn(BuiltIn::AsPath(value))
    }
}

impl<'a> TryFrom<&'a SafeValue> for &'a routecore::bgp::aspath::AsPath<Vec<u8>> {
    type Error = ();

    fn try_from(value: &'a SafeValue) -> Result<Self, Self::Error> {
        match value {
            SafeValue::BuiltIn(BuiltIn::AsPath(x)) => Ok(x),
            _ => Err(()),
        }
    }
}

impl From<bool> for SafeValue {
    fn from(value: bool) -> Self {
        SafeValue::Bool(value)
    }
}

impl TryFrom<&SafeValue> for bool {
    type Error = ();

    fn try_from(value: &SafeValue) -> Result<Self, Self::Error> {
        match value {
            SafeValue::Bool(x) => Ok(*x),
            _ => Err(()),
        }
    }
}

impl TryFrom<SafeValue> for bool {
    type Error = ();

    fn try_from(value: SafeValue) -> Result<Self, Self::Error> {
        match value {
            SafeValue::Bool(x) => Ok(x),
            _ => Err(()),
        }
    }
}

impl From<u8> for SafeValue {
    fn from(value: u8) -> Self {
        SafeValue::U8(value)
    }
}

impl TryFrom<&SafeValue> for u8 {
    type Error = ();

    fn try_from(value: &SafeValue) -> Result<Self, Self::Error> {
        match value {
            SafeValue::U8(x) => Ok(*x),
            _ => Err(()),
        }
    }
}

impl From<u32> for SafeValue {
    fn from(value: u32) -> Self {
        SafeValue::U32(value)
    }
}

impl TryFrom<&SafeValue> for u32 {
    type Error = ();

    fn try_from(value: &SafeValue) -> Result<Self, Self::Error> {
        match value {
            SafeValue::U32(x) => Ok(*x),
            _ => Err(()),
        }
    }
}
