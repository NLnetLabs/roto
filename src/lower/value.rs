use std::any::Any;
use std::fmt::Debug;
use std::fmt::Display;
use std::rc::Rc;

use super::ir::Operand;

pub trait Value: Eq {
    fn as_unit(&self) {}
    fn as_bool(&self) -> bool;
    fn as_u32(&self) -> u32;
    fn switch_on(&self) -> u32;
}

/// A Roto value with type information at runtime
///
/// The purpose of [`SafeValue`] is to provide a safe way to test our
/// generated code. It is the value that is generally used by the HIR.
/// For this value, we prefer ease of use over performance, therefore,
/// the ubiquitous [`Vec`]s and [`Box`]es in this type are not a problem.
#[derive(Clone, Debug)]
pub enum SafeValue {
    Unit,
    Bool(bool),
    U8(u8),
    U16(u16),
    U32(u32),
    Verdict(Box<Verdict>),
    Record(Vec<(String, SafeValue)>),
    Enum(u32, Box<SafeValue>),
    Runtime(Rc<dyn Any>),
}

#[derive(Clone, Debug)]
pub enum Verdict {
    Accept(SafeValue),
    Reject(SafeValue),
}

impl PartialEq for SafeValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::U8(l0), Self::U8(r0)) => l0 == r0,
            (Self::U16(l0), Self::U16(r0)) => l0 == r0,
            (Self::U32(l0), Self::U32(r0)) => l0 == r0,
            (Self::Record(l0), Self::Record(r0)) => l0 == r0,
            (Self::Enum(l0, l1), Self::Enum(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Runtime(_), Self::Runtime(_)) => false,
            _ => {
                core::mem::discriminant(self)
                    == core::mem::discriminant(other)
            }
        }
    }
}

impl Eq for SafeValue {}

impl SafeValue {
    pub fn to_any(&self) -> &dyn Any {
        match &self {
            SafeValue::Unit => &(),
            SafeValue::Bool(x) => x,
            SafeValue::U8(x) => x,
            SafeValue::U16(x) => x,
            SafeValue::U32(x) => x,
            SafeValue::Record(_) => todo!(),
            SafeValue::Enum(_, _) => todo!(),
            SafeValue::Verdict(_) => todo!(),
            SafeValue::Runtime(x) => {
                let y = x.as_ref();
                dbg!(std::any::type_name_of_val(y));
                y
            }
        }
    }

    pub fn from_any(any: Box<dyn Any>) -> Self {
        if let Some(()) = any.downcast_ref() {
            SafeValue::Unit
        } else if let Some(x) = any.downcast_ref() {
            SafeValue::Bool(*x)
        } else if let Some(x) = any.downcast_ref() {
            SafeValue::U8(*x)
        } else if let Some(x) = any.downcast_ref() {
            SafeValue::U16(*x)
        } else if let Some(x) = any.downcast_ref() {
            SafeValue::U32(*x)
        } else {
            SafeValue::Runtime(Rc::from(any))
        }
    }
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
            SafeValue::U32(x) => *x,
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

impl From<SafeValue> for Operand {
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
            SafeValue::Verdict(v) => {
                match v.as_ref() {
                    Verdict::Accept(x) => write!(f, "Accept({x})"),
                    Verdict::Reject(x) => write!(f, "Reject({x})"),
                }
            }
            SafeValue::Runtime(..) => write!(f, "Runtime(..)"),
        }
    }
}

impl TryFrom<SafeValue> for () {
    type Error = ();

    fn try_from(value: SafeValue) -> Result<Self, Self::Error> {
        match value {
            SafeValue::Unit => Ok(()),
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

impl<T, E> TryFrom<SafeValue> for Result<T, E>
where
    T: TryFrom<SafeValue, Error = ()>,
    E: TryFrom<SafeValue, Error = ()>,
{
    type Error = ();

    fn try_from(value: SafeValue) -> Result<Self, Self::Error> {
        match value {
            SafeValue::Verdict(v) => Ok(match v.as_ref() {
                Verdict::Accept(x) => Ok(x.clone().try_into()?),
                Verdict::Reject(x) => Err(x.clone().try_into()?),
            }),
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
