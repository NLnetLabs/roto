use std::any::Any;
use std::fmt::Debug;
use std::fmt::Display;
use std::rc::Rc;

use super::ir::Operand;

/// A Roto value with type information at runtime
///
/// The purpose of [`IrValue`] is to provide a safe way to test our
/// generated code. It is the value that is generally used by the HIR.
#[derive(Clone, Debug)]
pub enum IrValue {
    Bool(bool),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    Pointer(usize),
    Runtime(Rc<dyn Any>),
}

/// The types for [`IrValue`]s
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IrType {
    Bool,
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    Pointer,
    Rt,
}

impl IrType {
    /// The size of the type in bytes
    pub fn bytes(&self) -> usize {
        use IrType::*;
        match self {
            Bool | U8 | I8 => 1,
            U16 | I16 => 2,
            U32 | I32 => 4,
            U64 | I64 => 8,
            Pointer => (usize::BITS / 8) as usize,
            Rt => todo!(),
        }
    }
}

impl Display for IrType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use IrType::*;
        let s = match self {
            Bool => "Bool",
            U8 => "U8",
            U16 => "U16",
            U32 => "U32",
            U64 => "U64",
            I8 => "I8",
            I16 => "I16",
            I32 => "I32",
            I64 => "I64",
            Pointer => "Pointer",
            Rt => "Rt",
        };
        write!(f, "{s}")
    }
}

impl PartialEq for IrValue {
    fn eq(&self, other: &Self) -> bool {
        use IrValue::*;
        match (self, other) {
            (Bool(l), Bool(r)) => l == r,
            (U8(l), U8(r)) => l == r,
            (U16(l), U16(r)) => l == r,
            (U32(l), U32(r)) => l == r,
            (I8(l), I8(r)) => l == r,
            (I16(l), I16(r)) => l == r,
            (I32(l), I32(r)) => l == r,
            (Runtime(_), Runtime(_)) => false,
            (Pointer(_), Pointer(_)) => panic!("can't compare pointers"),
            _ => panic!("tried comparing different types"),
        }
    }
}

impl Eq for IrValue {}

impl IrValue {
    pub fn get_type(&self) -> IrType {
        match self {
            Self::Bool(_) => IrType::Bool,
            Self::U8(_) => IrType::U8,
            Self::U16(_) => IrType::U16,
            Self::U32(_) => IrType::U32,
            Self::U64(_) => IrType::U32,
            Self::I8(_) => IrType::I8,
            Self::I16(_) => IrType::I16,
            Self::I32(_) => IrType::I32,
            Self::I64(_) => IrType::I64,
            Self::Pointer(_) => IrType::Pointer,
            Self::Runtime(_) => IrType::Rt,
        }
    }

    pub fn to_any(&self) -> &dyn Any {
        match &self {
            Self::Bool(x) => x,
            Self::U8(x) => x,
            Self::U16(x) => x,
            Self::U32(x) => x,
            Self::U64(x) => x,
            Self::I8(x) => x,
            Self::I16(x) => x,
            Self::I32(x) => x,
            Self::I64(x) => x,
            Self::Pointer(x) => x,
            Self::Runtime(x) => x,
        }
    }

    pub fn from_any(any: Box<dyn Any>) -> Self {
        if let Some(x) = any.downcast_ref() {
            Self::Bool(*x)
        } else if let Some(x) = any.downcast_ref() {
            Self::U8(*x)
        } else if let Some(x) = any.downcast_ref() {
            Self::U16(*x)
        } else if let Some(x) = any.downcast_ref() {
            IrValue::U32(*x)
        } else if let Some(x) = any.downcast_ref() {
            IrValue::U64(*x)
        } else if let Some(x) = any.downcast_ref() {
            IrValue::I8(*x)
        } else if let Some(x) = any.downcast_ref() {
            IrValue::I16(*x)
        } else if let Some(x) = any.downcast_ref() {
            IrValue::I32(*x)
        } else if let Some(x) = any.downcast_ref() {
            IrValue::I64(*x)
        } else {
            IrValue::Runtime(Rc::from(any))
        }
    }

    pub fn as_vec(&self) -> Vec<u8> {
        match self {
            IrValue::Bool(b) => (*b as u8).to_ne_bytes().into(),
            IrValue::U8(x) => x.to_ne_bytes().into(),
            IrValue::U16(x) => x.to_ne_bytes().into(),
            IrValue::U32(x) => x.to_ne_bytes().into(),
            IrValue::U64(x) => x.to_ne_bytes().into(),
            IrValue::I8(x) => x.to_ne_bytes().into(),
            IrValue::I16(x) => x.to_ne_bytes().into(),
            IrValue::I32(x) => x.to_ne_bytes().into(),
            IrValue::I64(x) => x.to_ne_bytes().into(),
            IrValue::Pointer(x) => x.to_ne_bytes().into(),
            IrValue::Runtime(_) => todo!(),
        }
    }

    pub fn from_slice(ty: &IrType, val: &[u8]) -> Self {
        match ty {
            IrType::Bool => {
                let val: &[u8; 1] = val.try_into().unwrap();
                Self::Bool(match val[0] {
                    0 => false,
                    1 => true,
                    _ => panic!("Invalid bool value"),
                })
            }
            IrType::U8 => {
                let val: &[u8; 1] = val.try_into().unwrap();
                Self::U8(u8::from_ne_bytes(*val))
            }
            IrType::U16 => {
                let val: &[u8; 2] = val.try_into().unwrap();
                Self::U16(u16::from_ne_bytes(*val))
            }
            IrType::U32 => {
                let val: &[u8; 4] = val.try_into().unwrap();
                Self::U32(u32::from_ne_bytes(*val))
            }
            IrType::U64 => {
                let val: &[u8; 8] = val.try_into().unwrap();
                Self::U64(u64::from_ne_bytes(*val))
            }
            IrType::I8 => {
                let val: &[u8; 1] = val.try_into().unwrap();
                Self::I8(i8::from_ne_bytes(*val))
            }
            IrType::I16 => {
                let val: &[u8; 2] = val.try_into().unwrap();
                Self::I16(i16::from_ne_bytes(*val))
            }
            IrType::I32 => {
                let val: &[u8; 4] = val.try_into().unwrap();
                Self::I32(i32::from_ne_bytes(*val))
            }
            IrType::I64 => {
                let val: &[u8; 8] = val.try_into().unwrap();
                Self::I64(i64::from_ne_bytes(*val))
            }
            IrType::Pointer => {
                const SIZE: usize = (usize::BITS / 8) as usize;
                let val: &[u8; SIZE] = val.try_into().unwrap();
                Self::Pointer(usize::from_ne_bytes(*val))
            }
            IrType::Rt => todo!(),
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            IrValue::Bool(x) => *x,
            _ => panic!("Invalid value!"),
        }
    }

    pub fn as_u32(&self) -> u32 {
        match self {
            IrValue::U8(x) => *x as u32,
            IrValue::U16(x) => *x as u32,
            IrValue::U32(x) => *x,
            _ => panic!("Invalid value!"),
        }
    }

    pub fn as_i32(&self) -> i32 {
        match self {
            IrValue::I8(x) => *x as i32,
            IrValue::I16(x) => *x as i32,
            IrValue::I32(x) => *x,
            _ => panic!("Invalid value!"),
        }
    }

    pub fn switch_on(&self) -> u32 {
        match self {
            IrValue::Bool(b) => *b as u32,
            IrValue::U8(x) => *x as u32,
            IrValue::U16(x) => *x as u32,
            IrValue::U32(x) => *x,
            _ => todo!(),
        }
    }
}

impl From<IrValue> for Operand {
    fn from(value: IrValue) -> Self {
        Operand::Value(value)
    }
}

impl Display for IrValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use IrValue::*;
        match self {
            Bool(x) => write!(f, "Bool({x})"),
            U8(x) => write!(f, "U8({x})"),
            U16(x) => write!(f, "U16({x})"),
            U32(x) => write!(f, "U32({x})"),
            U64(x) => write!(f, "U64({x})"),
            I8(x) => write!(f, "I8({x})"),
            I16(x) => write!(f, "I16({x})"),
            I32(x) => write!(f, "I32({x})"),
            I64(x) => write!(f, "I32({x})"),
            Pointer(x) => write!(f, "Pointer({x})"),
            Runtime(..) => write!(f, "Runtime(..)"),
        }
    }
}

impl From<bool> for IrValue {
    fn from(value: bool) -> Self {
        IrValue::Bool(value)
    }
}

impl TryFrom<&IrValue> for bool {
    type Error = ();

    fn try_from(value: &IrValue) -> Result<Self, Self::Error> {
        match value {
            IrValue::Bool(x) => Ok(*x),
            _ => Err(()),
        }
    }
}

impl TryFrom<IrValue> for bool {
    type Error = ();

    fn try_from(value: IrValue) -> Result<Self, Self::Error> {
        match value {
            IrValue::Bool(x) => Ok(x),
            _ => Err(()),
        }
    }
}

impl From<u8> for IrValue {
    fn from(value: u8) -> Self {
        IrValue::U8(value)
    }
}

impl TryFrom<&IrValue> for u8 {
    type Error = ();

    fn try_from(value: &IrValue) -> Result<Self, Self::Error> {
        match value {
            IrValue::U8(x) => Ok(*x),
            _ => Err(()),
        }
    }
}

impl From<u32> for IrValue {
    fn from(value: u32) -> Self {
        IrValue::U32(value)
    }
}

impl TryFrom<&IrValue> for u32 {
    type Error = ();

    fn try_from(value: &IrValue) -> Result<Self, Self::Error> {
        match value {
            IrValue::U32(x) => Ok(*x),
            _ => Err(()),
        }
    }
}
