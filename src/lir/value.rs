//! Values and types for the IR

use std::any::Any;
use std::fmt::{Debug, Display};

use inetnum::asn::Asn;

use super::Operand;

/// A Roto value with type information at runtime
///
/// The purpose of [`IrValue`] is to provide a safe way to test our
/// generated code. It is the value that is generally used by the IR.
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
    F32(f32),
    F64(f64),
    Asn(Asn),
    Pointer(usize),
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
    F32,
    F64,
    Asn,
    Pointer,
}

impl IrType {
    /// The size of the type in bytes
    pub fn bytes(&self) -> usize {
        use IrType::*;
        match self {
            Bool | U8 | I8 => 1,
            U16 | I16 => 2,
            U32 | I32 | F32 | Asn => 4,
            U64 | I64 | F64 => 8,
            Pointer => (usize::BITS / 8) as usize,
        }
    }

    pub fn alignment(&self) -> usize {
        self.bytes()
    }
}

impl Display for IrType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use IrType::*;
        let s = match self {
            Bool => "bool",
            U8 => "u8",
            U16 => "u16",
            U32 => "u32",
            U64 => "u64",
            I8 => "i8",
            I16 => "i16",
            I32 => "i32",
            I64 => "i64",
            F32 => "f32",
            F64 => "f64",
            Asn => "Asn",
            Pointer => "Pointer",
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
            (Asn(l), Asn(r)) => l == r,
            (Pointer(l), Pointer(r)) => l == r,
            _ => panic!("tried comparing different types"),
        }
    }
}

impl Eq for IrValue {}

impl IrValue {
    pub fn get_type(&self) -> IrType {
        use IrValue::*;
        match self {
            Bool(_) => IrType::Bool,
            U8(_) => IrType::U8,
            U16(_) => IrType::U16,
            U32(_) => IrType::U32,
            U64(_) => IrType::U32,
            I8(_) => IrType::I8,
            I16(_) => IrType::I16,
            I32(_) => IrType::I32,
            I64(_) => IrType::I64,
            F32(_) => IrType::F32,
            F64(_) => IrType::F64,
            Asn(_) => IrType::Asn,
            Pointer(_) => IrType::Pointer,
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
            Self::F32(x) => x,
            Self::F64(x) => x,
            Self::Asn(x) => x,
            Self::Pointer(x) => x,
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
        } else if let Some(x) = any.downcast_ref() {
            IrValue::Asn(*x)
        } else {
            panic!("Could not downcast");
        }
    }

    pub fn as_vec(&self) -> Vec<u8> {
        match self {
            Self::Bool(b) => (*b as u8).to_ne_bytes().into(),
            Self::U8(x) => x.to_ne_bytes().into(),
            Self::U16(x) => x.to_ne_bytes().into(),
            Self::U32(x) => x.to_ne_bytes().into(),
            Self::U64(x) => x.to_ne_bytes().into(),
            Self::I8(x) => x.to_ne_bytes().into(),
            Self::I16(x) => x.to_ne_bytes().into(),
            Self::I32(x) => x.to_ne_bytes().into(),
            Self::I64(x) => x.to_ne_bytes().into(),
            Self::F32(x) => x.to_ne_bytes().into(),
            Self::F64(x) => x.to_ne_bytes().into(),
            Self::Asn(x) => x.into_u32().to_ne_bytes().into(),
            Self::Pointer(x) => x.to_ne_bytes().into(),
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
            IrType::F32 => {
                let val: &[u8; 4] = val.try_into().unwrap();
                Self::F32(f32::from_ne_bytes(*val))
            }
            IrType::F64 => {
                let val: &[u8; 8] = val.try_into().unwrap();
                Self::F64(f64::from_ne_bytes(*val))
            }
            IrType::Asn => {
                let val: &[u8; 4] = val.try_into().unwrap();
                Self::Asn(Asn::from_u32(u32::from_ne_bytes(*val)))
            }
            IrType::Pointer => {
                const SIZE: usize = (usize::BITS / 8) as usize;
                let val: &[u8; SIZE] = val.try_into().unwrap();
                Self::Pointer(usize::from_ne_bytes(*val))
            }
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            IrValue::Bool(x) => *x,
            _ => panic!("Invalid value!"),
        }
    }

    pub fn as_u64(&self) -> u64 {
        match self {
            IrValue::U8(x) => *x as u64,
            IrValue::U16(x) => *x as u64,
            IrValue::U32(x) => *x as u64,
            IrValue::U64(x) => *x,
            _ => panic!("Invalid value!"),
        }
    }

    pub fn as_i64(&self) -> i64 {
        match self {
            IrValue::I8(x) => *x as i64,
            IrValue::I16(x) => *x as i64,
            IrValue::I32(x) => *x as i64,
            IrValue::I64(x) => *x,
            _ => panic!("Invalid value!"),
        }
    }

    pub fn as_f64(&self) -> f64 {
        match self {
            IrValue::F32(x) => *x as f64,
            IrValue::F64(x) => *x,
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
            Bool(x) => write!(f, "bool({x})"),
            U8(x) => write!(f, "u8({x})"),
            U16(x) => write!(f, "u16({x})"),
            U32(x) => write!(f, "u32({x})"),
            U64(x) => write!(f, "u64({x})"),
            I8(x) => write!(f, "i8({x})"),
            I16(x) => write!(f, "i16({x})"),
            I32(x) => write!(f, "i32({x})"),
            I64(x) => write!(f, "i64({x})"),
            F32(x) => write!(f, "f32({x})"),
            F64(x) => write!(f, "f64({x})"),
            Asn(x) => write!(f, "Asn({x})"),
            Pointer(x) => write!(f, "Pointer({x})"),
        }
    }
}
