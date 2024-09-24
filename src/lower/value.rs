use std::any::Any;
use std::fmt::{Debug, Display};

use inetnum::asn::Asn;

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
    Asn(Asn),
    IpAddr(std::net::IpAddr),
    Pointer(usize),
    ExtPointer(*mut ()),
    ExtValue(Vec<u8>),
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
    Asn,
    IpAddr,
    Pointer,
    ExtPointer,
    ExtValue,
}

impl IrType {
    /// The size of the type in bytes
    pub fn bytes(&self) -> usize {
        use IrType::*;
        match self {
            Bool | U8 | I8 => 1,
            U16 | I16 => 2,
            U32 | I32 | Asn => 4,
            U64 | I64 => 8,
            IpAddr => 4,
            Pointer | ExtValue | ExtPointer => (usize::BITS / 8) as usize,
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
            Asn => "Asn",
            IpAddr => "IpAddr",
            Pointer => "Pointer",
            ExtValue => "ExtValue",
            ExtPointer => "ExtPointer",
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
            (ExtValue(_), ExtValue(_)) => false,
            (ExtPointer(_), ExtPointer(_)) => false,
            (Pointer(_), Pointer(_)) => panic!("can't compare pointers"),
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
            Asn(_) => IrType::Asn,
            IpAddr(_) => IrType::I32,
            Pointer(_) => IrType::Pointer,
            ExtValue(_) => IrType::ExtValue,
            ExtPointer(_) => IrType::ExtPointer,
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
            Self::Asn(x) => x,
            Self::IpAddr(x) => x,
            Self::Pointer(x) => x,
            Self::ExtValue(x) => x,
            Self::ExtPointer(x) => x,
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
            Self::Asn(x) => x.into_u32().to_ne_bytes().into(),
            Self::IpAddr(_) => todo!(),
            Self::Pointer(x) => x.to_ne_bytes().into(),
            Self::ExtValue(x) => x.clone(),
            Self::ExtPointer(x) => {
                (x as *const _ as usize).to_ne_bytes().into()
            }
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
            IrType::Asn => {
                let val: &[u8; 4] = val.try_into().unwrap();
                Self::Asn(Asn::from_u32(u32::from_ne_bytes(*val)))
            }
            IrType::IpAddr => {
                let val: &[u8; 32] = val.try_into().unwrap();
                if val[0] == 0 {
                    let addr: [u8; 4] = val[1..5].try_into().unwrap();
                    Self::IpAddr(std::net::IpAddr::from(addr))
                } else {
                    let addr: [u8; 16] = val[1..17].try_into().unwrap();
                    Self::IpAddr(std::net::IpAddr::from(addr))
                }
            }
            IrType::Pointer => {
                const SIZE: usize = (usize::BITS / 8) as usize;
                let val: &[u8; SIZE] = val.try_into().unwrap();
                Self::Pointer(usize::from_ne_bytes(*val))
            }
            IrType::ExtPointer => {
                const SIZE: usize = (usize::BITS / 8) as usize;
                let val: &[u8; SIZE] = val.try_into().unwrap();
                let val = usize::from_ne_bytes(*val);
                Self::ExtPointer(val as *mut _)
            }
            IrType::ExtValue => Self::ExtValue(val.into()),
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
            Asn(x) => write!(f, "Asn({x})"),
            IpAddr(x) => write!(f, "IpAddr({x})"),
            Pointer(x) => write!(f, "Pointer({x})"),
            ExtValue(..) => write!(f, "ExtValue(..)"),
            ExtPointer(..) => write!(f, "ExtPointer(..)"),
        }
    }
}

pub struct ReturnValue(pub Option<IrValue>);

impl From<()> for ReturnValue {
    fn from((): ()) -> Self {
        Self(None)
    }
}

impl From<bool> for IrValue {
    fn from(value: bool) -> Self {
        IrValue::Bool(value)
    }
}

impl From<bool> for ReturnValue {
    fn from(value: bool) -> Self {
        Self(Some(value.into()))
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

impl From<u8> for ReturnValue {
    fn from(value: u8) -> Self {
        Self(Some(value.into()))
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

impl From<u32> for ReturnValue {
    fn from(value: u32) -> Self {
        Self(Some(value.into()))
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

impl<T> From<*const T> for IrValue {
    fn from(value: *const T) -> Self {
        IrValue::ExtPointer(value as *mut ())
    }
}

impl<T> TryFrom<&IrValue> for *const T {
    type Error = ();

    fn try_from(value: &IrValue) -> Result<Self, Self::Error> {
        match value {
            IrValue::ExtPointer(x) => Ok(*x as *const T),
            _ => Err(()),
        }
    }
}

impl<T> From<*mut T> for IrValue {
    fn from(value: *mut T) -> Self {
        IrValue::ExtPointer(value as *mut ())
    }
}

impl<T> TryFrom<&IrValue> for *mut T {
    type Error = ();

    fn try_from(value: &IrValue) -> Result<Self, Self::Error> {
        match value {
            IrValue::ExtPointer(x) => Ok(*x as *mut T),
            _ => Err(()),
        }
    }
}

impl<T> TryFrom<IrValue> for *mut T {
    type Error = ();

    fn try_from(value: IrValue) -> Result<Self, Self::Error> {
        match value {
            IrValue::ExtPointer(x) => Ok(x as *mut T),
            _ => Err(()),
        }
    }
}
