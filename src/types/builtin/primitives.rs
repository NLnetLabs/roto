use std::fmt::{Display, Formatter};
use std::net::IpAddr;

use log::{debug, error, trace};
use paste::paste;
use serde::Serialize;

use crate::ast::{IpAddressLiteral, PrefixLiteral};
// use crate::attr_change_set::{ScalarValue};
use crate::compiler::compile::CompileError;
use crate::traits::RotoType;
use crate::types::collections::ElementTypeValue;
use crate::types::enum_types::EnumVariant;
use crate::types::typedef::MethodProps;
use crate::vm::{FieldIndex, StackValue, VmError};
use crate::{
    createtoken, first_into_vm_err, intotype, _intotype, minimalscalartype,
    noconversioninto, setmethodonly, typevaluefromimpls,
    wrappedfromimpls, scalartype,
};
use crate::types::typedef::TypeDef::ConstEnumVariant;

use super::super::typedef::TypeDef;
use super::super::typevalue::TypeValue;
use super::builtin_type_value::BuiltinTypeValue;

use routecore::asn::{Asn, LongSegmentError};
use routecore::bgp::aspath::OwnedHop;
use routecore::bgp::message::nlri::PathId;
use routecore::bgp::path_attributes::AggregatorInfo;
use routecore::bgp::communities::{ExtendedCommunity, HumanReadableCommunity as Community};
use routecore::addr::Prefix;
use routecore::bgp::types::{AfiSafi, AtomicAggregate, LocalPref, MultiExitDisc, NextHop, Origin, OriginType};

//------------ U16 Type -----------------------------------------------------

createtoken!(U16; Set = 0);

impl RotoType for u16 {
    setmethodonly!(U16);

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::U16 => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::U16(self)))
            }
            TypeDef::U32 => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::U32(self as u32)))
            }
            TypeDef::Asn => Ok(TypeValue::Builtin(BuiltinTypeValue::Asn(
                routecore::asn::Asn::from(self as u32),
            ))),
            TypeDef::PrefixLength => match self {
                0..=128 => Ok(TypeValue::Builtin(
                    BuiltinTypeValue::PrefixLength(PrefixLength(self as u8)),
                )),
                _ => Err(format!(
                    "Cannot convert an instance of type U16 with \
                    a value greater than 128 into type {:?}",
                    type_def
                )
                .into()),
            },
            TypeDef::U8 => match self {
                0..=255 => {
                    Ok(TypeValue::Builtin(BuiltinTypeValue::U8(self as u8)))
                }
                _ => Err(format!(
                    "Cannot convert an instance of type U16 \
                    with a value greater than 128 into type {:?}",
                    type_def
                )
                .into()),
            },
            _ => Err(format!(
                "Cannot convert type U16 into type {:?}",
                type_def
            )
            .into()),
        }
    }
}

typevaluefromimpls!(u16);

// ----------- U32 Type ------------------------------------------------------

createtoken!(U32; Set = 0);

impl RotoType for u32 {
    setmethodonly!(U32);

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::U32 => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::U32(self)))
            }
            TypeDef::Asn => Ok(TypeValue::Builtin(BuiltinTypeValue::Asn(
                routecore::asn::Asn::from(self),
            ))),
            TypeDef::PrefixLength => match self {
                0..=128 => Ok(TypeValue::Builtin(
                    BuiltinTypeValue::PrefixLength(PrefixLength(self as u8)),
                )),
                _ => Err(format!(
                    "Cannot convert an instance of type U32 with \
                    a value greater than 128 into type {:?}",
                    type_def
                )
                .into()),
            },
            TypeDef::U8 => match self {
                0..=255 => {
                    Ok(TypeValue::Builtin(BuiltinTypeValue::U8(self as u8)))
                }
                _ => Err(format!(
                    "Cannot convert an instance of type U32 \
                    with a value greater than 128 into type {:?}",
                    type_def
                )
                .into()),
            },
            _ => Err(format!(
                "Cannot convert type U32 into type {:?}",
                type_def
            )
            .into()),
        }
    }
}

typevaluefromimpls!(u32);

impl TryInto<u32> for &TypeValue {
    type Error = VmError;

    fn try_into(self) -> Result<u32, Self::Error> {
        if let TypeValue::Builtin(BuiltinTypeValue::U32(value)) = self {
            Ok(*value)
        } else {
            Err(VmError::InvalidValueType)
        }
    }
}

// ----------- U8 Type -------------------------------------------------------

createtoken!(U8; Set = 0);

impl RotoType for u8 {
    setmethodonly!(U8);

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            // Self
            TypeDef::U8 => Ok(TypeValue::Builtin(BuiltinTypeValue::U8(self))),
            TypeDef::U16 => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::U16(self as u16)))
            }
            TypeDef::U32 => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::U32(self as u32)))
            }
            TypeDef::StringLiteral => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::StringLiteral(
                    self.into()
                )))
            }
            TypeDef::PrefixLength => match self {
                0..=128 => Ok(TypeValue::Builtin(
                    BuiltinTypeValue::PrefixLength(PrefixLength(self)),
                )),
                _ => Err(format!(
                    "Prefix length must be between 0 and 128, not {}",
                    self
                )
                .into()),
            },
            _ => {
                Err(format!("Cannot convert type U8 to type {:?}", type_def)
                    .into())
            }
        }
    }
}

typevaluefromimpls!(u8);

// ----------- Boolean Type --------------------------------------------------

createtoken!(bool; Set = 0);

impl RotoType for bool {
    setmethodonly!(bool);

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::Bool => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::Bool(self)))
            }
            TypeDef::StringLiteral => match self {
                true => Ok(true.into()),
                false => Ok(false.into()),
            },
            _ => Err(format!(
                "Cannot convert type Boolean to type {:?}",
                type_def
            )
            .into()),
        }
    }
}

typevaluefromimpls!(bool);

//------------ StringLiteral type -------------------------------------------

#[derive(Debug, Eq, PartialEq, Clone, Hash, Ord, PartialOrd, Serialize)]
pub struct StringLiteral(pub(crate) String);
impl StringLiteral {
    pub fn new(val: String) -> Self {
        StringLiteral(val)
    }
}

impl RotoType for StringLiteral {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError> {
        match method_name.ident.as_str() {
            "cmp" => Ok(MethodProps::new(
                TypeDef::Bool,
                StringLiteralToken::Cmp.into(),
                vec![TypeDef::StringLiteral, TypeDef::StringLiteral],
            )),
            "format" => Ok(MethodProps::new(
                TypeDef::StringLiteral,
                StringLiteralToken::Format.into(),
                vec![TypeDef::StringLiteral, TypeDef::StringLiteral],
            )),
            "set" => Ok(MethodProps::new(
                TypeDef::Unknown,
                StringLiteralToken::Set.into(),
                vec![TypeDef::StringLiteral],
            )
            .consume_value()),
            _ => Err(format!(
                "Unknown method: '{}' for type StringLiteral",
                method_name.ident
            )
            .into()),
        }
    }

    fn exec_value_method(
        &self,
        method_token: usize,
        args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.try_into()? {
            StringLiteralToken::Cmp => {
                if let TypeValue::Builtin(BuiltinTypeValue::StringLiteral(
                    sv,
                )) =
                    args.first().ok_or(VmError::InvalidMethodCall)?.as_ref()
                {
                    Ok(self.cmp(sv).is_eq().into())
                } else {
                    Err(VmError::InvalidMethodCall)
                }
            }
            StringLiteralToken::Set => Err(VmError::InvalidMethodCall),
            StringLiteralToken::Format => Err(VmError::InvalidMethodCall),
        }
    }

    fn exec_consume_value_method(
        self,
        method_token: usize,
        mut args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.try_into()? {
            StringLiteralToken::Set => {
                if let Ok(TypeValue::Builtin(
                    BuiltinTypeValue::StringLiteral(str),
                )) = args.swap_remove(0).into_type(&TypeDef::StringLiteral)
                {
                    Ok(TypeValue::Builtin(BuiltinTypeValue::StringLiteral(
                        str,
                    )))
                } else {
                    Err(VmError::InvalidValueType)
                }
            }
            StringLiteralToken::Cmp => Err(VmError::InvalidMethodCall),
            StringLiteralToken::Format => Err(VmError::InvalidMethodCall),
        }
    }

    fn exec_type_method<'a>(
        method_token: usize,
        args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.try_into()? {
            StringLiteralToken::Format => {
                trace!("string arguments {:?}", args);

                let format_str = if let TypeValue::Builtin(
                    BuiltinTypeValue::StringLiteral(StringLiteral(str)),
                ) =
                    args.first().ok_or(VmError::InvalidMethodCall)?.as_ref()
                {
                    str
                } else {
                    return Err(VmError::AnonymousArgumentNotFound);
                };

                let mut sub_str = format_str.splitn(2, "{}");

                let new_string = String::from_iter([
                    sub_str.next().ok_or(VmError::InvalidMethodCall)?,
                    &args
                        .get(1)
                        .ok_or(VmError::InvalidMethodCall)?
                        .as_ref()
                        .to_string(),
                    if let Some(s) = sub_str.next() { s } else { "" },
                ]);

                Ok(TypeValue::Builtin(BuiltinTypeValue::StringLiteral(
                    StringLiteral(new_string),
                )))
            }
            StringLiteralToken::Cmp => {
                if let TypeValue::Builtin(BuiltinTypeValue::StringLiteral(
                    sv_1,
                )) =
                    args.first().ok_or(VmError::InvalidMethodCall)?.as_ref()
                {
                    if let TypeValue::Builtin(
                        BuiltinTypeValue::StringLiteral(sv_2),
                    ) = args
                        .get(1)
                        .ok_or(VmError::InvalidMethodCall)?
                        .as_ref()
                    {
                        Ok(sv_1.cmp(sv_2).is_eq().into())
                    } else {
                        Err(VmError::InvalidMethodCall)
                    }
                } else {
                    Err(VmError::InvalidMethodCall)
                }
            }
            StringLiteralToken::Set => Err(VmError::InvalidMethodCall),
        }
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::StringLiteral => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::StringLiteral(self)))
            }
            TypeDef::Bool => match self.0.as_str() {
                "true" => {
                    Ok(TypeValue::Builtin(BuiltinTypeValue::Bool(true)))
                }
                "false" => {
                    Ok(TypeValue::Builtin(BuiltinTypeValue::Bool(false)))
                }
                _ => Err(format!(
                    "String {} cannot be converted into Boolean",
                    self.0
                )
                .into()),
            },
            _ => Err(format!(
                "Cannot convert type StringLiteral to type {:?}",
                type_def
            )
            .into()),
        }
    }
}

impl From<StringLiteral> for TypeValue {
    fn from(val: StringLiteral) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::StringLiteral(val))
    }
}

#[derive(Debug)]
pub enum StringLiteralToken {
    Cmp = 0,
    Format = 1,
    Set = 2,
}

// impl ScalarValue for StringLiteral {}

impl TryFrom<usize> for StringLiteralToken {
    type Error = VmError;
    fn try_from(val: usize) -> Result<Self, VmError> {
        match val {
            0 => Ok(StringLiteralToken::Cmp),
            1 => Ok(StringLiteralToken::Format),
            2 => Ok(StringLiteralToken::Set),
            _ => {
                debug!("Unknown token value: {}", val);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<StringLiteral> for BuiltinTypeValue {
    fn from(value: StringLiteral) -> Self {
        BuiltinTypeValue::StringLiteral(value)
    }
}

impl From<StringLiteralToken> for usize {
    fn from(val: StringLiteralToken) -> Self {
        val as usize
    }
}

impl<T: Display> From<T> for StringLiteral {
    fn from(value: T) -> Self {
        StringLiteral(format!("{}", value))
    }
}

//------------ IntegerLiteral type ------------------------------------------

#[derive(
    Debug, Eq, Ord, PartialEq, PartialOrd, Copy, Clone, Hash, Serialize,
)]
pub struct IntegerLiteral(pub(crate) i64);
impl IntegerLiteral {
    pub fn new(val: i64) -> Self {
        IntegerLiteral(val)
    }
}

impl RotoType for IntegerLiteral {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError> {
        match method_name.ident.as_str() {
            "cmp" => Ok(MethodProps::new(
                TypeDef::IntegerLiteral,
                IntegerLiteralToken::Cmp.into(),
                vec![TypeDef::IntegerLiteral, TypeDef::IntegerLiteral],
            )),
            _ => Err(format!(
                "Unknown method: '{}' for type Integer",
                method_name.ident
            )
            .into()),
        }
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::IntegerLiteral => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(self)))
            }
            TypeDef::StringLiteral => Ok(TypeValue::Builtin(
                BuiltinTypeValue::StringLiteral(self.into()),
            )),
            TypeDef::PrefixLength => match self.0 {
                0..=128 => {
                    Ok(TypeValue::Builtin(BuiltinTypeValue::PrefixLength(
                        PrefixLength(self.0 as u8),
                    )))
                }
                _ => Err(format!(
                    "Prefix length must be between 0 and 128, not {}",
                    self.0
                )
                .into()),
            },
            TypeDef::Asn => match self.0 {
                0..=4294967295 => {
                    Ok(TypeValue::Builtin(BuiltinTypeValue::Asn(
                        routecore::asn::Asn::from(self.0 as u32),
                    )))
                }
                i if i < 0 => Err(CompileError::from(
                    "Cannot convert type \
                IntegerLiteral < 0 into Asn"
                        .to_string(),
                )),
                _ => Err(CompileError::from(
                    "Cannot convert type \
                IntegerLiteral > 4294967295 into Asn"
                        .to_string(),
                )),
            },
            TypeDef::U32 => u32::try_from(self.0)
                .map(|v| TypeValue::Builtin(BuiltinTypeValue::U32(v)))
                .map_err(|_| {
                    CompileError::from(format!(
                        "Cannot convert instanc of type IntegerLiteral with \
                        value {} into U32",
                        self.0
                    ))
                }),
            TypeDef::U8 => u8::try_from(self.0)
                .map(|v| TypeValue::Builtin(BuiltinTypeValue::U8(v)))
                .map_err(|_| {
                    CompileError::from(format!(
                        "Cannot convert instance of type IntegerLiteral with \
                        value {} into U8",
                        self.0
                    ))
                }),
            TypeDef::U16 => u16::try_from(self.0)
                .map(|v| TypeValue::Builtin(BuiltinTypeValue::U16(v)))
                .map_err(|_| {
                    CompileError::from(format!(
                        "Cannot convert instance of type IntegerLiteral with \
                        value {} into U16",
                        self.0
                    ))
                }),
            TypeDef::ConstEnumVariant(e_num) => match self.0 {
                0..=255 => Ok(TypeValue::Builtin(
                    BuiltinTypeValue::ConstU8EnumVariant(EnumVariant {
                        enum_name: e_num.clone(),
                        value: u8::try_from(self.0).map_err(|_| {
                            CompileError::from(format!(
                                "Cannot convert type IntegerLiteral with \
                            value '{}' into Enum Variant with name '{}'",
                                self.0, e_num
                            ))
                        })?,
                    }),
                )),
                _ => Err(CompileError::from(format!(
                    "Cannot convert type \
                IntegerLiteral > 255 into ConstU8Variant of type {}",
                    e_num
                ))),
            },
            _ => Err(format!(
                "Cannot convert type IntegerLiteral to type {:?}",
                type_def
            )
            .into()),
        }
    }

    fn exec_value_method(
        &self,
        method_token: usize,
        args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.try_into()? {
            IntegerLiteralToken::Cmp if args.len() == 2 => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::Bool(
                    args[0] == args[1],
                )))
            }
            _ => Err(VmError::InvalidMethodCall),
        }
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        // There is no 'set' method for an IntegerLiteral, this type should
        // not be assigned to a Record or List (they should use the concrete
        // type, e.g. U8).
        Err(VmError::InvalidMethodCall)
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidMethodCall)
    }
}

impl From<IntegerLiteral> for TypeValue {
    fn from(val: IntegerLiteral) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(val))
    }
}

impl From<IntegerLiteral> for BuiltinTypeValue {
    fn from(value: IntegerLiteral) -> Self {
        BuiltinTypeValue::IntegerLiteral(value)
    }
}

impl From<i64> for BuiltinTypeValue {
    fn from(val: i64) -> Self {
        BuiltinTypeValue::IntegerLiteral(IntegerLiteral(val))
    }
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

// There's no Set, because concrete integer types should be used to set things
// like fields in collections, e.g. U8, etc.
#[derive(Debug)]
pub(crate) enum IntegerLiteralToken {
    Cmp,
}

impl TryFrom<usize> for IntegerLiteralToken {
    type Error = VmError;
    fn try_from(val: usize) -> Result<Self, VmError> {
        match val {
            0 => Ok(IntegerLiteralToken::Cmp),
            _ => {
                debug!("Unknown token value: {}", val);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<IntegerLiteralToken> for usize {
    fn from(val: IntegerLiteralToken) -> Self {
        val as usize
    }
}

//------------ HexLiteral type ----------------------------------------------

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash, Serialize)]
pub struct HexLiteral(pub(crate) u64);
impl HexLiteral {
    pub fn new(val: u64) -> Self {
        HexLiteral(val)
    }
}

impl RotoType for HexLiteral {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError> {
        match method_name.ident.as_str() {
            "cmp" => Ok(MethodProps::new(
                TypeDef::IntegerLiteral,
                HexLiteralToken::Cmp.into(),
                vec![TypeDef::HexLiteral, TypeDef::HexLiteral],
            )),
            _ => Err(format!(
                "Unknown method: '{}' for type Prefix",
                method_name.ident
            )
            .into()),
        }
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::HexLiteral => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::HexLiteral(self)))
            }
            TypeDef::Community => {
                // Convert to either a StandardCommunity (if the hex literal
                // fits into a u32), or..
                match self.0 {
                    v if v <= <u32>::MAX.into() => {
                        Ok(TypeValue::Builtin(BuiltinTypeValue::Community(
                            routecore::bgp::communities::StandardCommunity::from(v as u32).into(),
                        )))
                    }
                    // Convert to an ExtendedCommunity
                    v => Ok(TypeValue::Builtin(BuiltinTypeValue::Community(
                        routecore::bgp::communities::ExtendedCommunity::from(v.to_be_bytes()).into()),
                    )),
                }
            }
            TypeDef::U8 => u8::try_from(self.0)
                .map(|v| TypeValue::Builtin(BuiltinTypeValue::U8(v)))
                .map_err(|_| {
                    CompileError::from(format!(
                        "Cannot convert type IntegerLiteral with {} into U8",
                        self.0
                    ))
                }),
            TypeDef::U32 => u32::try_from(self.0)
                .map(|v| TypeValue::Builtin(BuiltinTypeValue::U32(v)))
                .map_err(|_| {
                    CompileError::from(format!(
                        "Cannot convert type IntegerLiteral with {} into U8",
                        self.0
                    ))
                }),
            _ => Err(format!(
                "Cannot convert type HexLiteral to type {:?}",
                type_def
            )
            .into()),
        }
    }

    fn exec_value_method(
        &self,
        _method_token: usize,

        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

impl From<HexLiteral> for TypeValue {
    fn from(val: HexLiteral) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::HexLiteral(val))
    }
}

impl From<HexLiteral> for BuiltinTypeValue {
    fn from(value: HexLiteral) -> Self {
        BuiltinTypeValue::HexLiteral(value)
    }
}

impl Display for HexLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub(crate) enum HexLiteralToken {
    Cmp,
}

impl TryFrom<usize> for HexLiteralToken {
    type Error = VmError;

    fn try_from(val: usize) -> Result<Self, VmError> {
        match val {
            0 => Ok(HexLiteralToken::Cmp),
            _ => {
                debug!("Unknown token value: {}", val);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<HexLiteralToken> for usize {
    fn from(val: HexLiteralToken) -> Self {
        val as usize
    }
}


// ----------- Prefix type ---------------------------------------------------

createtoken!(Prefix;
    From = 0
    Exists = 1
    Address = 2
    Len = 3
    Matches = 4
    Covers = 5
    IsCoveredBy = 6
    Contains = 7
);

impl RotoType for routecore::addr::Prefix {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "from" => Ok(MethodProps::new(
                TypeDef::Prefix,
                PrefixToken::From.into(),
                vec![TypeDef::IpAddr, TypeDef::PrefixLength],
            )),
            "address" => Ok(MethodProps::new(
                TypeDef::IpAddr,
                PrefixToken::Address.into(),
                vec![],
            )),
            "len" => Ok(MethodProps::new(
                TypeDef::PrefixLength,
                PrefixToken::Len.into(),
                vec![],
            )),
            "matches" => Ok(MethodProps::new(
                TypeDef::Bool,
                PrefixToken::Matches.into(),
                vec![TypeDef::Prefix],
            )),
            "exists" => Ok(MethodProps::new(
                TypeDef::Bool,
                PrefixToken::Exists.into(),
                vec![],
            )),
            "covers" => Ok(MethodProps::new(
                TypeDef::Bool,
                PrefixToken::Covers.into(),
                vec![TypeDef::Prefix],
            )),
            "is_covered_by" => Ok(MethodProps::new(
                TypeDef::Bool,
                PrefixToken::IsCoveredBy.into(),
                vec![TypeDef::Prefix],
            )),
            "contains" => Ok(MethodProps::new(
                TypeDef::Bool,
                PrefixToken::Contains.into(),
                vec![TypeDef::IpAddr],
            )),
            _ => Err(format!(
                "Unknown method: '{}' for type Prefix",
                method_name.ident
            )
            .into()),
        }
    }

    intotype!(Prefix;StringLiteral);

    fn exec_value_method(
        &self,
        method_token: usize,

        args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.try_into()? {
            PrefixToken::Address => {
                let prefix = self;
                Ok(TypeValue::Builtin(BuiltinTypeValue::IpAddr(
                    prefix.addr(),
                )))
            }
            PrefixToken::Len => {
                let pfx = self;
                Ok(TypeValue::Builtin(BuiltinTypeValue::PrefixLength(
                    PrefixLength(pfx.len()),
                )))
            }
            PrefixToken::From => unimplemented!(),
            PrefixToken::Exists => Ok(true.into()),
            PrefixToken::Matches => todo!(),
            PrefixToken::Covers => {
                if let Some(other_pfx) = args.first() {
                    if let TypeValue::Builtin(BuiltinTypeValue::Prefix(
                        other,
                    )) = other_pfx.as_ref()
                    {
                        Ok(self.covers(*other).into())
                    } else {
                        Err(VmError::InvalidMethodCall)
                    }
                } else {
                    Ok(TypeValue::Unknown)
                }
            }
            PrefixToken::IsCoveredBy => {
                if let Some(other_pfx) = args.first() {
                    if let TypeValue::Builtin(BuiltinTypeValue::Prefix(
                        other,
                    )) = other_pfx.as_ref()
                    {
                        Ok(other.covers(*self).into())
                    } else {
                        Err(VmError::InvalidMethodCall)
                    }
                } else {
                    Ok(TypeValue::Unknown)
                }
            }
            PrefixToken::Contains => {
                if let Some(other_ip) = args.first() {
                    if let TypeValue::Builtin(BuiltinTypeValue::IpAddr(
                        other,
                    )) = other_ip.as_ref()
                    {
                        Ok(self.contains(*other).into())
                    } else {
                        Err(VmError::InvalidMethodCall)
                    }
                } else {
                    Ok(TypeValue::Unknown)
                }
            }
        }
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        method_token: usize,
        args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.try_into()? {
            PrefixToken::From => {
                if let (Some(addr), Some(len)) = (args.first(), args.get(1)) {
                    if let TypeValue::Builtin(BuiltinTypeValue::IpAddr(
                        addr,
                    )) = addr.as_ref()
                    {
                        let len: PrefixLength = len
                            .as_ref()
                            .try_into()
                            .map_err(|_e| VmError::InvalidConversion)?;
                        // let ip = addr;
                        Ok(routecore::addr::Prefix::new(*addr, len.0)
                            .map_or_else(
                                |_| TypeValue::Unknown,
                                |p| p.into(),
                            ))
                    } else {
                        Err(VmError::AnonymousArgumentNotFound)
                    }
                } else {
                    Err(VmError::StackUnderflow)
                }
            }
            PrefixToken::Exists => unimplemented!(),
            PrefixToken::Address => unimplemented!(),
            PrefixToken::Len => unimplemented!(),
            PrefixToken::Matches => unimplemented!(),
            _ => Err(VmError::InvalidMethodCall),
        }
    }
}

typevaluefromimpls!(Prefix);

impl TryFrom<&'_ PrefixLiteral> for routecore::addr::Prefix {
    type Error = CompileError;

    fn try_from(value: &PrefixLiteral) -> Result<Self, Self::Error> {
        <Prefix as std::str::FromStr>::from_str(
            value.0.as_str(),
        )
        .map_err(|e| {
            CompileError::from(format!(
                "Cannot parse '{:?}' as a Prefix: {}",
                value, e
            ))
        })
    }
}

impl TryFrom<&TypeValue> for routecore::addr::Prefix {
    type Error = VmError;

    fn try_from(value: &TypeValue) -> Result<Self, Self::Error> {
        if let TypeValue::Builtin(BuiltinTypeValue::Prefix(
            pfx,
        )) = value
        {
            Ok(*pfx)
        } else {
            Err(VmError::InvalidConversion)
        }
    }
}


//------------ PrefixLength type ---------------------------------------------

createtoken!(PrefixLength; Set = 0);

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash, Serialize)]
pub struct PrefixLength(pub(crate) u8);

impl PrefixLength {
    pub fn new(val: u8) -> Self {
        PrefixLength(val)
    }
}

impl RotoType for PrefixLength {
    setmethodonly!(PrefixLength);
    intotype!(PrefixLength; U8, U16, U32, IntegerLiteral);
}

wrappedfromimpls!(PrefixLength; u32, u16, u8; IntegerLiteral = i64);

impl Display for PrefixLength {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "/{}", self.0)
    }
}

impl TryFrom<&TypeValue> for PrefixLength {
    type Error = CompileError;

    fn try_from(value: &TypeValue) -> Result<Self, Self::Error> {
        match value {
            TypeValue::Builtin(BuiltinTypeValue::PrefixLength(pl)) => Ok(*pl),
            TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(int_lit)) => {
                if let TypeValue::Builtin(BuiltinTypeValue::PrefixLength(
                    PrefixLength(pl),
                )) = int_lit.into_type(&TypeDef::PrefixLength)?
                {
                    Ok(PrefixLength(pl))
                } else {
                    Err(format!(
                        "Cannot convert type {:?} to type PrefixLength",
                        value
                    )
                    .into())
                }
            }
            _ => Err(format!(
                "Cannot convert type {:?} to type PrefixLength",
                value
            )
            .into()),
        }
    }
}


//------------ AfiSafi Type --------------------------------------------------

minimalscalartype!(AfiSafi);


//------------ PathId Type ---------------------------------------------------

minimalscalartype!(PathId);


//------------ Community Type ------------------------------------------------

createtoken!(Community;
    From = 0
    Standard = 1
    Extended = 2
    Large = 3
    As = 4
    Value = 5
    Exists = 6
    Set = 7
);

impl RotoType for Community {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "set" => Ok(MethodProps::new(
                TypeDef::Community,
                CommunityToken::Set.into(),
                vec![TypeDef::Community],
            )),
            "from" => Ok(MethodProps::new(
                TypeDef::Community,
                CommunityToken::From.into(),
                vec![TypeDef::StringLiteral],
            )),
            "standard" => Ok(MethodProps::new(
                TypeDef::Community,
                CommunityToken::Standard.into(),
                vec![TypeDef::U32],
            )),
            "extended" => Ok(MethodProps::new(
                TypeDef::Community,
                CommunityToken::Extended.into(),
                vec![TypeDef::U32],
            )),
            "large" => Ok(MethodProps::new(
                TypeDef::Community,
                CommunityToken::Large.into(),
                vec![TypeDef::U32],
            )),
            "as" => Ok(MethodProps::new(
                TypeDef::U32,
                CommunityToken::As.into(),
                vec![],
            )),
            "value" => Ok(MethodProps::new(
                TypeDef::U32,
                CommunityToken::Value.into(),
                vec![],
            )),
            "exists" => Ok(MethodProps::new(
                TypeDef::Bool,
                CommunityToken::Exists.into(),
                vec![],
            )),
            _ => Err(format!(
                "Unknown method: '{}' for type Community",
                method_name.ident
            )
            .into()),
        }
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::Community => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::Community(self)))
            }
            TypeDef::StringLiteral => Ok(TypeValue::Builtin(
                BuiltinTypeValue::StringLiteral(self.into()),
            )),
            _ => Err(format!(
                "Cannot convert type Community to type {:?}",
                type_def
            )
            .into()),
        }
    }

    fn exec_value_method(
        &self,
        method_token: usize,

        args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.try_into()? {
            CommunityToken::Set => {
                if let TypeValue::Builtin(BuiltinTypeValue::Community(comm)) =
                    args.first().ok_or(VmError::InvalidMethodCall)?.as_ref()
                {
                    Ok(TypeValue::from(*comm))
                } else {
                    Err(VmError::InvalidMethodCall)
                }
            }
            CommunityToken::From => todo!(),
            CommunityToken::Standard => todo!(),
            CommunityToken::Extended => todo!(),
            CommunityToken::Large => todo!(),
            CommunityToken::As => todo!(),
            CommunityToken::Value => todo!(),
            CommunityToken::Exists => todo!(),
        }
    }

    fn exec_consume_value_method(
        self,
        method_token: usize,
        mut args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.try_into()? {
            CommunityToken::Set => {
                if let Ok(TypeValue::Builtin(BuiltinTypeValue::Community(
                    comm_lit,
                ))) = args.remove(0).into_type(&TypeDef::Community)
                {
                    Ok(TypeValue::Builtin(BuiltinTypeValue::Community(
                        comm_lit,
                    )))
                } else {
                    Err(VmError::InvalidValueType)
                }
            }
            CommunityToken::From => todo!(),
            CommunityToken::Standard => todo!(),
            CommunityToken::Extended => todo!(),
            CommunityToken::Large => todo!(),
            CommunityToken::As => todo!(),
            CommunityToken::Value => todo!(),
            CommunityToken::Exists => todo!(),
        }
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

// impl ScalarValue for Community {}

impl From<Community> for TypeValue {
    fn from(val: Community) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Community(val))
    }
}

impl From<Community> for BuiltinTypeValue {
    fn from(value: Community) -> Self {
        BuiltinTypeValue::Community(value)
    }
}

impl From<Vec<Community>> for TypeValue {
    fn from(value: Vec<Community>) -> Self {
        let list: Vec<ElementTypeValue> = value
            .iter()
            .map(|c| ElementTypeValue::Primitive(TypeValue::from(*c)))
            .collect::<Vec<_>>();
        TypeValue::List(crate::types::collections::List(list))
    }
}

//------------ ExtendedCommunity ---------------------------------------------

// createtoken!(ExtendedCommunity; Set = 0);

// impl RotoType for ExtendedCommunity {
//     setmethodonly!(ExtendedCommunity);

//     fn into_type(
//         self,
//         type_def: &TypeDef,
//     ) -> Result<TypeValue, CompileError> {
//         match type_def {
//             TypeDef::ExtendedCommunity => {
//                 Ok(TypeValue::Builtin(BuiltinTypeValue::ExtendedCommunity(self)))
//             }
//             TypeDef::Community => Ok(
//                 TypeValue::Builtin(BuiltinTypeValue::Community(self))
//             ),
//             _ => Err(format!(
//                 "Cannot convert type Boolean to type {:?}",
//                 type_def
//             )
//             .into()),
//         }
//     }
// }

// typevaluefromimpls!(ExtendedCommunity);

//------------ Nlri ----------------------------------------------------------

pub type Nlri = routecore::bgp::message::nlri::Nlri<bytes::Bytes>;

createtoken!(
    Nlri;
    Set = 0
    Afi = 1
    Safi = 2
);

impl RotoType for Nlri {

    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "set" => Ok(MethodProps::new(
                TypeDef::Nlri,
                NlriToken::Set.into(),
                vec![TypeDef::Nlri],
            )),
            "afi" => Ok(MethodProps::new(
                ConstEnumVariant("AFI".into()),
                NlriToken::Afi.into(),
                vec![]
            )),
            "safi" => Ok(MethodProps::new(
                ConstEnumVariant("SAFI".into()),
                NlriToken::Safi.into(),
                vec![]
            )),
            _ => Err(format!(
                "Unknown method: '{}' for type Nlri",
                method_name.ident
            )
            .into()),
        }
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::Nlri => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::Nlri(self)))
            }
            TypeDef::StringLiteral => Ok(TypeValue::Builtin(
                BuiltinTypeValue::StringLiteral(self.into()),
            )),
            _ => Err(format!(
                "Cannot convert type Nlri to type {:?}",
                type_def
            )
            .into()),
        }
    }

    fn exec_value_method(
        &self,
        method_token: usize,

        args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.try_into()? {
            NlriToken::Set => {
                if let TypeValue::Builtin(BuiltinTypeValue::Nlri(nlri)) =
                    args.first().ok_or(VmError::InvalidMethodCall)?.as_ref()
                {
                    Ok(TypeValue::from(nlri.clone()))
                } else {
                    Err(VmError::InvalidMethodCall)
                }
            }
            NlriToken::Afi => {
                Ok(TypeValue::Builtin(
                    BuiltinTypeValue::ConstU16EnumVariant(
                        EnumVariant::<u16>::new(
                            ("AFI".into(), self.afi_safi().afi().into())
                        )
                    )
                ))
            }
            NlriToken::Safi => {
                Ok(TypeValue::Builtin(
                    BuiltinTypeValue::ConstU8EnumVariant(
                        EnumVariant::<u8>::new(
                            ("SAFI".into(), self.afi_safi().safi().into())
                        )
                    )
                ))
            }
        }
    }

    fn exec_consume_value_method(
        self,
        method_token: usize,
        mut args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.try_into()? {
            NlriToken::Set => {
                if let Ok(TypeValue::Builtin(BuiltinTypeValue::Nlri(
                    comm_lit,
                ))) = args.remove(0).into_type(&TypeDef::Nlri)
                {
                    Ok(TypeValue::Builtin(BuiltinTypeValue::Nlri(
                        comm_lit,
                    )))
                } else {
                    Err(VmError::InvalidValueType)
                }
            },
            NlriToken::Afi => Err(VmError::InvalidMethodCall),
            NlriToken::Safi => Err(VmError::InvalidMethodCall)
        }
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

// impl ScalarValue for Nlri {}

impl From<Nlri> for TypeValue {
    fn from(val: Nlri) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Nlri(val))
    }
}

impl From<Nlri> for BuiltinTypeValue {
    fn from(value: Nlri) -> Self {
        BuiltinTypeValue::Nlri(value)
    }
}

impl From<Vec<Nlri>> for TypeValue {
    fn from(value: Vec<Nlri>) -> Self {
        let list: Vec<ElementTypeValue> = value
            .iter()
            .map(|c| ElementTypeValue::Primitive(TypeValue::from(c.clone())))
            .collect::<Vec<_>>();
        TypeValue::List(crate::types::collections::List(list))
    }
}

//------------ MatchType ----------------------------------------------------

#[derive(Debug, Eq, PartialEq)]
pub enum MatchType {
    ExactMatch,
    LongestMatch,
    EmptyMatch,
}

// ----------- IpAddr type ---------------------------------------------------

createtoken!(
    IpAddr;
    From = 0
    Matches = 1
);

impl RotoType for IpAddr {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "from" => Ok(MethodProps::new(
                TypeDef::IpAddr,
                IpAddrToken::From.into(),
                vec![TypeDef::StringLiteral],
            )),
            "matches" => Ok(MethodProps::new(
                TypeDef::Bool,
                IpAddrToken::Matches.into(),
                vec![TypeDef::Prefix],
            )),
            _ => Err(format!(
                "Unknown method: '{}' for type IpAddress",
                method_name.ident
            )
            .into()),
        }
    }

    intotype!(IpAddr;StringLiteral);

    fn exec_value_method(
        &self,
        method_token: usize,

        args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.try_into()? {
            IpAddrToken::From => {
                if let TypeValue::Builtin(BuiltinTypeValue::StringLiteral(str_lit)) =
                    args.first().ok_or(VmError::InvalidMethodCall)?.as_ref()
                {
                   str_lit.clone().into_type(&TypeDef::IpAddr).map_err(|_| VmError::InvalidConversion)
                } else {
                    Err(VmError::InvalidCommandArg)
                }
            },
            IpAddrToken::Matches => {
                if let TypeValue::Builtin(BuiltinTypeValue::Prefix(pfx)) =
                    args.first().ok_or(VmError::InvalidMethodCall)?.as_ref()
                {
                   Ok(TypeValue::Builtin(
                        BuiltinTypeValue::Bool(pfx.contains(*self))
                        )
                    )
                } else {
                    Err(VmError::InvalidCommandArg)
                }
            }
        }
    }

    fn exec_consume_value_method(
        self,
        method_token: usize,
        mut args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.try_into()? {
            IpAddrToken::From if args.len() == 1 => {
                if let TypeValue::Builtin(BuiltinTypeValue::StringLiteral(str_lit)) =
                    args.remove(0)
                {
                   str_lit.into_type(&TypeDef::IpAddr).map_err(|_| VmError::InvalidConversion)
                } else {
                    Err(VmError::InvalidCommandArg)
                }
            },
            IpAddrToken::Matches => {
                if let TypeValue::Builtin(BuiltinTypeValue::Prefix(pfx)) =
                    args.first().ok_or(VmError::InvalidMethodCall)?
                {
                   Ok(TypeValue::Builtin(
                        BuiltinTypeValue::Bool(pfx.contains(self))
                        )
                    )
                } else {
                    Err(VmError::InvalidCommandArg)
                }
            },
            _ => {
                Err(VmError::InvalidMethodCall)
            }
        }
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

typevaluefromimpls!(IpAddr);

impl TryFrom<&'_ IpAddressLiteral> for IpAddr {
    type Error = CompileError;

    fn try_from(value: &IpAddressLiteral) -> Result<Self, Self::Error> {
        <std::net::IpAddr as std::str::FromStr>::from_str(
                value.0.as_str(),
            )
            .map_err(|e| {
                CompileError::from(format!(
                    "Cannot parse '{:?}' as an IP Address: {}",
                    value, e
                ))
            })
    }
}


// ----------- Asn type -----------------------------------------------------

createtoken!(Asn; Set = 0);

impl RotoType for routecore::asn::Asn {
    setmethodonly!(Asn);

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::Asn => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::Asn(self)))
            }
            TypeDef::StringLiteral => Ok(TypeValue::Builtin(
                BuiltinTypeValue::StringLiteral(self.into()),
            )),
            TypeDef::U8 => match self.into_u32() {
                0..=255 => {
                    // this should work, shouldn't it?
                    Ok(TypeValue::Builtin(BuiltinTypeValue::U8(
                        self.into_u32() as u8,
                    )))
                }
                val => Err(format!(
                    "Cannot convert an instance of type \
                    Asn with a value {} into type U8. It's greater than 255",
                    val
                )
                .into()),
            },
            TypeDef::U32 => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::U32(self.into_u32())))
            }
            _ => {
                Err(format!("Cannot convert type Asn to type {:?}", type_def)
                    .into())
            }
        }
    }
}

typevaluefromimpls!(Asn);


// ----------- AsPath type --------------------------------------------------

type RoutecoreHop = routecore::bgp::aspath::Hop<Vec<u8>>;

impl RotoType for routecore::bgp::aspath::HopPath {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "origin" => Ok(MethodProps::new(
                TypeDef::Asn,
                AsPathToken::Origin.into(),
                vec![],
            )),
            "contains" => Ok(MethodProps::new(
                TypeDef::Bool,
                AsPathToken::Contains.into(),
                vec![TypeDef::Asn],
            )),
            "len" => Ok(MethodProps::new(
                TypeDef::U8,
                AsPathToken::Len.into(),
                vec![],
            )),
            "set" => Ok(MethodProps::new(
                TypeDef::Unknown,
                AsPathToken::Set.into(),
                vec![TypeDef::AsPath],
            )),
            _ => Err(format!(
                "Unknown method '{}' for type AsPath",
                method_name.ident
            )
            .into()),
        }
    }

    noconversioninto!(AsPath);

    fn exec_value_method<'a>(
        &'a self,
        method: usize,

        args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method.try_into()? {
            AsPathToken::Origin => match self.origin().cloned() {
                Some(origin_asn) => {
                    Ok(TypeValue::Builtin(BuiltinTypeValue::Asn(
                        origin_asn
                            .try_into_asn()
                            .map_err(|_| VmError::InvalidValueType)?,
                    )))
                }
                None => Err(VmError::InvalidPayload),
            },
            AsPathToken::Contains => {
                if let TypeValue::Builtin(BuiltinTypeValue::Asn(search_asn)) =
                    first_into_vm_err!(args, InvalidMethodCall)?.as_ref()
                {
                    let contains =
                        self.contains(&RoutecoreHop::from(*search_asn));
                    Ok(TypeValue::Builtin(BuiltinTypeValue::Bool(contains)))
                } else {
                    Ok(TypeValue::Unknown)
                }
            }
            AsPathToken::Len => {
                let len = self.hop_count();
                Ok(TypeValue::Builtin(BuiltinTypeValue::U8(len as u8)))
            }
            AsPathToken::Set => Err(VmError::InvalidMethodCall),
        }
    }

    fn exec_consume_value_method(
        self,
        method_token: usize,
        mut args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.try_into()? {
            AsPathToken::Set => try_remove_first(&mut args),
            AsPathToken::Origin => Err(VmError::InvalidMethodCall),
            AsPathToken::Contains => Err(VmError::InvalidMethodCall),
            AsPathToken::Len => Err(VmError::InvalidMethodCall),
        }
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

fn try_remove_first<T>(vec: &mut Vec::<T>) -> Result<T, VmError> {
    if vec.is_empty() {
        Ok(vec.swap_remove(0))
    } else {
        Err(VmError::InvalidMethodCall)
    }
}

// impl VectorValue for routecore::bgp::aspath::HopPath {
//     type ReadItem = routecore::bgp::aspath::OwnedHop;
//     type WriteItem = routecore::asn::Asn;

//     fn prepend_vec(
//         &mut self,
//         vector: Vec<Self::WriteItem>,
//     ) -> Result<(), LongSegmentError> {
//         let mut as_path = vector
//             .iter()
//             .map(|a| routecore::bgp::aspath::OwnedHop::Asn(*a))
//             .collect::<Vec<_>>();
//         as_path.extend_from_slice(std::mem::take(self).iter().as_slice());

//         std::mem::swap(self, &mut as_path.into());

//         Ok(())
//     }

//     fn append_vec(
//         &mut self,
//         vector: Vec<Self::WriteItem>,
//     ) -> Result<(), LongSegmentError> {
//         for asn in vector {
//             self.append(OwnedHop::Asn(asn));
//         }

//         Ok(())
//     }

//     // Naïve insert that will try to append to the segment that is already in
//     // place at the specified position. Fancier, more conditional ways are
//     // available to the roto user, but those methods are implemented directly
//     // on builtin::AsPath.
//     fn insert_vec(
//         &mut self,
//         pos: u8,
//         vector: Vec<Self::WriteItem>,
//     ) -> Result<(), LongSegmentError> {
//         let mut as_path = std::mem::take(self);
//         let mut left_path = as_path[..pos as usize].to_vec();
//         left_path.extend_from_slice(
//             vector
//                 .into_iter()
//                 .map(|a| a.into())
//                 .collect::<Vec<_>>()
//                 .as_slice(),
//         );
//         left_path.extend_from_slice(&self[pos as usize..]);

//         std::mem::swap(self, &mut as_path);

//         Ok(())
//     }

//     fn vec_len(&self) -> Option<usize> {
//         Some(self.hop_count())
//     }

//     fn vec_is_empty(&self) -> bool {
//         self.hop_count() == 0
//     }

//     fn into_vec(self) -> Vec<Self::ReadItem> {
//         self.into_iter().collect::<Vec<_>>()
//     }
// }

impl From<routecore::bgp::aspath::HopPath> for BuiltinTypeValue {
    fn from(value: routecore::bgp::aspath::HopPath) -> Self {
        BuiltinTypeValue::AsPath(value)
    }
}

impl From<routecore::bgp::aspath::HopPath> for TypeValue {
    fn from(value: routecore::bgp::aspath::HopPath) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::AsPath(
            value
        ))
    }
}

impl From<Vec<Asn>> for TypeValue {
    fn from(as_path: Vec<Asn>) -> Self {
        let as_path: Vec<routecore::bgp::aspath::Hop<Vec<u8>>> =
            as_path.iter().map(|p| (*p).into()).collect();
        let as_path = routecore::bgp::aspath::HopPath::from(as_path);
        TypeValue::Builtin(BuiltinTypeValue::AsPath(as_path))
    }
}

impl From<Vec<routecore::bgp::aspath::Hop<Vec<u8>>>> for TypeValue {
    fn from(as_path: Vec<routecore::bgp::aspath::Hop<Vec<u8>>>) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::AsPath(as_path.into()))
    }
}

#[repr(u8)]
#[derive(Debug)]
pub(crate) enum AsPathToken {
    Origin = 1,
    Contains = 2,
    Len = 3,
    Set = 4,
}

impl TryFrom<usize> for AsPathToken {
    type Error = VmError;

    fn try_from(value: usize) -> Result<Self, VmError> {
        match value {
            1 => Ok(AsPathToken::Origin),
            2 => Ok(AsPathToken::Contains),
            3 => Ok(AsPathToken::Len),
            4 => Ok(AsPathToken::Set),
            _ => {
                debug!("Unknown AsPathToken value: {}", value);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<AsPathToken> for usize {
    fn from(val: AsPathToken) -> Self {
        val as usize
    }
}


//------------ Hop type -----------------------------------------------------

// A read-only type that contains an ASN or a more complex segment of a AS
// PATH, e.g. an AS_SET.
use routecore::bgp::aspath::OwnedHop as Hop;

createtoken!(Hop; Set = 0);

impl RotoType for Hop {
    intotype!(Hop; StringLiteral);

    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "set" => Ok(MethodProps::new(
                TypeDef::Unknown,
                HopToken::Set.into(),
                vec![TypeDef::Hop],
            )
            .consume_value()),
            _ => Err(format!("Unknown method: '{}' for type Hop",
                method_name.ident
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidMethodCall)
    }

    fn exec_consume_value_method(
        self,
        method_token: usize,
        mut args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.try_into()? {
            HopToken::Set => {
                if let Ok(TypeValue::Builtin(BuiltinTypeValue::Hop(
                    value,
                ))) = args.remove(0).into_type(&TypeDef::Hop)
                {
                    Ok(TypeValue::Builtin(BuiltinTypeValue::Hop(value)))
                } else {
                    Err(VmError::InvalidValueType)
                }
            }
        }
    }

    fn exec_type_method(
        _method_token: usize,
        _args: &[crate::vm::StackValue],
        _res_type: crate::types::typedef::TypeDef,
    ) -> Result<TypeValue, crate::vm::VmError> {
        Err(VmError::InvalidMethodCall)
    }
}

typevaluefromimpls!(Hop);


//------------ OriginType type ----------------------------------------------

minimalscalartype!(Origin);

//------------ NextHop type -------------------------------------------------

minimalscalartype!(NextHop);

//------------ Multi Exit Discriminator type --------------------------------

minimalscalartype!(MultiExitDisc);

//------------ Unknown type -------------------------------------------------

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Unknown;

impl From<Unknown> for TypeValue {
    fn from(_value: Unknown) -> Self {
        TypeValue::Unknown
    }
}

impl RotoType for Unknown {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "is_unknown" => Ok(MethodProps::new(
                TypeDef::Bool,
                UnknownToken::IsUnknown.into(),
                vec![],
            )
            .consume_value()),
            _ => Err(format!(
                "Unknown method: '{}' for type Unknown",
                method_name.ident
            )
            .into()),
        }
    }

    fn into_type(
        self,
        _type_value: &TypeDef,
    ) -> Result<TypeValue, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,

        _args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

#[derive(Debug)]
pub(crate) enum UnknownToken {
    IsUnknown,
}

impl TryFrom<usize> for UnknownToken {
    type Error = VmError;

    fn try_from(val: usize) -> Result<Self, VmError> {
        match val {
            0 => Ok(UnknownToken::IsUnknown),
            _ => {
                debug!("Unknown token value: {}", val);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<UnknownToken> for usize {
    fn from(val: UnknownToken) -> Self {
        match val {
            UnknownToken::IsUnknown => 0,
        }
    }
}


//------------ Local Preference type ----------------------------------------

scalartype!(LocalPref; u32; IntegerLiteral = i64);


//------------ AtomicAggregate type -----------------------------------------

minimalscalartype!(AtomicAggregate);


//------------ Aggregator type -----------------------------------------------

minimalscalartype!(AggregatorInfo);


//------------ NlriStatus type -----------------------------------------------

// Status is piece of metadata that writes some (hopefully) relevant state of
// per-peer BGP session into every route. The goal is to be able to enable
// the logic in `rib-units` to decide whether routes should be send to its
// output and to be able output this information to API clients, without
// having to go back to the units that keep the per-peer session state.
#[derive(
    Debug,
    Eq,
    PartialEq,
    Copy,
    Clone,
    Default,
    Hash,
    Ord,
    PartialOrd,
    Serialize,
)]
pub enum NlriStatus {
    // Between start and EOR on a BGP peer-session
    InConvergence = 0,
    // After EOR for a BGP peer-session, either `Graceful Restart` or EOR
    UpToDate = 1,
    // After hold-timer expiry
    Stale = 2,
    // After the request for a Route Refresh to a peer and the reception of a
    // new route
    StartOfRouteRefresh = 3,
    // After the reception of a withdrawal
    Withdrawn = 4,
    // A routecore::bgp::ParseError happened on a part of the PDU!
    Unparsable = 5,
    // Status not relevant, e.g. a RIB that holds archived routes.
    #[default]
    Empty,
}

impl NlriStatus {
    pub fn get_field_by_index(&self, field_index: &[usize]) -> Result<TypeValue, VmError> {
        let tv: NlriStatus = FieldIndex::from(field_index).first()?.try_into()?;
        Ok(tv.into())
    }
}

impl std::fmt::Display for NlriStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NlriStatus::InConvergence => write!(f, "in convergence"),
            NlriStatus::UpToDate => write!(f, "up to date"),
            NlriStatus::Stale => write!(f, "stale"),
            NlriStatus::StartOfRouteRefresh => {
                write!(f, "start of route refresh")
            }
            NlriStatus::Withdrawn => write!(f, "withdrawn"),
            NlriStatus::Unparsable => write!(f, "UNPARSABLE"),
            NlriStatus::Empty => write!(f, "empty"),
        }
    }
}

typevaluefromimpls!(NlriStatus);

impl TryFrom<TypeValue> for NlriStatus {
    type Error = VmError;

    fn try_from(value: TypeValue) -> Result<Self, VmError> {
        if let TypeValue::Builtin(BuiltinTypeValue::NlriStatus(value)) =
            value
        {
            Ok(value)
        } else {
            debug!("invalid something");
            Err(VmError::InvalidMethodCall)
        }
    }
}

impl TryFrom<usize> for NlriStatus {
    type Error = VmError;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(NlriStatus::InConvergence),
            1 => Ok(NlriStatus::UpToDate),
            2 => Ok(NlriStatus::UpToDate),
            3 => Ok(NlriStatus::StartOfRouteRefresh),
            4 => Ok(NlriStatus::Withdrawn),
            5 => Ok(NlriStatus::Unparsable),
            _ => Err(VmError::InvalidContext)
        }
    }
}
