use std::fmt::{Display, Formatter};
use std::str::FromStr;

use log::{debug, error, trace};
use routecore::asn::LongSegmentError;
use routecore::bgp::communities::{
    ExtendedCommunity, LargeCommunity, StandardCommunity, Wellknown,
};
use serde::ser::SerializeSeq;
use serde::{Serialize, Serializer};

use crate::ast::{IpAddressLiteral, PrefixLiteral};
use crate::attr_change_set::VectorValue;
use crate::compiler::compile::CompileError;
use crate::first_into_vm_err;
use crate::traits::RotoType;
use crate::types::collections::{ElementTypeValue, List};
use crate::types::enum_types::EnumVariant;
use crate::types::typedef::MethodProps;
use crate::vm::{StackValue, VmError};

use super::super::typedef::TypeDef;
use super::super::typevalue::TypeValue;
use super::builtin_type_value::BuiltinTypeValue;

//------------ U16 Type -----------------------------------------------------

#[derive(Debug, Eq, Copy, Clone, Serialize)]
pub struct U16(pub(crate) u16);

impl U16 {
    pub fn new(val: u16) -> Self {
        U16(val)
    }
}

impl From<U16> for TypeValue {
    fn from(val: U16) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::U16(val))
    }
}

impl RotoType for U16 {
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
                U16Token::Set.into(),
                vec![TypeDef::IntegerLiteral],
            )
            .consume_value()),
            _ => Err(format!(
                "Unknown method: '{}' for type U16",
                method_name.ident
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
        Err(VmError::InvalidMethodCall)
    }

    fn exec_consume_value_method(
        self,
        method_token: usize,
        mut args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.try_into()? {
            U16Token::Set => {
                if let Ok(TypeValue::Builtin(BuiltinTypeValue::U16(
                    int_u16,
                ))) = args.remove(0).into_type(&TypeDef::U16)
                {
                    Ok(TypeValue::Builtin(BuiltinTypeValue::U16(int_u16)))
                } else {
                    Err(VmError::InvalidValueType)
                }
            }
        }
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidMethodCall)
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::U16 => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::U16(self)))
            }
            TypeDef::U32 => Ok(TypeValue::Builtin(U32(self.0 as u32).into())),
            TypeDef::Asn => Ok(TypeValue::Builtin(BuiltinTypeValue::Asn(
                Asn(routecore::asn::Asn::from(self.0 as u32)),
            ))),
            TypeDef::PrefixLength => match self.0 {
                0..=128 => {
                    Ok(TypeValue::Builtin(BuiltinTypeValue::PrefixLength(
                        PrefixLength(self.0 as u8),
                    )))
                }
                _ => Err(format!(
                    "Cannot convert an instance of type U16 with \
                    a value greater than 128 into type {:?}",
                    type_def
                )
                .into()),
            },
            TypeDef::U8 => match self.0 {
                0..=255 => Ok(TypeValue::Builtin(BuiltinTypeValue::U8(U8(
                    self.0 as u8,
                )))),
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

impl PartialEq for U16 {
    fn eq(&self, other: &Self) -> bool {
        if let Ok(TypeValue::Builtin(BuiltinTypeValue::U16(U16(o)))) =
            other.into_type(&TypeDef::U16)
        {
            o == self.0
        } else {
            false
        }
    }
}

impl std::hash::Hash for U16 {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl Display for U16 {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub enum U16Token {
    Set,
}

impl TryFrom<usize> for U16Token {
    type Error = VmError;

    fn try_from(val: usize) -> Result<Self, VmError> {
        match val {
            0 => Ok(U16Token::Set),
            t => {
                error!("Cannot find method on U16 for token: {}", t);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<U16Token> for usize {
    fn from(val: U16Token) -> Self {
        match val {
            U16Token::Set => 0,
        }
    }
}

// ----------- U32 Type --------------------------------------------

#[derive(Debug, Eq, Copy, Clone, Serialize)]
pub struct U32(pub(crate) u32);

impl U32 {
    pub fn new(val: u32) -> Self {
        U32(val)
    }
}

impl From<U32> for TypeValue {
    fn from(val: U32) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::U32(val))
    }
}

impl RotoType for U32 {
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
                U32Token::Set.into(),
                vec![TypeDef::IntegerLiteral],
            )
            .consume_value()),
            _ => Err(format!(
                "Unknown method: '{}' for type U32",
                method_name.ident
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
        Err(VmError::InvalidMethodCall)
    }

    fn exec_consume_value_method(
        self,
        method_token: usize,
        mut args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.try_into()? {
            U32Token::Set => {
                if let Ok(TypeValue::Builtin(BuiltinTypeValue::U32(
                    int_u32,
                ))) = args.remove(0).into_type(&TypeDef::U32)
                {
                    Ok(TypeValue::Builtin(BuiltinTypeValue::U32(int_u32)))
                } else {
                    Err(VmError::InvalidValueType)
                }
            }
        }
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidMethodCall)
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::U32 => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::U32(self)))
            }
            TypeDef::Asn => Ok(TypeValue::Builtin(BuiltinTypeValue::Asn(
                Asn(routecore::asn::Asn::from(self.0)),
            ))),
            TypeDef::PrefixLength => match self.0 {
                0..=128 => {
                    Ok(TypeValue::Builtin(BuiltinTypeValue::PrefixLength(
                        PrefixLength(self.0 as u8),
                    )))
                }
                _ => Err(format!(
                    "Cannot convert an instance of type U32 with \
                    a value greater than 128 into type {:?}",
                    type_def
                )
                .into()),
            },
            TypeDef::U8 => match self.0 {
                0..=255 => Ok(TypeValue::Builtin(BuiltinTypeValue::U8(U8(
                    self.0 as u8,
                )))),
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

impl PartialEq for U32 {
    fn eq(&self, other: &Self) -> bool {
        if let Ok(TypeValue::Builtin(BuiltinTypeValue::U32(U32(o)))) =
            other.into_type(&TypeDef::U32)
        {
            o == self.0
        } else {
            false
        }
    }
}

impl std::hash::Hash for U32 {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl Display for U32 {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub enum U32Token {
    Set,
}

impl TryFrom<usize> for U32Token {
    type Error = VmError;

    fn try_from(val: usize) -> Result<Self, VmError> {
        match val {
            0 => Ok(U32Token::Set),
            _ => {
                debug!("Unknown token value: {}", val);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<U32Token> for usize {
    fn from(val: U32Token) -> Self {
        match val {
            U32Token::Set => 0,
        }
    }
}

// ----------- U8 Type ---------------------------------------------

#[derive(Debug, Eq, Copy, Clone, Ord, PartialOrd, Serialize)]
pub struct U8(pub(crate) u8);

impl U8 {
    pub fn new(val: u8) -> Self {
        U8(val)
    }
}

impl RotoType for U8 {
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
                U8Token::Set.into(),
                vec![TypeDef::IntegerLiteral],
            )
            .consume_value()),
            _ => Err(format!(
                "Unknown method: '{}' for type U8",
                method_name.ident
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
        Err(VmError::InvalidMethodCall)
    }

    fn exec_consume_value_method(
        self,
        method_token: usize,
        mut args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.try_into()? {
            U32Token::Set => {
                if let Ok(TypeValue::Builtin(BuiltinTypeValue::U8(int_u8))) =
                    args.remove(0).into_type(&TypeDef::U8)
                {
                    Ok(TypeValue::Builtin(BuiltinTypeValue::U8(int_u8)))
                } else {
                    Err(VmError::InvalidValueType)
                }
            }
        }
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidMethodCall)
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            // Self
            TypeDef::U8 => Ok(TypeValue::Builtin(BuiltinTypeValue::U8(self))),
            TypeDef::U16 => {
                let value = self.0;
                Ok(TypeValue::Builtin(BuiltinTypeValue::U16(U16(
                    value as u16
                ))))
            }
            TypeDef::U32 => {
                let value = self.0;
                Ok(TypeValue::Builtin(BuiltinTypeValue::U32(U32(
                    value as u32
                ))))
            }
            TypeDef::PrefixLength => match self.0 {
                0..=128 => Ok(TypeValue::Builtin(
                    BuiltinTypeValue::PrefixLength(PrefixLength(self.0)),
                )),
                _ => Err(format!(
                    "Prefix length must be between 0 and 128, not {}",
                    self.0
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

impl PartialEq for U8 {
    fn eq(&self, other: &Self) -> bool {
        if let Ok(TypeValue::Builtin(BuiltinTypeValue::U8(U8(o)))) =
            other.into_type(&TypeDef::U8)
        {
            o == self.0
        } else {
            false
        }
    }
}

impl std::hash::Hash for U8 {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl Display for U8 {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<U8> for TypeValue {
    fn from(val: U8) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::U8(val))
    }
}

#[derive(Debug)]
pub enum U8Token {
    Set,
}

impl TryFrom<usize> for U8Token {
    type Error = VmError;

    fn try_from(val: usize) -> Result<Self, VmError> {
        match val {
            0 => Ok(U8Token::Set),
            _ => {
                debug!("Unknown token value: {}", val);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<U8Token> for usize {
    fn from(val: U8Token) -> Self {
        match val {
            U8Token::Set => 0,
        }
    }
}

// ----------- Boolean Type -------------------------------------------------

#[derive(
    Debug, Eq, PartialEq, Copy, Clone, Hash, Ord, PartialOrd, Serialize,
)]
pub struct Boolean(pub(crate) bool);
impl Boolean {
    pub fn new(val: bool) -> Self {
        Boolean(val)
    }

    pub fn is_false(&self) -> bool {
        !self.0
    }
}

impl RotoType for Boolean {
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
                BooleanToken::Set.into(),
                vec![TypeDef::Boolean],
            )
            .consume_value()),
            _ => Err(format!(
                "Unknown method: '{}' for type Boolean",
                method_name.ident
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
        Err(VmError::InvalidMethodCall)
    }

    fn exec_consume_value_method(
        self,
        method_token: usize,
        mut args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.try_into()? {
            BooleanToken::Set => {
                if let Ok(TypeValue::Builtin(BuiltinTypeValue::Boolean(b))) =
                    args.remove(0).into_type(&TypeDef::Boolean)
                {
                    Ok(TypeValue::Builtin(BuiltinTypeValue::Boolean(b)))
                } else {
                    Err(VmError::InvalidValueType)
                }
            }
        }
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidMethodCall)
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::Boolean => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::Boolean(self)))
            }
            TypeDef::StringLiteral => match self.0 {
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

impl From<Boolean> for TypeValue {
    fn from(val: Boolean) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Boolean(val))
    }
}

impl From<Boolean> for BuiltinTypeValue {
    fn from(val: Boolean) -> Self {
        BuiltinTypeValue::Boolean(val)
    }
}

impl From<bool> for BuiltinTypeValue {
    fn from(value: bool) -> Self {
        BuiltinTypeValue::Boolean(Boolean(value))
    }
}

impl Display for Boolean {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub enum BooleanToken {
    Set,
}

impl TryFrom<usize> for BooleanToken {
    type Error = VmError;
    fn try_from(val: usize) -> Result<Self, VmError> {
        match val {
            0 => Ok(BooleanToken::Set),
            _ => {
                debug!("Unknown token value: {}", val);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<BooleanToken> for usize {
    fn from(val: BooleanToken) -> Self {
        val as usize
    }
}

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
                TypeDef::Boolean,
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
                    args.get(0).ok_or(VmError::InvalidMethodCall)?.as_ref()
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
                )) = args.remove(0).into_type(&TypeDef::StringLiteral)
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
                    args.get(0).ok_or(VmError::InvalidMethodCall)?.as_ref()
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
                    args.get(0).ok_or(VmError::InvalidMethodCall)?.as_ref()
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
            TypeDef::Boolean => match self.0.as_str() {
                "true" => Ok(TypeValue::Builtin(BuiltinTypeValue::Boolean(
                    Boolean(true),
                ))),
                "false" => Ok(TypeValue::Builtin(BuiltinTypeValue::Boolean(
                    Boolean(false),
                ))),
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
                    Ok(TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(
                        routecore::asn::Asn::from(self.0 as u32),
                    ))))
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
                .map(|v| TypeValue::Builtin(BuiltinTypeValue::U32(U32(v))))
                .map_err(|_| {
                    CompileError::from(format!(
                        "Cannot convert instanc of type IntegerLiteral with \
                        value {} into U32",
                        self.0
                    ))
                }),
            TypeDef::U8 => u8::try_from(self.0)
                .map(|v| TypeValue::Builtin(BuiltinTypeValue::U8(U8(v))))
                .map_err(|_| {
                    CompileError::from(format!(
                        "Cannot convert instance of type IntegerLiteral with \
                        value {} into U8",
                        self.0
                    ))
                }),
            TypeDef::U16 => u16::try_from(self.0)
                .map(|v| TypeValue::Builtin(BuiltinTypeValue::U16(U16(v))))
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
                Ok(TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(
                    args[0] == args[1],
                ))))
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
                            Community::from(v as u32),
                        )))
                    }
                    // Convert to an ExtendedCommunity
                    v => Ok(TypeValue::Builtin(BuiltinTypeValue::Community(
                        Community::from(v),
                    ))),
                }
            }
            TypeDef::U8 => u8::try_from(self.0)
                .map(|v| TypeValue::Builtin(BuiltinTypeValue::U8(U8(v))))
                .map_err(|_| {
                    CompileError::from(format!(
                        "Cannot convert type IntegerLiteral with {} into U8",
                        self.0
                    ))
                }),
            TypeDef::U32 => u32::try_from(self.0)
                .map(|v| TypeValue::Builtin(BuiltinTypeValue::U32(U32(v))))
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

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash, Serialize)]
pub struct Prefix(pub(crate) routecore::addr::Prefix);

impl Prefix {
    pub fn new(prefix: routecore::addr::Prefix) -> Self {
        Self(prefix)
    }
}

impl RotoType for Prefix {
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
                vec![TypeDef::IpAddress, TypeDef::PrefixLength],
            )),
            "address" => Ok(MethodProps::new(
                TypeDef::IpAddress,
                PrefixToken::Address.into(),
                vec![],
            )),
            "len" => Ok(MethodProps::new(
                TypeDef::PrefixLength,
                PrefixToken::Len.into(),
                vec![],
            )),
            "matches" => Ok(MethodProps::new(
                TypeDef::Boolean,
                PrefixToken::Matches.into(),
                vec![TypeDef::Prefix],
            )),
            "exists" => Ok(MethodProps::new(
                TypeDef::Boolean,
                PrefixToken::Exists.into(),
                vec![],
            )),
            "covers" => Ok(MethodProps::new(
                TypeDef::Boolean,
                PrefixToken::Covers.into(),
                vec![TypeDef::Prefix],
            )),
            "is_covered_by" => Ok(MethodProps::new(
                TypeDef::Boolean,
                PrefixToken::IsCoveredBy.into(),
                vec![TypeDef::Prefix],
            )),
            "contains" => Ok(MethodProps::new(
                TypeDef::Boolean,
                PrefixToken::Contains.into(),
                vec![TypeDef::IpAddress],
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
            TypeDef::Prefix => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::Prefix(self)))
            }
            _ => Err(format!(
                "Cannot convert type Prefix to type {:?}",
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
            PrefixToken::Address => {
                let prefix = self.0;
                Ok(TypeValue::Builtin(BuiltinTypeValue::IpAddress(
                    IpAddress(prefix.addr()),
                )))
            }
            PrefixToken::Len => {
                let Prefix(pfx) = self;
                Ok(TypeValue::Builtin(BuiltinTypeValue::PrefixLength(
                    PrefixLength(pfx.len()),
                )))
            }
            PrefixToken::From => unimplemented!(),
            PrefixToken::Exists => Ok(true.into()),
            PrefixToken::Matches => todo!(),
            PrefixToken::Covers => {
                if let Some(other_pfx) = args.get(0) {
                    if let TypeValue::Builtin(BuiltinTypeValue::Prefix(
                        Prefix(other),
                    )) = other_pfx.as_ref()
                    {
                        Ok(self.0.covers(*other).into())
                    } else {
                        Err(VmError::InvalidMethodCall)
                    }
                } else {
                    Ok(TypeValue::Unknown)
                }
            }
            PrefixToken::IsCoveredBy => {
                if let Some(other_pfx) = args.get(0) {
                    if let TypeValue::Builtin(BuiltinTypeValue::Prefix(
                        Prefix(other),
                    )) = other_pfx.as_ref()
                    {
                        Ok(other.covers(self.0).into())
                    } else {
                        Err(VmError::InvalidMethodCall)
                    }
                } else {
                    Ok(TypeValue::Unknown)
                }
            }
            PrefixToken::Contains => {
                if let Some(other_ip) = args.get(0) {
                    if let TypeValue::Builtin(BuiltinTypeValue::IpAddress(
                        IpAddress(other),
                    )) = other_ip.as_ref()
                    {
                        Ok(self.0.contains(*other).into())
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
            PrefixToken::From if args.len() == 2 => {
                if let TypeValue::Builtin(BuiltinTypeValue::IpAddress(ip)) =
                    args[0].as_ref()
                {
                    let len: PrefixLength = args[1]
                        .as_ref()
                        .try_into()
                        .map_err(|_e| VmError::InvalidConversion)?;
                    let ip = ip.0;
                    Ok(routecore::addr::Prefix::new(ip, len.0)
                        .map_or_else(|_| TypeValue::Unknown, |p| p.into()))
                } else {
                    Err(VmError::AnonymousArgumentNotFound)
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

impl From<Prefix> for TypeValue {
    fn from(val: Prefix) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Prefix(val))
    }
}

impl TryFrom<&'_ PrefixLiteral> for Prefix {
    type Error = CompileError;

    fn try_from(value: &PrefixLiteral) -> Result<Self, Self::Error> {
        Ok(Prefix(
            <routecore::addr::Prefix as std::str::FromStr>::from_str(
                value.0.as_str(),
            )
            .map_err(|e| {
                CompileError::from(format!(
                    "Cannot parse '{:?}' as a Prefix: {}",
                    value, e
                ))
            })?,
        ))
    }
}

impl From<routecore::addr::Prefix> for Prefix {
    fn from(val: routecore::addr::Prefix) -> Self {
        Prefix(val)
    }
}

impl Display for Prefix {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<Prefix> for routecore::addr::Prefix {
    fn from(val: Prefix) -> Self {
        val.0
    }
}

#[derive(Debug)]
#[repr(u8)]
pub(crate) enum PrefixToken {
    From = 0,
    Exists = 1,
    Address = 2,
    Len = 3,
    Matches = 4,
    Covers = 5,
    IsCoveredBy = 6,
    Contains = 7,
}

impl TryFrom<usize> for PrefixToken {
    type Error = VmError;

    fn try_from(val: usize) -> Result<Self, VmError> {
        match val {
            0 => Ok(PrefixToken::From),
            1 => Ok(PrefixToken::Exists),
            2 => Ok(PrefixToken::Address),
            3 => Ok(PrefixToken::Len),
            4 => Ok(PrefixToken::Matches),
            5 => Ok(PrefixToken::Covers),
            6 => Ok(PrefixToken::IsCoveredBy),
            7 => Ok(PrefixToken::Contains),
            _ => {
                debug!("Unknown token value: {}", val);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<PrefixToken> for usize {
    fn from(val: PrefixToken) -> Self {
        val as usize
    }
}

//------------ PrefixLengthLiteral type --------------------------------------

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash, Serialize)]
pub struct PrefixLength(pub(crate) u8);

impl PrefixLength {
    pub fn new(val: u8) -> Self {
        PrefixLength(val)
    }
}

impl RotoType for PrefixLength {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "from" => Ok(MethodProps::new(
                TypeDef::PrefixLength,
                PrefixLengthToken::From.into(),
                vec![TypeDef::U8],
            )),
            _ => Err(format!(
                "Unknown method: '{}' for type PrefixLength",
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
            TypeDef::PrefixLength => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::PrefixLength(self)))
            }
            TypeDef::U8 => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::U8(U8(self.0))))
            }
            TypeDef::U16 => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::U16(U16(self
                    .0
                    .into()))))
            }
            TypeDef::U32 => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::U32(U32(self
                    .0
                    .into()))))
            }
            _ => Err(format!(
                "Cannot convert type PrefixLength to type {:?}",
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
        _type_def: TypeDef,
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

impl From<PrefixLength> for TypeValue {
    fn from(val: PrefixLength) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::PrefixLength(val))
    }
}

impl Display for PrefixLength {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "/{}", self.0)
    }
}

#[derive(Debug)]
pub(crate) enum PrefixLengthToken {
    From,
}

impl TryFrom<usize> for PrefixLengthToken {
    type Error = VmError;

    fn try_from(val: usize) -> Result<Self, VmError> {
        match val {
            0 => Ok(PrefixLengthToken::From),
            _ => {
                debug!("Unknown token value: {}", val);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<PrefixLengthToken> for usize {
    fn from(val: PrefixLengthToken) -> Self {
        val as usize
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

// ----------- Community ----------------------------------------------------

/// A BGP community.
///
/// # Serialization
///
/// The routecore types implement the Serialize trait but do so in a way
/// suitable for machine <-> machine interaction where the consuming code is
/// also routecore, i.e. the details of how (de)serialization is done are not
/// important to nor intended to be visible to anyone except routecore itself.
///
/// In roto however, a TypeValue (which may contain a Community) can be
/// serialized in order to render it to consumers outside the application,
/// e.g. as JSON served by a HTTP API or contained in an MQTT payload. The
/// operators of the deployed application can be expected to be familiar with
/// details of BGP and it is more useful to them if the rendered form of a BGP
/// community is somewhat relatable to the way communities are defined by and
/// referred to in BGP RFCs and not exposing internally structural details of
/// how routecore stores communities (e.g. as raw byte vectors for example).
///
/// We therefore provide our Serialize impl which will be used by callers when
/// serializing our Community type and in turn the contained routecore
/// Community type and its children.
#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub struct Community(pub(crate) routecore::bgp::communities::Community);

impl Community {
    pub fn new(com: routecore::bgp::communities::Community) -> Self {
        Self(com)
    }
}

pub trait SerializeForOperators: Serialize {
    fn serialize_for_operator<S>(
        &self,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer;
}

impl Serialize for Community {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if serializer.is_human_readable() {
            match &self.0 {
                routecore::bgp::communities::Community::Standard(c) => {
                    c.serialize_for_operator(serializer)
                }
                routecore::bgp::communities::Community::Large(c) => {
                    c.serialize_for_operator(serializer)
                }
                routecore::bgp::communities::Community::Extended(c) => {
                    c.serialize_for_operator(serializer)
                }
                routecore::bgp::communities::Community::Ipv6Extended(c) => {
                    // TODO: Also implement SerializeForOperators for IPv6
                    // Extended Communities.
                    c.serialize(serializer)
                }
            }
        } else {
            self.0.serialize(serializer)
        }
    }
}

impl SerializeForOperators for StandardCommunity {
    fn serialize_for_operator<S>(
        &self,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self.to_wellknown() {
            Some(Wellknown::Unrecognized(_)) => ser::Community {
                raw_fields: vec![format!("{:#010X}", self.to_u32())],
                r#type: "standard",
                parsed: ser::Parsed::ExplicitValue {
                    value: ser::Value::Plain(ser::PlainValue {
                        r#type: "well-known-unrecognised",
                    }),
                },
            }
            .serialize(serializer),

            Some(wk) => ser::Community {
                raw_fields: vec![format!("{:#010X}", self.to_u32())],
                r#type: "standard",
                parsed: ser::Parsed::ExplicitValue {
                    value: ser::Value::Attribute(ser::AttributeValue {
                        r#type: "well-known",
                        attribute: format!("{}", wk),
                    }),
                },
            }
            .serialize(serializer),

            None if self.is_reserved() => ser::Community {
                raw_fields: vec![format!("{:#010X}", self.to_u32())],
                r#type: "standard",
                parsed: ser::Parsed::ExplicitValue {
                    value: ser::Value::Plain(ser::PlainValue {
                        r#type: "reserved",
                    }),
                },
            }
            .serialize(serializer),

            None if self.is_private() => {
                let asn = if let Some(asn) = self.asn() {
                    // ASNs can only be 2-byte in standard communities, so not
                    // being able to parse it into one is a (weird) error.
                    if let Ok(asn) = asn.try_into_u16() {
                        asn
                    } else {
                        return Err(serde::ser::Error::custom(format!(
                            "ASN {} is not a 2-byte ASN and cannot \
                            be converted",
                            asn
                        )));
                    }
                } else {
                    return Err(serde::ser::Error::custom(format!(
                        "ASN {:?} contains invalid characters",
                        self.asn()
                    )));
                };
                let tag: u16 = if let Some(tag) = self.tag() {
                    tag.value()
                } else {
                    return Err(serde::ser::Error::custom(format!(
                        "Tag {:?} contains invalid characters",
                        self.tag()
                    )));
                };
                let formatted_asn = format!("AS{}", asn); // to match Routinator JSON style
                ser::Community {
                    raw_fields: vec![
                        format!("{:#06X}", asn),
                        format!("{:#06X}", tag),
                    ],
                    r#type: "standard",
                    parsed: ser::Parsed::ExplicitValue {
                        value: ser::Value::AsnTag(ser::AsnTagValue {
                            r#type: "private",
                            asn: formatted_asn,
                            tag,
                        }),
                    },
                }
                .serialize(serializer)
            }

            _ => serializer.serialize_none(),
        }
    }
}

impl SerializeForOperators for LargeCommunity {
    fn serialize_for_operator<S>(
        &self,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let asn = format!("AS{}", self.global());

        ser::Community {
            raw_fields: vec![
                format!("{:#06X}", self.global()),
                format!("{:#06X}", self.local1()),
                format!("{:#06X}", self.local2()),
            ],
            r#type: "large",
            parsed: ser::Parsed::InlineValue(
                ser::Value::GlobalLocalDataParts(
                    ser::GlobalLocalDataPartsValue {
                        globalAdmin: ser::GlobalAdmin {
                            r#type: "asn",
                            value: asn,
                        },
                        localDataPart1: self.local1(),
                        localDataPart2: self.local2(),
                    },
                ),
            ),
        }
        .serialize(serializer)
    }
}

impl SerializeForOperators for ExtendedCommunity {
    fn serialize_for_operator<S>(
        &self,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // The structure doesn't tell us if we have to look at the "type low"
        // (subtype()) value or not, we can only know that based on knowledge
        // of the "type value" stored in the IANA BGP Extended Communities
        // registry [1].
        //
        // [1]: https://www.iana.org/assignments/bgp-extended-communities/
        //      bgp-extended-communities.xhtml
        use routecore::bgp::communities::ExtendedCommunitySubType::*;
        use routecore::bgp::communities::ExtendedCommunityType::*;

        let mut raw_fields = Vec::new();
        let (typ, subtyp) = self.types();

        match (typ, subtyp) {
            // 0x00 = Transitive Two-Octet AS-Specific Extended Community (RFC 7153)
            // - 0x02 = Route Target (RFC 4360)
            // - 0x03 = Route Origin (RFC 4360)
            (TransitiveTwoOctetSpecific, RouteTarget)
            | (TransitiveTwoOctetSpecific, RouteOrigin) => {
                let global_admin = if let Some(ga) = self.as2() {
                    ga
                } else {
                    return Err(serde::ser::Error::custom(format!(
                        "Global Admin {:?} contains invalid characters",
                        self.as2()
                    )));
                };
                let local_admin = if let Some(la) = self.an4() {
                    la
                } else {
                    return Err(serde::ser::Error::custom(format!(
                        "Local Admin {:?} contains invalid characters",
                        self.an4()
                    )));
                };
                raw_fields.push(format!("{:#04X}", self.type_raw()));
                let field = if let Some(field) = self.to_raw().get(1) {
                    *field
                } else {
                    return Err(serde::ser::Error::custom(format!(
                        "Cannot parse extended community from: '{:?}'",
                        self.to_raw()
                    )));
                };
                raw_fields.push(format!("{:#04X}", field));
                raw_fields.push(format!("{:#06X}", global_admin.to_u16()));
                raw_fields.push(format!("{:#010X}", local_admin));
                ser::Community {
                    raw_fields,
                    r#type: "extended",
                    parsed: ser::Parsed::InlineValue(ser::Value::Extended(
                        ser::ExtendedValue {
                            r#type: "as2-specific",
                            transitive: self.is_transitive(),
                            inner: ser::TypedValueInner::As2Specific {
                                rfc7153SubType: match subtyp {
                                    RouteTarget => "route-target",
                                    RouteOrigin => "route-origin",
                                    _ => unreachable!(),
                                },
                                globalAdmin: ser::GlobalAdmin {
                                    r#type: "asn",
                                    value: global_admin.to_string(),
                                },
                                localAdmin: local_admin,
                            },
                        },
                    )),
                }
            }

            // 0x01 = Transitive IPv4-Address-Specific Extended Community (RFC 7153)
            // - 0x02 = Route Target (RFC 4360)
            // - 0x03 = Route Origin (RFC 4360)
            (TransitiveIp4Specific, RouteTarget)
            | (TransitiveIp4Specific, RouteOrigin) => {
                let global_admin = if let Some(ga) = self.ip4() {
                    ga
                } else {
                    return Err(serde::ser::Error::custom(format!(
                        "global Admin {:?} contains invalid characters",
                        self.ip4()
                    )));
                };
                let local_admin = if let Some(la) = self.an2() {
                    la
                } else {
                    return Err(serde::ser::Error::custom(format!(
                        "Local Admin {:?} contains invalid characters",
                        self.an2()
                    )));
                };
                raw_fields.push(format!("{:#04X}", self.type_raw()));
                let field = if let Some(field) = self.to_raw().get(1) {
                    *field
                } else {
                    return Err(serde::ser::Error::custom(format!(
                        "Cannot parse extended community from: '{:?}'",
                        self.to_raw()
                    )));
                };
                raw_fields.push(format!("{:#04X}", field));
                raw_fields.push(format!("{:#010X}", u32::from(global_admin)));
                raw_fields.push(format!("{:#06X}", local_admin));
                ser::Community {
                    raw_fields,
                    r#type: "extended",
                    parsed: ser::Parsed::InlineValue(ser::Value::Extended(
                        ser::ExtendedValue {
                            r#type: "ipv4-address-specific",
                            transitive: self.is_transitive(),
                            inner:
                                ser::TypedValueInner::Ipv4AddressSpecific {
                                    rfc7153SubType: match subtyp {
                                        RouteTarget => "route-target",
                                        RouteOrigin => "route-origin",
                                        _ => unreachable!(),
                                    },
                                    globalAdmin: ser::GlobalAdmin {
                                        r#type: "ipv4-address",
                                        value: global_admin.to_string(),
                                    },
                                    localAdmin: local_admin,
                                },
                        },
                    )),
                }
            }

            _ => {
                raw_fields.extend(
                    self.to_raw().iter().map(|x| format!("{:#04X}", x)),
                );
                ser::Community {
                    raw_fields,
                    r#type: "extended",
                    parsed: ser::Parsed::InlineValue(ser::Value::Extended(
                        ser::ExtendedValue {
                            r#type: "unrecognised",
                            transitive: self.is_transitive(),
                            inner: ser::TypedValueInner::Unrecognised,
                        },
                    )),
                }
            }
        }
        .serialize(serializer)
    }
}

// Types used only by our own Serialize implementation to structure the
// serialized output differently than is done by routecore.
mod ser {
    #[derive(serde::Serialize)]
    #[serde(rename = "value")]
    pub struct PlainValue {
        pub r#type: &'static str,
    }

    #[derive(serde::Serialize)]
    #[serde(rename = "value")]
    pub struct AsnTagValue {
        pub r#type: &'static str,
        pub asn: String,
        pub tag: u16,
    }

    #[derive(serde::Serialize)]
    #[serde(rename = "value")]
    pub struct AttributeValue {
        pub r#type: &'static str,
        pub attribute: String,
    }

    #[derive(serde::Serialize)]
    #[serde(rename = "value")]
    pub struct GlobalAdmin {
        pub r#type: &'static str,
        pub value: String,
    }

    #[derive(serde::Serialize)]
    #[serde(rename = "value")]
    #[allow(non_snake_case)]
    pub struct GlobalLocalDataPartsValue {
        pub globalAdmin: GlobalAdmin,
        pub localDataPart1: u32,
        pub localDataPart2: u32,
    }

    #[derive(serde::Serialize)]
    #[serde(rename = "value")]
    pub struct ExtendedValue {
        pub r#type: &'static str,
        pub transitive: bool,
        #[serde(flatten)]
        pub inner: TypedValueInner,
    }

    #[derive(serde::Serialize)]
    #[serde(untagged)]
    #[allow(non_snake_case)]
    pub enum TypedValueInner {
        As2Specific {
            rfc7153SubType: &'static str,
            globalAdmin: GlobalAdmin,
            localAdmin: u32,
        },
        Ipv4AddressSpecific {
            rfc7153SubType: &'static str,
            globalAdmin: GlobalAdmin,
            localAdmin: u16,
        },
        Unrecognised,
    }

    #[derive(serde::Serialize)]
    #[serde(untagged)]
    pub enum Value {
        AsnTag(AsnTagValue),
        Attribute(AttributeValue),
        GlobalLocalDataParts(GlobalLocalDataPartsValue),
        Plain(PlainValue),
        Extended(ExtendedValue),
    }

    #[derive(serde::Serialize)]
    #[serde(rename = "parsed", untagged)]
    pub enum Parsed {
        ExplicitValue { value: Value },
        InlineValue(Value),
    }

    #[derive(serde::Serialize)]
    #[serde(rename = "Community")]
    pub struct Community {
        #[serde(rename = "rawFields")]
        pub raw_fields: Vec<String>,
        pub r#type: &'static str,
        pub parsed: Parsed,
    }
}

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
                TypeDef::Boolean,
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
                    args.get(0).ok_or(VmError::InvalidMethodCall)?.as_ref()
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

impl From<Community> for TypeValue {
    fn from(val: Community) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Community(val))
    }
}

impl From<routecore::bgp::communities::Community> for TypeValue {
    fn from(val: routecore::bgp::communities::Community) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Community(Community(val)))
    }
}

impl From<routecore::bgp::communities::Community> for BuiltinTypeValue {
    fn from(value: routecore::bgp::communities::Community) -> Self {
        BuiltinTypeValue::Community(Community(value))
    }
}

impl From<Vec<routecore::bgp::communities::Community>> for TypeValue {
    fn from(value: Vec<routecore::bgp::communities::Community>) -> Self {
        let list: Vec<ElementTypeValue> = value
            .iter()
            .map(|c| ElementTypeValue::Primitive(TypeValue::from(*c)))
            .collect::<Vec<_>>();
        TypeValue::List(crate::types::collections::List(list))
    }
}

impl From<Vec<routecore::bgp::communities::Community>> for BuiltinTypeValue {
    fn from(value: Vec<routecore::bgp::communities::Community>) -> Self {
        let list: Vec<ElementTypeValue> = value
            .iter()
            .map(|c| ElementTypeValue::Primitive(TypeValue::from(*c)))
            .collect::<Vec<_>>();
        BuiltinTypeValue::Communities(List(list))
    }
}

impl TryFrom<&str> for Community {
    type Error = VmError;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        routecore::bgp::communities::Community::from_str(value)
            .map(Community::new)
            .map_err(|_| VmError::InvalidConversion)
    }
}

// A U32 that we encounter in a source code as a literal gets cast to a
// Standard Community.
impl From<u32> for Community {
    fn from(value: u32) -> Self {
        Self(
            routecore::bgp::communities::StandardCommunity::from_u32(value)
                .into(),
        )
    }
}

impl From<u64> for Community {
    fn from(value: u64) -> Self {
        Self(
            routecore::bgp::communities::ExtendedCommunity::from(
                value.to_be_bytes(),
            )
            .into(),
        )
    }
}

impl Display for Community {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if !f.alternate() {
            write!(f, "{}", self.0)
        } else {
            write!(f, "{:#?}", self.0)
        }
    }
}

#[derive(Debug)]
pub enum CommunityToken {
    From,
    Standard,
    Extended,
    Large,
    As,
    Value,
    Exists,
    Set,
}

impl TryFrom<usize> for CommunityToken {
    type Error = VmError;

    fn try_from(val: usize) -> Result<Self, VmError> {
        match val {
            0 => Ok(CommunityToken::From),
            1 => Ok(CommunityToken::Standard),
            2 => Ok(CommunityToken::Extended),
            3 => Ok(CommunityToken::Large),
            4 => Ok(CommunityToken::As),
            5 => Ok(CommunityToken::Value),
            6 => Ok(CommunityToken::Exists),
            7 => Ok(CommunityToken::Set),
            _ => {
                debug!("Unknown token value: {}", val);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<CommunityToken> for usize {
    fn from(val: CommunityToken) -> Self {
        val as usize
    }
}

//------------ MatchType ----------------------------------------------------

#[derive(Debug, Eq, PartialEq)]
pub enum MatchType {
    ExactMatch,
    LongestMatch,
    EmptyMatch,
}

// ----------- IpAddress type -----------------------------------------------

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash, Serialize)]
pub struct IpAddress(pub(crate) std::net::IpAddr);

impl IpAddress {
    pub fn new(addr: std::net::IpAddr) -> Self {
        IpAddress(addr)
    }
}

impl RotoType for IpAddress {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "from" => Ok(MethodProps::new(
                TypeDef::IpAddress,
                IpAddressToken::From.into(),
                vec![TypeDef::StringLiteral],
            )),
            "matches" => Ok(MethodProps::new(
                TypeDef::Boolean,
                IpAddressToken::Matches.into(),
                vec![TypeDef::Prefix],
            )),
            _ => Err(format!(
                "Unknown method: '{}' for type IpAddress",
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
            TypeDef::IpAddress => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::IpAddress(self)))
            }
            _ => Err(format!(
                "Cannot convert type IpAddress to type {:?}",
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
        todo!();
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

impl From<IpAddress> for TypeValue {
    fn from(val: IpAddress) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::IpAddress(val))
    }
}

impl TryFrom<&'_ IpAddressLiteral> for IpAddress {
    type Error = CompileError;

    fn try_from(value: &IpAddressLiteral) -> Result<Self, Self::Error> {
        Ok(IpAddress(
            <std::net::IpAddr as std::str::FromStr>::from_str(
                value.0.as_str(),
            )
            .map_err(|e| {
                CompileError::from(format!(
                    "Cannot parse '{:?}' as an IP Address: {}",
                    value, e
                ))
            })?,
        ))
    }
}

impl Display for IpAddress {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub(crate) enum IpAddressToken {
    From,
    Matches,
}

impl TryFrom<usize> for IpAddressToken {
    type Error = VmError;

    fn try_from(val: usize) -> Result<Self, VmError> {
        match val {
            0 => Ok(IpAddressToken::From),
            1 => Ok(IpAddressToken::Matches),
            _ => {
                debug!("Unknown token value: {}", val);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<IpAddressToken> for usize {
    fn from(val: IpAddressToken) -> Self {
        val as usize
    }
}

// ----------- Asn type -----------------------------------------------------

#[derive(Debug, Eq, Copy, Clone, Hash, PartialEq, Serialize)]
pub struct Asn(pub(crate) routecore::asn::Asn);

impl Asn {
    pub fn new(asn: routecore::asn::Asn) -> Self {
        Asn(asn)
    }

    pub fn from_u32(asn: u32) -> Self {
        Asn(routecore::asn::Asn::from(asn))
    }

    pub fn get_asn(&self) -> routecore::asn::Asn {
        self.0
    }
}

impl RotoType for Asn {
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
                AsnToken::Set.into(),
                vec![TypeDef::Asn],
            )
            .consume_value()),
            _ => Err(format!(
                "Unknown method: '{}' for type Asn",
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
            TypeDef::Asn => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::Asn(self)))
            }
            TypeDef::StringLiteral => Ok(TypeValue::Builtin(
                BuiltinTypeValue::StringLiteral(self.into()),
            )),
            TypeDef::U8 => match self.0.into_u32() {
                0..=255 => {
                    // this should work, shouldn't it?
                    Ok(TypeValue::Builtin(BuiltinTypeValue::U8(U8(self
                        .0
                        .into_u32()
                        as u8))))
                }
                val => Err(format!(
                    "Cannot convert an instance of type \
                    Asn with a value {} into type ASN. It's greater than 255",
                    val
                )
                .into()),
            },
            TypeDef::U32 => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::U32(U32(self
                    .0
                    .into_u32()))))
            }
            _ => {
                Err(format!("Cannot convert type Asn to type {:?}", type_def)
                    .into())
            }
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.try_into()? {
            AsnToken::Set => {
                if let TypeValue::Builtin(BuiltinTypeValue::Asn(asn)) =
                    args.get(0).ok_or(VmError::InvalidMethodCall)?.as_ref()
                {
                    Ok(TypeValue::from(Asn::new(asn.0)))
                } else {
                    Err(VmError::AnonymousArgumentNotFound)
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
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

impl From<Asn> for TypeValue {
    fn from(value: Asn) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Asn(value))
    }
}

impl From<u32> for Asn {
    fn from(value: u32) -> Self {
        Asn(value.into())
    }
}

impl From<u16> for Asn {
    fn from(value: u16) -> Self {
        Asn((value as u32).into())
    }
}

impl Display for Asn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub enum AsnToken {
    Set,
}

impl TryFrom<usize> for AsnToken {
    type Error = VmError;

    fn try_from(val: usize) -> Result<Self, VmError> {
        match val {
            0 => Ok(AsnToken::Set),
            _ => {
                debug!("Unknown token value: {} for Asn", val);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<AsnToken> for usize {
    fn from(val: AsnToken) -> Self {
        val as usize
    }
}

// ----------- AsPath type --------------------------------------------------

type RoutecoreHop = routecore::bgp::aspath::Hop<Vec<u8>>;

#[derive(Debug, Eq, PartialEq, Clone, Default, Hash)]
pub struct AsPath(pub(crate) routecore::bgp::aspath::HopPath);

impl AsPath {
    pub fn new(
        as_path: Vec<routecore::asn::Asn>,
    ) -> Result<Self, LongSegmentError> {
        let path = routecore::bgp::aspath::HopPath::try_from(as_path)
            .map_err(|_| LongSegmentError)?;
        Ok(AsPath(path))
    }

    pub fn from_vec_u32(as_path: Vec<u32>) -> Result<Self, LongSegmentError> {
        let as_path = as_path
            .into_iter()
            .map(routecore::asn::Asn::from_u32)
            .collect();
        AsPath::new(as_path)
    }

    pub fn into_hops(self) -> Vec<Hop> {
        self.0.into_iter().map(Hop).collect::<Vec<_>>()
    }

    fn into_routecore_hops(
        self,
    ) -> Vec<routecore::bgp::aspath::Hop<Vec<u8>>> {
        self.0.into_iter().collect::<Vec<_>>()
    }

    pub fn contains(&self, hop: &Hop) -> bool {
        self.0.contains(&hop.0)
    }
}

impl Serialize for AsPath {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if serializer.is_human_readable() {
            self.serialize_for_operator(serializer)
        } else {
            self.0.serialize(serializer)
        }
    }
}

impl SerializeForOperators for AsPath {
    fn serialize_for_operator<S>(
        &self,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.0.hop_count()))?;
        for hop in self.0.iter() {
            seq.serialize_element(&format!("{}", hop))?;
        }
        seq.end()
    }
}

impl RotoType for AsPath {
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
                TypeDef::Boolean,
                AsPathToken::Contains.into(),
                vec![TypeDef::Asn],
            )),
            "len" => Ok(MethodProps::new(
                TypeDef::U8,
                AsPathToken::Len.into(),
                vec![],
            )),
            _ => Err(format!(
                "Unknown method '{}' for type AsPath",
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
            TypeDef::AsPath => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::AsPath(self)))
            }
            _ => Err(format!(
                "Cannot convert type AsPath to type {:?}",
                type_def
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        method: usize,

        args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method.try_into()? {
            AsPathToken::Origin => match self.0.origin().cloned() {
                Some(origin_asn) => Ok(TypeValue::Builtin(
                    BuiltinTypeValue::Asn(Asn(origin_asn
                        .try_into_asn()
                        .map_err(|_| VmError::InvalidValueType)?)),
                )),
                None => Err(VmError::InvalidPayload),
            },
            AsPathToken::Contains => {
                if let TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(
                    search_asn,
                ))) = first_into_vm_err!(args, InvalidMethodCall)?.as_ref()
                {
                    let contains =
                        self.contains(&Hop(RoutecoreHop::from(*search_asn)));
                    Ok(TypeValue::Builtin(BuiltinTypeValue::Boolean(
                        Boolean(contains),
                    )))
                } else {
                    Ok(TypeValue::Unknown)
                }
            }
            AsPathToken::Len => {
                let len = self.0.hop_count();
                Ok(TypeValue::Builtin(BuiltinTypeValue::U8(U8(len as u8))))
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
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

impl VectorValue for crate::types::builtin::AsPath {
    type ReadItem = Hop;
    type WriteItem = Asn;

    fn prepend_vec(
        &mut self,
        vector: Vec<Self::WriteItem>,
    ) -> Result<(), LongSegmentError> {
        let mut as_path = vector
            .iter()
            .map(|a| routecore::bgp::aspath::Hop::from(a.0))
            .collect::<Vec<_>>();
        as_path.extend_from_slice(
            std::mem::take(self).into_routecore_hops().as_slice(),
        );

        self.0 = as_path.into();

        Ok(())
    }

    fn append_vec(
        &mut self,
        vector: Vec<Self::WriteItem>,
    ) -> Result<(), LongSegmentError> {
        let mut as_path = std::mem::take(self).into_routecore_hops();
        as_path.extend_from_slice(
            vector
                .iter()
                .map(|a| a.0.into())
                .collect::<Vec<_>>()
                .as_slice(),
        );

        self.0 = as_path.into();

        Ok(())
    }

    // Naïve insert that will try to append to the segment that is already in
    // place at the specified position. Fancier, more conditional ways are
    // available to the roto user, but those methods are implemented directly
    // on builtin::AsPath.
    fn insert_vec(
        &mut self,
        pos: u8,
        vector: Vec<Self::WriteItem>,
    ) -> Result<(), LongSegmentError> {
        let as_path = std::mem::take(self).into_routecore_hops();
        let mut left_path = as_path[..pos as usize].to_vec();
        left_path.extend_from_slice(
            vector
                .into_iter()
                .map(|a| a.0.into())
                .collect::<Vec<_>>()
                .as_slice(),
        );
        left_path.extend_from_slice(&self.0[pos as usize..]);

        self.0 = as_path.into();

        Ok(())
    }

    fn vec_len(&self) -> Option<usize> {
        Some(self.0.hop_count())
    }

    fn vec_is_empty(&self) -> bool {
        self.0.hop_count() == 0
    }

    fn into_vec(self) -> Vec<Self::ReadItem> {
        self.into_hops()
    }
}

impl From<AsPath> for TypeValue {
    fn from(val: AsPath) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::AsPath(val))
    }
}

impl From<routecore::bgp::aspath::HopPath> for AsPath {
    fn from(value: routecore::bgp::aspath::HopPath) -> Self {
        AsPath(value)
    }
}

impl Display for AsPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[repr(u8)]
#[derive(Debug)]
pub(crate) enum AsPathToken {
    Origin = 1,
    Contains = 2,
    Len = 3,
}

impl TryFrom<usize> for AsPathToken {
    type Error = VmError;

    fn try_from(value: usize) -> Result<Self, VmError> {
        match value {
            1 => Ok(AsPathToken::Origin),
            2 => Ok(AsPathToken::Contains),
            3 => Ok(AsPathToken::Len),
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

impl From<Vec<Hop>> for AsPath {
    fn from(value: Vec<Hop>) -> Self {
        AsPath(value.into_iter().map(|h| h.0).collect::<Vec<_>>().into())
    }
}

//------------ Hop type -----------------------------------------------------

// A read-only type that contains an ASN or a more complex segment of a AS
// PATH, e.g. an AS_SET.

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize)]
pub struct Hop(pub(crate) routecore::bgp::aspath::Hop<Vec<u8>>);

impl RotoType for Hop {
    fn get_props_for_method(
        _ty: TypeDef,
        _method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
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

impl Display for Hop {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<Asn> for Hop {
    fn from(value: Asn) -> Self {
        Hop(value.0.into())
    }
}

//------------ OriginType type ----------------------------------------------

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Serialize)]
pub struct OriginType(pub(crate) routecore::bgp::types::OriginType);

impl OriginType {
    pub fn into_inner(self) -> routecore::bgp::types::OriginType {
        self.0
    }
}

impl RotoType for OriginType {
    fn get_props_for_method(
        _ty: TypeDef,
        _method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn into_type(self, _type_def: &TypeDef) -> Result<TypeValue, CompileError>
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

impl From<OriginType> for TypeValue {
    fn from(value: OriginType) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::OriginType(value))
    }
}

impl From<routecore::bgp::types::OriginType> for TypeValue {
    fn from(value: routecore::bgp::types::OriginType) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::OriginType(OriginType(value)))
    }
}

impl From<routecore::bgp::types::OriginType> for BuiltinTypeValue {
    fn from(value: routecore::bgp::types::OriginType) -> Self {
        BuiltinTypeValue::OriginType(OriginType(value))
    }
}

impl Display for OriginType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

//------------ NextHop type -------------------------------------------------

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Serialize)]
pub struct NextHop(pub(crate) routecore::bgp::types::NextHop);

impl RotoType for NextHop {
    fn get_props_for_method(
        _ty: TypeDef,
        _method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
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

impl From<NextHop> for TypeValue {
    fn from(value: NextHop) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::NextHop(value))
    }
}

impl From<routecore::bgp::types::NextHop> for TypeValue {
    fn from(value: routecore::bgp::types::NextHop) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::NextHop(NextHop(value)))
    }
}

impl From<routecore::bgp::types::NextHop> for BuiltinTypeValue {
    fn from(value: routecore::bgp::types::NextHop) -> Self {
        BuiltinTypeValue::NextHop(NextHop(value))
    }
}

impl Display for NextHop {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

//------------ Multi Exit Discriminator type --------------------------------

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Serialize)]
pub struct MultiExitDisc(pub(crate) routecore::bgp::types::MultiExitDisc);

impl RotoType for MultiExitDisc {
    fn get_props_for_method(
        _ty: TypeDef,
        _method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
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

impl From<MultiExitDisc> for TypeValue {
    fn from(value: MultiExitDisc) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::MultiExitDisc(value))
    }
}

impl From<routecore::bgp::types::MultiExitDisc> for TypeValue {
    fn from(value: routecore::bgp::types::MultiExitDisc) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::MultiExitDisc(MultiExitDisc(
            value,
        )))
    }
}

impl From<routecore::bgp::types::MultiExitDisc> for BuiltinTypeValue {
    fn from(value: routecore::bgp::types::MultiExitDisc) -> Self {
        BuiltinTypeValue::MultiExitDisc(MultiExitDisc(value))
    }
}

impl Display for MultiExitDisc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

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
                TypeDef::Boolean,
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

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Serialize)]
pub struct LocalPref(pub(crate) routecore::bgp::types::LocalPref);

impl RotoType for LocalPref {
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
                LocalPrefToken::Set.into(),
                vec![TypeDef::IntegerLiteral],
            )
            .consume_value()),
            _ => Err(format!(
                "Unknown method: '{}' for type LocalPref",
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
        method_token: usize,

        _args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.try_into()? {
            LocalPrefToken::Set => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::LocalPref(*self)))
            } // _ => Err(VmError::InvalidMethodCall),
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
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

impl From<LocalPref> for TypeValue {
    fn from(value: LocalPref) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::LocalPref(value))
    }
}

impl From<routecore::bgp::types::LocalPref> for TypeValue {
    fn from(value: routecore::bgp::types::LocalPref) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::LocalPref(LocalPref(value)))
    }
}

impl From<routecore::bgp::types::LocalPref> for BuiltinTypeValue {
    fn from(value: routecore::bgp::types::LocalPref) -> Self {
        BuiltinTypeValue::LocalPref(LocalPref(value))
    }
}

impl Display for LocalPref {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub enum LocalPrefToken {
    Set,
}

impl TryFrom<usize> for LocalPrefToken {
    type Error = VmError;

    fn try_from(val: usize) -> Result<Self, VmError> {
        match val {
            0 => Ok(LocalPrefToken::Set),
            _ => {
                debug!("Unknown token value: {}", val);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<LocalPrefToken> for usize {
    fn from(val: LocalPrefToken) -> Self {
        match val {
            LocalPrefToken::Set => 0,
        }
    }
}

//------------ AtomicAggregate type -----------------------------------------

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Serialize)]
pub struct AtomicAggregate(
    pub(crate) routecore::bgp::path_attributes::AtomicAggregate,
);

impl RotoType for AtomicAggregate {
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
                AtomicAggregateToken::Set.into(),
                vec![TypeDef::Asn, TypeDef::IpAddress],
            )
            .consume_value()),
            _ => Err(format!(
                "Unknown method: '{}' for type Atomic Aggregator",
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

impl From<AtomicAggregate> for TypeValue {
    fn from(value: AtomicAggregate) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::AtomicAggregate(value))
    }
}

impl From<AtomicAggregate> for BuiltinTypeValue {
    fn from(value: AtomicAggregate) -> Self {
        BuiltinTypeValue::AtomicAggregate(value)
    }
}

impl From<routecore::bgp::path_attributes::AtomicAggregate> for TypeValue {
    fn from(value: routecore::bgp::path_attributes::AtomicAggregate) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::AtomicAggregate(
            AtomicAggregate(value),
        ))
    }
}

impl From<routecore::bgp::path_attributes::AtomicAggregate>
    for BuiltinTypeValue
{
    fn from(value: routecore::bgp::path_attributes::AtomicAggregate) -> Self {
        BuiltinTypeValue::AtomicAggregate(AtomicAggregate(value))
    }
}

impl std::fmt::Display for AtomicAggregate {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

#[derive(Debug)]
pub enum AtomicAggregateToken {
    Set,
}

impl TryFrom<usize> for AtomicAggregateToken {
    type Error = VmError;

    fn try_from(val: usize) -> Result<Self, VmError> {
        match val {
            0 => Ok(AtomicAggregateToken::Set),
            _ => {
                debug!("Unknown token value: {}", val);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<AtomicAggregateToken> for usize {
    fn from(val: AtomicAggregateToken) -> Self {
        match val {
            AtomicAggregateToken::Set => 0,
        }
    }
}

//------------ Aggregator type ----------------------------------------------

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Serialize)]
pub struct Aggregator(
    pub(crate) routecore::bgp::path_attributes::AggregatorInfo,
);

impl RotoType for Aggregator {
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
                AtomicAggregateToken::Set.into(),
                vec![TypeDef::Asn, TypeDef::IpAddress],
            )
            .consume_value()),
            _ => Err(format!(
                "Unknown method: '{}' for type Aggregator",
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

impl From<Aggregator> for TypeValue {
    fn from(value: Aggregator) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Aggregator(value))
    }
}

impl From<Aggregator> for BuiltinTypeValue {
    fn from(value: Aggregator) -> Self {
        BuiltinTypeValue::Aggregator(value)
    }
}

impl From<routecore::bgp::path_attributes::AggregatorInfo> for TypeValue {
    fn from(value: routecore::bgp::path_attributes::AggregatorInfo) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Aggregator(Aggregator(value)))
    }
}

impl From<routecore::bgp::path_attributes::AggregatorInfo>
    for BuiltinTypeValue
{
    fn from(value: routecore::bgp::path_attributes::AggregatorInfo) -> Self {
        BuiltinTypeValue::Aggregator(Aggregator(value))
    }
}

impl std::fmt::Display for Aggregator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

#[derive(Debug)]
pub enum AggregatorToken {
    Set,
}

impl TryFrom<usize> for AggregatorToken {
    type Error = VmError;

    fn try_from(val: usize) -> Result<Self, VmError> {
        match val {
            0 => Ok(AggregatorToken::Set),
            _ => {
                debug!("Unknown token value: {}", val);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<AggregatorToken> for usize {
    fn from(val: AggregatorToken) -> Self {
        match val {
            AggregatorToken::Set => 0,
        }
    }
}

//------------ RouteStatus type ---------------------------------------------

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
pub enum RouteStatus {
    // Between start and EOR on a BGP peer-session
    InConvergence,
    // After EOR for a BGP peer-session, either `Graceful Restart` or EOR
    UpToDate,
    // After hold-timer expiry
    Stale,
    // After the request for a Route Refresh to a peer and the reception of a
    // new route
    StartOfRouteRefresh,
    // After the reception of a withdrawal
    Withdrawn,
    // A routecore::bgp::ParseError happened on a part of the PDU!
    Unparsable,
    // Status not relevant, e.g. a RIB that holds archived routes.
    #[default]
    Empty,
}

impl std::fmt::Display for RouteStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RouteStatus::InConvergence => write!(f, "in convergence"),
            RouteStatus::UpToDate => write!(f, "up to date"),
            RouteStatus::Stale => write!(f, "stale"),
            RouteStatus::StartOfRouteRefresh => {
                write!(f, "start of route refresh")
            }
            RouteStatus::Withdrawn => write!(f, "withdrawn"),
            RouteStatus::Unparsable => write!(f, "UNPARSABLE"),
            RouteStatus::Empty => write!(f, "empty"),
        }
    }
}

impl TryFrom<TypeValue> for RouteStatus {
    type Error = VmError;

    fn try_from(value: TypeValue) -> Result<Self, VmError> {
        if let TypeValue::Builtin(BuiltinTypeValue::RouteStatus(value)) =
            value
        {
            Ok(value)
        } else {
            debug!("invalid something");
            Err(VmError::InvalidMethodCall)
        }
    }
}
