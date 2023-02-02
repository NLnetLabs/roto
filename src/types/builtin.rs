//------------ BuiltinTypeValue -------------------------------------------

// The built-in types

use std::convert::Infallible;
use std::fmt::{Display, Formatter};

use routecore::asn::LongSegmentError;

use crate::ast::ShortString;
use crate::compile::CompileError;
use crate::traits::{MethodProps, RotoFilter, TokenConvert};
use crate::vm::{Payload, VmError};

use super::collections::Record;
use super::typedef::TypeDef;
use super::typevalue::TypeValue;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum BuiltinTypeValue {
    U32(U32),
    U8(U8),
    IntegerLiteral(IntegerLiteral),
    StringLiteral(StringLiteral),
    Prefix(Prefix),
    PrefixLength(PrefixLength),
    Community(Community),
    IpAddress(IpAddress),
    Asn(Asn),
    AsPath(AsPath),
    Route(Route),
    RouteStatus(RouteStatus),
    Boolean(Boolean),
    HexLiteral(HexLiteral),
}

impl BuiltinTypeValue {
    // pub fn get_type_name(&self) -> &'static str {
    //     match self {
    //         BuiltinTypeValue::U32(_) => "U32",
    //         BuiltinTypeValue::U8(_) => "U8",
    //         BuiltinTypeValue::IntegerLiteral(_) => "IntegerLiteral",
    //         BuiltinTypeValue::StringLiteral(_) => "StringLiteral",
    //         BuiltinTypeValue::Boolean(_) => "Boolean",
    //         BuiltinTypeValue::Prefix(_) => "Prefix",
    //         BuiltinTypeValue::PrefixLength(_) => "PrefixLengthLiteral",
    //         BuiltinTypeValue::Community(_) => "Community",
    //         BuiltinTypeValue::IpAddress(_) => "IpAddress",
    //         BuiltinTypeValue::Asn(_) => "Asn",
    //         BuiltinTypeValue::AsPath(_) => "AsPath",
    //         BuiltinTypeValue::Route(_) => "Route",
    //         BuiltinTypeValue::RouteStatus(_) => "RouteStatus",
    //         BuiltinTypeValue::HexLiteral(_) => "HexLiteral",
    //     }
    // }

    // pub fn get_value(&self) -> &dyn std::any::Any {
    //     match self {
    //         BuiltinTypeValue::U32(val) => val,
    //         BuiltinTypeValue::U8(val) => val,
    //         BuiltinTypeValue::IntegerLiteral(val) => val,
    //         BuiltinTypeValue::StringLiteral(val) => val,
    //         BuiltinTypeValue::Boolean(val) => val,
    //         BuiltinTypeValue::Prefix(val) => val,
    //         BuiltinTypeValue::PrefixLength(val) => val,
    //         BuiltinTypeValue::Community(val) => val,
    //         BuiltinTypeValue::IpAddress(val) => val,
    //         BuiltinTypeValue::Asn(val) => val,
    //         BuiltinTypeValue::AsPath(val) => val,
    //         BuiltinTypeValue::Route(val) => val,
    //         BuiltinTypeValue::RouteStatus(val) => val,
    //         BuiltinTypeValue::HexLiteral(val) => val,
    //     }
    // }

    // pub fn exists(ty: &'_ str) -> bool {
    //     matches!(
    //         ty,
    //         "U32"
    //             | "U8"
    //             | "IntegerLiteral"
    //             | "Prefix"
    //             | "PrefixLength"
    //             | "Community"
    //             | "IpAddress"
    //             | "Asn"
    //             | "AsPath"
    //             | "Route"
    //             | "RouteStatus"
    //             | "Boolean"
    //             | "String"
    //             | "HexLiteral"
    //     )
    // }

    pub fn create_instance(
        ty: TypeDef,
        value: impl Into<BuiltinTypeValue>,
    ) -> Result<TypeValue, CompileError> {
        let var = match ty {
            TypeDef::U32 => {
                if let BuiltinTypeValue::U32(v) = value.into() {
                    BuiltinTypeValue::U32(v)
                } else {
                    return Err("Not a U32".into());
                }
            }
            TypeDef::U8 => {
                if let BuiltinTypeValue::U8(v) = value.into() {
                    BuiltinTypeValue::U8(v)
                } else {
                    return Err("Not a U8".into());
                }
            }
            TypeDef::IntegerLiteral => {
                if let BuiltinTypeValue::IntegerLiteral(v) = value.into() {
                    BuiltinTypeValue::IntegerLiteral(v)
                } else {
                    return Err("Not an IntegerLiteral".into());
                }
            }
            TypeDef::PrefixLength => {
                if let BuiltinTypeValue::PrefixLength(v) = value.into() {
                    BuiltinTypeValue::PrefixLength(v)
                } else {
                    return Err("Not a PrefixLength".into());
                }
            }
            TypeDef::HexLiteral => {
                if let BuiltinTypeValue::HexLiteral(v) = value.into() {
                    BuiltinTypeValue::HexLiteral(v)
                } else {
                    return Err("Not a HexLiteral".into());
                }
            }
            TypeDef::Prefix => {
                if let BuiltinTypeValue::Prefix(v) = value.into() {
                    BuiltinTypeValue::Prefix(v)
                } else {
                    return Err("Not a Prefix".into());
                }
            }
            TypeDef::IpAddress => {
                if let BuiltinTypeValue::IpAddress(v) = value.into() {
                    BuiltinTypeValue::IpAddress(v)
                } else {
                    return Err("Not an IP address".into());
                }
            }
            TypeDef::Asn => {
                if let BuiltinTypeValue::Asn(v) = value.into() {
                    BuiltinTypeValue::Asn(v)
                } else {
                    return Err("Not an ASN".into());
                }
            }
            TypeDef::AsPath => {
                if let BuiltinTypeValue::AsPath(v) = value.into() {
                    BuiltinTypeValue::AsPath(v)
                } else {
                    return Err("Not an AS Path".into());
                }
            }
            TypeDef::Community => {
                if let BuiltinTypeValue::Community(v) = value.into() {
                    BuiltinTypeValue::Community(v)
                } else {
                    return Err("Not a community".into());
                }
            }
            TypeDef::Route => {
                if let BuiltinTypeValue::Route(v) = value.into() {
                    BuiltinTypeValue::Route(v)
                } else {
                    return Err("Not a route".into());
                }
            }
            TypeDef::RouteStatus => {
                if let BuiltinTypeValue::RouteStatus(v) = value.into() {
                    BuiltinTypeValue::RouteStatus(v)
                } else {
                    return Err("Not a route status".into());
                }
            }
            _ => return Err("Not a primitive type".into()),
        };
        Ok(TypeValue::Builtin(var))
    }

    // pub(crate) fn exec_value_method(
    //     &self,
    //     method_token: usize,
    //     args: &[&TypeValue],
    //     return_type: TypeDef,
    // ) -> TypeValue {
    //     match self {
    //         BuiltinTypeValue::AsPath(as_path) => as_path
    //             .exec_value_method(method_token, args, return_type)
    //             .unwrap()(),
    //         BuiltinTypeValue::Prefix(prefix) => prefix
    //             .exec_value_method(method_token, args, return_type)
    //             .unwrap()(),
    //         BuiltinTypeValue::IntegerLiteral(lit_int) => lit_int
    //             .exec_value_method(method_token, args, return_type)
    //             .unwrap()(
    //         ),
    //         BuiltinTypeValue::StringLiteral(lit_str) => lit_str
    //             .exec_value_method(method_token, args, return_type)
    //             .unwrap()(),
    //         BuiltinTypeValue::U32(u32) => u32
    //             .exec_value_method(method_token, args, return_type)
    //             .unwrap()(),
    //         BuiltinTypeValue::Asn(asn) => asn
    //             .exec_value_method(method_token, args, return_type)
    //             .unwrap()(),
    //         BuiltinTypeValue::IpAddress(ip) => ip
    //             .exec_value_method(method_token, args, return_type)
    //             .unwrap()(),
    //         BuiltinTypeValue::Route(route) => route
    //             .exec_value_method(method_token, args, return_type)
    //             .unwrap()(),
    //         BuiltinTypeValue::U8(_) => todo!(),
    //         BuiltinTypeValue::PrefixLength(_) => todo!(),
    //         BuiltinTypeValue::Community(_) => todo!(),
    //         BuiltinTypeValue::RouteStatus(_) => todo!(),
    //         BuiltinTypeValue::Boolean(_) => todo!(),
    //         BuiltinTypeValue::HexLiteral(_) => todo!(),
    //     }
    // }
}

// These From impls allow the user to use the create_instance function with
// simple types like u32, u8, etc. (without the nested variants).

impl From<Asn> for BuiltinTypeValue {
    fn from(val: Asn) -> Self {
        BuiltinTypeValue::Asn(val)
    }
}

impl From<u32> for BuiltinTypeValue {
    fn from(val: u32) -> Self {
        BuiltinTypeValue::U32(U32(Some(val)))
    }
}

impl From<i64> for BuiltinTypeValue {
    fn from(val: i64) -> Self {
        BuiltinTypeValue::IntegerLiteral(IntegerLiteral(Some(val)))
    }
}

impl From<std::net::IpAddr> for BuiltinTypeValue {
    fn from(val: std::net::IpAddr) -> Self {
        BuiltinTypeValue::IpAddress(IpAddress(Some(val)))
    }
}

impl From<routecore::addr::Prefix> for BuiltinTypeValue {
    fn from(val: routecore::addr::Prefix) -> Self {
        BuiltinTypeValue::Prefix(Prefix(Some(val)))
    }
}

impl TryFrom<&'_ str> for BuiltinTypeValue {
    type Error = CompileError;

    fn try_from(val: &'_ str) -> Result<Self, Self::Error> {
        match val {
            "U32" => Ok(BuiltinTypeValue::U32(U32(None))),
            "U8" => Ok(BuiltinTypeValue::U8(U8(None))),
            "IntegerLiteral" => {
                Ok(BuiltinTypeValue::IntegerLiteral(IntegerLiteral(None)))
            }
            "PrefixLengthLiteral" => {
                Ok(BuiltinTypeValue::PrefixLength(PrefixLength(None)))
            }
            "Boolean" => Ok(BuiltinTypeValue::Boolean(Boolean(None))),
            "Prefix" => Ok(BuiltinTypeValue::Prefix(Prefix(None))),
            "PrefixLength" => {
                Ok(BuiltinTypeValue::PrefixLength(PrefixLength(None)))
            }
            "Community" => Ok(BuiltinTypeValue::Community(Community(None))),
            "IpAddress" => Ok(BuiltinTypeValue::IpAddress(IpAddress(None))),
            "Asn" => Ok(BuiltinTypeValue::Asn(Asn(None))),
            "AsPath" => Ok(BuiltinTypeValue::AsPath(AsPath(None))),
            "Route" => Ok(BuiltinTypeValue::Route(Route {
                prefix: None,
                bgp: None,
                status: RouteStatus::Empty,
            })),
            "RouteStatus" => {
                Ok(BuiltinTypeValue::RouteStatus(RouteStatus::Empty))
            }
            _ => Err(format!("Unknown type: {}", val).into()),
        }
    }
}

impl TryFrom<&TypeDef> for BuiltinTypeValue {
    type Error = CompileError;

    fn try_from(ty: &TypeDef) -> Result<Self, Self::Error> {
        match ty {
            TypeDef::U32 => Ok(BuiltinTypeValue::U32(U32(None))),
            TypeDef::U8 => Ok(BuiltinTypeValue::U8(U8(None))),
            TypeDef::IntegerLiteral => {
                Ok(BuiltinTypeValue::IntegerLiteral(IntegerLiteral(None)))
            }
            TypeDef::Boolean => Ok(BuiltinTypeValue::Boolean(Boolean(None))),
            TypeDef::Prefix => Ok(BuiltinTypeValue::Prefix(Prefix(None))),
            TypeDef::PrefixLength => {
                Ok(BuiltinTypeValue::PrefixLength(PrefixLength(None)))
            }
            TypeDef::Community => {
                Ok(BuiltinTypeValue::Community(Community(None)))
            }
            TypeDef::IpAddress => {
                Ok(BuiltinTypeValue::IpAddress(IpAddress(None)))
            }
            TypeDef::Asn => Ok(BuiltinTypeValue::Asn(Asn(None))),
            TypeDef::AsPath => Ok(BuiltinTypeValue::AsPath(AsPath(None))),
            TypeDef::Route => Ok(BuiltinTypeValue::Route(Route {
                prefix: None,
                bgp: None,
                status: RouteStatus::Empty,
            })),
            TypeDef::RouteStatus => {
                Ok(BuiltinTypeValue::RouteStatus(RouteStatus::Empty))
            }
            _ => Err(format!("Unknown type: {:?}", ty).into()),
        }
    }
}

impl Display for BuiltinTypeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            BuiltinTypeValue::U32(v) => write!(f, "{} (U32)", v),
            BuiltinTypeValue::U8(v) => write!(f, "{} (U8)", v),
            BuiltinTypeValue::IntegerLiteral(v) => {
                write!(f, "{} (Integer)", v)
            }
            BuiltinTypeValue::StringLiteral(v) => {
                write!(f, "{} (String)", v)
            }
            BuiltinTypeValue::Prefix(v) => write!(f, "{} (Prefix)", v),
            BuiltinTypeValue::PrefixLength(v) => {
                write!(f, "{} (Prefix Length)", v)
            }
            BuiltinTypeValue::Community(v) => write!(f, "{} (Community)", v),
            BuiltinTypeValue::IpAddress(v) => write!(f, "{} (IP Address)", v),
            BuiltinTypeValue::Asn(v) => write!(f, "{} (ASN)", v),
            BuiltinTypeValue::AsPath(v) => {
                write!(f, "{} (AS Path)", v)
            }
            BuiltinTypeValue::Route(v) => write!(f, "{} (Route)", v),
            BuiltinTypeValue::RouteStatus(v) => {
                write!(f, "{} (Route Status)", v)
            }
            BuiltinTypeValue::Boolean(v) => write!(f, "{} (Boolean)", v),
            BuiltinTypeValue::HexLiteral(v) => {
                write!(f, "{} (Hex)", v)
            }
        }
    }
}

// ----------- A simple u32 type --------------------------------------------

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct U32(pub(crate) Option<u32>);

impl U32 {
    pub fn new(val: u32) -> Self {
        U32(Some(val))
    }
}

impl From<U32> for TypeValue {
    fn from(val: U32) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::U32(val))
    }
}

impl RotoFilter for U32 {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "set" => Ok(MethodProps::new(
                TypeValue::Unknown,
                U32Token::Set.into(),
                vec![TypeDef::IntegerLiteral],
            ).consume_value()),
            _ => Err(format!(
                "Unknown method: '{}' for type U32",
                method_name.ident
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &[&TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::U32 => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::U32(self)))
            }
            _ => {
                Err(format!("Cannot convert type U32 to type {:?}", type_def)
                    .into())
            }
        }
    }
}

impl Display for U32 {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let v = match self.0 {
            Some(v) => v.to_string(),
            None => "None".to_string(),
        };
        write!(f, "{}", v)
    }
}

#[derive(Debug)]
pub enum U32Token {
    Set,
}

impl TokenConvert for U32Token {}

impl From<usize> for U32Token {
    fn from(val: usize) -> Self {
        match val {
            0 => U32Token::Set,
            _ => panic!("Unknown token value: {}", val),
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

// ----------- A simple u8 type ---------------------------------------------

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct U8(pub(crate) Option<u8>);

impl U8 {
    pub fn new(val: u8) -> Self {
        U8(Some(val))
    }
}

impl RotoFilter for U8 {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "set" => Ok(MethodProps::new(
                TypeValue::Unknown,
                U8Token::Set.into(),
                vec![TypeDef::IntegerLiteral],
            ).consume_value()),
            _ => Err(format!(
                "Unknown method: '{}' for type U8",
                method_name.ident
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &[&TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            // Self
            TypeDef::U8 => Ok(TypeValue::Builtin(BuiltinTypeValue::U8(self))),
            TypeDef::U32 => match self.0 {
                Some(value) => Ok(TypeValue::Builtin(BuiltinTypeValue::U32(
                    U32(Some(value as u32)),
                ))),
                None => Err("Cannot convert None to U32".into()),
            },
            TypeDef::PrefixLength => match self.0 {
                Some(value) => match value {
                    0..=128 => Ok(TypeValue::Builtin(
                        BuiltinTypeValue::PrefixLength(PrefixLength(Some(
                            value,
                        ))),
                    )),
                    _ => Err(format!(
                        "Prefix length must be between 0 and 128, not {}",
                        value
                    )
                    .into()),
                },
                None => Ok(TypeValue::Builtin(
                    BuiltinTypeValue::PrefixLength(PrefixLength(None)),
                )),
            },
            TypeDef::IntegerLiteral => match self.0 {
                Some(value) => match value {
                    0..=128 => Ok(TypeValue::Builtin(
                        BuiltinTypeValue::PrefixLength(PrefixLength(Some(
                            value,
                        ))),
                    )),
                    _ => Err(format!(
                        "Prefix length must be between 0 and 128, not {}",
                        value
                    )
                    .into()),
                },
                None => Ok(TypeValue::Builtin(
                    BuiltinTypeValue::PrefixLength(PrefixLength(None)),
                )),
            },
            _ => {
                Err(format!("Cannot convert type U8 to type {:?}", type_def)
                    .into())
            }
        }
    }
}

impl Display for U8 {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let v = match self.0 {
            Some(v) => v.to_string(),
            None => "None".to_string(),
        };
        write!(f, "{}", v)
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

impl TokenConvert for U8Token {}

impl From<usize> for U8Token {
    fn from(val: usize) -> Self {
        match val {
            0 => U8Token::Set,
            _ => panic!("Unknown token value: {}", val),
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

// ----------- Boolean type -------------------------------------------------

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct Boolean(pub(crate) Option<bool>);
impl Boolean {
    pub fn new(val: bool) -> Self {
        Boolean(Some(val))
    }

    pub fn is_false(&self) -> Result<bool, VmError> {
        if let Boolean(Some(bool_val)) = self {
            Ok(!*bool_val)
        } else {
            Err(VmError::InvalidValueType)
        }
    }
}

impl RotoFilter for Boolean {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "set" => Ok(MethodProps::new(
                TypeValue::Unknown,
                BooleanToken::Set.into(),
                vec![TypeDef::Boolean],
            ).consume_value()),
            _ => Err(format!(
                "Unknown method: '{}' for type Boolean",
                method_name.ident
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &[&TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::Boolean => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::Boolean(self)))
            }
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

impl Display for Boolean {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let v = match self.0 {
            Some(v) => v.to_string(),
            None => "None".to_string(),
        };
        write!(f, "{}", v)
    }
}

#[derive(Debug)]
pub enum BooleanToken {
    Set,
}

impl TokenConvert for BooleanToken {}

impl From<usize> for BooleanToken {
    fn from(val: usize) -> Self {
        match val {
            0 => BooleanToken::Set,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<BooleanToken> for usize {
    fn from(val: BooleanToken) -> Self {
        val as usize
    }
}

//------------ StringLiteral type -------------------------------------------

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StringLiteral(pub(crate) Option<ShortString>);
impl StringLiteral {
    pub fn new(val: ShortString) -> Self {
        StringLiteral(Some(val))
    }
}

impl RotoFilter for StringLiteral {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError> {
        match method_name.ident.as_str() {
            "cmp" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::IntegerLiteral),
                StringLiteralToken::Cmp.into(),
                vec![TypeDef::StringLiteral, TypeDef::StringLiteral],
            )),
            _ => Err(format!(
                "Unknown method: '{}' for type StringLiteral",
                method_name.ident
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &[&TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

     fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::StringLiteral => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::StringLiteral(self)))
            }
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

impl Display for StringLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let v = match &self.0 {
            Some(v) => v.to_string(),
            None => "None".to_string(),
        };
        write!(f, "{}", v)
    }
}

#[derive(Debug)]
pub enum StringLiteralToken {
    Cmp,
}

impl TokenConvert for StringLiteralToken {}

impl From<usize> for StringLiteralToken {
    fn from(val: usize) -> Self {
        match val {
            0 => StringLiteralToken::Cmp,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<StringLiteralToken> for usize {
    fn from(val: StringLiteralToken) -> Self {
        val as usize
    }
}

//------------ IntegerLiteral type ------------------------------------------

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct IntegerLiteral(pub(crate) Option<i64>);
impl IntegerLiteral {
    pub fn new(val: i64) -> Self {
        IntegerLiteral(Some(val))
    }
}

impl RotoFilter for IntegerLiteral {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError> {
        match method_name.ident.as_str() {
            "cmp" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::IntegerLiteral),
                IntegerLiteralToken::Cmp.into(),
                vec![TypeDef::IntegerLiteral, TypeDef::IntegerLiteral],
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
            TypeDef::IntegerLiteral => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(self)))
            }
            TypeDef::PrefixLength => match self.0 {
                Some(value) => match value {
                    0..=128 => Ok(TypeValue::Builtin(
                        BuiltinTypeValue::PrefixLength(PrefixLength(Some(
                            value as u8,
                        ))),
                    )),
                    _ => Err(format!(
                        "Prefix length must be between 0 and 128, not {}",
                        value
                    )
                    .into()),
                },
                None => Ok(TypeValue::Builtin(
                    BuiltinTypeValue::PrefixLength(PrefixLength(None)),
                )),
            },
            _ => Err(format!(
                "Cannot convert type IntegerLiteral to type {:?}",
                type_def
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        args: &[&TypeValue],
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

     fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        method_token: usize,
        args: &[&'a TypeValue],
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }
}

impl From<IntegerLiteral> for TypeValue {
    fn from(val: IntegerLiteral) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(val))
    }
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let v = match self.0 {
            Some(v) => v.to_string(),
            None => "None".to_string(),
        };
        write!(f, "{}", v)
    }
}

#[derive(Debug)]
pub(crate) enum IntegerLiteralToken {
    Cmp,
}

impl TokenConvert for IntegerLiteralToken {}

impl From<usize> for IntegerLiteralToken {
    fn from(val: usize) -> Self {
        match val {
            0 => IntegerLiteralToken::Cmp,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<IntegerLiteralToken> for usize {
    fn from(val: IntegerLiteralToken) -> Self {
        val as usize
    }
}

//------------ HexLiteral type ----------------------------------------------

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct HexLiteral(pub(crate) Option<u64>);
impl HexLiteral {
    pub fn new(val: u64) -> Self {
        HexLiteral(Some(val))
    }
}

impl RotoFilter for HexLiteral {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError> {
        match method_name.ident.as_str() {
            "cmp" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::IntegerLiteral),
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
                // still bogus, but at least it should convert from the hexliteral type.
                let c = Community::new(CommunityType::Standard);
                Ok(TypeValue::Builtin(BuiltinTypeValue::Community(c)))
            }
            _ => Err(format!(
                "Cannot convert type HexLiteral to type {:?}",
                type_def
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        args: &[&TypeValue],
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

     fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        method_token: usize,
        args: &[&'a TypeValue],
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
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
        let v = match self.0 {
            Some(v) => format!("0x{:x}", v),
            None => "None".to_string(),
        };
        write!(f, "{}", v)
    }
}

#[derive(Debug)]
pub(crate) enum HexLiteralToken {
    Cmp,
}

impl TokenConvert for HexLiteralToken {}

impl From<usize> for HexLiteralToken {
    fn from(val: usize) -> Self {
        match val {
            0 => HexLiteralToken::Cmp,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<HexLiteralToken> for usize {
    fn from(val: HexLiteralToken) -> Self {
        val as usize
    }
}

// ----------- Prefix type --------------------------------------------------

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct Prefix(pub(crate) Option<routecore::addr::Prefix>);

impl Prefix {
    pub fn new(prefix: routecore::addr::Prefix) -> Self {
        Self(Some(prefix))
    }

    pub fn empty() -> Self {
        Self(None)
    }

    pub fn exec_method(
        &self,
        _method: usize,
        _args: Vec<&TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce(TypeValue) -> TypeValue + '_>, CompileError>
    {
        todo!()
    }
}

impl RotoFilter for Prefix {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "from" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::Prefix),
                PrefixToken::From.into(),
                vec![TypeDef::IpAddress, TypeDef::PrefixLength],
            )),
            "address" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::IpAddress),
                PrefixToken::Address.into(),
                vec![],
            )),
            "len" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::PrefixLength),
                PrefixToken::Len.into(),
                vec![],
            )),
            "matches" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::Boolean),
                PrefixToken::Matches.into(),
                vec![TypeDef::Prefix],
            )),
            "exists" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(None))),
                PrefixToken::Exists.into(),
                vec![],
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

    fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        args: &[&'a TypeValue],
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        match method_token.into() {
            PrefixToken::Address => {
                let prefix = self.0.ok_or(VmError::InvalidConversion)?;
                Ok(Box::new(move || {
                    TypeValue::Builtin(BuiltinTypeValue::IpAddress(
                        IpAddress(Some(prefix.addr())),
                    ))
                }))
            }
            PrefixToken::Len => {
                if let Prefix(
                    Some(pfx),
                ) = self
                {
                    Ok(Box::new(move || {
                        TypeValue::Builtin(BuiltinTypeValue::PrefixLength(
                            PrefixLength(Some(pfx.len())),
                        ))
                    }))
                } else {
                    Err(VmError::ArgumentNotFound)
                }
            }
            PrefixToken::From => unimplemented!(),
            PrefixToken::Exists => {
                if self.0.is_some() {
                    Ok(Box::new(move || true.into()))
                } else {
                    Ok(Box::new(move || false.into()))
                }
            },
            PrefixToken::Matches => todo!(),
        }
    }

     fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        method_token: usize,
        args: &[&'a TypeValue],
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        match method_token.into() {
            PrefixToken::From => {
                if let TypeValue::Builtin(BuiltinTypeValue::IpAddress(ip)) =
                    args[0]
                {
                    let len: PrefixLength = args[1].try_into().map_err(|_e| VmError::InvalidConversion)?;
                    let ip = ip.0.ok_or("Cannot convert empty IP address").map_err(|_e| VmError::InvalidConversion)?;
                    Ok(Box::new(move || {
                        TypeValue::Builtin(BuiltinTypeValue::Prefix(
                            Prefix::new(
                                routecore::addr::Prefix::new(ip, len.into())
                                    .map_err(|e| {
                                        format!("Invalid prefix: {}", e)
                                    })
                                    .unwrap(),
                            ),
                        ))
                    }))
                } else {
                    Err(VmError::ArgumentNotFound)
                }
            }
            PrefixToken::Exists => unimplemented!(),
            PrefixToken::Address => unimplemented!(),
            PrefixToken::Len => unimplemented!(),
            PrefixToken::Matches => unimplemented!(),
            // _ => {
            //     Err(VmError::InvalidMethodCall)
            // }
        }
    }
}

impl From<Prefix> for TypeValue {
    fn from(val: Prefix) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Prefix(val))
    }
}

impl From<routecore::addr::Prefix> for Prefix {
    fn from(val: routecore::addr::Prefix) -> Self {
        Prefix(Some(val))
    }
}

impl Display for Prefix {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(prefix) = &self.0 {
            write!(f, "{}", prefix)
        } else {
            write!(f, "empty")
        }
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
}

impl TokenConvert for PrefixToken {}

impl From<usize> for PrefixToken {
    fn from(val: usize) -> Self {
        match val {
            0 => PrefixToken::From,
            1 => PrefixToken::Exists,
            2 => PrefixToken::Address,
            3 => PrefixToken::Len,
            4 => PrefixToken::Matches,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<PrefixToken> for usize {
    fn from(val: PrefixToken) -> Self {
        val as usize
    }
}

//------------ PrefixLengthLiteral type -------------------------------------

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct PrefixLength(pub(crate) Option<u8>);

impl PrefixLength {
    pub fn new(val: u8) -> Self {
        PrefixLength(Some(val))
    }
}

impl RotoFilter for PrefixLength {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "from" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::PrefixLength),
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
            _ => Err(format!(
                "Cannot convert type PrefixLength to type {:?}",
                type_def
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        args: &[&TypeValue],
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

     fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        method_token: usize,
        args: &[&'a TypeValue],
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
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
        if let Some(len) = &self.0 {
            write!(f, "/{}", len)
        } else {
            write!(f, "None")
        }
    }
}

#[derive(Debug)]
pub(crate) enum PrefixLengthToken {
    From,
}

impl TokenConvert for PrefixLengthToken {}

impl From<usize> for PrefixLengthToken {
    fn from(val: usize) -> Self {
        match val {
            0 => PrefixLengthToken::From,
            _ => panic!("Unknown token value: {}", val),
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

impl From<PrefixLength> for u8 {
    fn from(val: PrefixLength) -> Self {
        val.0.unwrap()
    }
}

// ----------- Community ----------------------------------------------------

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum CommunityType {
    Standard,
    Extended,
    Large,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct Community(pub(crate) Option<CommunityType>);

impl Community {
    pub fn new(community_type: CommunityType) -> Self {
        Self(Some(community_type))
    }
}

impl RotoFilter for Community {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "from" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::Community),
                CommunityToken::From.into(),
                vec![TypeDef::U32],
            )),
            "standard" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::Community),
                CommunityToken::Standard.into(),
                vec![TypeDef::U32],
            )),
            "extended" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::Community),
                CommunityToken::Extended.into(),
                vec![TypeDef::U32],
            )),
            "large" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::Community),
                CommunityToken::Large.into(),
                vec![TypeDef::U32],
            )),
            "as" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::U32),
                CommunityToken::As.into(),
                vec![],
            )),
            "value" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::U32),
                CommunityToken::Value.into(),
                vec![],
            )),
            "exists" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::Boolean),
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
            _ => Err(format!(
                "Cannot convert type Community to type {:?}",
                type_def
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &[&TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

     fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }
}

impl From<Community> for TypeValue {
    fn from(val: Community) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Community(val))
    }
}

impl Display for Community {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
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
}

impl TokenConvert for CommunityToken {}

impl From<usize> for CommunityToken {
    fn from(val: usize) -> Self {
        match val {
            0 => CommunityToken::From,
            1 => CommunityToken::Standard,
            2 => CommunityToken::Extended,
            3 => CommunityToken::Large,
            4 => CommunityToken::As,
            5 => CommunityToken::Value,
            6 => CommunityToken::Exists,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<CommunityToken> for usize {
    fn from(val: CommunityToken) -> Self {
        val as usize
    }
}

// ----------- PrefixRecord -------------------------------------------------

#[derive(Debug, PartialEq)]
pub struct PrefixRecord {
    pub prefix: routecore::addr::Prefix,
    pub matches: bool,
    pub match_type: MatchType,
    pub record: Record,
}

//------------ MatchType ----------------------------------------------------

#[derive(Debug, Eq, PartialEq)]
pub enum MatchType {
    ExactMatch,
    LongestMatch,
    EmptyMatch,
}

// ----------- IpAddress type -----------------------------------------------

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct IpAddress(pub(crate) Option<std::net::IpAddr>);

impl IpAddress {
    pub fn new(addr: std::net::IpAddr) -> Self {
        IpAddress(Some(addr))
    }
}

impl RotoFilter for IpAddress {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "from" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::IpAddress),
                IpAddressToken::From.into(),
                vec![TypeDef::String],
            )),
            "matches" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::Boolean),
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

    fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        args: &[&TypeValue],
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!();
        // match method_token.into() {
        //     IpAddressToken::From => {
        //         let addr_string: &str = args[0].try_into()?;
        //         let addr = addr_string.parse::<std::net::IpAddr>()?;
        //         Ok(Box::new(move |_| TypeValue::from(IpAddress::new(addr))))
        //     }
        //     _ => {
        //         Err(format!("Unknown method token: {}", method_token).into())
        //     }
        // }
    }

     fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        method_token: usize,
        args: &[&'a TypeValue],
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }
}

impl From<IpAddress> for TypeValue {
    fn from(val: IpAddress) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::IpAddress(val))
    }
}

impl Display for IpAddress {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(addr) = self.0 {
            write!(f, "{}", addr)
        } else {
            write!(f, "None")
        }
    }
}

#[derive(Debug)]
pub(crate) enum IpAddressToken {
    From,
    Matches,
}

impl TokenConvert for IpAddressToken {}

impl From<usize> for IpAddressToken {
    fn from(val: usize) -> Self {
        match val {
            0 => IpAddressToken::From,
            1 => IpAddressToken::Matches,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<IpAddressToken> for usize {
    fn from(val: IpAddressToken) -> Self {
        val as usize
    }
}

// ----------- Asn type -----------------------------------------------------

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct Asn(pub(crate) Option<routecore::asn::Asn>);

impl Asn {
    pub fn new(asn: routecore::asn::Asn) -> Self {
        Asn(Some(asn))
    }

    pub fn from_u32(asn: u32) -> Self {
        Asn(Some(routecore::asn::Asn::from(asn)))
    }

    pub fn get_asn(&self) -> routecore::asn::Asn {
        self.0.unwrap()
    }
}

impl RotoFilter for Asn {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "set" => Ok(MethodProps::new(
                TypeValue::Unknown,
                AsnToken::Set.into(),
                vec![TypeDef::Asn],
            ).consume_value()),
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
            _ => {
                Err(format!("Cannot convert type Asn to type {:?}", type_def)
                    .into())
            }
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        args: &'a [&'a TypeValue],
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        match method_token.into() {
            AsnToken::Set => {
                if let TypeValue::Builtin(BuiltinTypeValue::Asn(asn)) =
                    args[0]
                {
                    Ok(Box::new(move || {
                        TypeValue::from(Asn::new(asn.0.unwrap()))
                    }))
                } else {
                    Err(VmError::ArgumentNotFound)
                }
            }
        }
    }

     fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        method_token: usize,
        args: &[&'a TypeValue],
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }
}

impl From<Asn> for TypeValue {
    fn from(val: Asn) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Asn(val))
    }
}

impl Display for Asn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(asn) = self.0 {
            write!(f, "{}", asn)
        } else {
            write!(f, "None")
        }
    }
}

#[derive(Debug)]
pub enum AsnToken {
    Set,
}

impl TokenConvert for AsnToken {}

impl From<usize> for AsnToken {
    fn from(val: usize) -> Self {
        match val {
            0 => AsnToken::Set,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<AsnToken> for usize {
    fn from(val: AsnToken) -> Self {
        val as usize
    }
}

// ----------- AsPath type --------------------------------------------------

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct AsPath(
    pub(crate) Option<routecore::asn::AsPath<Vec<routecore::asn::Asn>>>,
);

impl AsPath {
    pub fn new(
        as_path: Vec<routecore::asn::Asn>,
    ) -> Result<Self, LongSegmentError> {
        let mut new_as_path = routecore::asn::AsPathBuilder::new();
        for asn in as_path {
            new_as_path.push(asn)?;
        }
        let new_as_path = new_as_path.finalize();
        Ok(AsPath(Some(new_as_path)))
    }

    pub fn from_vec_u32(as_path: Vec<u32>) -> Result<Self, LongSegmentError> {
        let as_path = as_path
            .into_iter()
            .map(routecore::asn::Asn::from_u32)
            .collect();
        AsPath::new(as_path)
    }

    pub fn contains(&self, asn: routecore::asn::Asn) -> bool {
        if let Some(as_path) = &self.0 {
            as_path.iter().any(|a| a.elements().contains(&asn))
        } else {
            false
        }
    }

    fn inner_from_typevalue(
        type_value: TypeValue,
    ) -> Result<routecore::asn::AsPath<Vec<routecore::asn::Asn>>, CompileError>
    where
        Self: std::marker::Sized,
    {
        match type_value {
            TypeValue::Builtin(BuiltinTypeValue::AsPath(as_path)) => {
                if let Some(as_path) = as_path.0 {
                    Ok(as_path)
                } else {
                    Err("Invalid AsPath".into())
                }
            }
            _ => Err("Not an AsPath type".into()),
        }
    }
}

impl RotoFilter for AsPath {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "origin" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(None))),
                AsPathToken::Origin.into(),
                vec![],
            )),
            "contains" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(None))),
                AsPathToken::Contains.into(),
                vec![TypeDef::Asn],
            )),
            "len" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::U8(U8(None))),
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
        args: &'a [&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<(dyn FnOnce() -> TypeValue + 'a)>, VmError> {
        match method.into() {
            AsPathToken::Origin => {
                if let Some(rc_as_path) = &self.0 {
                    Ok(Box::new(move || {
                        let origin: routecore::asn::Asn =
                            rc_as_path.iter().next().unwrap().elements()[0];

                        TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(Some(
                            origin,
                        ))))
                    }))
                } else {
                    Ok(Box::new(move || {
                        TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(None)))
                    }))
                }
            }
            AsPathToken::Contains => {
                if let Some(rc_as_path) = &self.0 {
                    {
                        Ok(Box::new(move || {
                            if let TypeValue::Builtin(
                                BuiltinTypeValue::Asn(Asn(search_asn)),
                            ) = args[0]
                            {
                                let contains =
                                    rc_as_path.contains(search_asn.unwrap());
                                TypeValue::Builtin(BuiltinTypeValue::Boolean(
                                    Boolean(Some(contains)),
                                ))
                            } else {
                                TypeValue::Unknown
                            }
                        }))
                    }
                } else {
                    Ok(Box::new(|| {
                        TypeValue::Unknown
                    }))
                }
            }
            AsPathToken::Len => {
                if let Some(rc_as_path) = &self.0 {
                    Ok(Box::new(|| {
                        let len = rc_as_path.iter().count();
                        TypeValue::Builtin(BuiltinTypeValue::U8(U8(Some(
                            len as u8,
                        ))))
                    }))
                } else {
                    Ok(Box::new(|| {
                        TypeValue::Builtin(BuiltinTypeValue::U8(U8(None)))
                    }))
                }
            }
        }
    }

     fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        method_token: usize,
        args: &[&'a TypeValue],
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }
}

impl From<AsPath> for TypeValue {
    fn from(val: AsPath) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::AsPath(val))
    }
}

impl Display for AsPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(rc_as_path) = &self.0 {
            write!(f, "{}", rc_as_path)
        } else {
            write!(f, "None")
        }
    }
}

#[repr(u8)]
#[derive(Debug)]
pub(crate) enum AsPathToken {
    Origin = 1,
    Contains = 2,
    Len = 3,
}

impl TokenConvert for AsPathToken {}

impl From<usize> for AsPathToken {
    fn from(value: usize) -> Self {
        match value {
            1 => AsPathToken::Origin,
            2 => AsPathToken::Contains,
            3 => AsPathToken::Len,
            _ => panic!("Unknown AsPathToken value: {}", value),
        }
    }
}

impl From<AsPathToken> for usize {
    fn from(val: AsPathToken) -> Self {
        val as usize
    }
}

//------------ Route type ---------------------------------------------------

// Generic RFC4271 Route type, that can be parsed with routecore.

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Route {
    pub prefix: Option<Prefix>,
    pub bgp: Option<BgpAttributes>,
    pub status: RouteStatus,
}

impl RotoFilter for Route {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "prefix" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Prefix(Prefix(None))),
                RouteToken::Prefix.into(),
                vec![],
            )),
            "as_path" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::AsPath(AsPath(None))),
                RouteToken::AsPath.into(),
                vec![],
            )),
            "communities" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Community(Community(
                    None,
                ))),
                RouteToken::Communities.into(),
                vec![],
            )),
            "status" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::RouteStatus(
                    RouteStatus::Empty,
                )),
                RouteToken::Status.into(),
                vec![],
            )),
            _ => Err(format!(
                "Unknown method '{}' for type Route",
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
            TypeDef::Route => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::Route(self)))
            }
            _ => Err(format!(
                "Cannot convert type Route to type {:?}",
                type_def
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        _method: usize,
        _args: &[&TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<(dyn FnOnce() -> TypeValue + 'a)>, VmError> {
        todo!()
    }

     fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        method_token: usize,
        args: &[&'a TypeValue],
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }
}

impl From<Route> for TypeValue {
    fn from(val: Route) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Route(val))
    }
}

impl Display for Route {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(prefix) = &self.prefix {
            write!(f, "{}", prefix)?;
        } else {
            write!(f, "None")?;
        }
        if let Some(bgp) = &self.bgp {
            write!(f, " {}", bgp)?;
        }
        write!(f, " {}", self.status)
    }
}

#[derive(Debug)]
pub enum RouteToken {
    Prefix,
    AsPath,
    Communities,
    Status,
}

impl TokenConvert for RouteToken {}

impl From<usize> for RouteToken {
    fn from(value: usize) -> Self {
        match value {
            1 => RouteToken::Prefix,
            2 => RouteToken::AsPath,
            3 => RouteToken::Communities,
            4 => RouteToken::Status,
            _ => panic!("Unknown RouteToken value: {}", value),
        }
    }
}

impl From<RouteToken> for usize {
    fn from(val: RouteToken) -> Self {
        val as usize
    }
}

impl Payload for Route {
    fn set_field(
        &mut self,
        field: crate::ast::ShortString,
        value: TypeValue,
    ) {
        todo!()
    }

    fn get(&self, field: crate::ast::ShortString) -> Option<&TypeValue> {
        todo!()
    }

    fn take_value(self) -> TypeValue {
        TypeValue::Builtin(BuiltinTypeValue::Route(self))
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum RouteStatus {
    InConvergence, // Between start and EOR on a BGP peer-session
    UpToDate, // After EOR for a BGP peer-session, either Graceful Restart or EOR
    Stale,    // After hold-timer expiry
    StartOfRouteRefresh, // After the request for a Route Refresh to a peer and the reception of a new route
    Withdrawn,           // After the reception of a withdrawal
    Empty, // Status not relevant, e.g. a RIB that holds archived routes.
}

impl RotoFilter for RouteStatus {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "is_in_convergence" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(None))),
                RouteStatusToken::IsInConvergence.into(),
                vec![],
            )),
            "is_up_to_date" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(None))),
                RouteStatusToken::IsUpToDate.into(),
                vec![],
            )),
            "is_stale" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(None))),
                RouteStatusToken::IsStale.into(),
                vec![],
            )),
            "is_start_of_route_refresh" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(None))),
                RouteStatusToken::IsStartOfRouteRefresh.into(),
                vec![],
            )),
            "is_withdrawn" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(None))),
                RouteStatusToken::IsWithdrawn.into(),
                vec![],
            )),
            "is_empty" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(None))),
                RouteStatusToken::IsEmpty.into(),
                vec![],
            )),
            _ => Err(format!(
                "Unknown method '{}' for type RouteStatus",
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
            TypeDef::RouteStatus => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::RouteStatus(self)))
            }
            _ => Err(format!(
                "Cannot convert type RouteStatus to type {:?}",
                type_def
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &[&TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

     fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        method_token: usize,
        args: &[&'a TypeValue],
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }
}

impl From<RouteStatus> for TypeValue {
    fn from(val: RouteStatus) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::RouteStatus(val))
    }
}

impl Display for RouteStatus {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RouteStatus::InConvergence => write!(f, "in convergence"),
            RouteStatus::UpToDate => write!(f, "up to date"),
            RouteStatus::Stale => write!(f, "stale"),
            RouteStatus::StartOfRouteRefresh => {
                write!(f, "start of route refresh")
            }
            RouteStatus::Withdrawn => write!(f, "withdrawn"),
            RouteStatus::Empty => write!(f, "empty"),
        }
    }
}

#[derive(Debug)]
pub enum RouteStatusToken {
    IsInConvergence,
    IsUpToDate,
    IsStale,
    IsStartOfRouteRefresh,
    IsWithdrawn,
    IsEmpty,
}

impl TokenConvert for RouteStatusToken {}

impl From<usize> for RouteStatusToken {
    fn from(value: usize) -> Self {
        match value {
            1 => RouteStatusToken::IsInConvergence,
            2 => RouteStatusToken::IsUpToDate,
            3 => RouteStatusToken::IsStale,
            4 => RouteStatusToken::IsStartOfRouteRefresh,
            5 => RouteStatusToken::IsWithdrawn,
            6 => RouteStatusToken::IsEmpty,
            _ => panic!("Unknown RouteStatusToken value: {}", value),
        }
    }
}

impl From<RouteStatusToken> for usize {
    fn from(val: RouteStatusToken) -> Self {
        val as usize
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct BgpAttributes {
    pub as_path: AsPath,
    pub communities: Vec<Community>,
}

impl Display for BgpAttributes {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "BGP attributes: {:?} {:?}",
            self.as_path, self.communities
        )
    }
}
