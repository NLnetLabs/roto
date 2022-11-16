//------------ BuiltinTypeValue -------------------------------------------

// The built-in types

use routecore::asn::LongSegmentError;

use crate::traits::{MethodProps, RotoFilter, TokenConvert, Token};

use super::collections::Record;
use super::typedef::TypeDef;
use super::typevalue::TypeValue;

#[derive(Debug, Eq, PartialEq)]
pub enum BuiltinTypeValue {
    U32(U32),
    U8(U8),
    IntegerLiteral(IntegerLiteral),
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
    pub fn get_type_name(&self) -> &'static str {
        match self {
            BuiltinTypeValue::U32(_) => "U32",
            BuiltinTypeValue::U8(_) => "U8",
            BuiltinTypeValue::IntegerLiteral(_) => "IntegerLiteral",
            BuiltinTypeValue::Boolean(_) => "Boolean",
            BuiltinTypeValue::Prefix(_) => "Prefix",
            BuiltinTypeValue::PrefixLength(_) => "PrefixLengthLiteral",
            BuiltinTypeValue::Community(_) => "Community",
            BuiltinTypeValue::IpAddress(_) => "IpAddress",
            BuiltinTypeValue::Asn(_) => "Asn",
            BuiltinTypeValue::AsPath(_) => "AsPath",
            BuiltinTypeValue::Route(_) => "Route",
            BuiltinTypeValue::RouteStatus(_) => "RouteStatus",
            BuiltinTypeValue::HexLiteral(_) => "HexLiteral",
        }
    }

    pub fn get_value(&self) -> &dyn std::any::Any {
        match self {
            BuiltinTypeValue::U32(val) => val,
            BuiltinTypeValue::U8(val) => val,
            BuiltinTypeValue::IntegerLiteral(val) => val,
            BuiltinTypeValue::Boolean(val) => val,
            BuiltinTypeValue::Prefix(val) => val,
            BuiltinTypeValue::PrefixLength(val) => val,
            BuiltinTypeValue::Community(val) => val,
            BuiltinTypeValue::IpAddress(val) => val,
            BuiltinTypeValue::Asn(val) => val,
            BuiltinTypeValue::AsPath(val) => val,
            BuiltinTypeValue::Route(val) => val,
            BuiltinTypeValue::RouteStatus(val) => val,
            BuiltinTypeValue::HexLiteral(val) => val,
        }
    }

    pub fn exists(ty: &'_ str) -> bool {
        matches!(
            ty,
            "U32"
                | "U8"
                | "IntegerLiteral"
                | "Prefix"
                | "PrefixLength"
                | "Community"
                | "IpAddress"
                | "Asn"
                | "AsPath"
                | "Route"
                | "RouteStatus"
                | "Boolean"
                | "String"
                | "HexLiteral"
        )
    }

    pub fn create_instance(
        ty: TypeDef,
        value: impl Into<BuiltinTypeValue>,
    ) -> Result<TypeValue, Box<dyn std::error::Error>> {
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
        BuiltinTypeValue::IntegerLiteral(IntegerLiteral(Some(val as i64)))
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
    type Error = Box<dyn std::error::Error>;

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
            "RouteStatus" => Ok(BuiltinTypeValue::RouteStatus(RouteStatus::Empty)),
            _ => Err(format!("Unknown type: {}", val).into()),
        }
    }
}

impl TryFrom<&TypeDef> for BuiltinTypeValue {
    type Error = Box<dyn std::error::Error>;

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

impl std::fmt::Display for BuiltinTypeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            BuiltinTypeValue::U32(_) => write!(f, "unsigned 32-bits integer"),
            BuiltinTypeValue::U8(_) => write!(f, "unsigned 8-bits integer"),
            BuiltinTypeValue::IntegerLiteral(_) => {
                write!(f, "host-sized unsigned integer")
            }
            BuiltinTypeValue::Prefix(_) => write!(f, "Prefix"),
            BuiltinTypeValue::PrefixLength(_) => write!(f, "Prefix length"),
            BuiltinTypeValue::Community(_) => write!(f, "Community"),
            BuiltinTypeValue::IpAddress(_) => write!(f, "IP Address"),
            BuiltinTypeValue::Asn(_) => write!(f, "Autonomous System Number"),
            BuiltinTypeValue::AsPath(_) => {
                write!(f, "AsPath (BGP AS_PATH attribute)")
            }
            BuiltinTypeValue::Route(_) => write!(f, "Route (BGP Route)"),
            BuiltinTypeValue::RouteStatus(_) => write!(f, "BGP Route status"),
            BuiltinTypeValue::Boolean(_) => write!(f, "Boolean"),
            BuiltinTypeValue::HexLiteral(_) => {
                write!(f, "Hexadecimal literal")
            }
        }
    }
}

// ----------- A simple u32 type --------------------------------------------

#[derive(Debug, Eq, PartialEq)]
pub struct U32(pub(crate) Option<u32>);

impl U32 {
    pub fn new(val: u32) -> Self {
        U32(Some(val))
    }
}

impl RotoFilter<U32Token> for U32 {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "set" => Ok(MethodProps::new(
                TypeValue::None,
                U32Token::Set.into_u8(),
                vec![TypeDef::IntegerLiteral])),
            _ => Err(format!(
                "Unknown method: '{}' for type U32",
                method_name.ident
            )
            .into()),
        }
    }

    fn exec_method<'a>(
        &'a self,
        method_token: U32Token,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> Result<
        Box<dyn FnOnce(TypeValue) -> TypeValue + 'a>,
        Box<dyn std::error::Error>,
    > {
        todo!()
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, Box<dyn std::error::Error>> {
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

pub enum U32Token {
    Set,
}

impl TokenConvert for U32Token {}

// ----------- A simple u8 type ---------------------------------------------

#[derive(Debug, Eq, PartialEq)]
pub struct U8(pub(crate) Option<u8>);

impl U8 {
    pub fn new(val: u8) -> Self {
        U8(Some(val))
    }
}

impl RotoFilter<U8Token> for U8 {
    // type TokenList = U8Token;

    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "set" => Ok(MethodProps::new(
                TypeValue::None,
                U8Token::Set.into_u8(),
                vec![TypeDef::IntegerLiteral],
            )),
            _ => Err(format!(
                "Unknown method: '{}' for type U8",
                method_name.ident
            )
            .into()),
        }
    }

    fn exec_method<'a>(
        &'a self,
        method_token: U8Token,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> Result<
        Box<dyn FnOnce(TypeValue) -> TypeValue + 'a>,
        Box<dyn std::error::Error>,
    > {
        todo!()
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, Box<dyn std::error::Error>> {
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
                Some(value) => {
                    match value {
                        0..=128 => Ok(TypeValue::Builtin(
                            BuiltinTypeValue::PrefixLength(PrefixLength(
                                Some(value),
                            )),
                        )),
                        _ => Err(format!(
                            "Prefix length must be between 0 and 128, not {}",
                            value
                        )
                        .into()),
                    }
                }
                None => Ok(TypeValue::Builtin(
                    BuiltinTypeValue::PrefixLength(PrefixLength(None)),
                )),
            },
            TypeDef::IntegerLiteral => match self.0 {
                Some(value) => {
                    match value {
                        0..=128 => Ok(TypeValue::Builtin(
                            BuiltinTypeValue::PrefixLength(PrefixLength(
                                Some(value),
                            )),
                        )),
                        _ => Err(format!(
                            "Prefix length must be between 0 and 128, not {}",
                            value
                        )
                        .into()),
                    }
                }
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

pub enum U8Token {
    Set,
}

impl TokenConvert for U8Token {}

// ----------- Boolean type -------------------------------------------------

#[derive(Debug, Eq, PartialEq)]
pub struct Boolean(pub(crate) Option<bool>);
impl Boolean {
    pub fn new(val: bool) -> Self {
        Boolean(Some(val))
    }
}

impl RotoFilter<BooleanToken> for Boolean {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "set" => Ok(MethodProps::new(
                TypeValue::None,
                BooleanToken::Set.into_u8(),
                vec![TypeDef::Boolean],
            )),
            _ => Err(format!(
                "Unknown method: '{}' for type Boolean",
                method_name.ident
            )
            .into()),
        }
    }

    fn exec_method<'a>(
        &'a self,
        method_token: BooleanToken,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> Result<
        Box<dyn FnOnce(TypeValue) -> TypeValue + 'a>,
        Box<dyn std::error::Error>,
    > {
        todo!()
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, Box<dyn std::error::Error>> {
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

pub enum BooleanToken {
    Set,
}

impl TokenConvert for BooleanToken {}

//------------ IntegerLiteral type ------------------------------------------

#[derive(Debug, Eq, PartialEq)]
pub struct IntegerLiteral(pub(crate) Option<i64>);
impl IntegerLiteral {
    pub fn new(val: i64) -> Self {
        IntegerLiteral(Some(val))
    }
}

impl RotoFilter<IntegerLiteralToken> for IntegerLiteral {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, Box<dyn std::error::Error>> {
        match method_name.ident.as_str() {
            "cmp" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::IntegerLiteral),
                IntegerLiteralToken::Cmp.into_u8(),
                vec![
                    TypeDef::IntegerLiteral,
                    TypeDef::IntegerLiteral,
                ],
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
    ) -> Result<TypeValue, Box<dyn std::error::Error>> {
        match type_def {
            TypeDef::IntegerLiteral => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(self)))
            },
            TypeDef::PrefixLength => {
                match self.0 {
                    Some(value) => {
                        match value {
                            0..=128 => Ok(TypeValue::Builtin(
                                BuiltinTypeValue::PrefixLength(PrefixLength(
                                    Some(value as u8),
                                )),
                            )),
                            _ => Err(format!(
                                "Prefix length must be between 0 and 128, not {}",
                                value
                            )
                            .into()),
                        }
                    }
                    None => Ok(TypeValue::Builtin(
                        BuiltinTypeValue::PrefixLength(PrefixLength(None)),
                    )),
                }
            }
            _ => Err(format!(
                "Cannot convert type IntegerLiteral to type {:?}",
                type_def
            )
            .into()),
        }
    }

    fn exec_method<'a>(
        &'a self,
        method_token: IntegerLiteralToken,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> Result<
        Box<dyn FnOnce(TypeValue) -> TypeValue + 'a>,
        Box<dyn std::error::Error>,
    > {
        todo!()
    }
}

pub(crate) enum IntegerLiteralToken {
    Cmp,
}

impl TokenConvert for IntegerLiteralToken {}

//------------ HexLiteral type ----------------------------------------------

#[derive(Debug, Eq, PartialEq)]
pub struct HexLiteral(pub(crate) Option<u64>);
impl HexLiteral {
    pub fn new(val: u64) -> Self {
        HexLiteral(Some(val))
    }
}

impl RotoFilter<HexLiteralToken> for HexLiteral {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, Box<dyn std::error::Error>> {
        match method_name.ident.as_str() {
            "cmp" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::IntegerLiteral),
                HexLiteralToken::Cmp.into_u8(),
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
    ) -> Result<TypeValue, Box<dyn std::error::Error>> {
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

    fn exec_method<'a>(
        &'a self,
        method_token: HexLiteralToken,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> Result<
        Box<dyn FnOnce(TypeValue) -> TypeValue + 'a>,
        Box<dyn std::error::Error>,
    > {
        todo!()
    }
}

pub(crate) enum HexLiteralToken {
    Cmp,
}

impl TokenConvert for HexLiteralToken {}

// ----------- Prefix type --------------------------------------------------

#[derive(Debug, Eq, PartialEq)]
pub struct Prefix(pub(crate) Option<routecore::addr::Prefix>);

impl Prefix {
    pub fn new(prefix: routecore::addr::Prefix) -> Self {
        Self(Some(prefix))
    }

    pub fn empty() -> Self {
        Self(None)
    }
}

impl RotoFilter<PrefixToken> for Prefix {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "from" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::Prefix),
                PrefixToken::From.into_u8(),
                vec![TypeDef::IpAddress, TypeDef::PrefixLength],
            )),
            "address" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::IpAddress),
                PrefixToken::Address.into_u8(),
                vec![],
            )),
            "len" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::IntegerLiteral),
                PrefixToken::Len.into_u8(),
                vec![],
            )),
            "matches" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::Boolean),
                PrefixToken::Matches.into_u8(),
                vec![TypeDef::Prefix],
            )),
            "exists" => Ok(MethodProps::new(
                TypeValue::Builtin(
                    BuiltinTypeValue::Boolean(Boolean(None)),
                ),
                PrefixToken::Exists.into_u8(),
                vec![]
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
    ) -> Result<TypeValue, Box<dyn std::error::Error>> {
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

    fn exec_method<'a>(
        &'a self,
        method_token: PrefixToken,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> Result<
        Box<dyn FnOnce(TypeValue) -> TypeValue + 'a>,
        Box<dyn std::error::Error>,
    > {
        todo!()
    }
}

pub(crate) enum PrefixToken {
    From,
    Exists,
    Address,
    Len,
    Matches,
}

impl TokenConvert for PrefixToken {}

//------------ PrefixLengthLiteral type -------------------------------------

#[derive(Debug, Eq, PartialEq)]
pub struct PrefixLength(pub(crate) Option<u8>);

impl PrefixLength {
    pub fn new(val: u8) -> Self {
        PrefixLength(Some(val))
    }
}

impl RotoFilter<PrefixLengthToken> for PrefixLength {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "from" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::PrefixLength),
                PrefixLengthToken::From.into_u8(),
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
    ) -> Result<TypeValue, Box<dyn std::error::Error>> {
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

    fn exec_method<'a>(
        &'a self,
        method_token: PrefixLengthToken,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> Result<
        Box<dyn FnOnce(TypeValue) -> TypeValue + 'a>,
        Box<dyn std::error::Error>,
    > {
        todo!()
    }
}

pub(crate) enum PrefixLengthToken {
    From,
}

impl TokenConvert for PrefixLengthToken {}

// ----------- Community ----------------------------------------------------

#[derive(Debug, Eq, PartialEq)]
pub enum CommunityType {
    Standard,
    Extended,
    Large,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Community(pub(crate) Option<CommunityType>);

impl Community {
    pub fn new(community_type: CommunityType) -> Self {
        Self(Some(community_type))
    }
}

impl RotoFilter<CommunityToken> for Community {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "from" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::Community),
                CommunityToken::From.into_u8(),
                vec![TypeDef::U32],
            )),
            "standard" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::Community),
                CommunityToken::Standard.into_u8(),
                vec![TypeDef::U32],
            )),
            "extended" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::Community),
                CommunityToken::Extended.into_u8(),
                vec![TypeDef::U32],
            )),
            "large" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::Community),
                CommunityToken::Large.into_u8(),
                vec![TypeDef::U32],
            )),
            "as" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::U32),
                CommunityToken::As.into_u8(),
                vec![],
            )),
            "value" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::U32),
                CommunityToken::Value.into_u8(),
                vec![],
            )),
            "exists" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::Boolean),
                CommunityToken::Exists.into_u8(),
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
    ) -> Result<TypeValue, Box<dyn std::error::Error>> {
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

    fn exec_method<'a>(
        &'a self,
        method_token: CommunityToken,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> Result<
        Box<dyn FnOnce(TypeValue) -> TypeValue + 'a>,
        Box<dyn std::error::Error>,
    > {
        todo!()
    }


}

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

#[derive(Debug, Eq, PartialEq)]
pub struct IpAddress(pub(crate) Option<std::net::IpAddr>);

impl IpAddress {
    pub fn new(addr: std::net::IpAddr) -> Self {
        IpAddress(Some(addr))
    }
}

impl RotoFilter<IpAddressToken> for IpAddress {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "from" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::IpAddress),
                IpAddressToken::From.into_u8(),
                vec![TypeDef::String],
            )),
            "matches" => Ok(MethodProps::new(
                TypeValue::from(&TypeDef::Boolean),
                IpAddressToken::Matches.into_u8(),
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
    ) -> Result<TypeValue, Box<dyn std::error::Error>> {
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

    fn exec_method<'a>(
        &'a self,
        method_token: IpAddressToken,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> Result<
        Box<dyn FnOnce(TypeValue) -> TypeValue + 'a>,
        Box<dyn std::error::Error>,
    > {
        todo!()
    }
}

pub(crate) enum IpAddressToken {
    From,
    Matches,
}

impl TokenConvert for IpAddressToken {}

// ----------- Asn type -----------------------------------------------------

#[derive(Debug, Eq, PartialEq)]
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

impl RotoFilter<AsnToken> for Asn {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "set" => Ok(MethodProps::new(
                TypeValue::None,
                AsnToken::Set.into_u8(),
                vec![TypeDef::Asn],
            )),
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
    ) -> Result<TypeValue, Box<dyn std::error::Error>> {
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

    fn exec_method<'a>(
        &'a self,
        method_token: AsnToken,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> Result<
        Box<dyn FnOnce(TypeValue) -> TypeValue + 'a>,
        Box<dyn std::error::Error>,
    > {
        todo!()
    }
}

pub enum AsnToken {
    Set,
}

impl TokenConvert for AsnToken {}

// ----------- AsPath type --------------------------------------------------

#[derive(Debug, Eq, PartialEq)]
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
    ) -> Result<
        routecore::asn::AsPath<Vec<routecore::asn::Asn>>,
        Box<dyn std::error::Error>,
    >
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

impl RotoFilter<AsPathToken> for AsPath {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, Box<(dyn std::error::Error + 'static)>>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "origin" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(None))),
                AsPathToken::Origin.into_u8(),
                vec![],
            )),
            "contains" => Ok(MethodProps::new(
                TypeValue::Builtin(
                    BuiltinTypeValue::AsPath(AsPath(None)),
                ),
                AsPathToken::Contains.into_u8(),
                vec![TypeDef::Asn],
            )),
            "len" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::U8(
                    U8(None)
                )),
                AsPathToken::Len.into_u8(),
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
    ) -> Result<TypeValue, Box<dyn std::error::Error>> {
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

    fn exec_method<'b>(
        &'b self,
        method: AsPathToken,
        args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<
        Box<(dyn FnOnce(TypeValue) -> TypeValue + 'b)>,
        Box<dyn std::error::Error>,
    > {
        match method {
            AsPathToken::Origin => {
                if let Some(rc_as_path) = &self.0 {
                    Ok(Box::new(move |as_path| {
                        let origin: routecore::asn::Asn =
                            rc_as_path.iter().next().unwrap().elements()[0];

                        TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(Some(
                            origin,
                        ))))
                    }))
                } else {
                    Ok(Box::new(move |_| {
                        TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(None)))
                    }))
                }
            }
            AsPathToken::Contains => {
                if let Some(_rc_as_path) = &self.0 {
                    {
                        Ok(Box::new(move |as_path| {
                            if let TypeValue::Builtin(
                                BuiltinTypeValue::AsPath(x_as_path),
                            ) = as_path
                            {
                                if let TypeValue::Builtin(
                                    BuiltinTypeValue::Asn(Asn(search_asn)),
                                ) = args[0]
                                {
                                    let contains = x_as_path
                                        .contains(search_asn.unwrap());
                                    TypeValue::Builtin(
                                        BuiltinTypeValue::Boolean(Boolean(
                                            Some(contains),
                                        )),
                                    )
                                } else {
                                    TypeValue::Builtin(
                                        BuiltinTypeValue::Boolean(Boolean(
                                            None,
                                        )),
                                    )
                                }
                            } else {
                                TypeValue::Builtin(BuiltinTypeValue::Boolean(
                                    Boolean(None),
                                ))
                            }
                        }))
                    }
                } else {
                    Ok(Box::new(move |_| {
                        TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(None)))
                    }))
                }
            }

            AsPathToken::Len => {
                if let Some(rc_as_path) = &self.0 {
                    Ok(Box::new(move |as_path| {
                        let len = rc_as_path.iter().count();
                        TypeValue::Builtin(BuiltinTypeValue::U8(U8(Some(
                            len as u8,
                        ))))
                    }))
                } else {
                    Ok(Box::new(move |_| {
                        TypeValue::Builtin(BuiltinTypeValue::U8(U8(None)))
                    }))
                }
            }
        }
    }
}

#[repr(u8)]
pub(crate) enum AsPathToken {
    Origin = 1,
    Contains = 2,
    Len = 3,
}

impl TokenConvert for AsPathToken {}

//------------ Route type ---------------------------------------------------

// Generic RFC4271 Route type, that can be parsed with routecore.

#[derive(Debug, Eq, PartialEq)]
pub struct Route {
    pub prefix: Option<Prefix>,
    pub bgp: Option<BgpAttributes>,
    pub status: RouteStatus,
}

impl RotoFilter<RouteToken> for Route {

    fn get_props_for_method(
            self,
            method_name: &crate::ast::Identifier,
        ) -> Result<MethodProps, Box<dyn std::error::Error>>
        where
            Self: std::marker::Sized {
        match method_name.ident.as_str() {
            "prefix" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Prefix(Prefix(None))),
                RouteToken::Prefix.into_u8(),
                vec![],
            )),
            "as_path" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::AsPath(AsPath(None))),
                RouteToken::AsPath.into_u8(),
               vec![],
            )),
            "communities" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Community(Community(None))),
                RouteToken::Communities.into_u8(),
                vec![],
            )),
            "status" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::RouteStatus(RouteStatus::Empty)),
                RouteToken::Status.into_u8(),
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
    ) -> Result<TypeValue, Box<dyn std::error::Error>> {
        match type_def {
            TypeDef::Route => Ok(TypeValue::Builtin(BuiltinTypeValue::Route(
                self,
            ))),
            _ => Err(format!(
                "Cannot convert type Route to type {:?}",
                type_def
            )
            .into()),
        }
    }

    fn exec_method<'b>(
        &'b self,
        _method: RouteToken,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<
        Box<(dyn FnOnce(TypeValue) -> TypeValue + 'b)>,
        Box<dyn std::error::Error>,
    > {
        todo!()
    }
}

pub enum RouteToken {
    Prefix,
    AsPath,
    Communities,
    Status,
}

impl TokenConvert for RouteToken {}

#[derive(Debug, Eq, PartialEq)]
pub enum RouteStatus {
    InConvergence, // Between start and EOR on a BGP peer-session
    UpToDate, // After EOR for a BGP peer-session, either Graceful Restart or EOR
    Stale,    // After hold-timer expiry
    StartOfRouteRefresh, // After the request for a Route Refresh to a peer and the reception of a new route
    Withdrawn,           // After the reception of a withdrawal
    Empty, // Status not relevant, e.g. a RIB that holds archived routes.
}

impl RotoFilter<RouteStatusToken> for RouteStatus {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "is_in_convergence" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(None))),
                RouteStatusToken::IsInConvergence.into_u8(),
                vec![],
            )),
            "is_up_to_date" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(None))),
                RouteStatusToken::IsUpToDate.into_u8(),
                vec![],
            )),
            "is_stale" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(None))),
                RouteStatusToken::IsStale.into_u8(),
                vec![],
            )),
            "is_start_of_route_refresh" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(None))),
                RouteStatusToken::IsStartOfRouteRefresh.into_u8(),
                vec![],
            )),  
            "is_withdrawn" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(None))),
                RouteStatusToken::IsWithdrawn.into_u8(),
                vec![],
            )),
            "is_empty" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(None))),
                RouteStatusToken::IsEmpty.into_u8(),
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
    ) -> Result<TypeValue, Box<dyn std::error::Error>> {
        match type_def {
            TypeDef::RouteStatus => Ok(TypeValue::Builtin(BuiltinTypeValue::RouteStatus(
                self,
            ))),
            _ => Err(format!(
                "Cannot convert type RouteStatus to type {:?}",
                type_def
            )
            .into()),
        }
    }

    fn exec_method<'a>(
        &'a self,
        _method_token: RouteStatusToken,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<
        Box<dyn FnOnce(TypeValue) -> TypeValue + 'a>,
        Box<dyn std::error::Error>,
    > {
        todo!()
    }
}

pub enum RouteStatusToken {
    IsInConvergence,
    IsUpToDate,
    IsStale,
    IsStartOfRouteRefresh,
    IsWithdrawn,
    IsEmpty,
}

impl TokenConvert for RouteStatusToken {}

#[derive(Debug, Eq, PartialEq)]
pub struct BgpAttributes {
    pub as_path: AsPath,
    pub communities: Vec<Community>,
}