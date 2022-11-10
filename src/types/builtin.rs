//------------ BuiltinTypeValue -------------------------------------------

// The built-in types

use routecore::asn::LongSegmentError;

use crate::symbols::Symbol;
use crate::traits::{MethodProps, RotoFilter};

use super::collections::Record;
use super::typedef::TypeDef;
use super::typevalue::TypeValue;

#[derive(Debug, PartialEq)]
pub enum BuiltinTypeValue {
    U32(U32),
    U8(U8),
    IntegerLiteral(IntegerLiteral),
    Prefix(Prefix),
    PrefixRecord((Prefix, Record)),
    Community(Community),
    IpAddress(IpAddress),
    Asn(Asn),
    AsPath(AsPath),
    Route(Route),
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
            BuiltinTypeValue::PrefixRecord(_) => "PrefixRecord",
            BuiltinTypeValue::Community(_) => "Community",
            BuiltinTypeValue::IpAddress(_) => "IpAddress",
            BuiltinTypeValue::Asn(_) => "Asn",
            BuiltinTypeValue::AsPath(_) => "AsPath",
            BuiltinTypeValue::Route(_) => "Route",
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
            BuiltinTypeValue::PrefixRecord(val) => val,
            BuiltinTypeValue::Community(val) => val,
            BuiltinTypeValue::IpAddress(val) => val,
            BuiltinTypeValue::Asn(val) => val,
            BuiltinTypeValue::AsPath(val) => val,
            BuiltinTypeValue::Route(val) => val,
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
                | "PrefixRecord"
                | "Community"
                | "IpAddress"
                | "Asn"
                | "AsPath"
                | "Route"
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
            TypeDef::PrefixRecord => {
                if let BuiltinTypeValue::PrefixRecord(v) = value.into() {
                    BuiltinTypeValue::PrefixRecord(v)
                } else {
                    return Err("Not a Prefix Record".into());
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

impl From<usize> for BuiltinTypeValue {
    fn from(val: usize) -> Self {
        BuiltinTypeValue::IntegerLiteral(IntegerLiteral(Some(val as usize)))
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
            "Boolean" => Ok(BuiltinTypeValue::Boolean(Boolean(None))),
            "Prefix" => Ok(BuiltinTypeValue::Prefix(Prefix(None))),
            "PrefixRecord" => Ok(BuiltinTypeValue::PrefixRecord((
                Prefix(None),
                Record(vec![]),
            ))),
            "Community" => Ok(BuiltinTypeValue::Community(Community(None))),
            "IpAddress" => Ok(BuiltinTypeValue::IpAddress(IpAddress(None))),
            "Asn" => Ok(BuiltinTypeValue::Asn(Asn(None))),
            "AsPath" => Ok(BuiltinTypeValue::AsPath(AsPath(None))),
            "Route" => Ok(BuiltinTypeValue::Route(Route {
                prefix: None,
                bgp: None,
                status: Status::InConvergence,
            })),
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
            TypeDef::PrefixRecord => Ok(BuiltinTypeValue::PrefixRecord((
                Prefix(None),
                Record(vec![]),
            ))),
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
                status: Status::InConvergence,
            })),
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
            BuiltinTypeValue::PrefixRecord(_) => write!(f, "Prefix Record"),
            BuiltinTypeValue::Community(_) => write!(f, "Community"),
            BuiltinTypeValue::IpAddress(_) => write!(f, "IP Address"),
            BuiltinTypeValue::Asn(_) => write!(f, "Autonomous System Number"),
            BuiltinTypeValue::AsPath(_) => {
                write!(f, "AsPath (BGP AS_PATH attribute)")
            }
            BuiltinTypeValue::Route(_) => write!(f, "Route (BGP Route)"),
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

// ----------- A simple u8 type ---------------------------------------------

#[derive(Debug, Eq, PartialEq)]
pub struct U8(pub(crate) Option<u8>);

impl U8 {
    pub fn new(val: u8) -> Self {
        U8(Some(val))
    }
}

// ----------- Boolean type -------------------------------------------------

#[derive(Debug, Eq, PartialEq)]
pub struct Boolean(pub(crate) Option<bool>);
impl Boolean {
    pub fn new(val: bool) -> Self {
        Boolean(Some(val))
    }
}

//------------ IntegerLiteral type ------------------------------------------

#[derive(Debug, Eq, PartialEq)]
pub struct IntegerLiteral(pub(crate) Option<usize>);
impl IntegerLiteral {
    pub fn new(val: usize) -> Self {
        IntegerLiteral(Some(val))
    }
}

impl RotoFilter<IntegerLiteralToken> for IntegerLiteral {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, Box<dyn std::error::Error>> {
        match method_name.ident.as_str() {
            "cmp" => Ok(MethodProps {
                method_token: std::mem::size_of_val(&IntegerLiteralToken::Cmp)
                    as u8,
                return_type_value: TypeValue::from(&TypeDef::IntegerLiteral),
                arg_types: vec![
                    Symbol::new_argument_type(TypeDef::IntegerLiteral),
                    Symbol::new_argument_type(TypeDef::IntegerLiteral),
                ],
            }),
            _ => Err(format!(
                "Unknown method: {} for type Prefix",
                method_name.ident
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

//------------ HexLiteral type ----------------------------------------------

#[derive(Debug, Eq, PartialEq)]
pub struct HexLiteral(pub(crate) Option<u64>);
impl HexLiteral {
    pub fn new(val: u64) -> Self {
        HexLiteral(Some(val))
    }
}

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
            "from" => Ok(MethodProps {
                method_token: std::mem::size_of_val(&PrefixToken::From) as u8,
                return_type_value: TypeValue::from(&TypeDef::Prefix),
                arg_types: vec![
                    Symbol::new_argument_type(TypeDef::IpAddress),
                    Symbol::new_argument_type(TypeDef::U8),
                ],
            }),
            "address" => Ok(MethodProps {
                method_token: std::mem::size_of_val(&PrefixToken::Address)
                    as u8,
                return_type_value: TypeValue::from(&TypeDef::IpAddress),
                arg_types: vec![],
            }),
            "len" => Ok(MethodProps {
                method_token: std::mem::size_of_val(&PrefixToken::Len) as u8,
                return_type_value: TypeValue::from(&TypeDef::IntegerLiteral),
                arg_types: vec![],
            }),
            "matches" => Ok(MethodProps {
                method_token: std::mem::size_of_val(&PrefixToken::Matches)
                    as u8,
                return_type_value: TypeValue::from(&TypeDef::Boolean),
                arg_types: vec![Symbol::new_argument_type(TypeDef::Prefix)],
            }),
            _ => Err(format!(
                "Unknown method: {} for type Prefix",
                method_name.ident
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
    Address,
    Len,
    Matches,
}

// ----------- Community ----------------------------------------------------

#[derive(Debug, Eq, PartialEq)]
pub enum CommunityType {
    Normal,
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
            "origin" => Ok(MethodProps {
                method_token: std::mem::size_of_val(&AsPathToken::Origin)
                    as u8,
                return_type_value: TypeValue::Builtin(BuiltinTypeValue::Asn(
                    Asn(None),
                )),
                arg_types: vec![],
            }),
            "contains" => Ok(MethodProps {
                method_token: std::mem::size_of_val(&AsPathToken::Contains)
                    as u8,
                return_type_value: TypeValue::Builtin(
                    BuiltinTypeValue::AsPath(AsPath(None)),
                ),
                arg_types: vec![Symbol::new_argument_type(TypeDef::Asn)],
            }),
            "len" => Ok(MethodProps {
                method_token: std::mem::size_of_val(&AsPathToken::Len) as u8,
                return_type_value: TypeValue::Builtin(BuiltinTypeValue::U8(
                    U8(None),
                )),
                arg_types: vec![],
            }),
            _ => {
                Err(format!("Unknown method '{}'", method_name.ident).into())
            }
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

// routecore, roto
// Message iterator -> Routes
#[derive(Debug, Eq, PartialEq)]
pub struct Route {
    pub prefix: Option<Prefix>,
    pub bgp: Option<BgpAttributes>,
    pub status: Status,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Status {
    InConvergence, // Between start and EOR on a BGP peer-session
    UpToDate, // After EOR for a BGP peer-session, either Graceful Restart or EOR
    Stale,    // After hold-timer expiry
    StartOfRouteRefresh, // After the request for a Route Refresh to a peer and the reception of a new route
    Withdrawn,           // After the reception of a withdrawal
    Empty, // Status not relevant, e.g. a RIB that holds archived routes.
}

#[derive(Debug, Eq, PartialEq)]
pub struct BgpAttributes {
    pub as_path: AsPath,
    pub communities: Vec<Community>,
}
