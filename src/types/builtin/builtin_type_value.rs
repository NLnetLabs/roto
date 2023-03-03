//------------ BuiltinTypeValue -------------------------------------------

// The built-in types

use std::fmt::Display;

use routecore::bgp::route::RouteStatus;

use crate::compile::CompileError;

use super::super::typedef::TypeDef;
use super::super::typevalue::TypeValue;

use super::*;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum BuiltinTypeValue {
    U32(U32), // scalar
    U8(U8), // scalar
    IntegerLiteral(IntegerLiteral), // scalar
    StringLiteral(StringLiteral), // scalar
    Prefix(Prefix), // scalar
    PrefixLength(PrefixLength), // scalar
    Community(Community), // vector
    IpAddress(IpAddress), // scalar
    Asn(Asn), // scalar 
    AsPath(AsPath), // vector
    OriginType(OriginType), // scalar
    Route(RawRouteWithDeltas), // vector
    RouteStatus(RouteStatus), // scalar
    Boolean(Boolean), // scalar
    HexLiteral(HexLiteral), // scalar
}

impl BuiltinTypeValue {
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
        BuiltinTypeValue::U32(U32(val))
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

// impl TryFrom<&'_ str> for BuiltinTypeValue {
//     type Error = CompileError;

//     fn try_from(val: &'_ str) -> Result<Self, Self::Error> {
//         match val {
//             "U32" => Ok(BuiltinTypeValue::U32(U32(None))),
//             "U8" => Ok(BuiltinTypeValue::U8(U8(None))),
//             "IntegerLiteral" => {
//                 Ok(BuiltinTypeValue::IntegerLiteral(IntegerLiteral(None)))
//             }
//             "PrefixLengthLiteral" => {
//                 Ok(BuiltinTypeValue::PrefixLength(PrefixLength(None)))
//             }
//             "Boolean" => Ok(BuiltinTypeValue::Boolean(Boolean(None))),
//             "Prefix" => Ok(BuiltinTypeValue::Prefix(Prefix(None))),
//             "PrefixLength" => {
//                 Ok(BuiltinTypeValue::PrefixLength(PrefixLength(None)))
//             }
//             "Community" => Ok(BuiltinTypeValue::Community(Community(None))),
//             "IpAddress" => Ok(BuiltinTypeValue::IpAddress(IpAddress(None))),
//             "Asn" => Ok(BuiltinTypeValue::Asn(Asn(None))),
//             "AsPath" => Ok(BuiltinTypeValue::AsPath(AsPath(None))),
//             "Route" => Ok(BuiltinTypeValue::Route(None)),
//             "RouteStatus" => {
//                 Ok(BuiltinTypeValue::RouteStatus(RouteStatus::Empty))
//             }
//             _ => Err(format!("Unknown type: {}", val).into()),
//         }
//     }
// }

// impl TryFrom<&TypeDef> for BuiltinTypeValue {
//     type Error = CompileError;

//     fn try_from(ty: &TypeDef) -> Result<Self, Self::Error> {
//         match ty {
//             TypeDef::U32 => Ok(BuiltinTypeValue::U32(U32(None))),
//             TypeDef::U8 => Ok(BuiltinTypeValue::U8(U8(None))),
//             TypeDef::IntegerLiteral => {
//                 Ok(BuiltinTypeValue::IntegerLiteral(IntegerLiteral(None)))
//             }
//             TypeDef::Boolean => Ok(BuiltinTypeValue::Boolean(Boolean(None))),
//             TypeDef::Prefix => Ok(BuiltinTypeValue::Prefix(Prefix(None))),
//             TypeDef::PrefixLength => {
//                 Ok(BuiltinTypeValue::PrefixLength(PrefixLength(None)))
//             }
//             TypeDef::Community => {
//                 Ok(BuiltinTypeValue::Community(Community(None)))
//             }
//             TypeDef::IpAddress => {
//                 Ok(BuiltinTypeValue::IpAddress(IpAddress(None)))
//             }
//             TypeDef::Asn => Ok(BuiltinTypeValue::Asn(Asn(None))),
//             TypeDef::AsPath => Ok(BuiltinTypeValue::AsPath(AsPath(None))),
//             TypeDef::Route => Ok(BuiltinTypeValue::Route(None)),
//             TypeDef::RouteStatus => {
//                 Ok(BuiltinTypeValue::RouteStatus(RouteStatus::Empty))
//             }
//             _ => Err(format!("Unknown type: {:?}", ty).into()),
//         }
//     }
// }

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
            BuiltinTypeValue::OriginType(v) => {
                write!(f, "{} (Origin Type)", v)
            }
            BuiltinTypeValue::Route(r) => write!(f, "{} (Route)", r),
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
