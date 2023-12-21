//------------ BuiltinTypeValue -------------------------------------------

// The built-in types

use std::fmt::Display;

use routecore::bgp::message::nlri::PathId;
use routecore::bgp::types::{AfiSafi, NextHop};
use routecore::addr::Prefix;
use routecore::bgp::communities::HumanReadableCommunity as Community;
use serde::Serialize;

use crate::compiler::compile::CompileError;
use crate::traits::RotoType;
use crate::types::collections::BytesRecord;
use crate::types::enum_types::EnumVariant;
use crate::types::lazyrecord_types::{
    BmpMessage, InitiationMessage, PeerDownNotification, PeerUpNotification,
    RouteMonitoring, StatisticsReport, TerminationMessage,
};

use super::super::typedef::TypeDef;
use super::super::typevalue::TypeValue;

use super::{
    Aggregator, Asn, AtomicAggregate, BgpUpdateMessage, Boolean,
    HexLiteral, Hop, IntegerLiteral, IpAddress, LocalPref,
    MultiExitDisc, OriginType, PrefixLength,
    RawRouteWithDeltas, RouteStatus, StringLiteral,
};

#[derive(Debug, Eq, Clone, Hash, PartialEq, Serialize)]
#[serde(untagged)]
pub enum BuiltinTypeValue {
    U32(u32),                         // scalar
    U16(u16),                         // scalar
    U8(u8),                           // scalar
    IntegerLiteral(IntegerLiteral),   // scalar
    StringLiteral(StringLiteral),     // scalar
    Boolean(Boolean),                 // scalar
    HexLiteral(HexLiteral),           // scalar
    IpAddress(IpAddress),             // scalar
    Prefix(Prefix),                   // scalar
    AfiSafi(AfiSafi),                 // scalar
    PathId(PathId),                   // scalar
    PrefixLength(PrefixLength),       // scalar
    LocalPref(LocalPref),             // scalar
    AtomicAggregate(AtomicAggregate), // scalar
    Aggregator(Aggregator),           // scalar
    NextHop(NextHop),                 // scalar
    MultiExitDisc(MultiExitDisc),     // scalar
    RouteStatus(RouteStatus),         // scalar
    Community(Community),             // scalar
    Asn(Asn),                         // scalar
    AsPath(routecore::bgp::aspath::HopPath),                   // vector
    Hop(Hop),                         // read-only scalar
    OriginType(OriginType),           // scalar
    Route(RawRouteWithDeltas),        // vector
    // A read-only enum variant for capturing constants
    ConstU8EnumVariant(EnumVariant<u8>),
    ConstU16EnumVariant(EnumVariant<u16>),
    ConstU32EnumVariant(EnumVariant<u32>),
    // Used for filtering on the properties of the whole message,
    // not taking into account any individual prefixes.
    BgpUpdateMessage(BgpUpdateMessage), // scalar
    BmpMessage(BytesRecord<BmpMessage>),
    BmpRouteMonitoringMessage(BytesRecord<RouteMonitoring>),
    BmpPeerUpNotification(BytesRecord<PeerUpNotification>),
    BmpPeerDownNotification(BytesRecord<PeerDownNotification>),
    BmpInitiationMessage(BytesRecord<InitiationMessage>),
    BmpTerminationMessage(BytesRecord<TerminationMessage>),
    BmpStatisticsReport(BytesRecord<StatisticsReport>),
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
            TypeDef::StringLiteral => {
                if let BuiltinTypeValue::StringLiteral(v) = value.into() {
                    BuiltinTypeValue::StringLiteral(v)
                } else {
                    return Err("Not a StringLiteral".into());
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
        Ok(var.into())
    }

    pub(crate) fn into_type(
        self,
        ty: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match self {
            BuiltinTypeValue::U32(v) => v.into_type(ty),
            BuiltinTypeValue::U16(v) => v.into_type(ty),
            BuiltinTypeValue::U8(v) => v.into_type(ty),
            BuiltinTypeValue::ConstU8EnumVariant(v) => v.into_type(ty),
            BuiltinTypeValue::ConstU16EnumVariant(v) => v.into_type(ty),
            BuiltinTypeValue::ConstU32EnumVariant(v) => v.into_type(ty),
            BuiltinTypeValue::IntegerLiteral(v) => v.into_type(ty),
            BuiltinTypeValue::StringLiteral(v) => v.into_type(ty),
            BuiltinTypeValue::Prefix(v) => v.into_type(ty),
            BuiltinTypeValue::AfiSafi(v) => v.into_type(ty),
            BuiltinTypeValue::PathId(v) => v.into_type(ty),
            BuiltinTypeValue::PrefixLength(v) => v.into_type(ty),
            BuiltinTypeValue::Community(v) => v.into_type(ty),
            BuiltinTypeValue::IpAddress(v) => v.into_type(ty),
            BuiltinTypeValue::AsPath(v) => v.into_type(ty),
            BuiltinTypeValue::Hop(h) => h.into_type(ty),
            BuiltinTypeValue::OriginType(v) => v.into_type(ty),
            BuiltinTypeValue::Route(r) => r.into_type(ty),
            BuiltinTypeValue::BgpUpdateMessage(_raw) => Err(CompileError::from(
                "Cannot convert raw BGP message into any other type.",
            )),
            BuiltinTypeValue::BmpMessage(_raw) => Err(CompileError::from(
                "Cannot convert raw BMP message into any other type.",
            )),
            BuiltinTypeValue::BmpRouteMonitoringMessage(_raw) => Err(CompileError::from(
                "Cannot convert raw BMP Route Monitoring message into any other type.",
            )),
            BuiltinTypeValue::BmpPeerUpNotification(_raw) => Err(CompileError::from(
                "Cannot convert raw BMP Peer Up Notification into any other type.",
            )),
            BuiltinTypeValue::BmpPeerDownNotification(_raw) => Err(CompileError::from(
                "Cannot convert raw BMP Peer Down Notification into any other type.",
            )),
            BuiltinTypeValue::BmpInitiationMessage(_raw) => Err(CompileError::from(
                "Cannot convert raw BMP Initiation into any other type.",
            )),
            BuiltinTypeValue::BmpTerminationMessage(_raw) => Err(CompileError::from(
                "Cannot convert raw BMP Termination into any other type.",
            )),
            BuiltinTypeValue::BmpStatisticsReport(_raw) => Err(CompileError::from(
                "Cannot convert raw BMP Statistics Report into any other type.",
            )),
            BuiltinTypeValue::RouteStatus(v) => v.into_type(ty),
            BuiltinTypeValue::Boolean(v) => v.into_type(ty),
            BuiltinTypeValue::HexLiteral(v) => v.into_type(ty),
            BuiltinTypeValue::Asn(v) => v.into_type(ty),
            BuiltinTypeValue::LocalPref(v) => v.into_type(ty),
            BuiltinTypeValue::AtomicAggregate(v) => v.into_type(ty),
            BuiltinTypeValue::Aggregator(v) => v.into_type(ty),
            BuiltinTypeValue::NextHop(v) => v.into_type(ty),
            BuiltinTypeValue::MultiExitDisc(v) => v.into_type(ty),
        }
    }
}

// These From implementations allow the user to use the create_instance
// function with simple types like u32, u8, etc. (without the nested
// variants).

impl From<Asn> for BuiltinTypeValue {
    fn from(val: Asn) -> Self {
        BuiltinTypeValue::Asn(val)
    }
}

impl From<u32> for BuiltinTypeValue {
    fn from(value: u32) -> Self {
        BuiltinTypeValue::U32(value)
    }
}

impl From<u16> for BuiltinTypeValue {
    fn from(value: u16) -> Self {
        BuiltinTypeValue::U16(value)
    }
}

impl From<u8> for BuiltinTypeValue {
    fn from(value: u8) -> Self {
        BuiltinTypeValue::U8(value)
    }
}

// impl From<U8> for BuiltinTypeValue {
//     fn from(value: U8) -> Self {
//         BuiltinTypeValue::U8(value)
//     }
// }

impl From<PrefixLength> for BuiltinTypeValue {
    fn from(val: PrefixLength) -> Self {
        BuiltinTypeValue::PrefixLength(val)
    }
}

impl From<routecore::bgp::aspath::HopPath> for BuiltinTypeValue {
    fn from(value: routecore::bgp::aspath::HopPath) -> Self {
        BuiltinTypeValue::AsPath(value)
    }
}

impl From<HexLiteral> for BuiltinTypeValue {
    fn from(value: HexLiteral) -> Self {
        BuiltinTypeValue::HexLiteral(value)
    }
}

impl From<i64> for BuiltinTypeValue {
    fn from(val: i64) -> Self {
        BuiltinTypeValue::IntegerLiteral(IntegerLiteral(val))
    }
}

impl From<std::net::IpAddr> for BuiltinTypeValue {
    fn from(val: std::net::IpAddr) -> Self {
        BuiltinTypeValue::IpAddress(IpAddress(val))
    }
}

impl From<routecore::addr::Prefix> for BuiltinTypeValue {
    fn from(value: routecore::addr::Prefix) -> Self {
        BuiltinTypeValue::Prefix(value)
    }
}

// impl From<crate::types::builtin::primitives::Community> for BuiltinTypeValue {
//     fn from(value: crate::types::builtin::primitives::Community) -> Self {
//         BuiltinTypeValue::Community(value)
//     }
// }

impl From<crate::types::builtin::primitives::IpAddress> for BuiltinTypeValue {
    fn from(value: crate::types::builtin::primitives::IpAddress) -> Self {
        BuiltinTypeValue::IpAddress(value)
    }
}

impl From<BytesRecord<BmpMessage>> for BuiltinTypeValue {
    fn from(value: BytesRecord<BmpMessage>) -> Self {
        BuiltinTypeValue::BmpMessage(value)
    }
}

impl From<BytesRecord<RouteMonitoring>> for BuiltinTypeValue {
    fn from(value: BytesRecord<RouteMonitoring>) -> Self {
        BuiltinTypeValue::BmpRouteMonitoringMessage(value)
    }
}

impl From<BytesRecord<PeerDownNotification>> for BuiltinTypeValue {
    fn from(value: BytesRecord<PeerDownNotification>) -> Self {
        BuiltinTypeValue::BmpPeerDownNotification(value)
    }
}

impl From<BytesRecord<PeerUpNotification>> for BuiltinTypeValue {
    fn from(value: BytesRecord<PeerUpNotification>) -> Self {
        BuiltinTypeValue::BmpPeerUpNotification(value)
    }
}

impl Display for BuiltinTypeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if !f.alternate() {
            match self {
                BuiltinTypeValue::U32(v) => write!(f, "{}", v),
                BuiltinTypeValue::U16(v) => write!(f, "{}", v),
                BuiltinTypeValue::U8(v) => write!(f, "{}", v),
                BuiltinTypeValue::IntegerLiteral(v) => {
                    write!(f, "{}", v)
                }
                BuiltinTypeValue::StringLiteral(v) => {
                    write!(f, "{}", v.0)
                }
                BuiltinTypeValue::ConstU8EnumVariant(v) => {
                    write!(f, "{}", v.value)
                }
                BuiltinTypeValue::ConstU16EnumVariant(v) => {
                    write!(f, "{}", v)
                }
                BuiltinTypeValue::ConstU32EnumVariant(v) => {
                    write!(f, "{}", v)
                }
                BuiltinTypeValue::Prefix(v) => write!(f, "{}", v),
                BuiltinTypeValue::PathId(v) => write!(f, "{}", v),
                BuiltinTypeValue::AfiSafi(v) => write!(f, "{}", v),
                BuiltinTypeValue::PrefixLength(v) => {
                    write!(f, "{}", v)
                }
                BuiltinTypeValue::Community(v) => write!(f, "{}", v),
                BuiltinTypeValue::IpAddress(v) => write!(f, "{}", v),
                BuiltinTypeValue::Asn(v) => write!(f, "{}", v),
                BuiltinTypeValue::AsPath(v) => {
                    write!(f, "{}", v)
                }
                BuiltinTypeValue::Hop(h) => {
                    write!(f, "{}", h)
                }
                BuiltinTypeValue::OriginType(v) => {
                    write!(f, "{}", v)
                }
                BuiltinTypeValue::Route(r) => write!(f, "{}", r),
                BuiltinTypeValue::BgpUpdateMessage(raw) => {
                    write!(f, "{:X?}", *raw)
                }
                BuiltinTypeValue::BmpMessage(raw) => {
                    write!(f, "{:X?}", *raw)
                }
                BuiltinTypeValue::BmpRouteMonitoringMessage(raw) => {
                    write!(f, "{:X?}", *raw)
                }
                BuiltinTypeValue::BmpPeerUpNotification(raw) => {
                    write!(f, "{:X?}", *raw)
                }
                BuiltinTypeValue::BmpPeerDownNotification(raw) => {
                    write!(f, "{:X?}", *raw)
                }
                BuiltinTypeValue::BmpInitiationMessage(raw) => {
                    write!(f, "{:X?}", *raw)
                }
                BuiltinTypeValue::BmpTerminationMessage(raw) => {
                    write!(f, "{:X?}", *raw)
                }
                BuiltinTypeValue::BmpStatisticsReport(raw) => {
                    write!(f, "{:X?}", *raw)
                }
                BuiltinTypeValue::RouteStatus(v) => {
                    write!(f, "{}", v)
                }
                BuiltinTypeValue::Boolean(v) => write!(f, "{}", v),
                BuiltinTypeValue::HexLiteral(v) => {
                    write!(f, "{}", v)
                }
                BuiltinTypeValue::LocalPref(v) => {
                    write!(f, "{}", v)
                }
                BuiltinTypeValue::AtomicAggregate(v) => {
                    write!(f, "{}", v)
                }
                BuiltinTypeValue::Aggregator(v) => {
                    write!(f, "{}", v)
                }
                BuiltinTypeValue::NextHop(v) => write!(f, "{}", v),
                BuiltinTypeValue::MultiExitDisc(v) => {
                    write!(f, "{}", v)
                }
            }
        } else {
            // This is the pretty printer: "{:#?}"
            match self {
                BuiltinTypeValue::U32(v) => write!(f, "{} (U32)", v),
                BuiltinTypeValue::U16(v) => write!(f, "{} (U16)", v),
                BuiltinTypeValue::U8(v) => write!(f, "{} (U8)", v),
                BuiltinTypeValue::IntegerLiteral(v) => {
                    write!(f, "{} (Integer)", v)
                }
                BuiltinTypeValue::StringLiteral(v) => {
                    write!(f, "{} (String)", v.0)
                }
                BuiltinTypeValue::ConstU8EnumVariant(v) => {
                    write!(f, "{} (Const U8 Enum Variant)", v.value)
                }
                BuiltinTypeValue::ConstU16EnumVariant(v) => {
                    write!(f, "{} (Const U16 Enum Variant)", v.value)
                }
                BuiltinTypeValue::ConstU32EnumVariant(v) => {
                    write!(f, "{} (Const U32 Enum Variant)", v.value)
                }
                BuiltinTypeValue::Prefix(v) => write!(f, "{} (Prefix)", v),
                BuiltinTypeValue::AfiSafi(v) => write!(f, "{} (AFI SAFI)", v),
                BuiltinTypeValue::PathId(v) => write!(f, "{} (Path ID)", v),
                BuiltinTypeValue::PrefixLength(v) => {
                    write!(f, "{} (Prefix Length)", v)
                }
                BuiltinTypeValue::Community(v) => {
                    write!(f, "{} (Community)", v)
                }
                BuiltinTypeValue::IpAddress(v) => {
                    write!(f, "{} (IP Address)", v)
                }
                BuiltinTypeValue::Asn(v) => write!(f, "{} (ASN)", v),
                BuiltinTypeValue::AsPath(v) => {
                    write!(f, "{} (AS Path)", v)
                }
                BuiltinTypeValue::Hop(h) => {
                    write!(f, "{} (Hop)", h)
                }
                BuiltinTypeValue::OriginType(v) => {
                    write!(f, "{} (Origin Type)", v)
                }
                BuiltinTypeValue::Route(r) => write!(f, "{} (Route)", r),
                BuiltinTypeValue::BgpUpdateMessage(raw) => {
                    write!(f, "{:X?} (RawBgpMessage)", *raw)
                }
                BuiltinTypeValue::BmpMessage(raw) => {
                    write!(f, "{:X?} (RawBgpMessage)", *raw)
                }
                BuiltinTypeValue::BmpRouteMonitoringMessage(raw) => {
                    write!(f, "{:X?} (BmpRouteMonitoringMessage)", *raw)
                }
                BuiltinTypeValue::BmpPeerUpNotification(raw) => {
                    write!(f, "{:X?} (BmpPeerUpNotification)", *raw)
                }
                BuiltinTypeValue::BmpPeerDownNotification(raw) => {
                    write!(f, "{:X?} (BmpPeerDownNotification)", *raw)
                }
                BuiltinTypeValue::BmpInitiationMessage(raw) => {
                    write!(f, "{:X?} (BmpInitiationMessage)", *raw)
                }
                BuiltinTypeValue::BmpTerminationMessage(raw) => {
                    write!(f, "{:X?} (BmpTerminationMessage)", *raw)
                }
                BuiltinTypeValue::BmpStatisticsReport(raw) => {
                    write!(f, "{:X?} (BmpStatisticsReport)", *raw)
                }
                BuiltinTypeValue::RouteStatus(v) => {
                    write!(f, "{} (Route Status)", v)
                }
                BuiltinTypeValue::Boolean(v) => write!(f, "{} (Boolean)", v),
                BuiltinTypeValue::HexLiteral(v) => {
                    write!(f, "{} (Hex)", v)
                }
                BuiltinTypeValue::LocalPref(v) => {
                    write!(f, "{} (Local Preference)", v)
                }
                BuiltinTypeValue::Aggregator(v) => {
                    write!(f, "{} (Aggregator)", v)
                }
                BuiltinTypeValue::AtomicAggregate(v) => {
                    write!(f, "{} (Atomic Aggregate)", v)
                }
                BuiltinTypeValue::NextHop(v) => write!(f, "{} (Next Hop)", v),
                BuiltinTypeValue::MultiExitDisc(v) => {
                    write!(f, "{} (Multi Exit Discriminator)", v)
                }
            }
        }
    }
}
