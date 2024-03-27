//------------ BuiltinTypeValue -------------------------------------------

// The built-in types

use std::fmt::Display;
use std::net::IpAddr;

use inetnum::asn::Asn;
use routecore::bgp::types::PathId;
use routecore::bgp::types::{AfiSafi, AtomicAggregate, MultiExitDisc, NextHop, Origin};
use routecore::bgp::path_attributes::AggregatorInfo;
use inetnum::addr::Prefix;
use routecore::bgp::communities::HumanReadableCommunity as Community;
use routecore::bgp::nlri::afisafi::Nlri;
use serde::Serialize;

use crate::compiler::compile::CompileError;
use crate::traits::RotoType;
use crate::types::collections::BytesRecord;
use crate::types::enum_types::EnumVariant;
use crate::types::lazyrecord_types::{
    BgpUpdateMessage, BmpMessage, InitiationMessage, PeerDownNotification, PeerUpNotification, RouteMonitoring, StatisticsReport, TerminationMessage
};

use super::super::typedef::TypeDef;
use super::super::typevalue::TypeValue;

use super::basic_route::{PeerId, PeerRibType, Provenance};
use super::{
    FlowSpecRoute, HexLiteral, IntegerLiteral, NlriStatus, PrefixLength, PrefixRoute, RouteContext, StringLiteral
};

#[derive(Debug, Eq, Clone, Hash, PartialEq, Serialize)]
#[serde(untagged)]
pub enum BuiltinTypeValue {
    U32(u32),                         // scalar
    U16(u16),                         // scalar
    U8(u8),                           // scalar
    IntegerLiteral(IntegerLiteral),   // scalar
    StringLiteral(StringLiteral),     // scalar
    Bool(bool),                       // scalar
    HexLiteral(HexLiteral),           // scalar
    IpAddr(IpAddr),                   // scalar
    Prefix(Prefix),                   // scalar
    AfiSafi(AfiSafi),                 // scalar
    PathId(PathId),                   // scalar
    PrefixLength(PrefixLength),       // scalar
    LocalPref(routecore::bgp::types::LocalPref),    // scalar
    AtomicAggregate(AtomicAggregate), // scalar
    AggregatorInfo(AggregatorInfo),           // scalar
    NextHop(NextHop),                 // scalar
    MultiExitDisc(MultiExitDisc),     // scalar
    NlriStatus(NlriStatus),         // scalar
    Community(Community),             // scalar
    Nlri(Nlri<bytes::Bytes>),                       // scalar
    Provenance(Provenance),           // scalar
    Asn(Asn),                         // scalar
    AsPath(routecore::bgp::aspath::HopPath),        // vector
    Hop(routecore::bgp::aspath::OwnedHop), // read-only scalar
    Origin(Origin),           // scalar
    PrefixRoute(PrefixRoute),
    FlowSpecRoute(FlowSpecRoute<bytes::Bytes>),
    RouteContext(RouteContext),
    PeerId(PeerId),                    // scalar
    PeerRibType(PeerRibType), // scalar
    // A read-only enum variant for capturing constants
    ConstU8EnumVariant(EnumVariant<u8>),
    ConstU16EnumVariant(EnumVariant<u16>),
    ConstU32EnumVariant(EnumVariant<u32>),
    // Used for filtering on the properties of the whole message,
    // not taking into account any individual prefixes.
    BgpUpdateMessage(BytesRecord<BgpUpdateMessage>),
    BmpMessage(BytesRecord<BmpMessage>),
    BmpRouteMonitoringMessage(BytesRecord<RouteMonitoring>),
    BmpPeerUpNotification(BytesRecord<PeerUpNotification>),
    BmpPeerDownNotification(BytesRecord<PeerDownNotification>),
    BmpInitiationMessage(BytesRecord<InitiationMessage>),
    BmpTerminationMessage(BytesRecord<TerminationMessage>),
    BmpStatisticsReport(BytesRecord<StatisticsReport>),
}

impl BuiltinTypeValue {
    pub fn into_type(
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
            BuiltinTypeValue::Nlri(v) => v.into_type(ty),
            BuiltinTypeValue::IpAddr(v) => v.into_type(ty),
            BuiltinTypeValue::AsPath(v) => v.into_type(ty),
            BuiltinTypeValue::Hop(h) => h.into_type(ty),
            BuiltinTypeValue::Origin(v) => v.into_type(ty),
            BuiltinTypeValue::PrefixRoute(r) => r.into_type(ty),
            BuiltinTypeValue::FlowSpecRoute(r) => r.into_type(ty),
            BuiltinTypeValue::RouteContext(c) => c.into_type(ty),
            BuiltinTypeValue::PeerId(r) => r.into_type(ty),
            BuiltinTypeValue::PeerRibType(p) => p.into_type(ty),
            BuiltinTypeValue::Provenance(p) => p.into_type(ty),
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
            BuiltinTypeValue::NlriStatus(v) => v.into_type(ty),
            BuiltinTypeValue::Bool(v) => v.into_type(ty),
            BuiltinTypeValue::HexLiteral(v) => v.into_type(ty),
            BuiltinTypeValue::Asn(v) => v.into_type(ty),
            BuiltinTypeValue::LocalPref(v) => v.into_type(ty),
            BuiltinTypeValue::AtomicAggregate(v) => v.into_type(ty),
            BuiltinTypeValue::AggregatorInfo(v) => v.into_type(ty),
            BuiltinTypeValue::NextHop(v) => v.into_type(ty),
            BuiltinTypeValue::MultiExitDisc(v) => v.into_type(ty),
        }
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
                BuiltinTypeValue::Nlri(v) => write!(f, "{:?}", v),
                BuiltinTypeValue::IpAddr(v) => write!(f, "{}", v),
                BuiltinTypeValue::Asn(v) => write!(f, "{}", v),
                BuiltinTypeValue::AsPath(v) => {
                    write!(f, "{}", v)
                }
                BuiltinTypeValue::Hop(h) => {
                    write!(f, "{}", h)
                }
                BuiltinTypeValue::Origin(v) => {
                    write!(f, "{:?}", v)
                }
                BuiltinTypeValue::PrefixRoute(r) => write!(f, "{:?}", r),
                BuiltinTypeValue::FlowSpecRoute(r) => write!(f, "{:?}", r),
                BuiltinTypeValue::RouteContext(c) => write!(f, "{:?}", c),
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
                BuiltinTypeValue::NlriStatus(v) => {
                    write!(f, "{}", v)
                }
                BuiltinTypeValue::Provenance(v) => {
                    write!(f, "{}", v)
                }
                BuiltinTypeValue::Bool(v) => write!(f, "{}", v),
                BuiltinTypeValue::HexLiteral(v) => {
                    write!(f, "{}", v)
                }
                BuiltinTypeValue::LocalPref(v) => {
                    write!(f, "{}", v)
                }
                BuiltinTypeValue::AtomicAggregate(v) => {
                    write!(f, "{:?}", v)
                }
                BuiltinTypeValue::AggregatorInfo(v) => {
                    write!(f, "{:?}", v)
                }
                BuiltinTypeValue::NextHop(v) => write!(f, "{}", v),
                BuiltinTypeValue::MultiExitDisc(v) => {
                    write!(f, "{}", v)
                }
                BuiltinTypeValue::PeerId(p) => write!(f, "{}", p),
                BuiltinTypeValue::PeerRibType(v) => write!(f, "{}", v)
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
                BuiltinTypeValue::Nlri(v) => { write!(f, "{:?} (NLRI)", v) }
                BuiltinTypeValue::Provenance(v) => { write!(f, "{} (Provenance Record)", v) }
                BuiltinTypeValue::IpAddr(v) => {
                    write!(f, "{} (IP Address)", v)
                }
                BuiltinTypeValue::Asn(v) => write!(f, "{} (ASN)", v),
                BuiltinTypeValue::AsPath(v) => {
                    write!(f, "{} (AS Path)", v)
                }
                BuiltinTypeValue::Hop(h) => {
                    write!(f, "{} (Hop)", h)
                }
                BuiltinTypeValue::Origin(v) => {
                    write!(f, "{:?} (Origin Type)", v)
                }
                BuiltinTypeValue::PrefixRoute(r) => write!(f, "{:?} (Prefix Route)", r),
                BuiltinTypeValue::FlowSpecRoute(r) => write!(f, "{:?} (FlowSpec Route)", r),
                BuiltinTypeValue::RouteContext(c) => write!(f, "{:?} (Context)", c),
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
                BuiltinTypeValue::NlriStatus(v) => {
                    write!(f, "{} (Route Status)", v)
                }
                BuiltinTypeValue::Bool(v) => write!(f, "{} (Boolean)", v),
                BuiltinTypeValue::HexLiteral(v) => {
                    write!(f, "{} (Hex)", v)
                }
                BuiltinTypeValue::LocalPref(v) => {
                    write!(f, "{} (Local Preference)", v)
                }
                BuiltinTypeValue::AggregatorInfo(v) => {
                    write!(f, "{:?} (Aggregator)", v)
                }
                BuiltinTypeValue::AtomicAggregate(v) => {
                    write!(f, "{:?} (Atomic Aggregate)", v)
                }
                BuiltinTypeValue::NextHop(v) => write!(f, "{} (Next Hop)", v),
                BuiltinTypeValue::MultiExitDisc(v) => {
                    write!(f, "{} (Multi Exit Discriminator)", v)
                }
                BuiltinTypeValue::PeerId(v) => write!(f,"{} (Peer ID)", v),
                BuiltinTypeValue::PeerRibType(v) => write!(f, "{} (Peer Rib Type)", v),
            }
        }
    }
}
