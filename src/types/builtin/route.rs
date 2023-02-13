//============ Route types ==================================================

// routcore/rotonda-runtime               roto
//                                        ┌─────────────┐
//                                        ├─────────────┤
//                 ┌─────────────┐        ├─────────────┤
//               ┌─▶   Updates   ├─write──▶    Route    │
// ┌────────────┐│ └─────────────┘        └──────▲──────┘
// │ BgpMessage ├┤                               │
// └────────────┘│ ┌─────────────┐               │
//               └─▶ Withdrawals │──change ──────┘
//                 └─────────────┘  status

use routecore::{
    bgp::message::update::{PathAttributeType, UpdateMessage},
    record::LogicalTime
};
use std::collections::HashMap;

use crate::{
    compile::CompileError,
    traits::{MethodProps, RotoType, TokenConvert},
    types::{typedef::TypeDef, typevalue::TypeValue},
    vm::{Payload, VmError},
};

use super::{
    Aggregator, AsPath, Asn, Boolean, BuiltinTypeValue, Communities,
    Community, IpAddress, LocalPref, MultiExitDisc, NextHop, OriginType,
    Prefix,
};

//============ Route ========================================================

// An RFC 4271 Route. The RFC:

// ```
// Route
// A unit of information that pairs a set of destinations with the
// attributes of a path to those destinations.  The set of
// destinations are systems whose IP addresses are contained in one
// IP address prefix carried in the Network Layer Reachability
// Information (NLRI) field of an UPDATE message.  The path is the
// information reported in the path attributes field of the same
// UPDATE message.
// ```

// So, a data structure in the form of (prefix, path attributes) with some
// added metadata, that is strictly speaking not part of a RFC4271 route.
// This is not the data-structure that Rotonda uses to store by default in
// a RIB (that is the RawRouteDelta down below), but the type that is used
// serialize the data into on export and transport.

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Route {
    pub prefix: Prefix,
    pub path_attributes: PathAttributes,
    pub status: RouteStatus,
}

//------------ BGP Path Attributes ------------------------------------------

// The record that holds all the BGP attributes in a BGP message to be used
// by the route type. Note that RawRouteDelta have their own path atttributes
// delta types.
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct PathAttributes {
    pub as_path: AsPath,
    pub origin: OriginType,
    pub next_hop: NextHop,
    pub multi_exit_disc: MultiExitDisc,
    pub local_pref: LocalPref,
    pub is_atomic_aggregate: bool,
    pub aggregator: Aggregator,
    pub communities: Communities,
    // pub originator_id: Option<OriginatorId>,
    // pub cluster_list: Vec<ClusterId>,
    // pub pmsi_tunnel: Option<PmsiTunnel>,
    // pub tunnel_encapsulation: Option<TunnelEncapsulation>,
    // pub traffic_engineering: Option<TrafficEngineering>,
    // pub aigp: Option<Aigp>,
    // pub pe_distinguisher_labels: Vec<PeDistinguisherLabel>,
    // pub bgp_ls: Option<BgpLs>,
    // pub bgpsec_path: Option<BgpsecPath>,
    // pub sfp: Option<Sfp>,
    // pub bfd_discriminator: Option<BfdDiscriminator>,
    // pub bgp_prefix_sid,
    // pub attr_set: Option<AttrSet>,
    // pub unknown: Vec<UnknownAttribute>,
}

//============ RawRouteDelta ================================================

// This is the default storage data-structure for a RIB, it will store the
// complete BGP message (as an Arc - to avoid duplication for every prefix
// in the NLRI) in a RIB.
//
// It will be stored as a array of bytes, its BGP path attributes can be
// extracted from the original bytes on the fly (with routecore).
// Additionally it features a data-structure that stores the changes made by
// the transformers (filters, etc.) along the way.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RawRouteDelta {
    prefix: Prefix,
    // Arc'ed BGP message
    raw_message: RawBgpMessage,
    // history of recorded changes to the route
    attributes_delta: AttributeDeltas,
    // history of status changes to the route
    status_deltas: RouteStatusDeltas,
}

//------------ AtributeDeltas -----------------------------------------------

// The history of changes to the route in the form of a hashmap, one entry
// per path atritbute. Each entry holds a vec of changes sorted by logical
// time, while also registereing the unit/instance that wrote the change.
#[derive(Debug, Clone, Eq, PartialEq)]
struct AttributeDeltas(HashMap<PathAttributeType, Vec<AttributeDelta>>);

#[derive(Debug, Clone, Eq, PartialEq)]
struct AttributeDelta {
    delta_id: (RotondaId, LogicalTime),
    attribute: AttributeTypeValue,
}

//------------ Path Attribute TypeValues ------------------------------------

// A data-structure that stores the changes made by the transformers on a
// BGP message.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AttributeTypeValue {
    AsPath(AsPath),
    Origin(Asn),
    NextHop(IpAddress),
    MultiExitDiscriminator(MultiExitDisc),
    LocalPref(LocalPref),
    AtomicAggregate(bool),
    Aggregator(Aggregator),
    Communities(Vec<Community>),
    // pub originator_id: Option<OriginatorId>,
    // pub cluster_list: Vec<ClusterId>,
    // pub pmsi_tunnel: Option<PmsiTunnel>,
    // pub tunnel_encapsulation: Option<TunnelEncapsulation>,
    // pub traffic_engineering: Option<TrafficEngineering>,
    // pub aigp: Option<Aigp>,
    // pub pe_distinguisher_labels: Vec<PeDistinguisherLabel>,
    // pub bgp_ls: Option<BgpLs>,
    // pub bgpsec_path: Option<BgpsecPath>,
    // pub sfp: Option<Sfp>,
    // pub bfd_discriminator: Option<BfdDiscriminator>,
    // pub bgp_prefix_sid,
    // pub attr_set: Option<AttrSet>,
    // pub unknown: Vec<UnknownAttribute>,
}

//------------ Route Status -------------------------------------------------

// Status is piece of metadata that writes some (hopefully) relevant state of
// per-peer BGP session into every route. The goal is to be able to enable
// the logic in `rib-units` to decide whether routes should be send to its
// output and to be able output this information to API clients, without
// having to go back to the units that keep the per-peer session state.
#[derive(Debug, Eq, PartialEq, Copy, Clone, Default)]
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
    // Status not relevant, e.g. a RIB that holds archived routes.
    #[default]
    Empty,
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct RouteStatusDelta {
    delta_id: (RotondaId, LogicalTime),
    status: RouteStatus,
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct RouteStatusDeltas(Vec<RouteStatusDelta>);

impl RouteStatusDeltas {
    pub fn current(&self) -> RouteStatus {
        self.0.iter().last().unwrap().status
    }
}

//------------ RawBgpMessage ------------------------------------------------

// A data-structure that stores the array of bytes of the incoming BGP Update
// message, together with its logical timestamp and an ID of the instance
// and/or unit that received it originally.
#[derive(Debug, Clone)]
pub struct RawBgpMessage {
    message_id: (RotondaId, LogicalTime),
    raw_message: UpdateMessage<bytes::Bytes>,
}

impl PartialEq for RawBgpMessage {
    fn eq(&self, other: &Self) -> bool {
        self.message_id == other.message_id
    }
}

impl Eq for RawBgpMessage {}

impl TryFrom<&RawRouteDelta> for Route {
    type Error = VmError;

    fn try_from(raw_route: &RawRouteDelta) -> Result<Self, Self::Error> {
        let u_bytes = &raw_route.raw_message.raw_message;

        Ok(Route {
            prefix: raw_route.prefix,
            path_attributes: PathAttributes {
                as_path: AsPath(u_bytes.aspath()),
                origin: OriginType(u_bytes.origin()),
                next_hop: NextHop(u_bytes.next_hop()),
                multi_exit_disc: MultiExitDisc(u_bytes.multi_exit_desc()),
                local_pref: LocalPref(u_bytes.local_pref()),
                is_atomic_aggregate: u_bytes.is_atomic_aggregate(),
                aggregator: Aggregator(u_bytes.aggregator()),
                communities: Communities(u_bytes.all_communities().map(
                    |v| {
                        v.iter()
                            .map(|c| Community(Some(*c)))
                            .collect::<Vec<_>>()
                    },
                )),
            },
            status: raw_route.status_deltas.current(),
        })
    }
}

impl RawRouteDelta {
    pub(crate) fn get_props_for_method_static(
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
}

impl RotoType for RawRouteDelta {
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
                Ok(TypeValue::Builtin(BuiltinTypeValue::Route(Some(self))))
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
        _method_token: usize,
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }
}

impl From<RawRouteDelta> for TypeValue {
    fn from(val: RawRouteDelta) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Route(Some(val)))
    }
}

impl std::fmt::Display for RawRouteDelta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "prefix : {}", self.prefix)?;
        writeln!(f, "raw    : {:#?}", self.raw_message)?;
        writeln!(f, "deltas : {:#?}", self.attributes_delta)?;
        writeln!(f, "status : {}", self.status_deltas.current())
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

impl Payload for RawRouteDelta {
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
        TypeValue::Builtin(BuiltinTypeValue::Route(Some(self)))
    }
}

impl RotoType for RouteStatus {
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

impl std::fmt::Display for PathAttributes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "BGP attributes: {:?} {:?}",
            self.as_path, self.communities
        )
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct RotondaId(usize);
