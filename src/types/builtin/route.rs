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
    bgp::{
        communities::{ExtendedCommunity, LargeCommunity},
        message::UpdateMessage,
        route::RouteStatus,
    },
    record::LogicalTime,
};
use std::sync::Arc;

use crate::{
    compile::CompileError,
    traits::{MethodProps, RotoType, TokenConvert},
    types::{
        builtin::attributes::AttrChangeSet, typedef::TypeDef,
        typevalue::TypeValue,
    },
    vm::{Payload, VmError},
};

use super::{
    attributes::ChangedOption, AsPath, Boolean, BuiltinTypeValue, Community,
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
pub struct MaterializedRoute {
    pub prefix: Prefix,
    pub path_attributes: AttrChangeSet,
    pub status: RouteStatus,
}

impl From<RawRouteWithDeltas> for MaterializedRoute {
    fn from(raw_route: RawRouteWithDeltas) -> Self {
        let status = raw_route.status_deltas.current();

        MaterializedRoute {
            prefix: raw_route.prefix.into(), // The roto prefix type
            path_attributes: raw_route.clone_latest_attrs(),
            status,
        }
    }
}

//------------ RawRouteWithDelta ============================================

// This is the default storage data-structure for a RIB, it will store the
// complete BGP message (as an Arc - to avoid duplication for every prefix
// in the NLRI) in a RIB.
//
// It will be stored as a array of bytes, its BGP path attributes can be
// extracted from the original bytes on the fly (with routecore).
// Additionally it features a data-structure that stores the changes made by
// the transformers (filters, etc.) along the way.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RawRouteWithDeltas {
    pub prefix: routecore::addr::Prefix,
    // Arc'ed BGP message
    pub raw_message: Arc<RawBgpMessage>,
    // history of recorded changes to the route
    attribute_deltas: AttributeDeltaList,
    // history of status changes to the route
    status_deltas: RouteStatusDeltaList,
}

impl RawRouteWithDeltas {
    pub fn new_with_message(
        delta_id: (RotondaId, LogicalTime),
        prefix: routecore::addr::Prefix,
        raw_message: UpdateMessage<bytes::Bytes>,
    ) -> Self {
        let raw_message = RawBgpMessage::new(delta_id, raw_message);

        Self {
            prefix,
            raw_message: Arc::new(raw_message),
            attribute_deltas: AttributeDeltaList::new(),
            status_deltas: RouteStatusDeltaList(vec![RouteStatusDelta::new(
                delta_id,
            )]),
        }
    }

    pub fn new_with_message_ref(
        delta_id: (RotondaId, LogicalTime),
        prefix: routecore::addr::Prefix,
        raw_message: &Arc<RawBgpMessage>,
    ) -> Self {
        Self {
            prefix,
            raw_message: Arc::clone(raw_message),
            attribute_deltas: AttributeDeltaList::new(),
            status_deltas: RouteStatusDeltaList(vec![RouteStatusDelta::new(
                delta_id,
            )]),
        }
    }

    pub fn new_delta(&self) -> AttrChangeSet {
        if let Some(attr_set) = self.attribute_deltas.deltas.last() {
            attr_set.attributes.clone()
        } else {
            self.changeset_from_raw()
        }
    }

    pub fn add_delta(
        &mut self,
        delta_id: (RotondaId, LogicalTime),
        attributes_list: AttrChangeSet,
    ) {
        self.attribute_deltas
            .add_new_delta(AttributeDelta::new(delta_id, attributes_list));
    }

    pub fn clone_latest_attrs(&self) -> AttrChangeSet {
        self.attribute_deltas
            .get_latest_delta()
            .cloned()
            .unwrap_or_else(|| self.changeset_from_raw())
    }

    pub fn get_latest_delta(&self) -> Option<&AttrChangeSet> {
        self.attribute_deltas.get_latest_delta()
    }

    pub fn get_attrs_for_rotonda_id(
        &self,
        rotonda_id: RotondaId,
    ) -> Option<&AttrChangeSet> {
        self.attribute_deltas
            .deltas
            .iter()
            .find(|d| d.delta_id.0 == rotonda_id)
            .map(|d| &d.attributes)
    }

    fn changeset_from_raw(&self) -> AttrChangeSet {
        AttrChangeSet {
            as_path: ChangedOption {
                value: self
                    .raw_message
                    .raw_message
                    .aspath()
                    .map(|p| p.into()),
                changed: false,
            },
            origin_type: ChangedOption {
                value: self.raw_message.raw_message.origin(),
                changed: false,
            },
            next_hop: ChangedOption {
                value: self.raw_message.raw_message.next_hop(),
                changed: false,
            },
            multi_exit_discriminator: ChangedOption {
                value: self.raw_message.raw_message.multi_exit_desc(),
                changed: false,
            },
            local_pref: ChangedOption {
                value: self.raw_message.raw_message.local_pref(),
                changed: false,
            },
            atomic_aggregate: ChangedOption {
                value: Some(
                    self.raw_message.raw_message.is_atomic_aggregate(),
                ),
                changed: false,
            },
            aggregator: ChangedOption {
                value: self.raw_message.raw_message.aggregator(),
                changed: false,
            },
            communities: ChangedOption {
                value: self.raw_message.raw_message.all_communities(),
                changed: false,
            },
            originator_id: ChangedOption {
                value: None,
                changed: false,
            },
            cluster_list: ChangedOption {
                value: None,
                changed: false,
            },
            extended_communities: ChangedOption {
                value: self
                    .raw_message
                    .raw_message
                    .ext_communities()
                    .map(|c| c.collect::<Vec<ExtendedCommunity>>()),
                changed: false,
            },
            as4_path: ChangedOption {
                value: self.raw_message.raw_message.as4path(),
                changed: false,
            },
            as4_aggregator: ChangedOption {
                value: None,
                changed: false,
            },
            connector: ChangedOption {
                value: None,
                changed: false,
            },
            as_path_limit: ChangedOption {
                value: None,
                changed: false,
            },
            pmsi_tunnel: ChangedOption {
                value: None,
                changed: false,
            },
            ipv6_extended_communities: ChangedOption {
                value: None,
                changed: false,
            },
            large_communities: ChangedOption {
                value: self
                    .raw_message
                    .raw_message
                    .large_communities()
                    .map(|c| c.collect::<Vec<LargeCommunity>>()),
                changed: false,
            },
            bgpsec_as_path: ChangedOption {
                value: None,
                changed: false,
            },
            attr_set: ChangedOption {
                value: None,
                changed: false,
            },
            rsrvd_development: ChangedOption {
                value: None,
                changed: false,
            },
        }
    }

    pub fn iter_deltas(&self) -> impl Iterator<Item = &AttributeDelta> + '_ {
        self.attribute_deltas.deltas.iter()
    }
}

//------------ RouteDeltas --------------------------------------------------

// The history of changes to the route in the form of a list of lists. The
// inner lists hold the all the attributes that were changed by a Rotonda
// writer in in one go (so with one logical timestamp). The outer list holds
// those attribute lists in chronological order, with newest first.
#[derive(Debug, Clone, Eq, PartialEq, Default)]
struct AttributeDeltaList {
    deltas: Vec<AttributeDelta>,
}

impl AttributeDeltaList {
    fn new() -> Self {
        Self { deltas: vec![] }
    }

    // Gets the most recently added value for this Path Attribute.
    fn get_latest_delta(&self) -> Option<&AttrChangeSet> {
        self.deltas.last().map(|d| &d.attributes)
    }

    // Adds a new delta to the list.
    fn add_new_delta(&mut self, delta: AttributeDelta) {
        let mut res = Vec::with_capacity(self.deltas.len() + 1);
        res.push(delta);
        res.extend_from_slice(self.deltas.as_slice());
        self.deltas = res;
    }
}

//------------ AttributeDelta ----------------------------------------------

// A set of attribute changes that were atomically created by a Rotonda
// writer in one go (with one logical timestamp).
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AttributeDelta {
    delta_id: (RotondaId, LogicalTime),
    attributes: AttrChangeSet,
}

impl AttributeDelta {
    pub fn new(
        delta_id: (RotondaId, LogicalTime),
        attributes: AttrChangeSet,
    ) -> Self {
        Self {
            delta_id,
            attributes,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct RouteStatusDelta {
    delta_id: (RotondaId, LogicalTime),
    status: RouteStatus,
}

impl RouteStatusDelta {
    pub fn new(delta_id: (RotondaId, LogicalTime)) -> Self {
        Self {
            delta_id,
            status: RouteStatus::Empty,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct RouteStatusDeltaList(Vec<RouteStatusDelta>);

impl RouteStatusDeltaList {
    pub fn current(&self) -> RouteStatus {
        self.0.iter().last().unwrap().status
    }
}

//------------ RawBgpMessage ------------------------------------------------

// A data-structure that stores the array of bytes of the incoming BGP Update
// message, together with its logical timestamp and an ID of the instance
// and/or unit that received it originally.
//
// The `attr_cache` AttributeList allows readers to get a reference to a path
// attribute. This avoids having to clone from the AttributeLists in the
// iterator over the latest attributes.
#[derive(Debug)]
pub struct RawBgpMessage {
    message_id: (RotondaId, LogicalTime),
    raw_message: UpdateMessage<bytes::Bytes>,
    // attr_cache: AttrChangeSet
}

impl RawBgpMessage {
    fn new(
        message_id: (RotondaId, u64),
        raw_message: UpdateMessage<bytes::Bytes>,
    ) -> Self {
        Self {
            message_id,
            raw_message,
        }
    }
}

impl PartialEq for RawBgpMessage {
    fn eq(&self, other: &Self) -> bool {
        self.message_id == other.message_id
    }
}

impl Eq for RawBgpMessage {}

impl RawRouteWithDeltas {
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

impl RotoType for RawRouteWithDeltas {
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

impl From<RawRouteWithDeltas> for TypeValue {
    fn from(val: RawRouteWithDeltas) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Route(Some(val)))
    }
}

impl std::fmt::Display for RawRouteWithDeltas {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "prefix : {}", self.prefix)?;
        writeln!(f, "raw    : {:#?}", self.raw_message)?;
        writeln!(f, "deltas : {:#?}", self.attribute_deltas)?;
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

impl Payload for RawRouteWithDeltas {
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

#[derive(Debug)]
pub enum RouteStatusToken {
    IsInConvergence = 1,
    IsUpToDate = 2,
    IsStale = 3,
    IsStartOfRouteRefresh = 4,
    IsWithdrawn = 5,
    IsEmpty = 6,
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

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct RotondaId(pub usize);
