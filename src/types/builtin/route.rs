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

use log::trace;
use routecore::bgp::message::SessionConfig;
use serde::{Serialize, Deserialize};
use smallvec::SmallVec;
use std::{net::IpAddr, sync::Arc};

/// Lamport Timestamp. Used to order messages between units/systems.
pub type LogicalTime = u64;

use crate::{
    attr_change_set::{ReadOnlyScalarOption, Todo},
    compile::CompileError,
    traits::{RotoType, Token},
    types::{
        enum_types::EnumVariant,
        typedef::{MethodProps, TypeDef},
        typevalue::TypeValue,
    },
    vm::{StackValue, VmError, CommandArg}, ast::StringLiteral,
};

use super::{
    AsPath, Asn, BuiltinTypeValue, IpAddress, NextHop, OriginType, Prefix,
    RouteStatus,
};
use crate::attr_change_set::{
    AttrChangeSet, ScalarOption, ScalarValue, VectorOption, VectorValue,
};

//============ MaterializedRoute ============================================

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

#[derive(Debug, Eq, PartialEq, Clone, Hash, Serialize)]
pub struct MaterializedRoute {
    pub route: AttrChangeSet,
    pub status: RouteStatus,
    route_id: (RotondaId, LogicalTime),
}

impl From<RawRouteWithDeltas> for MaterializedRoute {
    fn from(raw_route: RawRouteWithDeltas) -> Self {
        let status = raw_route.status_deltas.current();
        let route_id = raw_route.raw_message.message_id;

        MaterializedRoute {
            route: raw_route.take_latest_attrs(),
            status: status.into(),
            route_id,
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
// Additionally it features a data-structure (AttributesDeltaList) that
// stores the changes made by the transformers (filters, etc.) along the way.
//
// Each Delta describes the complete state of all the attributes at the time it
// was stored in the RawRouteWithDeltas instance. So it reflects both the
// original attributes from the raw message, their modifications and the
// newly set attributes.
#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize)]
#[serde(into = "MaterializedRoute")]
pub struct RawRouteWithDeltas {
    pub prefix: Prefix,
    // Arc'ed BGP message
    pub raw_message: Arc<BgpUpdateMessage>,
    // The IP address of the BGP speaker that originated the route, if known.
    peer_ip: Option<IpAddress>,
    // The ASN of the BGP speaker that originated the route, if known.
    peer_asn: Option<Asn>,
    // The ID (e.g. "<ip addr>:<port>", or "<BMP initiate sysName>") of the
    // connected router from which the route was received, if known.
    router_id: Option<Arc<String>>,
    // history of recorded changes to the route
    attribute_deltas: AttributeDeltaList,
    // history of status changes to the route
    status_deltas: RouteStatusDeltaList,
}

impl RawRouteWithDeltas {
    pub fn new_with_message(
        delta_id: (RotondaId, LogicalTime),
        prefix: Prefix,
        raw_message: UpdateMessage,
        route_status: RouteStatus,
    ) -> Self {
        let raw_message = BgpUpdateMessage::new(delta_id, raw_message);
        let mut attribute_deltas = AttributeDeltaList::new();
        let (peer_ip, peer_asn, router_id) = (None, None, None);
        // This stores the attributes in the raw message as the first delta.
        attribute_deltas
            .store_delta(AttributeDelta::new(
                delta_id,
                0,
                raw_message
                    .raw_message
                    .create_changeset(prefix, peer_ip, peer_asn, router_id.clone()),
            ))
            .unwrap();

        Self {
            prefix,
            raw_message: Arc::new(raw_message),
            peer_ip,
            peer_asn,
            router_id,
            attribute_deltas,
            status_deltas: RouteStatusDeltaList::new(RouteStatusDelta::new(
                delta_id,
                route_status,
            )),
        }
    }

    pub fn new_with_message_ref(
        delta_id: (RotondaId, LogicalTime),
        prefix: Prefix,
        raw_message: &Arc<BgpUpdateMessage>,
        route_status: RouteStatus,
    ) -> Self {
        Self {
            prefix,
            raw_message: Arc::clone(raw_message),
            peer_ip: None,
            peer_asn: None,
            router_id: None,
            attribute_deltas: AttributeDeltaList::new(),
            status_deltas: RouteStatusDeltaList::new(RouteStatusDelta::new(
                delta_id,
                route_status,
            )),
        }
    }

    pub fn with_peer_ip(self, peer_ip: IpAddr) -> Self {
        Self {
            prefix: self.prefix,
            raw_message: self.raw_message,
            peer_ip: Some(IpAddress::new(peer_ip)),
            peer_asn: self.peer_asn,
            router_id: self.router_id,
            attribute_deltas: self.attribute_deltas,
            status_deltas: self.status_deltas,
        }
    }

    pub fn with_peer_asn(self, peer_asn: routecore::asn::Asn) -> Self {
        Self {
            prefix: self.prefix,
            raw_message: self.raw_message,
            peer_ip: self.peer_ip,
            peer_asn: Some(Asn::new(peer_asn)),
            router_id: self.router_id,
            attribute_deltas: self.attribute_deltas,
            status_deltas: self.status_deltas,
        }
    }

    pub fn with_router_id(self, router_id: Arc<String>) -> Self {
        Self {
            prefix: self.prefix,
            raw_message: self.raw_message,
            peer_ip: self.peer_ip,
            peer_asn: self.peer_asn,
            router_id: Some(router_id),
            attribute_deltas: self.attribute_deltas,
            status_deltas: self.status_deltas,
        }
    }

    pub fn peer_ip(&self) -> Option<IpAddr> {
        self.peer_ip.map(|ip| ip.0)
    }

    pub fn peer_asn(&self) -> Option<routecore::asn::Asn> {
        self.peer_asn.map(|asn| asn.0)
    }

    pub fn router_id(&self) -> Option<Arc<String>> {
        self.router_id.clone()
    }

    pub fn update_status(
        &mut self,
        delta_id: (RotondaId, LogicalTime),
        new_status: RouteStatus,
    ) {
        let delta = RouteStatusDelta::new(delta_id, new_status);
        self.status_deltas.0.push(delta);
    }

    // Get a clone of the latest delta, or of the original attributes from
    // the raw message, if no delta has been added (yet).
    fn clone_latest_attrs(&self) -> AttrChangeSet {
        if let Some(attr_set) = self.attribute_deltas.deltas.last() {
            attr_set.attributes.clone()
        } else {
            self.raw_message.raw_message.create_changeset(
                self.prefix,
                self.peer_ip,
                self.peer_asn,
                self.router_id.clone(),
            )
        }
    }

    // Return a clone of the latest attribute set, so that changes can be
    // made to it by the caller.
    pub fn open_new_delta(
        &mut self,
        delta_id: (RotondaId, LogicalTime),
    ) -> Result<AttributeDelta, VmError> {
        let delta_index = self.attribute_deltas.acquire_new_delta()?;

        Ok(AttributeDelta {
            attributes: self.clone_latest_attrs(),
            delta_id,
            delta_index,
        })
    }

    // Get either the moved last delta (it is removed from the Delta list), or
    // get a freshly rolled ChangeSet from the raw message, if there are no
    // deltas.
    pub fn take_latest_attrs(mut self) -> AttrChangeSet {
        if self.attribute_deltas.deltas.is_empty() {
            return self.raw_message.raw_message.create_changeset(
                self.prefix,
                self.peer_ip,
                self.peer_asn,
                self.router_id,
            );
        }

        self.attribute_deltas
            .deltas
            .remove(self.attribute_deltas.deltas.len() - 1)
            .attributes
    }

    // Get a reference to the current state of the attributes, including the
    // original raw message. In the latter case it will copy the raw message
    // attributes into the attribute deltas, and return a reference from that,
    // hence the `&mut self`.
    pub fn get_latest_attrs(&mut self) -> &AttrChangeSet {
        if self.attribute_deltas.deltas.is_empty() {
            self.attribute_deltas
                .store_delta(AttributeDelta::new(
                    self.raw_message.message_id,
                    0,
                    self.raw_message.raw_message.create_changeset(
                        self.prefix,
                        self.peer_ip,
                        self.peer_asn,
                        self.router_id.clone(),
                    ),
                ))
                .unwrap();
        }
        self.attribute_deltas.get_latest_change_set().unwrap()
    }

    // Get a ChangeSet that was added by a specific unit, e.g. a filter.
    pub fn get_delta_for_rotonda_id(
        &self,
        rotonda_id: RotondaId,
    ) -> Option<&AttrChangeSet> {
        self.attribute_deltas
            .deltas
            .iter()
            .find(|d| d.delta_id.0 == rotonda_id)
            .map(|d| &d.attributes)
    }

    // Add a ChangeSet with some metadata to this RawRouteWithDelta.
    pub fn store_delta(
        &mut self,
        attr_delta: AttributeDelta,
    ) -> Result<(), VmError> {
        self.attribute_deltas.store_delta(attr_delta)
    }

    pub fn iter_deltas(&self) -> impl Iterator<Item = &AttributeDelta> + '_ {
        self.attribute_deltas.deltas.iter()
    }

    pub(crate) fn get_props_for_field(
        field_name: &crate::ast::Identifier,
    ) -> Result<(TypeDef, crate::traits::Token), CompileError>
    where
        Self: std::marker::Sized,
    {
        match field_name.ident.as_str() {
            "prefix" => Ok((
                TypeDef::Prefix,
                Token::FieldAccess(vec![RouteToken::Prefix.into()]),
            )),
            "as-path" => Ok((
                TypeDef::AsPath,
                Token::FieldAccess(vec![RouteToken::AsPath.into()]),
            )),
            "origin-type" => Ok((
                TypeDef::OriginType,
                Token::FieldAccess(vec![RouteToken::OriginType.into()]),
            )),
            "next-hop" => Ok((
                TypeDef::NextHop,
                Token::FieldAccess(vec![RouteToken::NextHop.into()]),
            )),
            "multi-exit-disc" => Ok((
                TypeDef::MultiExitDisc,
                Token::FieldAccess(vec![RouteToken::MultiExitDisc.into()]),
            )),
            "local-pref" => Ok((
                TypeDef::LocalPref,
                Token::FieldAccess(vec![RouteToken::LocalPref.into()]),
            )),
            "atomic-aggregate" => Ok((
                TypeDef::Boolean,
                Token::FieldAccess(vec![RouteToken::AtomicAggregate.into()]),
            )),
            "aggregator" => Ok((
                TypeDef::AtomicAggregator,
                Token::FieldAccess(vec![RouteToken::AtomicAggregator.into()]),
            )),
            "communities" => Ok((
                TypeDef::List(Box::new(TypeDef::Community)),
                Token::FieldAccess(vec![RouteToken::Communities.into()]),
            )),
            "status" => Ok((
                TypeDef::RouteStatus,
                Token::FieldAccess(vec![RouteToken::Status.into()]),
            )),
            "peer_ip" => Ok((
                TypeDef::IpAddress,
                Token::FieldAccess(vec![RouteToken::PeerIp.into()]),
            )),
            "peer_asn" => Ok((
                TypeDef::Asn,
                Token::FieldAccess(vec![RouteToken::PeerAsn.into()]),
            )),
            _ => Err(format!(
                "Unknown method '{}' for type Route",
                field_name.ident
            )
            .into()),
        }
    }

    pub(crate) fn get_value_ref_for_field(
        &self,
        field_token: usize,
    ) -> Option<&TypeValue> {
        let current_set = self.attribute_deltas.get_latest_change_set()?;

        match field_token.into() {
            RouteToken::Prefix => current_set.prefix.as_ref(),
            RouteToken::AsPath => current_set.as_path.as_ref(),
            RouteToken::OriginType => current_set.origin_type.as_ref(),
            RouteToken::NextHop => current_set.next_hop.as_ref(),
            RouteToken::MultiExitDisc => {
                current_set.multi_exit_discriminator.as_ref()
            }
            RouteToken::LocalPref => current_set.local_pref.as_ref(),
            RouteToken::AtomicAggregate => {
                current_set.atomic_aggregate.as_ref()
            }
            RouteToken::AtomicAggregator => current_set.aggregator.as_ref(),
            RouteToken::Communities => current_set.communities.as_ref(),
            RouteToken::Status => self.status_deltas.current_as_ref(),
            RouteToken::PeerIp => current_set.peer_ip.as_ref(),
            RouteToken::PeerAsn => current_set.peer_asn.as_ref(),
        }
    }

    pub(crate) fn get_field_by_index(
        &self,
        field_token: usize,
    ) -> Option<TypeValue> {
        match field_token.into() {
            RouteToken::AsPath => self.raw_message.raw_message.0.aspath().map(|p| p.to_hop_path()).map(TypeValue::from),
            RouteToken::OriginType => {
                self.raw_message.raw_message.0.origin().map(TypeValue::from)
            }
            RouteToken::NextHop => self
                .raw_message
                .raw_message
                .0
                .next_hop()
                .map(TypeValue::from),
            RouteToken::MultiExitDisc => self
                .raw_message
                .raw_message
                .0
                .multi_exit_desc()
                .map(TypeValue::from),
            RouteToken::LocalPref => self
                .raw_message
                .raw_message
                .0
                .local_pref()
                .map(TypeValue::from),
            RouteToken::AtomicAggregate => Some(TypeValue::from(
                self.raw_message.raw_message.0.is_atomic_aggregate(),
            )),
            RouteToken::AtomicAggregator => self
                .raw_message
                .raw_message
                .0
                .aggregator()
                .map(TypeValue::from),
            RouteToken::Communities => self
                .raw_message
                .raw_message
                .0
                .all_communities()
                .map(TypeValue::from),
            RouteToken::Prefix => Some(self.prefix.into()),
            RouteToken::Status => Some(self.status_deltas.current()),
            RouteToken::PeerIp => self.peer_ip.map(TypeValue::from),
            RouteToken::PeerAsn => self.peer_asn.map(TypeValue::from),
            // _ => None,
            // originator_id: ChangedOption {
            //     value: None,
            //     changed: false,
            // },
            // cluster_list: ChangedOption {
            //     value: None,
            //     changed: false,
            // },
            // RouteToken::ExtendedCommunities => self.raw_message.raw_message.ext_communities()
            //         .map(|c| c.collect::<Vec<ExtendedCommunity>>()).into(),
            // RouteToken::As4Path => self.raw_message.raw_message.as4path(),
            // as4_aggregator: ChangedOption {
            //     value: None,
            //     changed: false,
            // },
            // connector: ChangedOption {
            //     value: None,
            //     changed: false,
            // },
            // as_path_limit: ChangedOption {
            //     value: None,
            //     changed: false,
            // },
            // pmsi_tunnel: ChangedOption {
            //     value: None,
            //     changed: false,
            // },
            // ipv6_extended_communities: ChangedOption {
            //     value: None,
            //     changed: false,
            // },
            // RouteToken::largeCommunities => self.raw_message.raw_message
            //         .large_communities()
            //         .map(|c| c.collect::<Vec<LargeCommunity>>()).into()
            // bgpsec_as_path: ChangedOption {
            //     value: None,
            //     changed: false,
            // },
            // attr_set: ChangedOption {
            //     value: None,
            //     changed: false,
            // },
            // rsrvd_development: ChangedOption {
            //     value: None,
            //     changed: false,
            // },
        }
    }

    pub fn status(&self) -> RouteStatus {
        if let TypeValue::Builtin(BuiltinTypeValue::RouteStatus(
            route_status,
        )) = self.status_deltas.current()
        {
            route_status
        } else {
            unreachable!() // When we were constructed a RouteStatus was passed in so we always have at least one, and
                           // there shouldn't be a way to store something other than a RouteStatus as a subsequent item
                           // in the route status delta list so it also shouldn't be that we matched a different type.
        }
    }
}

//------------ RouteDeltas --------------------------------------------------

// The history of changes to this route. Each Delta holds the attributes that
// were originally present in the raw message, their modifications and newly
// created ones.
//
// The list of deltas describes the changes that were made by one Rotonda
// unit along the way.
#[derive(Debug, Clone, Eq, PartialEq, Default, Hash, Serialize)]
struct AttributeDeltaList {
    deltas: Vec<AttributeDelta>,
    // The delta that was handed out the most recently. This is the only
    // delta that can be written back!
    locked_delta: Option<usize>,
}

impl AttributeDeltaList {
    fn new() -> Self {
        Self {
            deltas: vec![],
            locked_delta: None,
        }
    }

    // Gets the most recently added delta in this list.
    fn get_latest_change_set(&self) -> Option<&AttrChangeSet> {
        self.deltas.last().map(|d| &d.attributes)
    }

    // Adds a new delta to the list.
    fn store_delta(&mut self, delta: AttributeDelta) -> Result<(), VmError> {
        if let Some(locked_delta) = self.locked_delta {
            if locked_delta != delta.delta_index {
                trace!("{:?} {}", self.locked_delta, delta.delta_index);
                return Err(VmError::DeltaLocked);
            }
        }

        self.deltas.push(delta);
        self.locked_delta = None;

        Ok(())
    }

    fn acquire_new_delta(&mut self) -> Result<usize, VmError> {
        if self.locked_delta.is_none() {
            let delta_index = self.deltas.len();
            self.locked_delta = Some(delta_index);
            Ok(delta_index)
        } else {
            Err(VmError::DeltaLocked)
        }
    }
}

//------------ AttributeDelta ----------------------------------------------

// A set of attribute changes that were atomically created by a Rotonda
// unit in one go (with one logical timestamp).
#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize)]
pub struct AttributeDelta {
    delta_id: (RotondaId, LogicalTime),
    delta_index: usize,
    pub attributes: AttrChangeSet,
}

impl AttributeDelta {
    fn new(
        delta_id: (RotondaId, LogicalTime),
        delta_index: usize,
        attributes: AttrChangeSet,
    ) -> Self {
        Self {
            delta_id,
            delta_index,
            attributes,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize)]
struct RouteStatusDelta {
    delta_id: (RotondaId, LogicalTime),
    status: RouteStatus,
}

impl RouteStatusDelta {
    pub fn new(
        delta_id: (RotondaId, LogicalTime),
        status: RouteStatus,
    ) -> Self {
        Self { delta_id, status }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize)]
struct RouteStatusDeltaList(SmallVec<[RouteStatusDelta; 2]>);

impl RouteStatusDeltaList {
    pub fn new(delta: RouteStatusDelta) -> Self {
        let mut v = SmallVec::with_capacity(1);
        v.push(delta);
        Self(v)
    }

    pub fn current(&self) -> TypeValue {
        self.0.iter().last().unwrap().status.into()
    }

    pub fn current_as_ref(&self) -> Option<&TypeValue> {
        None
    }
}

//------------ BgpUpdateMessage ------------------------------------------------

// A data-structure that stores the array of bytes of the incoming BGP Update
// message, together with its logical timestamp and an ID of the instance
// and/or unit that received it originally.
//
// The `attr_cache` AttributeList allows readers to get a reference to a path
// attribute. This avoids having to clone from the AttributeLists in the
// iterator over the latest attributes.
#[derive(Debug, Serialize)]
pub struct BgpUpdateMessage {
    message_id: (RotondaId, LogicalTime),
    raw_message: UpdateMessage,
}

impl BgpUpdateMessage {
    pub fn new(
        message_id: (RotondaId, u64),
        raw_message: UpdateMessage,
    ) -> Self {
        Self {
            message_id,
            raw_message,
        }
    }

    pub fn message_id(&self) -> (RotondaId, u64) {
        self.message_id
    }

    pub fn raw_message(&self) -> &UpdateMessage {
        &self.raw_message
    }
}

impl PartialEq for BgpUpdateMessage {
    fn eq(&self, other: &Self) -> bool {
        self.message_id == other.message_id
    }
}

impl std::hash::Hash for BgpUpdateMessage {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.raw_message.hash(state);
    }
}

impl Eq for BgpUpdateMessage {}

impl BgpUpdateMessage {
    pub(crate) fn get_props_for_field(
        field_name: &crate::ast::Identifier,
    ) -> Result<(TypeDef, crate::traits::Token), CompileError>
    where
        Self: std::marker::Sized,
    {
        match field_name.ident.as_str() {
            // `nlris` is a fake field (at least for now), it just serves to
            // host the `afi` and `safi` field.
            "nlris" => {
                Ok((TypeDef::BgpUpdateMessage, Token::FieldAccess(vec![])))
            }
            "afi" => Ok((
                TypeDef::ConstEnumVariant("AFI".into()),
                Token::FieldAccess(vec![usize::from(
                    BgpUpdateMessageToken::Afi,
                ) as u8]),
            )),
            "safi" => Ok((
                TypeDef::ConstEnumVariant("SAFI".into()),
                Token::FieldAccess(vec![usize::from(
                    BgpUpdateMessageToken::Safi,
                ) as u8]),
            )),
            _ => Err(format!(
                "Unknown field '{}' for type BgpUpdateMessage",
                field_name.ident
            )
            .into()),
        }
    }

    pub(crate) fn get_value_owned_for_field(
        &self,
        field_token: usize,
    ) -> Option<TypeValue> {
        match field_token.into() {
            BgpUpdateMessageToken::Nlris => Some(TypeValue::Unknown),
            BgpUpdateMessageToken::Afi => Some(TypeValue::Builtin(
                BuiltinTypeValue::ConstU16EnumVariant(EnumVariant {
                    enum_name: "AFI".into(),
                    value: self.raw_message.0.nlris().afi().into(),
                }),
            )),
            BgpUpdateMessageToken::Safi => Some(TypeValue::Builtin(
                BuiltinTypeValue::ConstU8EnumVariant(EnumVariant {
                    enum_name: "SAFI".into(),
                    value: self.raw_message.0.nlris().safi().into(),
                }),
            )),
        }
    }
}

impl RotoType for BgpUpdateMessage {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        Err(format!(
            "Unknown method: '{}' for type BgpUpdateMessage",
            method_name.ident
        )
        .into())
    }

    fn into_type(self, ty: &TypeDef) -> Result<TypeValue, CompileError>
    where
        Self: std::marker::Sized,
    {
        Err(format!(
            "BgpUpdateMessage cannot be converted to type {} (or any other type)",
            ty
        )
        .into())
    }

    fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        _extra_command_args: Option<CommandArg>,
        _args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.into() {
            BgpUpdateMessageToken::Afi => Ok(TypeValue::Builtin(
                BuiltinTypeValue::ConstU16EnumVariant(EnumVariant {
                    enum_name: "AFI".into(),
                    value: self.raw_message.0.nlris().afi().into(),
                }),
            )),
            BgpUpdateMessageToken::Safi => Ok(TypeValue::Builtin(
                BuiltinTypeValue::ConstU8EnumVariant(EnumVariant {
                    enum_name: "SAFI".into(),
                    value: self.raw_message.0.nlris().safi().into(),
                }),
            )),
            _ => Err(VmError::InvalidMethodCall),
        }
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
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

impl From<BgpUpdateMessage> for TypeValue {
    fn from(raw: BgpUpdateMessage) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::BgpUpdateMessage(Arc::new(raw)))
    }
}

#[derive(Debug)]
enum BgpUpdateMessageToken {
    Nlris = 0,
    Afi = 1,
    Safi = 2,
}

impl From<usize> for BgpUpdateMessageToken {
    fn from(val: usize) -> Self {
        match val {
            0 => BgpUpdateMessageToken::Nlris,
            1 => BgpUpdateMessageToken::Afi,
            2 => BgpUpdateMessageToken::Safi,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<BgpUpdateMessageToken> for usize {
    fn from(val: BgpUpdateMessageToken) -> Self {
        match val {
            BgpUpdateMessageToken::Nlris => 0,
            BgpUpdateMessageToken::Afi => 1,
            BgpUpdateMessageToken::Safi => 2,
        }
    }
}

// pub as_path: ChangedOption<AsPath<Vec<MaterializedPathSegment>>>,
//     pub origin_type: ChangedOption<OriginType>,
//     pub next_hop: ChangedOption<NextHop>,
//     pub multi_exit_discriminator: ChangedOption<MultiExitDisc>,
//     pub local_pref: ChangedOption<LocalPref>,
//     pub atomic_aggregate: ChangedOption<bool>,
//     pub aggregator: ChangedOption<Aggregator>,
//     pub communities: ChangedOption<Vec<Community>>,
//     // mp_reach_nlri: Vec<Prefix>,
//     // mp_unreach_nlri: Vec<Prefix>,
//     pub originator_id: ChangedOption<Todo>,
//     pub cluster_list: ChangedOption<Todo>,
//     pub extended_communities: ChangedOption<Vec<ExtendedCommunity>>,
//     pub as4_path: ChangedOption<AsPath<Vec<Asn>>>,
//     pub as4_aggregator: ChangedOption<Todo>,
//     pub connector: ChangedOption<Todo>, // Connector,
//     pub as_path_limit: ChangedOption<(u8, u32)>,
//     pub pmsi_tunnel: ChangedOption<Todo>, // PmsiTunnel,
//     pub ipv6_extended_communities: ChangedOption<Vec<Ipv6ExtendedCommunity>>,
//     pub large_communities: ChangedOption<Vec<LargeCommunity>>,
//     pub bgpsec_as_path: ChangedOption<Todo>, // BgpsecAsPath,
//     pub attr_set: ChangedOption<Todo>,       // AttrSet,
//     pub rsrvd_development: ChangedOption<Todo>

impl RotoType for RawRouteWithDeltas {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "prefix" => Ok(MethodProps::new(
                TypeDef::Prefix,
                RouteToken::Prefix.into(),
                vec![],
            )),
            "as-path" => Ok(MethodProps::new(
                TypeDef::AsPath,
                RouteToken::AsPath.into(),
                vec![],
            )),
            "origin-type" => Ok(MethodProps::new(
                TypeDef::OriginType,
                RouteToken::OriginType.into(),
                vec![],
            )),
            "next-hop" => Ok(MethodProps::new(
                TypeDef::NextHop,
                RouteToken::NextHop.into(),
                vec![],
            )),
            "multi-exit-disc" => Ok(MethodProps::new(
                TypeDef::MultiExitDisc,
                RouteToken::MultiExitDisc.into(),
                vec![],
            )),
            "local-pref" => Ok(MethodProps::new(
                TypeDef::LocalPref,
                RouteToken::LocalPref.into(),
                vec![],
            )),
            "atomic-aggregate" => Ok(MethodProps::new(
                TypeDef::Boolean,
                RouteToken::AtomicAggregate.into(),
                vec![],
            )),
            "aggregator" => Ok(MethodProps::new(
                TypeDef::AtomicAggregator,
                RouteToken::AtomicAggregator.into(),
                vec![],
            )),
            "communities" => Ok(MethodProps::new(
                TypeDef::List(Box::new(TypeDef::Community)),
                RouteToken::Communities.into(),
                vec![],
            )),
            "status" => Ok(MethodProps::new(
                TypeDef::RouteStatus,
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
        _extra_command_args: Option<CommandArg>,
        _args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidMethodCall)
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
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

impl From<RawRouteWithDeltas> for TypeValue {
    fn from(val: RawRouteWithDeltas) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Route(val))
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

#[derive(Copy, Clone, Debug, Deserialize, PartialEq, Eq)]
pub enum RouteToken {
    Prefix = 0,
    AsPath = 1,
    OriginType = 2,
    NextHop = 3,
    MultiExitDisc = 4,
    LocalPref = 5,
    AtomicAggregate = 6,
    AtomicAggregator = 7,
    Communities = 8,
    Status = 9,
    PeerIp = 10,
    PeerAsn = 11,
}

impl From<usize> for RouteToken {
    fn from(value: usize) -> Self {
        match value {
            0 => RouteToken::Prefix,
            1 => RouteToken::AsPath,
            2 => RouteToken::OriginType,
            3 => RouteToken::NextHop,
            4 => RouteToken::MultiExitDisc,
            5 => RouteToken::LocalPref,
            6 => RouteToken::AtomicAggregate,
            7 => RouteToken::AtomicAggregator,
            8 => RouteToken::Communities,
            9 => RouteToken::Status,
            10 => RouteToken::PeerIp,
            11 => RouteToken::PeerAsn,
            _ => panic!("Unknown RouteToken value: {}", value),
        }
    }
}

impl From<RouteToken> for usize {
    fn from(val: RouteToken) -> Self {
        val as usize
    }
}

impl From<RouteToken> for u8 {
    fn from(val: RouteToken) -> Self {
        val as u8
    }
}

impl RotoType for RouteStatus {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "is_in_convergence" => Ok(MethodProps::new(
                TypeDef::Boolean,
                RouteStatusToken::IsInConvergence.into(),
                vec![],
            )),
            "is_up_to_date" => Ok(MethodProps::new(
                TypeDef::Boolean,
                RouteStatusToken::IsUpToDate.into(),
                vec![],
            )),
            "is_stale" => Ok(MethodProps::new(
                TypeDef::Boolean,
                RouteStatusToken::IsStale.into(),
                vec![],
            )),
            "is_start_of_route_refresh" => Ok(MethodProps::new(
                TypeDef::Boolean,
                RouteStatusToken::IsStartOfRouteRefresh.into(),
                vec![],
            )),
            "is_withdrawn" => Ok(MethodProps::new(
                TypeDef::Boolean,
                RouteStatusToken::IsWithdrawn.into(),
                vec![],
            )),
            "is_empty" => Ok(MethodProps::new(
                TypeDef::Boolean,
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
        _extra_command_args: Option<CommandArg>,
        _args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidMethodCall)
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
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

#[derive(Debug)]
pub enum RouteStatusToken {
    IsInConvergence = 1,
    IsUpToDate = 2,
    IsStale = 3,
    IsStartOfRouteRefresh = 4,
    IsWithdrawn = 5,
    IsEmpty = 6,
}

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

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Serialize)]
pub struct RotondaId(pub usize);

//------------ Modification & Creation of new Updates -----------------------

#[derive(Debug, Hash, Serialize)]
pub struct UpdateMessage(
    pub routecore::bgp::message::UpdateMessage<bytes::Bytes>,
);

impl UpdateMessage {
    pub fn new(bytes: bytes::Bytes, config: SessionConfig) -> Self {
        Self(routecore::bgp::message::UpdateMessage::<bytes::Bytes>::from_octets(bytes, config).unwrap())
    }

    // Materialize a ChangeSet from the Update message. The materialized
    // Change set is completely self-contained (no references of any kind) &
    // holds all the attributes of the current BGP Update message.
    pub fn create_changeset(
        &self,
        prefix: Prefix,
        peer_ip: Option<IpAddress>,
        peer_asn: Option<Asn>,
        router_id: Option<Arc<String>>,
    ) -> AttrChangeSet {
        AttrChangeSet {
            prefix: ReadOnlyScalarOption::<Prefix>::new(prefix.into()),
            as_path: VectorOption::<AsPath>::from(
                self.0.aspath().map(|p| p.to_hop_path()),
            ),
            origin_type: ScalarOption::<OriginType>::from(self.0.origin()),
            next_hop: ScalarOption::<NextHop>::from(self.0.next_hop()),
            multi_exit_discriminator: ScalarOption::from(
                self.0.multi_exit_desc(),
            ),
            local_pref: ScalarOption::from(self.0.local_pref()),
            atomic_aggregate: ScalarOption::from(Some(
                self.0.is_atomic_aggregate(),
            )),
            aggregator: ScalarOption::from(self.0.aggregator()),
            communities: VectorOption::from(self.0.all_communities()),
            peer_ip: peer_ip.into(),
            peer_asn: peer_asn.into(),
            router_id: router_id.map(|v| StringLiteral(String::clone(&v))).into(),
            originator_id: Todo,
            cluster_list: Todo,
            extended_communities: Todo,
            // value: self
            //     .ext_communities()
            //     .map(|c| c.collect::<Vec<ExtendedCommunity>>()),
            as4_path: VectorOption::from(
                self.0.as4path().map(|p| p.to_hop_path()),
            ),
            connector: Todo,
            as_path_limit: Todo,
            pmsi_tunnel: Todo,
            ipv6_extended_communities: Todo,
            large_communities: Todo,
            // value: T::try_from(self
            //     .large_communities()
            //     .map(|c| c.collect::<Vec<LargeCommunity>>())),
            bgpsec_as_path: Todo,
            attr_set: Todo,
            rsrvd_development: Todo,
            as4_aggregator: Todo,
        }
    }

    // Create a new BGP Update message by applying the attributes changes
    // in the supplied change set to our current Update message.
    pub fn create_update_from_changeset<T: ScalarValue, V: VectorValue>(
        _change_set: &AttrChangeSet,
    ) -> Self {
        todo!()
    }
}
