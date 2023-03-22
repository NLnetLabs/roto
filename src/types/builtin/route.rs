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

use routecore::{bgp::message::SessionConfig, record::LogicalTime};
use std::sync::Arc;

use crate::{
    attr_change_set::{ReadOnlyScalarOption, Todo},
    compile::CompileError,
    traits::{RotoType, Token, TokenConvert},
    types::{
        typedef::{MethodProps, TypeDef},
        typevalue::TypeValue,
    },
    vm::VmError,
};

use super::{AsPath, BuiltinTypeValue, RouteStatus, NextHop, OriginType, Prefix};
use crate::attr_change_set::{
    AttrChangeSet, ScalarOption, ScalarValue, VectorOption,
    VectorValue,
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
            prefix: raw_route.prefix, // The roto prefix type
            path_attributes: raw_route.take_latest_attrs(),
            status: status.into(),
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
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RawRouteWithDeltas {
    pub prefix: Prefix,
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
        prefix: Prefix,
        raw_message: UpdateMessage,
    ) -> Self {
        let raw_message = RawBgpMessage::new(delta_id, raw_message);
        let mut attribute_deltas = AttributeDeltaList::new();
        // This stores the attributes in the raw message as the first delta.
        attribute_deltas
            .store_delta(AttributeDelta::new(
                delta_id,
                0,
                raw_message.raw_message.create_changeset(prefix),
            ))
            .unwrap();

        Self {
            prefix,
            raw_message: Arc::new(raw_message),
            attribute_deltas,
            status_deltas: RouteStatusDeltaList(vec![RouteStatusDelta::new(
                delta_id,
            )]),
        }
    }

    pub fn new_with_message_ref(
        delta_id: (RotondaId, LogicalTime),
        prefix: Prefix,
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

    // Get a clone of the latest delta, or of the original attributes from
    // the raw message, if no delta has been added (yet).
    fn clone_latest_attrs(&self) -> AttrChangeSet {
        if let Some(attr_set) = self.attribute_deltas.deltas.last() {
            attr_set.attributes.clone()
        } else {
            self.raw_message.raw_message.create_changeset(self.prefix)
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
            return self
                .raw_message
                .raw_message
                .create_changeset(self.prefix);
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
                    self.raw_message
                        .raw_message
                        .create_changeset(self.prefix),
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
        let current = self.attribute_deltas.get_latest_change_set()?;
        // let p = self.attribute_deltas.get_as_path()?; //?.as_path.as_ref();
        match field_token.into() {
            RouteToken::Prefix => current.prefix.value.as_ref(),
            RouteToken::AsPath => current.as_path.value.as_ref(),
            RouteToken::OriginType => current.origin_type.value.as_ref(),
            RouteToken::NextHop => current.next_hop.value.as_ref(),
            RouteToken::MultiExitDisc => {
                current.multi_exit_discriminator.value.as_ref()
            }
            RouteToken::LocalPref => current.local_pref.value.as_ref(),
            RouteToken::AtomicAggregate => {
                current.atomic_aggregate.value.as_ref()
            }
            RouteToken::AtomicAggregator => current.aggregator.value.as_ref(),
            RouteToken::Communities => current.communities.value.as_ref(),
            RouteToken::Status => self.status_deltas.current_as_ref(),
        }
    }

    pub(crate) fn get_field_by_index(
        &self,
        field_token: usize,
    ) -> Option<TypeValue> {
        match field_token.into() {
                RouteToken::AsPath => self.raw_message.raw_message.0.hop_path().map(TypeValue::from),
                RouteToken::OriginType => self.raw_message.raw_message.0.origin().map(TypeValue::from),
                RouteToken::NextHop => self.raw_message.raw_message.0.next_hop().map(TypeValue::from),
                RouteToken::MultiExitDisc => self.raw_message.raw_message.0.multi_exit_desc().map(TypeValue::from),
                RouteToken::LocalPref => self.raw_message.raw_message.0.local_pref().map(TypeValue::from),
                RouteToken::AtomicAggregate => Some(TypeValue::from(self.raw_message.raw_message.0.is_atomic_aggregate())),
                RouteToken::AtomicAggregator => self.raw_message.raw_message.0.aggregator().map(TypeValue::from),
                RouteToken::Communities => self.raw_message.raw_message.0.all_communities().map(TypeValue::from),
                RouteToken::Prefix => Some(self.prefix.into()),
                RouteToken::Status => Some(self.status_deltas.current().into()),
                _ => None,
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
}

//------------ RouteDeltas --------------------------------------------------

// The history of changes to this route. Each Delta holds the attributes that
// were originally present in the raw message, their modifications and newly
// created ones.
//
// The list of deltas describes the changes that were made by one Rotonda
// unit along the way.
#[derive(Debug, Clone, Eq, PartialEq, Default)]
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

    fn get_as_path(&self) -> Option<&TypeValue> {
        self.deltas
            .last()
            .map(|d| d.attributes.as_path.value.as_ref())
            .unwrap()
    }

    // Adds a new delta to the list.
    fn store_delta(&mut self, delta: AttributeDelta) -> Result<(), VmError> {
        if let Some(locked_delta) = self.locked_delta {
            if locked_delta != delta.delta_index {
                println!("{:?} {}", self.locked_delta, delta.delta_index);
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
#[derive(Debug, Clone, Eq, PartialEq)]
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

#[derive(Debug, Clone, Eq, PartialEq)]
struct RouteStatusDelta {
    delta_id: (RotondaId, LogicalTime),
    status: TypeValue,
}

impl RouteStatusDelta {
    pub fn new(delta_id: (RotondaId, LogicalTime)) -> Self {
        Self {
            delta_id,
            status: TypeValue::UnInit,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct RouteStatusDeltaList(Vec<RouteStatusDelta>);

impl RouteStatusDeltaList {
    pub fn current(&self) -> TypeValue {
        self.0.iter().last().unwrap().status.clone()
    }

    pub fn current_as_ref(&self) -> Option<&TypeValue> {
        self.0.iter().last().map(|s| &s.status)
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
    raw_message: UpdateMessage,
}

impl RawBgpMessage {
    fn new(message_id: (RotondaId, u64), raw_message: UpdateMessage) -> Self {
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

#[derive(Debug)]
pub enum RouteToken {
    Prefix = 1,
    AsPath = 2,
    OriginType = 3,
    NextHop = 4,
    MultiExitDisc = 5,
    LocalPref = 6,
    AtomicAggregate = 7,
    AtomicAggregator = 8,
    Communities = 9,
    Status = 10,
}

impl TokenConvert for RouteToken {}

impl From<usize> for RouteToken {
    fn from(value: usize) -> Self {
        match value {
            1 => RouteToken::Prefix,
            2 => RouteToken::AsPath,
            3 => RouteToken::OriginType,
            4 => RouteToken::NextHop,
            5 => RouteToken::MultiExitDisc,
            6 => RouteToken::LocalPref,
            7 => RouteToken::AtomicAggregate,
            8 => RouteToken::AtomicAggregator,
            9 => RouteToken::Communities,
            10 => RouteToken::Status,
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

//------------ Modification & Creation of new Updates -----------------------

#[derive(Debug)]
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
    pub fn create_changeset(&self, prefix: Prefix) -> AttrChangeSet {
        AttrChangeSet {
            prefix: ReadOnlyScalarOption::<Prefix>::new(prefix.into()),
            as_path: VectorOption::<AsPath>::from(self.0.hop_path()),
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
            originator_id: Todo,
            cluster_list: Todo,
            extended_communities: Todo,
            // value: self
            //     .ext_communities()
            //     .map(|c| c.collect::<Vec<ExtendedCommunity>>()),
            as4_path: VectorOption::from(self.0.as4_hop_path()),
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
