// //============ Route types ==================================================

// // routcore/rotonda-runtime               roto
// //                                        ┌─────────────┐
// //                                        ├─────────────┤
// //                 ┌─────────────┐        ├─────────────┤
// //               ┌─▶   Updates   ├─write──▶    Route    │
// // ┌────────────┐│ └─────────────┘        └──────▲──────┘
// // │ BgpMessage ├┤                               │
// // └────────────┘│ ┌─────────────┐               │
// //               └─▶ Withdrawals │──change ──────┘
// //                 └─────────────┘  status

// use chrono::Utc;
// use log::{debug, error};
// use routecore::addr::Prefix;
// use routecore::asn::Asn;
// use routecore::bgp::types::OriginType;
// use routecore::bgp::{
//     aspath::HopPath,
//     message::{nlri::PathId, SessionConfig},
//     types::{AfiSafi, NextHop},
// };
// use serde::{Deserialize, Serialize};
// use smallvec::SmallVec;
// use std::net::SocketAddr;
// use std::{net::IpAddr, sync::Arc};

// /// Lamport Timestamp. Used to order messages between units/systems.
// pub type LogicalTime = u64;

// use crate::types::builtin::bgp_update_message::BytesRecord;
// use crate::types::collections::RecordType;
// use crate::types::lazyrecord_types::BgpUpdateMessage;
// use crate::{
//     ast::StringLiteral,
//     attr_change_set::{ReadOnlyScalarOption, Todo},
//     compiler::compile::CompileError,
//     traits::{RotoType, Token},
//     types::{
//         enum_types::EnumVariant,
//         typedef::{MethodProps, TypeDef},
//         typevalue::TypeValue,
//     },
//     vm::{StackValue, VmError},
// };

// use super::path_attributes::{BasicRoute, BasicRouteToken};
// use super::{BuiltinTypeValue, Nlri, NlriStatus};
// use crate::attr_change_set::{
//     AttrChangeSet, PathAttributeSet, ScalarOption, ScalarValue, VectorOption,
//     VectorValue,
// };

// //============ MaterializedRoute ============================================

// // An RFC 4271 Route. The RFC:

// // ```
// // Route
// // A unit of information that pairs a set of destinations with the
// // attributes of a path to those destinations.  The set of
// // destinations are systems whose IP addresses are contained in one
// // IP address prefix carried in the Network Layer Reachability
// // Information (NLRI) field of an UPDATE message.  The path is the
// // information reported in the path attributes field of the same
// // UPDATE message.
// // ```

// // So, a data structure in the form of (prefix, path attributes) with some
// // added metadata, that is strictly speaking not part of a RFC4271 route.
// // This is not the data-structure that Rotonda uses to store by default in
// // a RIB (that is the RawRouteDelta down below), but the type that is used
// // serialize the data into on export and transport.

// #[derive(Debug, Eq, PartialEq, Clone, Hash, Serialize)]
// pub struct MaterializedRoute {
//     pub route: Option<AttrChangeSet>,
//     pub status: NlriStatus,
//     route_id: (RotondaId, LogicalTime),
// }

// impl From<RawRouteWithDeltas> for MaterializedRoute {
//     fn from(raw_route: RawRouteWithDeltas) -> Self {
//         let status = raw_route.status_deltas.current();
//         let route_id = (RotondaId(0), 0);

//         let status = if let Ok(status) = status.try_into() {
//             status
//         } else {
//             NlriStatus::Unparsable
//         };

//         if let Ok(route) = raw_route.take_latest_attrs() {
//             MaterializedRoute {
//                 route: Some(route),
//                 status,
//                 route_id,
//             }
//         } else {
//             MaterializedRoute {
//                 route: None,
//                 status: NlriStatus::Unparsable,
//                 route_id,
//             }
//         }
//     }
// }

// // impl From<NlriContext> for RawRouteWithDeltas {
// //     fn from(value: NlriContext) -> Self {
// //         let delta_id = (RotondaId(0), 0); // TODO
// //         RawRouteWithDeltas::new_with_message_ref(
// //             delta_id,
// //             value.nlri.prefix,
// //             value.msg,
// //             value.nlri.afi_safi,
// //             value.nlri.path_id,
// //             value.nlri.status,
// //         )
// //         .with_peer_ip(value.provenance.peer_id.addr)
// //         .with_peer_asn(value.provenance.peer_id.asn)
// //         .with_router_id(value.provenance.router_id)
// //     }
// // }

// //------------ RawRouteWithDelta ============================================

// // This is the default storage data-structure for a RIB, it will store the
// // complete BGP message (as an Arc - to avoid duplication for every prefix
// // in the NLRI) in a RIB.
// //
// // It will be stored as a array of bytes, its BGP path attributes can be
// // extracted from the original bytes on the fly (with routecore).
// // Additionally it features a data-structure (AttributesDeltaList) that
// // stores the changes made by the transformers (filters, etc.) along the way.
// //
// // Each Delta describes the complete state of all the attributes at the time it
// // was stored in the RawRouteWithDeltas instance. So it reflects both the
// // original attributes from the raw message, their modifications and the
// // newly set attributes.
// #[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize)]
// #[serde(into = "MaterializedRoute")]
// pub struct RawRouteWithDeltas {
//     pub prefix: Prefix,
//     // Arc'ed BGP message
//     pub raw_message: BytesRecord<BgpUpdateMessage>,
//     // AFI SAFI combination for the NLRI that the Prefix for this route
//     // belongs to.
//     pub afi_safi: routecore::bgp::types::AfiSafi,
//     // The IP address of the BGP speaker that originated the route, if known.
//     peer_ip: Option<IpAddr>,
//     // The ASN of the BGP speaker that originated the route, if known.
//     peer_asn: Option<Asn>,
//     // The ID (e.g. "<ip addr>:<port>", or "<BMP initiate sysName>") of the
//     // connected router from which the route was received, if known.
//     router_id: Option<Arc<String>>,
//     // The AddPath path Id, only valid for certain AFI SAFI combinations &
//     // note this ID is only supposed to be unique within one BGP peering
//     // session.
//     path_id: Option<routecore::bgp::message::nlri::PathId>,
//     // history of recorded changes to the route
//     attribute_deltas: AttributeDeltaList,
//     // history of status changes to the route
//     status_deltas: RouteStatusDeltaList,
// }

// impl RawRouteWithDeltas {
//     pub fn new_with_message(
//         delta_id: (RotondaId, LogicalTime),
//         prefix: Prefix,
//         raw_message: BytesRecord<BgpUpdateMessage>,
//         afi_safi: routecore::bgp::types::AfiSafi,
//         path_id: Option<routecore::bgp::message::nlri::PathId>,
//         route_status: NlriStatus,
//     ) -> Result<Self, VmError> {
//         let mut attribute_deltas = AttributeDeltaList::new();
//         let (peer_ip, peer_asn, router_id) = (None, None, None);
//         // This stores the attributes in the raw message as the first delta.
//         attribute_deltas.store_delta(AttributeDelta::new(
//             // delta_id,
//             0,
//             raw_message.create_changeset(
//                 prefix,
//                 peer_ip,
//                 peer_asn,
//                 router_id.clone(),
//                 afi_safi,
//                 path_id,
//             )?,
//         ))?;

//         Ok(Self {
//             prefix,
//             raw_message,
//             afi_safi,
//             path_id,
//             peer_ip,
//             peer_asn,
//             router_id,
//             attribute_deltas,
//             status_deltas: RouteStatusDeltaList::new(RouteStatusDelta::new(
//                 delta_id,
//                 route_status,
//             )),
//         })
//     }

//     pub fn new_with_message_ref(
//         delta_id: (RotondaId, LogicalTime),
//         prefix: Prefix,
//         raw_message: BytesRecord<BgpUpdateMessage>,
//         afi_safi: routecore::bgp::types::AfiSafi,
//         path_id: Option<routecore::bgp::message::nlri::PathId>,
//         route_status: NlriStatus,
//     ) -> Self {
//         Self {
//             prefix,
//             raw_message,
//             afi_safi,
//             path_id,
//             peer_ip: None,
//             peer_asn: None,
//             router_id: None,
//             attribute_deltas: AttributeDeltaList::new(),
//             status_deltas: RouteStatusDeltaList::new(RouteStatusDelta::new(
//                 delta_id,
//                 route_status,
//             )),
//         }
//     }

//     pub fn with_peer_ip(self, peer_ip: IpAddr) -> Self {
//         Self {
//             prefix: self.prefix,
//             raw_message: self.raw_message,
//             afi_safi: self.afi_safi,
//             path_id: self.path_id,
//             peer_ip: Some(peer_ip),
//             peer_asn: self.peer_asn,
//             router_id: self.router_id,
//             attribute_deltas: self.attribute_deltas,
//             status_deltas: self.status_deltas,
//         }
//     }

//     pub fn with_peer_asn(self, peer_asn: routecore::asn::Asn) -> Self {
//         Self {
//             prefix: self.prefix,
//             raw_message: self.raw_message,
//             afi_safi: self.afi_safi,
//             path_id: self.path_id,
//             peer_ip: self.peer_ip,
//             peer_asn: Some(peer_asn),
//             router_id: self.router_id,
//             attribute_deltas: self.attribute_deltas,
//             status_deltas: self.status_deltas,
//         }
//     }

//     pub fn with_router_id(self, router_id: Arc<String>) -> Self {
//         Self {
//             prefix: self.prefix,
//             raw_message: self.raw_message,
//             afi_safi: self.afi_safi,
//             path_id: self.path_id,
//             peer_ip: self.peer_ip,
//             peer_asn: self.peer_asn,
//             router_id: Some(router_id),
//             attribute_deltas: self.attribute_deltas,
//             status_deltas: self.status_deltas,
//         }
//     }

//     pub fn peer_ip(&self) -> Option<IpAddr> {
//         self.peer_ip
//     }

//     pub fn peer_asn(&self) -> Option<routecore::asn::Asn> {
//         self.peer_asn
//     }

//     pub fn router_id(&self) -> Option<Arc<String>> {
//         self.router_id.clone()
//     }

//     pub fn update_status(
//         &mut self,
//         delta_id: (RotondaId, LogicalTime),
//         new_status: NlriStatus,
//     ) {
//         let delta = RouteStatusDelta::new(delta_id, new_status);
//         self.status_deltas.0.push(delta);
//     }

//     // Get a clone of the latest delta, or of the original attributes from
//     // the raw message, if no delta has been added (yet).
//     fn clone_latest_attrs(&self) -> Result<AttrChangeSet, VmError> {
//         if let Some(attr_set) = self.attribute_deltas.deltas.last() {
//             Ok(attr_set.attributes.clone())
//         } else {
//             Ok(self.raw_message.create_changeset(
//                 self.prefix,
//                 self.peer_ip,
//                 self.peer_asn,
//                 self.router_id.clone(),
//                 self.afi_safi,
//                 self.path_id,
//             )?)
//         }
//     }

//     // Return a clone of the latest attribute set, so that changes can be
//     // made to it by the caller.
//     pub fn open_new_delta(
//         &mut self,
//         delta_id: (RotondaId, LogicalTime),
//     ) -> Result<AttributeDelta, VmError> {
//         let delta_index = self.attribute_deltas.acquire_new_delta()?;

//         Ok(AttributeDelta {
//             attributes: self.clone_latest_attrs()?,
//             // delta_id,
//             delta_index,
//         })
//     }

//     // Get either the moved last delta (it is removed from the Delta list), or
//     // get a freshly rolled ChangeSet from the raw message, if there are no
//     // deltas.
//     pub fn take_latest_attrs(mut self) -> Result<AttrChangeSet, VmError> {
//         if self.attribute_deltas.deltas.is_empty() {
//             return self.raw_message.create_changeset(
//                 self.prefix,
//                 self.peer_ip,
//                 self.peer_asn,
//                 self.router_id,
//                 self.afi_safi,
//                 self.path_id,
//             );
//         }

//         Ok(self
//             .attribute_deltas
//             .deltas
//             .remove(self.attribute_deltas.deltas.len() - 1)
//             .attributes)
//     }

//     // Get a reference to the current state of the attributes, including the
//     // original raw message. In the latter case it will copy the raw message
//     // attributes into the attribute deltas, and return a reference from that,
//     // hence the `&mut self`.
//     pub fn get_latest_attrs(&mut self) -> Result<&AttrChangeSet, VmError> {
//         if self.attribute_deltas.deltas.is_empty() {
//             self.attribute_deltas.store_delta(AttributeDelta::new(
//                 0,
//                 self.raw_message.create_changeset(
//                     self.prefix,
//                     self.peer_ip,
//                     self.peer_asn,
//                     self.router_id.clone(),
//                     self.afi_safi,
//                     self.path_id,
//                 )?,
//             ))?;
//         }
//         self.attribute_deltas
//             .get_latest_change_set()
//             .ok_or_else(|| {
//                 // This is a serious, weird error. We just added the change set in
//                 // the above code successfully! So we should NEVER end up here,
//                 // but if we do we can still return a VmError, log an error, and
//                 // keep on going. Something is (very) wrong though, internally.
//                 error!(
//                     "Cannot find Attribute Change Set: {:?} on Route",
//                     self.attribute_deltas
//                 );
//                 VmError::InvalidPayload
//             })
//     }

//     // Get a ChangeSet that was added by a specific unit, e.g. a filter.
//     // pub fn get_delta_for_rotonda_id(
//     //     &self,
//     //     rotonda_id: RotondaId,
//     // ) -> Option<&AttrChangeSet> {
//     //     self.attribute_deltas
//     //         .deltas
//     //         .iter()
//     //         .find(|d| d.delta_id.0 == rotonda_id)
//     //         .map(|d| &d.attributes)
//     // }

//     // Add a ChangeSet with some metadata to this RawRouteWithDelta.
//     pub fn store_delta(
//         &mut self,
//         attr_delta: AttributeDelta,
//     ) -> Result<(), VmError> {
//         self.attribute_deltas.store_delta(attr_delta)
//     }

//     pub fn iter_deltas(&self) -> impl Iterator<Item = &AttributeDelta> + '_ {
//         self.attribute_deltas.deltas.iter()
//     }

//     pub(crate) fn get_props_for_field(
//         field_name: &crate::ast::Identifier,
//     ) -> Result<(TypeDef, crate::traits::Token), CompileError>
//     where
//         Self: std::marker::Sized,
//     {
//         match field_name.ident.as_str() {
//             "prefix" => Ok((
//                 TypeDef::Prefix,
//                 Token::FieldAccess(vec![BasicRouteToken::Prefix.into()]),
//             )),
//             "as-path" => Ok((
//                 TypeDef::AsPath,
//                 Token::FieldAccess(vec![BasicRouteToken::AsPath.into()]),
//             )),
//             "origin-type" => Ok((
//                 TypeDef::OriginType,
//                 Token::FieldAccess(vec![BasicRouteToken::OriginType.into()]),
//             )),
//             "next-hop" => Ok((
//                 TypeDef::NextHop,
//                 Token::FieldAccess(vec![BasicRouteToken::NextHop.into()]),
//             )),
//             "multi-exit-disc" => Ok((
//                 TypeDef::MultiExitDisc,
//                 Token::FieldAccess(vec![
//                     BasicRouteToken::MultiExitDisc.into()
//                 ]),
//             )),
//             "local-pref" => Ok((
//                 TypeDef::LocalPref,
//                 Token::FieldAccess(vec![BasicRouteToken::LocalPref.into()]),
//             )),
//             "atomic-aggregate" => Ok((
//                 TypeDef::Bool,
//                 Token::FieldAccess(vec![
//                     BasicRouteToken::AtomicAggregate.into()
//                 ]),
//             )),
//             "aggregator" => Ok((
//                 TypeDef::AggregatorInfo,
//                 Token::FieldAccess(vec![BasicRouteToken::Aggregator.into()]),
//             )),
//             "communities" => Ok((
//                 TypeDef::List(Box::new(TypeDef::Community)),
//                 Token::FieldAccess(vec![BasicRouteToken::Communities.into()]),
//             )),
//             "status" => Ok((
//                 TypeDef::NlriStatus,
//                 Token::FieldAccess(vec![BasicRouteToken::Status.into()]),
//             )),
//             // "peer_ip" => Ok((
//             //     TypeDef::IpAddr,
//             //     Token::FieldAccess(vec![RouteToken::PeerIp.into()]),
//             // )),
//             // "peer_asn" => Ok((
//             //     TypeDef::Asn,
//             //     Token::FieldAccess(vec![RouteToken::PeerAsn.into()]),
//             // )),
//             _ => Err(format!(
//                 "Unknown method '{}' for type Route",
//                 field_name.ident
//             )
//             .into()),
//         }
//     }

//     pub(crate) fn get_field_num() -> usize {
//         12
//     }

//     pub(crate) fn get_value_ref_for_field(
//         &self,
//         field_token: usize,
//     ) -> Result<Option<&TypeValue>, VmError> {
//         let current_set = if let Some(atrd) =
//             self.attribute_deltas.get_latest_change_set()
//         {
//             atrd
//         } else {
//             return Err(VmError::InvalidRecord);
//         };

//         match field_token.try_into()? {
//             BasicRouteToken::Prefix => Ok(current_set.prefix.as_ref()),
//             BasicRouteToken::AsPath => Ok(current_set.as_path.as_ref()),
//             BasicRouteToken::OriginType => {
//                 Ok(current_set.origin_type.as_ref())
//             }
//             BasicRouteToken::NextHop => Ok(current_set.next_hop.as_ref()),
//             BasicRouteToken::MultiExitDisc => {
//                 Ok(current_set.multi_exit_discriminator.as_ref())
//             }
//             BasicRouteToken::LocalPref => Ok(current_set.local_pref.as_ref()),
//             BasicRouteToken::AtomicAggregate => {
//                 Ok(current_set.atomic_aggregate.as_ref())
//             }
//             BasicRouteToken::Aggregator => {
//                 Ok(current_set.aggregator.as_ref())
//             }
//             BasicRouteToken::Communities => {
//                 Ok(current_set.communities.as_ref())
//             }
//             BasicRouteToken::Status => {
//                 Ok(self.status_deltas.current_as_ref())
//             }
//             BasicRouteToken::Provenance => todo!(),
//             // RouteToken::PeerIp => Ok(current_set.peer_ip.as_ref()),
//             // RouteToken::PeerAsn => Ok(current_set.peer_asn.as_ref()),
//         }
//     }

//     pub fn get_field_by_index(
//         &self,
//         field_token: usize,
//     ) -> Result<TypeValue, VmError> {
//         match field_token.try_into()? {
//             BasicRouteToken::AsPath => self
//                 .raw_message
//                 .bytes_parser()
//                 .aspath()
//                 .ok()
//                 .flatten()
//                 .map(|p| p.to_hop_path())
//                 .map(TypeValue::from)
//                 .ok_or(VmError::InvalidFieldAccess),
//             BasicRouteToken::OriginType => self
//                 .raw_message
//                 .bytes_parser()
//                 .origin()
//                 .ok()
//                 .flatten()
//                 .map(TypeValue::from)
//                 .ok_or(VmError::InvalidFieldAccess),
//             BasicRouteToken::NextHop => self
//                 .raw_message
//                 .bytes_parser()
//                 .find_next_hop(self.afi_safi)
//                 .map_err(|_| VmError::InvalidFieldAccess)
//                 .map(TypeValue::from),
//             BasicRouteToken::MultiExitDisc => self
//                 .raw_message
//                 .bytes_parser()
//                 .multi_exit_disc()
//                 .ok()
//                 .flatten()
//                 .map(TypeValue::from)
//                 .ok_or(VmError::InvalidFieldAccess),
//             BasicRouteToken::LocalPref => self
//                 .raw_message
//                 .bytes_parser()
//                 .local_pref()
//                 .ok()
//                 .flatten()
//                 .map(TypeValue::from)
//                 .ok_or(VmError::InvalidFieldAccess),
//             BasicRouteToken::AtomicAggregate => Some(TypeValue::from(
//                 self.raw_message
//                     .bytes_parser()
//                     .is_atomic_aggregate()
//                     .unwrap_or(false),
//             ))
//             .ok_or(VmError::InvalidFieldAccess),
//             BasicRouteToken::Aggregator => self
//                 .raw_message
//                 .bytes_parser()
//                 .aggregator()
//                 .ok()
//                 .flatten()
//                 .map(TypeValue::from)
//                 .ok_or(VmError::InvalidFieldAccess),
//             BasicRouteToken::Communities => self
//                 .raw_message
//                 .bytes_parser()
//                 .all_human_readable_communities()
//                 .ok()
//                 .flatten()
//                 .map(TypeValue::from)
//                 .ok_or(VmError::InvalidFieldAccess),
//             BasicRouteToken::Prefix => Ok(self.prefix.into()),
//             BasicRouteToken::Status => Ok(self.status_deltas.current()),
//             BasicRouteToken::Provenance => todo!(),
//             // RouteToken::PeerIp => self
//             //     .peer_ip
//             //     .map(TypeValue::from)
//             //     .ok_or(VmError::InvalidFieldAccess),
//             // RouteToken::PeerAsn => self
//             //     .peer_asn
//             //     .map(TypeValue::from)
//             //     .ok_or(VmError::InvalidFieldAccess),
//             // _ => None,
//             // originator_id: ChangedOption {
//             //     value: None,
//             //     changed: false,
//             // },
//             // cluster_list: ChangedOption {
//             //     value: None,
//             //     changed: false,
//             // },
//             // RouteToken::ExtendedCommunities => self.raw_message.raw_message.ext_communities()
//             //         .map(|c| c.collect::<Vec<ExtendedCommunity>>()).into(),
//             // RouteToken::As4Path => self.raw_message.raw_message.as4path(),
//             // as4_aggregator: ChangedOption {
//             //     value: None,
//             //     changed: false,
//             // },
//             // connector: ChangedOption {
//             //     value: None,
//             //     changed: false,
//             // },
//             // as_path_limit: ChangedOption {
//             //     value: None,
//             //     changed: false,
//             // },
//             // pmsi_tunnel: ChangedOption {
//             //     value: None,
//             //     changed: false,
//             // },
//             // ipv6_extended_communities: ChangedOption {
//             //     value: None,
//             //     changed: false,
//             // },
//             // RouteToken::largeCommunities => self.raw_message.raw_message
//             //         .large_communities()
//             //         .map(|c| c.collect::<Vec<LargeCommunity>>()).into()
//             // bgpsec_as_path: ChangedOption {
//             //     value: None,
//             //     changed: false,
//             // },
//             // attr_set: ChangedOption {
//             //     value: None,
//             //     changed: false,
//             // },
//             // rsrvd_development: ChangedOption {
//             //     value: None,
//             //     changed: false,
//             // },
//         }
//     }

//     pub fn status(&self) -> NlriStatus {
//         if let TypeValue::Builtin(BuiltinTypeValue::NlriStatus(
//             route_status,
//         )) = self.status_deltas.current()
//         {
//             route_status
//         } else {
//             debug!("Invalid route status in {:?}. Marking as Unparsable and continuing", self.status_deltas);
//             // If somehow can't parse a part one or more Path Attributes in
//             // the PDU, we mark the state and move on.
//             NlriStatus::Unparsable
//         }
//     }
// }

// //------------ RouteDeltas --------------------------------------------------

// // The history of changes to this route. Each Delta holds the attributes that
// // were originally present in the raw message, their modifications and newly
// // created ones.
// //
// // The list of deltas describes the changes that were made by one Rotonda
// // unit along the way.
// #[derive(Debug, Clone, Eq, PartialEq, Default, Hash, Serialize)]
// struct AttributeDeltaList {
//     deltas: Vec<AttributeDelta>,
//     // The delta that was handed out the most recently. This is the only
//     // delta that can be written back!
//     locked_delta: Option<usize>,
// }

// impl AttributeDeltaList {
//     fn new() -> Self {
//         Self {
//             deltas: vec![],
//             locked_delta: None,
//         }
//     }

//     // Gets the most recently added delta in this list.
//     fn get_latest_change_set(&self) -> Option<&AttrChangeSet> {
//         self.deltas.last().map(|d| &d.attributes)
//     }

//     // Adds a new delta to the list.
//     fn store_delta(&mut self, delta: AttributeDelta) -> Result<(), VmError> {
//         if let Some(locked_delta) = self.locked_delta {
//             if locked_delta != delta.delta_index {
//                 debug!("Locked Attribute Delta {:?} with index {}. It cannot be written right now.", self.locked_delta, delta.delta_index);
//                 return Err(VmError::DeltaLocked);
//             }
//         }

//         self.deltas.push(delta);
//         self.locked_delta = None;

//         Ok(())
//     }

//     fn acquire_new_delta(&mut self) -> Result<usize, VmError> {
//         if self.locked_delta.is_none() {
//             let delta_index = self.deltas.len();
//             self.locked_delta = Some(delta_index);
//             Ok(delta_index)
//         } else {
//             Err(VmError::DeltaLocked)
//         }
//     }
// }

// //------------ AttributeDelta ----------------------------------------------

// // A set of attribute changes that were atomically created by a Rotonda
// // unit in one go (with one logical timestamp).
// #[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize)]
// pub struct AttributeDelta {
//     // delta_id: (RotondaId, LogicalTime),
//     delta_index: usize,
//     pub attributes: AttrChangeSet,
// }

// impl AttributeDelta {
//     fn new(
//         // delta_id: (RotondaId, LogicalTime),
//         delta_index: usize,
//         attributes: AttrChangeSet,
//     ) -> Self {
//         Self {
//             // delta_id,
//             delta_index,
//             attributes,
//         }
//     }
// }

// #[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize)]
// struct RouteStatusDelta {
//     delta_id: (RotondaId, LogicalTime),
//     status: NlriStatus,
// }

// impl RouteStatusDelta {
//     pub fn new(
//         delta_id: (RotondaId, LogicalTime),
//         status: NlriStatus,
//     ) -> Self {
//         Self { delta_id, status }
//     }
// }

// #[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize)]
// struct RouteStatusDeltaList(SmallVec<[RouteStatusDelta; 2]>);

// impl RouteStatusDeltaList {
//     pub fn new(delta: RouteStatusDelta) -> Self {
//         let mut v = SmallVec::with_capacity(1);
//         v.push(delta);
//         Self(v)
//     }

//     pub fn current(&self) -> TypeValue {
//         if let Some(status_delta) = self.0.iter().last() {
//             status_delta.status.into()
//         } else {
//             // This is an actual error, it should NOT happen and would be a
//             // logical internal error in Roto.
//             debug!("Ignoring empty Route Status Delta List: {:?}. Marking status and continuing", self.0);
//             NlriStatus::Unparsable.into()
//         }
//     }

//     pub fn current_as_ref(&self) -> Option<&TypeValue> {
//         None
//     }
// }

// //------------ BgpUpdateMessage ------------------------------------------------

// // A data-structure that stores the array of bytes of the incoming BGP Update
// // message, together with its logical timestamp and an ID of the instance
// // and/or unit that received it originally.
// //
// // The `attr_cache` AttributeList allows readers to get a reference to a path
// // attribute. This avoids having to clone from the AttributeLists in the
// // iterator over the latest attributes.
// // #[derive(Debug, Clone, Serialize)]
// // pub struct BgpUpdateMessageOld {
// //     pub(crate) message_id: (RotondaId, LogicalTime),
// //     pub(crate) raw_message: BgpUpdateMessage,
// // }

// // impl BgpUpdateMessageOld {
// //     pub fn new(
// //         message_id: (RotondaId, u64),
// //         raw_message: BgpUpdateMessage,
// //     ) -> Self {
// //         Self {
// //             message_id,
// //             raw_message,
// //         }
// //     }

// //     pub fn message_id(&self) -> (RotondaId, u64) {
// //         self.message_id
// //     }

// //     pub fn raw_message(&self) -> &BgpUpdateMessage {
// //         &self.raw_message
// //     }
// // }

// // impl PartialEq for BgpUpdateMessageOld {
// //     fn eq(&self, other: &Self) -> bool {
// //         self.message_id == other.message_id
// //     }
// // }

// // impl std::hash::Hash for BgpUpdateMessageOld {
// //     fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
// //         self.raw_message.hash(state);
// //     }
// // }

// // impl Eq for BgpUpdateMessageOld {}

// // impl BgpUpdateMessageOld {
// //     pub(crate) fn get_props_for_field(
// //         field_name: &crate::ast::Identifier,
// //     ) -> Result<(TypeDef, crate::traits::Token), CompileError>
// //     where
// //         Self: std::marker::Sized,
// //     {
// //         match field_name.ident.as_str() {
// //             // `nlris` is a fake field (at least for now), it just serves to
// //             // host the `afi` and `safi` field.
// //             "nlris" => {
// //                 Ok((TypeDef::BgpUpdateMessage, Token::FieldAccess(vec![])))
// //             }
// //             "afi" => Ok((
// //                 TypeDef::ConstEnumVariant("AFI".into()),
// //                 Token::FieldAccess(vec![usize::from(
// //                     OldBgpUpdateMessageToken::Afi,
// //                 ) as u8]),
// //             )),
// //             "safi" => Ok((
// //                 TypeDef::ConstEnumVariant("SAFI".into()),
// //                 Token::FieldAccess(vec![usize::from(
// //                     OldBgpUpdateMessageToken::Safi,
// //                 ) as u8]),
// //             )),
// //             _ => Err(format!(
// //                 "Unknown field '{}' for type BgpUpdateMessage",
// //                 field_name.ident
// //             )
// //             .into()),
// //         }
// //     }

// //     pub(crate) fn get_value_owned_for_field(
// //         &self,
// //         field_token: usize,
// //     ) -> Result<Option<TypeValue>, VmError> {
// //         match field_token.try_into()? {
// //             OldBgpUpdateMessageToken::Nlris => Ok(Some(TypeValue::Unknown)),
// //             OldBgpUpdateMessageToken::Afi => {
// //                 if let Ok(mut nlri) = self.raw_message.announcements() {
// //                     if let Some(Ok(announce)) = nlri.next() {
// //                         Ok(Some(TypeValue::Builtin(
// //                             BuiltinTypeValue::ConstU16EnumVariant(
// //                                 EnumVariant {
// //                                     enum_name: "AFI".into(),
// //                                     value: announce.afi_safi().afi().into(),
// //                                 },
// //                             ),
// //                         )))
// //                     } else {
// //                         Ok(None)
// //                     }
// //                 } else {
// //                     debug!(
// //                         "Ignoring invalid announcements in BGP update \
// //                         message with id ({},{})",
// //                         self.message_id.0, self.message_id.1
// //                     );
// //                     Err(VmError::InvalidPayload)
// //                 }
// //             }
// //             OldBgpUpdateMessageToken::Safi => {
// //                 if let Ok(mut nlri) = self.raw_message.announcements() {
// //                     if let Some(Ok(announce)) = nlri.next() {
// //                         Ok(Some(TypeValue::Builtin(
// //                             BuiltinTypeValue::ConstU8EnumVariant(
// //                                 EnumVariant {
// //                                     enum_name: "SAFI".into(),
// //                                     value: announce.afi_safi().safi().into(),
// //                                 },
// //                             ),
// //                         )))
// //                     } else {
// //                         Ok(None)
// //                     }
// //                 } else {
// //                     debug!(
// //                         "Ignoring invalid announcements in BGP update \
// //                         message with id ({},{})",
// //                         self.message_id.0, self.message_id.1
// //                     );
// //                     Err(VmError::InvalidPayload)
// //                 }
// //             }
// //         }
// //     }
// // }

// // impl RotoType for routecore::bgp::message::UpdateMessage<bytes::Bytes> {
// //     fn get_props_for_method(
// //         _ty: TypeDef,
// //         method_name: &crate::ast::Identifier,
// //     ) -> Result<MethodProps, CompileError>
// //     where
// //         Self: std::marker::Sized,
// //     {
// //         Err(format!(
// //             "Unknown method: '{}' for type BgpUpdateMessage",
// //             method_name.ident
// //         )
// //         .into())
// //     }

// //     fn into_type(self, ty: &TypeDef) -> Result<TypeValue, CompileError>
// //     where
// //         Self: std::marker::Sized,
// //     {
// //         Err(format!(
// //             "BgpUpdateMessage cannot be converted to type {} (or any other type)",
// //             ty
// //         )
// //         .into())
// //     }

// //     fn exec_value_method<'a>(
// //         &'a self,
// //         method_token: usize,

// //         _args: &'a [StackValue<T>],
// //         _res_type: TypeDef,
// //     ) -> Result<TypeValue, VmError> {
// //         panic!("STOP BEFORE EXEC THIS THING!");
// //         match method_token.try_into()? {
// //             OldBgpUpdateMessageToken::Afi => {
// //                 if let Ok(mut nlri) = self.announcements() {
// //                     if let Some(Ok(announce)) = nlri.next() {
// //                         Ok(TypeValue::Builtin(
// //                             BuiltinTypeValue::ConstU16EnumVariant(
// //                                 EnumVariant {
// //                                     enum_name: "AFI".into(),
// //                                     value: announce.afi_safi().afi().into(),
// //                                 },
// //                             ),
// //                         ))
// //                     } else {
// //                         Err(VmError::InvalidValueType)
// //                     }
// //                 } else {
// //                     Err(VmError::InvalidValueType)
// //                 }
// //             }
// //             OldBgpUpdateMessageToken::Safi => {
// //                 if let Ok(mut nlri) = self.announcements() {
// //                     if let Some(Ok(announce)) = nlri.next() {
// //                         Ok(TypeValue::Builtin(
// //                             BuiltinTypeValue::ConstU8EnumVariant(
// //                                 EnumVariant {
// //                                     enum_name: "SAFI".into(),
// //                                     value: announce.afi_safi().safi().into(),
// //                                 },
// //                             ),
// //                         ))
// //                     } else {
// //                         Err(VmError::InvalidValueType)
// //                     }
// //                 } else {
// //                     Err(VmError::InvalidValueType)
// //                 }
// //             }
// //             _ => Err(VmError::InvalidMethodCall),
// //         }
// //     }

// //     fn exec_consume_value_method(
// //         self,
// //         _method_token: usize,
// //         _args: Vec<TypeValue>,
// //         _res_type: TypeDef,
// //     ) -> Result<TypeValue, VmError> {
// //         Err(VmError::InvalidMethodCall)
// //     }

// //     fn exec_type_method<'a>(
// //         _method_token: usize,
// //         _args: &[StackValue<T>],
// //         _res_type: TypeDef,
// //     ) -> Result<TypeValue, VmError> {
// //         Err(VmError::InvalidMethodCall)
// //     }
// // }

// // impl From<routecore::bgp::message::UpdateMessage<bytes::Bytes>> for TypeValue {
// //     fn from(raw: routecore::bgp::message::UpdateMessage<bytes::Bytes>) -> Self {
// //         TypeValue::Builtin(BuiltinTypeValue::BgpUpdateMessage(BytesRecord::from(raw)))
// //     }
// // }

// // #[derive(Debug)]
// // enum OldBgpUpdateMessageToken {
// //     Nlris = 0,
// //     Afi = 1,
// //     Safi = 2,
// // }

// // impl TryFrom<usize> for OldBgpUpdateMessageToken {
// //     type Error = VmError;

// //     fn try_from(val: usize) -> Result<Self, VmError> {
// //         match val {
// //             0 => Ok(OldBgpUpdateMessageToken::Nlris),
// //             1 => Ok(OldBgpUpdateMessageToken::Afi),
// //             2 => Ok(OldBgpUpdateMessageToken::Safi),
// //             _ => Err(VmError::InvalidPayload),
// //         }
// //     }
// // }

// // impl From<OldBgpUpdateMessageToken> for usize {
// //     fn from(val: OldBgpUpdateMessageToken) -> Self {
// //         match val {
// //             OldBgpUpdateMessageToken::Nlris => 0,
// //             OldBgpUpdateMessageToken::Afi => 1,
// //             OldBgpUpdateMessageToken::Safi => 2,
// //         }
// //     }
// // }

// // pub as_path: ChangedOption<AsPath<Vec<MaterializedPathSegment>>>,
// //     pub origin_type: ChangedOption<OriginType>,
// //     pub next_hop: ChangedOption<NextHop>,
// //     pub multi_exit_discriminator: ChangedOption<MultiExitDisc>,
// //     pub local_pref: ChangedOption<LocalPref>,
// //     pub atomic_aggregate: ChangedOption<bool>,
// //     pub aggregator: ChangedOption<Aggregator>,
// //     pub communities: ChangedOption<Vec<Community>>,
// //     // mp_reach_nlri: Vec<Prefix>,
// //     // mp_unreach_nlri: Vec<Prefix>,
// //     pub originator_id: ChangedOption<Todo>,
// //     pub cluster_list: ChangedOption<Todo>,
// //     pub extended_communities: ChangedOption<Vec<ExtendedCommunity>>,
// //     pub as4_path: ChangedOption<AsPath<Vec<Asn>>>,
// //     pub as4_aggregator: ChangedOption<Todo>,
// //     pub connector: ChangedOption<Todo>, // Connector,
// //     pub as_path_limit: ChangedOption<(u8, u32)>,
// //     pub pmsi_tunnel: ChangedOption<Todo>, // PmsiTunnel,
// //     pub ipv6_extended_communities: ChangedOption<Vec<Ipv6ExtendedCommunity>>,
// //     pub large_communities: ChangedOption<Vec<LargeCommunity>>,
// //     pub bgpsec_as_path: ChangedOption<Todo>, // BgpsecAsPath,
// //     pub attr_set: ChangedOption<Todo>,       // AttrSet,
// //     pub rsrvd_development: ChangedOption<Todo>

// // impl From<BasicRoute> for TypeValue {
// //     fn from(val: BasicRoute) -> Self {
// //         TypeValue::Builtin(BuiltinTypeValue::Route(val))
// //     }
// // }

// // impl std::fmt::Display for RawRouteWithDeltas {
// //     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
// //         writeln!(f, "prefix : {}", self.prefix)?;
// //         writeln!(f, "raw    : {:#?}", self.raw_message)?;
// //         writeln!(f, "deltas : {:#?}", self.attribute_deltas)?;
// //         writeln!(f, "status : {:?}", self.status_deltas.current())
// //     }
// // }

// //------------ Modification & Creation of new Updates -----------------------

// // #[derive(Debug, Hash, Serialize, Clone, PartialEq, Eq)]
// // pub struct BgpUpdateMessage(
// //     pub routecore::bgp::message::UpdateMessage<bytes::Bytes>,
// // );

// // impl BgpUpdateMessage {
// //     pub fn new(
// //         bytes: bytes::Bytes,
// //         config: SessionConfig,
// //     ) -> Result<Self, VmError> {
// //         Ok(
// //             Self(
// //                 routecore::bgp::message::UpdateMessage::<bytes::Bytes>
// //                     ::from_octets(bytes, config).map_err(|_| VmError::InvalidPayload)?
// //                 )
// //             )
// //     }

// //     // Materialize a ChangeSet from the Update message. The materialized
// //     // Change set is completely self-contained (no references of any kind) &
// //     // holds all the attributes of the current BGP Update message.
// //     pub fn create_changeset(
// //         &self,
// //         prefix: Prefix,
// //         peer_ip: Option<IpAddr>,
// //         peer_asn: Option<Asn>,
// //         router_id: Option<Arc<String>>,
// //         afi_safi: AfiSafi,
// //         path_id: Option<PathId>,
// //     ) -> Result<AttrChangeSet, VmError> {
// //         let next_hop = self
// //             .0
// //             .mp_next_hop()
// //             .ok()
// //             .flatten()
// //             .or_else(|| self.0.conventional_next_hop().ok().flatten());
// //         Ok(AttrChangeSet {
// //             prefix: ReadOnlyScalarOption::<Prefix>::new(prefix.into()),
// //             as_path: VectorOption::<HopPath>::from(
// //                 self.0.aspath().ok().flatten().map(|p| p.to_hop_path()),
// //             ),
// //             origin_type: ScalarOption::<OriginType>::from(
// //                 self.0.origin().map_err(VmError::from)?,
// //             ),
// //             next_hop: ScalarOption::<NextHop>::from(next_hop),
// //             multi_exit_discriminator: ScalarOption::from(
// //                 self.0.multi_exit_disc().map_err(VmError::from)?,
// //             ),
// //             local_pref: ScalarOption::from(
// //                 self.0.local_pref().map_err(VmError::from)?,
// //             ),
// //             atomic_aggregate: ScalarOption::from(Some(
// //                 self.0.is_atomic_aggregate().map_err(VmError::from)?,
// //             )),
// //             aggregator: ScalarOption::from(
// //                 self.0.aggregator().map_err(VmError::from)?,
// //             ),
// //             communities: VectorOption::from(
// //                 self.0
// //                     .all_human_readable_communities()
// //                     .map_err(VmError::from)?,
// //             ),
// //             peer_ip: peer_ip.into(),
// //             peer_asn: peer_asn.into(),
// //             router_id: router_id
// //                 .map(|v| StringLiteral(String::clone(&v)))
// //                 .into(),
// //             afi_safi: ReadOnlyScalarOption::<AfiSafi>::new(afi_safi.into()),
// //             path_id: path_id.into(),
// //             originator_id: Todo,
// //             cluster_list: Todo,
// //             extended_communities: Todo,
// //             // value: self
// //             //     .ext_communities()
// //             //     .map(|c| c.collect::<Vec<ExtendedCommunity>>()),
// //             as4_path: VectorOption::from(
// //                 self.0.as4path().ok().flatten().map(|p| p.to_hop_path()),
// //             ),
// //             connector: Todo,
// //             as_path_limit: Todo,
// //             pmsi_tunnel: Todo,
// //             ipv6_extended_communities: Todo,
// //             large_communities: Todo,
// //             // large_communities: VectorOption::from(self.0
// //             //     .large_communities()
// //             //     .map(|c| c.collect::<Vec<LargeCommunity>>())),
// //             bgpsec_as_path: Todo,
// //             attr_set: Todo,
// //             rsrvd_development: Todo,
// //             as4_aggregator: Todo,
// //         })
// //     }

// //     pub fn create_changeset2(
// //         &self,
// //         // prefix: Prefix,
// //         // peer_ip: Option<IpAddr>,
// //         // peer_asn: Option<Asn>,
// //         // router_id: Option<Arc<String>>,
// //         afi_safi: AfiSafi,
// //         // path_id: Option<PathId>,
// //     ) -> Result<AttrChangeSet2, VmError> {
// //         let next_hop = self
// //             .0
// //             .find_next_hop(afi_safi)
// //             // .mp_next_hop()
// //             .ok();
// //             // .flatten()
// //             // .or_else(|| self.0.conventional_next_hop().ok().flatten());
// //         Ok(AttrChangeSet2 {
// //             as_path: VectorOption::<HopPath>::from(
// //                 self.0.aspath().ok().flatten().map(|p| p.to_hop_path()),
// //             ),
// //             origin_type: ScalarOption::<OriginType>::from(
// //                 self.0.origin().map_err(VmError::from)?,
// //             ),
// //             next_hop: ScalarOption::<NextHop>::from(next_hop),
// //             multi_exit_discriminator: ScalarOption::from(
// //                 self.0.multi_exit_disc().map_err(VmError::from)?,
// //             ),
// //             local_pref: ScalarOption::from(
// //                 self.0.local_pref().map_err(VmError::from)?,
// //             ),
// //             atomic_aggregate: ScalarOption::from(Some(
// //                 self.0.is_atomic_aggregate().map_err(VmError::from)?,
// //             )),
// //             aggregator: ScalarOption::from(
// //                 self.0.aggregator().map_err(VmError::from)?,
// //             ),
// //             communities: VectorOption::from(
// //                 self.0
// //                     .all_human_readable_communities()
// //                     .map_err(VmError::from)?,
// //             ),
// //             originator_id: Todo,
// //             cluster_list: Todo,
// //             extended_communities: Todo,
// //             // value: self
// //             //     .ext_communities()
// //             //     .map(|c| c.collect::<Vec<ExtendedCommunity>>()),
// //             as4_path: VectorOption::from(
// //                 self.0.as4path().ok().flatten().map(|p| p.to_hop_path()),
// //             ),
// //             connector: Todo,
// //             as_path_limit: Todo,
// //             pmsi_tunnel: Todo,
// //             ipv6_extended_communities: Todo,
// //             large_communities: Todo,
// //             // large_communities: VectorOption::from(self.0
// //             //     .large_communities()
// //             //     .map(|c| c.collect::<Vec<LargeCommunity>>())),
// //             bgpsec_as_path: Todo,
// //             attr_set: Todo,
// //             rsrvd_development: Todo,
// //             as4_aggregator: Todo,
// //         })
// //     }

// //     // Create a new BGP Update message by applying the attributes changes
// //     // in the supplied change set to our current Update message.
// //     pub fn create_update_from_changeset<T: ScalarValue, V: VectorValue>(
// //         _change_set: &AttrChangeSet,
// //     ) -> Self {
// //         todo!()
// //     }
// // }

// // impl From<routecore::bgp::message::UpdateMessage<bytes::Bytes>>
// //     for BgpUpdateMessage
// // {
// //     fn from(
// //         value: routecore::bgp::message::UpdateMessage<bytes::Bytes>,
// //     ) -> Self {
// //         BgpUpdateMessage(value)
// //     }
// // }
