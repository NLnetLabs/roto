use std::hash::Hash;
use std::hash::Hasher;
use std::net::IpAddr;
use std::net::SocketAddr;

use chrono::Utc;
use routecore::bgp::communities::HumanReadableCommunity;
use routecore::bgp::message::nlri::BasicNlri;
use routecore::bgp::message::nlri::FlowSpecNlri;
use routecore::bgp::message::nlri::Nlri;
use routecore::bgp::path_attributes::BgpIdentifier;
use routecore::bgp::path_attributes::PaMap;
use routecore::bgp::path_attributes::PathAttribute;
use routecore::bgp::types::LocalPref;
use routecore::bgp::types::MultiExitDisc;
use routecore::bgp::types::NextHop;
use routecore::bgp::workshop::afisafi_nlri::AfiSafiNlri;
use routecore::bgp::workshop::afisafi_nlri::HasBasicNlri;
use routecore::bgp::workshop::route::RouteWorkshop;
use serde::Deserialize;
use std::fmt::{Display, Formatter};

use log::{debug, error, trace};
use paste::paste;
use serde::{self, Serialize};

use crate::attr_change_set::ScalarValue;
use crate::compiler::compile::CompileError;
use crate::traits::RotoType;
use crate::types::collections::ElementTypeValue;
use crate::types::collections::List;
use crate::types::typedef::MethodProps;
use crate::types::typedef::RecordTypeDef;
use crate::vm::{StackValue, VmError};
use crate::{
    ast,
    traits::{self, Token},
    types::lazyrecord_types::BgpUpdateMessage,
    vm::FieldIndex,
};
use crate::{
    createtoken, intotype, minimalscalartype, setmethodonly,
    typevaluefromimpls,
};

use super::super::typevalue::TypeValue;
use super::BuiltinTypeValue;
use super::{super::typedef::TypeDef, BytesRecord, NlriStatus};

use routecore::addr::Prefix;
use routecore::asn::Asn;
use routecore::bgp::message::nlri::PathId;
use routecore::bgp::path_attributes::AttributesMap;

pub type LogicalTime = u64;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct BasicRoute(
    routecore::bgp::workshop::route::RouteWorkshop<bytes::Bytes, BasicNlri>,
);

impl BasicRoute {
    pub fn new<
        N: AfiSafiNlri<bytes::Bytes, Nlri = BasicNlri> + HasBasicNlri,
    >(
        route: routecore::bgp::workshop::route::RouteWorkshop<
            bytes::Bytes,
            N,
        >,
    ) -> Self {
        BasicRoute(RouteWorkshop::from_pa_map(
            route.nlri().basic_nlri(),
            route.attributes().clone(),
        ))
    }

    pub fn prefix(&self) -> Prefix {
        self.0.nlri().prefix()
    }

    pub fn path_id(&self) -> Option<PathId> {
        self.0.nlri().path_id()
    }

    pub fn get_field_by_index(
        &self,
        field_index: &FieldIndex,
    ) -> Result<TypeValue, VmError> {
        trace!("get_field_by_index {:?} for BasicRoute {:?}", field_index, self);
        let mut index_iter = field_index.iter();
        let index = index_iter.next();
        
        match index {
            // The NLRI
            Some(0) => match index_iter.next() {
                Some(0) => Ok(TypeValue::from(self.0.nlri().prefix)),
                Some(1) => self.0.nlri().path_id.map(TypeValue::from).ok_or_else(|| VmError::InvalidPathAttribute),
                _ => Err(VmError::InvalidPathAttribute)
            }
            // The Path Attributes
            Some(1) => {
                if let Some(index) = index_iter.next() {
                    let attr = self.0.attributes().get_by_type_code(index);

                    attr.map(|pa| TypeValue::from(pa.clone()))
                        .ok_or(VmError::InvalidPathAttribute)
                } else {
                    Err(VmError::InvalidPathAttribute)
                }
            }
            _ => Err(VmError::InvalidFieldAccess)
        }
    }

    pub(crate) fn get_mut_field_by_index(
        &mut self,
        field_index: &FieldIndex,
    ) -> Result<TypeValue, VmError> {
        trace!("get_mut_field_by_index ({:?}) for BasicRoute {:?}", field_index, self);
        let mut index_iter = field_index.iter();
        let index = index_iter.next();
        
        match index {
            // The NLRI
            Some(0) => match index_iter.next() {
                Some(0) => { 
                    trace!("index {:?}", index);
                    trace!("PREFIX! {:?}", self.0.nlri().prefix);
                    Ok(TypeValue::from(self.0.nlri().prefix))
                },
                Some(1) => self.0.nlri().path_id.map(TypeValue::from).ok_or_else(|| VmError::InvalidPathAttribute),
                _ => Err(VmError::InvalidPathAttribute)
            }
            // The Path Attributes
            Some(1) => {
                if let Some(index) = index_iter.next() {
                    let attr = self.0.attributes_mut().get_mut_by_type_code(index);
                    attr.map(|pa| TypeValue::from(pa.clone()))
                        .ok_or(VmError::InvalidPathAttribute)
                } else {
                    Err(VmError::InvalidPathAttribute)
                }
            }
            _ => Err(VmError::InvalidFieldAccess)
        }
    }

    pub(crate) fn hash_field<H: Hasher>(
        &self,
        state: &mut H,
        field_index: &FieldIndex,
    ) -> Result<(), VmError> {
        let index = u8::try_from(field_index.first()?)
            .map_err(|_| VmError::InvalidPathAttribute)?;
        self.0.attributes().get_by_type_code(index).hash(state);
        Ok(())
    }
}

impl From<RouteWorkshop<bytes::Bytes, BasicNlri>> for TypeValue {
    fn from(value: RouteWorkshop<bytes::Bytes, BasicNlri>) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Route(BasicRoute(value)))
    }
}

impl From<BasicRoute> for TypeValue {
    fn from(value: BasicRoute) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Route(value))
    }
}

//------------ SingleNlri ----------------------------------------------------

#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
pub struct Route<N: Clone + Hash> {
    // A single piece of NLRI, e.g. one
    nlri: N,
    status: NlriStatus,
    // afi_safi: AfiSafi,
    path_attributes: AttributesMap,
}

// impl<N: Clone + Hash> MutableNlri<N> {
//     // Get either the moved last delta (it is removed from the Delta list), or
//     // get a freshly rolled ChangeSet from the raw message, if there are no
//     // deltas.
//     pub fn take_attrs(mut self) -> AttributesMap {
//         self.changes
//     }
// }

// impl<N: Clone + Hash> std::hash::Hash for Route<N> {
//     fn hash<H: Hasher>(&self, state: &mut H) {
//         self.nlri.hash(state);
//         self.path_attributes.hash(state);
//     }
// }

// #[derive(Debug, Eq, PartialEq, Clone, Serialize)]
// pub struct MaterializedRoute2 {
//     pub path_attributes: AttributesMap,
//     pub status: NlriStatus,
//     route_id: (RotondaId, LogicalTime),
// }

// impl<N: Clone + Hash> From<Route<N>> for MaterializedRoute2 {
//     fn from(raw_nlri: Route<N>) -> Self {
//         let status = raw_nlri.status;
//         let route_id = (RotondaId(0), 0);

//         MaterializedRoute2 {
//             path_attributes: raw_nlri.path_attributes,
//             status,
//             route_id,
//         }
//     }
// }

//------------ MutableBasicRoute ---------------------------------------------

// pub type PathAttributeMap = BTreeMap<PathAttributeType, PathAttribute>;

impl From<PathAttribute> for TypeValue {
    fn from(value: PathAttribute) -> Self {
        match value {
            PathAttribute::Origin(v) => {
                TypeValue::Builtin(BuiltinTypeValue::OriginType(v.inner()))
            }
            PathAttribute::AsPath(hop_path) => {
                TypeValue::Builtin(BuiltinTypeValue::AsPath(hop_path.inner()))
            }
            PathAttribute::ConventionalNextHop(next_hop) => {
                TypeValue::Builtin(BuiltinTypeValue::NextHop(
                    NextHop::Unicast(std::net::IpAddr::V4(
                        next_hop.inner().0,
                    )),
                ))
            }
            PathAttribute::MultiExitDisc(med) => TypeValue::Builtin(
                BuiltinTypeValue::MultiExitDisc(MultiExitDisc(med.inner().0)),
            ),
            PathAttribute::LocalPref(lp) => TypeValue::Builtin(
                BuiltinTypeValue::LocalPref(LocalPref(lp.inner().into())),
            ),
            PathAttribute::AtomicAggregate(aa) => {
                TypeValue::Builtin(BuiltinTypeValue::AtomicAggregate(aa))
            }
            PathAttribute::Aggregator(a) => TypeValue::Builtin(
                BuiltinTypeValue::AggregatorInfo(a.inner()),
            ),
            PathAttribute::StandardCommunities(comms) => {
                TypeValue::List(List(comms.inner().fmap(|c| {
                    ElementTypeValue::Primitive(
                        HumanReadableCommunity::from(*c).into(),
                    )
                })))
            }
            PathAttribute::OriginatorId(_) => todo!(),
            PathAttribute::ClusterList(_) => todo!(),
            PathAttribute::MpReachNlri(_) => todo!(),
            PathAttribute::MpUnreachNlri(_) => todo!(),
            PathAttribute::ExtendedCommunities(comms) => {
                TypeValue::List(List(comms.inner().fmap(|c| {
                    ElementTypeValue::Primitive(
                        HumanReadableCommunity::from(c).into(),
                    )
                })))
            }
            PathAttribute::As4Path(_) => todo!(),
            PathAttribute::As4Aggregator(_) => todo!(),
            PathAttribute::Connector(_) => todo!(),
            PathAttribute::AsPathLimit(_) => todo!(),
            PathAttribute::Ipv6ExtendedCommunities(_) => todo!(),
            PathAttribute::LargeCommunities(comms) => {
                TypeValue::List(List(comms.inner().fmap(|c| {
                    ElementTypeValue::Primitive(
                        HumanReadableCommunity::from(c).into(),
                    )
                })))
            }
            PathAttribute::Otc(_) => todo!(),
            PathAttribute::AttrSet(_) => todo!(),
            PathAttribute::Reserved(_) => todo!(),
            PathAttribute::Unimplemented(_) => todo!(),
            PathAttribute::Invalid(_, _, _) => todo!(),
        }
    }
}

// #[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
// #[serde(into = "MaterializedRoute2")]
// pub struct MutableBasicRoute {
//     path_attributes: AttributesMap,
//     prefix: Prefix,
//     path_id: Option<PathId>,
//     afi_safi: AfiSafi,
//     status: NlriStatus,
// }

// impl Route<routecore::bgp::message::nlri::BasicNlri> {
//     pub fn new(
//         prefix: Prefix,
//         path_id: Option<PathId>,
//         // afi_safi: AfiSafi,
//         path_attributes: AttributesMap,
//         status: NlriStatus,
//     ) -> Self {
//         Self {
//             path_attributes,
//             nlri: BasicNlri { prefix, path_id },
//             // afi_safi,
//             status,
//         }
//     }

//     pub fn prefix(&self) -> Prefix {
//         self.nlri.prefix
//     }

//     pub fn path_id(&self) -> Option<PathId> {
//         self.nlri.path_id
//     }

//     pub fn status(&self) -> NlriStatus {
//         self.status
//     }

//     pub fn get_field_by_index(
//         &self,
//         field_index: &FieldIndex,
//     ) -> Result<TypeValue, VmError> {
//         let index = u8::try_from(field_index.first()?)
//             .map_err(|_| VmError::InvalidPathAttribute)?;
//         self.path_attributes
//             .get(&index.into())
//             .map(|pa| TypeValue::from(pa.clone()))
//             .ok_or(VmError::InvalidPathAttribute)
//     }

//     pub fn get_mut_field_by_index(
//         &mut self,
//         field_index: &FieldIndex,
//     ) -> Result<TypeValue, VmError> {
//         let index = u8::try_from(field_index.first()?)
//             .map_err(|_| VmError::InvalidPathAttribute)?;
//         self.path_attributes
//             .get_mut(&index.into())
//             .map(|pa| TypeValue::from(pa.clone()))
//             .ok_or(VmError::InvalidPathAttribute)
//     }

    // pub fn take_field_by_index(
    //     &mut self,
    //     field_index: &FieldIndex,
    // ) -> Result<TypeValue, VmError> {
    //     let index = u8::try_from(field_index.first()?).map_err(|_| VmError::InvalidPathAttribute)?;
    // self.path_attributes.get(&index.into()).map(|pa| TypeValue::from(*pa)).ok_or(VmError::InvalidPathAttribute)
    //{
    // BasicRouteToken::AsPath => {
    //     std::mem::take(&mut self.path_attributes.as_path)
    //         .into_opt()
    //         .map(TypeValue::from)
    //         .ok_or(VmError::InvalidFieldAccess)
    // }
    // BasicRouteToken::OriginType => {
    //     std::mem::take(&mut self.path_attributes.origin_type)
    //         .into_opt()
    //         .map(TypeValue::from)
    //         .ok_or(VmError::InvalidFieldAccess)
    // }
    // BasicRouteToken::NextHop => {
    //     std::mem::take(&mut self.path_attributes.next_hop)
    //         .into_opt()
    //         .map(TypeValue::from)
    //         .ok_or(VmError::InvalidFieldAccess)
    // }
    // BasicRouteToken::MultiExitDisc => std::mem::take(
    //     &mut self.path_attributes.multi_exit_discriminator,
    // )
    // .into_opt()
    // .map(TypeValue::from)
    // .ok_or(VmError::InvalidFieldAccess),
    // BasicRouteToken::LocalPref => {
    //     std::mem::take(&mut self.path_attributes.local_pref)
    //         .into_opt()
    //         .map(TypeValue::from)
    //         .ok_or(VmError::InvalidFieldAccess)
    // }
    // BasicRouteToken::AtomicAggregate => {
    //     std::mem::take(&mut self.path_attributes.atomic_aggregate)
    //         .into_opt()
    //         .map(TypeValue::from)
    //         .ok_or(VmError::InvalidFieldAccess)
    // }
    // BasicRouteToken::Aggregator => {
    //     std::mem::take(&mut self.path_attributes.aggregator)
    //         .into_opt()
    //         .map(TypeValue::from)
    //         .ok_or(VmError::InvalidFieldAccess)
    // }
    // BasicRouteToken::Communities => {
    //     std::mem::take(&mut self.path_attributes.communities)
    //         .into_opt()
    //         .map(TypeValue::from)
    //         .ok_or(VmError::InvalidFieldAccess)
    // }
    // BasicRouteToken::Prefix => Err(VmError::InvalidCommandArg),
    // BasicRouteToken::Status => Err(VmError::InvalidCommandArg),
    // BasicRouteToken::Provenance => Err(VmError::InvalidCommandArg),

    // }

    // pub fn overlay_field_as_stack_value(
    //     &self,
    //     field_index: &FieldIndex,
    //     message: &BytesRecord<BgpUpdateMessage>,
    // ) -> Result<StackValue, VmError> {
    //     match field_index.first()?.try_into()? {
    //         BasicRouteToken::AsPath => {
    //             self.path_attributes.get(&PathAttributeType::AsPath).as_stack_value_or(|| {
    //                 message
    //                     .bytes_parser()
    //                     .aspath()
    //                     .ok()
    //                     .flatten()
    //                     .map(|tv| {
    //                         StackValue::Owned(TypeValue::from(
    //                             tv.to_hop_path(),
    //                         ))
    //                     })
    //                     .ok_or(VmError::InvalidFieldAccess)
    //             })
    //         }
    //         BasicRouteToken::OriginType => {
    //             self.path_attributes.as_path.as_stack_value_or(|| {
    //                 message
    //                     .bytes_parser()
    //                     .origin()
    //                     .ok()
    //                     .flatten()
    //                     .map(|tv| StackValue::Owned(TypeValue::from(tv)))
    //                     .ok_or(VmError::InvalidFieldAccess)
    //             })
    //         }
    //         BasicRouteToken::NextHop => {
    //             self.path_attributes.as_path.as_stack_value_or(|| {
    //                 message
    //                     .bytes_parser()
    //                     .find_next_hop(self.afi_safi)
    //                     .map_err(|_| VmError::InvalidFieldAccess)
    //                     .map(|tv| StackValue::Owned(TypeValue::from(tv)))
    //             })
    //         }
    //         BasicRouteToken::MultiExitDisc => {
    //             self.path_attributes.as_path.as_stack_value_or(|| {
    //                 message
    //                     .bytes_parser()
    //                     .multi_exit_disc()
    //                     .ok()
    //                     .flatten()
    //                     .map(|tv| StackValue::Owned(TypeValue::from(tv)))
    //                     .ok_or(VmError::InvalidFieldAccess)
    //             })
    //         }
    //         BasicRouteToken::LocalPref => {
    //             self.path_attributes.as_path.as_stack_value_or(|| {
    //                 message
    //                     .bytes_parser()
    //                     .local_pref()
    //                     .ok()
    //                     .flatten()
    //                     .map(|tv| StackValue::Owned(TypeValue::from(tv)))
    //                     .ok_or(VmError::InvalidFieldAccess)
    //             })
    //         }
    //         BasicRouteToken::AtomicAggregate => {
    //             self.path_attributes.as_path.as_stack_value_or(|| {
    //                 message
    //                     .bytes_parser()
    //                     .is_atomic_aggregate()
    //                     .ok()
    //                     .map(|tv| StackValue::Owned(TypeValue::from(tv)))
    //                     .ok_or(VmError::InvalidFieldAccess)
    //             })
    //         }
    //         BasicRouteToken::Aggregator => {
    //             self.path_attributes.as_path.as_stack_value_or(|| {
    //                 message
    //                     .bytes_parser()
    //                     .aggregator()
    //                     .ok()
    //                     .flatten()
    //                     .map(|tv| StackValue::Owned(TypeValue::from(tv)))
    //                     .ok_or(VmError::InvalidFieldAccess)
    //             })
    //         }
    //         BasicRouteToken::Communities => {
    //             self.path_attributes.as_path.as_stack_value_or(|| {
    //                 message
    //                     .bytes_parser()
    //                     .all_human_readable_communities()
    //                     .ok()
    //                     .flatten()
    //                     .map(|tv| StackValue::Owned(TypeValue::from(tv)))
    //                     .ok_or(VmError::InvalidFieldAccess)
    //             })
    //         }
    //         BasicRouteToken::Prefix => Err(VmError::InvalidFieldAccess),
    //         BasicRouteToken::Status => Err(VmError::InvalidFieldAccess),
    //         BasicRouteToken::Provenance => Err(VmError::InvalidFieldAccess),
    //     }
    // }

//     pub fn hash_field<'a, H: Hasher>(
//         &self,
//         state: &'a mut H,
//         field_index: &FieldIndex,
//     ) -> Result<(), VmError> {
//         let index = u8::try_from(field_index.first()?)
//             .map_err(|_| VmError::InvalidPathAttribute)?;
//         self.path_attributes.get(&index.into()).hash(state);
//         Ok(())
//     }

//     // Get either the moved last delta (it is removed from the Delta list), or
//     // get a freshly rolled ChangeSet from the raw message, if there are no
//     // deltas.
//     pub fn take_attrs(self) -> AttributesMap {
//         self.path_attributes
//     }

//     pub fn store_attrs(&mut self, path_attribues: AttributesMap) {
//         self.path_attributes = path_attribues
//     }

//     pub fn get_attrs(&mut self) -> &AttributesMap {
//         &self.path_attributes
//     }

//     pub fn get_attrs_mut(&mut self) -> &mut AttributesMap {
//         &mut self.path_attributes
//     }
// }

// impl TryFrom<BasicRoute> for Route<BasicNlri> {
//     type Error = VmError;

//     fn try_from(value: BasicRoute) -> Result<Self, VmError> {
//         let target = bytes::BytesMut::new();

//         let path_attributes = routecore::bgp::message::update_builder::UpdateBuilder::from_update_message(
//                 value.message.bytes_parser(),
//                 SessionConfig::modern(),
//                 target
//             ).map(|b| b.into_attributes().clone()
//         ).map_err(|_| VmError::InvalidMsgType)?;

//         Ok(Self {
//             path_attributes,
//             // afi_safi: value.afi_safi,
//             status: value.status,
//             nlri: BasicNlri { prefix: value.prefix, path_id: value.path_id },
//         })
//     }
// }

// impl<T: Clone + Hash + std::fmt::Debug> Display for Route<T> {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         writeln!(f, "nlri  : {:?}", self.nlri)?;
//         writeln!(f, "status  : {}", self.status)?;
//         // writeln!(f, "afi_safi: {}", self.afi_safi)?;
//         writeln!(f, "path_attributes: {:?}", self.path_attributes)
//     }
// }

// impl<T: Clone + Hash> From<SingleNlri<T>> for MaterializedRoute2 {
//     fn from(route: MutableBasicRoute) -> Self {
//         let status = route.status;
//         let route_id = (RotondaId(0), 0);

//         MaterializedRoute2 {
//             path_attributes: route.take_attrs(),
//             status,
//             route_id,
//         }
//     }
// }

//------------ Route ---------------------------------------------------------

// pub struct RouteContextBuilder<'a, N> {
//     lazy_route: LazyRecordTypeDef,
//     provenance: Option<&'a Provenance>,
//     // afi_safi: AfiSafi,
//     nlri: &'a N,
//     nlri_status: Option<NlriStatus>,
// }

// impl<'a, N: AfiSafiNlri> RouteContextBuilder<'a, N> {
//     pub fn new(afi_safi: AfiSafi, nlri: &'a N) -> Self {
//         match afi_safi {
//             AfiSafi::Ipv4Unicast | AfiSafi::Ipv6Unicast | AfiSafi::Ipv4Multicast | AfiSafi::Ipv6Multicast => {
//                 Self {
//                     lazy_route: LazyRecordTypeDef::BasicRoute,
//                     provenance: None,
//                     // afi_safi,
//                     nlri,
//                     nlri_status: None
//                 }
//             },
//             AfiSafi::Ipv4MplsUnicast => todo!(),
//             AfiSafi::Ipv6MplsUnicast => todo!(),
//             AfiSafi::Ipv4MplsVpnUnicast => todo!(),
//             AfiSafi::Ipv6MplsVpnUnicast => todo!(),
//             AfiSafi::Ipv4RouteTarget => todo!(),
//             AfiSafi::Ipv4FlowSpec => todo!(),
//             AfiSafi::Ipv6FlowSpec => todo!(),
//             AfiSafi::L2VpnVpls => todo!(),
//             AfiSafi::L2VpnEvpn => todo!(),
//         }
//     }

//     pub fn finish(&self) -> Result<RouteContext<'a, N>, VmError> {
//         RouteContext::<N>::from_builder(self, )
//     }
// }

// pub trait AfiSafiNlri: Clone + Hash + std::fmt::Debug {
//     type Nlri;
//     fn nlri(&self) -> Self::Nlri;
//     fn afi_safi() -> AfiSafi;
//     fn make_route(&self, status: NlriStatus, path_attributes: PathAttributesBuilder) -> impl RotoType;
// }

// #[derive(Clone, Debug, Hash)]
// pub struct Ipv4UnicastNlri(pub BasicNlri);
// impl AfiSafiNlri for Ipv4UnicastNlri {
//     type Nlri = BasicNlri;

//     fn nlri(&self) -> Self::Nlri { self.0 }

//     fn afi_safi() -> AfiSafi {
//         AfiSafi::Ipv4Unicast
//     }

//     fn make_route(&self, status: NlriStatus, path_attributes: PathAttributesBuilder) -> impl RotoType {
//         Route::<BasicNlri> {
//             nlri: self.0,
//             status,
//             // afi_safi: AfiSafi::Ipv4Unicast,
//             path_attributes: path_attributes.into_inner()
//         }
//     }
// }

// #[derive(Clone, Debug, Hash)]
// pub struct Ipv6UnicastNlri(pub BasicNlri);

// impl AfiSafiNlri for Ipv6UnicastNlri {
//     type Nlri = BasicNlri;

//     fn nlri(&self) -> Self::Nlri { self.0 }

//     fn afi_safi() -> AfiSafi {
//         AfiSafi::Ipv6Unicast
//     }

//     fn make_route(&self, status: NlriStatus, path_attributes: PathAttributesBuilder) -> impl RotoType {
//         Route::<BasicNlri> {
//             nlri: self.0,
//             status,
//             path_attributes: path_attributes.into_inner()
//         }
//     }
// }

// impl From<Prefix> for Ipv6UnicastNlri {
//     fn from(value: Prefix) -> Self {
//         Self(BasicNlri { prefix: value, path_id: None })
//     }
// }

// #[derive(Clone, Debug, Hash)]
// pub struct Ipv4MulticastNlri(pub BasicNlri);

// impl AfiSafiNlri for Ipv4MulticastNlri {
//     type Nlri = BasicNlri;

//     fn nlri(&self) -> Self::Nlri { self.0 }

//     fn afi_safi() -> AfiSafi {
//         AfiSafi::Ipv4Multicast
//     }

//     fn make_route(&self, status: NlriStatus, path_attributes: PathAttributesBuilder) -> impl RotoType {
//         Route::<BasicNlri> {
//             nlri: self.0,
//             status,
//             path_attributes: path_attributes.into_inner()
//         }
//     }
// }

// #[derive(Clone, Debug, Hash)]
// pub struct Ipv6MulticastNlri(pub BasicNlri);

// impl AfiSafiNlri for Ipv6MulticastNlri {
//     type Nlri = BasicNlri;

//     fn nlri(&self) -> Self::Nlri { self.0 }

//     fn afi_safi() -> AfiSafi {
//         AfiSafi::Ipv6Multicast
//     }

//     fn make_route(&self, status: NlriStatus, path_attributes: PathAttributesBuilder) -> impl RotoType {
//         Route::<BasicNlri> {
//             nlri: self.0,
//             status,
//             path_attributes: path_attributes.into_inner()
//         }
//     }
// }

// #[derive(Clone, Debug, Hash)]
// pub struct Ipv4FlowSpecNlri(pub FlowSpecNlri<bytes::Bytes>);

// impl AfiSafiNlri for Ipv4FlowSpecNlri {
//     type Nlri = Ipv4FlowSpecNlri;

//     fn nlri(&self) -> Self::Nlri { Ipv4FlowSpecNlri(self.0.clone()) }

//     fn afi_safi() -> AfiSafi {
//         AfiSafi::Ipv4FlowSpec
//     }

//     fn make_route(&self, status: NlriStatus, path_attributes: PathAttributesBuilder) -> impl RotoType {
//         Route::<FlowSpecNlri<bytes::Bytes>> {
//             nlri: self.0.clone(),
//             status,
//             path_attributes: path_attributes.into_inner()
//         }
//     }
// }

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct RouteContext {
    pub(crate) bgp_msg: Option<BytesRecord<BgpUpdateMessage>>,
    pub(crate) provenance: Provenance,
    pub(crate) nlri_status: NlriStatus,
}

impl RouteContext
{
    pub fn new(
        bgp_msg: Option<BytesRecord<BgpUpdateMessage>>,
        nlri_status: NlriStatus,
        provenance: Provenance,
    ) -> Self {
        Self {
            bgp_msg,
            nlri_status,
            provenance,
        }
    }

    // fn from_builder(builder: &RouteContextBuilder<'a, N>, bgp_msg: BytesRecord<BgpUpdateMessage>) -> Result<Self, VmError> {
    //     Ok(Self {
    //         provenance: builder.provenance.ok_or_else(|| VmError::InvalidFieldAccess)?,
    //         // afi_safi: builder.afi_safi,
    //         nlri: builder.nlri,
    //         nlri_status: builder.nlri_status.ok_or(VmError::InvalidFieldAccess)?,
    //         bgp_msg,
    //     })
    // }

    pub fn message(&self) -> &Option<BytesRecord<BgpUpdateMessage>> {
        &self.bgp_msg
    }

    pub fn provenance(&self) -> &Provenance {
        &self.provenance
    }

    pub fn nlri_status(&self) -> NlriStatus {
        self.nlri_status
    }

    // pub fn nlri(&self) -> N::Nlri {
    //     self.single_nlri.nlri()
    // }

    // pub fn afi_safi_nlri(&self) -> &N {
    //     &self.single_nlri
    // }

    // pub fn into_route<N>(self) -> Result<Route<N>, VmError> where N: Clone
    //     + std::fmt::Debug
    //     + Hash
    //     + AfiSafiNlri<bytes::Bytes, Nlri = BasicNlri> {
    //     if let Some(msg) = self.bgp_msg {
    //         let path_attributes = PaMap::from_update_pdu(&msg.into_inner())
    //             .map_err(|_| VmError::InvalidPayload)?;
    //         Ok(Route {
    //             nlri: self.single_nlri.clone(),
    //             status: self.nlri_status,
    //             // afi_safi: self.afi_safi,
    //             path_attributes: path_attributes.attributes_owned(),
    //         })
    //     } else {
    //         Err(VmError::InvalidPayload)
    //     }
    // }

    pub fn get_attrs_builder(&self) -> Result<PaMap, VmError> {
        if let Some(msg) = &self.bgp_msg {
            PaMap::from_update_pdu(&msg.clone().into_inner())
                .map_err(|_| VmError::InvalidMsgType)
        } else {
            Err(VmError::InvalidPayload)
        }
    }

    pub fn into_type(self, ty: &TypeDef) -> Result<TypeValue, CompileError>
    where
        Self: std::marker::Sized,
    {
        Err(format!(
            "Context cannot be converted to type {} (or any other type)",
            ty
        )
        .into())
    }

    pub fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        args: &'a [StackValue],
        res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    pub fn exec_consume_value_method(
        self,
        method_token: usize,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    pub fn exec_type_method(
        method_token: usize,
        args: &[StackValue],
        res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    pub fn get_props_for_method(
        ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    // pub fn workshop<N>(
    //     &self,
    // ) -> Result<RouteWorkshop<bytes::Bytes, N>, VmError> where N: Clone
    // + std::fmt::Debug
    // + Hash
    // + AfiSafiNlri<bytes::Bytes, Nlri = BasicNlri> {
    //     if let Some(msg) = &self.bgp_msg {
    //         RouteWorkshop::from_update_pdu(
    //             self.afi_safi_nlri().clone(),
    //             &msg.clone().into_inner(),
    //         )
    //         .map_err(|_| VmError::InvalidMsgType)
    //     } else {
    //         Err(VmError::InvalidPayload)
    //     }
    // }

    pub fn clone_attrs(&self) -> Result<PaMap, VmError> {
        self.get_attrs_builder().map(|b| b.clone())
    }
    // if let Ok(builder) = self.get_attrs_builder() {
    //     Ok(builder.attributes().clone())
    // } else {
    //     Err(VmError::InvalidPayload)
    // }
    // let target = bytes::BytesMut::new();

    // if let Some(msg) = &self.bgp_msg {
    //     routecore::bgp::message::update_builder::UpdateBuilder::from_update_message(
    //             &msg.clone().into_inner(),
    //             SessionConfig::modern(),
    //             target
    //         ).map(|b| b.into_attributes().clone()
    //     ).map_err(|_| VmError::InvalidMsgType)
    // } else {
    //     Err(VmError::InvalidPayload)
    // }
}

// pub fn get_field_num(&self) -> usize {
//     self.lazy_route.get_field_num()
// }

// pub fn get_props_for_field(
//     &self,
//     field: &ast::Identifier,
// ) -> Result<(TypeDef, Token), CompileError> {
//     self.lazy_route.get_props_for_field(field)
// }

// impl RotoType for RouteContext<'_>
// // where
// //     N: Clone
// //         + std::fmt::Debug
// //         + Hash
// //         + AfiSafiNlri<bytes::Bytes, Nlri = BasicNlri>,
// //     TypeValue:
// //         From<routecore::bgp::workshop::route::RouteWorkshop<bytes::Bytes, N>>,
// {
//     fn into_type(
//         self,
//         type_value: &TypeDef,
//     ) -> Result<TypeValue, CompileError>
//     where
//         Self: std::marker::Sized,
//     {
//         todo!()
//     }

//     fn exec_value_method<'a>(
//         &'a self,
//         method_token: usize,
//         args: &'a [StackValue],
//         res_type: TypeDef,
//     ) -> Result<TypeValue, VmError> {
//         todo!()
//     }

//     fn exec_consume_value_method(
//         self,
//         method_token: usize,
//         args: Vec<TypeValue>,
//         res_type: TypeDef,
//     ) -> Result<TypeValue, VmError> {
//         todo!()
//     }

//     fn exec_type_method(
//         method_token: usize,
//         args: &[StackValue],
//         res_type: TypeDef,
//     ) -> Result<TypeValue, VmError> {
//         todo!()
//     }

//     fn get_props_for_method(
//         ty: TypeDef,
//         method_name: &crate::ast::Identifier,
//     ) -> Result<MethodProps, CompileError>
//     where
//         Self: std::marker::Sized,
//     {
//         todo!()
//     }
// }

// impl From<RouteContext<'_>> for TypeValue
// where
//     N: Clone
//         + std::fmt::Debug
//         + Hash
//         + AfiSafiNlri<bytes::Bytes, Nlri = BasicNlri>,
//     TypeValue:
//         From<routecore::bgp::workshop::route::RouteWorkshop<bytes::Bytes, N>>,
// {
//     fn from(value: RouteContext<'_>) -> Self {
//         if let Some(msg) = &value.bgp_msg {
//             if let Ok(path_attributes) = RouteWorkshop::from_update_pdu(
//                 value.nlri(),
//                 &msg.clone().into_inner(),
//             ) {
//                 path_attributes.into()
//             } else {
//                 TypeValue::Unknown
//             }
//         } else {
//             TypeValue::Unknown
//         }
//     }
// }

// impl From<RouteContext<'_, Ipv6UnicastNlri>> for TypeValue {
//     fn from(value: RouteContext<'_, Ipv6UnicastNlri>) -> Self {
//         if let Some(msg) = &value.bgp_msg {
//             if let Ok(path_attributes) = RouteWorkshop::from_update_pdu(&msg.clone().into_inner()) {
//                 let route = path_attributes.make_route_with_nlri(value.single_nlri);
//                 if let Some(r) = route {
//                     r.into()
//                 } else {
//                     TypeValue::Unknown
//                 }
//             } else {
//             TypeValue::Unknown
//             }
//         } else {
//             TypeValue::Unknown
//         }
//     }
// }

//------------ BasicRoute ----------------------------------------------------

// BasicRoute is the variant of routecore's `Nlri` enum that only has a
// `BasicNlri` struct as its data field. These variants are Unicast,
// Multicast. This is mainly for convenience of the Roto user so they don't
// have to any pattern matching to get to the properties of this BasicNlri,
// like prefix and path_id. Instead, the prefix and path_id fields just live
// in the root of the BasicRoute. To the Roto user this appears as a normal
// `Route` object. Another implication of the roto user using Route as the
// type for `rx` is that the filter that has this will return a `reject` for
// all other variants. E.g., if a Nlri that unpacks as the FlowSpec variant,
// it will be rejected right off the bat.

// #[derive(Clone, Debug, PartialEq, Eq, Hash)]
// pub struct BasicRoute_old {
//     message: BytesRecord<BgpUpdateMessage>,
//     provenance: Provenance,
//     prefix: Prefix,
//     path_id: Option<PathId>,
//     afi_safi: AfiSafi,
//     status: NlriStatus,
//     // changes: Option<Box<PathAttributeSet>>,
// }

// impl BasicRoute_old {
//     pub fn new(
//         prefix: Prefix,
//         message: BytesRecord<BgpUpdateMessage>,
//         afi_safi: routecore::bgp::types::AfiSafi,
//         path_id: Option<routecore::bgp::message::nlri::PathId>,
//         status: NlriStatus,
//         provenance: Provenance,
//     ) -> Self {
//         Self {
//             prefix,
//             message,
//             afi_safi,
//             path_id,
//             // changes: None,
//             status,
//             provenance,
//         }
//     }

//     pub fn provenance(&self) -> &Provenance {
//         &self.provenance
//     }

//     pub fn prefix(&self) -> Prefix {
//         self.prefix
//     }

//     pub fn afi_safi(&self) -> AfiSafi {
//         self.afi_safi
//     }

//     pub fn message(&self) -> &BytesRecord<BgpUpdateMessage> {
//         &self.message
//     }

//     // Get either the moved last delta (it is removed from the Delta list), or
//     // get a freshly rolled ChangeSet from the raw message, if there are no
//     // deltas.
//     // pub fn take_attrs(mut self) -> Result<PathAttributeSet, VmError> {
//     //     match &mut self.changes {
//     //         None => self.message.materialize_path_attributes(self.afi_safi),
//     //         Some(attr_list) => Ok(std::mem::take(attr_list)),
//     //     }
//     // }

//     // pub fn store_attrs(&mut self, path_attribues: PathAttributeSet) {
//     //     self.changes = Some(Box::new(path_attribues))
//     // }

//     pub fn get_attrs(&self) -> Result<PaMap, VmError> {
//         let target = bytes::BytesMut::new();

//         routecore::bgp::message::update_builder::UpdateBuilder::from_update_message(
//                 self.message.bytes_parser(),
//                 SessionConfig::modern(),
//                 target
//             ).map(|b| b.attributes().clone()
//         ).map_err(|_| VmError::InvalidMsgType)

//         // Ok(PathAttributeSet {
//         //     as_path: OverlayValue::new_from_opt(
//         //         self.message
//         //             .bytes_parser()
//         //             .aspath()
//         //             .ok()
//         //             .flatten()
//         //             .map(|p| p.to_hop_path())
//         //             .ok_or(VmError::InvalidFieldAccess)
//         //             .ok(),
//         //     ),
//         //     origin_type: OverlayValue::new_from_opt(
//         //         self.message
//         //             .bytes_parser()
//         //             .origin()
//         //             .ok()
//         //             .flatten()
//         //             .ok_or(VmError::InvalidFieldAccess)
//         //             .ok(),
//         //     ),
//         //     next_hop: OverlayValue::new_from_opt(
//         //         self.message
//         //             .bytes_parser()
//         //             .find_next_hop(self.afi_safi)
//         //             // .ok()
//         //             // .flatten()
//         //             .map_err(|_| VmError::InvalidFieldAccess)
//         //             .ok(),
//         //     ),
//         //     multi_exit_discriminator: OverlayValue::new_from_opt(
//         //         self.message
//         //             .bytes_parser()
//         //             .multi_exit_disc()
//         //             .ok()
//         //             .flatten()
//         //             .map_err(|_| VmError::InvalidFieldAccess),
//         //     ),
//         //     local_pref: OverlayValue::new_from_opt(
//         //         self.message
//         //             .bytes_parser()
//         //             .local_pref()
//         //             .ok()
//         //             .flatten()
//         //             .map_err(|_| VmError::InvalidFieldAccess),
//         //     ),
//         //     atomic_aggregate: OverlayValue::new_from_opt(
//         //         self.message
//         //             .bytes_parser()
//         //             .is_atomic_aggregate()
//         //             .ok()
//         //             .flatten()
//         //             .map_err(|_| VmError::InvalidFieldAccess),
//         //     ),
//         //     aggregator: OverlayValue::new_from_opt(
//         //         self.message
//         //             .bytes_parser()
//         //             .aggregator()
//         //             .ok()
//         //             .flatten()
//         //             .map_err(|_| VmError::InvalidFieldAccess),
//         //     ),
//         //     communities: OverlayValue::new_from_opt(
//         //         self.message
//         //             .bytes_parser()
//         //             .all_human_readable_communities()
//         //             .ok()
//         //             .flatten()
//         //             .map_err(|_| VmError::InvalidFieldAccess),
//         //     ),
//         //     originator_id: crate::attr_change_set::Todo,
//         //     cluster_list: crate::attr_change_set::Todo,
//         //     extended_communities: crate::attr_change_set::Todo,
//         //     as4_path: crate::attr_change_set::Todo,
//         //     as4_aggregator: crate::attr_change_set::Todo,
//         //     connector: crate::attr_change_set::Todo,
//         //     as_path_limit: crate::attr_change_set::Todo,
//         //     pmsi_tunnel: crate::attr_change_set::Todo,
//         //     ipv6_extended_communities: crate::attr_change_set::Todo,
//         //     large_communities: crate::attr_change_set::Todo,
//         //     bgpsec_as_path: crate::attr_change_set::Todo,
//         //     attr_set: crate::attr_change_set::Todo,
//         //     rsrvd_development: crate::attr_change_set::Todo,
//         // })
//     }

//     // pub fn get_attrs_mut(
//     //     &mut self,
//     // ) -> Result<&mut Option<Box<PathAttributeSet>>, VmError> {
//     //     match &mut self.changes {
//     //         None => {
//     //             self.changes = Some(Box::new(
//     //                 self.message
//     //                     .materialize_path_attributes(self.afi_safi)?,
//     //             ));
//     //             Ok(&mut self.changes)
//     //         }
//     //         Some(_) => Ok(&mut self.changes),
//     //     }
//     // }

//     pub fn get_field_by_index(
//         &self,
//         field_index: &FieldIndex,
//     ) -> Result<TypeValue, VmError> {
//         match field_index.first()?.try_into()? {
//             BasicRouteToken::AsPath => self
//                 .message
//                 .bytes_parser()
//                 .aspath()
//                 .ok()
//                 .flatten()
//                 .map(|p| p.to_hop_path())
//                 .map(TypeValue::from)
//                 .ok_or(VmError::InvalidFieldAccess),
//             BasicRouteToken::OriginType => self
//                 .message
//                 .bytes_parser()
//                 .origin()
//                 .ok()
//                 .flatten()
//                 .map(TypeValue::from)
//                 .ok_or(VmError::InvalidFieldAccess),
//             BasicRouteToken::NextHop => self
//                 .message
//                 .bytes_parser()
//                 .find_next_hop(self.afi_safi)
//                 .map_err(|_| VmError::InvalidFieldAccess)
//                 .map(TypeValue::from),
//             BasicRouteToken::MultiExitDisc => self
//                 .message
//                 .bytes_parser()
//                 .multi_exit_disc()
//                 .ok()
//                 .flatten()
//                 .map(TypeValue::from)
//                 .ok_or(VmError::InvalidFieldAccess),
//             BasicRouteToken::LocalPref => self
//                 .message
//                 .bytes_parser()
//                 .local_pref()
//                 .ok()
//                 .flatten()
//                 .map(TypeValue::from)
//                 .ok_or(VmError::InvalidFieldAccess),
//             BasicRouteToken::AtomicAggregate => Some(TypeValue::from(
//                 self.message
//                     .bytes_parser()
//                     .is_atomic_aggregate()
//                     .unwrap_or(false),
//             ))
//             .ok_or(VmError::InvalidFieldAccess),
//             BasicRouteToken::Aggregator => self
//                 .message
//                 .bytes_parser()
//                 .aggregator()
//                 .ok()
//                 .flatten()
//                 .map(TypeValue::from)
//                 .ok_or(VmError::InvalidFieldAccess),
//             BasicRouteToken::Communities => self
//                 .message
//                 .bytes_parser()
//                 .all_human_readable_communities()
//                 .ok()
//                 .flatten()
//                 .map(TypeValue::from)
//                 .ok_or(VmError::InvalidFieldAccess),
//             BasicRouteToken::Prefix => Ok(self.prefix.into()),
//             BasicRouteToken::Status => Ok(self.status.into()),
//             BasicRouteToken::Provenance => {
//                 if !field_index.is_empty() {
//                     trace!("get field_index {:?} on provenance", field_index);
//                     self.provenance
//                         .get_field_by_index(field_index.skip_first())
//                 } else {
//                     Ok(self.provenance.into())
//                 }
//             }
//         }
//     }

//     // pub(crate) fn get_value_ref_for_field(
//     //     &self,
//     //     field_token: usize,
//     // ) -> Result<Option<&TypeValue>, VmError> {
//     //     let current_set = if let Some(atrd) = &self.changes {
//     //         atrd
//     //     } else {
//     //         return Err(VmError::InvalidRecord);
//     //     };

//     //     match field_token.try_into()? {
//     //         BasicRouteToken::AsPath => Ok(current_set.as_path.as_ref()),
//     //         BasicRouteToken::OriginType => {
//     //             Ok(current_set.origin_type.as_ref())
//     //         }
//     //         BasicRouteToken::NextHop => Ok(current_set.next_hop.as_ref()),
//     //         BasicRouteToken::MultiExitDisc => {
//     //             Ok(current_set.multi_exit_discriminator.as_ref())
//     //         }
//     //         BasicRouteToken::LocalPref => Ok(current_set.local_pref.as_ref()),
//     //         BasicRouteToken::AtomicAggregate => {
//     //             Ok(current_set.atomic_aggregate.as_ref())
//     //         }
//     //         BasicRouteToken::Aggregator => {
//     //             Ok(current_set.aggregator.as_ref())
//     //         }
//     //         BasicRouteToken::Communities => {
//     //             Ok(current_set.communities.as_ref())
//     //         }
//     //         BasicRouteToken::Prefix => Err(VmError::InvalidCommandArg),
//     //         BasicRouteToken::Status => Err(VmError::InvalidCommandArg),
//     //         BasicRouteToken::Provenance => Err(VmError::InvalidCommandArg),
//     //     }
//     // }
// }

// // impl From<BasicRoute> for MaterializedRoute2 {
// //     fn from(route: BasicRoute) -> Self {
// //         let status = route.status;
// //         let route_id = (RotondaId(0), 0);

// //         MaterializedRoute2 {
// //             path_attributes: route.get_attrs().unwrap(),
// //             status,
// //             route_id,
// //         }
// //     }
// // }

impl std::fmt::Display for BasicRoute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "prefix  : {}", self.prefix())?;
        writeln!(f, "path_id : {:?}", self.path_id())?;
        writeln!(f, "attrs    : {:?}", self.0.attributes())
    }
}

impl BasicRoute {
    pub(crate) fn get_field_num() -> usize {
        todo!()
    }
    pub(crate) fn type_def() -> RecordTypeDef {
        todo!()
    }

    pub(crate) fn get_props_for_field(
        field_name: &ast::Identifier,
    ) -> Result<(TypeDef, traits::Token), CompileError>
    where
        Self: std::marker::Sized,
    {
        match field_name.ident.as_str() {
            // The NLRI
            "prefix" => Ok((
                TypeDef::Prefix,
                Token::FieldAccess(vec![0, BasicNlriToken::Prefix.into()]),
            )),
            "path-id" => Ok((
                TypeDef::PathId,
                Token::FieldAccess(vec![0, BasicNlriToken::PathId.into()]),
            )),
            // The Path Attributes. They follow the IANA BGP Attributes type
            // codes.
            "as-path" => Ok((
                TypeDef::AsPath,
                Token::FieldAccess(vec![1, BasicRouteToken::AsPath.into()]),
            )),
            "origin-type" => Ok((
                TypeDef::OriginType,
                Token::FieldAccess(vec![1, BasicRouteToken::OriginType.into()]),
            )),
            "next-hop" => Ok((
                TypeDef::NextHop,
                Token::FieldAccess(vec![1, BasicRouteToken::NextHop.into()]),
            )),
            "multi-exit-disc" => Ok((
                TypeDef::MultiExitDisc,
                Token::FieldAccess(vec![
                    1,
                    BasicRouteToken::MultiExitDisc.into()
                ]),
            )),
            "local-pref" => Ok((
                TypeDef::LocalPref,
                Token::FieldAccess(vec![1, BasicRouteToken::LocalPref.into()]),
            )),
            "atomic-aggregate" => Ok((
                TypeDef::Bool,
                Token::FieldAccess(vec![
                    1,
                    BasicRouteToken::AtomicAggregate.into()
                ]),
            )),
            "aggregator" => Ok((
                TypeDef::AggregatorInfo,
                Token::FieldAccess(vec![1, BasicRouteToken::Aggregator.into()]),
            )),
            "communities" => Ok((
                TypeDef::List(Box::new(TypeDef::Community)),
                Token::FieldAccess(vec![1, BasicRouteToken::Communities.into()]),
            )),
            "status" => Ok((
                TypeDef::NlriStatus,
                Token::FieldAccess(vec![RouteContextToken::Status.into()]),
            )),
            "provenance" => Ok((
                TypeDef::Provenance,
                Token::FieldAccess(vec![RouteContextToken::Provenance.into()]),
            )),
            _ => Err(format!(
                "Unknown method '{}' for type Route",
                field_name.ident
            )
            .into()),
        }
    }
}

impl RotoType for BasicRoute {
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
                BasicNlriToken::Prefix.into(),
                vec![],
            )),
            "path-id" => Ok(MethodProps::new(
                TypeDef::PathId,
                BasicNlriToken::PathId.into(),
                vec![],
            )),
            "as-path" => Ok(MethodProps::new(
                TypeDef::AsPath,
                BasicRouteToken::AsPath.into(),
                vec![],
            )),
            "origin-type" => Ok(MethodProps::new(
                TypeDef::OriginType,
                BasicRouteToken::OriginType.into(),
                vec![],
            )),
            "next-hop" => Ok(MethodProps::new(
                TypeDef::NextHop,
                BasicRouteToken::NextHop.into(),
                vec![],
            )),
            "multi-exit-disc" => Ok(MethodProps::new(
                TypeDef::MultiExitDisc,
                BasicRouteToken::MultiExitDisc.into(),
                vec![],
            )),
            "local-pref" => Ok(MethodProps::new(
                TypeDef::LocalPref,
                BasicRouteToken::LocalPref.into(),
                vec![],
            )),
            "atomic-aggregate" => Ok(MethodProps::new(
                TypeDef::Bool,
                BasicRouteToken::AtomicAggregate.into(),
                vec![],
            )),
            "aggregator" => Ok(MethodProps::new(
                TypeDef::AggregatorInfo,
                BasicRouteToken::Aggregator.into(),
                vec![],
            )),
            "communities" => Ok(MethodProps::new(
                TypeDef::List(Box::new(TypeDef::Community)),
                BasicRouteToken::Communities.into(),
                vec![],
            )),
            "status" => Ok(MethodProps::new(
                TypeDef::NlriStatus,
                RouteContextToken::Status.into(),
                vec![],
            )),
            "provenance" => Ok(MethodProps::new(
                TypeDef::Provenance,
                RouteContextToken::Provenance.into(),
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
            TypeDef::Route => Ok(TypeValue::Builtin(
                BuiltinTypeValue::Route(self),
            )),
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

    fn take_value(self) -> Self {
        self
    }
}

#[derive(Copy, Clone, Debug, Deserialize, PartialEq, Eq)]
pub enum BasicNlriToken {
    Prefix = 0,
    PathId = 1,
}

#[derive(Copy, Clone, Debug, Deserialize, PartialEq, Eq)]
pub enum BasicRouteToken {
    OriginType = 1,
    AsPath = 2,
    NextHop = 3,
    MultiExitDisc = 4,
    LocalPref = 5,
    AtomicAggregate = 6,
    Aggregator = 7,
    Communities = 8,
}

#[derive(Copy, Clone, Debug, Deserialize, PartialEq, Eq)]
pub enum RouteContextToken {
    Status = 9,
    Provenance = 10,
}

impl TryFrom<usize> for BasicRouteToken {
    type Error = VmError;

    fn try_from(value: usize) -> Result<Self, VmError> {
        match value {
            1 => Ok(BasicRouteToken::OriginType),
            2 => Ok(BasicRouteToken::AsPath),
            3 => Ok(BasicRouteToken::NextHop),
            4 => Ok(BasicRouteToken::MultiExitDisc),
            5 => Ok(BasicRouteToken::LocalPref),
            6 => Ok(BasicRouteToken::AtomicAggregate),
            7 => Ok(BasicRouteToken::Aggregator),
            8 => Ok(BasicRouteToken::Communities),
            _ => {
                debug!("Unknown RouteToken value: {}", value);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<BasicRouteToken> for usize {
    fn from(val: BasicRouteToken) -> Self {
        val as usize
    }
}

impl From<BasicRouteToken> for u8 {
    fn from(val: BasicRouteToken) -> Self {
        val as u8
    }
}

impl TryFrom<usize> for BasicNlriToken {
    type Error = VmError;

    fn try_from(value: usize) -> Result<Self, VmError> {
        match value {
            0 => Ok(BasicNlriToken::Prefix),
            1 => Ok(BasicNlriToken::PathId),
            _ => {
                debug!("Unknown RouteToken value: {}", value);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<BasicNlriToken> for usize {
    fn from(val: BasicNlriToken) -> Self {
        val as usize
    }
}

impl From<BasicNlriToken> for u8 {
    fn from(val: BasicNlriToken) -> Self {
        val as u8
    }
}

impl TryFrom<usize> for RouteContextToken {
    type Error = VmError;

    fn try_from(value: usize) -> Result<Self, VmError> {
        match value {
            9 => Ok(RouteContextToken::Status),
            10 => Ok(RouteContextToken::Provenance),
            _ => {
                debug!("Unknown RouteToken value: {}", value);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<RouteContextToken> for usize {
    fn from(val: RouteContextToken) -> Self {
        val as usize
    }
}

impl From<RouteContextToken> for u8 {
    fn from(val: RouteContextToken) -> Self {
        val as u8
    }
}


//------------ FlowSpecRoute -------------------------------------------------

impl RotoType for Route<FlowSpecNlri<bytes::Bytes>> {
    fn get_props_for_method(
        ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn into_type(
        self,
        type_value: &TypeDef,
    ) -> Result<TypeValue, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        args: &'a [StackValue],
        res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        method_token: usize,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method(
        method_token: usize,
        args: &[StackValue],
        res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

impl From<Route<FlowSpecNlri<bytes::Bytes>>> for TypeValue {
    fn from(value: Route<FlowSpecNlri<bytes::Bytes>>) -> Self {
        todo!()
    }
}

//------------ Provenance ----------------------------------------------------

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct Provenance {
    #[serde(skip)]
    pub timestamp: chrono::DateTime<Utc>,
    pub router_id: u32,     // hash over the string.
    pub connection_id: u32, // hash over the SourceId of the TCP connection on this Rotonda.
    pub peer_id: PeerId,
    pub peer_bgp_id: routecore::bgp::path_attributes::BgpIdentifier,
    pub peer_distuingisher: [u8; 8],
    pub peer_rib_type: PeerRibType,
}

impl Provenance {
    pub fn from_rotonda() -> Self {
        Self {
            timestamp: Utc::now(),
            router_id: 0,
            connection_id: 0,
            peer_id: PeerId {
                addr: "127.0.0.1".parse().unwrap(),
                asn: 0.into(),
            },
            peer_bgp_id: BgpIdentifier::from([0, 0, 0, 0]),
            peer_distuingisher: [0, 0, 0, 0, 0, 0, 0, 0],
            peer_rib_type: PeerRibType::Loc,
        }
    }

    pub fn peer_ip(&self) -> std::net::IpAddr {
        self.peer_id.addr
    }

    pub(crate) fn get_props_for_field(
        field_name: &ast::Identifier,
    ) -> Result<(TypeDef, traits::Token), CompileError>
    where
        Self: std::marker::Sized,
    {
        match field_name.ident.as_str() {
            "timestamp" => Ok((
                TypeDef::U32,
                Token::FieldAccess(vec![ProvenanceToken::Timestamp.into()]),
            )),
            "router-id" => Ok((
                TypeDef::U32,
                Token::FieldAccess(vec![ProvenanceToken::RouterId.into()]),
            )),
            "connection-id" => Ok((
                TypeDef::U32,
                Token::FieldAccess(
                    vec![ProvenanceToken::ConnectionId.into()],
                ),
            )),
            "peer-id" => Ok((
                TypeDef::PeerId,
                Token::FieldAccess(vec![ProvenanceToken::PeerId.into()]),
            )),
            "peer-bgp-id" => Ok((
                TypeDef::List(Box::new(TypeDef::U32)),
                Token::FieldAccess(vec![ProvenanceToken::PeerBgpId.into()]),
            )),
            "peer-distuingisher" => Ok((
                TypeDef::List(Box::new(TypeDef::U32)),
                Token::FieldAccess(vec![
                    ProvenanceToken::PeerDistuingisher.into()
                ]),
            )),
            "peer-rib-type" => Ok((
                TypeDef::PeerRibType,
                Token::FieldAccess(vec![ProvenanceToken::PeerRibType.into()]),
            )),
            _ => Err(format!(
                "Unknown method '{}' for type Provenance",
                field_name.ident
            )
            .into()),
        }
    }

    pub fn get_field_by_index(
        &self,
        field_index: &[usize],
    ) -> Result<TypeValue, VmError> {
        trace!("get_field_by_index {:?} for Provenance", field_index);
        match field_index.first().map(|i| (*i).try_into()) {
            Some(Ok(ProvenanceToken::Timestamp)) => todo!(),
            Some(Ok(ProvenanceToken::RouterId)) => Ok(self.router_id.into()),
            Some(Ok(ProvenanceToken::ConnectionId)) => {
                Ok(self.connection_id.into())
            }
            Some(Ok(ProvenanceToken::PeerId)) => {
                match field_index.len() {
                    // You shouldn't com here with an empty field_index, the
                    // method calling this should have return the whole
                    // Provenance struct already.
                    0 => Err(VmError::InvalidFieldAccess),
                    // We want the whole PeerId struct.
                    1 => Ok(self.peer_id.into()),
                    // We want a field on PeerId. PeerId has no nested fields,
                    // so we only need one usize, indicating the field, as
                    // argument.
                    _ => self.peer_id.get_field_by_index(field_index[1]),
                }
            }
            Some(Ok(ProvenanceToken::PeerBgpId)) => todo!(),
            Some(Ok(ProvenanceToken::PeerDistuingisher)) => todo!(),
            Some(Ok(ProvenanceToken::PeerRibType)) => {
                Ok(self.peer_rib_type.into())
            }
            _ => Err(VmError::InvalidFieldAccess),
        }
    }
}

impl Display for Provenance {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "timestamp: {}, router_id: {}, connection_id: {}, peer_id: \
        {}, peer_bgp_id: {:?}, peer_distuingisher: {:?}, peer_rib_type: {}",
            self.timestamp,
            self.router_id,
            self.connection_id,
            self.peer_id,
            self.peer_bgp_id,
            self.peer_distuingisher,
            self.peer_rib_type
        )
    }
}

impl RotoType for Provenance {
    fn get_props_for_method(
        _ty: TypeDef,
        _method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        Err(CompileError::from("Type Provenance has no methods"))
    }

    intotype!(Provenance;StringLiteral);

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidCommandArg)
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidCommandArg)
    }

    fn exec_type_method(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidCommandArg)
    }
}

createtoken!(
    Provenance;
    Timestamp = 0
    RouterId = 1
    ConnectionId = 2
    PeerId = 3
    PeerBgpId = 4
    PeerDistuingisher = 5
    PeerRibType = 6
);

impl From<ProvenanceToken> for u8 {
    fn from(val: ProvenanceToken) -> Self {
        val as u8
    }
}

impl From<Provenance> for TypeValue {
    fn from(value: Provenance) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Provenance(value))
    }
}

//------------ PeerRibType ---------------------------------------------------

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Default, Serialize)]
pub enum PeerRibType {
    InPre,
    InPost,
    Loc,
    OutPre,
    #[default]
    OutPost, // This is the default for BGP messages
}

impl Display for PeerRibType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PeerRibType::InPre => write!(f, "adj-RIB-in-pre"),
            PeerRibType::InPost => write!(f, "adj-RIB-in-post"),
            PeerRibType::Loc => write!(f, "RIB-loc"),
            PeerRibType::OutPre => write!(f, "adj-RIB-out-pre"),
            PeerRibType::OutPost => write!(f, "adj-RIB-out-pre"),
        }
    }
}

minimalscalartype!(PeerRibType);

impl From<(bool, routecore::bmp::message::RibType)> for PeerRibType {
    fn from(
        (is_post_policy, rib_type): (bool, routecore::bmp::message::RibType),
    ) -> Self {
        match rib_type {
            routecore::bmp::message::RibType::AdjRibIn => {
                if is_post_policy {
                    PeerRibType::InPost
                } else {
                    PeerRibType::InPre
                }
            }
            routecore::bmp::message::RibType::AdjRibOut => {
                if is_post_policy {
                    PeerRibType::OutPost
                } else {
                    PeerRibType::OutPre
                }
            }
            routecore::bmp::message::RibType::Unimplemented(_) => {
                PeerRibType::OutPost
            }
        }
    }
}

//------------ PeerId --------------------------------------------------------

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Serialize)]
pub struct PeerId {
    pub addr: IpAddr,
    pub asn: Asn,
}

impl PeerId {
    pub fn new(addr: IpAddr, asn: Asn) -> Self {
        Self { addr, asn }
    }

    pub(crate) fn get_props_for_field(
        field_name: &ast::Identifier,
    ) -> Result<(TypeDef, traits::Token), CompileError>
    where
        Self: std::marker::Sized,
    {
        match field_name.ident.as_str() {
            "addr" => Ok((
                TypeDef::IpAddr,
                Token::FieldAccess(vec![PeerIdToken::Addr.into()]),
            )),
            "asn" => Ok((
                TypeDef::Asn,
                Token::FieldAccess(vec![PeerIdToken::Asn.into()]),
            )),
            _ => Err(format!(
                "Unknown method '{}' for type PeerId",
                field_name.ident
            )
            .into()),
        }
    }

    pub(crate) fn get_field_by_index(
        &self,
        index: usize,
    ) -> Result<TypeValue, VmError> {
        trace!("get peer_id field");
        match index.try_into()? {
            PeerIdToken::Addr => Ok(self.addr.into()),
            PeerIdToken::Asn => Ok(self.asn.into()),
        }
    }
}

impl Display for PeerId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:AS{}", self.addr, self.asn)
    }
}

createtoken!(
    PeerId;
    Addr = 0
    Asn = 1
);

impl From<PeerIdToken> for u8 {
    fn from(val: PeerIdToken) -> Self {
        val as u8
    }
}

impl From<PeerId> for TypeValue {
    fn from(value: PeerId) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::PeerId(value))
    }
}

impl RotoType for PeerId {
    fn get_props_for_method(
        ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn into_type(
        self,
        type_value: &TypeDef,
    ) -> Result<TypeValue, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        args: &'a [StackValue],
        res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        method_token: usize,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method(
        method_token: usize,
        args: &[StackValue],
        res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

// We are abusing the method_token to convey the requested RouteStatus
// from the invoked method. So instead of creating all 'is_*' methods
// separately, we can just compare the method_token with self.

impl RotoType for NlriStatus {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "is_in_convergence" => Ok(MethodProps::new(
                TypeDef::Bool,
                usize::from(NlriStatus::InConvergence),
                vec![],
            )),
            "is_up_to_date" => Ok(MethodProps::new(
                TypeDef::Bool,
                usize::from(NlriStatus::UpToDate),
                vec![],
            )),
            "is_stale" => Ok(MethodProps::new(
                TypeDef::Bool,
                usize::from(NlriStatus::Stale),
                vec![],
            )),
            "is_start_of_route_refresh" => Ok(MethodProps::new(
                TypeDef::Bool,
                usize::from(NlriStatus::StartOfRouteRefresh),
                vec![],
            )),
            "is_withdrawn" => Ok(MethodProps::new(
                TypeDef::Bool,
                usize::from(NlriStatus::Withdrawn),
                vec![],
            )),
            "is_empty" => Ok(MethodProps::new(
                TypeDef::Bool,
                usize::from(NlriStatus::Empty),
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
            TypeDef::NlriStatus => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::NlriStatus(self)))
            }
            _ => Err(format!(
                "Cannot convert type RouteStatus to type {:?}",
                type_def
            )
            .into()),
        }
    }

    // The abuse happens here.
    fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        _args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Ok(TypeValue::Builtin(BuiltinTypeValue::Bool(
            method_token == usize::from(*self),
        )))
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

impl From<NlriStatus> for usize {
    fn from(val: NlriStatus) -> Self {
        val as usize
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Serialize)]
pub struct RotondaId(pub usize);

impl std::fmt::Display for RotondaId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let w = write!(f, "{}", self.0);
        w
    }
}

// Maybe this should go into routecore?!? or at least rotonda-fsm. It was
// lifted from rotonda, but since we need it both there and here, this is the
// place. I don't want circular deps.

//------------ SourceId ------------------------------------------------------

// The source of received updates.
//
// Not all incoming data has to come from a TCP/IP connection. This enum
// exists to represent both the TCP/IP type of incoming connection that we
// receive data from today as well as other connection types in future, and
// can also be used to represent alternate sources of incoming data such as
// replay from file or test data created on the fly.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SourceId {
    SocketAddr(SocketAddr),
    Named(std::sync::Arc<String>),
}

impl Default for SourceId {
    fn default() -> Self {
        "unknown".into()
    }
}

impl SourceId {
    pub fn generated() -> Self {
        Self::from("generated")
    }

    pub fn socket_addr(&self) -> Option<&SocketAddr> {
        match self {
            SourceId::SocketAddr(addr) => Some(addr),
            SourceId::Named(_) => None,
        }
    }

    pub fn ip(&self) -> Option<IpAddr> {
        match self {
            SourceId::SocketAddr(addr) => Some(addr.ip()),
            SourceId::Named(_) => None,
        }
    }

    pub fn name(&self) -> Option<&str> {
        match self {
            SourceId::SocketAddr(_) => None,
            SourceId::Named(name) => Some(name),
        }
    }
}

impl std::fmt::Display for SourceId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SourceId::SocketAddr(addr) => addr.fmt(f),
            SourceId::Named(name) => name.fmt(f),
        }
    }
}

impl From<SocketAddr> for SourceId {
    fn from(addr: SocketAddr) -> Self {
        SourceId::SocketAddr(addr)
    }
}

impl From<String> for SourceId {
    fn from(name: String) -> Self {
        SourceId::Named(name.into())
    }
}

impl From<&str> for SourceId {
    fn from(name: &str) -> Self {
        SourceId::Named(name.to_string().into())
    }
}
