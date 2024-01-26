use std::{net::IpAddr, sync::Arc};

use log::{error, trace};
use paste::paste;
use routecore::{
    addr::Prefix,
    asn::Asn,
    bgp::{
        aspath::{AsPath, HopPath},
        communities::HumanReadableCommunity,
        message::nlri::PathId,
        message::{nlri::Nlri, update::LocalPref, SessionConfig},
        path_attributes::AggregatorInfo,
        types::MultiExitDisc,
        types::{AfiSafi, NextHop, OriginType},
    },
};

use crate::{
    ast::ShortString,
    attr_change_set::{
        AttrChangeSet, AttrChangeSet2, ReadOnlyScalarOption, ScalarOption,
        Todo, VectorOption,
    },
    bytes_record_impl,
    compiler::compile::CompileError,
    createtoken, lazy_list_field, lazyelmtypevalue, lazyfield,
    traits::Token,
    types::collections::List,
    types::{
        builtin::BuiltinTypeValue,
        collections::{
            ElementTypeValue, LazyElementTypeValue,
            RecordType,
        },
        lazyrecord_types::BgpUpdateMessage,
        typedef::{LazyNamedTypeDef, RecordTypeDef, TypeDef},
        typevalue::TypeValue,
    },
    vm::VmError,
};

pub use crate::types::collections::BytesRecord;

use super::StringLiteral;

// 0 aggregator_type
// 1 announcements
// 2 as_path
// 3 as4_path
// 4 atomic_aggregate
// 5 communities
// 6 local_pref
// 7 multi_exit_discriminator
// 8 next_hop
// 9 origin_type

bytes_record_impl!(
    BgpUpdateMessage, // The Type alias for the wrapped routecore message
    BgpUpdateMessage, // The variant of TypeValue
    #[type_def(
        field(
            "aggregator_type"; 0,
            AggregatorInfo,
            aggregator
        ),
        list_field("announcements"; 1, Nlri, Vec<Nlri>, announcements_vec),
        field(
            "as_path"; 2,
            AsPath,
            aspath
        ),
        field(
            "as4_path"; 3,
            AsPath,
            as4path
        ),
        field("atomic_aggregate"; 4, AtomicAggregate, is_atomic_aggregate),
        list_field("communities"; 5, Community, Vec<Community>, all_human_readable_communities),
        field("local_pref"; 6, LocalPref, local_pref),
        field("multi_exit_discriminator"; 7, MultiExitDisc, multi_exit_disc),
        method_field("next_hop"; 8, NextHop, find_next_hop(self.afi_safi)),
        field("origin_type"; 9, OriginType, origin),
    )],
    10
);

impl BytesRecord<routecore::bgp::message::UpdateMessage<bytes::Bytes>> {
    pub fn new(
        bytes: bytes::Bytes,
        session_config: SessionConfig,
    ) -> Result<Self, VmError> {
        routecore::bgp::message::UpdateMessage::<bytes::Bytes>::from_octets(
            bytes,
            session_config,
        )
        .map(|msg| msg.into())
        .map_err(|_| VmError::InvalidPayload)
    }
}

impl From<BytesRecord<BgpUpdateMessage>> for TypeValue {
    fn from(value: BytesRecord<BgpUpdateMessage>) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::BgpUpdateMessage(value))
    }
}

impl From<Result<Option<AggregatorInfo>, routecore::bgp::ParseError>>
    for TypeValue
{
    fn from(
        value: Result<Option<AggregatorInfo>, routecore::bgp::ParseError>,
    ) -> Self {
        if let Ok(Some(aggr_info)) = value {
            TypeValue::Builtin(BuiltinTypeValue::AggregatorInfo(aggr_info))
        } else {
            TypeValue::Unknown
        }
    }
}

impl From<Result<Option<AsPath<bytes::Bytes>>, routecore::bgp::ParseError>>
    for TypeValue
{
    fn from(
        value: Result<
            Option<AsPath<bytes::Bytes>>,
            routecore::bgp::ParseError,
        >,
    ) -> Self {
        value
            .ok()
            .flatten()
            .map(|p| {
                TypeValue::Builtin(BuiltinTypeValue::AsPath(p.to_hop_path()))
            })
            .unwrap_or(TypeValue::Unknown)
    }
}

impl From<Result<bool, routecore::bgp::ParseError>> for TypeValue {
    fn from(value: Result<bool, routecore::bgp::ParseError>) -> Self {
        if let Ok(value) = value {
            TypeValue::Builtin(BuiltinTypeValue::Bool(value))
        } else {
            TypeValue::Unknown
        }
    }
}

impl
    From<
        Result<
            Option<Vec<HumanReadableCommunity>>,
            routecore::bgp::ParseError,
        >,
    > for TypeValue
{
    fn from(
        value: Result<
            Option<Vec<HumanReadableCommunity>>,
            routecore::bgp::ParseError,
        >,
    ) -> Self {
        value
            .ok()
            .flatten()
            .map(TypeValue::from)
            .unwrap_or(TypeValue::Unknown)
    }
}

impl
    From<
        Result<
            std::option::Option<Vec<HumanReadableCommunity>>,
            routecore::bgp::ParseError,
        >,
    > for List
{
    fn from(
        value: Result<
            std::option::Option<Vec<HumanReadableCommunity>>,
            routecore::bgp::ParseError,
        >,
    ) -> Self {
        List(
            value
                .ok()
                .flatten()
                .map(|opt| {
                    opt.into_iter()
                        .map(|c| {
                            ElementTypeValue::Primitive(TypeValue::Builtin(
                                BuiltinTypeValue::Community(c),
                            ))
                        })
                        .collect::<Vec<_>>()
                })
                .unwrap_or(vec![ElementTypeValue::Primitive(
                    TypeValue::Unknown,
                )]),
        )
    }
}

impl From<Result<Vec<Nlri<bytes::Bytes>>, routecore::bgp::ParseError>>
    for List
{
    fn from(
        value: Result<Vec<Nlri<bytes::Bytes>>, routecore::bgp::ParseError>,
    ) -> Self {
        List(
            value
                .ok()
                .map(|opt| {
                    opt.into_iter()
                        .map(|n| {
                            ElementTypeValue::Primitive(TypeValue::Builtin(
                                BuiltinTypeValue::Nlri(n),
                            ))
                        })
                        .collect::<Vec<_>>()
                })
                .unwrap_or(vec![ElementTypeValue::Primitive(
                    TypeValue::Unknown,
                )]),
        )
    }
}

impl From<Result<Option<LocalPref>, routecore::bgp::ParseError>>
    for TypeValue
{
    fn from(
        value: Result<Option<LocalPref>, routecore::bgp::ParseError>,
    ) -> Self {
        value
            .ok()
            .flatten()
            .map(TypeValue::from)
            .unwrap_or(TypeValue::Unknown)
    }
}

impl From<Result<Option<MultiExitDisc>, routecore::bgp::ParseError>>
    for TypeValue
{
    fn from(
        value: Result<Option<MultiExitDisc>, routecore::bgp::ParseError>,
    ) -> Self {
        value
            .ok()
            .flatten()
            .map(TypeValue::from)
            .unwrap_or(TypeValue::Unknown)
    }
}

impl From<Result<NextHop, routecore::bgp::ParseError>> for TypeValue {
    fn from(value: Result<NextHop, routecore::bgp::ParseError>) -> Self {
        value
            .ok()
            .map(TypeValue::from)
            .unwrap_or(TypeValue::Unknown)
    }
}

impl From<Result<Option<OriginType>, routecore::bgp::ParseError>>
    for TypeValue
{
    fn from(
        value: Result<Option<OriginType>, routecore::bgp::ParseError>,
    ) -> Self {
        value
            .ok()
            .flatten()
            .map(TypeValue::from)
            .unwrap_or(TypeValue::Unknown)
    }
}

impl BytesRecord<routecore::bgp::message::UpdateMessage<bytes::Bytes>> {
    // Materialize a ChangeSet from the Update message. The materialized
    // Change set is completely self-contained (no references of any kind) &
    // holds all the attributes of the current BGP Update message.
    pub fn create_changeset(
        &self,
        prefix: Prefix,
        peer_ip: Option<IpAddr>,
        peer_asn: Option<Asn>,
        router_id: Option<Arc<String>>,
        afi_safi: AfiSafi,
        path_id: Option<PathId>,
    ) -> Result<AttrChangeSet, VmError> {
        let next_hop = self
            .bytes_parser()
            .mp_next_hop()
            .ok()
            .flatten()
            .or_else(|| {
                self.bytes_parser().conventional_next_hop().ok().flatten()
            });
        Ok(AttrChangeSet {
            prefix: ReadOnlyScalarOption::<Prefix>::new(prefix.into()),
            as_path: VectorOption::<HopPath>::from(
                self.bytes_parser()
                    .aspath()
                    .ok()
                    .flatten()
                    .map(|p| p.to_hop_path()),
            ),
            origin_type: ScalarOption::<OriginType>::from(
                self.bytes_parser().origin().map_err(VmError::from)?,
            ),
            next_hop: ScalarOption::<NextHop>::from(next_hop),
            multi_exit_discriminator: ScalarOption::from(
                self.bytes_parser()
                    .multi_exit_disc()
                    .map_err(VmError::from)?,
            ),
            local_pref: ScalarOption::from(
                self.bytes_parser().local_pref().map_err(VmError::from)?,
            ),
            atomic_aggregate: ScalarOption::from(Some(
                self.bytes_parser()
                    .is_atomic_aggregate()
                    .map_err(VmError::from)?,
            )),
            aggregator: ScalarOption::from(
                self.bytes_parser().aggregator().map_err(VmError::from)?,
            ),
            communities: VectorOption::from(
                self.bytes_parser()
                    .all_human_readable_communities()
                    .map_err(VmError::from)?,
            ),
            peer_ip: peer_ip.into(),
            peer_asn: peer_asn.into(),
            router_id: router_id
                .map(|v| StringLiteral(String::clone(&v)))
                .into(),
            afi_safi: ReadOnlyScalarOption::<AfiSafi>::new(afi_safi.into()),
            path_id: path_id.into(),
            originator_id: Todo,
            cluster_list: Todo,
            extended_communities: Todo,
            // value: self
            //     .ext_communities()
            //     .map(|c| c.collect::<Vec<ExtendedCommunity>>()),
            as4_path: VectorOption::from(
                self.bytes_parser()
                    .as4path()
                    .ok()
                    .flatten()
                    .map(|p| p.to_hop_path()),
            ),
            connector: Todo,
            as_path_limit: Todo,
            pmsi_tunnel: Todo,
            ipv6_extended_communities: Todo,
            large_communities: Todo,
            // large_communities: VectorOption::from(self.0
            //     .large_communities()
            //     .map(|c| c.collect::<Vec<LargeCommunity>>())),
            bgpsec_as_path: Todo,
            attr_set: Todo,
            rsrvd_development: Todo,
            as4_aggregator: Todo,
        })
    }

    pub fn create_changeset2(
        &self,
        // prefix: Prefix,
        // peer_ip: Option<IpAddr>,
        // peer_asn: Option<Asn>,
        // router_id: Option<Arc<String>>,
        afi_safi: AfiSafi,
        // path_id: Option<PathId>,
    ) -> Result<AttrChangeSet2, VmError> {
        let next_hop = self
            .bytes_parser()
            .find_next_hop(afi_safi)
            // .mp_next_hop()
            .ok();
        // .flatten()
        // .or_else(|| self.0.conventional_next_hop().ok().flatten());
        Ok(AttrChangeSet2 {
            as_path: VectorOption::<HopPath>::from(
                self.bytes_parser()
                    .aspath()
                    .ok()
                    .flatten()
                    .map(|p| p.to_hop_path()),
            ),
            origin_type: ScalarOption::<OriginType>::from(
                self.bytes_parser().origin().map_err(VmError::from)?,
            ),
            next_hop: ScalarOption::<NextHop>::from(next_hop),
            multi_exit_discriminator: ScalarOption::from(
                self.bytes_parser()
                    .multi_exit_disc()
                    .map_err(VmError::from)?,
            ),
            local_pref: ScalarOption::from(
                self.bytes_parser().local_pref().map_err(VmError::from)?,
            ),
            atomic_aggregate: ScalarOption::from(Some(
                self.bytes_parser()
                    .is_atomic_aggregate()
                    .map_err(VmError::from)?,
            )),
            aggregator: ScalarOption::from(
                self.bytes_parser().aggregator().map_err(VmError::from)?,
            ),
            communities: VectorOption::from(
                self.bytes_parser()
                    .all_human_readable_communities()
                    .map_err(VmError::from)?,
            ),
            originator_id: Todo,
            cluster_list: Todo,
            extended_communities: Todo,
            // value: self
            //     .ext_communities()
            //     .map(|c| c.collect::<Vec<ExtendedCommunity>>()),
            as4_path: VectorOption::from(
                self.bytes_parser()
                    .as4path()
                    .ok()
                    .flatten()
                    .map(|p| p.to_hop_path()),
            ),
            connector: Todo,
            as_path_limit: Todo,
            pmsi_tunnel: Todo,
            ipv6_extended_communities: Todo,
            large_communities: Todo,
            // large_communities: VectorOption::from(self.0
            //     .large_communities()
            //     .map(|c| c.collect::<Vec<LargeCommunity>>())),
            bgpsec_as_path: Todo,
            attr_set: Todo,
            rsrvd_development: Todo,
            as4_aggregator: Todo,
        })
    }
}
