use log::{error, trace};
use paste::paste;

use routecore::
    bgp::{
        aspath::AsPath,
        communities::HumanReadableCommunity,
        message::{update::LocalPref, SessionConfig},
        path_attributes::AggregatorInfo,
        types::{AfiSafi, MultiExitDisc, NextHop, Origin, OriginType},
    };

use crate::{
    ast::ShortString,
    attr_change_set::{PathAttributeSet, Todo},
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
    vm::{StackValue, VmError},
};

pub use crate::types::collections::BytesRecord;

use super::Nlri;

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
        field("origin"; 9, Origin, origin),
    )],
    10
);

impl BytesRecord<BgpUpdateMessage> {
    pub fn new(
        bytes: bytes::Bytes,
        session_config: SessionConfig,
    ) -> Result<Self, VmError> {
        BgpUpdateMessage::from_octets(
            bytes,
            session_config,
        )
        .map(|msg| msg.into())
        .map_err(|_| VmError::InvalidPayload)
    }

    pub fn exec_value_method(method_token: usize,
        args: &[StackValue],
        res_type: TypeDef,
        record: &BytesRecord<BgpUpdateMessage>) -> Result<TypeValue, VmError> {
            todo!()
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

impl From<Result<Vec<Nlri>, routecore::bgp::ParseError>>
    for List
{
    fn from(
        value: Result<Vec<Nlri>, routecore::bgp::ParseError>,
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
            .map(|v| TypeValue::from(Origin::from(v)))
            .unwrap_or(TypeValue::Unknown)
    }
}

impl From<OriginType> for TypeValue {
    fn from(value: OriginType) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Origin(Origin::from(value)))
    }
}

impl BytesRecord<BgpUpdateMessage> {
    // Materialize a ChangeSet from the Update message. The materialized
    // Change set is completely self-contained (no references of any kind) &
    // holds all the attributes of the current BGP Update message.
    pub fn materialize_path_attributes(
        &self,
        afi_safi: AfiSafi,
    ) -> Result<PathAttributeSet, VmError> {
        let next_hop = self
            .bytes_parser()
            .find_next_hop(afi_safi)
            .ok();
        Ok(PathAttributeSet {
            as_path: 
                self.bytes_parser()
                    .aspath()
                    .ok()
                    .flatten()
                    .map(|p| p.to_hop_path()).into(),
            origin_type: 
                self.bytes_parser().origin().ok().flatten().into(),
            next_hop: next_hop.into(),
            multi_exit_discriminator: 
                self.bytes_parser()
                    .multi_exit_disc()
                    .ok().flatten().into(),
            local_pref: self.bytes_parser().local_pref().ok().flatten().into(),
            atomic_aggregate: self.bytes_parser()
                    .is_atomic_aggregate().ok().into(),
            aggregator: self.bytes_parser().aggregator().ok().flatten().into(),
            communities: self.bytes_parser()
                    .all_human_readable_communities().ok().flatten().into(),
            originator_id: Todo,
            cluster_list: Todo,
            extended_communities: Todo,
            // value: self
            //     .ext_communities()
            //     .map(|c| c.collect::<Vec<ExtendedCommunity>>()),
            as4_path: self.bytes_parser()
                    .as4path()
                    .ok()
                    .flatten()
                    .map(|p| p.to_hop_path()).into(),
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
