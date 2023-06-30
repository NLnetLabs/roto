use std::sync::Arc;

use routecore::bmp::message::{
    Message as RouteCoreBmpMessage, PeerDownNotification, PeerType,
};
use serde::Serialize;
use smallvec::SmallVec;

use crate::{
    compile::CompileError,
    traits::{RotoType, Token},
    types::{
        builtin::Boolean,
        collections::{LazyElementTypeValue, LazyRecord},
        constant_enum::EnumVariant,
        typedef::{LazyNamedTypeDef, MethodProps, TypeDef},
        typevalue::TypeValue,
    },
    vm::{StackValue, VmError},
};

use super::{BgpUpdateMessage, BuiltinTypeValue, LogicalTime, RotondaId};

type Message = RouteCoreBmpMessage<bytes::Bytes>;

//------------ BmpUpdateMessage ------------------------------------------------

// A data-structure that stores the array of bytes of the incoming BMP message,
// together with its logical timestamp and an ID of the instance
// and/or unit that received it originally.

// pub enum Message<Octets: AsRef<[u8]>> {
//     RouteMonitoring(RouteMonitoring<Octets>),
//     StatisticsReport(StatisticsReport<Octets>),
//     PeerDownNotification(PeerDownNotification<Octets>),
//     PeerUpNotification(PeerUpNotification<Octets>),
//     InitiationMessage(InitiationMessage<Octets>),
//     TerminationMessage(TerminationMessage<Octets>),
//     RouteMirroring(RouteMirroring<Octets>),
// }
#[derive(Debug, Serialize)]
pub struct BmpMessage {
    message_id: (RotondaId, LogicalTime),
    raw_message: Message,
}

impl std::hash::Hash for BmpMessage {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.message_id.hash(state);
    }
}

impl BmpMessage {
    pub fn new(message_id: (RotondaId, u64), raw_message: Message) -> Self {
        Self {
            message_id,
            raw_message,
        }
    }

    pub fn message_id(&self) -> (RotondaId, u64) {
        self.message_id
    }

    pub fn raw_message(&self) -> &RouteCoreBmpMessage<bytes::Bytes> {
        &self.raw_message
    }
}

impl PartialEq for BmpMessage {
    fn eq(&self, other: &Self) -> bool {
        self.message_id == other.message_id
    }
}

// impl std::hash::Hash for BmpMessage {
//     fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
//         self.raw_message.hash(state);
//     }
// }

impl Eq for BmpMessage {}

impl BmpMessage {
    pub(crate) fn get_props_for_field(
        field_name: &crate::ast::Identifier,
    ) -> Result<(TypeDef, crate::traits::Token), CompileError>
    where
        Self: std::marker::Sized,
    {
        match field_name.ident.as_str() {
            "route_monitoring" => Ok((
                TypeDef::Record(vec![(
                    "per_peer_header".into(),
                    TypeDef::Record(vec![
                        (
                            "peer_type".into(),
                            TypeDef::ConstEnumVariant("BMP_PEER_TYPE".into())
                                .into(),
                        ),
                        ("is_ipv4".into(), TypeDef::Boolean.into()),
                        ("is_ipv6".into(), TypeDef::Boolean.into()),
                        ("is_pre_policy".into(), TypeDef::Boolean.into()),
                        ("is_post_policy".into(), TypeDef::Boolean.into()),
                        ("is_legacy_format".into(), TypeDef::Boolean.into()),
                        (
                            "adj_rib_type".into(),
                            TypeDef::ConstEnumVariant(
                                "BMP_ADJ_RIB_TYPE".into(),
                            )
                            .into(),
                        ),
                        ("distinguisher".into(), TypeDef::HexLiteral.into()),
                        ("address".into(), TypeDef::IpAddress.into()),
                        ("asn".into(), TypeDef::Asn.into()),
                        ("bgp_id".into(), TypeDef::HexLiteral.into()),
                        ("ts_seconds".into(), TypeDef::U32.into()),
                    ])
                    .into(),
                )]),
                Token::FieldAccess(vec![usize::from(
                    BmpMessageToken::RouteMonitoringPerPeerHeaderPeerType,
                ) as u8]),
            )),
            // "statistics_report" => Ok((
            //     TypeDef::StatisticsReport,
            //     Token::FieldAccess(vec![
            //         BmpMessageToken::StatisticsReport.into() as u8
            //     ]),
            // )),
            _ => Err(format!(
                "Unknown field '{}' for type BgpUpdateMessage",
                field_name.ident
            )
            .into()),
        }
    }

    pub(crate) fn create_lazy_type_def(
        &self,
    ) -> Vec<crate::types::typedef::LazyNamedTypeDef<'_>> {
        vec![(
            "route_monitoring".into(),
            TypeDef::Record(vec![(
                "per_peer_header".into(),
                TypeDef::Record(vec![
                    (
                        "peer_type".into(),
                        TypeDef::ConstEnumVariant("BMP_PEER_TYPE".into())
                            .into(),
                    ),
                    ("is_ipv4".into(), TypeDef::Boolean.into()),
                    ("is_ipv6".into(), TypeDef::Boolean.into()),
                    ("is_pre_policy".into(), TypeDef::Boolean.into()),
                    ("is_post_policy".into(), TypeDef::Boolean.into()),
                    ("is_legacy_format".into(), TypeDef::Boolean.into()),
                    (
                        "adj_rib_type".into(),
                        TypeDef::ConstEnumVariant("BMP_ADJ_RIB_TYPE".into())
                            .into(),
                    ),
                    ("distinguisher".into(), TypeDef::HexLiteral.into()),
                    ("address".into(), TypeDef::IpAddress.into()),
                    ("asn".into(), TypeDef::Asn.into()),
                    ("bgp_id".into(), TypeDef::HexLiteral.into()),
                    ("ts_seconds".into(), TypeDef::U32.into()),
                ])
                .into(),
            )])
            .into(),
            Box::new(|| {
                match &self.raw_message {
                    RouteCoreBmpMessage::RouteMonitoring(rm_msg) => {
                        LazyElementTypeValue::LazyRecord(
                            LazyRecord::new(
                                vec![("route_monitoring".into(), 
                                LazyElementTypeValue::LazyRecord(
                                    LazyRecord::new(vec![("per_peer_header".into(), 
                                    LazyElementTypeValue::LazyRecord(LazyRecord::new(
                                        vec![
                                            (
                                                "peer_type".into(),
                                                LazyElementTypeValue::Lazy(
                                                    Box::new(|| { TypeValue::Builtin(
                                                        BuiltinTypeValue::ConstU8EnumVariant(
                                                            EnumVariant::<u8> {
                                                                enum_name: "BMP_PEER_TYPE"
                                                                    .into(),
                                                                value: rm_msg
                                                                    .per_peer_header()
                                                                    .peer_type()
                                                                    .into(),
                                                            },
                                                        )
                                                    ).into()}))
                                            ),
                                            (
                                                "is_ipv4".into(),
                                                LazyElementTypeValue::Lazy(Box::new(|| {
                                                    TypeValue::Builtin(
                                                        BuiltinTypeValue::from(
                                                            Boolean::new(
                                                                rm_msg.per_peer_header().is_ipv4()
                                                            )
                                                        )
                                                    ).into()
                                                }))
                                            ),
                                            (
                                                "is_ipv6".into(),
                                                LazyElementTypeValue::Lazy(Box::new(|| {
                                                    TypeValue::Builtin(
                                                        BuiltinTypeValue::from(
                                                            Boolean::new(
                                                                rm_msg.per_peer_header().is_ipv6()
                                                            )
                                                        )
                                                    ).into()
                                                }))
                                            ),
                                            (
                                                "is_pre_policy".into(),
                                                LazyElementTypeValue::Lazy(Box::new(|| {
                                                    TypeValue::Builtin(
                                                        BuiltinTypeValue::from(
                                                            Boolean::new(
                                                                rm_msg.per_peer_header().is_pre_policy()
                                                            )
                                                        )
                                                    ).into()
                                                }))
                                            ),
                                            (
                                                "is_post_policy".into(),
                                                LazyElementTypeValue::Lazy(Box::new(|| {
                                                    TypeValue::Builtin(
                                                        BuiltinTypeValue::from(
                                                            Boolean::new(
                                                                rm_msg.per_peer_header().is_post_policy()
                                                            )
                                                        )
                                                    ).into()
                                                }))
                                            ),
                                            (
                                                "is_legacy_format".into(),
                                                LazyElementTypeValue::Lazy(Box::new(|| {
                                                    TypeValue::Builtin(
                                                        BuiltinTypeValue::from(
                                                            Boolean::new(
                                                                rm_msg.per_peer_header().is_legacy_format()
                                                            )
                                                        )
                                                    ).into()
                                                }))
                                            ),
                                            (
                                                "adj_rib_type".into(),
                                                LazyElementTypeValue::Lazy(
                                                    Box::new(|| { TypeValue::Builtin(
                                                        BuiltinTypeValue::ConstU8EnumVariant(
                                                            EnumVariant::<u8> {
                                                                enum_name: "BMP_ADJ_RIB_TYPE"
                                                                    .into(),
                                                                value: rm_msg
                                                                    .per_peer_header()
                                                                    .adj_rib_type()
                                                                    .into(),
                                                            },
                                                        )
                                                    ).into()}))
                                            )
                                        ]).unwrap())
                                    )]).unwrap()
                                )
                            )]).unwrap()
                        )
                    },
                    RouteCoreBmpMessage::StatisticsReport(_) => todo!(),
                    RouteCoreBmpMessage::PeerDownNotification(_) => todo!(),
                    RouteCoreBmpMessage::PeerUpNotification(_) => todo!(),
                    RouteCoreBmpMessage::InitiationMessage(_) => todo!(),
                    RouteCoreBmpMessage::TerminationMessage(_) => todo!(),
                    RouteCoreBmpMessage::RouteMirroring(_) => todo!(),
                }
            }),
        )]
    }

    pub(crate) fn get_value_owned_for_field_index(
        &self,
        field_index: SmallVec<[usize; 8]>,
    ) -> Option<LazyRecord> {
        let mut field_iter = field_index.into_iter();
        if let Some(field_token) = field_iter.next() {
            match field_token.into() {
                BmpMessageToken::RouteMonitoring => {
                    if let RouteCoreBmpMessage::RouteMonitoring(rm_msg) =
                        &self.raw_message
                    {
                        match field_iter.next().unwrap().into() {
                            BmpMessageToken::RouteMonitoringPerPeerHeader => {
                                match field_iter.next().unwrap().into() {
                                        BmpMessageToken::RouteMonitoringPerPeerHeaderPeerType => {
                                            return Some(
                                                LazyRecord::new(
                                                    vec![(
                                                    "per_peer_header.peer_type".into(),
                                                    LazyElementTypeValue::Lazy(Box::new(|| {
                                                            TypeValue::Builtin(
                                                                BuiltinTypeValue::ConstU8EnumVariant(
                                                                    EnumVariant::<u8> {
                                                                        enum_name: "BMP_PEER_TYPE"
                                                                            .into(),
                                                                        value: rm_msg
                                                                            .per_peer_header()
                                                                            .peer_type()
                                                                            .into(),
                                                                    },
                                                                )
                                                            )
                                                            .into()
                                                    })),
                                                )])
                                                .unwrap(),
                                            );
                                        },
                                        _ => None
                                }
                            },
                            _ => None
                        }
                    } else {
                        None
                    }
                }
                BmpMessageToken::GetType => todo!(),
                BmpMessageToken::StatisticsReport => todo!(),
                BmpMessageToken::PeerDownNotification => todo!(),
                BmpMessageToken::PeerUpNotification => todo!(),
                BmpMessageToken::InitiationMessage => todo!(),
                BmpMessageToken::TerminationMessage => todo!(),
                BmpMessageToken::RouteMirroring => todo!(),
                _ => None, //     // BgpUpdateMessageToken::Afi => Some(TypeValue::Builtin(
                           //     //     BuiltinTypeValue::ConstU16EnumVariant(EnumVariant {
                           //     //         enum_name: "AFI".into(),
                           //     //         value: self.raw_message.0.nlris().afi().into(),
                           //     //     }),
                           //     // )),
                           //     // BgpUpdateMessageToken::Safi => Some(TypeValue::Builtin(
                           //     //     BuiltinTypeValue::ConstU8EnumVariant(EnumVariant {
                           //     //         enum_name: "SAFI".into(),
                           //     //         value: self.raw_message.0.nlris().safi().into(),
                           //     //     }),
                           //
            }
        } else {
            None
        }
    }
}

impl RotoType for BmpMessage {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "get_type" => Ok(MethodProps::new(
                TypeDef::ConstEnumVariant("BMP_MESSAGE_TYPES".into()),
                BmpMessageToken::GetType.into(),
                vec![],
            )),

            // "cmp" => Ok(MethodProps::new(
            //     TypeDef::IntegerLiteral,
            //     StringLiteralToken::Cmp.into(),
            //     vec![TypeDef::StringLiteral, TypeDef::StringLiteral],
            // )),
            _ => Err(format!(
                "Unknown method: '{}' for type BgpUpdateMessage",
                method_name.ident
            )
            .into()),
        }
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
        _method_token: usize,
        _args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        // match method_token.into() {
        //     // BmpMessageToken::GetType => Ok(TypeValue::Builtin(
        //     //     BuiltinTypeValue::ConstU16EnumVariant(EnumVariant {
        //     //         enum_name: "BMP_MESSAGE_TYPES".into(),
        //     //         value: self.raw_message.0.nlris().afi().into(),
        //     //     }),
        //     // )),
        //     // BmpMessageToken::Safi => Ok(TypeValue::Builtin(
        //     //     BuiltinTypeValue::ConstU8EnumVariant(EnumVariant {
        //     //         enum_name: "SAFI".into(),
        //     //         value: self.raw_message.0.nlris().safi().into(),
        //     //     }),
        //     // )),
        //     _ => Err(VmError::InvalidMethodCall),
        // }
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidMethodCall)
    }

    fn exec_type_method(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidMethodCall)
    }
}

impl From<BmpMessage> for TypeValue {
    fn from(raw: BmpMessage) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::BmpMessage(Arc::new(raw)))
    }
}

#[derive(Debug)]
enum BmpMessageToken {
    GetType = 0,
    RouteMonitoring = 1,
    RouteMonitoringPerPeerHeader = 2,
    RouteMonitoringPerPeerHeaderPeerType = 3,
    StatisticsReport = 4,
    PeerDownNotification = 5,
    PeerUpNotification = 6,
    InitiationMessage = 7,
    TerminationMessage = 8,
    RouteMirroring = 9,
}

impl From<usize> for BmpMessageToken {
    fn from(val: usize) -> Self {
        match val {
            0 => BmpMessageToken::GetType,
            1 => BmpMessageToken::RouteMonitoring,
            2 => BmpMessageToken::RouteMonitoringPerPeerHeader,
            3 => BmpMessageToken::RouteMonitoringPerPeerHeaderPeerType,
            4 => BmpMessageToken::StatisticsReport,
            5 => BmpMessageToken::PeerDownNotification,
            6 => BmpMessageToken::PeerUpNotification,
            7 => BmpMessageToken::InitiationMessage,
            8 => BmpMessageToken::TerminationMessage,
            9 => BmpMessageToken::RouteMirroring,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<BmpMessageToken> for usize {
    fn from(val: BmpMessageToken) -> Self {
        match val {
            BmpMessageToken::GetType => 0,
            BmpMessageToken::RouteMonitoring => 1,
            BmpMessageToken::RouteMonitoringPerPeerHeader => 2,
            BmpMessageToken::RouteMonitoringPerPeerHeaderPeerType => 3,
            BmpMessageToken::StatisticsReport => 4,
            BmpMessageToken::PeerDownNotification => 5,
            BmpMessageToken::PeerUpNotification => 6,
            BmpMessageToken::InitiationMessage => 7,
            BmpMessageToken::TerminationMessage => 8,
            BmpMessageToken::RouteMirroring => 9,
        }
    }
}

//------------ Modification & Creation of new Updates -----------------------

// #[derive(Debug, Hash, Serialize)]
// pub struct Message(
//     pub RouteCoreBmpMessage<bytes::Bytes>,
// );

// impl Message {
//     pub fn new(bytes: bytes::Bytes, config: SessionConfig) -> Self {
//         Self(routecore::bgp::message::UpdateMessage::
//              <bytes::Bytes>::from_octets(bytes, config).unwrap())
//     }

//     // Materialize a ChangeSet from the Update message. The materialized
//     // Change set is completely self-contained (no references of any kind) &
//     // holds all the attributes of the current BGP Update message.
//     pub fn create_changeset(&self, prefix: Prefix) -> AttrChangeSet {
//         AttrChangeSet {
//             prefix: ReadOnlyScalarOption::<Prefix>::new(prefix.into()),
//             as_path: VectorOption::<AsPath>::from(
//                 self.0.aspath().map(|p| p.to_hop_path()),
//             ),
//             origin_type: ScalarOption::<OriginType>::from(self.0.origin()),
//             next_hop: ScalarOption::<NextHop>::from(self.0.next_hop()),
//             multi_exit_discriminator: ScalarOption::from(
//                 self.0.multi_exit_desc(),
//             ),
//             local_pref: ScalarOption::from(self.0.local_pref()),
//             atomic_aggregate: ScalarOption::from(Some(
//                 self.0.is_atomic_aggregate(),
//             )),
//             aggregator: ScalarOption::from(self.0.aggregator()),
//             communities: VectorOption::from(self.0.all_communities()),
//             originator_id: Todo,
//             cluster_list: Todo,
//             extended_communities: Todo,
//             // value: self
//             //     .ext_communities()
//             //     .map(|c| c.collect::<Vec<ExtendedCommunity>>()),
//             as4_path: VectorOption::from(
//                 self.0.as4path().map(|p| p.to_hop_path()),
//             ),
//             connector: Todo,
//             as_path_limit: Todo,
//             pmsi_tunnel: Todo,
//             ipv6_extended_communities: Todo,
//             large_communities: Todo,
//             // value: T::try_from(self
//             //     .large_communities()
//             //     .map(|c| c.collect::<Vec<LargeCommunity>>())),
//             bgpsec_as_path: Todo,
//             attr_set: Todo,
//             rsrvd_development: Todo,
//             as4_aggregator: Todo,
//         }
//     }

//     // Create a new BGP Update message by applying the attributes changes
//     // in the supplied change set to our current Update message.
//     pub fn create_update_from_changeset<T: ScalarValue, V: VectorValue>(
//         _change_set: &AttrChangeSet,
//     ) -> Self {
//         todo!()
//     }
// }
