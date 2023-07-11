use log::trace;

use crate::{
    ast::ShortString,
    compile::CompileError,
    createtoken, lazyelmtypevalue, lazyenum, lazyfield, lazyrecord,
    traits::Token,
    types::{
        builtin::{Boolean, BuiltinTypeValue, IpAddress},
        collections::{LazyElementTypeValue, LazyRecord},
        constant_enum::EnumVariant,
        lazytypedef::{LazyTypeDef, PeerUpNotification, RouteMonitoring},
        typedef::{LazyNamedTypeDef, NamedTypeDef, TypeDef},
        typevalue::TypeValue,
    },
    vm::VmError,
};

pub use crate::types::collections::BytesRecord;

//------------ BmpRouteMonitoringMessage ------------------------------------

createtoken!(
    BmpMessageToken;
    RouteMonitoring = 0,
    StatisticsReport = 1,
    PeerDownNotification = 2,
    PeerUpNotification = 3,
    InitiationMessage = 4,
    TerminationMessage = 5,
    RouteMirroring = 6
);

createtoken!(
    RouteMonitoringToken;
    PerPeerHeader = 0
);

createtoken!(
    PerPeerHeaderToken;
    PeerType = 0,
    IsIpV4 = 1,
    IsIpV6 = 2
);

impl BytesRecord<RouteMonitoring> {
    pub fn new(bytes: bytes::Bytes) -> Result<Self, VmError> {
        if let routecore::bmp::message::Message::RouteMonitoring(rm_msg) =
            routecore::bmp::message::Message::<bytes::Bytes>::from_octets(
                bytes,
            )
            .unwrap()
        {
            Ok(Self(rm_msg))
        } else {
            Err(VmError::InvalidMsgType)
        }
    }

    pub fn type_def() -> Vec<NamedTypeDef> {
        vec![(
            "per_peer_header".into(),
            TypeDef::Record(vec![
                (
                    "peer_type".into(),
                    TypeDef::ConstEnumVariant("BMP_PEER_TYPE".into()).into(),
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
        )]
    }

    pub(crate) fn lazy_type_def<'a>() -> LazyNamedTypeDef<
        'a,
        routecore::bmp::message::RouteMonitoring<bytes::Bytes>,
    > {
        vec![(
            // "route_monitoring".into(),
            // LazyElementTypeValue::LazyRecord(lazyrecord!(vec![(
            "per_peer_header".into(),
            LazyElementTypeValue::LazyRecord(lazyrecord!(vec![
                // (
                //     "peer_type".into(),
                //     LazyElementTypeValue::Lazy(Box::new(
                //         |raw_message: &BytesRecord<routecore::bmp::message::RouteMonitoring<bytes::Bytes>>| {
                //         TypeValue::Builtin(
                //         // BuiltinTypeValue::ConstU8EnumVariant(
                //             EnumVariant::<u8>::new(
                //                 "BMP_PEER_TYPE".into(),
                //                 raw_message.bytes()
                //                     .per_peer_header()
                //                     .peer_type()
                //                     .into(),
                //         ).into(),
                //         // )
                //     ).into() }))
                // ),
                lazyenum!(
                    "peer_type",
                    EnumVariant<U8> = "BMP_PEER_TYPE",
                    BytesRecord<RouteMonitoring>,
                    per_peer_header.peer_type
                ),
                lazyfield!("is_ipv4", Boolean, per_peer_header.is_ipv4),
                lazyfield!("is_ipv6", Boolean, per_peer_header.is_ipv6),
                lazyfield!(
                    "is_pre_policy",
                    Boolean,
                    per_peer_header.is_pre_policy
                ),
                lazyfield!(
                    "is_post_policy",
                    Boolean,
                    per_peer_header.is_post_policy
                ),
                lazyfield!(
                    "is_legacy_format",
                    Boolean,
                    per_peer_header.is_legacy_format
                ),
                lazyenum!(
                    "adj_rib_type",
                    EnumVariant<U8> = "BMP_ADJ_RIB_TYPE",
                    BytesRecord<RouteMonitoring>,
                    per_peer_header.adj_rib_type
                ),
                lazyfield!("address", IpAddress, per_peer_header.address)
            ])),
        )] //)),
           //)]
    }

    pub(crate) fn get_props_for_field(
        field_name: &crate::ast::Identifier,
    ) -> Result<(TypeDef, crate::traits::Token), CompileError>
    where
        Self: std::marker::Sized,
    {
        match field_name.ident.as_str() {
            "per_peer_header" => Ok((
                TypeDef::LazyRecord(LazyTypeDef::BmpPerPeerHeader),
                Token::FieldAccess(vec![usize::from(
                    RouteMonitoringToken::PerPeerHeader,
                ) as u8]),
            )),
            // "statistics_report" => Ok((
            //     TypeDef::StatisticsReport,
            //     Token::FieldAccess(vec![
            //         BmpMessageToken::StatisticsReport.into() as u8
            //     ]),
            // )),
            _ => {
                trace!(
                    "Unknown field '{}' for type BmpUpdateMessage",
                    field_name.ident
                );
                Err(format!(
                    "Unknown field '{}' for type BmpUpdateMessage",
                    field_name.ident
                )
                .into())
            }
        }
    }
}

impl BytesRecord<PeerUpNotification> {
    pub(crate) fn lazy_type_def<'a>() -> LazyNamedTypeDef<
        'a,
        routecore::bmp::message::PeerUpNotification<bytes::Bytes>,
    > {
        vec![(
            "per_peer_header".into(),
            LazyElementTypeValue::LazyRecord(lazyrecord!(vec![
                lazyenum!(
                    "peer_type",
                    EnumVariant<U8> = "BMP_PEER_TYPE",
                    BytesRecord<PeerUpNotification>,
                    per_peer_header.peer_type
                ),
                lazyfield!("is_ipv4", Boolean, per_peer_header.is_ipv4),
                lazyfield!("is_ipv6", Boolean, per_peer_header.is_ipv6),
                lazyfield!(
                    "is_pre_policy",
                    Boolean,
                    per_peer_header.is_pre_policy
                ),
                lazyfield!(
                    "is_post_policy",
                    Boolean,
                    per_peer_header.is_post_policy
                ),
                lazyfield!(
                    "is_legacy_format",
                    Boolean,
                    per_peer_header.is_legacy_format
                ),
                lazyenum!(
                    "adj_rib_type",
                    EnumVariant<U8> = "BMP_ADJ_RIB_TYPE",
                    BytesRecord<routecore::bmp::message::PeerUpNotification<bytes::Bytes>>,
                    per_peer_header.adj_rib_type
                )
            ])),
        )]
    }

    pub(crate) fn type_def() -> Vec<NamedTypeDef> {
        vec![(
            "per_peer_header".into(),
            TypeDef::Record(vec![
                (
                    "peer_type".into(),
                    TypeDef::ConstEnumVariant("BMP_PEER_TYPE".into()).into(),
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
        )]
    }

    pub(crate) fn get_props_for_field(
        field_name: &crate::ast::Identifier,
    ) -> Result<(TypeDef, crate::traits::Token), CompileError>
    where
        Self: std::marker::Sized,
    {
        match field_name.ident.as_str() {
            "route_monitoring" => Ok((
                TypeDef::Record(BytesRecord::<PeerUpNotification>::type_def()),
                Token::FieldAccess(vec![usize::from(
                    BmpMessageToken::RouteMonitoring,
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
}

impl BytesRecord<routecore::bmp::message::PerPeerHeader<bytes::Bytes>> {
    pub(crate) fn type_def() -> Vec<NamedTypeDef> {
        vec![
                (
                    "peer_type".into(),
                    TypeDef::ConstEnumVariant("BMP_PEER_TYPE".into()).into(),
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
            ]
    }

    pub(crate) fn get_props_for_field(
        field_name: &crate::ast::Identifier,
    ) -> Result<(TypeDef, crate::traits::Token), CompileError> {
        match field_name.ident.as_str() {
            "is_ipv4" => Ok((
                TypeDef::Boolean,
                Token::FieldAccess(vec![usize::from(
                    PerPeerHeaderToken::IsIpV4,
                ) as u8]),
            )),
            "is_ipv6" => Ok((
                TypeDef::Boolean,
                Token::FieldAccess(vec![usize::from(
                    PerPeerHeaderToken::IsIpV6,
                ) as u8]),
            )),
            _ => Err(format!(
                "Unknown field '{}' for type PerPeerHEader",
                field_name.ident
            )
            .into()),
        }
    }
}
