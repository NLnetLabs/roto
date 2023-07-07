use crate::{
    lazyelmtypevalue,
    ast::ShortString,
    compile::CompileError,
    createtoken,
    traits::Token,
    types::{
        collections::{LazyRecord, LazyElementTypeValue},
        typedef::{TypeDef, NamedTypeDef, LazyNamedTypeDef},
        builtin::{Boolean, BuiltinTypeValue, IpAddress},constant_enum::EnumVariant,
        typevalue::TypeValue, lazytypedef::{PeerUpNotification, RouteMonitoring},
    }, lazyrecord, lazyenum, lazyfield,
};

pub use crate::types::collections::BytesRecord;

//------------ BmpRouteMonitoringMessage ------------------------------------

createtoken!(
    BmpMessageToken;
    GetType = 0,
    RouteMonitoring = 1,
    StatisticsReport = 2,
    PeerDownNotification = 3,
    PeerUpNotification = 4,
    InitiationMessage = 5,
    TerminationMessage = 6,
    RouteMirroring = 7
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
    pub fn type_def() -> Vec<NamedTypeDef> {
        vec![(
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
        )]
    }

    pub(crate) fn lazy_type_def<'a>() -> LazyNamedTypeDef<
        'a,
        routecore::bmp::message::RouteMonitoring<bytes::Bytes>,
    > {
        vec![(
            "route_monitoring".into(),
            LazyElementTypeValue::LazyRecord(lazyrecord!(vec![(
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
                        BytesRecord<routecore::bmp::message::RouteMonitoring<bytes::Bytes>>,
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
                        BytesRecord<routecore::bmp::message::RouteMonitoring<bytes::Bytes>>,
                        per_peer_header.adj_rib_type
                    ),
                    lazyfield!(
                        "address",
                        IpAddress,
                        per_peer_header.address
                    )
                ]))
            )])),
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

impl BytesRecord<PeerUpNotification> {
    pub(crate) fn lazy_type_def<'a>() -> LazyNamedTypeDef<
        'a,
        routecore::bmp::message::PeerUpNotification<bytes::Bytes>,
    > {
        vec![(
            "route_monitoring".into(),
            LazyElementTypeValue::LazyRecord(lazyrecord!(vec![(
                "per_peer_header".into(),
                LazyElementTypeValue::LazyRecord(lazyrecord!(vec![
                    lazyenum!(
                        "peer_type",
                        EnumVariant<U8> = "BMP_PEER_TYPE",
                        BytesRecord<routecore::bmp::message::PeerUpNotification<bytes::Bytes>>,
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
                ]))
            )])),
        )]
    }

    pub(crate) fn type_def() -> Vec<NamedTypeDef> {
        vec![(
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
