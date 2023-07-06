use crate::{
    ast::ShortString,
    compile::CompileError,
    createtoken, lazyelmtypevalue, lazyenum, lazyfield, lazyrecord,
    traits::Token,
    types::{
        collections::{BytesRecord, LazyElementTypeValue, LazyRecord},
        constant_enum::EnumVariant,
        typedef::{LazyNamedTypeDef, MethodProps, TypeDef},
        typevalue::TypeValue,
    },
};

use super::{Boolean, BuiltinTypeValue};

//------------ BmpRouteMonitoringMessage ------------------------------------
impl LazyRecord<'_, routecore::bmp::message::RouteMonitoring<bytes::Bytes>> {
    pub(crate) fn type_def<'a>() -> LazyNamedTypeDef<
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
                        routecore::bmp::message::RouteMonitoring<bytes::Bytes>,
                        per_peer_header.peer_type
                    ),
                    // (
                    //     "is_ipv4".into(),
                    //     lazyelmtypevalue!(
                    //         raw_message;
                    //         TypeValue::Builtin(
                    //             // BuiltinTypeValue::from(
                    //                 Boolean::new(
                    //                     raw_message.bytes().per_peer_header().is_ipv4()
                    //                 ).into()
                    //             // )
                    //         ).into()
                    //     )
                    // ),
                    // lazyfield!("is_ipv4";Boolean;raw_message;raw_message.bytes().per_peer_header().is_ipv4()),
                    lazyfield!("is_ipv4", Boolean, per_peer_header.is_ipv4),
                    // (
                    //     "is_ipv6".into(),
                    //     lazyelmtypevalue!(
                    //         raw_message;
                    //         TypeValue::Builtin(
                    //             // BuiltinTypeValue::from(
                    //                 Boolean::new(
                    //                     raw_message.bytes().per_peer_header().is_ipv6()
                    //                 ).into()
                    //             // )
                    //         ).into()
                    //     )
                    // ),
                    lazyfield!("is_ipv6", Boolean, per_peer_header.is_ipv6),
                    // (
                    //     "is_pre_policy".into(),
                    //     lazyelmtypevalue!(
                    //         raw_message;
                    //         TypeValue::Builtin(
                    //             // BuiltinTypeValue::from(
                    //                 Boolean::new(
                    //                     raw_message.bytes().per_peer_header().is_pre_policy()
                    //                 ).into()
                    //             // )
                    //         ).into()
                    //     )
                    // ),
                    lazyfield!(
                        "is_pre_policy",
                        Boolean,
                        per_peer_header.is_pre_policy
                    ),
                    // (
                    //     "is_post_policy".into(),
                    //     lazyelmtypevalue!(
                    //         raw_message;
                    //         TypeValue::Builtin(
                    //             // BuiltinTypeValue::from(
                    //                 Boolean::new(
                    //                     raw_message.bytes().per_peer_header().is_post_policy()
                    //                 ).into()
                    //             // )
                    //         ).into()
                    //     )
                    // ),
                    lazyfield!(
                        "is_post_policy",
                        Boolean,
                        per_peer_header.is_post_policy
                    ),
                    // (
                    //     "is_legacy_format".into(),
                    //     lazyelmtypevalue!(
                    //         raw_message;
                    //         TypeValue::Builtin(
                    //             BuiltinTypeValue::from(
                    //                 Boolean::new(
                    //                     raw_message.bytes().per_peer_header().is_legacy_format()
                    //                 )
                    //             )
                    //         ).into()
                    //     )
                    // ),
                    lazyfield!(
                        "is_legacy_format",
                        Boolean,
                        per_peer_header.is_legacy_format
                    ),
                    // (
                    //     "adj_rib_type".into(),
                    //     lazyelmtypevalue!(
                    //         raw_message;
                    //         TypeValue::Builtin(
                    //             BuiltinTypeValue::ConstU8EnumVariant(
                    //                 EnumVariant::<u8> {
                    //                     enum_name: "BMP_ADJ_RIB_TYPE"
                    //                         .into(),
                    //                     value: raw_message
                    //                         .bytes()
                    //                         .per_peer_header()
                    //                         .adj_rib_type()
                    //                         .into(),
                    //                 },
                    //             )
                    //         ).into()
                    //     )
                    // )
                    lazyenum!(
                        "adj_rib_type",
                        EnumVariant<U8> = "BMP_ADJ_RIB_TYPE",
                        routecore::bmp::message::RouteMonitoring<bytes::Bytes>,
                        per_peer_header.adj_rib_type
                    )
                ]))
            )])),
        )]
    }

    pub(crate) fn get_props_for_method(
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
                    PerPeerHeaderToken::PeerType,
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

    // pub(crate) fn lazy_field_for_field_index(
    //     // &self,
    //     raw_message: &routecore::bmp::message::Message<bytes::Bytes>,
    //     field_index: SmallVec<[usize; 8]>,
    // ) -> Option<TypeValue> {
    //     let mut field_iter = field_index.into_iter();
    //     if let Some(field_token) = field_iter.next() {
    //         match field_token.into() {
    //             BmpMessageToken::RouteMonitoring => {
    //                 if let routecore::bmp::message::Message::RouteMonitoring(
    //                     rm_msg,
    //                 ) = raw_message
    //                 {
    //                     match field_iter.next().unwrap().into() {
    //                         RouteMonitoringToken::PerPeerHeader => {
    //                             match field_iter.next().unwrap().into() {
    //                                 PerPeerHeaderToken::PeerType => {
    //                                     TypeValue::Builtin(
    //                                         BuiltinTypeValue::ConstU8EnumVariant(
    //                                             EnumVariant::<u8> {
    //                                                 enum_name: "BMP_PEER_TYPE"
    //                                                     .into(),
    //                                                 value: rm_msg
    //                                                     .per_peer_header()
    //                                                     .peer_type()
    //                                                     .into(),
    //                                             },
    //                                         )
    //                                     )
    //                                     .into()
    //                                 },
    //                                 PerPeerHeaderToken::IsIpV4 =>
    //                                         TypeValue::Builtin(
    //                                             BuiltinTypeValue::from(
    //                                                 Boolean::new(
    //                                                     rm_msg.per_peer_header().is_ipv6()
    //                                                 )
    //                                             )
    //                                         ).into(),
    //                                 _ => None
    //                             }
    //                         },
    //                         _ => None
    //                     }
    //                 } else {
    //                     None
    //                 }
    //             }
    //             BmpMessageToken::GetType => todo!(),
    //             BmpMessageToken::StatisticsReport => todo!(),
    //             BmpMessageToken::PeerDownNotification => todo!(),
    //             BmpMessageToken::PeerUpNotification => todo!(),
    //             BmpMessageToken::InitiationMessage => todo!(),
    //             BmpMessageToken::TerminationMessage => todo!(),
    //             BmpMessageToken::RouteMirroring => todo!(),
    //             _ => None,
    //         }
    //     } else {
    //         None
    //     }
    // }
}

// impl RotoType for LazyRecord<'_, RouteCoreBmpMessage<bytes::Bytes>> {
//     fn get_props_for_method(
//         _ty: TypeDef,
//         method_name: &crate::ast::Identifier,
//     ) -> Result<MethodProps, CompileError>
//     where
//         Self: std::marker::Sized,
//     {
//         match method_name.ident.as_str() {
//             "get_type" => Ok(MethodProps::new(
//                 TypeDef::ConstEnumVariant("BMP_MESSAGE_TYPES".into()),
//                 BmpMessageToken::GetType.into(),
//                 vec![],
//             )),

//             // "cmp" => Ok(MethodProps::new(
//             //     TypeDef::IntegerLiteral,
//             //     StringLiteralToken::Cmp.into(),
//             //     vec![TypeDef::StringLiteral, TypeDef::StringLiteral],
//             // )),
//             _ => Err(format!(
//                 "Unknown method: '{}' for type BgpUpdateMessage",
//                 method_name.ident
//             )
//             .into()),
//         }
//     }

//     fn into_type(self, ty: &TypeDef) -> Result<TypeValue, CompileError>
//     where
//         Self: std::marker::Sized,
//     {
//         Err(format!(
//             "BgpUpdateMessage cannot be converted to type {} (or any other type)",
//             ty
//         )
//         .into())
//     }

//     fn exec_value_method<'a>(
//         &'a self,
//         _method_token: usize,
//         _args: &'a [StackValue],
//         _res_type: TypeDef,
//     ) -> Result<TypeValue, VmError> {
//         // match method_token.into() {
//         //     // BmpMessageToken::GetType => Ok(TypeValue::Builtin(
//         //     //     BuiltinTypeValue::ConstU16EnumVariant(EnumVariant {
//         //     //         enum_name: "BMP_MESSAGE_TYPES".into(),
//         //     //         value: self.raw_message.0.nlris().afi().into(),
//         //     //     }),
//         //     // )),
//         //     // BmpMessageToken::Safi => Ok(TypeValue::Builtin(
//         //     //     BuiltinTypeValue::ConstU8EnumVariant(EnumVariant {
//         //     //         enum_name: "SAFI".into(),
//         //     //         value: self.raw_message.0.nlris().safi().into(),
//         //     //     }),
//         //     // )),
//         //     _ => Err(VmError::InvalidMethodCall),
//         // }
//         todo!()
//     }

//     fn exec_consume_value_method(
//         self,
//         _method_token: usize,
//         _args: Vec<TypeValue>,
//         _res_type: TypeDef,
//     ) -> Result<TypeValue, VmError> {
//         Err(VmError::InvalidMethodCall)
//     }

//     fn exec_type_method(
//         _method_token: usize,
//         _args: &[StackValue],
//         _res_type: TypeDef,
//     ) -> Result<TypeValue, VmError> {
//         Err(VmError::InvalidMethodCall)
//     }
// }

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

// impl<'a>
//     LazyRecord<'a, RawBytes<routecore::bmp::message::Message<bytes::Bytes>>>
// {
//     pub(crate) fn lazy_evaluator<T>(
//         raw_message: Arc<
//             RawBytes<routecore::bmp::message::Message<bytes::Bytes>>,
//         >,
//     ) -> LazyRecord<
//         'a,
//         RawBytes<T>,
//     > {
//         let peer_type = lazyelmtypevalue!(
//             raw_message;
//             {
//             if let RouteCoreBmpMessage::RouteMonitoring(rm_msg) =
//                 raw_message.bytes()
//             {
//                 TypeValue::Builtin(BuiltinTypeValue::ConstU8EnumVariant(
//                     EnumVariant::<u8> {
//                         enum_name: "BMP_PEER_TYPE".into(),
//                         value: rm_msg.per_peer_header().peer_type().into(),
//                     },
//                 ))
//                 .into()
//             } else {
//                 TypeValue::Unknown.into()
//             }
//         });

//         let per_peer_header = LazyElementTypeValue::LazyRecord(
//             LazyRecord::new(
//                 (RotondaId(0), 0),
//                 vec![("peer_type".into(), peer_type)],
//             )
//             .unwrap(),
//         );

//         let r_m = vec![(
//             "route_monitoring".into(),
//             LazyElementTypeValue::LazyRecord(
//                 LazyRecord::new(
//                     (RotondaId(0), 0),
//                     vec![("per_peer_header".into(), per_peer_header)],
//                 )
//                 .unwrap(),
//             ),
//         )];

//         LazyRecord::new((RotondaId(0), 0), r_m).unwrap()
//     }
// }

// impl<'a>
//     LazyRecord<
//         'a,
//         BytesRecord<routecore::bmp::message::RouteMonitoring<bytes::Bytes>>,
//     >
// {
//     pub(crate) fn lazy_evaluator2(
//         raw_message: BytesRecord<
//             routecore::bmp::message::RouteMonitoring<bytes::Bytes>,
//         >,
//     ) -> LazyRecord<'a, routecore::bmp::message::RouteMonitoring<bytes::Bytes>>
//     {
//         {
//             lazyrecord!(vec![(
//                 "route_monitoring".into(),
//                 LazyElementTypeValue::LazyRecord(lazyrecord!(vec![(
//                     "per_peer_header".into(),
//                     LazyElementTypeValue::LazyRecord(lazyrecord!(vec![
//                         (
//                             "peer_type".into(),
//                             LazyElementTypeValue::Lazy(Box::new(
//                                 |raw_message: &BytesRecord<
//                                     routecore::bmp::message::RouteMonitoring<
//                                         bytes::Bytes,
//                                     >,
//                                 >| {
//                                     TypeValue::Builtin(
//                                         BuiltinTypeValue::ConstU8EnumVariant(
//                                             EnumVariant::<u8> {
//                                                 enum_name: "BMP_PEER_TYPE"
//                                                     .into(),
//                                                 value: raw_message
//                                                     .bytes()
//                                                     .per_peer_header()
//                                                     .peer_type()
//                                                     .into(),
//                                             },
//                                         ),
//                                     )
//                                     .into()
//                                 }
//                             )) // .into())
//                         ),
//                         (
//                             "is_ipv4".into(),
//                             lazyelmtypevalue!(
//                                 raw_message;
//                                 TypeValue::Builtin(
//                                     BuiltinTypeValue::from(
//                                         Boolean::new(
//                                             raw_message.bytes().per_peer_header().is_ipv4()
//                                         )
//                                     )
//                                 ).into()
//                             )
//                         ),
//                         // (
//                         //     "is_ipv6".into(),
//                         //     lazyelmtypevalue!(
//                         //         TypeValue::Builtin(
//                         //             BuiltinTypeValue::from(
//                         //                 Boolean::new(
//                         //                     rm_msg.per_peer_header().is_ipv6()
//                         //                 )
//                         //             )
//                         //         ).into()
//                         //     )
//                         // ),
//                         // (
//                         //     "is_pre_policy".into(),
//                         //     lazyelmtypevalue!(
//                         //         TypeValue::Builtin(
//                         //             BuiltinTypeValue::from(
//                         //                 Boolean::new(
//                         //                     rm_msg.per_peer_header().is_pre_policy()
//                         //                 )
//                         //             )
//                         //         ).into()
//                         //     )
//                         // ),
//                         // (
//                         //     "is_post_policy".into(),
//                         //     lazyelmtypevalue!(
//                         //         TypeValue::Builtin(
//                         //             BuiltinTypeValue::from(
//                         //                 Boolean::new(
//                         //                     rm_msg.per_peer_header().is_post_policy()
//                         //                 )
//                         //             )
//                         //         ).into()
//                         //     )
//                         // ),
//                         // (
//                         //     "is_legacy_format".into(),
//                         //     lazyelmtypevalue!(
//                         //         TypeValue::Builtin(
//                         //             BuiltinTypeValue::from(
//                         //                 Boolean::new(
//                         //                     rm_msg.per_peer_header().is_legacy_format()
//                         //                 )
//                         //             )
//                         //         ).into()
//                         //     )
//                         // ),
//                         // (
//                         //     "adj_rib_type".into(),
//                         //     lazyelmtypevalue!(TypeValue::Builtin(
//                         //             BuiltinTypeValue::ConstU8EnumVariant(
//                         //                 EnumVariant::<u8> {
//                         //                     enum_name: "BMP_ADJ_RIB_TYPE"
//                         //                         .into(),
//                         //                     value: rm_msg
//                         //                         .per_peer_header()
//                         //                         .adj_rib_type()
//                         //                         .into(),
//                         //                 },
//                         //             )
//                         //         ).into()
//                         //     )
//                         // )
//                     ]))
//                 )]))
//             )])
//         }
//     }
// }

// impl LazyRecord<RouteCoreBmpMessage<bytes::Bytes>> {
//     pub(crate) fn lazy_evaluator(
//         &self,
//     ) -> Box<
//         dyn Fn() -> Vec<
//             crate::types::typedef::LazyNamedTypeDef<
//                 RouteMonitoring<bytes::Bytes>,
//             >,
//         >,
//     > {
//         Box::new(|| {
//             vec![(
//                 "route_monitoring".into(),
//                 TypeDef::Record(vec![(
//                     "per_peer_header".into(),
//                     TypeDef::Record(vec![
//                         (
//                             "peer_type".into(),
//                             TypeDef::ConstEnumVariant("BMP_PEER_TYPE".into())
//                                 .into(),
//                         ),
//                         ("is_ipv4".into(), TypeDef::Boolean.into()),
//                         ("is_ipv6".into(), TypeDef::Boolean.into()),
//                         ("is_pre_policy".into(), TypeDef::Boolean.into()),
//                         ("is_post_policy".into(), TypeDef::Boolean.into()),
//                         ("is_legacy_format".into(), TypeDef::Boolean.into()),
//                         (
//                             "adj_rib_type".into(),
//                             TypeDef::ConstEnumVariant(
//                                 "BMP_ADJ_RIB_TYPE".into(),
//                             )
//                             .into(),
//                         ),
//                         ("distinguisher".into(), TypeDef::HexLiteral.into()),
//                         ("address".into(), TypeDef::IpAddress.into()),
//                         ("asn".into(), TypeDef::Asn.into()),
//                         ("bgp_id".into(), TypeDef::HexLiteral.into()),
//                         ("ts_seconds".into(), TypeDef::U32.into()),
//                     ])
//                     .into(),
//                 )])
//                 .into(),
//                 // LazyElementTypeValue::LazyRecord(
//                 //     LazyRecord::new(
//                         LazyElementTypeValue::LazyRecord(
//                             LazyRecord::new(
//                                 (RotondaId(0),0).into(),
//                                 vec![("route_monitoring".into(),
//                                 LazyElementTypeValue::LazyRecord(
//                                     LazyRecord::<routecore::bmp::message::PerPeerHeader<bytes::Bytes>>::new(
//                                         rm_msg.into(),
//                                         vec![("per_peer_header".into(),
//                                     LazyElementTypeValue::LazyRecord::<routecore::bmp::message::PerPeerHeader<bytes::Bytes>>(LazyRecord::new(
//                                         rm_msg.per_peer_header().into(),
//                                         vec![
//                                             (
//                                                 "peer_type".into(),
//                                                 LazyElementTypeValue::Lazy(
//                                                     Box::new(|| { TypeValue::Builtin(
//                                                         BuiltinTypeValue::ConstU8EnumVariant(
//                                                             EnumVariant::<u8> {
//                                                                 enum_name: "BMP_PEER_TYPE"
//                                                                     .into(),
//                                                                 value: rm_msg
//                                                                     .per_peer_header()
//                                                                     .peer_type()
//                                                                     .into(),
//                                                             },
//                                                         )
//                                                     ).into()}))
//                                             ),
//                                             (
//                                                 "is_ipv4".into(),
//                                                 LazyElementTypeValue::Lazy(Box::new(|| {
//                                                     TypeValue::Builtin(
//                                                         BuiltinTypeValue::from(
//                                                             Boolean::new(
//                                                                 rm_msg.per_peer_header().is_ipv4()
//                                                             )
//                                                         )
//                                                     ).into()
//                                                 }))
//                                             ),
//                                             (
//                                                 "is_ipv6".into(),
//                                                 LazyElementTypeValue::Lazy(Box::new(|| {
//                                                     TypeValue::Builtin(
//                                                         BuiltinTypeValue::from(
//                                                             Boolean::new(
//                                                                 rm_msg.per_peer_header().is_ipv6()
//                                                             )
//                                                         )
//                                                     ).into()
//                                                 }))
//                                             ),
//                                             (
//                                                 "is_pre_policy".into(),
//                                                 LazyElementTypeValue::Lazy(Box::new(|| {
//                                                     TypeValue::Builtin(
//                                                         BuiltinTypeValue::from(
//                                                             Boolean::new(
//                                                                 rm_msg.per_peer_header().is_pre_policy()
//                                                             )
//                                                         )
//                                                     ).into()
//                                                 }))
//                                             ),
//                                             (
//                                                 "is_post_policy".into(),
//                                                 LazyElementTypeValue::Lazy(Box::new(|| {
//                                                     TypeValue::Builtin(
//                                                         BuiltinTypeValue::from(
//                                                             Boolean::new(
//                                                                 rm_msg.per_peer_header().is_post_policy()
//                                                             )
//                                                         )
//                                                     ).into()
//                                                 }))
//                                             ),
//                                             (
//                                                 "is_legacy_format".into(),
//                                                 LazyElementTypeValue::Lazy(Box::new(|| {
//                                                     TypeValue::Builtin(
//                                                         BuiltinTypeValue::from(
//                                                             Boolean::new(
//                                                                 rm_msg.per_peer_header().is_legacy_format()
//                                                             )
//                                                         )
//                                                     ).into()
//                                                 }))
//                                             ),
//                                             (
//                                                 "adj_rib_type".into(),
//                                                 LazyElementTypeValue::Lazy(
//                                                     Box::new(|| { TypeValue::Builtin(
//                                                         BuiltinTypeValue::ConstU8EnumVariant(
//                                                             EnumVariant::<u8> {
//                                                                 enum_name: "BMP_ADJ_RIB_TYPE"
//                                                                     .into(),
//                                                                 value: rm_msg
//                                                                     .per_peer_header()
//                                                                     .adj_rib_type()
//                                                                     .into(),
//                                                             },
//                                                         )
//                                                     ).into()}))
//                                             )
//                                         ]).unwrap())
//                                     )]).unwrap()
//                                 )
//                             )].into()).unwrap()
//                         )
//                         // ,
//                         //     RouteCoreBmpMessage::StatisticsReport(_) => todo!(),
//                         //     RouteCoreBmpMessage::PeerDownNotification(_) => todo!(),
//                         //     RouteCoreBmpMessage::PeerUpNotification(_) => todo!(),
//                         //     RouteCoreBmpMessage::InitiationMessage(_) => todo!(),
//                         //     RouteCoreBmpMessage::TerminationMessage(_) => todo!(),
//                         //     RouteCoreBmpMessage::RouteMirroring(_) => todo!(),
//                     )]
//         })
//         // )]
//         // )
//     }
//                         }
//                         // peer_type
//                     }
//                 },
//             )]
//         })
//     }
// }
// }
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
// }
