use log::trace;
use paste::paste;
use routecore::bmp::message::MessageType;

use crate::{
    ast::ShortString,
    bytes_record_impl,
    compile::CompileError,
    createtoken, lazyelmtypevalue, lazyenum, lazyfield,
    lazyrecord,
    traits::Token,
    types::{
        builtin::{Asn, Boolean, BuiltinTypeValue, IpAddress, U16},
        collections::{LazyElementTypeValue, LazyRecord},
        enum_types::EnumVariant,
        lazytypedef::{
            BmpMessage, LazyRecordTypeDef, PeerDownNotification,
            PeerUpNotification, RouteMonitoring,
        },
        typedef::{LazyNamedTypeDef, RecordTypeDef, TypeDef},
        typevalue::TypeValue,
    },
    vm::{CommandArg, StackValue, VmError},
};

pub use crate::types::collections::BytesRecord;


//------------ BmpMessage ---------------------------------------------------

createtoken!(
    BmpMessage;
    route_monitoring = 0
    // statistics_report = 1
    peer_down_notification = 2
    peer_up_notification = 3
    // initiation_message = 4
    // termination_message = 5
    // route_mirroring = 6
);

// bytes_record_impl!(
//     BmpMessage,
//     #[type_def(
//         enum_data_field(
//             "route_monitoring";
//             "",
//             EnumVariant<RouteMonitoring>,
//             BytesRecord<BmpMessage>,
//             BmpMessage::RouteMonitoring,
//             BmpRouteMonitoringMessage

//             // $next_enum_data_field_name: literal;
//             //         $next_enum_data_variant_identifier: literal,
//             //         $next_enum_data_ty: path, // = $next_enum_data_name: literal,
//             //         $next_enum_data_raw_ty: path,
//             //         $next_enum_data_variant: path,
//             //         $target_ty_value: path
//         ),
//         enum_data_field(
//             "peer_down_notification";
//             "",
//             EnumVariant<PeerDownNotification>,
//             BytesRecord<BmpMessage>,
//             BmpMessage::PeerDownNotification,
//             BmpPeerDownNotification
//         ),
//         enum_data_field(
//             "peer_up_notification";
//             "",
//             EnumVariant<PeerUpNotification>,
//             BytesRecord<BmpMessage>,
//             BmpMessage::PeerUpNotification,
//             BmpPeerUpNotification
//         ),
//     )]
// );

// impl BytesRecord<BmpMessage> {
//     pub fn new(bytes: bytes::Bytes) -> Result<Self, VmError> {
//         Ok(Self(
//             routecore::bmp::message::Message::<bytes::Bytes>::from_octets(
//                 bytes,
//             )
//             .map_err(|_| VmError::InvalidMsgType)?,
//         ))
//     }
// }

// pub(crate) enum LazyEnumVariant<'a> {
//     RouteMonitoring(LazyElementTypeValue<'a, RouteMonitoring>),
//     // StatisticsReport(LazyElementTypeValue<'a, StatisticsReport>),
//     PeerUpNotification(LazyElementTypeValue<'a, PeerUpNotification>),
//     PeerDownNotification(LazyElementTypeValue<'a, PeerDownNotification>),
// }

// pub(crate) struct LazyEnum<'a>(Vec<(ShortString, LazyEnumVariant<'a>)>);

// pub enum LazyEnum<'a> {
//     RouteMonitoring(LazyRecord<'a, RouteMonitoring>),
//     PeerUpNotification(LazyRecord<'a, PeerUpNotification>),
//     PeerDownNotification(LazyRecord<'a, PeerDownNotification>),
// }

// impl<'a> From<LazyRecord<'a, RouteMonitoring>> for LazyEnum<'a> {
//     fn from(value: LazyRecord<'a, RouteMonitoring>) -> Self {
//         LazyEnum::RouteMonitoring(value)
//     }
// }

// impl<'a> From<LazyRecord<'a, PeerDownNotification>> for LazyEnum<'a> {
//     fn from(value: LazyRecord<'a, PeerDownNotification>) -> Self {
//         LazyEnum::PeerDownNotification(value)
//     }
// }

// impl<'a> From<LazyRecord<'a, PeerUpNotification>> for LazyEnum<'a> {
//     fn from(value: LazyRecord<'a, PeerUpNotification>) -> Self {
//         LazyEnum::PeerUpNotification(value)
//     }
// }

// impl<'a> LazyEnum<'a> {
// pub fn exec_value_method(
//     &self,
//     variant_token: usize,
//     method_token: usize,
//     _args: &[StackValue],
//     res_type: TypeDef,
//     raw_bytes: &[u8]
// ) -> Result<TypeValue, VmError> {
//     match variant_token {
//         0 => {
//                 // let b_rec = if let LazyEnumVariant::RouteMonitoring(rm) = self.0[0].1 {
//                 //     rm.into_materialized(raw_bytes)
//                 // } else { panic!("FGFJDFDJF"); };
//                 // self.as_variant_0()?;
//                 let lazy_elm = &BytesRecord::<RouteMonitoring>::lazy_type_def()[method_token].1;
//                 let rm = routecore::bmp::message::RouteMonitoring::from_octets(bytes::Bytes::copy_from_slice(raw_bytes)).unwrap();
//                 let elm = lazy_elm.as_materialized(BytesRecord(rm))?;
//                 Ok(elm.into_type(&res_type).unwrap())
//         },
//         _ => Err(VmError::InvalidMethodCall)
//     }
// }

//     pub fn exec_consume_value_method(
//         &self,
//         _variant_token: usize,
//         _method_token: usize,
//         _args: Vec<TypeValue>,
//         _res_type: TypeDef,
//     ) -> Result<TypeValue, VmError> {
//         todo!()
//     }
// }

impl BytesRecord<BmpMessage> {
    pub fn new(bytes: bytes::Bytes) -> Result<Self, VmError> {
        Ok(Self(
            routecore::bmp::message::Message::<bytes::Bytes>::from_octets(
                bytes,
            )
            .map_err(|_| VmError::InvalidMsgType)?,
        ))
    }

    // pub fn exec_value_method(
    //     &self,
    //     variant_token: usize,
    //     _args: &[StackValue],
    //     _res_type: TypeDef,
    //     raw_bytes: &[u8],
    // ) -> Result<TypeValue, VmError> {
    //     match variant_token {
    //         0 => {
    //             trace!("exec value method on Route Monitoring");
    //             trace!(
    //                 "type def {:#?}",
    //                 &BytesRecord::<RouteMonitoring>::lazy_type_def()
    //             );
    //             let lazy_elm =
    //                 &BytesRecord::<RouteMonitoring>::lazy_type_def()[0].1;
    //             let rm =
    //                 routecore::bmp::message::RouteMonitoring::from_octets(
    //                     bytes::Bytes::copy_from_slice(raw_bytes),
    //                 )
    //                 .unwrap();
    //             let elm = lazy_elm.as_materialized(BytesRecord(rm))?;
    //             Ok(elm.into())
    //         }
    //         2 => {
    //             let lazy_elm =
    //                 &BytesRecord::<PeerDownNotification>::lazy_type_def()[0]
    //                     .1;
    //             let rm =
    //                 routecore::bmp::message::PeerDownNotification::from_octets(
    //                     bytes::Bytes::copy_from_slice(raw_bytes),
    //                 )
    //                 .unwrap();
    //             let elm = lazy_elm.as_materialized(BytesRecord(rm))?;
    //             Ok(elm.into())
    //         }
    //         3 => {
    //             let lazy_elm =
    //                 &BytesRecord::<PeerUpNotification>::lazy_type_def()[0].1;
    //             let rm =
    //                 routecore::bmp::message::PeerUpNotification::from_octets(
    //                     bytes::Bytes::copy_from_slice(raw_bytes),
    //                 )
    //                 .unwrap();
    //             let elm = lazy_elm.as_materialized(BytesRecord(rm))?;
    //             Ok(elm.into())
    //         }
    //         _ => Err(VmError::InvalidMethodCall),
    //     }
    // }

    pub fn get_variant(&self) -> MessageType {
        self.0.common_header().msg_type()
    }

    // Unlike a normal record, a bytes record need to have its recursive
    // fields resolved in one go, there can be no intermediary methods
    // that return a (sub)-field value and then other methods can take
    // that as argument for the next recursion IN THE VM, becuause that
    // would mean having to clone the (sub-)field and probably the whole
    // bytes message. This would defy the point of lazy evaluation.
    // Therefore this method takes the bytes record AND the complete
    // field index vec to go to do all the recursion in this method.
    // The data-fields of the variants in this enum are handled as
    // closely as possible to actual lazy fields. Note that we're still
    // copying bytes out into the actual variant. Grrr, TODO. Another
    // thing is the different layout of the arguments of this method
    // as compared to the `get_field_*` for non-enum records. 
    pub fn get_field_index_for_variant(
        &self,
        variant_token: usize,
        field_index: Option<CommandArg>,
        _args: &[StackValue],
        _res_type: TypeDef,
        raw_bytes: &[u8],
    ) -> Result<TypeValue, VmError> {
        // let field_index = args
        //     .iter()
        //     .filter_map(move |sv| {
        //         if let StackValue::Owned(TypeValue::Builtin(
        //             BuiltinTypeValue::U8(U8(i)),
        //         )) = sv
        //         {
        //             Some(*i as usize)
        //         } else {
        //             None
        //         }
        //     })
        //     .collect::<Vec<_>>();

        if let Some(CommandArg::FieldIndex(field_index)) =
            field_index
        {

            if variant_token != u8::from(self.get_variant()).into() {
                return Ok(TypeValue::Unknown);
            };

            let lazy_rec: TypeValue = match variant_token {
                0 => {
                    trace!("get_field_index_for_variant on Route Monitoring");
                    trace!("field index {:?}", field_index);
                    trace!(
                        "type def {:#?}",
                        &BytesRecord::<RouteMonitoring>::lazy_type_def()
                    );
                    let rm =
                        routecore::bmp::message::RouteMonitoring::from_octets(
                            bytes::Bytes::copy_from_slice(raw_bytes),
                        )
                        .unwrap();
                    LazyRecord::new(
                        BytesRecord::<RouteMonitoring>::lazy_type_def(),
                    )
                    .get_field_by_index(&field_index, &BytesRecord(rm))
                    .map(|elm| elm.into())
                    .unwrap()
                }
                2 => {
                    let pd =
                        routecore::bmp::message::PeerDownNotification::from_octets(
                            bytes::Bytes::copy_from_slice(raw_bytes),
                        )
                        .unwrap();
                    LazyRecord::new(
                        BytesRecord::<PeerDownNotification>::lazy_type_def(),
                    )
                    .get_field_by_index(&field_index, &BytesRecord(pd))
                    .map(|elm| elm.into())
                    .unwrap()
                }
                3 => {
                    let pu =
                        routecore::bmp::message::PeerUpNotification::from_octets(
                            bytes::Bytes::copy_from_slice(raw_bytes),
                        )
                        .unwrap();
                    LazyRecord::new(
                        BytesRecord::<PeerUpNotification>::lazy_type_def(),
                    )
                    .get_field_by_index(&field_index, &BytesRecord(pu))
                    .map(|elm| elm.into())
                    .unwrap()
                }
                _ => {
                    return Err(VmError::InvalidMethodCall);
                }
            };

            Ok(lazy_rec)
        } else {    
            Err(VmError::InvalidMethodCall)
        }
    }

    pub fn exec_consume_value_method(
        &self,
        _variant_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    pub(crate) fn get_props_for_variant(
        field_name: &crate::ast::Identifier,
    ) -> Result<(TypeDef, Token), CompileError> {
        match field_name.ident.as_str() {
            "RouteMonitoring" => Ok((
                // TypeDef::Record(BytesRecord::<RouteMonitoring>::type_def()),
                TypeDef::LazyRecord(LazyRecordTypeDef::RouteMonitoring),
                // TypeDef::LazyRecord(LazyRecordTypeDef::RouteMonitoring),
                Token::Variant(BmpMessageToken::RouteMonitoring.into()),
            )),
            // "statistics_report" => {
            //     BytesRecord::<StatisticsReport>::get_props_for_field(
            //         field_name,
            //     )
            // }
            "PeerUpNotification" => Ok((
                // TypeDef::Record(BytesRecord::<PeerUpNotification>::type_def()),
                TypeDef::LazyRecord(LazyRecordTypeDef::PeerUpNotification),
                Token::Variant(BmpMessageToken::PeerUpNotification.into()),
            )),
            "PeerDownNotification" => Ok((
                TypeDef::LazyRecord(LazyRecordTypeDef::PeerDownNotification),
                Token::Variant(BmpMessageToken::PeerDownNotification.into()),
            )),
            name => Err(CompileError::from(format!(
                "No variant name {} for BmpMessage",
                name
            ))),
        }
    }
}

//------------ BmpRouteMonitoringMessage ------------------------------------

// THe fields of a bytes_record_impl should be STRICTLY alphabeticallay
// ordered by the the name of the key and numbered in that order.

bytes_record_impl!(
    RouteMonitoring,
    #[type_def(
        record_field(
            "per_peer_header"; 0,
            field("address"; 1, IpAddress, per_peer_header.address),
            enum_field(
                "adj_rib_type"; 2,
                EnumVariant<U8> = "BMP_ADJ_RIB_TYPE",
                BytesRecord<RouteMonitoring>,
                per_peer_header.adj_rib_type
            ),
            field("asn"; 3, Asn, per_peer_header.asn),
            field("is_ipv4"; 4, Boolean, per_peer_header.is_ipv4),
            field("is_ipv6"; 5, Boolean, per_peer_header.is_ipv6),
            field(
                "is_legacy_format"; 6,
                Boolean,
                per_peer_header.is_legacy_format
            ),
            field(
                "is_post_policy"; 7,
                Boolean,
                per_peer_header.is_post_policy
            ),
            field(
                "is_pre_policy"; 8,
                Boolean,
                per_peer_header.is_pre_policy
            ),
            enum_field(
                "peer_type"; 9,
                EnumVariant<U8> = "BMP_PEER_TYPE",
                BytesRecord<RouteMonitoring>,
                per_peer_header.peer_type
            ),
        ),
    )]
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
}

//------------ PeerUpNotification -------------------------------------------

// THe fields of a bytes_record_impl should be STRICTLY alphabeticallay
// ordered by the the name of the key and numbered in that order.

bytes_record_impl!(
    PeerUpNotification,
    #[type_def(
        field(
            "local_address"; 0,
            IpAddress,
            local_address
        ),
        field(
            "local_port"; 1,
            U16,
            local_port
        ),
        record_field(
            "per_peer_header"; 2,
            field("address"; 3, IpAddress, per_peer_header.address),
            enum_field(
                "adj_rib_type"; 4,
                EnumVariant<U8> = "BMP_ADJ_RIB_TYPE",
                BytesRecord<PeerUpNotification>,
                per_peer_header.adj_rib_type
            ),
            field("is_ipv4"; 5, Boolean, per_peer_header.is_ipv4),
            field("is_ipv6"; 6, Boolean, per_peer_header.is_ipv6),
            field(
                "is_legacy_format"; 7,
                Boolean,
                per_peer_header.is_legacy_format
            ),
            field(
                "is_post_policy"; 8,
                Boolean,
                per_peer_header.is_post_policy
            ),
            field(
                "is_pre_policy"; 9,
                Boolean,
                per_peer_header.is_pre_policy
            ),
            enum_field(
                "peer_type"; 10,
                EnumVariant<U8> = "BMP_PEER_TYPE",
                BytesRecord<PeerUpNotification>,
                per_peer_header.peer_type
            ),
        ),
        field(
            "remote_port"; 11,
            U16,
            remote_port
        ),
        record_field(
            "session_config"; 12,
            field(
                "has_four_octet_asn"; 13,
                Boolean,
                session_config.has_four_octet_asn
            ),
        ),
    )]
);

impl BytesRecord<PeerUpNotification> {
    pub fn new(bytes: bytes::Bytes) -> Result<Self, VmError> {
        if let routecore::bmp::message::Message::PeerUpNotification(pu_msg) =
            routecore::bmp::message::Message::<bytes::Bytes>::from_octets(
                bytes,
            )
            .unwrap()
        {
            Ok(Self(pu_msg))
        } else {
            Err(VmError::InvalidMsgType)
        }
    }
}

//------------ PeerDownNotification -------------------------------------------

bytes_record_impl!(
    PeerDownNotification,
    #[type_def(
        record_field(
            "per_peer_header"; 0,
            field("address"; 1, IpAddress, per_peer_header.address),
            enum_field(
                "adj_rib_type"; 2,
                EnumVariant<U8> = "BMP_ADJ_RIB_TYPE",
                BytesRecord<PeerDownNotification>,
                per_peer_header.adj_rib_type
            ),
            field("is_ipv4"; 3, Boolean, per_peer_header.is_ipv4),
            field("is_ipv6"; 4, Boolean, per_peer_header.is_ipv6),
            field(
                "is_legacy_format"; 5,
                Boolean,
                per_peer_header.is_legacy_format
            ),
            field(
                "is_post_policy"; 6,
                Boolean,
                per_peer_header.is_post_policy
            ),
            field(
                "is_pre_policy"; 7,
                Boolean,
                per_peer_header.is_pre_policy
            ),
            enum_field(
                "peer_type"; 8,
                EnumVariant<U8> = "BMP_PEER_TYPE",
                BytesRecord<PeerDownNotification>,
                per_peer_header.peer_type
            ),
        ),
    )]
);

impl BytesRecord<PeerDownNotification> {
    pub fn new(bytes: bytes::Bytes) -> Result<Self, VmError> {
        if let routecore::bmp::message::Message::PeerDownNotification(
            pd_msg,
        ) = routecore::bmp::message::Message::<bytes::Bytes>::from_octets(
            bytes,
        )
        .unwrap()
        {
            Ok(Self(pd_msg))
        } else {
            Err(VmError::InvalidMsgType)
        }
    }
}
