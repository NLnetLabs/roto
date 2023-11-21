use log::trace;
use paste::paste;
use routecore::bmp::message::MessageType;
use smallvec::SmallVec;

use crate::{
    ast::ShortString,
    bytes_record_impl,
    compiler::compile::CompileError,
    createtoken, lazyelmtypevalue, lazyenum, lazyfield, lazyrecord,
    traits::Token,
    types::{
        builtin::{Asn, Boolean, BuiltinTypeValue, IpAddress, U8, U16},
        collections::{EnumBytesRecord, LazyElementTypeValue, LazyRecord, RecordType},
        enum_types::EnumVariant,
        lazyrecord_types::{
            BmpMessage, LazyRecordTypeDef, PeerDownNotification,
            PeerUpNotification, RouteMonitoring, StatisticsReport, InitiationMessage,
        },
        typedef::{LazyNamedTypeDef, RecordTypeDef, TypeDef},
        typevalue::TypeValue,
    },
    vm::VmError,
};

pub use crate::types::collections::BytesRecord;

//------------ BmpMessage ---------------------------------------------------

createtoken!(
    BmpMessage;
    route_monitoring = 0
    statistics_report = 1
    peer_down_notification = 2
    peer_up_notification = 3
    initiation_message = 4
    termination_message = 5
    // route_mirroring = 6
);

impl BytesRecord<BmpMessage> {
    pub fn new(bytes: bytes::Bytes) -> Result<Self, VmError> {
        Ok(Self(
            routecore::bmp::message::Message::<bytes::Bytes>::from_octets(
                bytes,
            )
            .map_err(|_| VmError::InvalidMsgType)?,
        ))
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
            "InitiationMessage" => Ok((
                TypeDef::LazyRecord(LazyRecordTypeDef::InitiationMessage),
                Token::Variant(BmpMessageToken::InitiationMessage.into()),
            )),
            "RouteMonitoring" => Ok((
                TypeDef::LazyRecord(LazyRecordTypeDef::RouteMonitoring),
                Token::Variant(BmpMessageToken::RouteMonitoring.into()),
            )),
            "PeerUpNotification" => Ok((
                TypeDef::LazyRecord(LazyRecordTypeDef::PeerUpNotification),
                Token::Variant(BmpMessageToken::PeerUpNotification.into()),
            )),
            "PeerDownNotification" => Ok((
                TypeDef::LazyRecord(LazyRecordTypeDef::PeerDownNotification),
                Token::Variant(BmpMessageToken::PeerDownNotification.into()),
            )),
            "StatisticsReport" => Ok((
                TypeDef::LazyRecord(LazyRecordTypeDef::StatisticsReport),
                Token::Variant(BmpMessageToken::StatisticsReport.into()),
            )),
            "TerminationMessage" => Ok((
                TypeDef::LazyRecord(LazyRecordTypeDef::TerminationMessage),
                Token::Variant(BmpMessageToken::TerminationMessage.into()),
            )),
            name => Err(CompileError::from(format!(
                "No variant name {} for BmpMessage",
                name
            ))),
        }
    }
}

impl EnumBytesRecord for BytesRecord<BmpMessage> {
    fn get_variant(&self) -> LazyRecordTypeDef {
        trace!("this variant is {:?}", LazyRecordTypeDef::from(self.0.common_header().msg_type()));
        self.0.common_header().msg_type().into()
    }

    // Returns the typevalue for a variant and field_index on this
    // bytes_record. Returns a TypeValue::Unknown if the requested
    // variant does not match the bytes record. Returns an error if
    // no field_index was specified.
    fn get_field_index_for_variant(
        &self,
        variant_token: LazyRecordTypeDef,
        field_index: &SmallVec<[usize; 8]>,
    ) -> Result<TypeValue, VmError> {
        if field_index.is_empty() {
            return Err(VmError::InvalidMethodCall);
        }

        if variant_token != self.get_variant() {
            return Ok(TypeValue::Unknown);
        };

        let raw_bytes = self.0.as_ref();
        let lazy_rec: TypeValue = match variant_token {
            LazyRecordTypeDef::RouteMonitoring => {
                trace!("get_field_index_for_variant on Route Monitoring");
                trace!("field index {:?}", field_index);
                trace!(
                    "type def {:#?}",
                    &BytesRecord::<RouteMonitoring>::lazy_type_def()
                );
                let rm =
                    routecore::bmp::message::RouteMonitoring::from_octets(
                        bytes::Bytes::copy_from_slice(raw_bytes),
                    ).map_err(|_| VmError::InvalidPayload)?;
                LazyRecord::<RouteMonitoring>::new(BytesRecord::<
                    RouteMonitoring,
                >::lazy_type_def(
                ))
                .get_field_by_index(
                    field_index,
                    &BytesRecord::<RouteMonitoring>(rm),
                )
                .map(|elm| elm.into()).map_err(|_| VmError::InvalidPayload)?
            }
            LazyRecordTypeDef::PeerDownNotification => {
                let pd =
                    routecore::bmp::message::PeerDownNotification::from_octets(
                        bytes::Bytes::copy_from_slice(raw_bytes),
                    )
                    .map_err(|_| VmError::InvalidPayload)?;
                LazyRecord::<PeerDownNotification>::new(BytesRecord::<
                    PeerDownNotification,
                >::lazy_type_def(
                ))
                .get_field_by_index(
                    field_index,
                    &BytesRecord::<PeerDownNotification>(pd),
                )
                .map(|elm| elm.into()).map_err(|_| VmError::InvalidPayload)?
            }
            LazyRecordTypeDef::PeerUpNotification => {
                let pu =
                    routecore::bmp::message::PeerUpNotification::from_octets(
                        bytes::Bytes::copy_from_slice(raw_bytes),
                    )
                    .map_err(|_| VmError::InvalidPayload)?;
                LazyRecord::<PeerUpNotification>::new(BytesRecord::<
                    PeerUpNotification,
                >::lazy_type_def(
                ))
                .get_field_by_index(
                    field_index,
                    &BytesRecord::<PeerUpNotification>(pu),
                )
                .map(|elm| elm.into()).map_err(|_| VmError::InvalidPayload)?
            }
            LazyRecordTypeDef::InitiationMessage => {
                let pu =
                    routecore::bmp::message::InitiationMessage::from_octets(
                        bytes::Bytes::copy_from_slice(raw_bytes),
                    )
                    .map_err(|_| VmError::InvalidPayload)?;
                LazyRecord::<InitiationMessage>::new(BytesRecord::<
                    InitiationMessage,
                >::lazy_type_def(
                ))
                .get_field_by_index(
                    field_index,
                    &BytesRecord::<InitiationMessage>(pu),
                )
                .map(|elm| elm.into()).map_err(|_| VmError::InvalidPayload)?
            }
            LazyRecordTypeDef::StatisticsReport => {
                let pu =
                    routecore::bmp::message::StatisticsReport::from_octets(
                        bytes::Bytes::copy_from_slice(raw_bytes),
                    )
                    .map_err(|_| VmError::InvalidPayload)?;
                LazyRecord::<StatisticsReport>::new(BytesRecord::<
                    StatisticsReport,
                >::lazy_type_def(
                ))
                .get_field_by_index(
                    field_index,
                    &BytesRecord::<StatisticsReport>(pu),
                )
                .map(|elm| elm.into()).map_err(|_| VmError::InvalidPayload)?
            }
            _ => {
                return Err(VmError::InvalidMethodCall);
            }
        };

        Ok(lazy_rec)
    }

    fn is_variant(&self, variant_token: Token) -> bool {
        trace!("requested variant_token {:?}", variant_token);
        trace!("actual variant {:?}", self.get_variant());
        trace!("conversion {:?}", usize::from(self.get_variant()));
        if let Token::Variant(variant_index) = variant_token {
            variant_index == self.get_variant().into()
        } else {
            false
        }
    }
}

impl From<MessageType> for LazyRecordTypeDef {
    fn from(value: MessageType) -> Self {
        match value {
            MessageType::RouteMonitoring => {
                LazyRecordTypeDef::RouteMonitoring
            }
            MessageType::StatisticsReport => {
                LazyRecordTypeDef::StatisticsReport
            }
            MessageType::PeerDownNotification => {
                LazyRecordTypeDef::PeerDownNotification
            }
            MessageType::PeerUpNotification => {
                LazyRecordTypeDef::PeerUpNotification
            }
            MessageType::InitiationMessage => {
                LazyRecordTypeDef::InitiationMessage
            }
            MessageType::TerminationMessage => {
                LazyRecordTypeDef::TerminationMessage
            }
            MessageType::RouteMirroring => todo!(),
            MessageType::Unimplemented(_) => todo!(),
        }
    }
}


//------------ BmpRouteMonitoringMessage -------------------------------------

// THe fields of a bytes_record_impl should be STRICTLY alphabetically ordered
// by the the name of the key and numbered in that order.

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
    )],
    10
);

impl BytesRecord<RouteMonitoring> {
    pub fn new(bytes: bytes::Bytes) -> Result<Self, VmError> {
        if let routecore::bmp::message::Message::RouteMonitoring(rm_msg) =
            routecore::bmp::message::Message::<bytes::Bytes>::from_octets(
                bytes,
            )
            .map_err(|_| VmError::InvalidPayload)?
        {
            Ok(Self(rm_msg))
        } else {
            Err(VmError::InvalidMsgType)
        }
    }
}

//------------ PeerUpNotification -------------------------------------------

// THe fields of a bytes_record_impl should be STRICTLY alphabetically
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
            field("asn"; 5, Asn, per_peer_header.asn),
            field("is_ipv4"; 6, Boolean, per_peer_header.is_ipv4),
            field("is_ipv6"; 7, Boolean, per_peer_header.is_ipv6),
            field(
                "is_legacy_format"; 8,
                Boolean,
                per_peer_header.is_legacy_format
            ),
            field(
                "is_post_policy"; 9,
                Boolean,
                per_peer_header.is_post_policy
            ),
            field(
                "is_pre_policy"; 10,
                Boolean,
                per_peer_header.is_pre_policy
            ),
            enum_field(
                "peer_type"; 11,
                EnumVariant<U8> = "BMP_PEER_TYPE",
                BytesRecord<PeerUpNotification>,
                per_peer_header.peer_type
            ),
        ),
        field(
            "remote_port"; 12,
            U16,
            remote_port
        ),
        record_field(
            "session_config"; 13,
            field(
                "has_four_octet_asn"; 14,
                Boolean,
                session_config.has_four_octet_asn
            ),
        ),
    )],
    14
);

impl BytesRecord<PeerUpNotification> {
    pub fn new(bytes: bytes::Bytes) -> Result<Self, VmError> {
        if let routecore::bmp::message::Message::PeerUpNotification(pu_msg) =
            routecore::bmp::message::Message::<bytes::Bytes>::from_octets(
                bytes,
            )
            .map_err(|_| VmError::InvalidPayload)?
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
                BytesRecord<PeerDownNotification>,
                per_peer_header.peer_type
            ),
        ),
    )],
    10
);

impl BytesRecord<PeerDownNotification> {
    pub fn new(bytes: bytes::Bytes) -> Result<Self, VmError> {
        if let routecore::bmp::message::Message::PeerDownNotification(
            pd_msg,
        ) = routecore::bmp::message::Message::<bytes::Bytes>::from_octets(
            bytes,
        )
        .map_err(|_| VmError::InvalidPayload)?
        {
            Ok(Self(pd_msg))
        } else {
            Err(VmError::InvalidMsgType)
        }
    }
}

//------------ InitiationMessage --------------------------------------------

bytes_record_impl!(
    InitiationMessage,
    #[type_def(
        record_field(
            "common_header"; 0,
            field("version"; 1, U8, common_header.version),
        ),
    )],
    2
);

impl BytesRecord<InitiationMessage> {
    pub fn new(bytes: bytes::Bytes) -> Result<Self, VmError> {
        if let routecore::bmp::message::Message::InitiationMessage(
            pd_msg,
        ) = routecore::bmp::message::Message::<bytes::Bytes>::from_octets(
            bytes,
        )
        .map_err(|_| VmError::InvalidPayload)?
        {
            Ok(Self(pd_msg))
        } else {
            Err(VmError::InvalidMsgType)
        }
    }
}

//------------ StatisticsReport ---------------------------------------------

bytes_record_impl!(
    StatisticsReport,
    #[type_def(
        record_field(
            "per_peer_header"; 0,
            field("address"; 1, IpAddress, per_peer_header.address),
            enum_field(
                "adj_rib_type"; 2,
                EnumVariant<U8> = "BMP_ADJ_RIB_TYPE",
                BytesRecord<StatisticsReport>,
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
                BytesRecord<StatisticsReport>,
                per_peer_header.peer_type
            ),
        ),
    )],
    10
);

impl BytesRecord<StatisticsReport> {
    pub fn new(bytes: bytes::Bytes) -> Result<Self, VmError> {
        if let routecore::bmp::message::Message::StatisticsReport(sr_msg) =
            routecore::bmp::message::Message::<bytes::Bytes>::from_octets(
                bytes,
            )
            .map_err(|_| VmError::InvalidPayload)?
        {
            Ok(Self(sr_msg))
        } else {
            Err(VmError::InvalidMsgType)
        }
    }
}
