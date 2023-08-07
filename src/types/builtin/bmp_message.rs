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
        collections::{LazyElementTypeValue, LazyRecord, EnumBytesRecord},
        enum_types::EnumVariant,
        lazytypedef::{
            BmpMessage, LazyRecordTypeDef, PeerDownNotification,
            PeerUpNotification, RouteMonitoring,
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
    // statistics_report = 1
    peer_down_notification = 2
    peer_up_notification = 3
    // initiation_message = 4
    // termination_message = 5
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

impl EnumBytesRecord for BytesRecord<BmpMessage> {
    fn get_variant(&self) -> LazyRecordTypeDef {
        self.0.common_header().msg_type().into()
    }
}

impl From<MessageType> for LazyRecordTypeDef {
    fn from(value: MessageType) -> Self {
        match value {
            MessageType::RouteMonitoring => LazyRecordTypeDef::RouteMonitoring,
            MessageType::StatisticsReport => LazyRecordTypeDef::StatisticsReport,
            MessageType::PeerDownNotification => LazyRecordTypeDef::PeerDownNotification,
            MessageType::PeerUpNotification => LazyRecordTypeDef::PeerUpNotification,
            MessageType::InitiationMessage => todo!(),
            MessageType::TerminationMessage => todo!(),
            MessageType::RouteMirroring => todo!(),
            MessageType::Unimplemented(_) => todo!(),
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

impl EnumBytesRecord for BytesRecord<RouteMonitoring> {
    fn get_variant(&self) -> LazyRecordTypeDef {
        self.0.common_header().msg_type().into()
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

impl EnumBytesRecord for BytesRecord<PeerUpNotification> {
    fn get_variant(&self) -> LazyRecordTypeDef {
        self.0.common_header().msg_type().into()
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

impl EnumBytesRecord for BytesRecord<PeerDownNotification> {
    fn get_variant(&self) -> LazyRecordTypeDef {
        self.0.common_header().msg_type().into()
    }
}
