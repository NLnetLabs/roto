use log::trace;
use paste::paste;

use crate::{
    ast::ShortString,
    compile::CompileError,
    createtoken, lazyelmtypevalue, lazyenum, lazyfield, lazyrecord,
    bytes_record_impl,
    traits::Token,
    types::{
        builtin::{
            Asn, Boolean, BuiltinTypeValue, IpAddress, U16,
        },
        collections::{LazyElementTypeValue, LazyRecord},
        constant_enum::EnumVariant,
        lazytypedef::{
            LazyTypeDef, PeerUpNotification, RouteMonitoring,
        },
        typedef::{LazyNamedTypeDef, NamedTypeDef, TypeDef},
        typevalue::TypeValue,
    },
    vm::VmError,
};

pub use crate::types::collections::BytesRecord;

//------------ BmpRouteMonitoringMessage ------------------------------------

createtoken!(
    BmpMessage;
    route_monitoring = 0
    statistics_report = 1
    peer_down_notification = 2
    peer_up_notification = 3
    initiation_message = 4
    termination_message = 5
    route_mirroring = 6
);

bytes_record_impl!(
    RouteMonitoring,
    #[type_def(
        record_field(
            "per_peer_header"; 0,
            field("is_ipv4"; 1, Boolean, per_peer_header.is_ipv4),
            field("is_ipv6"; 2, Boolean, per_peer_header.is_ipv6),
            field(
                "is_pre_policy"; 3,
                Boolean,
                per_peer_header.is_pre_policy
            ),
            field(
                "is_post_policy"; 4,
                Boolean,
                per_peer_header.is_post_policy
            ),
            field(
                "is_legacy_format"; 5,
                Boolean,
                per_peer_header.is_legacy_format
            ),
            field("address"; 6, IpAddress, per_peer_header.address),
            field("asn"; 7, Asn, per_peer_header.asn),
            enum_field(
                "peer_type"; 8,
                EnumVariant<U8> = "BMP_PEER_TYPE",
                BytesRecord<RouteMonitoring>,
                per_peer_header.peer_type
            ),
            enum_field(
                "adj_rib_type"; 9,
                EnumVariant<U8> = "BMP_ADJ_RIB_TYPE",
                BytesRecord<RouteMonitoring>,
                per_peer_header.adj_rib_type
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

//------------ BmpPeerUpNotificationMessage ---------------------------------

bytes_record_impl!(
    PeerUpNotification,
    #[type_def(
        record_field(
            "per_peer_header"; 0,
            field("is_ipv4"; 1, Boolean, per_peer_header.is_ipv4),
            field("is_ipv6"; 2, Boolean, per_peer_header.is_ipv6),
            field(
                "is_pre_policy"; 3,
                Boolean,
                per_peer_header.is_pre_policy
            ),
            field(
                "is_post_policy"; 4,
                Boolean,
                per_peer_header.is_post_policy
            ),
            field(
                "is_legacy_format"; 5,
                Boolean,
                per_peer_header.is_legacy_format
            ),
            field("address"; 6, IpAddress, per_peer_header.address),
            enum_field(
                "peer_type"; 7,
                EnumVariant<U8> = "BMP_PEER_TYPE",
                BytesRecord<PeerUpNotification>,
                per_peer_header.peer_type
            ),
            enum_field(
                "adj_rib_type"; 8,
                EnumVariant<U8> = "BMP_ADJ_RIB_TYPE",
                BytesRecord<PeerUpNotification>,
                per_peer_header.adj_rib_type
            ),
        ),
        record_field(
            "session_config"; 12,
            field(
                "has_four_octet_asn"; 13, 
                Boolean, 
                session_config.has_four_octet_asn
            ),
        ),
        field(
            "local_address"; 9,
            IpAddress,
            local_address
        ),
        field(
            "local_port"; 10,
            U16,
            local_port
        ),
        field(
            "remote_port"; 11,
            U16,
            remote_port
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

//------------ PerPeerHeader ------------------------------------------------

// subrecord_impl!(
//     PerPeerHeader,
//     "per_peer_header",
//     {
//         ("is_ipv4"; 0, Boolean),
//         ("is_ipv6"; 1, Boolean),
//         (
//             "is_pre_policy"; 2,
//             Boolean
//         ),
//         (
//             "is_post_policy"; 3,
//             Boolean
//         ),
//         (
//             "is_legacy_format"; 4,
//             Boolean
//         ),
//         ("address"; 5, IpAddress),
//     },
//     {
//         (
//             "peer_type"; 6,
//             EnumVariant<U8> = "BMP_PEER_TYPE"
//         ),
//         (
//             "adj_rib_type"; 7,
//             EnumVariant<U8> = "BMP_ADJ_RIB_TYPE"
//         ),
//     }
// );
