use log::trace;
use serde::Serialize;

use super::typedef::{MethodProps, TypeDef, RecordTypeDef};
use crate::{
    ast::Identifier, compile::CompileError, traits::Token,
    types::builtin::BytesRecord,
};

pub type BmpMessage =
    routecore::bmp::message::Message<bytes::Bytes>;
pub type InitiationMessage =
    routecore::bmp::message::InitiationMessage<bytes::Bytes>;
pub type StatisticsReport =
    routecore::bmp::message::StatisticsReport<bytes::Bytes>;
pub type RouteMonitoring =
    routecore::bmp::message::RouteMonitoring<bytes::Bytes>;
pub type PeerUpNotification =
    routecore::bmp::message::PeerUpNotification<bytes::Bytes>;
pub type PeerDownNotification =
    routecore::bmp::message::PeerDownNotification<bytes::Bytes>;

// This is the complete enumeration of all Lazy Record types available to
// roto users. Note that this does *NOT* include BgpMessage, which is a
// BytesRecord, by it's also an Enum, therefore it contains a Lazy Record,
// but it isn't one itself.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize)]
pub enum LazyRecordTypeDef {
    InitiationMessage,
    RouteMonitoring,
    StatisticsReport,
    PeerUpNotification,
    PeerDownNotification,
    RouteMirroring,
}

impl LazyRecordTypeDef {
    pub fn type_def(&self) -> RecordTypeDef {
        match &self {
            LazyRecordTypeDef::InitiationMessage => todo!(),
            LazyRecordTypeDef::RouteMonitoring => {
                BytesRecord::<RouteMonitoring>::type_def()
            }
            LazyRecordTypeDef::StatisticsReport => todo!(),
            LazyRecordTypeDef::PeerUpNotification => {
                BytesRecord::<PeerUpNotification>::type_def()
            }
            LazyRecordTypeDef::PeerDownNotification => {
                BytesRecord::<PeerDownNotification>::type_def()
            }
            LazyRecordTypeDef::RouteMirroring => todo!(),
        }
    }

    pub(crate) fn get_props_for_method(
        &self,
        ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError> {
        match self {
            LazyRecordTypeDef::InitiationMessage => todo!(),
            LazyRecordTypeDef::RouteMonitoring => {
                BytesRecord::<RouteMonitoring>::get_props_for_method(
                    ty,
                    method_name,
                )
            }
            LazyRecordTypeDef::StatisticsReport => todo!(),
            LazyRecordTypeDef::PeerUpNotification => {
                BytesRecord::<PeerUpNotification>::get_props_for_method(
                    ty,
                    method_name,
                )
            },
            LazyRecordTypeDef::PeerDownNotification => {
                BytesRecord::<PeerDownNotification>::get_props_for_method(
                    ty,
                    method_name,
                )
            },
            LazyRecordTypeDef::RouteMirroring => todo!(),
        }
    }

    pub(crate) fn get_props_for_field(
        &self,
        field: &Identifier,
    ) -> Result<(TypeDef, Token), CompileError> {
        match self {
            LazyRecordTypeDef::InitiationMessage => todo!(),
            LazyRecordTypeDef::RouteMonitoring => {
                trace!("BmpRouteMonitoring w/ field '{}'", field);
                BytesRecord::<RouteMonitoring>::get_props_for_field(field)
            }
            LazyRecordTypeDef::StatisticsReport => todo!(),
            LazyRecordTypeDef::PeerUpNotification => {
                trace!("BmpPeerUpNotification w/ field '{}'", field);
                BytesRecord::<PeerUpNotification>::get_props_for_field(field)
            }
            LazyRecordTypeDef::PeerDownNotification => {
                trace!("BmpPeerDownNotification w/ field '{}'", field);
                BytesRecord::<PeerDownNotification>::get_props_for_field(
                    field,
                )
            }
            LazyRecordTypeDef::RouteMirroring => todo!(),
        }
    }
}

impl From<LazyRecordTypeDef> for Box<TypeDef> {
    fn from(value: LazyRecordTypeDef) -> Self {
        TypeDef::Record(value.type_def()).into()
    }
}

impl PartialEq<RecordTypeDef> for LazyRecordTypeDef {
    fn eq(&self, _other: &RecordTypeDef) -> bool {
        todo!()
    }
}

impl PartialEq<Box<TypeDef>> for LazyRecordTypeDef {
    fn eq(&self, other: &Box<TypeDef>) -> bool {
        if let TypeDef::Record(rec_def) = &**other {
            rec_def == &self.type_def()
        } else {
            false
        }
    }
}

impl std::fmt::Display for LazyRecordTypeDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LazyRecordTypeDef::InitiationMessage => write!(f, "InitiationMessage"),
            LazyRecordTypeDef::RouteMonitoring => write!(f, "RouteMonitoring"),
            LazyRecordTypeDef::StatisticsReport => write!(f, "StatiscticsReport"),
            LazyRecordTypeDef::PeerUpNotification => write!(f, "PeerUpNotification"),
            LazyRecordTypeDef::PeerDownNotification => write!(f, "PeerDownNotification"),
            LazyRecordTypeDef::RouteMirroring => write!(f, "RouteMirroring"),
        }
    }
}

impl From<LazyRecordTypeDef> for usize {
    fn from(value: LazyRecordTypeDef) -> Self {
        match value {
            LazyRecordTypeDef::RouteMonitoring => 0,
            LazyRecordTypeDef::StatisticsReport => 1,
            LazyRecordTypeDef::PeerUpNotification => 2,
            LazyRecordTypeDef::PeerDownNotification => 3,
            LazyRecordTypeDef::RouteMirroring => 4,
            LazyRecordTypeDef::InitiationMessage => 5,
        }
    }
}

impl From<usize> for LazyRecordTypeDef {
    fn from(value: usize) -> Self {
        match value {
            0 => LazyRecordTypeDef::RouteMonitoring,
            1 => LazyRecordTypeDef::StatisticsReport,
            2 => LazyRecordTypeDef::PeerDownNotification,
            3 => LazyRecordTypeDef::PeerUpNotification,
            4 => LazyRecordTypeDef::RouteMonitoring,
            5 => LazyRecordTypeDef::InitiationMessage,
            _ => unimplemented!()
        }
    }
}