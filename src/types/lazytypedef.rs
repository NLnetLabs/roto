use std::sync::Arc;

use log::trace;
use serde::Serialize;

use super::typedef::{MethodProps, TypeDef, RecordTypeDef};
use crate::{
    ast::Identifier, compile::CompileError, traits::Token,
    types::builtin::BytesRecord,
};

pub type BmpMessage =
    routecore::bmp::message::Message<bytes::Bytes>;
pub type RouteMonitoring =
    routecore::bmp::message::RouteMonitoring<bytes::Bytes>;
pub type PeerUpNotification =
    routecore::bmp::message::PeerUpNotification<bytes::Bytes>;
pub type PeerDownNotification =
    routecore::bmp::message::PeerDownNotification<bytes::Bytes>;

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize)]
pub enum LazyTypeDef {
    BmpMessage,
    RouteMonitoring,
    PeerUpNotification,
    PeerDownNotification,
    StatisticsReport,
    RouteMirroring,
}

impl LazyTypeDef {
    pub fn type_def(&self) -> RecordTypeDef {
        match &self {
            LazyTypeDef::BmpMessage => {
                BytesRecord::<BmpMessage>::type_def()
            }
            LazyTypeDef::RouteMonitoring => {
                BytesRecord::<RouteMonitoring>::type_def()
            }
            LazyTypeDef::PeerUpNotification => {
                BytesRecord::<PeerUpNotification>::type_def()
            }
            LazyTypeDef::PeerDownNotification => {
                BytesRecord::<PeerDownNotification>::type_def()
            }
            LazyTypeDef::StatisticsReport => todo!(),
            LazyTypeDef::RouteMirroring => todo!(),
        }
    }

    pub(crate) fn get_props_for_method(
        &self,
        ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError> {
        match self {
            LazyTypeDef::BmpMessage => {
                BytesRecord::<BmpMessage>::get_props_for_method(
                    ty,
                    method_name,
                )
            }
            LazyTypeDef::RouteMonitoring => {
                BytesRecord::<RouteMonitoring>::get_props_for_method(
                    ty,
                    method_name,
                )
            }
            LazyTypeDef::PeerUpNotification => {
                BytesRecord::<PeerUpNotification>::get_props_for_method(
                    ty,
                    method_name,
                )
            },
            LazyTypeDef::PeerDownNotification => {
                BytesRecord::<PeerDownNotification>::get_props_for_method(
                    ty,
                    method_name,
                )
            },
            LazyTypeDef::StatisticsReport => todo!(),
            LazyTypeDef::RouteMirroring => todo!(),
        }
    }

    pub(crate) fn get_props_for_field(
        &self,
        field: &Identifier,
    ) -> Result<(TypeDef, Token), CompileError> {
        match self {
            LazyTypeDef::BmpMessage => {
                trace!("BmpMessage w/ field '{}'", field);
                BytesRecord::<BmpMessage>::get_props_for_field(field)
            }
            LazyTypeDef::RouteMonitoring => {
                trace!("BmpRouteMonitoring w/ field '{}'", field);
                BytesRecord::<RouteMonitoring>::get_props_for_field(field)
            }
            LazyTypeDef::PeerUpNotification => {
                trace!("BmpPeerUpNotification w/ field '{}'", field);
                BytesRecord::<PeerUpNotification>::get_props_for_field(field)
            }
            LazyTypeDef::PeerDownNotification => {
                trace!("BmpPeerDownNotification w/ field '{}'", field);
                BytesRecord::<PeerDownNotification>::get_props_for_field(
                    field,
                )
            }
            LazyTypeDef::StatisticsReport => todo!(),
            LazyTypeDef::RouteMirroring => todo!(),
        }
    }
}

impl From<LazyTypeDef> for Box<TypeDef> {
    fn from(value: LazyTypeDef) -> Self {
        TypeDef::Record(value.type_def()).into()
    }
}

impl PartialEq<RecordTypeDef> for LazyTypeDef {
    fn eq(&self, _other: &RecordTypeDef) -> bool {
        todo!()
    }
}

impl PartialEq<Box<TypeDef>> for LazyTypeDef {
    fn eq(&self, other: &Box<TypeDef>) -> bool {
        if let TypeDef::Record(rec_def) = &**other {
            rec_def == &self.type_def()
        } else {
            false
        }
    }
}