use log::trace;
use routecore::bmp::message::PerPeerHeader;
use serde::Serialize;

use super::{
    typedef::{MethodProps, NamedTypeDef, TypeDef}
};
use crate::{
    ast::Identifier, compile::CompileError, traits::Token, types::builtin::BytesRecord,
};

pub type RouteMonitoring = routecore::bmp::message::RouteMonitoring<bytes::Bytes>;
pub type PeerUpNotification =
    routecore::bmp::message::PeerUpNotification<bytes::Bytes>;

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize)]
pub enum LazyTypeDef {
    BmpRouteMonitoringMessage,
    BmpPeerUpNotificationMessage,
    BmpPeerDownNotificationMessage,
    BmpStatisticsReport,
    BmpRouteMirroringMessage,
    BmpPerPeerHeader
}

impl LazyTypeDef {
    pub fn type_def(&self) -> Vec<NamedTypeDef> {
        match &self {
            LazyTypeDef::BmpRouteMonitoringMessage => 
                BytesRecord::<RouteMonitoring>::type_def(),
            LazyTypeDef::BmpPeerUpNotificationMessage => 
                BytesRecord::<PeerUpNotification>::type_def(),
            LazyTypeDef::BmpPeerDownNotificationMessage => todo!(),
            LazyTypeDef::BmpStatisticsReport => todo!(),
            LazyTypeDef::BmpRouteMirroringMessage => todo!(),
            LazyTypeDef::BmpPerPeerHeader => BytesRecord::<PerPeerHeader<bytes::Bytes>>::type_def()
        }
    }

    pub(crate) fn get_props_for_method(
        &self,
        ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError> {
        match self {
            LazyTypeDef::BmpRouteMonitoringMessage => BytesRecord::<
                RouteMonitoring,
            >::get_props_for_method(
                ty, method_name
            ),
            LazyTypeDef::BmpPeerUpNotificationMessage => todo!(),
            LazyTypeDef::BmpPeerDownNotificationMessage => todo!(),
            LazyTypeDef::BmpStatisticsReport => todo!(),
            LazyTypeDef::BmpRouteMirroringMessage => todo!(),
            LazyTypeDef::BmpPerPeerHeader => Err(CompileError::from("Record 'PerPeerHeader' doesn't have methods"))
        }
    }

    pub(crate) fn get_props_for_field(
        &self,
        field: &Identifier,
    ) -> Result<(TypeDef, Token), CompileError> {
        match self {
            LazyTypeDef::BmpRouteMonitoringMessage => {
                trace!("BmpRouteMonitoringMessage w/ field '{}'", field);
                BytesRecord::<RouteMonitoring>::get_props_for_field(field)
            }
            LazyTypeDef::BmpPeerUpNotificationMessage => {
                trace!("BmpPeerUpNotificationMessage w/ field '{}'", field);
                BytesRecord::<PeerUpNotification>::get_props_for_field(field)
            },
            LazyTypeDef::BmpPeerDownNotificationMessage => todo!(),
            LazyTypeDef::BmpStatisticsReport => todo!(),
            LazyTypeDef::BmpRouteMirroringMessage => todo!(),
            LazyTypeDef::BmpPerPeerHeader => {
                trace!("BmpPerPeerHeader w/ field '{}'", field);
                BytesRecord::<PerPeerHeader<bytes::Bytes>>::get_props_for_field(field)
            }
       }
    }
}
