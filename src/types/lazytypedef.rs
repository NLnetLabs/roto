use log::trace;
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
// pub type PerPeerHeader = routecore::bmp::message::PerPeerHeader<bytes::Bytes>;

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize)]
pub enum LazyTypeDef {
    RouteMonitoring,
    PeerUpNotification,
    PeerDownNotification,
    StatisticsReport,
    RouteMirroring,
    // BmpPerPeerHeader
}

impl LazyTypeDef {
    pub fn type_def(&self) -> Vec<NamedTypeDef> {
        match &self {
            LazyTypeDef::RouteMonitoring => 
                BytesRecord::<RouteMonitoring>::type_def(),
            LazyTypeDef::PeerUpNotification => 
                BytesRecord::<PeerUpNotification>::type_def(),
            LazyTypeDef::PeerDownNotification => todo!(),
            LazyTypeDef::StatisticsReport => todo!(),
            LazyTypeDef::RouteMirroring => todo!(),
            // LazyTypeDef::BmpPerPeerHeader => BytesRecord::<PerPeerHeader>::type_def()
        }
    }

    pub(crate) fn get_props_for_method(
        &self,
        ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError> {
        match self {
            LazyTypeDef::RouteMonitoring => BytesRecord::<
                RouteMonitoring,
            >::get_props_for_method(
                ty, method_name
            ),
            LazyTypeDef::PeerUpNotification => todo!(),
            LazyTypeDef::PeerDownNotification => todo!(),
            LazyTypeDef::StatisticsReport => todo!(),
            LazyTypeDef::RouteMirroring => todo!(),
            // LazyTypeDef::BmpPerPeerHeader => Err(CompileError::from("Record 'PerPeerHeader' doesn't have methods"))
        }
    }

    pub(crate) fn get_props_for_field(
        &self,
        field: &Identifier,
    ) -> Result<(TypeDef, Token), CompileError> {
        match self {
            LazyTypeDef::RouteMonitoring => {
                trace!("BmpRouteMonitoringMessage w/ field '{}'", field);
                BytesRecord::<RouteMonitoring>::get_props_for_field(field)
            }
            LazyTypeDef::PeerUpNotification => {
                trace!("BmpPeerUpNotificationMessage w/ field '{}'", field);
                BytesRecord::<PeerUpNotification>::get_props_for_field(field)
            },
            LazyTypeDef::PeerDownNotification => todo!(),
            LazyTypeDef::StatisticsReport => todo!(),
            LazyTypeDef::RouteMirroring => todo!(),
            // LazyTypeDef::BmpPerPeerHeader => {
            //     trace!("BmpPerPeerHeader w/ field '{}'", field);
            //     BytesRecord::<PerPeerHeader>::get_props_for_field(field)
            // }
       }
    }
}
