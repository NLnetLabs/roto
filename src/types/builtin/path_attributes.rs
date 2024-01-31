use std::net::IpAddr;
use std::net::SocketAddr;

use chrono::Utc;
use serde::Deserialize;
use std::fmt::{Display, Formatter};

use log::{debug, error, trace};
use paste::paste;
use serde::Serialize;

use crate::attr_change_set::ScalarValue;
use crate::compiler::compile::CompileError;
use crate::traits::RotoType;
use crate::types::typedef::MethodProps;
use crate::types::typedef::RecordTypeDef;
use crate::vm::{StackValue, VmError};
use crate::{
    ast,
    attr_change_set::PathAttributeSet,
    traits::{self, Token},
    types::lazyrecord_types::BgpUpdateMessage,
    vm::FieldIndex,
};
use crate::{
    createtoken, intotype, minimalscalartype, setmethodonly,
    typevaluefromimpls,
};

use super::super::typevalue::TypeValue;
use super::builtin_type_value::BuiltinTypeValue;
use super::{
    super::typedef::TypeDef, BytesRecord, Nlri,
    NlriStatus
};

use routecore::addr::Prefix;
use routecore::asn::Asn;
use routecore::bgp::message::nlri::PathId;
use routecore::bgp::types::AfiSafi;

pub type LogicalTime = u64;


//------------ MutableNlri ---------------------------------------------------

#[derive(Debug, Clone, Eq, PartialEq, Serialize)]
#[serde(into = "MaterializedRoute2")]
pub struct MutableNlri {
    #[serde(skip)]
    message: BytesRecord<BgpUpdateMessage>,
    #[serde(skip)]
    provenance: Provenance,
    nlri: Nlri,
    status: NlriStatus,
    changes: Option<PathAttributeSet>,
}

impl MutableNlri {
    // Get either the moved last delta (it is removed from the Delta list), or
    // get a freshly rolled ChangeSet from the raw message, if there are no
    // deltas.
    pub fn take_attrs(mut self) -> Result<PathAttributeSet, VmError> {
        match &mut self.changes {
            None => self
                .message
                .create_path_attributes_set(self.nlri.afi_safi()),
            Some(attr_list) => Ok(std::mem::take(attr_list)),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Serialize)]
pub struct MaterializedRoute2 {
    pub route: Option<PathAttributeSet>,
    pub status: NlriStatus,
    route_id: (RotondaId, LogicalTime),
}

impl From<MutableNlri> for MaterializedRoute2 {
    fn from(raw_nlri: MutableNlri) -> Self {
        let status = raw_nlri.status;
        let route_id = (RotondaId(0), 0);

        if let Ok(route) = raw_nlri.take_attrs() {
            MaterializedRoute2 {
                route: Some(route),
                status,
                route_id,
            }
        } else {
            MaterializedRoute2 {
                route: None,
                status: NlriStatus::Unparsable,
                route_id,
            }
        }
    }
}

//------------ BasicRoute ----------------------------------------------------

// BasicRoute is the variant of routecore's `Nlri` enum that only has a
// `BasicNlri` struct as its data field. These variants are Unicast,
// Multicast. This is mainly for convenience of the Roto user so they don't
// have to any pattern matching to get to the properties of this BasicNlri,
// like prefix and path_id. Instead, the prefix and path_id fields just live
// in the root of the BasicRoute. To the Roto user this appears as a normal
// `Route` object. Another implication of the roto user using Route as the
// type for `rx` is that the filter that has this will return a `reject` for
// all other variants. E.g., if a Nlri that unpacks as the FlowSpec variant,
// it will be rejected right off the bat.

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
#[serde(into = "MaterializedRoute2")]
pub struct BasicRoute {
    #[serde(skip)]
    message: BytesRecord<BgpUpdateMessage>,
    provenance: Provenance,
    prefix: Prefix,
    path_id: Option<PathId>,
    afi_safi: AfiSafi,
    status: NlriStatus,
    changes: Option<Box<PathAttributeSet>>,
}

impl BasicRoute {
    pub fn new(
        prefix: Prefix,
        message: BytesRecord<BgpUpdateMessage>,
        afi_safi: routecore::bgp::types::AfiSafi,
        path_id: Option<routecore::bgp::message::nlri::PathId>,
        status: NlriStatus,
        provenance: Provenance,
    ) -> Result<Self, VmError> {
        Ok(Self {
            prefix,
            message,
            afi_safi,
            path_id,
            changes: None,
            status,
            provenance,
        })
    }

    pub fn provenance(&self) -> &Provenance {
        &self.provenance
    }

    pub fn prefix(&self) -> Prefix {
        self.prefix
    }

    pub fn afi_safi(&self) -> AfiSafi {
        self.afi_safi
    }

    pub fn message(&self) -> &BytesRecord<BgpUpdateMessage> {
        &self.message
    }

    // Get either the moved last delta (it is removed from the Delta list), or
    // get a freshly rolled ChangeSet from the raw message, if there are no
    // deltas.
    pub fn take_attrs(mut self) -> Result<PathAttributeSet, VmError> {
        match &mut self.changes {
            None => self.message.create_path_attributes_set(self.afi_safi),
            Some(attr_list) => Ok(std::mem::take(attr_list)),
        }
    }

    pub fn store_attrs(&mut self, path_attribues: PathAttributeSet) {
        self.changes = Some(Box::new(path_attribues))
    }

    pub fn get_attrs(&mut self) -> Result<&Option<Box<PathAttributeSet>>, VmError> {
        match &self.changes {
            None => { 
                self.changes = Some(Box::new(self.message.create_path_attributes_set(self.afi_safi)?));
                Ok(&self.changes)
            },
            Some(_) => Ok(&self.changes)
        }
    }

    pub fn get_attrs_mut(&mut self) -> Result<&mut Option<Box<PathAttributeSet>>, VmError> {
        match &mut self.changes {
            None => { 
                self.changes = Some(Box::new(self.message.create_path_attributes_set(self.afi_safi)?));
                Ok(&mut self.changes)
            },
            Some(_) => Ok(&mut self.changes)
        }
    }

    pub fn get_field_by_index(
        &self,
        field_index: &FieldIndex,
    ) -> Result<TypeValue, VmError> {
        match field_index.first()?.try_into()? {
            BasicRouteToken::AsPath => self
                .message
                .bytes_parser()
                .aspath()
                .ok()
                .flatten()
                .map(|p| p.to_hop_path())
                .map(TypeValue::from)
                .ok_or(VmError::InvalidFieldAccess),
            BasicRouteToken::OriginType => self
                .message
                .bytes_parser()
                .origin()
                .ok()
                .flatten()
                .map(TypeValue::from)
                .ok_or(VmError::InvalidFieldAccess),
            BasicRouteToken::NextHop => self
                .message
                .bytes_parser()
                .find_next_hop(self.afi_safi)
                .map_err(|_| VmError::InvalidFieldAccess)
                .map(TypeValue::from),
            BasicRouteToken::MultiExitDisc => self
                .message
                .bytes_parser()
                .multi_exit_disc()
                .ok()
                .flatten()
                .map(TypeValue::from)
                .ok_or(VmError::InvalidFieldAccess),
            BasicRouteToken::LocalPref => self
                .message
                .bytes_parser()
                .local_pref()
                .ok()
                .flatten()
                .map(TypeValue::from)
                .ok_or(VmError::InvalidFieldAccess),
            BasicRouteToken::AtomicAggregate => Some(TypeValue::from(
                self.message
                    .bytes_parser()
                    .is_atomic_aggregate()
                    .unwrap_or(false),
            ))
            .ok_or(VmError::InvalidFieldAccess),
            BasicRouteToken::Aggregator => self
                .message
                .bytes_parser()
                .aggregator()
                .ok()
                .flatten()
                .map(TypeValue::from)
                .ok_or(VmError::InvalidFieldAccess),
            BasicRouteToken::Communities => self
                .message
                .bytes_parser()
                .all_human_readable_communities()
                .ok()
                .flatten()
                .map(TypeValue::from)
                .ok_or(VmError::InvalidFieldAccess),
            BasicRouteToken::Prefix => Ok(self.prefix.into()),
            BasicRouteToken::Status => Ok(self.status.into()),
            BasicRouteToken::Provenance => {
                if !field_index.is_empty() {
                    trace!("get field_index {:?} on provenance", field_index);
                    self.provenance
                        .get_field_by_index(field_index.skip_first())
                } else {
                    Ok(self.provenance.into())
                }
            }
        }
    }

    pub(crate) fn get_value_ref_for_field(
        &self,
        field_token: usize,
    ) -> Result<Option<&TypeValue>, VmError> {
        let current_set = if let Some(atrd) = &self.changes {
            atrd
        } else {
            return Err(VmError::InvalidRecord);
        };

        match field_token.try_into()? {
            BasicRouteToken::AsPath => Ok(current_set.as_path.as_ref()),
            BasicRouteToken::OriginType => {
                Ok(current_set.origin_type.as_ref())
            }
            BasicRouteToken::NextHop => Ok(current_set.next_hop.as_ref()),
            BasicRouteToken::MultiExitDisc => {
                Ok(current_set.multi_exit_discriminator.as_ref())
            }
            BasicRouteToken::LocalPref => Ok(current_set.local_pref.as_ref()),
            BasicRouteToken::AtomicAggregate => {
                Ok(current_set.atomic_aggregate.as_ref())
            }
            BasicRouteToken::Aggregator => {
                Ok(current_set.aggregator.as_ref())
            }
            BasicRouteToken::Communities => {
                Ok(current_set.communities.as_ref())
            }
            BasicRouteToken::Prefix => Err(VmError::InvalidCommandArg),
            BasicRouteToken::Status => Err(VmError::InvalidCommandArg),
            BasicRouteToken::Provenance => Err(VmError::InvalidCommandArg),
        }
    }
}

impl From<BasicRoute> for MaterializedRoute2 {
    fn from(raw_route: BasicRoute) -> Self {
        let status = raw_route.status;
        let route_id = (RotondaId(0), 0);

        if let Ok(route) = raw_route.take_attrs() {
            MaterializedRoute2 {
                route: Some(route),
                status,
                route_id,
            }
        } else {
            MaterializedRoute2 {
                route: None,
                status: NlriStatus::Unparsable,
                route_id,
            }
        }
    }
}

impl std::fmt::Display for BasicRoute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "prefix  : {}", self.prefix)?;
        writeln!(f, "raw     : {:#?}", self.message)?;
        writeln!(f, "changes : {:#?}", self.changes)?;
        writeln!(f, "status  : {:?}", self.status)
    }
}

impl BasicRoute {
    pub(crate) fn get_field_num() -> usize {
        todo!()
    }
    pub(crate) fn type_def() -> RecordTypeDef {
        todo!()
    }

    pub(crate) fn get_props_for_field(
        field_name: &ast::Identifier,
    ) -> Result<(TypeDef, traits::Token), CompileError>
    where
        Self: std::marker::Sized,
    {
        match field_name.ident.as_str() {
            "prefix" => Ok((
                TypeDef::Prefix,
                Token::FieldAccess(vec![BasicRouteToken::Prefix.into()]),
            )),
            "as-path" => Ok((
                TypeDef::AsPath,
                Token::FieldAccess(vec![BasicRouteToken::AsPath.into()]),
            )),
            "origin-type" => Ok((
                TypeDef::OriginType,
                Token::FieldAccess(vec![BasicRouteToken::OriginType.into()]),
            )),
            "next-hop" => Ok((
                TypeDef::NextHop,
                Token::FieldAccess(vec![BasicRouteToken::NextHop.into()]),
            )),
            "multi-exit-disc" => Ok((
                TypeDef::MultiExitDisc,
                Token::FieldAccess(vec![
                    BasicRouteToken::MultiExitDisc.into()
                ]),
            )),
            "local-pref" => Ok((
                TypeDef::LocalPref,
                Token::FieldAccess(vec![BasicRouteToken::LocalPref.into()]),
            )),
            "atomic-aggregate" => Ok((
                TypeDef::Bool,
                Token::FieldAccess(vec![
                    BasicRouteToken::AtomicAggregate.into()
                ]),
            )),
            "aggregator" => Ok((
                TypeDef::AggregatorInfo,
                Token::FieldAccess(vec![BasicRouteToken::Aggregator.into()]),
            )),
            "communities" => Ok((
                TypeDef::List(Box::new(TypeDef::Community)),
                Token::FieldAccess(vec![BasicRouteToken::Communities.into()]),
            )),
            "status" => Ok((
                TypeDef::NlriStatus,
                Token::FieldAccess(vec![BasicRouteToken::Status.into()]),
            )),
            "provenance" => Ok((
                TypeDef::Provenance,
                Token::FieldAccess(vec![BasicRouteToken::Provenance.into()]),
            )),
            _ => Err(format!(
                "Unknown method '{}' for type Route",
                field_name.ident
            )
            .into()),
        }
    }
}

impl RotoType for BasicRoute {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "prefix" => Ok(MethodProps::new(
                TypeDef::Prefix,
                BasicRouteToken::Prefix.into(),
                vec![],
            )),
            "as-path" => Ok(MethodProps::new(
                TypeDef::AsPath,
                BasicRouteToken::AsPath.into(),
                vec![],
            )),
            "origin-type" => Ok(MethodProps::new(
                TypeDef::OriginType,
                BasicRouteToken::OriginType.into(),
                vec![],
            )),
            "next-hop" => Ok(MethodProps::new(
                TypeDef::NextHop,
                BasicRouteToken::NextHop.into(),
                vec![],
            )),
            "multi-exit-disc" => Ok(MethodProps::new(
                TypeDef::MultiExitDisc,
                BasicRouteToken::MultiExitDisc.into(),
                vec![],
            )),
            "local-pref" => Ok(MethodProps::new(
                TypeDef::LocalPref,
                BasicRouteToken::LocalPref.into(),
                vec![],
            )),
            "atomic-aggregate" => Ok(MethodProps::new(
                TypeDef::Bool,
                BasicRouteToken::AtomicAggregate.into(),
                vec![],
            )),
            "aggregator" => Ok(MethodProps::new(
                TypeDef::AggregatorInfo,
                BasicRouteToken::Aggregator.into(),
                vec![],
            )),
            "communities" => Ok(MethodProps::new(
                TypeDef::List(Box::new(TypeDef::Community)),
                BasicRouteToken::Communities.into(),
                vec![],
            )),
            "status" => Ok(MethodProps::new(
                TypeDef::NlriStatus,
                BasicRouteToken::Status.into(),
                vec![],
            )),
            "provenance" => Ok(MethodProps::new(
                TypeDef::Provenance,
                BasicRouteToken::Provenance.into(),
                vec![]
            )),
            _ => Err(format!(
                "Unknown method '{}' for type Route",
                method_name.ident
            )
            .into()),
        }
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::Route => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::Route(self)))
            }
            _ => Err(format!(
                "Cannot convert type Route to type {:?}",
                type_def
            )
            .into()),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        _method: usize,

        _args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidMethodCall)
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidMethodCall)
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidMethodCall)
    }
}

impl From<BasicRoute> for TypeValue {
    fn from(value: BasicRoute) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Route(value))
    }
}


#[derive(Copy, Clone, Debug, Deserialize, PartialEq, Eq)]
pub enum BasicRouteToken {
    Prefix = 0,
    AsPath = 1,
    OriginType = 2,
    NextHop = 3,
    MultiExitDisc = 4,
    LocalPref = 5,
    AtomicAggregate = 6,
    Aggregator = 7,
    Communities = 8,
    Status = 9,
    Provenance = 10,
    // PeerIp = 10,
    // PeerAsn = 11,
}

impl TryFrom<usize> for BasicRouteToken {
    type Error = VmError;

    fn try_from(value: usize) -> Result<Self, VmError> {
        match value {
            0 => Ok(BasicRouteToken::Prefix),
            1 => Ok(BasicRouteToken::AsPath),
            2 => Ok(BasicRouteToken::OriginType),
            3 => Ok(BasicRouteToken::NextHop),
            4 => Ok(BasicRouteToken::MultiExitDisc),
            5 => Ok(BasicRouteToken::LocalPref),
            6 => Ok(BasicRouteToken::AtomicAggregate),
            7 => Ok(BasicRouteToken::Aggregator),
            8 => Ok(BasicRouteToken::Communities),
            9 => Ok(BasicRouteToken::Status),
            10 => Ok(BasicRouteToken::Provenance),
            // 10 => Ok(RouteToken::PeerIp),
            // 11 => Ok(RouteToken::PeerAsn),
            _ => {
                debug!("Unknown RouteToken value: {}", value);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<BasicRouteToken> for usize {
    fn from(val: BasicRouteToken) -> Self {
        val as usize
    }
}

impl From<BasicRouteToken> for u8 {
    fn from(val: BasicRouteToken) -> Self {
        val as u8
    }
}

//------------ Provenance ----------------------------------------------------

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct Provenance {
    #[serde(skip)]
    pub timestamp: chrono::DateTime<Utc>,
    pub router_id: u32,     // hash over the string.
    pub connection_id: u32, // hash over the SourceId of the TCP connection on this Rotonda.
    pub peer_id: PeerId,
    pub peer_bgp_id: routecore::bgp::path_attributes::BgpIdentifier,
    pub peer_distuingisher: [u8; 8],
    pub peer_rib_type: PeerRibType,
}

impl Provenance {
    pub fn peer_ip(&self) -> std::net::IpAddr {
        self.peer_id.addr
    }

    pub(crate) fn get_props_for_field(
        field_name: &ast::Identifier,
    ) -> Result<(TypeDef, traits::Token), CompileError>
    where
        Self: std::marker::Sized,
    {
        match field_name.ident.as_str() {
            "timestamp" => Ok((
                TypeDef::U32,
                Token::FieldAccess(vec![ProvenanceToken::Timestamp.into()]),
            )),
            "router-id" => Ok((
                TypeDef::U32,
                Token::FieldAccess(vec![ProvenanceToken::RouterId.into()]),
            )),
            "connection-id" => Ok((
                TypeDef::U32,
                Token::FieldAccess(
                    vec![ProvenanceToken::ConnectionId.into()],
                ),
            )),
            "peer-id" => Ok((
                TypeDef::PeerId,
                Token::FieldAccess(vec![ProvenanceToken::PeerId.into()]),
            )),
            "peer-bgp-id" => Ok((
                TypeDef::List(Box::new(TypeDef::U32)),
                Token::FieldAccess(vec![ProvenanceToken::PeerBgpId.into()]),
            )),
            "peer-distuingisher" => Ok((
                TypeDef::List(Box::new(TypeDef::U32)),
                Token::FieldAccess(vec![
                    ProvenanceToken::PeerDistuingisher.into()
                ]),
            )),
            "peer-rib-type" => Ok((
                TypeDef::PeerRibType,
                Token::FieldAccess(vec![ProvenanceToken::PeerRibType.into()]),
            )),
            _ => Err(format!(
                "Unknown method '{}' for type Provenance",
                field_name.ident
            )
            .into()),
        }
    }

    pub fn get_field_by_index(
        &self,
        field_index: &[usize],
    ) -> Result<TypeValue, VmError> {
        match field_index.first().map(|i| (*i).try_into()) {
            Some(Ok(ProvenanceToken::Timestamp)) => todo!(),
            Some(Ok(ProvenanceToken::RouterId)) => Ok(self.router_id.into()),
            Some(Ok(ProvenanceToken::ConnectionId)) => {
                Ok(self.connection_id.into())
            }
            Some(Ok(ProvenanceToken::PeerId)) => {
                match field_index.len() {
                    // You shouldn't com here with an empty field_index, the
                    // method calling this should have return the whole
                    // Provenance struct already.
                    0 => Err(VmError::InvalidFieldAccess),
                    // We want the whole PeerId struct.
                    1 => Ok(self.peer_id.into()),
                    // We want a field on PeerId. PeerId has no nested fields,
                    // so we only need one usize, indicating the field, as
                    // argument.
                    _ => self.peer_id.get_field_by_index(field_index[1]),
                }
            }
            Some(Ok(ProvenanceToken::PeerBgpId)) => todo!(),
            Some(Ok(ProvenanceToken::PeerDistuingisher)) => todo!(),
            Some(Ok(ProvenanceToken::PeerRibType)) => {
                Ok(self.peer_rib_type.into())
            }
            _ => Err(VmError::InvalidFieldAccess),
        }
    }
}

impl Display for Provenance {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "timestamp: {}, router_id: {}, connection_id: {}, peer_id: \
        {}, peer_bgp_id: {:?}, peer_distuingisher: {:?}, peer_rib_type: {}",
            self.timestamp,
            self.router_id,
            self.connection_id,
            self.peer_id,
            self.peer_bgp_id,
            self.peer_distuingisher,
            self.peer_rib_type
        )
    }
}

impl RotoType for Provenance {
    fn get_props_for_method(
        _ty: TypeDef,
        _method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        Err(CompileError::from("Type Provenance has no methods"))
    }

    intotype!(Provenance;StringLiteral);

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidCommandArg)
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidCommandArg)
    }

    fn exec_type_method(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidCommandArg)
    }
}

createtoken!(
    Provenance;
    Timestamp = 0
    RouterId = 1
    ConnectionId = 2
    PeerId = 3
    PeerBgpId = 4
    PeerDistuingisher = 5
    PeerRibType = 6
);

impl From<ProvenanceToken> for u8 {
    fn from(val: ProvenanceToken) -> Self {
        val as u8
    }
}

impl From<Provenance> for TypeValue {
    fn from(value: Provenance) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Provenance(value))
    }
}

//------------ PeerRibType ---------------------------------------------------

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Default, Serialize)]
pub enum PeerRibType {
    InPre,
    InPost,
    Loc,
    OutPre,
    #[default]
    OutPost, // This is the default for BGP messages
}

impl Display for PeerRibType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PeerRibType::InPre => write!(f, "adj-RIB-in-pre"),
            PeerRibType::InPost => write!(f, "adj-RIB-in-post"),
            PeerRibType::Loc => write!(f, "RIB-loc"),
            PeerRibType::OutPre => write!(f, "adj-RIB-out-pre"),
            PeerRibType::OutPost => write!(f, "adj-RIB-out-pre"),
        }
    }
}

minimalscalartype!(PeerRibType);

impl From<(bool, routecore::bmp::message::RibType)> for PeerRibType {
    fn from(
        (is_post_policy, rib_type): (bool, routecore::bmp::message::RibType),
    ) -> Self {
        match rib_type {
            routecore::bmp::message::RibType::AdjRibIn => {
                if is_post_policy {
                    PeerRibType::InPost
                } else {
                    PeerRibType::InPre
                }
            }
            routecore::bmp::message::RibType::AdjRibOut => {
                if is_post_policy {
                    PeerRibType::OutPost
                } else {
                    PeerRibType::OutPre
                }
            }
            routecore::bmp::message::RibType::Unimplemented(_) => {
                PeerRibType::OutPost
            }
        }
    }
}

//------------ PeerId --------------------------------------------------------

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Serialize)]
pub struct PeerId {
    pub addr: IpAddr,
    pub asn: Asn,
}

impl PeerId {
    pub fn new(addr: IpAddr, asn: Asn) -> Self {
        Self { addr, asn }
    }

    pub(crate) fn get_props_for_field(
        field_name: &ast::Identifier,
    ) -> Result<(TypeDef, traits::Token), CompileError>
    where
        Self: std::marker::Sized,
    {
        match field_name.ident.as_str() {
            "addr" => Ok((
                TypeDef::IpAddr,
                Token::FieldAccess(vec![PeerIdToken::Addr.into()]),
            )),
            "asn" => Ok((
                TypeDef::Asn,
                Token::FieldAccess(vec![PeerIdToken::Asn.into()]),
            )),
            _ => Err(format!(
                "Unknown method '{}' for type PeerId",
                field_name.ident
            )
            .into()),
        }
    }

    pub(crate) fn get_field_by_index(
        &self,
        index: usize,
    ) -> Result<TypeValue, VmError> {
        trace!("get peer_id field");
        match index.try_into()? {
            PeerIdToken::Addr => Ok(self.addr.into()),
            PeerIdToken::Asn => Ok(self.asn.into()),
        }
    }
}

impl Display for PeerId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:AS{}", self.addr, self.asn)
    }
}

createtoken!(
    PeerId;
    Addr = 0
    Asn = 1
);

impl From<PeerIdToken> for u8 {
    fn from(val: PeerIdToken) -> Self {
        val as u8
    }
}

impl From<PeerId> for TypeValue {
    fn from(value: PeerId) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::PeerId(value))
    }
}

impl RotoType for PeerId {
    fn get_props_for_method(
        ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn into_type(
        self,
        type_value: &TypeDef,
    ) -> Result<TypeValue, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        args: &'a [StackValue],
        res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        method_token: usize,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method(
        method_token: usize,
        args: &[StackValue],
        res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

// We are abusing the method_token to convey the requested RouteStatus
// from the invoked method. So instead of creating all 'is_*' methods
// separately, we can just compare the method_token with self.

impl RotoType for NlriStatus {
    fn get_props_for_method(
        _ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "is_in_convergence" => Ok(MethodProps::new(
                TypeDef::Bool,
                usize::from(NlriStatus::InConvergence),
                vec![],
            )),
            "is_up_to_date" => Ok(MethodProps::new(
                TypeDef::Bool,
                usize::from(NlriStatus::UpToDate),
                vec![],
            )),
            "is_stale" => Ok(MethodProps::new(
                TypeDef::Bool,
                usize::from(NlriStatus::Stale),
                vec![],
            )),
            "is_start_of_route_refresh" => Ok(MethodProps::new(
                TypeDef::Bool,
                usize::from(NlriStatus::StartOfRouteRefresh),
                vec![],
            )),
            "is_withdrawn" => Ok(MethodProps::new(
                TypeDef::Bool,
                usize::from(NlriStatus::Withdrawn),
                vec![],
            )),
            "is_empty" => Ok(MethodProps::new(
                TypeDef::Bool,
                usize::from(NlriStatus::Empty),
                vec![],
            )),
            _ => Err(format!(
                "Unknown method '{}' for type RouteStatus",
                method_name.ident
            )
            .into()),
        }
    }

    fn into_type(
        self,
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::NlriStatus => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::NlriStatus(self)))
            }
            _ => Err(format!(
                "Cannot convert type RouteStatus to type {:?}",
                type_def
            )
            .into()),
        }
    }

    // The abuse happens here.
    fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        _args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Ok(TypeValue::Builtin(BuiltinTypeValue::Bool(
            method_token == usize::from(*self),
        )))
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidMethodCall)
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        Err(VmError::InvalidMethodCall)
    }
}

impl From<NlriStatus> for usize {
    fn from(val: NlriStatus) -> Self {
        val as usize
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Serialize)]
pub struct RotondaId(pub usize);

impl std::fmt::Display for RotondaId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let w = write!(f, "{}", self.0);
        w
    }
}

// Maybe this should go into routecore?!? or at least rotonda-fsm. It was
// lifted from rotonda, but since we need it both there and here, this is the
// place. I don't want circular deps.

//------------ SourceId ------------------------------------------------------

// The source of received updates.
//
// Not all incoming data has to come from a TCP/IP connection. This enum
// exists to represent both the TCP/IP type of incoming connection that we
// receive data from today as well as other connection types in future, and
// can also be used to represent alternate sources of incoming data such as
// replay from file or test data created on the fly.
// #[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SourceId {
    SocketAddr(SocketAddr),
    Named(std::sync::Arc<String>),
}

impl Default for SourceId {
    fn default() -> Self {
        "unknown".into()
    }
}

impl SourceId {
    pub fn generated() -> Self {
        Self::from("generated")
    }

    pub fn socket_addr(&self) -> Option<&SocketAddr> {
        match self {
            SourceId::SocketAddr(addr) => Some(addr),
            SourceId::Named(_) => None,
        }
    }

    pub fn ip(&self) -> Option<IpAddr> {
        match self {
            SourceId::SocketAddr(addr) => Some(addr.ip()),
            SourceId::Named(_) => None,
        }
    }

    pub fn name(&self) -> Option<&str> {
        match self {
            SourceId::SocketAddr(_) => None,
            SourceId::Named(name) => Some(name),
        }
    }
}

impl std::fmt::Display for SourceId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SourceId::SocketAddr(addr) => addr.fmt(f),
            SourceId::Named(name) => name.fmt(f),
        }
    }
}

impl From<SocketAddr> for SourceId {
    fn from(addr: SocketAddr) -> Self {
        SourceId::SocketAddr(addr)
    }
}

impl From<String> for SourceId {
    fn from(name: String) -> Self {
        SourceId::Named(name.into())
    }
}

impl From<&str> for SourceId {
    fn from(name: &str) -> Self {
        SourceId::Named(name.to_string().into())
    }
}
