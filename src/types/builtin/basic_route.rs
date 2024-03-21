use std::hash::Hash;
use std::hash::Hasher;
use std::net::IpAddr;
use std::net::SocketAddr;

use chrono::Utc;
use routecore::bgp::communities::Community;
use routecore::bgp::communities::HumanReadableCommunity;
use routecore::bgp::nlri::flowspec::FlowSpecNlri;
use routecore::bgp::path_attributes::PaMap;
use routecore::bgp::path_attributes::PathAttribute;
use routecore::bgp::types::LocalPref;
use routecore::bgp::types::MultiExitDisc;
use routecore::bgp::types::NextHop;
use routecore::bgp::workshop::route::BasicNlri;
use routecore::bgp::workshop::route::RouteWorkshop;
use serde::Deserialize;
use std::fmt::{Display, Formatter};

use log::{debug, error, trace};
use paste::paste;
use serde::{self, Serialize};

use crate::compiler::compile::CompileError;
use crate::traits::RotoType;
use crate::types::collections::ElementTypeValue;
use crate::types::collections::LazyRecord;
use crate::types::collections::List;
use crate::types::typedef::MethodProps;
use crate::types::typedef::RecordTypeDef;
use crate::vm::{StackValue, VmError};
use crate::{
    ast,
    traits::{self, Token},
    types::lazyrecord_types::BgpUpdateMessage,
    vm::FieldIndex,
};
use crate::{
    createtoken, intotype, minimalscalartype, setmethodonly,
    typevaluefromimpls,
};

use super::super::typevalue::TypeValue;
use super::BuiltinTypeValue;
use super::Nlri;
use super::{super::typedef::TypeDef, BytesRecord, NlriStatus};

use routecore::addr::Prefix;
use routecore::asn::Asn;
use routecore::bgp::types::PathId;

pub type LogicalTime = u64;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct BasicRoute(
    pub routecore::bgp::workshop::route::RouteWorkshop<BasicNlri>,
);

impl BasicRoute {

    pub fn prefix(&self) -> Prefix {
        self.0.nlri().prefix()
    }

    pub fn path_id(&self) -> Option<PathId> {
        self.0.nlri().path_id()
    }

    pub fn get_field_by_index(
        &self,
        field_index: &FieldIndex,
    ) -> Result<TypeValue, VmError> {
        trace!(
            "get_field_by_index {:?} for BasicRoute {:?}",
            field_index,
            self
        );
        let mut index_iter = field_index.iter();
        let index = index_iter.next();

        match index {
            // The NLRI
            Some(0) => match index_iter.next() {
                Some(0) => Ok(TypeValue::from(self.0.nlri().prefix)),
                Some(1) => self
                    .0
                    .nlri()
                    .path_id
                    .map(TypeValue::from)
                    .ok_or_else(|| VmError::InvalidPathAttribute),
                _ => Err(VmError::InvalidPathAttribute),
            },
            // The Path Attributes
            
            // BEWARE! Not all tokens used on BasicRoute are mapped one-to-one
            // to the IANA assignment codes for path attributes.
            // - `NextHop`` uses type code 3, which is the CONVENTIONAL next
            //   hop in the IANA assignments, but we are using it for the next
            //   hop, wherever it lives.
            // - `Vec<Community>` (the list of ALL communities) uses type code
            //   8, which in the IANA assignments is used for STANDARD
            //   communities only.
            Some(1) => {
                if let Some(index) = index_iter.next() {
                    let attr: Option<TypeValue> = match index {
                        3 => {
                            todo!()
                            // self.0.get_attr::<NextHop>().map(TypeValue::from)
                        }
                        8 => self.0.get_attr::<Vec<Community>>().map(|c| {
                            TypeValue::List(List(
                                c.into_iter()
                                    .map(|c| {
                                        ElementTypeValue::Primitive(
                                            HumanReadableCommunity(c).into(),
                                        )
                                    })
                                    .collect::<Vec<_>>(),
                            ))
                        }),
                        _ => self
                            .0
                            .attributes()
                            .get_by_type_code(index)
                            .map(|pa| TypeValue::from(pa.clone())),
                    };
                    attr.ok_or(VmError::InvalidPathAttribute)
                } else {
                    Err(VmError::InvalidPathAttribute)
                }
            }
            _ => Err(VmError::InvalidFieldAccess),
        }
    }

    pub(crate) fn get_mut_field_by_index(
        &mut self,
        field_index: &FieldIndex,
    ) -> Result<TypeValue, VmError> {
        trace!(
            "get_mut_field_by_index ({:?}) for BasicRoute {:?}",
            field_index,
            self
        );
        let mut index_iter = field_index.iter();
        let index = index_iter.next();

        match index {
            // The NLRI
            Some(0) => match index_iter.next() {
                Some(0) => {
                    trace!("index {:?}", index);
                    trace!("PREFIX! {:?}", self.0.nlri().prefix);
                    Ok(TypeValue::from(self.0.nlri().prefix))
                }
                Some(1) => self
                    .0
                    .nlri()
                    .path_id
                    .map(TypeValue::from)
                    .ok_or_else(|| VmError::InvalidPathAttribute),
                _ => Err(VmError::InvalidPathAttribute),
            },
            // The Path Attributes
            Some(1) => {
                if let Some(index) = index_iter.next() {
                    let attr =
                        self.0.attributes_mut().get_mut_by_type_code(index);
                    attr.map(|pa| TypeValue::from(pa.clone()))
                        .ok_or(VmError::InvalidPathAttribute)
                } else {
                    Err(VmError::InvalidPathAttribute)
                }
            }
            _ => Err(VmError::InvalidFieldAccess),
        }
    }

    pub(crate) fn hash_field<H: Hasher>(
        &self,
        state: &mut H,
        field_index: &FieldIndex,
    ) -> Result<(), VmError> {
        let index = u8::try_from(field_index.first()?)
            .map_err(|_| VmError::InvalidPathAttribute)?;
        self.0.attributes().get_by_type_code(index).hash(state);
        Ok(())
    }
}

impl From<RouteWorkshop<BasicNlri>> for TypeValue {
    fn from(value: RouteWorkshop<BasicNlri>) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Route(BasicRoute(value)))
    }
}

impl From<BasicRoute> for TypeValue {
    fn from(value: BasicRoute) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Route(value))
    }
}

// impl<O: routecore::Octets> From<RouteWorkshop<O, Ipv4UnicastNlri>> for TypeValue {
//     fn from(_value: RouteWorkshop<O, Ipv4UnicastNlri>) -> Self {
//         todo!()
//     }
// }

//------------ MutableBasicRoute ---------------------------------------------

// pub type PathAttributeMap = BTreeMap<PathAttributeType, PathAttribute>;

impl From<PathAttribute> for TypeValue {
    fn from(value: PathAttribute) -> Self {
        match value {
            PathAttribute::Origin(v) => {
                TypeValue::Builtin(BuiltinTypeValue::Origin(v))
            }
            PathAttribute::AsPath(hop_path) => {
                TypeValue::Builtin(BuiltinTypeValue::AsPath(hop_path))
            }
            PathAttribute::ConventionalNextHop(next_hop) => {
                TypeValue::Builtin(BuiltinTypeValue::NextHop(
                    NextHop::Unicast(std::net::IpAddr::V4(next_hop.0)),
                ))
            }
            PathAttribute::MultiExitDisc(med) => TypeValue::Builtin(
                BuiltinTypeValue::MultiExitDisc(MultiExitDisc(med.0)),
            ),
            PathAttribute::LocalPref(lp) => TypeValue::Builtin(
                BuiltinTypeValue::LocalPref(LocalPref(lp.into())),
            ),
            PathAttribute::AtomicAggregate(aa) => {
                TypeValue::Builtin(BuiltinTypeValue::AtomicAggregate(aa))
            }
            PathAttribute::Aggregator(a) => {
                TypeValue::Builtin(BuiltinTypeValue::AggregatorInfo(a))
            }
            PathAttribute::StandardCommunities(comms) => {
                TypeValue::List(List(comms.fmap(|c| {
                    ElementTypeValue::Primitive(
                        HumanReadableCommunity::from(*c).into(),
                    )
                })))
            }
            PathAttribute::OriginatorId(_) => todo!(),
            PathAttribute::ClusterList(_) => todo!(),
            // PathAttribute::MpReachNlri(_) => todo!(),
            // PathAttribute::MpUnreachNlri(_) => todo!(),
            PathAttribute::ExtendedCommunities(comms) => {
                TypeValue::List(List(comms.fmap(|c| {
                    ElementTypeValue::Primitive(
                        HumanReadableCommunity::from(c).into(),
                    )
                })))
            }
            PathAttribute::As4Path(_) => todo!(),
            PathAttribute::As4Aggregator(_) => todo!(),
            PathAttribute::Connector(_) => todo!(),
            PathAttribute::AsPathLimit(_) => todo!(),
            PathAttribute::Ipv6ExtendedCommunities(_) => todo!(),
            PathAttribute::LargeCommunities(comms) => {
                TypeValue::List(List(comms.fmap(|c| {
                    ElementTypeValue::Primitive(
                        HumanReadableCommunity::from(c).into(),
                    )
                })))
            }
            PathAttribute::Otc(_) => todo!(),
            PathAttribute::AttrSet(_) => todo!(),
            PathAttribute::Reserved(_) => todo!(),
            PathAttribute::Unimplemented(_) => todo!(),
            PathAttribute::Invalid(_, _, _) => todo!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct RouteContext {
    pub(crate) bgp_msg: Option<BytesRecord<BgpUpdateMessage>>,
    pub(crate) provenance: Provenance,
    pub(crate) nlri_status: NlriStatus,
}

impl RouteContext {
    pub fn new(
        bgp_msg: Option<BytesRecord<BgpUpdateMessage>>,
        nlri_status: NlriStatus,
        provenance: Provenance,
    ) -> Self {
        Self {
            bgp_msg,
            nlri_status,
            provenance,
        }
    }

    pub fn message(&self) -> &Option<BytesRecord<BgpUpdateMessage>> {
        &self.bgp_msg
    }

    pub fn provenance(&self) -> &Provenance {
        &self.provenance
    }

    pub fn nlri_status(&self) -> NlriStatus {
        self.nlri_status
    }

    pub fn get_attrs_builder(&self) -> Result<PaMap, VmError> {
        if let Some(msg) = &self.bgp_msg {
            PaMap::from_update_pdu(&msg.clone().into_inner())
                .map_err(|_| VmError::InvalidMsgType)
        } else {
            Err(VmError::InvalidPayload)
        }
    }

    pub fn into_type(self, ty: &TypeDef) -> Result<TypeValue, CompileError>
    where
        Self: std::marker::Sized,
    {
        Err(format!(
            "Context cannot be converted to type {} (or any other type)",
            ty
        )
        .into())
    }

    pub fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    pub fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    pub fn exec_type_method(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    pub fn get_props_for_method(
        _ty: TypeDef,
        _method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    pub fn get_props_for_field(
        field: &ast::Identifier,
    ) -> Result<(TypeDef, Token), CompileError> {
        match field.ident.as_str() {
            "update-message" => Ok((
                TypeDef::LazyRecord(crate::types::lazyrecord_types::LazyRecordTypeDef::UpdateMessage),
                Token::FieldAccess(vec![RouteContextToken::BgpMessage.into()])
            )),
            "provenance" => Ok((
                TypeDef::Provenance,
                Token::FieldAccess(vec![RouteContextToken::Provenance.into()])
            )),
            "status" => Ok((
                TypeDef::NlriStatus,
                Token::FieldAccess(vec![RouteContextToken::Status.into()])
            )),
            _ => Err(CompileError::from("bla"))
        }
    }

    pub fn get_field_by_index(
        &self,
        field_index: &FieldIndex,
    ) -> Result<TypeValue, VmError> {
        trace!(
            "get_field_by_index {:?} for RouteContext {:?}",
            field_index,
            self
        );

        match RouteContextToken::try_from(field_index.first()?) {
            Ok(RouteContextToken::Provenance) => {
                trace!("provenance w/ field index {:?}", field_index);
                // recurse into provenance.
                self.provenance()
                    .get_field_by_index(field_index.skip_first())
            }
            Ok(RouteContextToken::Status) => self
                .nlri_status()
                .get_field_by_index(field_index.skip_first()),
            Ok(RouteContextToken::BgpMessage) => {
                // recurse into Update Message.
                if let Some(bgp_msg) = &self.bgp_msg {
                    let tv = LazyRecord::from_type_def(BytesRecord::<
                        BgpUpdateMessage,
                    >::lazy_type_def(
                    ))?
                    .get_field_by_index(
                        &field_index.skip_first().into(),
                        bgp_msg,
                    )?;

                    // There are no deeper nested records in a Update message
                    // (just attributes + prefix), so we can stop t this level.
                    if let ElementTypeValue::Primitive(tv) = tv {
                        Ok(tv)
                    } else {
                        Err(VmError::InvalidContext)
                    }
                } else {
                    Err(VmError::InvalidContext)
                }
            }
            _ => Err(VmError::InvalidFieldAccess),
        }
    }
}

impl AsRef<RouteContext> for RouteContext {
    fn as_ref(&self) -> &RouteContext {
        self
    }
}

impl std::fmt::Display for BasicRoute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "prefix  : {}", self.prefix())?;
        writeln!(f, "path_id : {:?}", self.path_id())?;
        writeln!(f, "attrs    : {:?}", self.0.attributes())
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
            // The NLRI
            "prefix" => Ok((
                TypeDef::Prefix,
                Token::FieldAccess(vec![0, BasicNlriToken::Prefix.into()]),
            )),
            "path-id" => Ok((
                TypeDef::PathId,
                Token::FieldAccess(vec![0, BasicNlriToken::PathId.into()]),
            )),
            // The Path Attributes. They follow the IANA BGP Attributes type
            // codes.
            "as-path" => Ok((
                TypeDef::AsPath,
                Token::FieldAccess(vec![1, BasicRouteToken::AsPath.into()]),
            )),
            "origin" => Ok((
                TypeDef::Origin,
                Token::FieldAccess(vec![
                    1,
                    BasicRouteToken::OriginType.into(),
                ]),
            )),
            "next-hop" => Ok((
                TypeDef::NextHop,
                Token::FieldAccess(vec![1, BasicRouteToken::NextHop.into()]),
            )),
            "multi-exit-disc" => Ok((
                TypeDef::MultiExitDisc,
                Token::FieldAccess(vec![
                    1,
                    BasicRouteToken::MultiExitDisc.into(),
                ]),
            )),
            "local-pref" => Ok((
                TypeDef::LocalPref,
                Token::FieldAccess(vec![
                    1,
                    BasicRouteToken::LocalPref.into(),
                ]),
            )),
            "atomic-aggregate" => Ok((
                TypeDef::Bool,
                Token::FieldAccess(vec![
                    1,
                    BasicRouteToken::AtomicAggregate.into(),
                ]),
            )),
            "aggregator" => Ok((
                TypeDef::AggregatorInfo,
                Token::FieldAccess(vec![
                    1,
                    BasicRouteToken::Aggregator.into(),
                ]),
            )),
            "communities" => Ok((
                TypeDef::List(Box::new(TypeDef::Community)),
                Token::FieldAccess(vec![
                    1,
                    BasicRouteToken::Communities.into(),
                ]),
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
                BasicNlriToken::Prefix.into(),
                vec![],
            )),
            "path-id" => Ok(MethodProps::new(
                TypeDef::PathId,
                BasicNlriToken::PathId.into(),
                vec![],
            )),
            "as-path" => Ok(MethodProps::new(
                TypeDef::AsPath,
                BasicRouteToken::AsPath.into(),
                vec![],
            )),
            "origin" => Ok(MethodProps::new(
                TypeDef::Origin,
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

    fn take_value(self) -> Self {
        self
    }
}

#[derive(Copy, Clone, Debug, Deserialize, PartialEq, Eq)]
pub enum BasicNlriToken {
    Prefix = 0,
    PathId = 1,
}


//------------ BasicRouteToken -----------------------------------------------

// This token is mapped to the IANA assignment codes
// (https://www.iana.org/assignments/bgp-parameters/bgp-parameters.xhtml)
// With a few major exceptions:
// - `NextHop`` uses type code 3, which is the CONVENTIONAL next hop in the
//   IANA assignments, but we are using it for the next hop, wherever it
//   lives.
// - `Vec<Community>` (the list of ALL communities) uses type code 8, which in
//   the IANA assignments is used for STANDARD communities only.

#[derive(Copy, Clone, Debug, Deserialize, PartialEq, Eq)]
pub enum BasicRouteToken {
    OriginType = 1,
    AsPath = 2,
    NextHop = 3,
    MultiExitDisc = 4,
    LocalPref = 5,
    AtomicAggregate = 6,
    Aggregator = 7,
    Communities = 8,
}

impl TryFrom<usize> for BasicRouteToken {
    type Error = VmError;

    fn try_from(value: usize) -> Result<Self, VmError> {
        match value {
            1 => Ok(BasicRouteToken::OriginType),
            2 => Ok(BasicRouteToken::AsPath),
            3 => Ok(BasicRouteToken::NextHop),
            4 => Ok(BasicRouteToken::MultiExitDisc),
            5 => Ok(BasicRouteToken::LocalPref),
            6 => Ok(BasicRouteToken::AtomicAggregate),
            7 => Ok(BasicRouteToken::Aggregator),
            8 => Ok(BasicRouteToken::Communities),
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

#[derive(Copy, Clone, Debug, Deserialize, PartialEq, Eq)]
pub enum RouteContextToken {
    BgpMessage = 0,
    Provenance = 1,
    Status = 2,
}

impl TryFrom<usize> for RouteContextToken {
    type Error = VmError;

    fn try_from(value: usize) -> Result<Self, VmError> {
        match value {
            0 => Ok(RouteContextToken::BgpMessage),
            1 => Ok(RouteContextToken::Provenance),
            2 => Ok(RouteContextToken::Status),
            _ => {
                debug!("Unknown RouteContextToken value: {}", value);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<RouteContextToken> for usize {
    fn from(val: RouteContextToken) -> Self {
        val as usize
    }
}

impl From<RouteContextToken> for u8 {
    fn from(val: RouteContextToken) -> Self {
        val as u8
    }
}

impl TryFrom<usize> for BasicNlriToken {
    type Error = VmError;

    fn try_from(value: usize) -> Result<Self, VmError> {
        match value {
            0 => Ok(BasicNlriToken::Prefix),
            1 => Ok(BasicNlriToken::PathId),
            _ => {
                debug!("Unknown BasicNlriToken value: {}", value);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<BasicNlriToken> for usize {
    fn from(val: BasicNlriToken) -> Self {
        val as usize
    }
}

impl From<BasicNlriToken> for u8 {
    fn from(val: BasicNlriToken) -> Self {
        val as u8
    }
}


//------------ FlowSpecRoute -------------------------------------------------

impl RotoType for RouteWorkshop<FlowSpecNlri<bytes::Bytes>> {
    fn get_props_for_method(
        _ty: TypeDef,
        _method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn into_type(
        self,
        _type_value: &TypeDef,
    ) -> Result<TypeValue, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

impl From<RouteWorkshop<FlowSpecNlri<bytes::Bytes>>>
    for TypeValue
{
    fn from(
        value: RouteWorkshop<FlowSpecNlri<bytes::Bytes>>,
    ) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Nlri(Nlri::Ipv4FlowSpec(value.nlri().clone().into())))
    }
}

//------------ Provenance ----------------------------------------------------

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct Provenance {
    #[serde(skip)]
    pub timestamp: chrono::DateTime<Utc>,
    // The SocketAddr of the monitored router over BMP, or the SocketAddr of
    // the BGP peer over BGP.
    pub connection_id: SocketAddr,
    // The (IP Address, ASN) of a peer of the monitored router over BMP, or
    // the (IP Address, ASN) tuple of the connected BGP peer.
    pub peer_id: PeerId,
    pub peer_bgp_id: routecore::bgp::path_attributes::BgpIdentifier,
    pub peer_distuingisher: [u8; 8],
    pub peer_rib_type: PeerRibType,
}

impl Provenance {
    // pub fn from_rotonda() -> Self {
    //     Self {
    //         timestamp: Utc::now(),
    //         // router_id: 0,
    //         connection_id: 0,
    //         peer_id: PeerId {
    //             addr: "127.0.0.1".parse().unwrap(),
    //             asn: 0.into(),
    //         },
    //         peer_bgp_id: BgpIdentifier::from([0, 0, 0, 0]),
    //         peer_distuingisher: [0, 0, 0, 0, 0, 0, 0, 0],
    //         peer_rib_type: PeerRibType::Loc,
    //     }
    // }

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
        trace!("get_field_by_index {:?} for Provenance", field_index);
        match field_index.first().map(|i| (*i).try_into()) {
            Some(Ok(ProvenanceToken::Timestamp)) => todo!(),
            // Some(Ok(ProvenanceToken::RouterId)) => Ok(self.router_id.into()),
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
            "timestamp: {}, connection_id: {}, peer_id: \
        {}, peer_bgp_id: {:?}, peer_distuingisher: {:?}, peer_rib_type: {}",
            self.timestamp,
            // self.router_id,
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
        _ty: TypeDef,
        _method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn into_type(
        self,
        _type_value: &TypeDef,
    ) -> Result<TypeValue, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
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
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
