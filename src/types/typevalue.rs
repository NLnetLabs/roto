use std::net::IpAddr;
use std::{cmp::Ordering, fmt::Display, sync::Arc};

use log::{debug, trace};
use primitives::{Nlri, NlriStatus};

use inetnum::asn::Asn;
use routecore::bgp::aspath::{HopPath, OwnedHop as Hop};
use routecore::bgp::communities::HumanReadableCommunity as Community;
use routecore::bgp::nlri::afisafi::{
    Ipv4MulticastAddpathNlri, Ipv4MulticastNlri, Ipv4UnicastAddpathNlri, 
    Ipv4UnicastNlri, Ipv6FlowSpecNlri, Ipv6MulticastAddpathNlri, 
    Ipv6MulticastNlri, Ipv6UnicastAddpathNlri, Ipv6UnicastNlri
};
use routecore::bgp::types::PathId;
use routecore::bgp::types::{
    AfiSafiType, LocalPref, MultiExitDisc, NextHop, Origin
};
use routecore::bgp::workshop::route::RouteWorkshop;
use serde::Serialize;


//============ TypeValue ====================================================
use crate::{
    ast::{
        AnonymousRecordValueExpr, ListValueExpr, ShortString,
        TypedRecordValueExpr,
    },
    // attr_change_set::ScalarValue,
    compiler::compile::CompileError,
    traits::{RotoType, Token},
    vm::{FieldIndex, StackValue, VmError},
};

use super::builtin::basic_route::{PeerId, PeerRibType, Provenance};
use super::builtin::{FlowSpecNlri, FlowSpecRoute, PrefixRoute, PrefixRouteWs, RouteContext};
use super::lazyrecord_types::BgpUpdateMessage;
use super::{
    builtin::{
        primitives, BuiltinTypeValue, HexLiteral, IntegerLiteral,
        PrefixLength, StringLiteral,
    },
    collections::{
        BytesRecord, ElementTypeValue, EnumBytesRecord, LazyRecord, List,
        Record, RecordType,
    },
    lazyrecord_types::{
        InitiationMessage, PeerDownNotification, PeerUpNotification,
        RouteMonitoring, StatisticsReport, TerminationMessage,
    },
    outputs::OutputStreamMessage,
    typedef::TypeDef,
};

/// These are the actual types that are used in the Roto language. This enum
/// holds both the type-level information and the value. The collection
/// variants can hold multiple values recursively, e.g. a List of Records.
#[derive(Debug, Eq, Default, Clone, Serialize)]
#[serde(untagged)]
pub enum TypeValue {
    /// All the built-in scalars and vectors
    Builtin(BuiltinTypeValue),
    /// An ordered list of one user-defined type
    List(List),
    /// A map of (key, value) pairs, where value can be any of the other types.
    /// Always user-defined.
    Record(Record),
    // Enum(Enum),
    /// A Record meant to be handled by an Output stream.
    OutputStreamMessage(Arc<OutputStreamMessage>),
    /// A wrapper around an immutable value that lives in an external
    /// datasource, i.e. a table or a rib
    SharedValue(Arc<TypeValue>),
    /// Unknown is NOT EQUAL to empty or unitialized, e.g. it may be the
    /// result of a search. A ternary logic value, if you will.
    Unknown,
    /// Used for LinearMemory only, it's the initial state of all positions
    /// except the first two positions (rx and tx). Taking a typevalue in the
    /// vm runtime, also puts this value in its place.
    #[default]
    UnInit,
}

impl TypeValue {
    pub fn is_unitialized(&self) -> bool {
        matches!(self, TypeValue::UnInit)
    }

    pub fn create_record(
        type_ident_pairs: Vec<(&str, TypeValue)>,
    ) -> Result<Record, CompileError> {
        let mut elems = vec![];

        for (ident, ty) in type_ident_pairs {
            elems.push((
                ShortString::from(ident),
                ty.try_into().map_err(|e: VmError| {
                    CompileError::from(e.to_string())
                })?,
            ));
        }
        // let elems = type_ident_pairs
        //     .into_iter()
        //     .map(|(ident, ty)| (ShortString::from(ident), ty.try_into().map_err(|e: VmError| CompileError::from("bla"))? ))
        //     .collect::<Vec<_>>();

        Ok(Record::new(elems))
    }

    pub fn is_boolean_type(&self) -> bool {
        matches!(self, TypeValue::Builtin(BuiltinTypeValue::Bool(_)))
    }

    pub fn is_false(&self) -> Result<bool, VmError> {
        if let TypeValue::Builtin(BuiltinTypeValue::Bool(bool_val)) = self {
            Ok(!bool_val)
        } else {
            Err(VmError::InvalidValueType)
        }
    }

    pub(crate) fn builtin_as_cloned_type_value(
        &self,
    ) -> Result<TypeValue, CompileError> {
        match self {
            TypeValue::Builtin(b) => Ok(TypeValue::Builtin(b.clone())),
            _ => {
                Err(format!("Type '{:?}' is not a builtin type.", self)
                    .into())
            }
        }
    }

    pub(crate) fn into_builtin(
        self,
    ) -> Result<BuiltinTypeValue, CompileError> {
        match self {
            TypeValue::Builtin(b) => Ok(b),
            _ => {
                Err(format!("Type '{:?}' is not a builtin type.", self)
                    .into())
            }
        }
    }

    pub fn get_field_by_index(
        &self,
        index: FieldIndex,
    ) -> Result<&ElementTypeValue, CompileError> {
        trace!("get_field_by_index for TypeValue {:?}", self);
        match self {
            TypeValue::Record(r) => {
                let field = r.get_field_by_index(&index);
                field.ok_or_else(|| {
                    format!(
                        "Index {:?} out of bounds for record '{:?}'",
                        index, self
                    )
                    .as_str()
                    .into()
                })
            }
            TypeValue::List(l) => {
                let field = l.get_field_by_index(index.clone());
                field.ok_or_else(|| {
                    format!(
                        "Index {:?} out of bounds for list '{:?}'",
                        index, self
                    )
                    .as_str()
                    .into()
                })
            }
            _ => Err(format!("Type '{:?}' is not a record.", self).into()),
        }
    }

    pub(crate) fn _set_field(
        mut self,
        field_index: FieldIndex,
        value: TypeValue,
    ) -> Result<Self, VmError> {
        match self {
            TypeValue::Record(ref mut rec) => {
                rec.set_value_on_field_index(field_index, value)?;
            }
            TypeValue::List(ref mut list) => {
                list.set_field_for_index(field_index, value)?;
            }
            _ => return Err(VmError::InvalidWrite),
        };
        Ok(self)
    }

    pub fn mp_is_variant(&self, variant_token: Token) -> bool {
        match self {
            // We only know how to deal with BmpMessages currently.
            TypeValue::Builtin(BuiltinTypeValue::BmpMessage(bytes_rec)) => {
                bytes_rec.is_variant(variant_token)
            }
            _ => false,
        }
    }

    /// Return a TypeValue if the memory holds the enum variant specified by
    /// the varian_token. If the memory position holds an enum, but not of the
    /// specified variant, then return TypeValue::Unknown.
    pub fn get_mp_as_variant_or_unknown(
        &self,
        variant_token: Token,
    ) -> Result<&TypeValue, VmError> {
        if let TypeValue::Builtin(BuiltinTypeValue::BmpMessage(bytes_rec)) =
            self
        {
            if bytes_rec.is_variant(variant_token) {
                Ok(self)
            } else {
                Ok(&TypeValue::Unknown)
            }
        } else {
            Err(VmError::InvalidValueType)
        }
    }

    pub fn is_bytes_record(&self) -> bool {
        matches!(
            self,
            TypeValue::Builtin(BuiltinTypeValue::BmpMessage(_))
                | TypeValue::Builtin(
                    BuiltinTypeValue::BmpRouteMonitoringMessage(_),
                )
                | TypeValue::Builtin(
                    BuiltinTypeValue::BmpPeerDownNotification(_),
                )
                | TypeValue::Builtin(
                    BuiltinTypeValue::BmpPeerUpNotification(_,)
                )
                | TypeValue::Builtin(BuiltinTypeValue::BmpInitiationMessage(
                    _
                ))
                | TypeValue::Builtin(BuiltinTypeValue::BmpStatisticsReport(
                    _
                ))
                | TypeValue::Builtin(
                    BuiltinTypeValue::BmpTerminationMessage(_,)
                )
        )
    }

    pub fn into_route_monitoring_bytes_record(
        self,
    ) -> Option<BytesRecord<RouteMonitoring>> {
        if let TypeValue::Builtin(
            BuiltinTypeValue::BmpRouteMonitoringMessage(bytes_rec),
        ) = self
        {
            Some(bytes_rec)
        } else {
            None
        }
    }

    pub fn into_initiation_message_bytes_record(
        self,
    ) -> Option<BytesRecord<InitiationMessage>> {
        if let TypeValue::Builtin(BuiltinTypeValue::BmpInitiationMessage(
            bytes_rec,
        )) = self
        {
            Some(bytes_rec)
        } else {
            None
        }
    }

    pub fn into_bytes(self) -> Option<bytes::Bytes> {
        match self {
            TypeValue::Builtin(BuiltinTypeValue::BmpInitiationMessage(
                bytes_rec,
            )) => Some(bytes::Bytes::copy_from_slice(bytes_rec.as_ref())),
            TypeValue::Builtin(
                BuiltinTypeValue::BmpRouteMonitoringMessage(bytes_rec),
            ) => Some(bytes::Bytes::copy_from_slice(bytes_rec.as_ref())),
            TypeValue::Builtin(
                BuiltinTypeValue::BmpPeerDownNotification(bytes_rec),
            ) => Some(bytes::Bytes::copy_from_slice(bytes_rec.as_ref())),
            TypeValue::Builtin(BuiltinTypeValue::BmpPeerUpNotification(
                bytes_rec,
            )) => Some(bytes::Bytes::copy_from_slice(bytes_rec.as_ref())),
            TypeValue::Builtin(BuiltinTypeValue::BmpStatisticsReport(
                bytes_rec,
            )) => Some(bytes::Bytes::copy_from_slice(bytes_rec.as_ref())),
            TypeValue::Builtin(BuiltinTypeValue::BmpTerminationMessage(
                bytes_rec,
            )) => Some(bytes::Bytes::copy_from_slice(bytes_rec.as_ref())),
            _ => None,
        }
    }

    pub fn into_prefix_route(self) -> Result<PrefixRoute, Self> {
        if let TypeValue::Builtin(BuiltinTypeValue::PrefixRoute(route)) = self {
            Ok(route)
        } else {
            Err(self)
        }
    }

    pub fn into_flowspec_route(self) -> Result<FlowSpecRoute<bytes::Bytes>, Self> {
        if let TypeValue::Builtin(BuiltinTypeValue::FlowSpecRoute(route)) = self {
            Ok(route)
        } else {
            Err(self)
        }
    }
    // pub fn into_mp_variant_or_self(
    //     self,
    //     lazy_record_type: LazyRecordTypeDef,
    //     variant_field_index: &FieldIndex
    // ) -> Self {
    //         match self {
    //             TypeValue::Builtin(BuiltinTypeValue::BmpMessage(ref bytes_rec)) => {
    //                 if let Ok(var) = bytes_rec
    //                     .get_field_index_for_variant(
    //                         lazy_record_type,
    //                         variant_field_index,
    //                     ) {
    //                         var
    //                     } else { self }
    //                 // mem.set_mem_pos(mem_pos, v);
    //                 // stack.push(v.into())?;
    //             }
    //             _ => {
    //                 self
    //             }
    //         }
    //     }
}

pub enum BytesRecordType {
    InitiationMessage(
        routecore::bmp::message::InitiationMessage<bytes::Bytes>,
    ),
}

pub fn into_inner<T: RecordType>(tv: TypeValue) -> Option<BytesRecordType> {
    match tv {
        TypeValue::Builtin(BuiltinTypeValue::BmpInitiationMessage(msg)) => {
            Some(BytesRecordType::InitiationMessage(msg.into_inner()))
        }
        _ => None,
    }
}

impl RotoType for TypeValue {
    fn get_props_for_method(
        ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<super::typedef::MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match ty {
            TypeDef::AcceptReject(_) => Err(CompileError::new(
                "Unsupported TypeDef::AcceptReject in TypeValue::\
                get_props_for_method()"
                    .to_string(),
            )),
            TypeDef::Asn => Asn::get_props_for_method(ty, method_name),
            TypeDef::AsPath => HopPath::get_props_for_method(ty, method_name),
            TypeDef::AtomicAggregate => Err(CompileError::new(
                "Unsupported TypeDef::AtomicAggregator in TypeValue::\
                get_props_for_method()"
                    .to_string(),
            )),
            TypeDef::AggregatorInfo => Err(CompileError::new(
                "Unsupported TypeDef::Aggregator in TypeValue::\
                get_props_for_method()"
                    .to_string(),
            )),
            // TypeDef::BgpUpdateMessage => {
            //     BytesRecord::<BgpUpdateMessage>::get_props_for_method(ty, method_name)
            // }
            TypeDef::LazyRecord(ref bytes_parser) => {
                bytes_parser.get_props_for_method(ty.clone(), method_name)
            }
            TypeDef::Bool => bool::get_props_for_method(ty, method_name),
            TypeDef::Community => {
                Community::get_props_for_method(ty, method_name)
            }
            TypeDef::Nlri => Nlri::get_props_for_method(ty, method_name),
            TypeDef::ConstEnumVariant(_) => Err(CompileError::new(
                "Unsupported TypeDef::ConstEnumVariant in TypeValue::\
                get_props_for_method()"
                    .to_string(),
            )),
            TypeDef::GlobalEnum(_to) => Err(CompileError::new(
                "Unsupported TypeDef::GlobalEnum in TypeValue::\
                get_props_for_method()"
                    .to_string(),
            )),
            TypeDef::HexLiteral => {
                HexLiteral::get_props_for_method(ty, method_name)
            }
            TypeDef::Hop => Hop::get_props_for_method(ty, method_name),
            TypeDef::IntegerLiteral => {
                IntegerLiteral::get_props_for_method(ty, method_name)
            }
            TypeDef::IpAddr => IpAddr::get_props_for_method(ty, method_name),
            TypeDef::List(ty) => Self::get_props_for_method(*ty, method_name),
            TypeDef::LocalPref => {
                LocalPref::get_props_for_method(ty, method_name)
            }
            TypeDef::MultiExitDisc => {
                MultiExitDisc::get_props_for_method(ty, method_name)
            }
            TypeDef::NextHop => {
                NextHop::get_props_for_method(ty, method_name)
            }
            TypeDef::Origin => {
                Origin::get_props_for_method(ty, method_name)
            }
            TypeDef::OutputStream(ty) => {
                Self::get_props_for_method(*ty, method_name)
            }
            TypeDef::Prefix => {
                inetnum::addr::Prefix::get_props_for_method(ty, method_name)
            }
            TypeDef::PrefixLength => {
                PrefixLength::get_props_for_method(ty, method_name)
            }
            TypeDef::AfiSafiType => {
                AfiSafiType::get_props_for_method(ty, method_name)
            }
            TypeDef::PathId => PathId::get_props_for_method(ty, method_name),
            TypeDef::Record(_) => {
                Record::get_props_for_method(ty, method_name)
            }
            TypeDef::Rib(ty) => {
                Self::get_props_for_method(*ty.0, method_name)
            }
            TypeDef::PrefixRoute => {
                PrefixRoute::get_props_for_method(ty, method_name)
            }
            TypeDef::FlowSpecRoute => {
                FlowSpecRoute::get_props_for_method(ty, method_name)
            }
            TypeDef::RouteContext => {
                RouteContext::get_props_for_method(ty, method_name)
            }
            TypeDef::Provenance => {
                Provenance::get_props_for_method(ty, method_name)
            }
            TypeDef::PeerId => PeerId::get_props_for_method(ty, method_name),
            TypeDef::PeerRibType => {
                PeerRibType::get_props_for_method(ty, method_name)
            }
            TypeDef::NlriStatus => {
                NlriStatus::get_props_for_method(ty, method_name)
            }
            TypeDef::StringLiteral => {
                StringLiteral::get_props_for_method(ty, method_name)
            }
            TypeDef::Table(ty) => {
                Self::get_props_for_method(*ty, method_name)
            }
            TypeDef::U32 => u32::get_props_for_method(ty, method_name),
            TypeDef::U16 => u16::get_props_for_method(ty, method_name),
            TypeDef::U8 => u8::get_props_for_method(ty, method_name),
            TypeDef::Unknown => Err(CompileError::new(
                "Unsupported TypeDef::Unknown in TypeValue::\
                get_props_for_method()"
                    .to_string(),
            )),
        }
    }

    fn into_type(self, ty: &TypeDef) -> Result<TypeValue, CompileError>
    where
        Self: std::marker::Sized,
    {
        match self {
            TypeValue::Builtin(builtin) => match builtin {
                BuiltinTypeValue::Asn(v) => v.into_type(ty),
                BuiltinTypeValue::AsPath(v) => v.into_type(ty),
                BuiltinTypeValue::AggregatorInfo(v) => v.into_type(ty),
                BuiltinTypeValue::AtomicAggregate(v) => v.into_type(ty),
                BuiltinTypeValue::BgpUpdateMessage(_) => {
                    Err(CompileError::new(
                        "Unsupported TypeValue::BgpUpdateMessage in \
                    TypeValue::into_type()"
                            .to_string(),
                    ))
                }
                BuiltinTypeValue::Bool(v) => v.into_type(ty),
                BuiltinTypeValue::Community(v) => v.into_type(ty),
                BuiltinTypeValue::Nlri(v) => v.into_type(ty),
                BuiltinTypeValue::ConstU16EnumVariant(v) => v.into_type(ty),
                BuiltinTypeValue::ConstU32EnumVariant(v) => v.into_type(ty),
                BuiltinTypeValue::ConstU8EnumVariant(v) => v.into_type(ty),
                BuiltinTypeValue::HexLiteral(v) => v.into_type(ty),
                BuiltinTypeValue::Hop(v) => v.into_type(ty),
                BuiltinTypeValue::IntegerLiteral(v) => v.into_type(ty),
                BuiltinTypeValue::IpAddr(v) => v.into_type(ty),
                BuiltinTypeValue::LocalPref(v) => v.into_type(ty),
                BuiltinTypeValue::MultiExitDisc(v) => v.into_type(ty),
                BuiltinTypeValue::NextHop(v) => v.into_type(ty),
                BuiltinTypeValue::Origin(v) => v.into_type(ty),
                BuiltinTypeValue::Prefix(v) => v.into_type(ty),
                BuiltinTypeValue::PrefixLength(v) => v.into_type(ty),
                BuiltinTypeValue::AfiSafiType(v) => v.into_type(ty),
                BuiltinTypeValue::PathId(v) => v.into_type(ty),
                BuiltinTypeValue::PrefixRoute(v) => v.into_type(ty),
                BuiltinTypeValue::FlowSpecRoute(v) => v.into_type(ty),
                BuiltinTypeValue::RouteContext(c) => c.into_type(ty),
                BuiltinTypeValue::Provenance(v) => v.into_type(ty),
                BuiltinTypeValue::NlriStatus(v) => v.into_type(ty),
                BuiltinTypeValue::StringLiteral(v) => v.into_type(ty),
                BuiltinTypeValue::U32(v) => v.into_type(ty),
                BuiltinTypeValue::U16(v) => v.into_type(ty),
                BuiltinTypeValue::U8(v) => v.into_type(ty),
                BuiltinTypeValue::PeerId(v) => v.into_type(ty),
                BuiltinTypeValue::PeerRibType(v) => v.into_type(ty),
                BuiltinTypeValue::BmpMessage(_v) => Err(CompileError::new(
                    "Unsupported TypeValue::BmpMessage in TypeValue::\
                    into_type()"
                        .to_string(),
                )),
                BuiltinTypeValue::BmpRouteMonitoringMessage(_v) => {
                    Err(CompileError::new(
                        "Unsupported TypeValue::\
                        BmpRouteMonitoringMessage in TypeValue::into_type()"
                            .to_string(),
                    ))
                }
                BuiltinTypeValue::BmpPeerUpNotification(_v) => {
                    Err(CompileError::new(
                        "Unsupported TypeValue::BmpPeerUpNotification in \
                        TypeValue::into_type()"
                            .to_string(),
                    ))
                }
                BuiltinTypeValue::BmpPeerDownNotification(_v) => {
                    Err(CompileError::new(
                        "Unsupported TypeValue::\
                        BmpPeerDownNotification in TypeValue::into_type()"
                            .to_string(),
                    ))
                }
                BuiltinTypeValue::BmpInitiationMessage(_v) => {
                    Err(CompileError::new(
                        "Unsupported TypeValue::\
                        BmpInitiationMessage in TypeValue::into_type()"
                            .to_string(),
                    ))
                }
                BuiltinTypeValue::BmpTerminationMessage(_v) => {
                    Err(CompileError::new(
                        "Unsupported TypeValue::\
                        BmpTerminationMessage in TypeValue::into_type()"
                            .to_string(),
                    ))
                }
                BuiltinTypeValue::BmpStatisticsReport(_v) => {
                    Err(CompileError::new(
                        "Unsupported TypeValue::\
                        BmpStatisticsReport in TypeValue::into_type()"
                            .to_string(),
                    ))
                }
            },

            // TypeValue::Enum(v) => v.into_type(ty),
            TypeValue::List(v) => v.into_type(ty),
            TypeValue::OutputStreamMessage(_) => Err(CompileError::new(
                "Unsupported TypeValue::OutputStreamMessage in TypeValue\
                ::into_type()"
                    .to_string(),
            )),
            TypeValue::Record(v) => v.into_type(ty),
            TypeValue::SharedValue(v) => (*v).clone().into_type(ty),
            TypeValue::UnInit => Err(CompileError::new(
                "Unsupported TypeValue::UnInit in TypeValue::into_type()"
                    .to_string(),
            )),
            TypeValue::Unknown => Err(CompileError::new(
                "Unsupported TypeValue::Unknown in TypeValue::into_type()"
                    .to_string(),
            )),
        }
    }

    fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        args: &'a [StackValue],
        res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match self {
            TypeValue::Builtin(builtin) => match builtin {
                BuiltinTypeValue::Asn(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::AsPath(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::AggregatorInfo(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::AtomicAggregate(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::BgpUpdateMessage(bytes_rec) => {
                    BytesRecord::<BgpUpdateMessage>::exec_value_method(
                        method_token,
                        args,
                        res_type,
                        bytes_rec,
                    )
                }
                BuiltinTypeValue::BmpMessage(_bytes_rec) => {
                    Err(VmError::InvalidValueType)
                }
                BuiltinTypeValue::BmpRouteMonitoringMessage(bytes_rec) => {
                    LazyRecord::new(
                        BytesRecord::<RouteMonitoring>::lazy_type_def(),
                    )
                    .exec_value_method(
                        method_token,
                        args,
                        res_type,
                        bytes_rec.as_ref(),
                    )
                }
                BuiltinTypeValue::BmpPeerUpNotification(bytes_rec) => {
                    LazyRecord::new(
                        BytesRecord::<PeerUpNotification>::lazy_type_def(),
                    )
                    .exec_value_method(
                        method_token,
                        args,
                        res_type,
                        bytes_rec.as_ref(),
                    )
                }
                BuiltinTypeValue::BmpPeerDownNotification(bytes_rec) => {
                    LazyRecord::new(
                        BytesRecord::<PeerDownNotification>::lazy_type_def(),
                    )
                    .exec_value_method(
                        method_token,
                        args,
                        res_type,
                        bytes_rec.as_ref(),
                    )
                }
                BuiltinTypeValue::BmpInitiationMessage(bytes_rec) => {
                    LazyRecord::new(
                        BytesRecord::<InitiationMessage>::lazy_type_def(),
                    )
                    .exec_value_method(
                        method_token,
                        args,
                        res_type,
                        bytes_rec.as_ref(),
                    )
                }
                BuiltinTypeValue::BmpTerminationMessage(bytes_rec) => {
                    LazyRecord::new(
                        BytesRecord::<TerminationMessage>::lazy_type_def(),
                    )
                    .exec_value_method(
                        method_token,
                        args,
                        res_type,
                        bytes_rec.as_ref(),
                    )
                }
                BuiltinTypeValue::BmpStatisticsReport(bytes_rec) => {
                    LazyRecord::new(
                        BytesRecord::<StatisticsReport>::lazy_type_def(),
                    )
                    .exec_value_method(
                        method_token,
                        args,
                        res_type,
                        bytes_rec.as_ref(),
                    )
                }
                BuiltinTypeValue::Bool(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::Community(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::Nlri(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::ConstU16EnumVariant(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::ConstU32EnumVariant(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::ConstU8EnumVariant(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::HexLiteral(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::Hop(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::IntegerLiteral(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::IpAddr(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::LocalPref(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::MultiExitDisc(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::NextHop(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::Origin(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::Prefix(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::AfiSafiType(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::PathId(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::PrefixLength(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::PrefixRoute(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::FlowSpecRoute(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::RouteContext(c) => {
                    c.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::Provenance(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::NlriStatus(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::StringLiteral(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::U32(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::U16(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::U8(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::PeerId(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::PeerRibType(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
            },
            TypeValue::List(v) => {
                v.exec_value_method(method_token, args, res_type)
            }
            TypeValue::OutputStreamMessage(v) => {
                v.exec_value_method(method_token, args, res_type)
            }
            TypeValue::Record(v) => {
                v.exec_value_method(method_token, args, res_type)
            }
            TypeValue::SharedValue(v) => {
                v.exec_value_method(method_token, args, res_type)
            }
            TypeValue::UnInit => Err(VmError::InvalidValueType),
            TypeValue::Unknown => Err(VmError::InvalidValueType),
        }
    }

    fn exec_consume_value_method(
        self,
        method_token: usize,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match self {
            TypeValue::Builtin(builtin) => match builtin {
                BuiltinTypeValue::Asn(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::AsPath(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::AggregatorInfo(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::AtomicAggregate(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::BgpUpdateMessage(_) => {
                    Err(VmError::InvalidValueType)
                }
                BuiltinTypeValue::BmpMessage(bytes_rec) => bytes_rec
                    .exec_consume_value_method(method_token, args, res_type),
                BuiltinTypeValue::BmpRouteMonitoringMessage(_) => {
                    Err(VmError::InvalidValueType)
                }
                BuiltinTypeValue::BmpPeerUpNotification(_) => {
                    Err(VmError::InvalidValueType)
                }
                BuiltinTypeValue::BmpPeerDownNotification(_) => {
                    Err(VmError::InvalidValueType)
                }
                BuiltinTypeValue::BmpInitiationMessage(_) => {
                    Err(VmError::InvalidValueType)
                }
                BuiltinTypeValue::BmpTerminationMessage(_) => {
                    Err(VmError::InvalidValueType)
                }
                BuiltinTypeValue::BmpStatisticsReport(_) => {
                    Err(VmError::InvalidValueType)
                }
                BuiltinTypeValue::Bool(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::Community(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::Nlri(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::ConstU16EnumVariant(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::ConstU32EnumVariant(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::ConstU8EnumVariant(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::HexLiteral(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::Hop(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::IntegerLiteral(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::IpAddr(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::LocalPref(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::MultiExitDisc(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::NextHop(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::Origin(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::Prefix(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::AfiSafiType(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::PathId(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::PrefixLength(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::PrefixRoute(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::FlowSpecRoute(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::RouteContext(c) => {
                    c.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::Provenance(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::NlriStatus(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::StringLiteral(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::U32(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::U16(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::U8(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::PeerId(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::PeerRibType(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
            },
            // TypeValue::Enum(v) => {
            //     v.exec_consume_value_method(method_token, args, res_type)
            // }
            TypeValue::List(v) => {
                v.exec_consume_value_method(method_token, args, res_type)
            }
            TypeValue::OutputStreamMessage(_) => {
                Err(VmError::InvalidValueType)
            }
            TypeValue::Record(v) => {
                v.exec_consume_value_method(method_token, args, res_type)
            }
            TypeValue::SharedValue(_) => Err(VmError::InvalidValueType),
            TypeValue::UnInit => Err(VmError::InvalidValueType),
            TypeValue::Unknown => Err(VmError::InvalidValueType),
        }
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

impl Display for TypeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if !f.alternate() {
            match self {
                TypeValue::Builtin(p) => write!(f, "{}", p),
                TypeValue::List(l) => write!(f, "{}", l),
                TypeValue::Record(r) => {
                    write!(f, "{}", r)
                }
                // TypeValue::Enum(c_enum) => {
                //     write!(f, "{}", c_enum)
                // }
                TypeValue::OutputStreamMessage(m) => {
                    write!(f, "{}", m)
                }
                TypeValue::SharedValue(sv) => write!(f, "{}", sv),
                TypeValue::Unknown => write!(f, "Unknown"),
                TypeValue::UnInit => write!(f, "Uninitialized"),
            }
        } else {
            // The pretty printer "{:?}"
            match self {
                TypeValue::Builtin(p) => write!(f, "{}", p),
                TypeValue::List(l) => write!(f, "{} (List)", l),
                TypeValue::Record(r) => {
                    write!(f, "{} (Record)", r)
                }
                // TypeValue::Enum(c_enum) => {
                //     write!(f, "{} (Enum)", c_enum)
                // }
                TypeValue::OutputStreamMessage(m) => {
                    write!(f, "{} (Stream message)", m)
                }
                TypeValue::SharedValue(sv) => {
                    write!(f, "{} (Shared Value)", sv)
                }
                TypeValue::Unknown => write!(f, "Unknown"),
                TypeValue::UnInit => write!(f, "Uninitialized"),
            }
        }
    }
}

// This PartialEq is used at VM runtime to convert types when performing a
// `Cmp` operation. The `Cmp` operation just performs a
// `<TypeValue> == <TypeValue>` operation (in Rust), that comes here. Note
// that at this point NO errors or panics are created for invalid type
// comparisons. Lower-level types (types that are wrapped by TypeValue) do
// not implement type-conversions through their PartialEq impls (they use)
// derive). They do have `into_type()` methods, though, that are called here.
impl PartialEq for TypeValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Builtins

            // Builtins to Builtins
            (TypeValue::Builtin(b1), TypeValue::Builtin(b2)) => {
                if b1 == b2 {
                    return true;
                }
                if let Ok(TypeValue::Builtin(b2_convert)) =
                    b2.clone().into_type(&(self.into())).as_ref()
                {
                    b1 == b2_convert
                } else {
                    false
                }
            }

            (TypeValue::Builtin(_), _) => false,

            // Lists

            // Lists to Lists
            (TypeValue::List(l1), TypeValue::List(l2)) => {
                if l1.0[..] == l2.0[..] {
                    true
                } else if l1.0.len() != l2.0.len() {
                    false
                } else {
                    l1.0.iter().enumerate().all(|(i, elm)| {
                        if let Ok(tv) =
                            l2.0[i].clone().into_type(&(elm.into()))
                        {
                            elm == &tv
                        } else {
                            false
                        }
                    })
                }
            }
            (TypeValue::List(_), _) => false,

            // Records

            // Records to Records
            (TypeValue::Record(r1), TypeValue::Record(r2)) => {
                if r1.get_values() == r2.get_values() {
                    true
                } else if r1.len() != r2.len() {
                    false
                } else {
                    r1.iter().enumerate().all(|(i, elm)| {
                        if let Some(ktv) = r2.get_field_by_single_index(i) {
                            if let Ok(tv) =
                                ktv.1.clone().into_type(&(&elm.1).into())
                            {
                                elm.1 == tv
                            } else {
                                false
                            }
                        } else {
                            false
                        }
                    })
                }
            }
            (TypeValue::Record(_), _) => false,

            // SharedValues

            // SharedValues (coming out of data-sources)
            (TypeValue::SharedValue(sv1), TypeValue::SharedValue(sv2)) => {
                sv1.as_ref() == sv2.as_ref()
            }
            (TypeValue::SharedValue(sv1), tv) => sv1.as_ref() == tv,

            // Remaining TypeValues to SharedValues
            (tv, TypeValue::SharedValue(sv)) => {
                if tv == sv.as_ref() {
                    return true;
                }
                if let Ok(ref sv_convert) =
                    sv.as_ref().clone().into_type(&(self.into()))
                {
                    tv == sv_convert
                } else {
                    false
                }
            }

            (TypeValue::Unknown, TypeValue::Unknown) => true,
            // Inconvertible & Incomparable types
            (TypeValue::Unknown, _) => false,
            (TypeValue::UnInit, _) => false,
            (TypeValue::OutputStreamMessage(_), _) => false,
            // (TypeValue::Enum(_), _) => false,
        }
    }
}

impl std::hash::Hash for TypeValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            TypeValue::Builtin(bv) => {
                bv.hash(state);
            }
            TypeValue::List(lv) => {
                lv.hash(state);
            }
            TypeValue::Record(rec) => {
                rec.hash(state);
            }
            // TypeValue::Enum(e) => {
            //     e.hash(state);
            // }
            // Shouldn't appear in the payload out (tx)
            TypeValue::OutputStreamMessage(_msg) => {}
            TypeValue::SharedValue(sv) => {
                sv.hash(state);
            }
            // Fields that can result in an unknown value Shouldn't be used
            // in a hash calculation
            TypeValue::Unknown => {}
            // Shouldn't appear here
            TypeValue::UnInit => {}
        }
    }
}

impl PartialOrd for TypeValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (
                TypeValue::Builtin(BuiltinTypeValue::U8(u)),
                TypeValue::Builtin(BuiltinTypeValue::U8(v)),
            ) => Some(u.cmp(v)),
            (
                TypeValue::Builtin(BuiltinTypeValue::U32(u)),
                TypeValue::Builtin(BuiltinTypeValue::U32(v)),
            ) => Some(u.cmp(v)),
            (
                TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(
                    IntegerLiteral(u),
                )),
                TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(
                    IntegerLiteral(v),
                )),
            ) => Some(u.cmp(v)),
            (
                TypeValue::Builtin(BuiltinTypeValue::StringLiteral(
                    StringLiteral(u),
                )),
                TypeValue::Builtin(BuiltinTypeValue::StringLiteral(
                    StringLiteral(v),
                )),
            ) => Some(u.cmp(v)),
            (
                TypeValue::Builtin(BuiltinTypeValue::Bool(u)),
                TypeValue::Builtin(BuiltinTypeValue::Bool(v)),
            ) => Some(u.cmp(v)),
            (
                TypeValue::Builtin(BuiltinTypeValue::Prefix(u)),
                TypeValue::Builtin(BuiltinTypeValue::Prefix(v)),
            ) => Some(u.cmp(v)),
            (
                TypeValue::Builtin(BuiltinTypeValue::PrefixLength(
                    PrefixLength(u),
                )),
                TypeValue::Builtin(BuiltinTypeValue::PrefixLength(
                    PrefixLength(v),
                )),
            ) => Some(u.cmp(v)),
            (
                TypeValue::Builtin(BuiltinTypeValue::Community(_)),
                TypeValue::Builtin(BuiltinTypeValue::Community(_)),
            ) => {
                debug!("Communities have no ordering.");
                None
            }
            (
                TypeValue::Builtin(BuiltinTypeValue::IpAddr(u)),
                TypeValue::Builtin(BuiltinTypeValue::IpAddr(v)),
            ) => Some(u.cmp(v)),
            (
                TypeValue::Builtin(BuiltinTypeValue::Asn(u)),
                TypeValue::Builtin(BuiltinTypeValue::Asn(v)),
            ) => Some(u.cmp(v)),
            (
                TypeValue::Builtin(BuiltinTypeValue::AsPath(_)),
                TypeValue::Builtin(BuiltinTypeValue::AsPath(_)),
            ) => {
                debug!("AS Paths have no ordering.");
                None
            }
            (
                TypeValue::Builtin(BuiltinTypeValue::PrefixRoute(_)),
                TypeValue::Builtin(BuiltinTypeValue::PrefixRoute(_)),
            ) => {
                debug!("Routes have no ordering.");
                None
            }
            (
                TypeValue::Builtin(BuiltinTypeValue::NlriStatus(_)),
                TypeValue::Builtin(BuiltinTypeValue::NlriStatus(_)),
            ) => {
                debug!("Route statuses have no ordering.");
                None
            }
            (
                TypeValue::Builtin(BuiltinTypeValue::HexLiteral(HexLiteral(
                    u,
                ))),
                TypeValue::Builtin(BuiltinTypeValue::HexLiteral(HexLiteral(
                    v,
                ))),
            ) => Some(u.cmp(v)),
            (TypeValue::List(_), TypeValue::List(_)) => {
                debug!("Lists are not comparable.");
                None
            }
            (TypeValue::Record(_), TypeValue::Record(_)) => {
                debug!("Records are not comparable.");
                None
            }
            (TypeValue::Unknown, TypeValue::Unknown) => {
                debug!("Unknown is unsortable.");
                None
            }
            _ => {
                debug!("Incomparable types.");
                None
            }
        }
    }
}

impl<'a> TryFrom<StackValue<'a>> for bool {
    type Error = VmError;

    fn try_from(t: StackValue) -> Result<Self, Self::Error> {
        match t {
            StackValue::Ref(TypeValue::Builtin(BuiltinTypeValue::Bool(
                ref b,
            ))) => Ok(*b),
            StackValue::Arc(bv) => {
                if let TypeValue::Builtin(BuiltinTypeValue::Bool(ref b)) = *bv
                {
                    Ok(*b)
                } else {
                    Err(VmError::ImpossibleComparison)
                }
            }
            _ => Err(VmError::ImpossibleComparison),
        }
    }
}

impl<'a> TryFrom<&'a TypeValue> for bool {
    type Error = VmError;

    fn try_from(t: &TypeValue) -> Result<Self, Self::Error> {
        match t {
            TypeValue::Builtin(BuiltinTypeValue::Bool(ref b)) => Ok(*b),
            _ => Err(VmError::ImpossibleComparison),
        }
    }
}

impl From<BuiltinTypeValue> for TypeValue {
    fn from(t: BuiltinTypeValue) -> Self {
        TypeValue::Builtin(t)
    }
}

impl From<&'_ str> for TypeValue {
    fn from(value: &str) -> Self {
        StringLiteral(String::from(value)).into()
    }
}

// impl ScalarValue for TypeValue {}

// Type conversions for Records

// Records do not know how their String- and Integerliterals are going to be
// used/converted, so they store them as actual TypeValue::*Literal variants

impl From<crate::ast::StringLiteral> for TypeValue {
    fn from(value: crate::ast::StringLiteral) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::StringLiteral(StringLiteral(
            value.0,
        )))
    }
}

impl From<crate::ast::IntegerLiteral> for TypeValue {
    fn from(value: crate::ast::IntegerLiteral) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(IntegerLiteral(
            value.0,
        )))
    }
}

impl From<&'_ crate::ast::IpAddress> for TypeValue {
    fn from(value: &crate::ast::IpAddress) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::IpAddr(value.into()))
    }
}

impl TryFrom<&'_ crate::ast::Prefix> for TypeValue {
    type Error = CompileError;
    fn try_from(value: &crate::ast::Prefix) -> Result<Self, Self::Error> {
        Ok(TypeValue::Builtin(BuiltinTypeValue::Prefix(
            value.try_into()?,
        )))
    }
}

impl From<crate::ast::PrefixLengthLiteral> for TypeValue {
    fn from(value: crate::ast::PrefixLengthLiteral) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::PrefixLength(PrefixLength(
            value.0,
        )))
    }
}

impl From<crate::ast::AsnLiteral> for TypeValue {
    fn from(value: crate::ast::AsnLiteral) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Asn(value.0.into()))
    }
}

impl TryFrom<crate::ast::StandardCommunityLiteral> for TypeValue {
    type Error = CompileError;

    fn try_from(
        value: crate::ast::StandardCommunityLiteral,
    ) -> Result<Self, Self::Error> {
        let comm = value.0.into();
        Ok(TypeValue::Builtin(BuiltinTypeValue::Community(comm)))
    }
}

impl TryFrom<crate::ast::ExtendedCommunityLiteral> for TypeValue {
    type Error = CompileError;

    fn try_from(
        value: crate::ast::ExtendedCommunityLiteral,
    ) -> Result<Self, Self::Error> {
        let comm = value.0.into();
        Ok(TypeValue::Builtin(BuiltinTypeValue::Community(comm)))
    }
}

impl TryFrom<crate::ast::LargeCommunityLiteral> for TypeValue {
    type Error = CompileError;

    fn try_from(
        value: crate::ast::LargeCommunityLiteral,
    ) -> Result<Self, Self::Error> {
        let comm = value.0.into();
        Ok(TypeValue::Builtin(BuiltinTypeValue::Community(comm)))
    }
}

impl From<crate::ast::HexLiteral> for TypeValue {
    fn from(value: crate::ast::HexLiteral) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::HexLiteral(HexLiteral(value.0)))
    }
}

impl From<crate::ast::BooleanLiteral> for TypeValue {
    fn from(value: crate::ast::BooleanLiteral) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Bool(value.0))
    }
}

impl TryFrom<ListValueExpr> for TypeValue {
    type Error = CompileError;

    fn try_from(value: ListValueExpr) -> Result<Self, Self::Error> {
        Ok(TypeValue::List(value.try_into()?))
    }
}

impl TryFrom<AnonymousRecordValueExpr> for TypeValue {
    type Error = CompileError;

    fn try_from(
        value: AnonymousRecordValueExpr,
    ) -> Result<Self, Self::Error> {
        Ok(TypeValue::Record(value.try_into()?))
    }
}

impl TryFrom<TypedRecordValueExpr> for TypeValue {
    type Error = CompileError;

    fn try_from(value: TypedRecordValueExpr) -> Result<Self, Self::Error> {
        Ok(TypeValue::Record(Record::try_from(value)?))
    }
}

impl From<RouteMonitoring> for TypeValue {
    fn from(value: RouteMonitoring) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::BmpRouteMonitoringMessage(
            value.into(),
        ))
    }
}

impl From<InitiationMessage> for TypeValue {
    fn from(value: InitiationMessage) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::BmpInitiationMessage(
            value.into(),
        ))
    }
}

impl From<TerminationMessage> for TypeValue {
    fn from(value: TerminationMessage) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::BmpTerminationMessage(
            value.into(),
        ))
    }
}

impl From<PeerUpNotification> for TypeValue {
    fn from(value: PeerUpNotification) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::BmpPeerUpNotification(
            value.into(),
        ))
    }
}

impl From<PeerDownNotification> for TypeValue {
    fn from(value: PeerDownNotification) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::BmpPeerDownNotification(
            value.into(),
        ))
    }
}

impl From<StatisticsReport> for TypeValue {
    fn from(value: StatisticsReport) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::BmpStatisticsReport(
            value.into(),
        ))
    }
}

impl From<std::net::SocketAddr> for TypeValue {
    fn from(value: std::net::SocketAddr) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::StringLiteral(
            value.into()
        ))
    }
}

impl From<RouteWorkshop<Ipv4UnicastNlri>> for TypeValue {
    fn from(value: RouteWorkshop<Ipv4UnicastNlri>) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::PrefixRoute(PrefixRoute(
            PrefixRouteWs::Ipv4Unicast(
                value
            )
        )))
    }
}

impl From<RouteWorkshop<Ipv4UnicastAddpathNlri>> for TypeValue {
    fn from(value: RouteWorkshop<Ipv4UnicastAddpathNlri>) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::PrefixRoute(PrefixRoute(
            PrefixRouteWs::Ipv4UnicastAddpath(
                value
            )
        )))
    }
}

impl From<RouteWorkshop<Ipv6UnicastNlri>> for TypeValue {
    fn from(value: RouteWorkshop<Ipv6UnicastNlri>) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::PrefixRoute(PrefixRoute(
            PrefixRouteWs::Ipv6Unicast(
                value
            )
        )))
    }
}

impl From<RouteWorkshop<Ipv6UnicastAddpathNlri>> for TypeValue {
    fn from(value: RouteWorkshop<Ipv6UnicastAddpathNlri>) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::PrefixRoute(PrefixRoute(
            PrefixRouteWs::Ipv6UnicastAddpath(
                value
            )
        )))
    }
}

impl From<RouteWorkshop<Ipv4MulticastNlri>> for TypeValue {
    fn from(value: RouteWorkshop<Ipv4MulticastNlri>) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::PrefixRoute(PrefixRoute(
            PrefixRouteWs::Ipv4Multicast(
                value
            )
        )))
    }
}

impl From<RouteWorkshop<Ipv4MulticastAddpathNlri>> for TypeValue {
    fn from(value: RouteWorkshop<Ipv4MulticastAddpathNlri>) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::PrefixRoute(PrefixRoute(
            PrefixRouteWs::Ipv4MulticastAddpath(
                value
            )
        )))
    }
}

impl From<RouteWorkshop<Ipv6MulticastNlri>> for TypeValue {
    fn from(value: RouteWorkshop<Ipv6MulticastNlri>) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::PrefixRoute(PrefixRoute(
            PrefixRouteWs::Ipv6Multicast(
                value
            )
        )))
    }
}

impl From<RouteWorkshop<Ipv6MulticastAddpathNlri>> for TypeValue {
    fn from(value: RouteWorkshop<Ipv6MulticastAddpathNlri>) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::PrefixRoute(PrefixRoute(
            PrefixRouteWs::Ipv6MulticastAddpath(
                value
            )
        )))
    }
}

/*
impl From<RouteWorkshop<Ipv6FlowSpecNlri<bytes::Bytes>>> for TypeValue {
    fn from(value: RouteWorkshop<Ipv6FlowSpecNlri<bytes::Bytes>>) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::FlowSpecRoute(FlowSpecRoute {
            nlri: FlowSpecNlri::Ipv6FlowSpec(value.nlri().clone()),
            attributes: value.attributes().clone(),
        }))
    }
}
*/
