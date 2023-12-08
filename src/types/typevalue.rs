use std::{cmp::Ordering, fmt::Display, sync::Arc};

use log::debug;
use primitives::{
    AsPath, Community, Hop, LocalPref, MultiExitDisc, NextHop, OriginType,
    RouteStatus, U16,
};
use serde::Serialize;

//============ TypeValue ====================================================
use crate::{
    ast::{
        AnonymousRecordValueExpr, ListValueExpr, ShortString,
        TypedRecordValueExpr,
    },
    attr_change_set::ScalarValue,
    compiler::compile::CompileError,
    traits::{RotoType, Token},
    vm::{StackValue, VmError, FieldIndex},
};

use super::{
    builtin::{
        primitives, Asn, BgpUpdateMessage, Boolean, BuiltinTypeValue,
        HexLiteral, IntegerLiteral, IpAddress, Prefix, PrefixLength,
        RawRouteWithDeltas, StringLiteral, U32, U8,
    },
    collections::{BytesRecord, ElementTypeValue, LazyRecord, List, Record, EnumBytesRecord},
    lazyrecord_types::{
        InitiationMessage, PeerDownNotification, PeerUpNotification,
        RouteMonitoring,
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
    // All the built-in scalars and vectors
    Builtin(BuiltinTypeValue),
    // An ordered list of one user-defined type
    List(List),
    // A map of (key, value) pairs, where value can be any of the other types.
    // Always user-defined.
    Record(Record),
    // Enum(Enum),
    // A Record meant to be handled by an Output stream.
    OutputStreamMessage(Arc<OutputStreamMessage>),
    // A wrapper around an immutable value that lives in an external
    // datasource, i.e. a table or a rib
    SharedValue(Arc<TypeValue>),
    // Unknown is NOT EQUAL to empty or unitialized, e.g. it may be the
    // result of a search. A ternary logic value, if you will.
    Unknown,
    // Used for LinearMemory only, it's the initial state of all positions
    // except the first two positions (rx and tx). Taking a typevalue in the
    // vm runtime, also puts this value in its place.
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
            elems.push((ShortString::from(ident), ty.try_into().map_err(|e: VmError| CompileError::from(e.to_string()))?));
        }
        // let elems = type_ident_pairs
        //     .into_iter()
        //     .map(|(ident, ty)| (ShortString::from(ident), ty.try_into().map_err(|e: VmError| CompileError::from("bla"))? ))
        //     .collect::<Vec<_>>();

        Ok(Record::new(elems))
    }

    pub fn is_boolean_type(&self) -> bool {
        matches!(self, TypeValue::Builtin(BuiltinTypeValue::Boolean(_)))
    }

    pub fn is_false(&self) -> Result<bool, VmError> {
        if let TypeValue::Builtin(BuiltinTypeValue::Boolean(bool_val)) = self
        {
            Ok(bool_val.is_false())
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

    pub fn mp_is_variant(
        &self,
        variant_token: Token,
    ) -> bool {
        match self {
            // We only know how to deal with BmpMessages currently.
            TypeValue::Builtin(BuiltinTypeValue::BmpMessage(
                bytes_rec,
            )) => bytes_rec.is_variant(variant_token),
            _ => false,
        }
    }

    // Return a TypeValue if the memory holds the enum variant specified by
    // the varian_token. If the memory position holds an enum, but not of the
    // specified variant, then return TypeValue::Unknown.
    pub fn get_mp_as_variant_or_unknown(
        &self,
        variant_token: Token,
    ) -> Result<TypeValue, VmError> {
            if let TypeValue::Builtin(BuiltinTypeValue::BmpMessage(
                bytes_rec,
            )) = self
            {
                if bytes_rec.is_variant(variant_token) {
                    Ok(self.clone())
                } else {
                    Ok(TypeValue::Unknown)
                }
            } else {
                Err(VmError::InvalidValueType)
            }
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
            TypeDef::AsPath => AsPath::get_props_for_method(ty, method_name),
            TypeDef::AtomicAggregate => Err(CompileError::new(
                "Unsupported TypeDef::AtomicAggregator in TypeValue::\
                get_props_for_method()"
                    .to_string(),
            )),
            TypeDef::Aggregator => Err(CompileError::new(
                "Unsupported TypeDef::Aggregator in TypeValue::\
                get_props_for_method()"
                    .to_string(),
            )),
            TypeDef::BgpUpdateMessage => {
                BgpUpdateMessage::get_props_for_method(ty, method_name)
            }
            TypeDef::LazyRecord(ref bytes_parser) => {
                bytes_parser.get_props_for_method(ty.clone(), method_name)
            }
            TypeDef::Boolean => {
                Boolean::get_props_for_method(ty, method_name)
            }
            TypeDef::Community => {
                Community::get_props_for_method(ty, method_name)
            }
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
            TypeDef::IpAddress => {
                IpAddress::get_props_for_method(ty, method_name)
            }
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
            TypeDef::OriginType => {
                OriginType::get_props_for_method(ty, method_name)
            }
            TypeDef::OutputStream(ty) => {
                Self::get_props_for_method(*ty, method_name)
            }
            TypeDef::Prefix => Prefix::get_props_for_method(ty, method_name),
            TypeDef::PrefixLength => {
                PrefixLength::get_props_for_method(ty, method_name)
            }
            TypeDef::Record(_) => {
                Record::get_props_for_method(ty, method_name)
            }
            TypeDef::Rib(ty) => {
                Self::get_props_for_method(*ty.0, method_name)
            }
            TypeDef::Route => {
                RawRouteWithDeltas::get_props_for_method(ty, method_name)
            }
            TypeDef::RouteStatus => {
                RouteStatus::get_props_for_method(ty, method_name)
            }
            TypeDef::StringLiteral => {
                StringLiteral::get_props_for_method(ty, method_name)
            }
            TypeDef::Table(ty) => {
                Self::get_props_for_method(*ty, method_name)
            }
            TypeDef::U32 => U32::get_props_for_method(ty, method_name),
            TypeDef::U16 => U16::get_props_for_method(ty, method_name),
            TypeDef::U8 => U8::get_props_for_method(ty, method_name),
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
                BuiltinTypeValue::Aggregator(v) => v.into_type(ty),
                BuiltinTypeValue::AtomicAggregate(v) => v.into_type(ty),
                BuiltinTypeValue::BgpUpdateMessage(_) => {
                    Err(CompileError::new(
                        "Unsupported TypeValue::BgpUpdateMessage in \
                    TypeValue::into_type()"
                            .to_string(),
                    ))
                }
                BuiltinTypeValue::Boolean(v) => v.into_type(ty),
                BuiltinTypeValue::Community(v) => v.into_type(ty),
                BuiltinTypeValue::ConstU16EnumVariant(v) => v.into_type(ty),
                BuiltinTypeValue::ConstU32EnumVariant(v) => v.into_type(ty),
                BuiltinTypeValue::ConstU8EnumVariant(v) => v.into_type(ty),
                BuiltinTypeValue::HexLiteral(v) => v.into_type(ty),
                BuiltinTypeValue::Hop(v) => v.into_type(ty),
                BuiltinTypeValue::IntegerLiteral(v) => v.into_type(ty),
                BuiltinTypeValue::IpAddress(v) => v.into_type(ty),
                BuiltinTypeValue::LocalPref(v) => v.into_type(ty),
                BuiltinTypeValue::MultiExitDisc(v) => v.into_type(ty),
                BuiltinTypeValue::NextHop(v) => v.into_type(ty),
                BuiltinTypeValue::OriginType(v) => v.into_type(ty),
                BuiltinTypeValue::Prefix(v) => v.into_type(ty),
                BuiltinTypeValue::PrefixLength(v) => v.into_type(ty),
                BuiltinTypeValue::Route(v) => v.into_type(ty),
                BuiltinTypeValue::RouteStatus(v) => v.into_type(ty),
                BuiltinTypeValue::StringLiteral(v) => v.into_type(ty),
                BuiltinTypeValue::U32(v) => v.into_type(ty),
                BuiltinTypeValue::U16(v) => v.into_type(ty),
                BuiltinTypeValue::U8(v) => v.into_type(ty),
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
                BuiltinTypeValue::BmpInitationMessage(_v) => {
                    Err(CompileError::new(
                        "Unsupported TypeValue::\
                        BmpInitiationMessage in TypeValue::into_type()"
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
                BuiltinTypeValue::Aggregator(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::AtomicAggregate(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::BgpUpdateMessage(v) => {
                    v.exec_value_method(method_token, args, res_type)
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
                        bytes_rec.0.as_ref(),
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
                        bytes_rec.0.as_ref(),
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
                        bytes_rec.0.as_ref(),
                    )
                }
                BuiltinTypeValue::BmpInitationMessage(bytes_rec) => {
                    LazyRecord::new(
                        BytesRecord::<InitiationMessage>::lazy_type_def(),
                    )
                    .exec_value_method(
                        method_token,
                        args,
                        res_type,
                        bytes_rec.0.as_ref(),
                    )
                }
                BuiltinTypeValue::Boolean(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::Community(v) => {
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
                BuiltinTypeValue::IpAddress(v) => {
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
                BuiltinTypeValue::OriginType(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::Prefix(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::PrefixLength(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::Route(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::RouteStatus(v) => {
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
                BuiltinTypeValue::Aggregator(v) => {
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
                BuiltinTypeValue::BmpInitationMessage(_) => {
                    Err(VmError::InvalidValueType)
                }
                BuiltinTypeValue::Boolean(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::Community(v) => {
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
                BuiltinTypeValue::IpAddress(v) => {
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
                BuiltinTypeValue::OriginType(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::Prefix(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::PrefixLength(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::Route(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::RouteStatus(v) => {
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
                TypeValue::Builtin(BuiltinTypeValue::U8(U8(u))),
                TypeValue::Builtin(BuiltinTypeValue::U8(U8(v))),
            ) => Some(u.cmp(v)),
            (
                TypeValue::Builtin(BuiltinTypeValue::U32(U32(u))),
                TypeValue::Builtin(BuiltinTypeValue::U32(U32(v))),
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
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(u))),
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(v))),
            ) => Some(u.cmp(v)),
            (
                TypeValue::Builtin(BuiltinTypeValue::Prefix(Prefix(u))),
                TypeValue::Builtin(BuiltinTypeValue::Prefix(Prefix(v))),
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
                TypeValue::Builtin(BuiltinTypeValue::IpAddress(IpAddress(u))),
                TypeValue::Builtin(BuiltinTypeValue::IpAddress(IpAddress(v))),
            ) => Some(u.cmp(v)),
            (
                TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(u))),
                TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(v))),
            ) => Some(u.cmp(v)),
            (
                TypeValue::Builtin(BuiltinTypeValue::AsPath(_)),
                TypeValue::Builtin(BuiltinTypeValue::AsPath(_)),
            ) => {
                debug!("AS Paths have no ordering.");
                None
            }
            (
                TypeValue::Builtin(BuiltinTypeValue::Route(_)),
                TypeValue::Builtin(BuiltinTypeValue::Route(_)),
            ) => {
                debug!("Routes have no ordering.");
                None
            }
            (
                TypeValue::Builtin(BuiltinTypeValue::RouteStatus(_)),
                TypeValue::Builtin(BuiltinTypeValue::RouteStatus(_)),
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
            StackValue::Ref(TypeValue::Builtin(
                BuiltinTypeValue::Boolean(ref b),
            )) => Ok(b.0),
            StackValue::Arc(bv) => {
                if let TypeValue::Builtin(BuiltinTypeValue::Boolean(ref b)) =
                    *bv
                {
                    Ok(b.0)
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
            TypeValue::Builtin(BuiltinTypeValue::Boolean(ref b)) => Ok(b.0),
            _ => Err(VmError::ImpossibleComparison),
        }
    }
}

impl From<BuiltinTypeValue> for TypeValue {
    fn from(t: BuiltinTypeValue) -> Self {
        TypeValue::Builtin(t)
    }
}

//------------ Client-side From implementations -----------------------------

// These are the impl's for the end-users sake, so they can use:
// `TypeValue::from(my_value)`, where `my_value` is the Rust primitive type
// (u, u32, bool, etc.) or the routecore type (Prefix, Asn, etc.)

impl From<u32> for TypeValue {
    fn from(value: u32) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::U32(U32(value)))
    }
}

impl TryInto<u32> for &TypeValue {
    type Error = VmError;

    fn try_into(self) -> Result<u32, Self::Error> {
        if let TypeValue::Builtin(BuiltinTypeValue::U32(U32(value))) = self {
            Ok(*value)
        } else {
            Err(VmError::InvalidValueType)
        }
    }
}

impl From<u8> for TypeValue {
    fn from(value: u8) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::U8(U8(value)))
    }
}

impl From<bool> for TypeValue {
    fn from(val: bool) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(val)))
    }
}

impl From<&'_ str> for TypeValue {
    fn from(value: &str) -> Self {
        StringLiteral(String::from(value)).into()
    }
}

impl From<primitives::RouteStatus> for TypeValue {
    fn from(val: primitives::RouteStatus) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::RouteStatus(val))
    }
}

impl From<routecore::addr::Prefix> for TypeValue {
    fn from(value: routecore::addr::Prefix) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Prefix(primitives::Prefix(
            value,
        )))
    }
}

impl TryFrom<&TypeValue> for routecore::addr::Prefix {
    type Error = VmError;

    fn try_from(value: &TypeValue) -> Result<Self, Self::Error> {
        if let TypeValue::Builtin(BuiltinTypeValue::Prefix(
            primitives::Prefix(pfx),
        )) = value
        {
            Ok(*pfx)
        } else {
            Err(VmError::InvalidConversion)
        }
    }
}

impl From<std::net::IpAddr> for TypeValue {
    fn from(ip_addr: std::net::IpAddr) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::IpAddress(
            primitives::IpAddress(ip_addr),
        ))
    }
}

impl From<routecore::asn::Asn> for TypeValue {
    fn from(value: routecore::asn::Asn) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(value)))
    }
}

impl From<routecore::bgp::aspath::HopPath> for TypeValue {
    fn from(value: routecore::bgp::aspath::HopPath) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::AsPath(
            primitives::AsPath::from(value),
        ))
    }
}

impl From<routecore::bgp::aspath::HopPath> for BuiltinTypeValue {
    fn from(as_path: routecore::bgp::aspath::HopPath) -> Self {
        BuiltinTypeValue::AsPath(primitives::AsPath::from(as_path))
    }
}

impl From<Hop> for TypeValue {
    fn from(hop: Hop) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Hop(hop))
    }
}

impl From<routecore::bgp::aspath::Hop<Vec<u8>>> for BuiltinTypeValue {
    fn from(hop: routecore::bgp::aspath::Hop<Vec<u8>>) -> Self {
        BuiltinTypeValue::Hop(primitives::Hop(hop))
    }
}

// impl From<routecore::bgp::communities::Communities> for TypeValue {
//     fn from(value: routecore::bgp::communities::Communities) -> Self {
//         let list = List(value.communities.iter().map(|c| ElementTypeValue::Primitive((*c).into())).collect());
//         TypeValue::Builtin(BuiltinTypeValue::Communities(list))
//     }
// }

impl From<Vec<Asn>> for TypeValue {
    fn from(as_path: Vec<Asn>) -> Self {
        let as_path: Vec<routecore::bgp::aspath::Hop<Vec<u8>>> =
            as_path.iter().map(|p| p.0.into()).collect();
        let as_path = crate::types::builtin::AsPath::from(as_path);
        TypeValue::Builtin(BuiltinTypeValue::AsPath(as_path))
    }
}

impl From<Vec<routecore::bgp::aspath::Hop<Vec<u8>>>> for TypeValue {
    fn from(as_path: Vec<routecore::bgp::aspath::Hop<Vec<u8>>>) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::AsPath(as_path.into()))
    }
}

impl From<Vec<crate::types::builtin::Community>> for TypeValue {
    fn from(value: Vec<crate::types::builtin::Community>) -> Self {
        TypeValue::List(List::new(
            value
                .iter()
                .map(|v| ElementTypeValue::Primitive((*v).into()))
                .collect::<Vec<_>>(),
        ))
    }
}

impl ScalarValue for TypeValue {}

// Type conversions for Records

// Records do not know how their literals are going to be used/converted, so
// they store them as actual TypeValue::*Literal variants

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

impl TryFrom<&'_ crate::ast::IpAddressLiteral> for TypeValue {
    type Error = CompileError;

    fn try_from(value: &crate::ast::IpAddressLiteral) -> Result<Self, Self::Error> {
        Ok(TypeValue::Builtin(BuiltinTypeValue::IpAddress(
                value.try_into()?
        )))
    }
}

impl TryFrom<&'_ crate::ast::PrefixLiteral> for TypeValue {
    type Error = CompileError;

    fn try_from(value: &crate::ast::PrefixLiteral) -> Result<Self, Self::Error> {
        Ok(TypeValue::Builtin(BuiltinTypeValue::Prefix(
            value.try_into()?
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
        TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(value.0.into())))
    }
}

impl TryFrom<crate::ast::StandardCommunityLiteral> for TypeValue {
    type Error = CompileError;

    fn try_from(value: crate::ast::StandardCommunityLiteral) -> Result<Self, Self::Error> {
        let comm = (&value).try_into()?;
        Ok(TypeValue::Builtin(BuiltinTypeValue::Community(
                comm
        )))
    }
}

impl TryFrom<crate::ast::ExtendedCommunityLiteral> for TypeValue {
    type Error = CompileError;

    fn try_from(value: crate::ast::ExtendedCommunityLiteral) -> Result<Self, Self::Error> {
        let comm = (&value).try_into()?;
        Ok(TypeValue::Builtin(BuiltinTypeValue::Community(
                comm
        )))
    }
}

impl TryFrom<crate::ast::LargeCommunityLiteral> for TypeValue {
    type Error = CompileError;

    fn try_from(value: crate::ast::LargeCommunityLiteral) -> Result<Self, Self::Error> {
        let comm = (&value).try_into()?;
        Ok(TypeValue::Builtin(BuiltinTypeValue::Community(
                comm
        )))
    }

}

impl From<crate::ast::HexLiteral> for TypeValue {
    fn from(value: crate::ast::HexLiteral) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::HexLiteral(HexLiteral(value.0)))
    }
}

impl From<crate::ast::BooleanLiteral> for TypeValue {
    fn from(value: crate::ast::BooleanLiteral) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(value.0)))
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

    fn try_from(value: AnonymousRecordValueExpr) -> Result<Self, Self::Error> {
        Ok(TypeValue::Record(value.try_into()?))
    }
}

impl TryFrom<TypedRecordValueExpr> for TypeValue {
    type Error = CompileError;

    fn try_from(value: TypedRecordValueExpr) -> Result<Self, Self::Error> {
        Ok(TypeValue::Record(Record::try_from(value)?))
    }
}
