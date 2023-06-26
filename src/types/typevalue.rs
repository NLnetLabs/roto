use std::{cmp::Ordering, fmt::Display, sync::Arc};

use primitives::{
    AsPath, Community, Hop, LocalPref, MultiExitDisc, NextHop, OriginType,
    RouteStatus,
};
use serde::Serialize;
use smallvec::SmallVec;

//============ TypeValue ====================================================
use crate::{
    ast::{
        AnonymousRecordValueExpr, ListValueExpr, ShortString,
        TypedRecordValueExpr,
    },
    attr_change_set::ScalarValue,
    compile::CompileError,
    traits::RotoType,
    vm::{StackValue, VmError},
};

use super::{
    builtin::{
        primitives, Asn, BgpUpdateMessage, Boolean, BuiltinTypeValue,
        HexLiteral, IntegerLiteral, IpAddress, Prefix, PrefixLength,
        RawRouteWithDeltas, StringLiteral, U32, U8,
    },
    collections::{ElementTypeValue, List, Record},
    constant_enum::Enum,
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
    Enum(Enum),
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
        let def_ = type_ident_pairs
            .into_iter()
            .map(|(ident, ty)| (ShortString::from(ident), ty.into()))
            .collect::<Vec<_>>();
        Record::new(def_)
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
        index: SmallVec<[usize; 8]>,
    ) -> Result<&ElementTypeValue, CompileError> {
        match self {
            TypeValue::Record(r) => {
                let field = r.get_field_by_index(index.clone());
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
        field_index: SmallVec<[usize; 8]>,
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

    pub(crate) fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        args: &'a [StackValue],
        return_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match self {
            TypeValue::Record(rec_type) => {
                rec_type.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Enum(c_enum) => {
                c_enum.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::ConstU8EnumVariant(
                enum_var,
            )) => enum_var.exec_value_method(method_token, args, return_type),
            TypeValue::Builtin(BuiltinTypeValue::ConstU16EnumVariant(
                enum_var,
            )) => enum_var.exec_value_method(method_token, args, return_type),
            TypeValue::Builtin(BuiltinTypeValue::ConstU32EnumVariant(
                enum_var,
            )) => enum_var.exec_value_method(method_token, args, return_type),
            TypeValue::List(list) => {
                list.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::AsPath(as_path)) => {
                as_path.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::Prefix(prefix)) => {
                prefix.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(lit_int)) => {
                lit_int.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::StringLiteral(lit_str)) => {
                lit_str.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::HexLiteral(lit_hex)) => {
                lit_hex.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::U32(u32)) => {
                u32.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::Asn(asn)) => {
                asn.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::Hop(hop)) => {
                hop.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::IpAddress(ip)) => {
                ip.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::Route(route)) => {
                route.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::BgpUpdateMessage(raw)) => {
                raw.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::Community(community)) => {
                community.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::Communities(
                communities,
            )) => {
                communities.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::OriginType(origin)) => {
                origin.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::LocalPref(local_pref)) => {
                local_pref.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::NextHop(next_hop)) => {
                next_hop.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::AtomicAggregator(
                aggregator,
            )) => {
                aggregator.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::MultiExitDisc(med)) => {
                med.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::U8(u8_lit)) => {
                u8_lit.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::Boolean(boolean)) => {
                boolean.exec_value_method(method_token, args, return_type)
            }

            TypeValue::Builtin(BuiltinTypeValue::PrefixLength(
                prefix_length,
            )) => prefix_length.exec_value_method(
                method_token,
                args,
                return_type,
            ),
            TypeValue::Builtin(BuiltinTypeValue::RouteStatus(
                route_status,
            )) => route_status.exec_value_method(
                method_token,
                args,
                return_type,
            ),
            TypeValue::OutputStreamMessage(stream) => {
                stream.exec_value_method(method_token, args, return_type)
            }
            TypeValue::SharedValue(sv) => {
                sv.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Unknown => Ok(TypeValue::Unknown),
            TypeValue::UnInit => {
                panic!("Unitialized memory cannot be read. That's fatal.");
            }
        }
    }

    pub(crate) fn exec_consume_value_method<'a>(
        self,
        method_token: usize,
        args: Vec<TypeValue>,
        return_type: TypeDef,
        _field_index: SmallVec<[usize; 8]>,
    ) -> Result<TypeValue, VmError> {
        match self {
            TypeValue::Record(rec_type) => rec_type
                .exec_consume_value_method(method_token, args, return_type),
            TypeValue::Enum(c_enum) => c_enum.exec_consume_value_method(
                method_token,
                args,
                return_type,
            ),
            TypeValue::Builtin(BuiltinTypeValue::ConstU8EnumVariant(
                enum_var,
            )) => enum_var.exec_consume_value_method(
                method_token,
                args,
                return_type,
            ),
            TypeValue::Builtin(BuiltinTypeValue::ConstU16EnumVariant(
                enum_var,
            )) => enum_var.exec_consume_value_method(
                method_token,
                args,
                return_type,
            ),
            TypeValue::Builtin(BuiltinTypeValue::ConstU32EnumVariant(
                enum_var,
            )) => enum_var.exec_consume_value_method(
                method_token,
                args,
                return_type,
            ),
            TypeValue::List(list) => list.exec_consume_value_method(
                method_token,
                args,
                return_type,
            ),
            TypeValue::Builtin(BuiltinTypeValue::AsPath(as_path)) => as_path
                .exec_consume_value_method(method_token, args, return_type),
            TypeValue::Builtin(BuiltinTypeValue::Prefix(prefix)) => prefix
                .exec_consume_value_method(method_token, args, return_type),
            TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(lit_int)) => {
                lit_int.exec_consume_value_method(
                    method_token,
                    args,
                    return_type,
                )
            }
            TypeValue::Builtin(BuiltinTypeValue::StringLiteral(lit_str)) => {
                lit_str.exec_consume_value_method(
                    method_token,
                    args,
                    return_type,
                )
            }
            TypeValue::Builtin(BuiltinTypeValue::HexLiteral(lit_hex)) => {
                lit_hex.exec_consume_value_method(
                    method_token,
                    args,
                    return_type,
                )
            }
            TypeValue::Builtin(BuiltinTypeValue::U32(u32)) => {
                u32.exec_consume_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::Asn(asn)) => {
                asn.exec_consume_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::Hop(hop)) => {
                hop.exec_consume_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::IpAddress(ip)) => {
                ip.exec_consume_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::Route(route)) => route
                .exec_consume_value_method(method_token, args, return_type),
            TypeValue::Builtin(BuiltinTypeValue::BgpUpdateMessage(_raw)) => {
                Err(VmError::InvalidMethodCall)
            }
            TypeValue::Builtin(BuiltinTypeValue::Communities(
                communities,
            )) => communities.exec_consume_value_method(
                method_token,
                args,
                return_type,
            ),
            TypeValue::Builtin(BuiltinTypeValue::Community(community)) => {
                community.exec_consume_value_method(
                    method_token,
                    args,
                    return_type,
                )
            }
            TypeValue::Builtin(BuiltinTypeValue::OriginType(origin)) => {
                origin.exec_consume_value_method(
                    method_token,
                    args,
                    return_type,
                )
            }
            TypeValue::Builtin(BuiltinTypeValue::U8(u8_lit)) => u8_lit
                .exec_consume_value_method(method_token, args, return_type),
            TypeValue::Builtin(BuiltinTypeValue::Boolean(boolean)) => boolean
                .exec_consume_value_method(method_token, args, return_type),
            TypeValue::Builtin(BuiltinTypeValue::PrefixLength(
                prefix_length,
            )) => prefix_length.exec_consume_value_method(
                method_token,
                args,
                return_type,
            ),
            TypeValue::Builtin(BuiltinTypeValue::LocalPref(local_pref)) => {
                local_pref.exec_consume_value_method(
                    method_token,
                    args,
                    return_type,
                )
            }
            TypeValue::Builtin(BuiltinTypeValue::AtomicAggregator(
                aggregator,
            )) => aggregator.exec_consume_value_method(
                method_token,
                args,
                return_type,
            ),
            TypeValue::Builtin(BuiltinTypeValue::NextHop(next_hop)) => {
                next_hop.exec_consume_value_method(
                    method_token,
                    args,
                    return_type,
                )
            }
            TypeValue::Builtin(BuiltinTypeValue::MultiExitDisc(med)) => {
                med.exec_consume_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::RouteStatus(
                route_status,
            )) => route_status.exec_consume_value_method(
                method_token,
                args,
                return_type,
            ),
            TypeValue::OutputStreamMessage(_stream) => {
                Err(VmError::InvalidMethodCall)
            }
            TypeValue::SharedValue(_sv) => {
                panic!("Shared values cannot be consumed. They're read-only.")
            }
            TypeValue::Unknown => Ok(TypeValue::Unknown),
            TypeValue::UnInit => {
                panic!("Unitialized memory cannot be read. That's fatal.");
            }
        }
    }

    // try to return the converted type with its value, if it's set. This is
    // basically only the case if the kin of self, is Constant.
    // Otherwise create an empty value for the type of self, and recursively
    // try to convert that into the desired type.
    // If these two steps fail, return an error.
    // pub fn try_convert_into_variant(
    //     mut self,
    //     type_def: TypeDef,
    // ) -> Result<Self, CompileError> {
    //     trace!("CONVERT TYPEVALUE {:#?} -> {}", self, type_def);
    //     match self {
    //         TypeValue::List(list) => {
    //             self = list.into_type(&type_def)?;
    //         }
    //         TypeValue::Record(_rec) => {
    //             return Err(CompileError::new(
    //                 format!("A value of type Record can't be converted into type {}.", type_def)
    //             ))
    //         }
    //         TypeValue::Enum(_enum) => {
    //             return Err(CompileError::new(
    //                 format!("A value of type Enum can't be converted into type {}.", type_def)
    //             ))
    //         }
    //         TypeValue::Builtin(BuiltinTypeValue::ConstU8EnumVariant(val)) => {
    //             self = val.into_type(&type_def)?;
    //         }
    //         TypeValue::Builtin(BuiltinTypeValue::ConstU16EnumVariant(val)) => {
    //             self = val.into_type(&type_def)?;
    //         }
    //         TypeValue::Builtin(BuiltinTypeValue::ConstU32EnumVariant(val)) => {
    //             self = val.into_type(&type_def)?;
    //         }
    //         TypeValue::Builtin(BuiltinTypeValue::U32(int_u32)) => {
    //             self = int_u32.into_type(&type_def)?;
    //         }
    //         TypeValue::Builtin(BuiltinTypeValue::U8(val)) => {
    //             self = val.into_type(&type_def)?;
    //         }
    //         TypeValue::Builtin(BuiltinTypeValue::Boolean(val)) => {
    //             self = val.into_type(&type_def)?;
    //         }

    //         TypeValue::Builtin(BuiltinTypeValue::Prefix(val)) => {
    //             self = val.into_type(&type_def)?;
    //         }

    //         TypeValue::Builtin(BuiltinTypeValue::PrefixLength(val)) => {
    //             self = val.into_type(&type_def)?;
    //         }

    //         TypeValue::Builtin(BuiltinTypeValue::IpAddress(val)) => {
    //             self = val.into_type(&type_def)?;
    //         }

    //         TypeValue::Builtin(BuiltinTypeValue::Asn(val)) => {
    //             self = val.into_type(&type_def)?;
    //         }

    //         TypeValue::Builtin(BuiltinTypeValue::AsPath(val)) => {
    //             self = val.into_type(&type_def)?;
    //         }
    //         TypeValue::Builtin(BuiltinTypeValue::Hop(val)) => {
    //             self = val.into_type(&type_def)?;
    //         }
    //         TypeValue::Builtin(BuiltinTypeValue::Communities(val)) => {
    //             self = val.into_type(&type_def)?;
    //         }
    //         TypeValue::Builtin(BuiltinTypeValue::Community(val)) => {
    //             self = val.into_type(&type_def)?;
    //         }

    //         TypeValue::Builtin(BuiltinTypeValue::OriginType(val)) => {
    //             self = val.into_type(&type_def)?;
    //         }

    //         TypeValue::Builtin(BuiltinTypeValue::Route(val)) => {
    //             self = val.into_type(&type_def)?;
    //         }
    //         TypeValue::Builtin(BuiltinTypeValue::BgpUpdateMessage(_val)) => {
    //             return Err(CompileError::new(
    //                 "Raw BGP message value can't be converted.".into(),
    //             ))
    //         }
    //         TypeValue::Builtin(BuiltinTypeValue::LocalPref(val)) => {
    //             self = val.into_type(&type_def)?;
    //         }

    //         TypeValue::Builtin(BuiltinTypeValue::MultiExitDisc(val)) => {
    //             self = val.into_type(&type_def)?;
    //         }

    //         TypeValue::Builtin(BuiltinTypeValue::NextHop(val)) => {
    //             self = val.into_type(&type_def)?;
    //         }

    //         TypeValue::Builtin(BuiltinTypeValue::AtomicAggregator(val)) => {
    //             self = val.into_type(&type_def)?;
    //         }

    //         TypeValue::Builtin(BuiltinTypeValue::RouteStatus(val)) => {
    //             self = val.into_type(&type_def)?;
    //         }

    //         TypeValue::Builtin(BuiltinTypeValue::HexLiteral(val)) => {
    //             self = val.into_type(&type_def)?;
    //         }

    //         TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(val)) => {
    //             self = val.into_type(&type_def)?;
    //         }

    //         TypeValue::Builtin(BuiltinTypeValue::StringLiteral(val)) => {
    //             self = val.into_type(&type_def)?;
    //         }
    //         TypeValue::OutputStreamMessage(_) => {
    //             return Err(CompileError::new(
    //                 "Value from type OutputStreamMessage can't be converted."
    //                     .into(),
    //             ))
    //         }
    //         TypeValue::SharedValue(_) => {
    //             return Err(CompileError::new(
    //                 "Value from SharedValue type can't be converted.".into(),
    //             ))
    //         }
    //         TypeValue::UnInit => {
    //             return Err(CompileError::new(
    //                 "Fatal: An unitialized type can't be converted.".into(),
    //             ))
    //         }
    //         TypeValue::Unknown => {
    //             return Err(CompileError::new(
    //             format!(
    //                 "An instance of an unknown type can't be converted into type {}", type_def),
    //             ))
    //         }
    //     }

    //     Ok(self)
    // }
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
            TypeDef::AcceptReject(_) => Err(CompileError::new("Unsupported TypeDef::AcceptReject in TypeValue::get_props_for_method()".to_string())),
            TypeDef::Asn => Asn::get_props_for_method(ty, method_name),
            TypeDef::AsPath => AsPath::get_props_for_method(ty, method_name),
            TypeDef::AtomicAggregator => Err(CompileError::new("Unsupported TypeDef::AtomicAggregator in TypeValue::get_props_for_method()".to_string())),
            TypeDef::BgpUpdateMessage => BgpUpdateMessage::get_props_for_method(ty, method_name),
            TypeDef::Boolean => Boolean::get_props_for_method(ty, method_name),
            TypeDef::Community => Community::get_props_for_method(ty, method_name),
            TypeDef::ConstEnumVariant(_) => Err(CompileError::new("Unsupported TypeDef::ConstEnumVariant in TypeValue::get_props_for_method()".to_string())),
            TypeDef::Enum(ty) => Self::get_props_for_method(*ty, method_name),
            TypeDef::HexLiteral => HexLiteral::get_props_for_method(ty, method_name),
            TypeDef::Hop => Hop::get_props_for_method(ty, method_name),
            TypeDef::IntegerLiteral => IntegerLiteral::get_props_for_method(ty, method_name),
            TypeDef::IpAddress => IpAddress::get_props_for_method(ty, method_name),
            TypeDef::List(ty) => Self::get_props_for_method(*ty, method_name),
            TypeDef::LocalPref => LocalPref::get_props_for_method(ty, method_name),
            TypeDef::MultiExitDisc => MultiExitDisc::get_props_for_method(ty, method_name),
            TypeDef::NextHop => NextHop::get_props_for_method(ty, method_name),
            TypeDef::OriginType => OriginType::get_props_for_method(ty, method_name),
            TypeDef::OutputStream(ty) => Self::get_props_for_method(*ty, method_name),
            TypeDef::Prefix => Prefix::get_props_for_method(ty, method_name),
            TypeDef::PrefixLength => PrefixLength::get_props_for_method(ty, method_name),
            TypeDef::Record(_) => Record::get_props_for_method(ty, method_name),
            TypeDef::Rib(ty) => Self::get_props_for_method(*ty.0, method_name),
            TypeDef::Route => RawRouteWithDeltas::get_props_for_method(ty, method_name),
            TypeDef::RouteStatus => RouteStatus::get_props_for_method(ty, method_name),
            TypeDef::StringLiteral => StringLiteral::get_props_for_method(ty, method_name),
            TypeDef::Table(ty) => Self::get_props_for_method(*ty, method_name),
            TypeDef::U32 => U32::get_props_for_method(ty, method_name),
            TypeDef::U8 => U8::get_props_for_method(ty, method_name),
            TypeDef::Unknown => Err(CompileError::new("Unsupported TypeDef::Unknown in TypeValue::get_props_for_method()".to_string())),
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
                BuiltinTypeValue::AtomicAggregator(v) => v.into_type(ty),
                BuiltinTypeValue::BgpUpdateMessage(_) => Err(CompileError::new("Unsupported TypeValue::BgpUpdateMessage in TypeValue::into_type()".to_string())),
                BuiltinTypeValue::Boolean(v) => v.into_type(ty),
                BuiltinTypeValue::Communities(v) => v.into_type(ty),
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
                BuiltinTypeValue::U8(v) => v.into_type(ty),
            }
            TypeValue::Enum(v) => v.into_type(ty),
            TypeValue::List(v) => v.into_type(ty),
            TypeValue::OutputStreamMessage(_) => Err(CompileError::new("Unsupported TypeValue::OutputStreamMessage in TypeValue::into_type()".to_string())),
            TypeValue::Record(v) => v.into_type(ty),
            TypeValue::SharedValue(v) => v.into_type(ty),
            TypeValue::UnInit => Err(CompileError::new("Unsupported TypeValue::UnInit in TypeValue::into_type()".to_string())),
            TypeValue::Unknown => Err(CompileError::new("Unsupported TypeValue::Unknown in TypeValue::into_type()".to_string())),
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
                BuiltinTypeValue::AtomicAggregator(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::BgpUpdateMessage(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::Boolean(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::Communities(v) => {
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
                BuiltinTypeValue::U8(v) => {
                    v.exec_value_method(method_token, args, res_type)
                }
            },
            TypeValue::Enum(v) => {
                v.exec_value_method(method_token, args, res_type)
            }
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
                BuiltinTypeValue::AtomicAggregator(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::BgpUpdateMessage(_) => {
                    Err(VmError::InvalidValueType)
                }
                BuiltinTypeValue::Boolean(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
                BuiltinTypeValue::Communities(v) => {
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
                BuiltinTypeValue::U8(v) => {
                    v.exec_consume_value_method(method_token, args, res_type)
                }
            },
            TypeValue::Enum(v) => {
                v.exec_consume_value_method(method_token, args, res_type)
            }
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
        match self {
            TypeValue::Builtin(p) => write!(f, "{}", p),
            TypeValue::List(l) => write!(f, "{} (List)", l),
            TypeValue::Record(r) => {
                write!(f, "{} (Record)", r)
            }
            TypeValue::Enum(c_enum) => {
                write!(f, "{} (Enum)", c_enum)
            }
            TypeValue::OutputStreamMessage(m) => {
                write!(f, "{} (Stream message)", m)
            }
            TypeValue::SharedValue(sv) => write!(f, "{} (Shared Value)", sv),
            TypeValue::Unknown => write!(f, "Unknown"),
            TypeValue::UnInit => write!(f, "Uninitialized"),
        }
    }
}

// This PartialEq is used at VM runtime to convert types when performing a
// `Cmp` operation. The `Cmp` operation just performs a
// `<TypeValue> == <TypeValue>` operation (in Rust), that comes here. Note
// that at this point NO errors or panics are created for invalid type
// comparisons.
impl PartialEq for TypeValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
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

            // Builtins::Communities to Lists
            (
                TypeValue::Builtin(BuiltinTypeValue::Communities(c)),
                TypeValue::List(l),
            ) => {
                if c == l {
                    return true;
                }
                if let Ok(TypeValue::List(l2)) =
                    l.clone().into_type(&(self.into())).as_ref()
                {
                    c == l2
                } else {
                    false
                }
            }
            (TypeValue::Builtin(_), _) => false,

            // Lists
            (TypeValue::List(l1), TypeValue::List(l2)) => {
                if l1 == l2 {
                    return true;
                } else {
                    false
                }
            }
            (
                TypeValue::List(l),
                TypeValue::Builtin(BuiltinTypeValue::Communities(c)),
            ) => {
                if c == l {
                    return true;
                }
                if let Ok(TypeValue::List(l2)) =
                    l.clone().into_type(&(self.into())).as_ref()
                {
                    c == l2
                } else {
                    false
                }
            }
            (TypeValue::List(_), _) => false,

            // Records
            (TypeValue::Record(r1), TypeValue::Record(r2)) => r1 == r2,
            (
                TypeValue::Record(_),
                TypeValue::Builtin(BuiltinTypeValue::Route(_r)),
            ) => todo!(),
            (TypeValue::Record(_), _) => false,

            // SharedValues (coming out of data-sources)
            (TypeValue::SharedValue(sv1), TypeValue::SharedValue(sv2)) => {
                sv1.as_ref() == sv2.as_ref()
            }
            (TypeValue::SharedValue(sv1), tv) => sv1.as_ref() == tv,

            // remaining TypeValues to SharedValues
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

            // Inconvertible & Incomparable types
            (TypeValue::Unknown, _) => false,
            (TypeValue::UnInit, _) => false,
            (TypeValue::OutputStreamMessage(_), _) => false,
            (TypeValue::Enum(_), _) => false,
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
            TypeValue::Enum(e) => {
                e.hash(state);
            }
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
                panic!("Communities have no ordering.")
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
                panic!("AS Paths have no ordering.")
            }
            (
                TypeValue::Builtin(BuiltinTypeValue::Route(_)),
                TypeValue::Builtin(BuiltinTypeValue::Route(_)),
            ) => {
                panic!("Routes have no ordering.")
            }
            (
                TypeValue::Builtin(BuiltinTypeValue::RouteStatus(_)),
                TypeValue::Builtin(BuiltinTypeValue::RouteStatus(_)),
            ) => {
                panic!("Route statuses have no ordering.")
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
                panic!("Lists are not comparable.")
            }
            (TypeValue::Record(_), TypeValue::Record(_)) => {
                panic!("Records are not comparable.")
            }
            // (TypeValue::Rib(_), TypeValue::Rib(_)) => {
            //     panic!("Ribs are not comparable.")
            // }
            // (TypeValue::Table(_), TypeValue::Table(_)) => {
            //     panic!("Tables are not comparable.")
            // }
            (TypeValue::Unknown, TypeValue::Unknown) => {
                panic!("Unknown is unsortable.")
            }
            _ => {
                panic!("Incomparable types.")
            }
        }
    }
}

// impl Ord for TypeValue {
//     fn cmp(&self, other: &Self) -> Ordering {
//         match (self, other) {
//             (
//                 TypeValue::Builtin(BuiltinTypeValue::U8(U8(u1))),
//                 TypeValue::Builtin(BuiltinTypeValue::U8(U8(u2))),
//             ) => u1.cmp(u2),
//             (
//                 TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(il1)),
//                 TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(il2)),
//             ) => il1.cmp(il2),
//             (TypeValue::List(_l1), TypeValue::List(_l2)) => {
//                 panic!("Lists are not comparable.")
//             }
//             (TypeValue::Record(_r1), TypeValue::Record(_r2)) => {
//                 panic!("Records are not comparable.")
//             }
//             (TypeValue::Unknown, TypeValue::Unknown) => Ordering::Equal,
//             (_, TypeValue::Builtin(_)) => Ordering::Greater,
//             (TypeValue::Builtin(_), _) => Ordering::Greater,
//             (TypeValue::List(_), _) => Ordering::Less,
//             (_, TypeValue::List(_)) => Ordering::Greater,
//             (TypeValue::Record(_), _) => Ordering::Less,
//             (_, TypeValue::Record(_)) => Ordering::Greater,
//             (_, TypeValue::UnInit) => {
//                 panic!("comparing with uninitialized memory.")
//             }
//             (TypeValue::UnInit, _) => {
//                 panic!("comparing with uninitialized memory.")
//             }
//             (TypeValue::OutputStreamMessage(_), _) => todo!(),
//             (_, TypeValue::OutputStreamMessage(_)) => todo!(),
//             (TypeValue::SharedValue(_), _) => Ordering::Less,
//             (_, TypeValue::SharedValue(_)) => Ordering::Greater,
//         }
//     }
// }

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

impl From<Vec<TypeValue>> for TypeValue {
    fn from(value: Vec<TypeValue>) -> Self {
        TypeValue::List(List::new(
            value
                .iter()
                .map(|v| ElementTypeValue::Primitive((*v).clone()))
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

impl From<crate::ast::PrefixLengthLiteral> for TypeValue {
    fn from(value: crate::ast::PrefixLengthLiteral) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::PrefixLength(PrefixLength(
            value.0 as u8,
        )))
    }
}

impl From<crate::ast::AsnLiteral> for TypeValue {
    fn from(value: crate::ast::AsnLiteral) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(value.0.into())))
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

impl From<ListValueExpr> for TypeValue {
    fn from(value: ListValueExpr) -> Self {
        TypeValue::List(value.into())
    }
}

impl From<AnonymousRecordValueExpr> for TypeValue {
    fn from(value: AnonymousRecordValueExpr) -> Self {
        TypeValue::Record(value.into())
    }
}

impl From<TypedRecordValueExpr> for TypeValue {
    fn from(value: TypedRecordValueExpr) -> Self {
        TypeValue::Record(value.into())
    }
}
