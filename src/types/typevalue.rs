use std::{cmp::Ordering, fmt::Display, sync::Arc};

use primitives::Hop;

//============ TypeValue ====================================================
use crate::{
    ast::{ShortString, RecordValueExpr},
    compile::CompileError,
    traits::RotoType,
    vm::{StackRef, StackRefPos, VmError, StackValue}, attr_change_set::ScalarValue,
};

use super::{
    builtin::{
        Asn, Boolean, BuiltinTypeValue, HexLiteral,
        IntegerLiteral, IpAddress, Prefix, PrefixLength, StringLiteral, U32,
        U8, primitives,
    },
    collections::{ElementTypeValue, List, Record},
    outputs::OutputStreamMessage,
    typedef::TypeDef,
};

/// These are the actual types that are used in the Roto language. This enum
/// holds both the type-level information and the value. The collection
/// variants can hold multiple values recursively, e.g. a List of Records.

#[derive(Debug, PartialEq, Eq, Default, Clone)]
pub enum TypeValue {
    // All the built-in scalars and vectors
    Builtin(BuiltinTypeValue),
    // An ordered list of one user-defined type
    List(List),
    // A map of (key, value) pairs, where value can be any of the other types.
    // Always user-defined.
    Record(Record),
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

    pub(crate) fn into_builtin(self) -> Result<BuiltinTypeValue, CompileError> {
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
        index: usize,
    ) -> Result<&ElementTypeValue, CompileError> {
        match self {
            TypeValue::Record(r) => {
                let field = r.get_field_by_index(index);
                field.ok_or_else(|| {
                    format!(
                        "Index {} out of bounds for record '{:?}'",
                        index, self
                    )
                    .as_str()
                    .into()
                })
            }
            TypeValue::List(l) => {
                let field = l.get_field_by_index(index);
                field.ok_or_else(|| {
                    format!(
                        "Index {} out of bounds for list '{:?}'",
                        index, self
                    )
                    .as_str()
                    .into()
                })
            }
            _ => Err(format!("Type '{:?}' is not a record.", self).into()),
        }
    }

    pub(crate) fn _set_field_by_stack_ref(
        &mut self,
        stack_ref: &StackRef,
        value: TypeValue,
    ) -> Result<(), VmError> {
        if let StackRefPos::MemPos(index) = stack_ref.pos {
            match self {
                TypeValue::Record(rec) => {
                    rec.set_field_for_index(index as usize, value)?
                }
                TypeValue::List(list) => {
                    list.set_field_for_index(index as usize, value)?
                }
                _ => return Err(VmError::InvalidWrite),
            };
        } else {
            return Err(VmError::InvalidWrite);
        };

        Ok(())
    }

    pub(crate) fn _set_field(
        mut self,
        field_index: usize,
        value: TypeValue,
    ) -> Result<Self, VmError> {
        match self {
            TypeValue::Record(ref mut rec) => {
                rec.set_field_for_index(field_index, value)?;
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
    ) -> Result<Box<dyn FnOnce() -> TypeValue + '_>, VmError> {
        match self {
            TypeValue::Record(rec_type) => {
                rec_type.exec_value_method(method_token, args, return_type)
            }
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
            TypeValue::Builtin(BuiltinTypeValue::RawBgpMessage(raw)) => {
                raw.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::Community(community)) => {
                community.exec_value_method(method_token, args, return_type)
            }
            TypeValue::Builtin(BuiltinTypeValue::Communities(communities)) => {
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
            TypeValue::Builtin(BuiltinTypeValue::AtomicAggregator(aggregator)) => {
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
            // TypeValue::Rib(rib) => {
            //     rib.exec_value_method(method_token, args, return_type)
            // }
            // TypeValue::Table(rec) => {
            //     rec.exec_value_method(method_token, args, return_type)
            // }
            TypeValue::OutputStreamMessage(stream) => {
                stream.exec_value_method(method_token, args, return_type)
            }
            TypeValue::SharedValue(sv) => sv.exec_value_method(method_token, args, return_type),
            TypeValue::Unknown => Ok(Box::new(|| TypeValue::Unknown)),
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
        _field_index: Option<usize>,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        match self {
            TypeValue::Record(rec_type) => rec_type
                .exec_consume_value_method(method_token, args, return_type),
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
            TypeValue::Builtin(BuiltinTypeValue::RawBgpMessage(_raw)) => 
                Err(VmError::InvalidMethodCall),
            TypeValue::Builtin(BuiltinTypeValue::Communities(communities)) => {
                // let l = communities.into_iter().map(|c| ElementTypeValue::Primitive(c.into())).collect::<Vec<_>>();
                communities.exec_consume_value_method(
                    method_token,
                    args,
                    return_type,
                )
            }
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
            TypeValue::Builtin(BuiltinTypeValue::LocalPref(
                local_pref,
            )) => local_pref.exec_consume_value_method(
                method_token,
                args,
                return_type,
            ),
            TypeValue::Builtin(BuiltinTypeValue::AtomicAggregator(
                aggregator,
            )) => aggregator.exec_consume_value_method(
                method_token,
                args,
                return_type,
            ),
            TypeValue::Builtin(BuiltinTypeValue::NextHop(
                next_hop,
            )) => next_hop.exec_consume_value_method(
                method_token,
                args,
                return_type,
            ),
            TypeValue::Builtin(BuiltinTypeValue::MultiExitDisc(
                med,
            )) => med.exec_consume_value_method(
                method_token,
                args,
                return_type,
            ),
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
            TypeValue::SharedValue(_sv) => panic!("Shared values cannot be consumed. They're read-only."),
            TypeValue::Unknown => Ok(Box::new(|| TypeValue::Unknown)),
            TypeValue::UnInit => {
                panic!("Unitialized memory cannot be read. That's fatal.");
            }
        }
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
            TypeValue::OutputStreamMessage(m) => {
                write!(f, "{} (Stream message)", m)
            }
            TypeValue::SharedValue(sv) => write!(f, "{} (Shared Value)", sv),
            TypeValue::Unknown => write!(f, "Unknown"),
            TypeValue::UnInit => write!(f, "Uninitialized"),
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

impl Ord for TypeValue {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (
                TypeValue::Builtin(BuiltinTypeValue::U8(U8(u1))),
                TypeValue::Builtin(BuiltinTypeValue::U8(U8(u2))),
            ) => u1.cmp(u2),
            (TypeValue::List(_l1), TypeValue::List(_l2)) => {
                panic!("Lists are not comparable.")
            }
            (TypeValue::Record(_r1), TypeValue::Record(_r2)) => {
                panic!("Records are not comparable.")
            }
            // (TypeValue::Rib(_r1), TypeValue::Rib(_r2)) => {
            //     panic!("Ribs are not comparable.")
            // }
            // (TypeValue::Table(_r1), TypeValue::Table(_r2)) => {
            //     panic!("Tables are not comparable.")
            // }
            (TypeValue::Unknown, TypeValue::Unknown) => Ordering::Equal,
            (TypeValue::Builtin(_), _) => Ordering::Less,
            (_, TypeValue::Builtin(_)) => Ordering::Greater,
            (TypeValue::List(_), _) => Ordering::Less,
            (_, TypeValue::List(_)) => Ordering::Greater,
            (TypeValue::Record(_), _) => Ordering::Less,
            (_, TypeValue::Record(_)) => Ordering::Greater,
            // (TypeValue::Rib(_), _) => Ordering::Less,
            // (_, TypeValue::Rib(_)) => Ordering::Greater,
            // (TypeValue::Table(_), _) => Ordering::Less,
            // (_, TypeValue::Table(_)) => Ordering::Greater,
            (_, TypeValue::UnInit) => {
                panic!("comparing with uninitialized memory.")
            }
            (TypeValue::UnInit, _) => {
                panic!("comparing with uninitialized memory.")
            }
            (TypeValue::OutputStreamMessage(_), _) => todo!(),
            (_, TypeValue::OutputStreamMessage(_)) => todo!(),
            (TypeValue::SharedValue(_), _) => Ordering::Less,
            (_, TypeValue::SharedValue(_)) => Ordering::Greater,
        }
    }
}

impl<'a> TryFrom<StackValue<'a>> for bool {
    type Error = VmError;

    fn try_from(t: StackValue) -> Result<Self, Self::Error> {
        match t {
            StackValue::Ref(TypeValue::Builtin(BuiltinTypeValue::Boolean(ref b))) => {
                Ok(b.0) //.ok_or(VmError::ImpossibleComparison)
            }
            StackValue::Arc(bv) => {
                if let TypeValue::Builtin(BuiltinTypeValue::Boolean(ref b)) = *bv {
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
            TypeValue::Builtin(BuiltinTypeValue::Boolean(ref b)) => {
                Ok(b.0) //.ok_or(VmError::ImpossibleComparison)
            }
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
        TypeValue::Builtin(BuiltinTypeValue::Prefix(primitives::Prefix(value)))
    }
}

impl TryFrom<&TypeValue> for routecore::addr::Prefix {
    type Error = VmError;

    fn try_from(value: &TypeValue) -> Result<Self, Self::Error> {
        if let TypeValue::Builtin(BuiltinTypeValue::Prefix(primitives::Prefix(pfx))) = value {
            Ok(*pfx)
        } else {
            Err(VmError::InvalidConversion)
        }
    }
}

impl From<std::net::IpAddr> for TypeValue {
    fn from(ip_addr: std::net::IpAddr) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::IpAddress(primitives::IpAddress(ip_addr)))
    }
}

impl From<routecore::asn::Asn> for TypeValue {
    fn from(value: routecore::asn::Asn) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(value)))
    }
}

impl From<routecore::bgp::aspath::HopPath> for TypeValue {
    fn from(value: routecore::bgp::aspath::HopPath) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::AsPath(primitives::AsPath::from(value)))
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
        let as_path: Vec<routecore::bgp::aspath::Hop<Vec<u8>>> = as_path.iter().map(|p| p.0.into()).collect();
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
        TypeValue::List(List::new(value.iter().map(|v| ElementTypeValue::Primitive((*v).into())).collect::<Vec<_>>()))
    }
}

impl From<Vec<TypeValue>> for TypeValue {
    fn from(value: Vec<TypeValue>) -> Self {
        TypeValue::List(List::new(value.iter().map(|v| ElementTypeValue::Primitive((*v).clone())).collect::<Vec<_>>()))
    }
}

impl ScalarValue for TypeValue {}

// Type conversions for Records

// Records do not know how their literals are going to be used/converted, so
// they store them as actual TypeValue::*Literal variants


impl From<crate::ast::StringLiteral> for TypeValue {
   fn from(value: crate::ast::StringLiteral) -> Self {
       TypeValue::Builtin(BuiltinTypeValue::StringLiteral(StringLiteral(value.0)))
   }
}

impl From<crate::ast::IntegerLiteral> for TypeValue {
    fn from(value: crate::ast::IntegerLiteral) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(IntegerLiteral(value.0)))
    }
}

impl From<crate::ast::PrefixLengthLiteral> for TypeValue {
    fn from(value: crate::ast::PrefixLengthLiteral) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::PrefixLength(PrefixLength(value.0 as u8)))
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

impl From<RecordValueExpr> for TypeValue {
    fn from(value: RecordValueExpr) -> Self {
        TypeValue::Record(value.into())
    }
}
