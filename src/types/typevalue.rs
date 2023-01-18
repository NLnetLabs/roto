use std::{cmp::Ordering, fmt::Display};

//============ TypeValue ====================================================
use crate::{
    ast::ShortString, compile::CompileError, traits::RotoFilter, vm::{VmError, StackRef, StackRefPos, Payload},
};

use super::{
    builtin::{
        AsPath, Asn, Boolean, BuiltinTypeValue, Community, HexLiteral,
        IntegerLiteral, IpAddress, Prefix, PrefixLength,
        StringLiteral, U32, U8
    },
    collections::{ElementTypeValue, List, Record},
    datasources::{Rib, Table},
    typedef::TypeDef,
};

/// These are the actual types that are used in the Roto language. This enum
/// holds both the type-level information and the value. The collection
/// variants can hold multiple values recursively, e.g. a List of Records.

#[derive(Debug, PartialEq, Eq, Default)]
pub enum TypeValue {
    // All the built-in scalars
    Builtin(BuiltinTypeValue),
    // An ordered list of one user-defined type
    List(List),
    // A map of (key, value) pairs, where value can be any of the other types.
    // Always user-defined.
    Record(Record),
    // A collection of Records, keyed on Prefix and with special methods for
    // matching prefixes.
    Rib(Rib),
    // Another collections of Records, but in a tabular format without any
    // key, e.g. parsed csv files.
    Table(Table),
    #[default]
    // Unknown is NOT EQUAL tO empty or unitialized, e.g. it may be the
    // result of a search. A ternary logic value, if you will.
    Unknown,
    // Used for LinearMemory online, it's the initial state of all positions
    // except the first two positions (rx and tx)
    UnInit
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
        if let TypeValue::Builtin(BuiltinTypeValue::Boolean(bool_val)) = self {
            bool_val.is_false()
        } else {
            Err(VmError::InvalidValueType)
        }
    }

    pub(crate) fn as_cloned_builtin(
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
                    ).as_str()
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
                    .as_str().into()
                })
            }
            _ => Err(format!("Type '{:?}' is not a record.", self).into()),
        }
    }

    pub(crate) fn set_field_by_stack_ref(&mut self, stack_ref: &StackRef, value: TypeValue) -> Result<(), VmError> {
        if let StackRefPos::MemPos(index) = stack_ref.pos {
            match self {
                TypeValue::Record(rec) => {
                    rec.set_field_for_index(index as usize, value)?
                }
                TypeValue::List(list) => {
                    list.set_field_for_index(index as usize, value)?
                }
                _ => return Err(VmError::InvalidWrite)
            };
        } else {
            return Err(VmError::InvalidWrite);
        };
        
        Ok(())
    }

    pub(crate) fn exec_value_method(
        &self,
        method_token: usize,
        args: &[&TypeValue],
        return_type: TypeDef,
    ) -> TypeValue {
        match self {
            TypeValue::Record(rec_type) => rec_type
                .exec_value_method(method_token, args, return_type)
                .unwrap()(),
            TypeValue::List(list) => list
                .exec_value_method(method_token, args, return_type)
                .unwrap()(),
            TypeValue::Builtin(BuiltinTypeValue::AsPath(as_path)) => as_path
                .exec_value_method(method_token, args, return_type)
                .unwrap()(
            ),
            TypeValue::Builtin(BuiltinTypeValue::Prefix(prefix)) => prefix
                .exec_value_method(method_token, args, return_type)
                .unwrap()(
            ),
            TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(lit_int)) => {
                lit_int
                    .exec_value_method(method_token, args, return_type)
                    .unwrap()()
            }
            TypeValue::Builtin(BuiltinTypeValue::StringLiteral(lit_str)) => {
                lit_str
                    .exec_value_method(method_token, args, return_type)
                    .unwrap()()
            }
            TypeValue::Builtin(BuiltinTypeValue::HexLiteral(lit_hex)) => {
                lit_hex
                    .exec_value_method(method_token, args, return_type)
                    .unwrap()()
            }
            TypeValue::Builtin(BuiltinTypeValue::U32(u32)) => {
                u32.exec_value_method(method_token, args, return_type)
                    .unwrap()()
            }
            TypeValue::Builtin(BuiltinTypeValue::Asn(asn)) => {
                asn.exec_value_method(method_token, args, return_type)
                    .unwrap()()
            }
            TypeValue::Builtin(BuiltinTypeValue::IpAddress(ip)) => {
                ip.exec_value_method(method_token, args, return_type)
                    .unwrap()()
            }
            TypeValue::Builtin(BuiltinTypeValue::Route(route)) => route
                .exec_value_method(method_token, args, return_type)
                .unwrap()(
            ),
            TypeValue::Builtin(BuiltinTypeValue::Community(community)) => {
                community
                    .exec_value_method(method_token, args, return_type)
                    .unwrap()()
            }
            TypeValue::Builtin(BuiltinTypeValue::U8(u8_lit)) => u8_lit
                .exec_value_method(method_token, args, return_type)
                .unwrap()(
            ),
            TypeValue::Builtin(BuiltinTypeValue::Boolean(boolean)) => boolean
                .exec_value_method(method_token, args, return_type)
                .unwrap()(
            ),
            TypeValue::Builtin(BuiltinTypeValue::PrefixLength(
                prefix_length,
            )) => prefix_length
                .exec_value_method(method_token, args, return_type)
                .unwrap()(),
            TypeValue::Builtin(BuiltinTypeValue::RouteStatus(
                route_status,
            )) => route_status
                .exec_value_method(method_token, args, return_type)
                .unwrap()(),
            TypeValue::Rib(rib) => rib
                .exec_value_method(method_token, args, return_type)
                .unwrap()(),
            TypeValue::Table(rec) => rec
                .exec_value_method(method_token, args, return_type)
                .unwrap()(),
            TypeValue::Unknown => {
                TypeValue::Unknown
            }
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
            TypeValue::List(l) => write!(f, "{} (List Element)", l),
            TypeValue::Record(r) => {
                write!(f, "{} (Record)", r)
            }
            TypeValue::Rib(r) => {
                write!(f, "{} (Rib Record)", r)
            }
            TypeValue::Table(r) => {
                write!(f, "{} (Table Entry)", r)
            }
            TypeValue::Unknown => write!(f, "Unknown"),
            TypeValue::UnInit => write!(f, "Uninitialized")
        }
    }
}

impl PartialOrd for &TypeValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (
                TypeValue::Builtin(BuiltinTypeValue::U8(U8(Some(u)))),
                TypeValue::Builtin(BuiltinTypeValue::U8(U8(Some(v)))),
            ) => Some(u.cmp(v)),
            (
                TypeValue::Builtin(BuiltinTypeValue::U32(U32(Some(u)))),
                TypeValue::Builtin(BuiltinTypeValue::U32(U32(Some(v)))),
            ) => Some(u.cmp(v)),
            (
                TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(
                    IntegerLiteral(Some(u)),
                )),
                TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(
                    IntegerLiteral(Some(v)),
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
            (TypeValue::Rib(_), TypeValue::Rib(_)) => {
                panic!("Ribs are not comparable.")
            }
            (TypeValue::Table(_), TypeValue::Table(_)) => {
                panic!("Tables are not comparable.")
            }
            (TypeValue::Unknown, TypeValue::Unknown) => {
                panic!("Unknown is unsortable.")
            }
            _ => {
                panic!("Incomparable types.")
            }
        }
    }
}

impl Ord for &TypeValue {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (
                TypeValue::Builtin(BuiltinTypeValue::U8(U8(Some(u1)))),
                TypeValue::Builtin(BuiltinTypeValue::U8(U8(Some(u2)))),
            ) => u1.cmp(u2),
            (TypeValue::List(_l1), TypeValue::List(_l2)) => {
                panic!("Lists are not comparable.")
            }
            (TypeValue::Record(_r1), TypeValue::Record(_r2)) => {
                panic!("Records are not comparable.")
            }
            (TypeValue::Rib(_r1), TypeValue::Rib(_r2)) => {
                panic!("Ribs are not comparable.")
            }
            (TypeValue::Table(_r1), TypeValue::Table(_r2)) => {
                panic!("Tables are not comparable.")
            }
            (TypeValue::Unknown, TypeValue::Unknown) => Ordering::Equal,
            (TypeValue::Builtin(_), _) => Ordering::Less,
            (_, TypeValue::Builtin(_)) => Ordering::Greater,
            (TypeValue::List(_), _) => Ordering::Less,
            (_, TypeValue::List(_)) => Ordering::Greater,
            (TypeValue::Record(_), _) => Ordering::Less,
            (_, TypeValue::Record(_)) => Ordering::Greater,
            (TypeValue::Rib(_), _) => Ordering::Less,
            (_, TypeValue::Rib(_)) => Ordering::Greater,
            (TypeValue::Table(_), _) => Ordering::Less,
            (_, TypeValue::Table(_)) => Ordering::Greater,
            (_, TypeValue::UnInit) => panic!("comparing with unitialized memory."),
            (TypeValue::UnInit, _) => panic!("comparing with unitialized memory.")
        }
    }
}

impl<'a> TryFrom<&'a TypeValue> for bool {
    type Error = VmError;

    fn try_from(t: &'a TypeValue) -> Result<Self, Self::Error> {
        match t {
            TypeValue::Builtin(BuiltinTypeValue::Boolean(b)) => {
                b.0.ok_or(VmError::ImpossibleComparison)
            }
            _ => Err(VmError::ImpossibleComparison),
        }
    }
}

impl<'a> TryFrom<&'a str> for TypeValue {
    type Error = CompileError;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        match s {
            "U32" => Ok(TypeValue::Builtin(BuiltinTypeValue::U32(U32(None)))),
            "U8" => Ok(TypeValue::Builtin(BuiltinTypeValue::U8(U8(None)))),
            "Prefix" => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::Prefix(Prefix(None))))
            }
            "IpAddress" => Ok(TypeValue::Builtin(
                BuiltinTypeValue::IpAddress(IpAddress(None)),
            )),
            "Asn" => Ok(TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(None)))),
            "AsPath" => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::AsPath(AsPath(None))))
            }
            "Community" => Ok(TypeValue::Builtin(
                BuiltinTypeValue::Community(Community(None)),
            )),
            "Boolean" => Ok(TypeValue::Builtin(BuiltinTypeValue::Boolean(
                Boolean(None),
            ))),
            _ => Err(CompileError::new(format!("Unknown type: {}", s))),
        }
    }
}

impl<'a> From<&'a TypeDef> for Box<TypeValue> {
    fn from(t: &'a TypeDef) -> Self {
        match t {
            TypeDef::U32 => {
                Box::new(TypeValue::Builtin(BuiltinTypeValue::U32(U32(None))))
            }
            TypeDef::U8 => {
                Box::new(TypeValue::Builtin(BuiltinTypeValue::U8(U8(None))))
            }
            TypeDef::PrefixLength => Box::new(TypeValue::Builtin(
                BuiltinTypeValue::PrefixLength(PrefixLength(None)),
            )),
            TypeDef::Prefix => Box::new(TypeValue::Builtin(
                BuiltinTypeValue::Prefix(Prefix(None)),
            )),
            TypeDef::IpAddress => Box::new(TypeValue::Builtin(
                BuiltinTypeValue::IpAddress(IpAddress(None)),
            )),
            TypeDef::Asn => {
                Box::new(TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(None))))
            }
            TypeDef::AsPath => Box::new(TypeValue::Builtin(
                BuiltinTypeValue::AsPath(AsPath(None)),
            )),
            TypeDef::Community => Box::new(TypeValue::Builtin(
                BuiltinTypeValue::Community(Community(None)),
            )),
            TypeDef::List(ty) => {
                Box::new(TypeValue::List(ty.as_ref().into()))
            }
            TypeDef::Record(kv_list) => {
                let def_ = kv_list
                    .iter()
                    .map(|(ident, ty)| (ident.clone(), ty.as_ref().into()))
                    .collect::<Vec<_>>();
                Box::new(TypeValue::Record(Record::new(def_).unwrap()))
            }
            // Literals
            // They have no business here, but IntegerLiteral and HexLiteral
            // are special, since they can be converted into different types
            // based on who's using them as arguments.

            // IntegerLiteral can be converted into U32, U8, I64.
            TypeDef::IntegerLiteral => Box::new(TypeValue::Builtin(
                BuiltinTypeValue::IntegerLiteral(IntegerLiteral(None)),
            )),
            // HexLiteral can be converted into different community types
            // (standard, extended, large, etc.)
            TypeDef::HexLiteral => Box::new(TypeValue::Builtin(
                BuiltinTypeValue::HexLiteral(HexLiteral(None)),
            )),

            _ => {
                println!("panic on type {:?}", t);
                panic!("Unknown type")
            }
        }
    }
}

impl<'a> From<&'a TypeDef> for TypeValue {
    fn from(t: &'a TypeDef) -> Self {
        match t {
            TypeDef::U32 => {
                // let v = U32::into_type(U32(None), t).unwrap();
                TypeValue::Builtin(BuiltinTypeValue::U32(U32(None)))
            }
            TypeDef::U8 => TypeValue::Builtin(BuiltinTypeValue::U8(U8(None))),
            TypeDef::Prefix => {
                TypeValue::Builtin(BuiltinTypeValue::Prefix(Prefix(None)))
            }
            TypeDef::PrefixLength => TypeValue::Builtin(
                BuiltinTypeValue::PrefixLength(PrefixLength(None)),
            ),
            TypeDef::IpAddress => TypeValue::Builtin(
                BuiltinTypeValue::IpAddress(IpAddress(None)),
            ),
            TypeDef::Asn => {
                println!("into asn");
                TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(None)))
            }
            TypeDef::AsPath => {
                TypeValue::Builtin(BuiltinTypeValue::AsPath(AsPath(None)))
            }
            TypeDef::Community => TypeValue::Builtin(
                BuiltinTypeValue::Community(Community(None)),
            ),
            TypeDef::Boolean => {
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(None)))
            }
            TypeDef::List(ty) => TypeValue::List(ty.as_ref().into()),
            TypeDef::Record(kv_list) => {
                let def_ = kv_list
                    .iter()
                    .map(|(ident, ty)| (ident.clone(), ty.as_ref().into()))
                    .collect::<Vec<_>>();
                TypeValue::Record(Record::new(def_).unwrap())
            }
            TypeDef::Rib(rec) => {
                if let TypeDef::Record(kv_list) = rec.as_ref() {
                    let def_ = kv_list
                        .iter()
                        .map(|(ident, ty)| (ident.clone(), ty.clone()))
                        .collect::<Vec<_>>();
                    TypeValue::Rib(Rib {
                        ty: TypeDef::Record(def_),
                        records: vec![]
                    })
                } else {
                    panic!("Rib must contains records")
                }
            }
            TypeDef::Table(rec) => {
                if let TypeDef::Record(kv_list) = rec.as_ref() {
                    let def_ = kv_list
                        .iter()
                        .map(|(ident, ty)| (ident.clone(), ty.clone()))
                        .collect::<Vec<_>>();
                    TypeValue::Table(Table {
                        ty: TypeDef::Record(def_),
                        records: vec![],
                    })
                } else {
                    panic!("Table must contain records")
                }
            }
            // Literals
            // They have no business here, but IntegerLiteral and HexLiteral
            // are special, since they can be converted into different types
            // based on who's using them as arguments.

            // IntegerLiteral can be converted into U32, U8, I64.
            TypeDef::IntegerLiteral => TypeValue::Builtin(
                BuiltinTypeValue::IntegerLiteral(IntegerLiteral(None)),
            ),
            // HexLiteral can be converted into different community types
            // (standard, extended, large, etc.)
            TypeDef::HexLiteral => TypeValue::Builtin(
                BuiltinTypeValue::HexLiteral(HexLiteral(None)),
            ),
            _ => panic!("Unknown type {:?}", t),
        }
    }
}

impl From<BuiltinTypeValue> for TypeValue {
    fn from(t: BuiltinTypeValue) -> Self {
        TypeValue::Builtin(t)
    }
}

impl From<bool> for TypeValue {
    fn from(val: bool) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(Some(val))))
    }
}