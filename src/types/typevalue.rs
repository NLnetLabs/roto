//------------ TypeValue ------------------------------------------------

use crate::ast::ShortString;

use super::{builtin::{BuiltinTypeValue, U32, U8, Asn, AsPath, Community, Prefix, IpAddress}, collections::{List, Record}, datasources::{Rib, Table}, typedef::TypeDef};

/// These are the actual types that are used in the Roto language. This enum
/// holds both the type-level information and the value. The collection
/// variants can hold multiple values recursively, e.g. a List of Records.

#[derive(Debug, PartialEq)]
pub enum TypeValue {
    // All the built-in scalars
    Builtin(BuiltinTypeValue),
    // An ordered list of one type
    List(List),
    // A map of (key, value) pairs, where value can be any of the other types
    Record(Record),
    // A collection of Records, keyed on Prefix and with special methods for
    // matching prefixes.
    Rib(Rib),
    // Another collections of Records, but in a tabular format without any
    // key, e.g. parsed csv files.
    Table(Table),
    None,
}

impl TypeValue {
    pub fn is_empty(&self) -> bool {
        matches!(self, TypeValue::None)
    }

    pub fn from_literal(s: &str) -> Result<Self, Box<dyn std::error::Error>> {
        match s {
            "U32" => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::U32(U32(None))))
            }
            "U8" => Ok(TypeValue::Builtin(BuiltinTypeValue::U8(U8(None)))),
            "prefix" => Ok(TypeValue::Builtin(BuiltinTypeValue::Prefix(
                Prefix(None),
            ))),
            "PrefixRecord" => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::PrefixRecord((
                    Prefix(None),
                    Record(vec![]),
                ))))
            }
            "IpAddress" => Ok(TypeValue::Builtin(
                BuiltinTypeValue::IpAddress(IpAddress(None)),
            )),
            "Asn" => {
                Ok(TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(None))))
            }
            "AsPath" => Ok(TypeValue::Builtin(BuiltinTypeValue::AsPath(
                AsPath(None),
            ))),
            "Community" => Ok(TypeValue::Builtin(
                BuiltinTypeValue::Community(Community(None)),
            )),
            _ => Err(Box::new(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!("Unknown type: {}", s),
            ))),
        }
    }

    pub fn create_record(
        type_ident_pairs: Vec<(&str, TypeValue)>,
    ) -> Result<Record, Box<dyn std::error::Error>> {
        let def_ = type_ident_pairs
            .into_iter()
            .map(|(ident, ty)| (ShortString::from(ident), ty.into()))
            .collect::<Vec<_>>();
        Record::new(def_)
    }
}

impl<'a> From<&'a TypeDef> for Box<TypeValue> {
    fn from(t: &'a TypeDef) -> Self {
        match t {
            TypeDef::U32 => Box::new(TypeValue::Builtin(
                BuiltinTypeValue::U32(U32(None)),
            )),
            TypeDef::U8 => {
                Box::new(TypeValue::Builtin(BuiltinTypeValue::U8(U8(None))))
            }
            TypeDef::Prefix => Box::new(TypeValue::Builtin(
                BuiltinTypeValue::Prefix(Prefix(None)),
            )),
            TypeDef::IpAddress => Box::new(TypeValue::Builtin(
                BuiltinTypeValue::IpAddress(IpAddress(None)),
            )),
            TypeDef::Asn => Box::new(TypeValue::Builtin(
                BuiltinTypeValue::Asn(Asn(None)),
            )),
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
            _ => panic!("Unknown type"),
        }
    }
}

impl<'a> From<&'a TypeDef> for TypeValue {
    fn from(t: &'a TypeDef) -> Self {
        match t {
            TypeDef::U32 => {
                TypeValue::Builtin(BuiltinTypeValue::U32(U32(None)))
            }
            TypeDef::U8 => {
                TypeValue::Builtin(BuiltinTypeValue::U8(U8(None)))
            }
            TypeDef::Prefix => {
                TypeValue::Builtin(BuiltinTypeValue::Prefix(Prefix(None)))
            }
            TypeDef::IpAddress => TypeValue::Builtin(
                BuiltinTypeValue::IpAddress(IpAddress(None)),
            ),
            TypeDef::Asn => {
                TypeValue::Builtin(BuiltinTypeValue::Asn(Asn(None)))
            }
            TypeDef::AsPath => {
                TypeValue::Builtin(BuiltinTypeValue::AsPath(AsPath(None)))
            }
            TypeDef::Community => TypeValue::Builtin(
                BuiltinTypeValue::Community(Community(None)),
            ),
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
                        .map(|(ident, ty)| {
                            (ident.clone(), ty.as_ref().into())
                        })
                        .collect::<Vec<_>>();
                    TypeValue::Rib(Rib {
                        record: Record::new(def_).unwrap(),
                    })
                } else {
                    panic!("Rib must contains records")
                }
            }
            TypeDef::Table(rec) => {
                if let TypeDef::Record(kv_list) = rec.as_ref() {
                    let def_ = kv_list
                        .iter()
                        .map(|(ident, ty)| {
                            (ident.clone(), ty.as_ref().into())
                        })
                        .collect::<Vec<_>>();
                    TypeValue::Table(Table {
                        record: Record::new(def_).unwrap(),
                    })
                } else {
                    panic!("Table must contain records")
                }
            }
            _ => panic!("Unknown type"),
        }
    }
}

impl std::fmt::Display for TypeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TypeValue::Builtin(p) => write!(f, "primitive type {}", p),
            TypeValue::List(l) => write!(f, "list that contains type {}", l),
            TypeValue::Record(r) => {
                write!(f, "record that contains type {}", r)
            }
            TypeValue::Rib(r) => {
                write!(f, "rib that contains type {}", r)
            }
            TypeValue::Table(r) => {
                write!(f, "table that contains type {}", r)
            }
            TypeValue::None => write!(f, "None"),
        }
    }
}