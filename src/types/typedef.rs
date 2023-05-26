//------------ TypeDef -----------------------------------------------------

// These are all the types the user can create. This enum is used to create
// `user defined` types.

use crate::compile::CompileError;
use crate::traits::Token;
use crate::types::collections::ElementTypeValue;
use crate::types::datasources::NamedTypeDef;
use crate::vm::StackValueRef;
use crate::{
    ast::{AcceptReject, ShortString},
    traits::RotoType,
};

use super::builtin::{
    AsPath, Asn, Boolean, Community, HexLiteral, IntegerLiteral, IpAddress,
    OriginType, Prefix, PrefixLength, RawRouteWithDeltas, StringLiteral, U32,
    U8, LocalPref, NextHop, AtomicAggregator, MultiExitDisc, Hop, RouteStatus,
};
use super::collections::Record;
use super::datasources::{Rib, Table};
use super::outputs::OutputStream;
use super::{
    builtin::BuiltinTypeValue, collections::List, typevalue::TypeValue,
};

#[derive(Clone, Debug, Eq, PartialEq, Default)]
pub enum TypeDef {
    // Data Sources
    Rib(Box<TypeDef>),
    Table(Box<TypeDef>),
    OutputStream(Box<TypeDef>),
    // Collection Types
    List(Box<TypeDef>),
    Record(Vec<NamedTypeDef>),
    // Builtin Types
    U32,
    U8,
    Boolean,
    Prefix,
    PrefixLength, // A u8 prefixes by a /
    IpAddress,
    Asn,
    Route, // BGP Update path attributes
    AsPath,
    Hop,
    Community,
    OriginType,
    LocalPref,
    MultiExitDisc,
    NextHop,
    AtomicAggregator,
    RouteStatus,
    // Literals
    HexLiteral,
    IntegerLiteral,
    StringLiteral,
    AcceptReject(AcceptReject), // used in the apply section
    #[default]
    Unknown,
}

impl TypeDef {
    pub(crate) fn new_record_type_from_short_string(
        type_ident_pairs: Vec<NamedTypeDef>,
    ) -> Result<TypeDef, CompileError> {
        Ok(TypeDef::Record(type_ident_pairs))
    }

    pub fn has_field(&self, field: &str) -> bool {
        match self {
            TypeDef::Record(fields) => {
                fields.iter().any(|(ident, _)| ident == &field)
            }
            _ => false,
        }
    }

    pub fn is_builtin(&self) -> bool {
        !matches!(
            self,
            TypeDef::Rib(_)
                | TypeDef::Table(_)
                | TypeDef::List(_)
                | TypeDef::Record(_)
                | TypeDef::Unknown
        )
    }

    pub fn new_record_type(
        type_ident_pairs: Vec<(&str, Box<TypeDef>)>,
    ) -> Result<TypeDef, CompileError> {
        Ok(TypeDef::Record(
            type_ident_pairs
                .iter()
                .map(|(k, v)| (ShortString::from(*k), v.clone()))
                .collect(),
        ))
    }

    // this function checks that the that the `fields` vec describes the
    // fields present in self. If so it returns the positions in the vec
    // of the corresponding fields, to serve as the token for each field.
    pub(crate) fn has_fields_chain(
        &self,
        fields: &[crate::ast::Identifier],
    ) -> Result<(TypeDef, Token), CompileError> {
        // Data sources (rib and table) are special cases, because they have
        // their methods on the container (the datasource) and not on the
        // contained type. They don't have field access.
        let mut current_type_token: (TypeDef, Token) = (
            if let TypeDef::Table(rec) | TypeDef::Rib(rec) | TypeDef::OutputStream(rec) = self {
                *rec.clone()
            } else {
                self.clone()
            },
            Token::FieldAccess(vec![]),
        );
        for field in fields {
            let mut index = 0;
            match current_type_token {
                (TypeDef::Record(_fields), _) => {
                    if let Some((_, (_, ty))) =
                        _fields.iter().enumerate().find(|(i, (ident, _))| {
                            index = *i;
                            ident == &field.ident.as_str()
                        })
                    {
                        // recurse into the TypeDef of self.
                        current_type_token =
                            (*ty.clone(), current_type_token.1);
                        current_type_token.1.push(index as u8);
                    } else {
                        return Err(format!(
                            "No field named '{}'",
                            field.ident.as_str()
                        )
                        .into());
                    }
                }
                // Route is also special since it doesn't actually have
                // fields access (it is backed by the raw bytes of the
                // update message), but we want to create the illusion
                // that it does have them.
                (TypeDef::Route, _) => {
                    current_type_token =
                        RawRouteWithDeltas::get_props_for_field(
                            field,
                        )?;
                }
                _ => {
                    return Err(format!(
                        "No field named '{}'",
                        field.ident.as_str()
                    )
                    .into());
                }
            };

            // if let (TypeDef::Record(_fields), _) = current_type_token {
            //     if let Some((_, (_, ty))) =
            //         _fields.iter().enumerate().find(|(i, (ident, _))| {
            //             index = *i;
            //             ident == &field.ident.as_str()
            //         })
            //     {
            //         // recurse into the TypeDef of self.
            //         current_type_token = (ty, current_type_token.1);
            //         current_type_token.1.push(index as u8);
            //     } else {
            //         return Err(format!(
            //             "No field named '{}'",
            //             field.ident.as_str()
            //         )
            //         .into());
            //     }
            // } else {
            //     return Err(format!(
            //         "No field named '{}'",
            //         field.ident.as_str()
            //     )
            //     .into());
            // }
        }
        Ok((current_type_token.0.clone(), current_type_token.1))
    }

    pub(crate) fn _check_record_fields(
        &self,
        fields: &[(ShortString, &TypeValue)],
    ) -> bool {
        if let TypeDef::Record(rec) = self {
            for (name, ty) in fields {
                if !rec.iter().any(|(k, v)| k == name && v.as_ref() == *ty) {
                    return false;
                }
            }
            true
        } else {
            false
        }
    }

    pub(crate) fn _check_record_fields_for_ref(
        &self,
        fields: &[(ShortString, TypeDef)],
    ) -> bool {
        if let TypeDef::Record(rec) = self {
            for (name, ty) in fields {
                if !rec.iter().any(|(k, v)| k == name && v.as_ref() == ty) {
                    return false;
                }
            }
            true
        } else {
            false
        }
    }

    pub(crate) fn get_props_for_method(
        &self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError> {
        match self {
            TypeDef::Record(_) => {
                Record::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::Rib(_) => {
                Rib::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::Table(_) => {
                Table::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::OutputStream(_) => {
                OutputStream::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::List(_) => {
                List::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::U32 => {
                U32::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::U8 => {
                U8::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::Boolean => {
                Boolean::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::Prefix => {
                Prefix::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::PrefixLength => {
                PrefixLength::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::IpAddress => {
                IpAddress::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::Asn => {
                Asn::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::AsPath => {
                AsPath::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::Hop => {
                Hop::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::Community => {
                Community::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::OriginType => {
                OriginType::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::Route => RawRouteWithDeltas::get_props_for_method(
                self.clone(),
                method_name,
            ),
            TypeDef::RouteStatus => {
                RouteStatus::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::HexLiteral => {
                HexLiteral::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::IntegerLiteral => IntegerLiteral::get_props_for_method(
                self.clone(),
                method_name,
            ),
            TypeDef::StringLiteral => StringLiteral::get_props_for_method(self.clone(), method_name),
            TypeDef::AcceptReject(_) => todo!(),
            TypeDef::Unknown => todo!(),
            TypeDef::LocalPref => LocalPref::get_props_for_method(self.clone(), method_name),
            TypeDef::MultiExitDisc => MultiExitDisc::get_props_for_method(self.clone(), method_name),
            TypeDef::NextHop => NextHop::get_props_for_method(self.clone(), method_name),
            TypeDef::AtomicAggregator => AtomicAggregator::get_props_for_method(self.clone(), method_name),
        }
    }

    pub(crate) fn exec_type_method<'a>(
        &'a self,
        method_token: usize,
        args: &[StackValueRef],
        return_type: TypeDef,
    ) -> TypeValue {
        match self {
            TypeDef::Record(_rec_type) => {
                Record::exec_type_method(method_token, args, return_type)
                    .unwrap()()
            }
            TypeDef::List(_list) => {
                List::exec_type_method(method_token, args, return_type)
                    .unwrap()()
            }
            TypeDef::AsPath => {
                AsPath::exec_type_method(method_token, args, return_type)
                    .unwrap()()
            }
            TypeDef::Prefix => {
                Prefix::exec_type_method(method_token, args, return_type)
                    .unwrap()()
            }
            TypeDef::U32 => {
                U32::exec_type_method(method_token, args, return_type)
                    .unwrap()()
            }
            TypeDef::StringLiteral => {
                StringLiteral::exec_type_method(method_token, args, return_type)
                    .unwrap()()
            }
            TypeDef::Asn => {
                Asn::exec_type_method(method_token, args, return_type)
                    .unwrap()()
            }
            TypeDef::IpAddress => {
                IpAddress::exec_type_method(method_token, args, return_type)
                    .unwrap()()
            }
            TypeDef::Route => RawRouteWithDeltas::exec_type_method(
                method_token,
                args,
                return_type,
            )
            .unwrap()(),
            TypeDef::Rib(_rib) => {
                Rib::exec_type_method(method_token, args, return_type)
                    .unwrap()()
            }
            TypeDef::Table(_rec) => {
                Table::exec_type_method(method_token, args, return_type)
                    .unwrap()()
            }
            _ => panic!("No corresponding Type method found for {:?}.", self),
        }
    }
}

pub struct MethodProps {
    pub(crate) return_type: TypeDef,
    pub(crate) method_token: Token,
    pub(crate) arg_types: Vec<TypeDef>,
    pub(crate) consume: bool,
}

impl MethodProps {
    pub(crate) fn new(
        return_type_value: TypeDef,
        method_token: usize,
        arg_types: Vec<TypeDef>,
    ) -> Self {
        MethodProps {
            return_type: return_type_value,
            method_token: Token::Method(method_token),
            arg_types,
            consume: false,
        }
    }

    pub(crate) fn consume_value(mut self) -> Self {
        self.consume = true;
        self
    }
}

impl std::fmt::Display for TypeDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeDef::Record(rec) => {
                write!(f, "Record {{")?;
                for (name, ty) in rec {
                    write!(f, "{}: {}, ", name, ty)?;
                }
                write!(f, "}}")
            }
            TypeDef::List(list) => write!(f, "List of {}", list),
            TypeDef::AsPath => write!(f, "AsPath"),
            TypeDef::Hop => write!(f, "Hop"),
            TypeDef::Prefix => write!(f, "Prefix"),
            TypeDef::U32 => write!(f, "U32"),
            TypeDef::Asn => write!(f, "Asn"),
            TypeDef::IpAddress => write!(f, "IpAddress"),
            TypeDef::Route => write!(f, "Route"),
            TypeDef::Rib(rib) => write!(f, "Rib of {}", rib),
            TypeDef::Table(table) => write!(f, "Table of {}", table),
            TypeDef::OutputStream(stream) => write!(f, "Output Stream of {}", stream),
            TypeDef::PrefixLength => write!(f, "PrefixLength"),
            TypeDef::IntegerLiteral => write!(f, "IntegerLiteral"),
            TypeDef::U8 => write!(f, "U8"),
            TypeDef::Boolean => write!(f, "Boolean"),
            TypeDef::Community => write!(f, "Community"),
            TypeDef::OriginType => write!(f, "OriginType"),
            TypeDef::RouteStatus => write!(f, "RouteStatus"),
            TypeDef::HexLiteral => write!(f, "HexLiteral"),
            TypeDef::StringLiteral => write!(f, "String"),
            TypeDef::AcceptReject(_) => write!(f, "AcceptReject"),
            TypeDef::Unknown => write!(f, "None"),
            TypeDef::LocalPref => write!(f, "Local Preference"),
            TypeDef::MultiExitDisc => write!(f, "Multi Exit Discriminator"),
            TypeDef::NextHop => write!(f, "Next Hop"),
            TypeDef::AtomicAggregator => write!(f, "Atomic Aggregator"),
        }
    }
}

impl PartialEq<BuiltinTypeValue> for TypeDef {
    fn eq(&self, other: &BuiltinTypeValue) -> bool {
        match self {
            TypeDef::U32 => {
                matches!(other, BuiltinTypeValue::U32(_))
            }
            TypeDef::U8 => {
                matches!(other, BuiltinTypeValue::U8(_))
            }
            TypeDef::IntegerLiteral => {
                matches!(other, BuiltinTypeValue::IntegerLiteral(_))
            }
            TypeDef::StringLiteral => {
                matches!(other, BuiltinTypeValue::StringLiteral(_))
            }
            TypeDef::Prefix => {
                matches!(other, BuiltinTypeValue::Prefix(_))
            }
            TypeDef::PrefixLength => {
                matches!(other, BuiltinTypeValue::PrefixLength(_))
            }
            TypeDef::IpAddress => {
                matches!(other, BuiltinTypeValue::IpAddress(_))
            }
            TypeDef::Asn => {
                matches!(other, BuiltinTypeValue::Asn(_))
            }
            TypeDef::AsPath => {
                matches!(other, BuiltinTypeValue::AsPath(_))
            }
            TypeDef::RouteStatus => {
                matches!(other, BuiltinTypeValue::RouteStatus(_))
            }
            TypeDef::Community => {
                matches!(other, BuiltinTypeValue::Community(_))
            }
            _ => false,
        }
    }
}

impl PartialEq<TypeValue> for TypeDef {
    fn eq(&self, other: &TypeValue) -> bool {
        match (self, other) {
            (a, TypeValue::Builtin(b)) => a == b,
            (TypeDef::List(a), TypeValue::List(b)) => match (a.as_ref(), b) {
                (TypeDef::List(aa), List(bb)) => match &bb[0] {
                    ElementTypeValue::Nested(bb) => {
                        return aa.as_ref() == bb.as_ref()
                    }
                    ElementTypeValue::Primitive(bb) => {
                        return aa.as_ref() == bb
                    }
                },
                _ => false,
            },
            (TypeDef::Record(a), TypeValue::Record(_b)) => self
                ._check_record_fields_for_ref(
                    a.iter()
                        .map(|ty| (ty.0.clone(), *ty.1.clone()))
                        .collect::<Vec<_>>()
                        .as_slice(),
                ),
            _ => false,
        }
    }
}

// This From impl creates the link between the AST and the TypeDef enum
// for built-in types.
impl TryFrom<crate::ast::TypeIdentifier> for TypeDef {
    type Error = CompileError;
    fn try_from(
        ty: crate::ast::TypeIdentifier,
    ) -> Result<TypeDef, CompileError> {
        match ty.ident.as_str() {
            "U32" => Ok(TypeDef::U32),
            "U8" => Ok(TypeDef::U8),
            "IntegerLiteral" => Ok(TypeDef::IntegerLiteral),
            // StringLiterals are referred to as 'String' in roto. To avond
            // confusion with the Rust `String` it called `StringLiteral`
            // internally
            "String" => Ok(TypeDef::StringLiteral),
            "Prefix" => Ok(TypeDef::Prefix),
            "PrefixLength" => Ok(TypeDef::PrefixLength),
            "IpAddress" => Ok(TypeDef::IpAddress),
            "Asn" => Ok(TypeDef::Asn),
            "AsPath" => Ok(TypeDef::AsPath),
            "Community" => Ok(TypeDef::Community),
            "Route" => Ok(TypeDef::Route),
            "RouteStatus" => Ok(TypeDef::RouteStatus),
            "HexLiteral" => Ok(TypeDef::HexLiteral),
            _ => Err(format!("Undefined type: {}", ty.ident).into()),
        }
    }
}

impl TryFrom<crate::ast::Identifier> for TypeDef {
    type Error = CompileError;
    fn try_from(ty: crate::ast::Identifier) -> Result<TypeDef, CompileError> {
        match ty.ident.as_str() {
            "U32" => Ok(TypeDef::U32),
            "U8" => Ok(TypeDef::U8),
            "IntegerLiteral" => Ok(TypeDef::IntegerLiteral),
            // StringLiterals are referred to as 'String' in roto. To avond
            // confusion with the Rust `String` it called `StringLiteral`
            // internally
            "String" => Ok(TypeDef::StringLiteral),
            "Prefix" => Ok(TypeDef::Prefix),
            "PrefixLength" => Ok(TypeDef::PrefixLength),
            "IpAddress" => Ok(TypeDef::IpAddress),
            "Asn" => Ok(TypeDef::Asn),
            "AsPath" => Ok(TypeDef::AsPath),
            "Community" => Ok(TypeDef::Community),
            "Route" => Ok(TypeDef::Route),
            "RouteStatus" => Ok(TypeDef::RouteStatus),
            "HexLiteral" => Ok(TypeDef::HexLiteral),
            _ => Err(format!("Undefined type: {}", ty.ident).into()),
        }
    }
}

impl From<&BuiltinTypeValue> for TypeDef {
    fn from(ty: &BuiltinTypeValue) -> TypeDef {
        match ty {
            BuiltinTypeValue::U32(_) => TypeDef::U32,
            BuiltinTypeValue::U8(_) => TypeDef::U8,
            BuiltinTypeValue::IntegerLiteral(_) => TypeDef::IntegerLiteral,
            BuiltinTypeValue::StringLiteral(_) => TypeDef::StringLiteral,
            BuiltinTypeValue::Boolean(_) => TypeDef::Boolean,
            BuiltinTypeValue::Prefix(_) => TypeDef::Prefix,
            BuiltinTypeValue::PrefixLength(_) => TypeDef::PrefixLength,
            BuiltinTypeValue::IpAddress(_) => TypeDef::IpAddress,
            BuiltinTypeValue::Asn(_) => TypeDef::Asn,
            BuiltinTypeValue::Hop(_) => TypeDef::Hop,
            BuiltinTypeValue::OriginType(_) => TypeDef::OriginType,
            BuiltinTypeValue::AsPath(_) => TypeDef::AsPath,
            BuiltinTypeValue::Community(_) => TypeDef::Community,
            BuiltinTypeValue::Communities(_) =>  TypeDef::List(Box::new(TypeDef::Community)),
            BuiltinTypeValue::Route(_) => TypeDef::Route,
            BuiltinTypeValue::RouteStatus(_) => TypeDef::RouteStatus,
            BuiltinTypeValue::HexLiteral(_) => TypeDef::HexLiteral,
            BuiltinTypeValue::LocalPref(_) => TypeDef::LocalPref,
            BuiltinTypeValue::AtomicAggregator(_) => TypeDef::AtomicAggregator,
            BuiltinTypeValue::NextHop(_) => TypeDef::NextHop,
            BuiltinTypeValue::MultiExitDisc(_) => TypeDef::MultiExitDisc,
        }
    }
}

impl From<BuiltinTypeValue> for TypeDef {
    fn from(ty: BuiltinTypeValue) -> TypeDef {
        match ty {
            BuiltinTypeValue::U32(_) => TypeDef::U32,
            BuiltinTypeValue::U8(_) => TypeDef::U8,
            BuiltinTypeValue::IntegerLiteral(_) => TypeDef::IntegerLiteral,
            BuiltinTypeValue::StringLiteral(_) => TypeDef::StringLiteral,
            BuiltinTypeValue::Boolean(_) => TypeDef::Boolean,
            BuiltinTypeValue::Prefix(_) => TypeDef::Prefix,
            BuiltinTypeValue::PrefixLength(_) => TypeDef::PrefixLength,
            BuiltinTypeValue::IpAddress(_) => TypeDef::IpAddress,
            BuiltinTypeValue::Asn(_) => TypeDef::Asn,
            BuiltinTypeValue::Hop(_) => TypeDef::Hop,
            BuiltinTypeValue::AsPath(_) => TypeDef::AsPath,
            BuiltinTypeValue::Community(_) => TypeDef::Community,
            BuiltinTypeValue::Communities(_) => TypeDef::List(Box::new(TypeDef::Community)),
            BuiltinTypeValue::OriginType(_) => TypeDef::OriginType,
            BuiltinTypeValue::Route(_) => TypeDef::Route,
            BuiltinTypeValue::RouteStatus(_) => TypeDef::RouteStatus,
            BuiltinTypeValue::HexLiteral(_) => TypeDef::HexLiteral,
            BuiltinTypeValue::LocalPref(_) => TypeDef::LocalPref,
            BuiltinTypeValue::AtomicAggregator(_) => TypeDef::AtomicAggregator,
            BuiltinTypeValue::NextHop(_) => TypeDef::NextHop,
            BuiltinTypeValue::MultiExitDisc(_) => TypeDef::MultiExitDisc
        }
    }
}

impl From<&ElementTypeValue> for TypeDef {
    fn from(ty: &ElementTypeValue) -> TypeDef {
        match ty {
            ElementTypeValue::Primitive(ty) => ty.into(),
            ElementTypeValue::Nested(ty) => ty.as_ref().into(),
        }
    }
}

impl From<&TypeValue> for TypeDef {
    fn from(ty: &TypeValue) -> TypeDef {
        match ty {
            TypeValue::Builtin(b) => b.into(),
            TypeValue::List(l) => match l {
                List(l) => match &l[0] {
                    ElementTypeValue::Nested(n) => {
                        TypeDef::List(Box::new((&(**n)).into()))
                    }
                    ElementTypeValue::Primitive(p) => {
                        TypeDef::List(Box::new(p.into()))
                    }
                },
            },
            TypeValue::Record(r) => TypeDef::Record(
                r.0.iter()
                    .map(|(k, v)| (k.clone(), Box::new(v.into())))
                    .collect(),
            ),
            TypeValue::Rib(r) => r.ty.clone(),
            TypeValue::Table(t) => t.ty.clone(),
            TypeValue::OutputStream(m) => m.record_type.clone(),
            TypeValue::SharedValue(sv) => TypeDef::from(sv.as_ref()),
            TypeValue::Unknown => TypeDef::Unknown,
            TypeValue::UnInit => TypeDef::Unknown,
        }
    }
}
