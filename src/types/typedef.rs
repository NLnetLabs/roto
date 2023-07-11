//------------ TypeDef -----------------------------------------------------

use std::hash::{Hash, Hasher};

// These are all the types the user can create. This enum is used to create
// `user defined` types.
use log::trace;
use serde::Serialize;
use smallvec::SmallVec;

use crate::compile::CompileError;
use crate::traits::Token;
use crate::typedefconversion;
use crate::types::builtin::BgpUpdateMessage;
use crate::types::collections::{ElementTypeValue};
use crate::vm::{StackValue, VmError};
use crate::{
    ast::{AcceptReject, ShortString},
    traits::RotoType,
};

use super::builtin::{
    AsPath, Asn, AtomicAggregator, Boolean, Community, HexLiteral, Hop,
    IntegerLiteral, IpAddress, LocalPref, MultiExitDisc, NextHop, OriginType,
    Prefix, PrefixLength, RawRouteWithDeltas, RouteStatus, StringLiteral,
    Unknown, U32, U8,
};
use super::collections::{LazyElementTypeValue, Record};
use super::constant_enum::{Enum, EnumVariant};
use super::datasources::{RibType, Table};
use super::lazytypedef::LazyTypeDef;
use super::outputs::OutputStreamMessage;
use super::{
    builtin::BuiltinTypeValue, collections::List, typevalue::TypeValue,
};

// the type definition of the type that's stored in the RIB and the
// vec of field_indexes that are used in the hash to calculate
// uniqueness for an entry.
pub type RibTypeDef = (Box<TypeDef>, Option<Vec<SmallVec<[usize; 8]>>>);
pub type NamedTypeDef = (ShortString, Box<TypeDef>);
pub type LazyNamedTypeDef<'a, T> =
    Vec<(ShortString, LazyElementTypeValue<'a, T>)>;

#[derive(Clone, Debug, Eq, PartialEq, Default, Hash, Serialize)]
pub enum TypeDef {
    // Data Sources, the data field in the enum represents the contained
    // type.
    Rib(RibTypeDef),
    Table(Box<TypeDef>),
    OutputStream(Box<TypeDef>),
    // Collection Types
    List(Box<TypeDef>),
    Record(Vec<NamedTypeDef>),
    Enum(Box<TypeDef>),
    // The data field holds the name of the enum this variant belongs to.
    ConstEnumVariant(ShortString),
    // The data field holds the name of the enum this variant belongs to.
    // ConstU16EnumVariant(ShortString),
    // ConstU32EnumVariant(ShortString),
    // A raw BGP message as bytes
    BgpUpdateMessage,
    LazyRecord(LazyTypeDef),
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
    // The function defined by this macro called `can_convert_into_type()`
    // indicates whether the self type can be converted into another
    // specified type. This function is used during Evaluation.
    // This happens ONLY ON THE UNBOUNDED TYPES, meaning that Type
    // dependencies, e.g. a PrefixLength can't be converted from an U32 that
    // holds a value bigger than 128, is NOT checked here. That check is done
    // during compilation.

    // Likewise for a Record, the only check is that it
    // can be converted into another Record, not that the sub-types of the
    // records match.

    // The conversions indicated here is unidirectional, e.g. a line
    // `U8(U32,PrefixLength,IntegerLiteral;),` means that an U8 can converted
    // to U32, PrefixLength and IntegerLiteral, but not the other way around.
    typedefconversion!(
        // have conversions, no data field
        // SOURCE TYPE(TARGET TYPE WITHOUT DATA FIELD, ..;
        // TARGET TYPE WITH DATA FIELD)
        U8(U32,PrefixLength,IntegerLiteral;),
        U32(StringLiteral,IntegerLiteral;),
        Boolean(StringLiteral;),
        IpAddress(StringLiteral;),
        Prefix(StringLiteral;),
        Hop(StringLiteral;),
        Community(StringLiteral;),
        OriginType(StringLiteral;),
        NextHop(StringLiteral;),
        RouteStatus(StringLiteral;),
        IntegerLiteral(U8,U32,PrefixLength,LocalPref,Asn;),
        StringLiteral(Asn;),
        HexLiteral(U8,U32,Community;),
        PrefixLength(U8,U32;),
        Asn(U32,StringLiteral;),
        AsPath(StringLiteral;List),
        LocalPref(U8,IntegerLiteral,StringLiteral;),
        MultiExitDisc(U8,IntegerLiteral,StringLiteral;),
        AtomicAggregator(U8;);
        // have conversions, have data field
        Record(;Record,OutputStream),
        LazyRecord(;Record,OutputStream),
        AcceptReject(StringLiteral;);
        // no conversions, no data field
        // SOURCE TYPE
        Route,
        BgpUpdateMessage,
        Unknown;
        // no conversions, have data field
        // SOURCE TYPE
        List,
        Enum,
        Rib,
        Table,
        OutputStream,
        ConstEnumVariant
    );

    pub(crate) fn new_record_type_from_short_string(
        type_ident_pairs: Vec<NamedTypeDef>,
    ) -> Result<TypeDef, CompileError> {
        Ok(TypeDef::Record(type_ident_pairs))
    }

    // Gets the type of a field of a Record Type, which canbe a porimitive,
    // but it can also be an anonymous record type.
    pub fn get_field(&self, field: &str) -> Option<TypeDef> {
        match self {
            TypeDef::Record(fields) => fields
                .iter()
                .find(|(ident, _)| ident == &field)
                .map(|td| *td.1.clone()),
            _ => None,
        }
    }

    pub fn is_builtin(&self) -> bool {
        !matches!(
            self,
            TypeDef::Rib(_)
                | TypeDef::Table(_)
                | TypeDef::List(_)
                | TypeDef::Record(_)
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

    // this function checks that the `fields` vec describes the fields
    // present in self. If so it returns the positions in the vec of the
    // corresponding fields, to serve as the token for each field.
    pub(crate) fn has_fields_chain(
        &self,
        check_fields: &[crate::ast::Identifier],
    ) -> Result<(TypeDef, Token), CompileError> {
        // Data sources (rib and table) are special cases, because they have
        // their methods on the container (the datasource) and not on the
        // contained type. They don't have field access.
        let mut parent_type: (TypeDef, Token) = (
            if let TypeDef::Table(rec)
            | TypeDef::Rib((rec, None))
            | TypeDef::OutputStream(rec) = self
            {
                *rec.clone()
            } else {
                self.clone()
            },
            Token::FieldAccess(vec![]),
        );

        let mut result_type = parent_type.clone();

        for field in check_fields {
            let mut index = 0;

            match &parent_type {
                (TypeDef::Record(found_fields), _) => {
                    trace!("Record w/ field '{}'", field);

                    // Check if this field exists in the TypeDef of the
                    // Record.
                    if let Some((_, (_, ty))) = found_fields
                        .iter()
                        .enumerate()
                        .find(|(i, (ident, _))| {
                            index = *i;
                            ident == &field.ident.as_str()
                        })
                    {
                        // Add up all the type defs in the data field
                        // of self.
                        parent_type = (*ty.clone(), parent_type.1);
                        parent_type.1.push(index as u8);
                    } else {
                        return Err(format!(
                            "No field named '{}'",
                            field.ident.as_str()
                        )
                        .into());
                    }

                    result_type = parent_type.clone();
                }
                // Route is also special since it doesn't actually have
                // fields access (it is backed by the raw bytes of the
                // update message), but we want to create the illusion
                // that it does have them.
                (TypeDef::Route, _) => {
                    trace!("Route w/ field '{}'", field);
                    let this_token =
                        RawRouteWithDeltas::get_props_for_field(field)?;

                    // Add the token to the FieldAccess vec.
                    result_type = if let Token::FieldAccess(to_f) =
                        &result_type.1
                    {
                        if let Token::FieldAccess(fa) = &this_token.1 {
                            let mut to_f1 = to_f.clone();
                            to_f1.extend(fa);
                            (this_token.0.clone(), Token::FieldAccess(to_f1))
                        } else {
                            result_type
                        }
                    } else {
                        result_type
                    };
                }
                // Another special case: BgpUpdateMessage also doesn't have
                // actual fields, they are all simulated
                (TypeDef::BgpUpdateMessage, _) => {
                    trace!("BgpUpdateMessage w/ field '{}'", field);
                    parent_type =
                        BgpUpdateMessage::get_props_for_field(field)?;

                    // Add the token to the FieldAccess vec.
                    result_type = if let Token::FieldAccess(to_f) =
                        &result_type.1
                    {
                        if let Token::FieldAccess(fa) = &parent_type.1 {
                            let mut to_f1 = to_f.clone();
                            to_f1.extend(fa);
                            (parent_type.0.clone(), Token::FieldAccess(to_f1))
                        } else {
                            result_type
                        }
                    } else {
                        result_type
                    };
                }
                // Another special case: BgpUpdateMessage also doesn't have
                // actual fields, they are all simulated
                (TypeDef::LazyRecord(lazy_type_def), _) => {
                    parent_type =
                        lazy_type_def.get_props_for_field(field)?;
                    // Add the token to the FieldAccess vec.
                    result_type = if let Token::FieldAccess(to_f) =
                        &result_type.1
                    {
                        if let Token::FieldAccess(fa) = &parent_type.1 {
                            let mut to_f1 = to_f.clone();
                            to_f1.extend(fa);
                            (parent_type.0.clone(), Token::FieldAccess(to_f1))
                        } else {
                            result_type
                        }
                    } else {
                        result_type
                    };
                }
                _ => {
                    return Err(format!(
                        "No field named '{}'",
                        field.ident.as_str()
                    )
                    .into());
                }
            };
        }

        trace!("has_fields_chain {:?}", parent_type);

        Ok(result_type)
    }

    // This does a strict check to see if all the names of the fields and
    // their types match up. It does not take into account possible type
    // conversions on fields.
    pub(crate) fn _check_record_fields(
        &self,
        fields: &[(ShortString, &TypeValue)],
    ) -> bool {
        let mut field_count = 0;
        if let TypeDef::Record(rec) = self {
            for (name, ty) in fields {
                if !rec.iter().any(|(k, v)| k == name && v.as_ref() == *ty) {
                    trace!(
                        "Error in field instance '{}' of type {}",
                        name,
                        ty
                    );
                    trace!("record {:?}", rec);
                    return false;
                }
                field_count += 1;
            }

            if field_count != rec.len() {
                trace!("Missing fields in record {:?}", self);
                return false;
            }
            true
        } else {
            trace!("no record, return false for type {}", self);
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
            TypeDef::Enum(_) => {
                Enum::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::ConstEnumVariant(_) => {
                EnumVariant::<u8>::get_props_for_method(
                    self.clone(),
                    method_name,
                )
            }
            // TypeDef::ConstU16EnumVariant(_) => {
            //     EnumVariant::<u16>::get_props_for_method(self.clone(), method_name)
            // }
            // TypeDef::ConstU32EnumVariant(_) => {
            //     EnumVariant::<u32>::get_props_for_method(self.clone(), method_name)
            // }
            TypeDef::Rib(_) => {
                RibType::get_props_for_method(self, method_name)
            }
            TypeDef::Table(_) => {
                Table::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::OutputStream(_) => {
                OutputStreamMessage::get_props_for_method(
                    self.clone(),
                    method_name,
                )
            }
            TypeDef::List(_) => {
                List::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::BgpUpdateMessage => {
                BgpUpdateMessage::get_props_for_method(
                    self.clone(),
                    method_name,
                )
            }
            TypeDef::LazyRecord(lazy_type_def) => 
                lazy_type_def.get_props_for_method(
                    self.clone(), method_name
                )
            ,
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
            TypeDef::StringLiteral => {
                StringLiteral::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::AcceptReject(_) => {
                Err(CompileError::from("AcceptReject type has no methods"))
            }
            TypeDef::Unknown => {
                Unknown::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::LocalPref => {
                LocalPref::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::MultiExitDisc => {
                MultiExitDisc::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::NextHop => {
                NextHop::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::AtomicAggregator => {
                AtomicAggregator::get_props_for_method(
                    self.clone(),
                    method_name,
                )
            }
        }
    }

    pub(crate) fn exec_type_method(
        &self,
        method_token: usize,
        args: &[StackValue],
        return_type: TypeDef,
    ) -> TypeValue {
        match self {
            TypeDef::Record(_rec_type) => {
                Record::exec_type_method(method_token, args, return_type)
                    .unwrap()
            }
            TypeDef::List(_list) => {
                List::exec_type_method(method_token, args, return_type)
                    .unwrap()
            }
            TypeDef::AsPath => {
                AsPath::exec_type_method(method_token, args, return_type)
                    .unwrap()
            }
            TypeDef::Prefix => {
                Prefix::exec_type_method(method_token, args, return_type)
                    .unwrap()
            }
            TypeDef::U32 => {
                U32::exec_type_method(method_token, args, return_type)
                    .unwrap()
            }
            TypeDef::StringLiteral => StringLiteral::exec_type_method(
                method_token,
                args,
                return_type,
            )
            .unwrap(),
            TypeDef::Asn => {
                Asn::exec_type_method(method_token, args, return_type)
                    .unwrap()
            }
            TypeDef::IpAddress => {
                IpAddress::exec_type_method(method_token, args, return_type)
                    .unwrap()
            }
            TypeDef::Route => RawRouteWithDeltas::exec_type_method(
                method_token,
                args,
                return_type,
            )
            .unwrap(),
            TypeDef::Rib(_rib) => {
                RibType::exec_type_method(method_token, args, return_type)
                    .unwrap()
            }
            TypeDef::Table(_rec) => {
                Table::exec_type_method(method_token, args, return_type)
                    .unwrap()
            }
            _ => panic!("No corresponding Type method found for {:?}.", self),
        }
    }

    // Calculates the hash over the fields that are referenced in the unique
    // field indexes vec  that lives on the Rib typedef.
    // If there's no field indexes vec then simply calculate the hash over
    // the (whole) typevalue that was passed in.
    pub fn hash_key_values<'a, H: Hasher>(
        &'a self,
        state: &'a mut H,
        value: &'a TypeValue,
    ) -> Result<(), VmError> {
        if let TypeDef::Rib((_ty, Some(uniq_field_indexes))) = self {
            match value {
                TypeValue::Record(rec) => {
                    for field_index in uniq_field_indexes {
                        rec.get_field_by_index(field_index.clone())
                            .hash(state);
                    }
                }
                TypeValue::Builtin(BuiltinTypeValue::Route(route)) => {
                    for field_index in uniq_field_indexes {
                        route.get_field_by_index(field_index[0]).hash(state);
                    }
                }
                TypeValue::Builtin(btv) => {
                    btv.hash(state);
                }
                TypeValue::List(l) => {
                    l.hash(state);
                }
                TypeValue::Enum(e) => {
                    e.hash(state);
                }
                TypeValue::SharedValue(sv) => {
                    sv.hash(state);
                }
                _ => {
                    return Err(VmError::InvalidPayload);
                }
            }
        } else {
            return Err(VmError::InvalidWrite);
        };
        Ok(())
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
            TypeDef::Enum(c_enum) => write!(f, "Enum of {}", c_enum),
            TypeDef::ConstEnumVariant(c_enum) => {
                write!(f, "ConsU8tEnumVariant('{}')", c_enum)
            }
            TypeDef::AsPath => write!(f, "AsPath"),
            TypeDef::Hop => write!(f, "Hop"),
            TypeDef::Prefix => write!(f, "Prefix"),
            TypeDef::U32 => write!(f, "U32"),
            TypeDef::Asn => write!(f, "Asn"),
            TypeDef::IpAddress => write!(f, "IpAddress"),
            TypeDef::Route => write!(f, "Route"),
            TypeDef::BgpUpdateMessage => write!(f, "BgpUpdateMessage"),
            TypeDef::LazyRecord(lazy_type_def) => {
                write!(f, "Lazy Record {{")?;
                for (name, ty) in lazy_type_def.type_def() {
                    write!(f, "{}: {}, ", name, ty)?;
                }
                write!(f, "}}")
            },
            TypeDef::Rib(rib) => write!(f, "Rib of {}", rib.0),
            TypeDef::Table(table) => write!(f, "Table of {}", table),
            TypeDef::OutputStream(stream) => {
                write!(f, "Output Stream of {}", stream)
            }
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
            (a, TypeValue::List(b)) => match (a, b) {
                (TypeDef::List(aa), List(bb)) => match &bb[0] {
                    ElementTypeValue::Nested(bb) => {
                        trace!("element type value nested {}", bb);
                        return aa.as_ref() == bb.as_ref();
                    }
                    ElementTypeValue::Primitive(bb) => {
                        trace!("compare {} with primitive type value nested {}; result {}", aa, bb, aa.as_ref() == bb);
                        return aa.as_ref() == bb;
                    }
                },
                _ => {
                    trace!("False: {:?} <-> {:?}", a, b);
                    false
                }
            },
            (TypeDef::Record(rec), TypeValue::Record(_b)) => {
                trace!("compare {:?} <-> {:?}", rec, _b);

                let fields =
                    _b.0.iter()
                        .map(|ty| (ty.0.clone(), (&ty.1).into()))
                        .collect::<Vec<(_, TypeDef)>>();
                let mut field_count = 0;

                for (name, ty) in fields.as_slice() {
                    if !rec.iter().any(|(k, v)| k == name && v.as_ref() == ty)
                    {
                        trace!(
                            "Error in field instance '{}' of type {}",
                            name,
                            ty
                        );
                        trace!("record {:?}", rec);
                        return false;
                    }
                    field_count += 1;
                }

                if field_count != rec.len() {
                    trace!("Missing fields in record {:?}", self);
                    return false;
                }
                true
            }
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
            "BgpUpdateMessage" => Ok(TypeDef::BgpUpdateMessage),
            "BmpRouteMonitoringMessage" => Ok(TypeDef::LazyRecord(LazyTypeDef::BmpRouteMonitoringMessage)),
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
            "BgpUpdateMessage" => Ok(TypeDef::BgpUpdateMessage),
            "BmpRouteMonitoringMessage" => Ok(TypeDef::LazyRecord(LazyTypeDef::BmpRouteMonitoringMessage)),
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
            BuiltinTypeValue::ConstU8EnumVariant(c_enum) => {
                TypeDef::ConstEnumVariant(c_enum.enum_name.clone())
            }
            BuiltinTypeValue::ConstU16EnumVariant(c_enum) => {
                TypeDef::ConstEnumVariant(c_enum.enum_name.clone())
            }
            BuiltinTypeValue::ConstU32EnumVariant(c_enum) => {
                TypeDef::ConstEnumVariant(c_enum.enum_name.clone())
            }
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
            BuiltinTypeValue::Communities(_) => {
                TypeDef::List(Box::new(TypeDef::Community))
            }
            BuiltinTypeValue::Route(_) => TypeDef::Route,
            BuiltinTypeValue::BgpUpdateMessage(_) => {
                TypeDef::BgpUpdateMessage
            }
            BuiltinTypeValue::BmpRouteMonitoringMessage(_) => {
                TypeDef::LazyRecord(LazyTypeDef::BmpRouteMonitoringMessage)
            }
            BuiltinTypeValue::BmpPeerUpNotificationMessage(_) => {
                TypeDef::LazyRecord(LazyTypeDef::BmpPeerUpNotificationMessage)
            }
            BuiltinTypeValue::RouteStatus(_) => TypeDef::RouteStatus,
            BuiltinTypeValue::HexLiteral(_) => TypeDef::HexLiteral,
            BuiltinTypeValue::LocalPref(_) => TypeDef::LocalPref,
            BuiltinTypeValue::AtomicAggregator(_) => {
                TypeDef::AtomicAggregator
            }
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
            BuiltinTypeValue::ConstU8EnumVariant(c_enum) => {
                TypeDef::ConstEnumVariant(c_enum.enum_name)
            }
            BuiltinTypeValue::ConstU16EnumVariant(c_enum) => {
                TypeDef::ConstEnumVariant(c_enum.enum_name)
            }
            BuiltinTypeValue::ConstU32EnumVariant(c_enum) => {
                TypeDef::ConstEnumVariant(c_enum.enum_name)
            }
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
            BuiltinTypeValue::Communities(_) => {
                TypeDef::List(Box::new(TypeDef::Community))
            }
            BuiltinTypeValue::OriginType(_) => TypeDef::OriginType,
            BuiltinTypeValue::Route(_) => TypeDef::Route,
            BuiltinTypeValue::BgpUpdateMessage(_) => {
                TypeDef::BgpUpdateMessage
            }
            BuiltinTypeValue::BmpRouteMonitoringMessage(_) => {
                TypeDef::LazyRecord(LazyTypeDef::BmpRouteMonitoringMessage)
            }
            BuiltinTypeValue::BmpPeerUpNotificationMessage(_) => {
                TypeDef::LazyRecord(LazyTypeDef::BmpPeerUpNotificationMessage)
            }
            BuiltinTypeValue::RouteStatus(_) => TypeDef::RouteStatus,
            BuiltinTypeValue::HexLiteral(_) => TypeDef::HexLiteral,
            BuiltinTypeValue::LocalPref(_) => TypeDef::LocalPref,
            BuiltinTypeValue::AtomicAggregator(_) => {
                TypeDef::AtomicAggregator
            }
            BuiltinTypeValue::NextHop(_) => TypeDef::NextHop,
            BuiltinTypeValue::MultiExitDisc(_) => TypeDef::MultiExitDisc,
            // BuiltinTypeValue::BmpMessage(_) => todo!(),
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
            TypeValue::Enum(e) => e.get_type(),
            // TypeValue::Rib(r) => r.ty.clone(),
            // TypeValue::Table(t) => t.ty.clone(),
            TypeValue::OutputStreamMessage(m) => m.record_type.clone(),
            TypeValue::SharedValue(sv) => TypeDef::from(sv.as_ref()),
            TypeValue::Unknown => TypeDef::Unknown,
            TypeValue::UnInit => TypeDef::Unknown,
        }
    }
}
