//------------ TypeDef -----------------------------------------------------

use std::hash::{Hash, Hasher};

// These are all the types the user can create. This enum is used to create
// `user defined` types.
use log::{trace, debug};
use routecore::bgp::aspath::HopPath;
use serde::Serialize;
use routecore::bgp::{types::AfiSafi, message::nlri::PathId};
use routecore::addr::Prefix;
use routecore::bgp::communities::HumanReadableCommunity as Community;

use crate::compiler::compile::CompileError;
use crate::traits::Token;
use crate::typedefconversion;
use crate::types::builtin::BgpUpdateMessage;
use crate::types::collections::ElementTypeValue;
use crate::vm::{StackValue, VmError, FieldIndex};
use crate::{
    ast::{AcceptReject, ShortString},
    traits::RotoType,
};

use super::builtin::{
    Asn, AtomicAggregate, Aggregator, Boolean, HexLiteral, Hop,
    IntegerLiteral, IpAddress, LocalPref, MultiExitDisc, OriginType,
    PrefixLength, RawRouteWithDeltas, RouteStatus, StringLiteral,
    Unknown,
};
use super::collections::{LazyElementTypeValue, Record};
use super::datasources::{RibType, Table};
use super::enum_types::{EnumVariant, GlobalEnumTypeDef};
use super::lazyrecord_types::LazyRecordTypeDef;
use super::outputs::OutputStreamMessage;
use super::{
    builtin::BuiltinTypeValue, collections::List, typevalue::TypeValue,
};

// the type definition of the type that's stored in the RIB and the
// vec of field_indexes that are used in the hash to calculate
// uniqueness for an entry.
pub type RibTypeDef = (Box<TypeDef>, Option<Vec<FieldIndex>>);
pub type NamedTypeDef = (ShortString, Box<TypeDef>);
pub type LazyNamedTypeDef<'a, T> =
    Vec<(ShortString, LazyElementTypeValue<'a, T>)>;

// This struct mainly serves the purpose of making sure that all inner
// Vec<NamedTypeDef> are being sorted at creation time, so that they
// are comparable (for equivalence) at all times without the need to
// ever sort them again.
#[derive(Clone, Debug, Eq, Default, Ord, PartialOrd, Serialize)]
pub struct RecordTypeDef(Vec<NamedTypeDef>);

impl RecordTypeDef {
    pub(crate) fn new(mut named_type_vec: Vec<NamedTypeDef>) -> Self {
        named_type_vec.sort();
        Self(named_type_vec)
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &NamedTypeDef> + '_ {
        self.0.iter()
    }

    pub(crate) fn level_0_len(&self) -> usize {
        self.0.len()
    }

    // returns the number of all fields in the flattened record type.
    pub(crate) fn recursive_len(&self) -> usize {
        let mut len = self.0.len();
        for field in self.0.iter() {
            if let TypeDef::Record(rec_type) = &*(field.1) {
                len += rec_type.0.len() - 1;
            }
        }
        trace!("total len() for record {}", len);
        len
    }

    pub(crate) fn get_index_for_field_name(&self, name: &ShortString) -> Option<usize> {
        self.0.iter().enumerate().find(|(_, td)| td.0 == name).map(|(i, _)| i)
    }
}

// Equivalence of two RecordTypeDefs is defined as the two vectors being
// completely the same, or the field names being the same. The types
// of the fields are being left out of the equivalence comparison
// here! That is the responsibility of the evaluator.
impl PartialEq for RecordTypeDef {
    fn eq(&self, other: &Self) -> bool {
        if self.0 == other.0 {
            return true;
        }

        self.0
            .iter()
            .zip(other.iter())
            .all(|(self_field, other_field)| {
                if self_field.0 != other_field.0 {
                    return false;
                }
                true
            })
    }
}

impl std::hash::Hash for RecordTypeDef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for ntd in &self.0 {
            ntd.hash(state);
        }
    }
}

impl From<Vec<NamedTypeDef>> for RecordTypeDef {
    fn from(mut value: Vec<NamedTypeDef>) -> Self {
        value.sort();
        Self(value)
    }
}

impl From<Vec<(&str, Box<TypeDef>)>> for RecordTypeDef {
    fn from(mut value: Vec<(&str, Box<TypeDef>)>) -> Self {
        value.sort();
        Self(
            value
                .into_iter()
                .map(|(s, td)| (s.into(), td))
                .collect::<Vec<_>>(),
        )
    }
}

impl std::fmt::Display for RecordTypeDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (name, ty) in &self.0 {
            write!(f, "{}: {}, ", name, ty)?;
        }
        write!(f, "}}")
    }
}

#[derive(
    Clone, Debug, Eq, PartialEq, Default, Ord, PartialOrd, Serialize, Hash
)]
pub enum TypeDef {
    // Data Sources, the data field in the enum represents the contained
    // type.
    Rib(RibTypeDef),
    Table(Box<TypeDef>),
    OutputStream(Box<TypeDef>),
    // Collection Types
    List(Box<TypeDef>),
    // Record with sorted named fields
    Record(RecordTypeDef),
    // Built-in Enums in the global namespace
    GlobalEnum(GlobalEnumTypeDef),
    // The data field holds the name of the enum this variant belongs to.
    ConstEnumVariant(ShortString),
    // The data field holds the name of the enum this variant belongs to.
    // ConstU16EnumVariant(ShortString),
    // ConstU32EnumVariant(ShortString),
    // A raw BGP message as bytes (a so called BytesRecord)
    BgpUpdateMessage,
    // A intermediate record where fields are only evaluated when called by
    // the VM, or when modified. This type doesn't have a corresponding
    // typevalue, it needs to be materialized into a record, or only be used
    // read-only (for filter operations).
    LazyRecord(LazyRecordTypeDef),
    // Builtin Types
    U32,
    U16,
    U8,
    Boolean,
    Prefix,
    PrefixLength, // A u8 prefixes by a /
    AfiSafi,
    PathId,
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
    AtomicAggregate,
    Aggregator,
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
    // The function defined by this macro called `typedefconversion` indicates
    // whether the self type can be converted into another specified type.
    // This function is used during Evaluation. This happens ONLY ON THE
    // NON-REFINED VERSION OF THE TYPES, meaning that Type refinements, e.g. a
    // PrefixLength can't be converted from an U32 that holds a value bigger
    // than 128, is NOT checked here. That refinement type check is done
    // during compilation, in the `into_type()` methods on the structs
    // representing the types in `primitives.rs`.

    // For all types that are expressed as variants with a data field, where
    // the data field holds a sub-type definition, the sub-types names are
    // also checked for equivalence. Field types are left alone, checking
    // equivalence or type conversion for those should done at the symbol
    // level.

    // The conversions indicated here is unidirectional, e.g. a line
    // `U8(U32,PrefixLength,IntegerLiteral;),` means that an U8 can converted
    // to U32, PrefixLength and IntegerLiteral, but not the other way around.
    typedefconversion!(
        // have conversions, no data field
        // SOURCE TYPE(TARGET TYPE WITHOUT DATA FIELD, ..;
        // TARGET TYPE WITH DATA FIELD)
        U8(StringLiteral,U16,U32,PrefixLength,Asn,IntegerLiteral;),
        U16(StringLiteral,U32,PrefixLength,Asn,IntegerLiteral,LocalPref;),
        U32(StringLiteral,Asn,IntegerLiteral;),
        Boolean(StringLiteral;),
        IpAddress(StringLiteral;),
        Prefix(StringLiteral;),
        Hop(StringLiteral;),
        Community(StringLiteral;),
        OriginType(StringLiteral;),
        NextHop(StringLiteral;),
        RouteStatus(StringLiteral;),
        IntegerLiteral(StringLiteral,U8,U32,StringLiteral,PrefixLength,LocalPref,Asn;ConstEnumVariant),
        StringLiteral(Asn;),
        HexLiteral(StringLiteral,U8,U32,Community;),
        PrefixLength(StringLiteral,U8,U32;),
        AfiSafi(StringLiteral;),
        PathId(StringLiteral,IntegerLiteral,U32;),
        Asn(StringLiteral,U32;),
        AsPath(StringLiteral;List),
        LocalPref(StringLiteral,U8,U16,U32,IntegerLiteral;),
        MultiExitDisc(StringLiteral,U8,IntegerLiteral;),
        AtomicAggregate(StringLiteral,Boolean;),
        Aggregator(StringLiteral,U8;);
        // have conversions, have data field
        // Records can be converted to other type of Records under certain
        // conditions:
        // - The fields match but are not sorted differently
        // - Field types do not match, but conversion is possible
        // - The fields set of the target record type is a superset of the
        //   source record type (TODO!)
        Record(StringLiteral;Record,OutputStream),
        LazyRecord(StringLiteral;Record,OutputStream),
        AcceptReject(StringLiteral;),
        List(StringLiteral;),
        ConstEnumVariant(StringLiteral,U32,Community;);
        // no conversions, no data field
        // SOURCE TYPE
        Route,
        BgpUpdateMessage,
        Unknown;
        // no conversions, have data field
        // SOURCE TYPE
        GlobalEnum,
        Rib,
        Table,
        OutputStream
    );

    pub(crate) fn new_record_type_from_short_string(
        mut type_ident_pairs: Vec<NamedTypeDef>,
    ) -> Result<TypeDef, CompileError> {
        type_ident_pairs.sort();
        Ok(TypeDef::Record(RecordTypeDef::new(type_ident_pairs)))
    }

    // Gets the type of a field of a Record Type, which can be a primitive,
    // but it can also be an anonymous record type.
    pub fn get_field(&self, field: &str) -> Option<TypeDef> {
        trace!("Self on get_field() {:?}", self);
        match self {
            TypeDef::Record(fields) => fields
                .iter()
                .find(|(ident, _)| ident == &field)
                .map(|td| *td.1.clone()),
            TypeDef::OutputStream(type_def) => {
                if let TypeDef::Record(fields) = &(**type_def) {
                    fields
                        .iter()
                        .find(|(ident, _)| ident == &field)
                        .map(|td| *td.1.clone())
                } else {
                    None
                }
            }
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
        Ok(TypeDef::Record(type_ident_pairs.into()))
    }

    // compute the number of fields in this typedef, mostly relevant for
    // records to figure how many values should be popped from the stack for
    // method calls.
    pub fn get_field_num(&self) -> usize {
        match self {
            TypeDef::Record(rec_type) => rec_type.recursive_len(),
            TypeDef::LazyRecord(l_rec) => l_rec.get_field_num(),
            TypeDef::List(list) => list.get_field_num(),
            TypeDef::Route => RawRouteWithDeltas::get_field_num(),
            TypeDef::OutputStream(rec) => rec.get_field_num(),
            _ => 1
        }
    }

    // this function checks that the `fields` vec describes the fields
    // present in self. If so it returns the positions in the vec of the
    // corresponding fields, to serve as the token for each field.
    pub(crate) fn has_fields_chain(
        &self,
        check_fields: &[crate::ast::Identifier],
    ) -> Result<(TypeDef, Token, Option<TypeValue>), CompileError> {
        // Data sources (rib and table) are special cases, because they have
        // their methods on the container (the datasource) and not on the
        // contained type. They don't have field access.
        trace!("has_fields_chain for {:?} with {:?}", self, check_fields);
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
        let mut existing_tv = None;

        for field in check_fields {
            let mut index = 0;

            match &parent_type {
                (TypeDef::Record(found_fields), _) => {
                    trace!("Record w/ field '{}'", field);

                    // Check if this field exists in the TypeDef of the
                    // Record.
                    if let Some((_, (_, ty))) =
                        found_fields.0.iter().enumerate().find(
                            |(i, (ident, _))| {
                                index = *i;
                                ident == &field.ident.as_str()
                            },
                        )
                    {
                        // Add up all the type defs in the data field
                        // of self.
                        parent_type = (*ty.clone(), parent_type.1);
                        parent_type.1.push(index as u8)?;
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
                (TypeDef::LazyRecord(lazy_type_def), _) => {
                    parent_type = lazy_type_def.get_props_for_field(field)?;
                    // Add the token to the FieldAccess vec, rewrite the 
                    // FieldAccess token into a LazyFieldAccess token so that
                    // the compiler can insert a LoadLazyValue command.
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
                // Enums can look up their fieldin the token (that is passed
                // in in the TypeDef)
                (TypeDef::GlobalEnum(enum_type), _) => {
                    let btv = enum_type
                        .get_value_for_variant(&field.ident)
                        .map_err(|_| {
                        CompileError::from(format!(
                            "Cannot find global enum '{}'",
                            enum_type
                        ))
                    })?;
                    result_type = //enum_type
                        // .get_value_for_variant(&field.ident)
                        // .map_err(|_| CompileError::from(format!("Cannot find global enum '{}'", enum_type)))
                        ((&btv).into(), Token::ConstEnumVariant);
                    existing_tv = Some(btv.into());
                }
                _ => {
                    trace!("can't find field '{}'", field.ident.as_str());
                    return Err(format!(
                        "No field named '{}'",
                        field.ident.as_str()
                    )
                    .into());
                }
            };
        }

        trace!("has_fields_chain {:?}", parent_type);

        Ok((result_type.0, result_type.1, existing_tv))
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
                        "Error in field instance '{:?}' of type {:?}",
                        name,
                        ty
                    );
                    trace!("record {:?}", rec);
                    return false;
                }
                field_count += 1;
            }

            if field_count != rec.level_0_len() {
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
            TypeDef::GlobalEnum(_) => {
                Err(CompileError::from("GlobalEnum type has no methods"))
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
            TypeDef::LazyRecord(lazy_type_def) => {
                lazy_type_def.get_props_for_method(self.clone(), method_name)
            }
            TypeDef::U32 => {
                u32::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::U16 => {
                u16::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::U8 => {
                u8::get_props_for_method(self.clone(), method_name)
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
            TypeDef::AfiSafi => {
                AfiSafi::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::PathId => {
                PathId::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::IpAddress => {
                IpAddress::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::Asn => {
                Asn::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::AsPath => {
                HopPath::get_props_for_method(self.clone(), method_name)
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
                routecore::bgp::types::NextHop::get_props_for_method(self.clone(), method_name)
            }
            TypeDef::AtomicAggregate => {
                AtomicAggregate::get_props_for_method(
                    self.clone(),
                    method_name,
                )
            }
            TypeDef::Aggregator => {
                Aggregator::get_props_for_method(
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
    ) -> Result<TypeValue, VmError> {
        match self {
            TypeDef::Record(_rec_type) => {
                Record::exec_type_method(method_token, args, return_type)
            }
            TypeDef::List(_list) => {
                List::exec_type_method(method_token, args, return_type)
            }
            TypeDef::AsPath => {
                HopPath::exec_type_method(method_token, args, return_type)
            }
            TypeDef::Prefix => {
                Prefix::exec_type_method(method_token, args, return_type)
            }
            TypeDef::U32 => {
                u32::exec_type_method(method_token, args, return_type)
            }
            TypeDef::StringLiteral => StringLiteral::exec_type_method(
                method_token,
                args,
                return_type,
            ),
            TypeDef::Asn => {
                Asn::exec_type_method(method_token, args, return_type)
            }
            TypeDef::IpAddress => {
                IpAddress::exec_type_method(method_token, args, return_type)
            }
            TypeDef::Route => RawRouteWithDeltas::exec_type_method(
                method_token,
                args,
                return_type,
            ),
            TypeDef::Rib(_rib) => {
                RibType::exec_type_method(method_token, args, return_type)
            }
            TypeDef::Table(_rec) => {
                Table::exec_type_method(method_token, args, return_type)
            }
            _ => Err(VmError::InvalidMethodCall)
        }
    }

    // Calculates the hash over the fields that are referenced in the unique
    // field indexes vec that lives on the Rib typedef.
    // If there's no field indexes vec then simply calculate the hash over
    // the (whole) TypeValue that was passed in.
    pub fn hash_key_values<'a, H: Hasher>(
        &'a self,
        state: &'a mut H,
        value: &'a TypeValue,
    ) -> Result<(), VmError> {
        if let TypeDef::Rib((_ty, Some(uniq_field_indexes))) = self {
            match value {
                TypeValue::Record(rec) => {
                    for field_index in uniq_field_indexes {
                        rec.get_field_by_index(field_index)
                            .hash(state);
                    }
                }
                TypeValue::Builtin(BuiltinTypeValue::Route(route)) => {
                    for field_index in uniq_field_indexes {
                        if let Ok(fi) = field_index.first() {
                            if let Ok(f) = route.get_field_by_index(fi) {
                                f.hash(state);
                            }
                        } else {
                            route.hash(state);
                        }
                    }
                }
                TypeValue::Builtin(btv) => {
                    btv.hash(state);
                }
                TypeValue::List(l) => {
                    l.hash(state);
                }
                // TypeValue::Enum(e) => {
                //     e.hash(state);
                // }
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

#[derive(Debug)]
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
            TypeDef::Record(rec_def) => {
                write!(f, "Record {}", rec_def)
            }
            TypeDef::List(list) => write!(f, "List of {}", list),
            TypeDef::GlobalEnum(c_enum) => write!(f, "Enum of {}", c_enum),
            TypeDef::ConstEnumVariant(c_enum) => {
                write!(f, "ConstU8EnumVariant('{}')", c_enum)
            }
            TypeDef::AsPath => write!(f, "AsPath"),
            TypeDef::Hop => write!(f, "Hop"),
            TypeDef::Prefix => write!(f, "Prefix"),
            TypeDef::AfiSafi => write!(f, "AFI SAFI"),
            TypeDef::PathId => write!(f, "Path ID"),
            TypeDef::U32 => write!(f, "U32"),
            TypeDef::U16 => write!(f, "U16"),
            TypeDef::Asn => write!(f, "Asn"),
            TypeDef::IpAddress => write!(f, "IpAddress"),
            TypeDef::Route => write!(f, "Route"),
            TypeDef::BgpUpdateMessage => write!(f, "BgpUpdateMessage"),
            TypeDef::LazyRecord(lazy_type_def) => {
                write!(f, "Lazy Record {}", lazy_type_def.type_def())
            }
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
            TypeDef::Unknown => write!(f, "Unknown"),
            TypeDef::LocalPref => write!(f, "Local Preference"),
            TypeDef::MultiExitDisc => write!(f, "Multi Exit Discriminator"),
            TypeDef::NextHop => write!(f, "Next Hop"),
            TypeDef::AtomicAggregate => write!(f, "Atomic Aggregate"),
            TypeDef::Aggregator => write!(f, "Aggregator"),
        }
    }
}

// This impl is used to see if a TypeValue (an instance of a Type) is equal
// to a TypeDef (a Roto type).
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
            TypeDef::ConstEnumVariant(name) => match other {
                BuiltinTypeValue::ConstU8EnumVariant(o_enum) => {
                    o_enum.enum_name == name
                }
                BuiltinTypeValue::ConstU16EnumVariant(o_enum) => {
                    o_enum.enum_name == name
                }
                BuiltinTypeValue::ConstU32EnumVariant(o_enum) => {
                    o_enum.enum_name == name
                }
                _ => false,
            },
            _ => false,
        }
    }
}

impl PartialEq<TypeValue> for TypeDef {
    fn eq(&self, other: &TypeValue) -> bool {
        trace!("compare typedef {:?} with typevalue {:?}", self, other);
        match (self, other) {
            // unknown *values* can be of any type!
            (_, TypeValue::Unknown) => {
                true
            }
            (a, TypeValue::Builtin(b)) => a == b,
            (a, TypeValue::List(b)) => match (a, b) {
                (TypeDef::List(aa), List(bb)) if !bb.is_empty() => match &bb.get(0) {
                    Some(ElementTypeValue::Nested(bb)) => {
                        trace!("element type value nested {}", bb);
                        return aa.as_ref() == bb.as_ref();
                    }
                    Some(ElementTypeValue::Primitive(bb)) => {
                        trace!("compare {} with primitive type value nested {}; result {}", aa, bb, aa.as_ref() == bb);
                        return aa.as_ref() == bb;
                    },
                    _ => {
                        debug!("Comparison involving empty list: '{}'", other);
                        false
                    }
                },
                _ => {
                    trace!("False: {:?} <-> {:?}", a, b);
                    false
                }
            },
            (TypeDef::Record(a_rec_type), TypeValue::Record(b_rec)) => {
                trace!("rec-rec compare {:?} <-> {:?}", a_rec_type, b_rec);
                let fields: Vec<(ShortString, TypeDef)> =
                    b_rec.clone().into();

                let mut field_count = 0;

                for (name, ty) in fields.as_slice() {
                    if !a_rec_type.iter().any(|(k, v)| {
                        // again, an Unknown TypeValue may be represent any
                        // TypeDef. The 2nd equal comparison here actually
                        // recurses to the first variant of this eq() method
                        k == name
                            && (v.as_ref() == ty || ty == &TypeValue::Unknown)
                    }) {
                        trace!(
                            "Error in field instance '{}' of type {}",
                            name,
                            ty
                        );
                        trace!("record {:?}", a_rec_type);
                        return false;
                    }
                    field_count += 1;
                }

                trace!("field count {} != rec len() {}", field_count, a_rec_type.level_0_len());

                if field_count != a_rec_type.level_0_len() {
                    trace!("Missing fields in record {:?}", self);
                    return false;
                }
                true
            }
            (TypeDef::OutputStream(a), b) => {
                trace!("compare output stream record to record");
                **a == *b
            }
            _ => false,
        }
    }
}

impl TryInto<RecordTypeDef> for Box<TypeDef> {
    type Error = CompileError;
    fn try_into(self) -> Result<RecordTypeDef, Self::Error> {
        if let TypeDef::Record(rec_def) = *self {
            Ok(rec_def)
        } else {
            Err(CompileError::from(format!(
                "Cannot convert type {} into a record type",
                self
            )))
        }
    }
}

impl From<RecordTypeDef> for Box<TypeDef> {
    fn from(value: RecordTypeDef) -> Self {
        TypeDef::Record(value).into()
    }
}

impl PartialEq<RecordTypeDef> for Box<TypeDef> {
    fn eq(&self, other: &RecordTypeDef) -> bool {
        if let TypeDef::Record(ref rec_def) = **self {
            rec_def == other
        } else {
            false
        }
    }
}

impl PartialEq<Box<TypeDef>> for RecordTypeDef {
    fn eq(&self, other: &Box<TypeDef>) -> bool {
        if let TypeDef::Record(ref rec_def) = **other {
            rec_def == self
        } else {
            false
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
            "BmpMessage" => {
                Ok(TypeDef::GlobalEnum(GlobalEnumTypeDef::BmpMessageType))
            }
            "BgpUpdateMessage" => Ok(TypeDef::BgpUpdateMessage),
            "BmpRouteMonitoringMessage" => {
                Ok(TypeDef::LazyRecord(LazyRecordTypeDef::RouteMonitoring))
            }
            "BmpPeerDownNotification" => Ok(TypeDef::LazyRecord(
                LazyRecordTypeDef::PeerDownNotification,
            )),
            "BmpPeerUpNotification" => {
                Ok(TypeDef::LazyRecord(LazyRecordTypeDef::PeerUpNotification))
            }
            "HexLiteral" => Ok(TypeDef::HexLiteral),
            "BmpMessageType" => {
                Ok(TypeDef::ConstEnumVariant("BMP_MESSAGE_TYPE".into()))
            }
            _ => Err(format!("Undefined type: {}", ty.ident).into()),
        }
    }
}

impl TryFrom<crate::ast::Identifier> for TypeDef {
    type Error = CompileError;
    fn try_from(ty: crate::ast::Identifier) -> Result<TypeDef, CompileError> {
        match ty.ident.as_str() {
            "U32" => Ok(TypeDef::U32),
            "U16" => Ok(TypeDef::U16),
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
            "BmpMessage" => {
                Ok(TypeDef::GlobalEnum(GlobalEnumTypeDef::BmpMessageType))
            }
            "BmpRouteMonitoringMessage" => {
                Ok(TypeDef::LazyRecord(LazyRecordTypeDef::RouteMonitoring))
            }
            "BmpPeerDownNotification" => Ok(TypeDef::LazyRecord(
                LazyRecordTypeDef::PeerDownNotification,
            )),
            "BmpPeerUpNotification" => {
                Ok(TypeDef::LazyRecord(LazyRecordTypeDef::PeerUpNotification))
            }
            "HexLiteral" => Ok(TypeDef::HexLiteral),
            "BmpMessageType" => {
                Ok(TypeDef::ConstEnumVariant("BMP_MESSAGE_TYPE".into()))
            }
            _ => Err(format!("Undefined type: {}", ty.ident).into()),
        }
    }
}

impl From<&BuiltinTypeValue> for TypeDef {
    fn from(ty: &BuiltinTypeValue) -> TypeDef {
        match ty {
            BuiltinTypeValue::U32(_) => TypeDef::U32,
            BuiltinTypeValue::U16(_) => TypeDef::U16,
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
            BuiltinTypeValue::AfiSafi(_) => TypeDef::AfiSafi,
            BuiltinTypeValue::PathId(_) => TypeDef::PathId,
            BuiltinTypeValue::PrefixLength(_) => TypeDef::PrefixLength,
            BuiltinTypeValue::IpAddress(_) => TypeDef::IpAddress,
            BuiltinTypeValue::Asn(_) => TypeDef::Asn,
            BuiltinTypeValue::Hop(_) => TypeDef::Hop,
            BuiltinTypeValue::OriginType(_) => TypeDef::OriginType,
            BuiltinTypeValue::AsPath(_) => TypeDef::AsPath,
            BuiltinTypeValue::Community(_) => TypeDef::Community,
            BuiltinTypeValue::Route(_) => TypeDef::Route,
            BuiltinTypeValue::BgpUpdateMessage(_) => {
                TypeDef::BgpUpdateMessage
            }
            BuiltinTypeValue::BmpMessage(_) => {
                TypeDef::GlobalEnum(GlobalEnumTypeDef::BmpMessageType)
            }
            BuiltinTypeValue::BmpRouteMonitoringMessage(_) => {
                TypeDef::LazyRecord(LazyRecordTypeDef::RouteMonitoring)
            }
            BuiltinTypeValue::BmpPeerUpNotification(_) => {
                TypeDef::LazyRecord(LazyRecordTypeDef::PeerUpNotification)
            }
            BuiltinTypeValue::BmpPeerDownNotification(_) => {
                TypeDef::LazyRecord(LazyRecordTypeDef::PeerDownNotification)
            }
            BuiltinTypeValue::BmpInitiationMessage(_) => {
                TypeDef::LazyRecord(LazyRecordTypeDef::InitiationMessage)
            }
            BuiltinTypeValue::BmpTerminationMessage(_) => {
                TypeDef::LazyRecord(LazyRecordTypeDef::TerminationMessage)
            }
            BuiltinTypeValue::BmpStatisticsReport(_) => {
                TypeDef::LazyRecord(LazyRecordTypeDef::StatisticsReport)
            }
            BuiltinTypeValue::RouteStatus(_) => TypeDef::RouteStatus,
            BuiltinTypeValue::HexLiteral(_) => TypeDef::HexLiteral,
            BuiltinTypeValue::LocalPref(_) => TypeDef::LocalPref,
            BuiltinTypeValue::AtomicAggregate(_) => TypeDef::AtomicAggregate,
            BuiltinTypeValue::Aggregator(_) => {
                TypeDef::Aggregator
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
            BuiltinTypeValue::U16(_) => TypeDef::U16,
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
            BuiltinTypeValue::AfiSafi(_) => TypeDef::AfiSafi,
            BuiltinTypeValue::PathId(_) => TypeDef::PathId,
            BuiltinTypeValue::PrefixLength(_) => TypeDef::PrefixLength,
            BuiltinTypeValue::IpAddress(_) => TypeDef::IpAddress,
            BuiltinTypeValue::Asn(_) => TypeDef::Asn,
            BuiltinTypeValue::Hop(_) => TypeDef::Hop,
            BuiltinTypeValue::AsPath(_) => TypeDef::AsPath,
            BuiltinTypeValue::Community(_) => TypeDef::Community,
            BuiltinTypeValue::OriginType(_) => TypeDef::OriginType,
            BuiltinTypeValue::Route(_) => TypeDef::Route,
            BuiltinTypeValue::BgpUpdateMessage(_) => {
                TypeDef::BgpUpdateMessage
            }
            BuiltinTypeValue::BmpMessage(_) => {
                TypeDef::GlobalEnum(GlobalEnumTypeDef::BmpMessageType)
            }
            BuiltinTypeValue::BmpRouteMonitoringMessage(_) => {
                TypeDef::LazyRecord(LazyRecordTypeDef::RouteMonitoring)
            }
            BuiltinTypeValue::BmpPeerUpNotification(_) => {
                TypeDef::LazyRecord(LazyRecordTypeDef::PeerUpNotification)
            }
            BuiltinTypeValue::BmpPeerDownNotification(_) => {
                TypeDef::LazyRecord(LazyRecordTypeDef::PeerDownNotification)
            }
            BuiltinTypeValue::BmpInitiationMessage(_) => {
                TypeDef::LazyRecord(LazyRecordTypeDef::InitiationMessage)
            }
            BuiltinTypeValue::BmpTerminationMessage(_) => {
                TypeDef::LazyRecord(LazyRecordTypeDef::TerminationMessage)
            }
            BuiltinTypeValue::BmpStatisticsReport(_) => {
                TypeDef::LazyRecord(LazyRecordTypeDef::StatisticsReport)
            }
            BuiltinTypeValue::RouteStatus(_) => TypeDef::RouteStatus,
            BuiltinTypeValue::HexLiteral(_) => TypeDef::HexLiteral,
            BuiltinTypeValue::LocalPref(_) => TypeDef::LocalPref,
            BuiltinTypeValue::AtomicAggregate(_) => TypeDef::AtomicAggregate,
            BuiltinTypeValue::Aggregator(_) => {
                TypeDef::Aggregator
            }
            BuiltinTypeValue::NextHop(_) => TypeDef::NextHop,
            BuiltinTypeValue::MultiExitDisc(_) => TypeDef::MultiExitDisc,
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
                List(l) => match &l.get(0) {
                    Some(ElementTypeValue::Nested(n)) => {
                        TypeDef::List(Box::new((&(**n)).into()))
                    }
                    Some(ElementTypeValue::Primitive(p)) => {
                        TypeDef::List(Box::new(p.into()))
                    },
                    _ => {
                        debug!("Empty list type encountered in TypeValue '{}'", ty);
                        TypeDef::List(Box::new(TypeDef::Unknown))
                    }
                },
            },
            TypeValue::Record(r) => TypeDef::Record(RecordTypeDef::new(
                r.iter()
                    .map(|(k, v)| (k.clone(), Box::new(v.into())))
                    .collect::<Vec<_>>(),
            )),
            // TypeValue::Enum(e) => e.get_type(),
            // TypeValue::Rib(r) => r.ty.clone(),
            // TypeValue::Table(t) => t.ty.clone(),
            TypeValue::OutputStreamMessage(m) => m.get_record().into(),
            TypeValue::SharedValue(sv) => TypeDef::from(sv.as_ref()),
            TypeValue::Unknown => TypeDef::Unknown,
            TypeValue::UnInit => TypeDef::Unknown,
        }
    }
}
