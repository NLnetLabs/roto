use log::{trace, error, debug};
use serde::ser::SerializeMap;
use serde::{Serialize, Serializer};

use crate::ast::{
    AnonymousRecordValueExpr, Identifier, ListValueExpr, ShortString,
    TypedRecordValueExpr, ValueExpr,
};
use crate::compiler::compile::CompileError;
use crate::traits::{RotoType, Token};
use crate::vm::{StackValue, VmError, FieldIndex};
use std::fmt::{Display, Formatter};
use std::marker::PhantomData;

use super::builtin::{BuiltinTypeValue, U32};
use super::lazyrecord_types::LazyRecordTypeDef;
use super::typedef::{LazyNamedTypeDef, MethodProps, TypeDef};
use super::typevalue::TypeValue;

//============ Collections ==================================================

// Roughly the collection types fall into two categories: Materialized
// collections and their element types, and lazy evaluated collection types
// and their element types.

// The materialized collection types are Record and List. The element type for
// both is called ElementTypeValue. The latter has the ability to store nested
// TypeValues, so the roto user can create/modify things like Lists of lists,
// or Record with List-typed fields. The collection types themselves are
// straight-forward vectors of ElementTypeValues (with a ShortString added for
// the Record type). The List type is ordered (order of insert), the Record
// type MUST be ordered alphabetically by key. The Roto user should not have
// to worry about this, though.

// The lazy types are BytesRecord and LazyRecord. BytesRecord is the type that
// wraps the more complex routecore types, mainly the different BMP message
// types. They are a wrapper around this message. A LazyRecord is a special
// type that provides the translation between the method calls on a routecore
// type and the corresponding fields that are offered to the roto user. So,
// from the perspective of the roto user a LazyRecord is just a regular
// Record, with (field_name, value) pairs. The LazyRecord type instances
// *cannot* be stored in a TypeValue enum, they are strictly to be used as
// intermediary types inside a Rotonda instance. If they need to be stored
// (e.g. in a RIB), they need to be materialized first. Materializing them
// only makes sense when the Roto user has modified them, otherwise it's
// advisable to store the related BytesRecord, which *do* have
// BuiltinTypeValue variants to store them in, e.g. BuiltinTypeValue::
// BmpMessage. Each BytesRecord type has a variant in the `LazyRecordTypeDef`
// enum, so this acts as a registry for them (see LazyRecord_types).

// There's also an `EnumBytesRecord` trait that should be implemented for
// BytesRecord types that contain an enum, e.g. BytesRecord<BmpMessage>. This
// allows the roto user to create a match pattern on them.

// Note that the roto user should not be have to worry about or ever be
// confronted with the lazy evaluated types.

//------------ ElementType --------------------------------------------------

// This enum is used to differentiate between recursive collections and
// simple collections (that only contain primitive types). The latter do not
// need to be boxed, while the former do.

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize)]
#[serde(untagged)]
pub enum ElementTypeValue {
    Primitive(TypeValue),
    Nested(Box<TypeValue>),
}

impl ElementTypeValue {
    fn into_record(self) -> Result<Record, VmError> {
        if let ElementTypeValue::Primitive(TypeValue::Record(rec)) = self {
            Ok(rec)
        } else {
            Err(VmError::InvalidConversion)
        }
    }

    fn as_record(&self) -> Result<&Record, VmError> {
        if let ElementTypeValue::Nested(rec) = self {
            if let TypeValue::Record(rec2) = &**rec {
                return Ok(rec2);
            }
        }

        Err(VmError::InvalidValueType)
    }

    fn as_mut_record(&mut self) -> Result<&mut Record, VmError> {
        if let ElementTypeValue::Nested(rec) = &mut *self {
            if let TypeValue::Record(rec2) = &mut **rec {
                return Ok(rec2);
            }
        }

        Err(VmError::InvalidValueType)
    }

    pub(crate) fn into_type(
        self,
        ty: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match self {
            ElementTypeValue::Primitive(tv) => tv.into_type(ty),
            ElementTypeValue::Nested(elm) => elm.into_type(ty),
        }
    }
}

impl TryFrom<TypeValue> for ElementTypeValue {
    type Error = VmError;

    fn try_from(t: TypeValue) -> Result<Self, VmError> {
        match t {
            TypeValue::Builtin(v) => Ok(ElementTypeValue::Primitive(v.into())),
            TypeValue::List(ty) => {
                Ok(ElementTypeValue::Nested(Box::new(TypeValue::List(ty))))
            }
            TypeValue::Record(kv_list) => {
                Ok(ElementTypeValue::Nested(Box::new(TypeValue::Record(kv_list))))
            }
            TypeValue::Unknown => {
                Ok(ElementTypeValue::Primitive(TypeValue::Unknown))
            }
            ty => {
                error!("Cannot find ElementTypeValue for TypeValue: {}", ty);
                Err(VmError::InvalidValueType)
            }
        }
    }
}

impl TryFrom<ElementTypeValue> for TypeValue {
    type Error = VmError;

    fn try_from(t: ElementTypeValue) -> Result<Self, VmError> {
        match t {
            ElementTypeValue::Primitive(v) => Ok(v),
            ElementTypeValue::Nested(ty) => match *ty {
                TypeValue::List(ty) => Ok(TypeValue::List(ty)),
                TypeValue::Record(kv_list) => Ok(TypeValue::Record(kv_list)),
                ty => {
                    error!("Cannot find TypeValue for ElemenTypeValue: {}", ty);
                    Err(VmError::InvalidValueType)
                }
            },
        }
    }
}

impl<'a> TryFrom<&'a ElementTypeValue> for &'a TypeValue {
    type Error = VmError;

    fn try_from(t: &'a ElementTypeValue) -> Result<Self, VmError> {
        match t {
            ElementTypeValue::Primitive(v) => Ok(v),
            ElementTypeValue::Nested(ty) => match ty.as_ref() {
                TypeValue::List(_li) => Ok(ty),
                TypeValue::Record(_kv_list) => Ok(ty),
                ty => {
                    error!("Cannot find &TypeValue for &ElemenTypeValue: {}", ty);
                    Err(VmError::InvalidValueType)
                }
            },
        }
    }
}

impl PartialEq<TypeValue> for ElementTypeValue {
    fn eq(&self, other: &TypeValue) -> bool {
        match self {
            ElementTypeValue::Primitive(v) => v == other,
            _ => false,
        }
    }
}

impl PartialEq<StackValue<'_>> for &'_ ElementTypeValue {
    fn eq(&self, other: &StackValue) -> bool {
        match other {
            StackValue::Ref(v) => match self {
                ElementTypeValue::Primitive(ov) => *v == ov,
                _ => false,
            },
            StackValue::Arc(v) => match self {
                ElementTypeValue::Primitive(ov) => **v == *ov,
                _ => false,
            },
            StackValue::Owned(v) => match self {
                ElementTypeValue::Primitive(ov) => v == ov,
                _ => false,
            },
        }
    }
}

impl Default for ElementTypeValue {
    fn default() -> Self {
        Self::Primitive(TypeValue::Unknown)
    }
}

// Conversion for Records. Records hold `ElementTypeValue`s, the literals
// provided by the user in a a Roto script need to be converted. This returns
// a Result because the conversion may fail in the eval() phase, where we have
// syntactically correct structures, but they overflow or have unknown values.
impl TryFrom<ValueExpr> for ElementTypeValue {
    type Error = CompileError;

    fn try_from(value: ValueExpr) -> Result<Self, Self::Error> {
        match value {
            ValueExpr::StringLiteral(s_lit) => {
                Ok(ElementTypeValue::Primitive(s_lit.into()))
            }
            ValueExpr::IntegerLiteral(i_lit) => {
                Ok(ElementTypeValue::Primitive(i_lit.into()))
            }
            ValueExpr::PrefixLiteral(pfx_lit) => {
                Ok(ElementTypeValue::Primitive((&pfx_lit).try_into()?))
            }
            ValueExpr::IpAddressLiteral(ip_lit) => {
                Ok(ElementTypeValue::Primitive((&ip_lit).try_into()?))
            }
            ValueExpr::PrefixLengthLiteral(pl_lit) => {
                Ok(ElementTypeValue::Primitive(pl_lit.into()))
            }
            ValueExpr::AsnLiteral(asn_lit) => {
                Ok(ElementTypeValue::Primitive(asn_lit.into()))
            }
            ValueExpr::StandardCommunityLiteral(s_comm_lit) => {
                Ok(ElementTypeValue::Primitive(s_comm_lit.try_into()?))
            }
            ValueExpr::ExtendedCommunityLiteral(e_comm_lit) => {
                Ok(ElementTypeValue::Primitive(e_comm_lit.try_into()?))
            }
            ValueExpr::LargeCommunityLiteral(l_comm_lit) => {
                Ok(ElementTypeValue::Primitive(l_comm_lit.try_into()?))
            }
            ValueExpr::HexLiteral(hex_lit) => {
                Ok(ElementTypeValue::Primitive(hex_lit.into()))
            }
            ValueExpr::BooleanLit(bool_lit) => {
                Ok(ElementTypeValue::Primitive(bool_lit.into()))
            }
            ValueExpr::PrefixMatchExpr(_) => todo!(),
            ValueExpr::ComputeExpr(_) => todo!(),
            ValueExpr::RootMethodCallExpr(_) => todo!(),
            ValueExpr::AnonymousRecordExpr(rec) => {
                Ok(ElementTypeValue::Nested(Box::new(rec.try_into()?)))
            }
            ValueExpr::TypedRecordExpr(rec) => {
                Ok(ElementTypeValue::Nested(Box::new(rec.try_into()?)))
            }
            ValueExpr::ListExpr(list) => {
                Ok(ElementTypeValue::Nested(Box::new(list.try_into()?)))
            }
        }
    }
}

impl std::fmt::Display for ElementTypeValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ElementTypeValue::Nested(n) => write!(f, "{}", n),
            ElementTypeValue::Primitive(p) => write!(f, "{}", p),
        }
    }
}

// These conversions are used when creating OutputStreamMessages as a poor
// men's serializer (for `name` and `topic` fields). Probably want to make
// a more structural solution.
impl From<&ElementTypeValue> for String {
    fn from(value: &ElementTypeValue) -> String {
        match value {
            ElementTypeValue::Nested(n) => format!("{}", n),
            ElementTypeValue::Primitive(p) => format!("{}", p),
        }
    }
}

impl From<&ElementTypeValue> for ShortString {
    fn from(value: &ElementTypeValue) -> ShortString {
        match value {
            ElementTypeValue::Nested(n) => {
                ShortString::from(format!("{}", n).as_str())
            }
            ElementTypeValue::Primitive(p) => {
                ShortString::from(format!("{}", p).as_str())
            }
        }
    }
}

//------------ List type ----------------------------------------------------

// A recursive, materialized list that can contain any TypeValue variant,
// including Records and Lists.

#[derive(Debug, Eq, Clone, Hash, PartialEq, Serialize)]
pub struct List(pub(crate) Vec<ElementTypeValue>);

impl List {
    pub fn new(elem_type: Vec<ElementTypeValue>) -> Self {
        List(elem_type)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub(crate) fn iter(&self) -> std::slice::Iter<ElementTypeValue> {
        self.0.iter()
    }

    pub fn exec_method(
        &self,
        _method_token: usize,
        _args: Vec<&TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce(TypeValue) -> TypeValue + '_>, CompileError>
    {
        todo!()
    }

    // Get a reference to a field on a List, indicated by the first index in
    // the field index, and then descent into that field, based on the
    // following indexes in the field_index. Returns None if the field index
    // is empty.
    pub fn get_field_by_index(
        &self,
        field_index: FieldIndex,
    ) -> Option<&ElementTypeValue> {
        let mut elm = if let Ok(fi) = field_index.first() {
            self.0.get(fi)
        } else {
            debug!("Cannot find field index {:?} in {:?}", field_index, self);
            return None;
        };

        for index in field_index.skip_first() {
            elm = elm
                .or(None)?
                .as_record()
                .ok()?
                .0
                .get(*index)
                .map(|f| &f.1)
        }

        elm
    }

    // Get the owned value of a field on a List, indicated by the first index
    // in the field index, and then descent into that field, based on the
    // following indexes in the field_index. Returns None if the field index
    // is empty.
    pub fn get_field_by_index_owned(
        &mut self,
        field_index: FieldIndex,
    ) -> Option<ElementTypeValue> {
        let mut elm = if let Ok(fi) = field_index.first() {
            self.0.get_mut(fi).map(std::mem::take)
        } else {
            debug!("Cannot find field index {:?} in {:?}", field_index, self);
            return None;
        };

        for index in field_index.skip_first() {
            elm = elm
                .or(None)?
                .into_record()
                .ok()?
                .0
                .get_mut(*index)
                .map(|f| std::mem::take(&mut f.1));
        }
        elm
    }

    pub fn set_field_for_index(
        &mut self,
        field_index: FieldIndex,
        value: TypeValue,
    ) -> Result<(), VmError> {
        let mut elm = self.0.get_mut(field_index.first()?);

        for index in field_index.skip_first() {
            elm = elm
                .ok_or(VmError::MemOutOfBounds)?
                .as_mut_record()?
                .0
                .get_mut(*index)
                .map(|f| &mut f.1)
        }

        let e_tv = elm.ok_or(VmError::MemOutOfBounds)?;
        let new_field = &mut ElementTypeValue::try_from(value)?;
        std::mem::swap(e_tv, new_field);
        Ok(())
    }

    pub fn prepend_value(&mut self, value: TypeValue) -> Result<(), VmError> {
        let exist_v = std::mem::take(&mut self.0);
        let mut new_v = Vec::with_capacity(exist_v.len() + 1);
        new_v.push(ElementTypeValue::try_from(value)?);
        new_v.extend(exist_v);
        self.0 = new_v;

        Ok(())
    }

    pub fn append_value(&mut self, value: TypeValue) -> Result<(), VmError> {
        self.0.push(ElementTypeValue::try_from(value)?);
        Ok(())
    }

    pub fn insert_value(
        &mut self,
        index: usize,
        value: TypeValue,
    ) -> Result<(), VmError> {
        let mut new_v = Vec::with_capacity(self.0.len() + 1);

        new_v.extend_from_slice(&self.0[..index]);
        new_v.push(ElementTypeValue::try_from(value)?);
        new_v.extend_from_slice(&self.0[index..]);
        self.0 = new_v;

        Ok(())
    }

    pub fn prepend_vec(
        &mut self,
        values: Vec<TypeValue>,
    ) -> Result<(), VmError> {
        let exist_v = std::mem::take(&mut self.0);
        let mut new_v = Vec::with_capacity(exist_v.len() + values.len());

        for v in values {
            new_v.push(ElementTypeValue::try_from(v)?);
        }

        new_v.extend(exist_v);
        self.0 = new_v;

        Ok(())
    }

    pub fn append_vec(
        &mut self,
        values: Vec<TypeValue>,
    ) -> Result<(), VmError> {
        for v in values {
            self.0.push(ElementTypeValue::try_from(v)?)
        }

        Ok(())
    }

    pub fn insert_vec(
        &mut self,
        index: usize,
        values: Vec<TypeValue>,
    ) -> Result<(), VmError> {
        let mut new_v = Vec::with_capacity(self.0.len() + values.len());

        new_v.extend_from_slice(&self.0[..index]);

        for v in values {
            new_v.push(ElementTypeValue::try_from(v)?)
        }
        new_v.extend_from_slice(&self.0[index..]);
        self.0 = new_v;

        Ok(())
    }
}

impl<'a> From<&'a TypeDef> for List {
    fn from(_t: &'a TypeDef) -> Self {
        List::new(vec![ElementTypeValue::Primitive(TypeValue::Unknown)])
    }
}

impl std::fmt::Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[")?;
        for (i, elem) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            match elem {
                ElementTypeValue::Primitive(v) => write!(f, "{}", v)?,
                ElementTypeValue::Nested(v) => write!(f, "{}", v)?,
            }
        }
        write!(f, "]")
    }
}

impl RotoType for List {
    fn get_props_for_method(
        ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        if let TypeDef::List(ref list_ty_def) = ty {
            match method_name.ident.as_str() {
                "get" => Ok(MethodProps::new(
                    ty.clone(),
                    ListToken::Get.into(),
                    vec![TypeDef::U32],
                )),
                "remove" => Ok(MethodProps::new(
                    ty.clone(),
                    ListToken::Remove.into(),
                    vec![TypeDef::U32],
                )
                .consume_value()),
                "push" => Ok(MethodProps::new(
                    TypeDef::U32,
                    ListToken::Push.into(),
                    vec![*list_ty_def.clone()],
                )
                .consume_value()),
                "contains" => Ok(MethodProps::new(
                    TypeDef::Boolean,
                    ListToken::Contains.into(),
                    vec![*list_ty_def.clone()],
                )),
                _ => Err(format!(
                    "Unknown method '{}' for list",
                    method_name.ident
                )
                .into()),
            }
        } else {
            Err(CompileError::new("Invalid list item type.".into()))
        }
    }

    fn into_type(
        self,
        _type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        Err("List type cannot be converted into another type".into())
    }

    fn exec_value_method<'a>(
        &'a self,
        method: usize,
        args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method.try_into()? {
            ListToken::Len => Ok(TypeValue::Builtin(BuiltinTypeValue::U32(
                U32(self.0.len() as u32),
            ))),
            ListToken::Contains if args.len() == 1 => {
                trace!("contains on collection, search: {:?}", args);
                Ok(self.iter().any(|e| e == args[0]).into())
            }
            _ => Err(VmError::InvalidMethodCall),
        }
    }

    fn exec_consume_value_method(
        mut self,
        method: usize,
        mut args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method.try_into()? {
            ListToken::Get if args.len() == 1 => {
                Ok(match self.0.into_iter().find(|e| *e == args[0]) {
                    Some(e) => e.try_into()?,
                    None => TypeValue::Unknown,
                })
            }
            ListToken::Push => {
                self.0.push((args.remove(0)).try_into()?);
                Ok(TypeValue::List(self))
            }
            ListToken::Pop => todo!(),
            ListToken::Remove => todo!(),
            ListToken::Insert => todo!(),
            ListToken::Clear => todo!(),
            _m => Err(VmError::InvalidMethodCall),
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

impl TryFrom<ListValueExpr> for List {
    type Error = CompileError;

    fn try_from(value: ListValueExpr) -> Result<Self, Self::Error> {
        let mut lvs = vec![];
        for v in value.values.iter() {
            match v.clone().try_into() {
                Ok(v) => { lvs.push(v) },
                Err(e) => { return Err(e); }
            };
        }

        Ok(List(lvs))
    }
}

impl From<List> for TypeValue {
    fn from(value: List) -> Self {
        TypeValue::List(value)
    }
}

impl From<Vec<TypeValue>> for List {
    fn from(value: Vec<TypeValue>) -> Self {
        List::new(
            value
                .iter()
                .map(|v| ElementTypeValue::Primitive((*v).clone()))
                .collect::<Vec<_>>(),
        )
    }
}

#[derive(Debug)]
#[repr(u8)]
pub enum ListToken {
    Len,
    Contains,
    Get,
    Push,
    Pop,
    Remove,
    Insert,
    Clear,
}

impl TryFrom<usize> for ListToken {
    type Error = VmError;

    fn try_from(i: usize) -> Result<Self, VmError> {
        match i {
            0 => Ok(ListToken::Len),
            1 => Ok(ListToken::Contains),
            2 => Ok(ListToken::Get),
            3 => Ok(ListToken::Push),
            4 => Ok(ListToken::Pop),
            5 => Ok(ListToken::Remove),
            6 => Ok(ListToken::Insert),
            7 => Ok(ListToken::Clear),
            t => {
                error!("Cannot find method on List for token: {}", t);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<ListToken> for usize {
    fn from(t: ListToken) -> Self {
        t as usize
    }
}

//---------------- Record type ----------------------------------------------

// A recursive, materialized Record type that can contain any TypeValue
// variant, including Lists and Records.

#[derive(Debug, PartialEq, Eq, Default, Clone, Hash)]
pub struct Record(Vec<(ShortString, ElementTypeValue)>);

impl<'a> Record {
    pub(crate) fn new(
        mut elems: Vec<(ShortString, ElementTypeValue)>,
    ) -> Self {
        elems.sort_by_key(|v| v.0.clone());
        Self(elems)
    }

    // Sorted inserts on record creation time are essential for the PartialEq
    // impl (equivalence testing) of a Record TypeDef. The TypeDefs of two
    // Records are compared field-by-field in a simple zipped loop. Therefore
    // all the fields need to be sorted in the same way (aligned), otherwise
    // they'll be unequal, even if all the values are the same. This method
    // assumes the TypeValues are ordered, and DOES NOT CHECK THE SORTING, but
    // it does check whether the TypeValue of the record-to-be-created matches
    // its type definition.
    pub fn create_instance_with_ordered_fields(
        ty: &TypeDef,
        kvs: Vec<(&str, TypeValue)>,
    ) -> Result<Record, CompileError> {
        if kvs.is_empty() {
            return Err(CompileError::new(
                "Can't create empty instance.".into(),
            ));
        }

        let shortstring_vec = kvs
            .iter()
            .map(|(name, ty)| (ShortString::from(*name), ty))
            .collect::<Vec<_>>();

        if let TypeDef::Record(_rec) = ty {
            if ty._check_record_fields(shortstring_vec.as_slice()) {
                TypeValue::create_record(kvs)
            } else {
                Err(CompileError::new(format!(
                    "Record fields do not match record type, \
                    expected instance of type {}, but got {:?}",
                    ty, shortstring_vec
                )))
            }
        } else {
            Err(CompileError::new("Not a record type".into()))
        }
    }

    // This method actually sorts first. It may save your ass, or it may
    // waste your time.
    pub fn create_instance_with_sort(
        ty: &TypeDef,
        mut kvs: Vec<(&str, TypeValue)>,
    ) -> Result<Record, CompileError> {
        if kvs.is_empty() {
            return Err(CompileError::new(
                "Can't create empty instance.".into(),
            ));
        }
        kvs.sort_by(|a, b| a.0.cmp(b.0));

        let shortstring_vec = kvs
            .iter()
            .map(|(name, ty)| (ShortString::from(*name), ty))
            .collect::<Vec<_>>();

        if let TypeDef::Record(_rec) = ty {
            if ty._check_record_fields(shortstring_vec.as_slice()) {
                TypeValue::create_record(kvs)
            } else {
                Err(CompileError::new(format!(
                    "Record fields do not match record type, expected \
                    instance of type {}, but got {:?}",
                    ty, shortstring_vec
                )))
            }
        } else {
            Err(CompileError::new("Not a record type".into()))
        }
    }

    pub fn _recurse_create(
        ty: &TypeDef,
        // the sequential TypeValue vec
        mut values: Vec<TypeValue>,
    ) -> Result<Vec<(ShortString, ElementTypeValue)>, VmError> {
        if values.is_empty() {
            return Err(VmError::InvalidRecord);
        }

        trace!("ordered values {:?}", values);
        let mut kvs: Vec<(ShortString, ElementTypeValue)> = vec![];
        if let TypeDef::Record(rec) = ty {
            for (field_name, field_type) in rec.iter() {
                if let TypeDef::Record(ref rec_type) = &**field_type {
                    let num_values = rec_type.level_0_len();
                    let part_values = values.split_off(values.len() - num_values);
                    kvs.extend(Self::_recurse_create(field_type, part_values)?);
                } else if let Some(value) = values.pop() {
                    kvs.push((field_name.clone(), ElementTypeValue::Primitive(value)));
                } else {
                    return Err(VmError::InvalidRecord);
                }
            }
        } else {
            trace!("TypeDef for ordered fields {:?}", ty);
            return Err(VmError::InvalidRecord);
        }
        trace!("KVS {:#?}", kvs);

        Ok(kvs)
    }

    // This function requires quite the trust from our VM and the user, it
    // takes a Vec of TypeValues under the assumption that they are exactly
    // ordered the way the resulting Record is, so that the caller can omit
    // the field NAMES, only supplying the values. If you have the field
    // names available you should probably use the
    // `create_instance_with_ordered_fields` method, which does check whether
    // field names and type match.
    pub fn create_instance_from_ordered_fields(
        ty: &TypeDef,
        mut values: Vec<TypeValue>,
    ) -> Result<Record, VmError> {
        if values.is_empty() {
            return Err(VmError::InvalidRecord);
        }

        trace!("ordered values {:?}", values);
        let mut kvs = vec![];
        if let TypeDef::Record(rec) = ty {
            for (field_name, field_type) in rec.iter() {
                if let TypeDef::Record(ref rec_type) = &**field_type {
                    let num_values = rec_type.level_0_len();
                    let value = values.split_off(values.len() - num_values);
                    let a = Record::new(Self::_recurse_create(field_type, value)?);
                    kvs.push((field_name.clone(), ElementTypeValue::Nested(Box::new(a.into()))));
                } else if let Some(value) = values.pop() {
                    kvs.push((field_name.clone(), ElementTypeValue::Primitive(value)));
                } else {
                    return Err(VmError::InvalidRecord);
                }
            }
        } else {
            trace!("TypeDef for ordered fields {:?}", ty);
            return Err(VmError::InvalidRecord);
        }
        trace!("KVS {:#?}", kvs);

        Ok(Self(kvs))
    }

    pub fn get_values(&self) -> &[(ShortString, ElementTypeValue)] {
        &self.0[..]
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn iter(
        &self,
    ) -> impl Iterator<Item = &(ShortString, ElementTypeValue)> + '_ {
        self.0.iter()
    }

    pub fn get_value_for_field(
        &self,
        field: &'a str,
    ) -> Option<&ElementTypeValue> {
        self.0.iter().find(|(f, _)| f == &field).map(|(_, v)| v)
    }

    pub fn pop_value_for_field(
        &mut self,
        field: &'a str,
    ) -> Option<ElementTypeValue> {
        self.0
            .iter()
            .position(|(f, _)| f == &field)
            .map(|i| self.0.remove(i).1)
    }

    // Get a reference to a field on a List, indicated by the first index in
    // the field index, and then descent into that field, based on the
    // following indexes in the field_index. Returns None if the field index
    // is empty.
    pub fn get_field_by_index(
        &'a self,
        field_index: &FieldIndex,
    ) -> Option<&'a ElementTypeValue> {
        let mut elm = if let Ok(fi) = field_index.first() {
            self.0.get(fi).map(|f| &f.1)
        } else {
            return None;
        };

        for index in field_index.skip_first() {
            elm = elm?.as_record().ok()?.0.get(*index).map(|f| &f.1)
        }
        elm
    }

    // Get a reference to a field on a List, indicated by the first index in
    // the field index, and then descent into that field, based on the
    // following indexes in the field_index. Returns None if the field index
    // is empty.
    pub fn get_field_by_index_owned(
        &mut self,
        field_index: FieldIndex,
    ) -> Option<ElementTypeValue> {
        let mut elm = if let Ok(fi) = field_index.first() {
            self.0.get_mut(fi).map(|f| std::mem::take(&mut f.1))
        } else {
            return None;
        };

        for index in field_index.skip_first() {
            elm = elm
                .or(None)?
                .into_record()
                .ok()?
                .0
                .get_mut(*index)
                .map(|f| std::mem::take(&mut f.1))
        }

        elm
    }

    pub fn get_field_by_single_index(
        &self,
        index: usize,
    ) -> Option<&(ShortString, ElementTypeValue)> {
        self.0.get(index)
    }

    pub fn set_value_on_field_index(
        &mut self,
        field_index: FieldIndex,
        value: TypeValue,
    ) -> Result<(), VmError> {
        let mut elm = self.0.get_mut(field_index.first()?);

        for index in field_index.skip_first() {
            elm = elm
                .ok_or(VmError::MemOutOfBounds)?
                .1
                .as_mut_record()
                .or(Err(VmError::InvalidValueType))?
                .0
                .get_mut(*index)
        }

        let e_tv = elm.ok_or(VmError::MemOutOfBounds)?;
        let new_field = &mut (e_tv.0.clone(), ElementTypeValue::try_from(value)?);
        std::mem::swap(e_tv, new_field);

        Ok(())
    }
}

impl Display for Record {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (i, (field, elem)) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "\n\t{}: ", field)?;
            match elem {
                ElementTypeValue::Primitive(v) => write!(f, "{}", v)?,
                ElementTypeValue::Nested(v) => write!(f, "{}", v)?,
            }
        }
        write!(f, "\n   }}")
    }
}

impl RotoType for Record {
    fn get_props_for_method(
        ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "get" => Ok(MethodProps::new(
                ty,
                RecordToken::Get.into(),
                vec![TypeDef::U32],
            )),
            "get_all" => {
                Ok(MethodProps::new(ty, RecordToken::GetAll.into(), vec![]))
            }
            "contains" => Ok(MethodProps::new(
                TypeDef::Boolean,
                RecordToken::Contains.into(),
                vec![ty],
            )),
            _ => Err(format!(
                "Unknown method '{}' for Record type with fields {:?}",
                method_name.ident, ty
            )
            .into()),
        }
    }

    fn into_type(
        self,
        into_type: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        // Converting from a Record into a Record is not as straight-forward
        // as it is for primitive types, since we have to check whether the
        // fields in both completely match.
        trace!("CONVERT RECORD TYPE INSTANCE");
        match into_type {
            TypeDef::Record(_) => {
                if into_type == &TypeValue::Record(self.clone()) {
                    Ok(TypeValue::Record(self))
                } else {
                    Err(CompileError::from(format!(
                        "Record instance {} can't be converted into \
                            record type {}",
                        TypeDef::from(&TypeValue::from(self)),
                        into_type
                    )))
                }
            }
            _ => Err("Instance of type Record cannot be converted into \
            another type"
                .to_string()
                .into()),
        }
    }

    fn exec_value_method(
        &self,
        _method: usize,

        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

impl Serialize for Record {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(self.len()))?;
        for (key, value) in self.iter() {
            map.serialize_entry(key.as_str(), value)?;
        }
        map.end()
    }
}

// Value Expressions that contain a Record parsed as a pair of
// (field_name, value) pairs. This turns it into an actual Record.
impl TryFrom<AnonymousRecordValueExpr> for Record {
    type Error = CompileError;

    fn try_from(value: AnonymousRecordValueExpr) -> Result<Self, Self::Error> {
        trace!("FROM ANONYMOUS RECORD VALUE EXPR");

        let mut kvs: Vec<(ShortString, ElementTypeValue)> = vec![];
        for (s, t) in value.key_values.iter() {
            match ElementTypeValue::try_from(t.clone()) {
                Ok(t) => { kvs.push((s.ident.clone(),t)) },
                Err(e) => { return Err(e); }
            };
        }
        kvs.sort_by(|a, b| a.0.cmp(&b.0));
        Ok(Record(kvs))
    }
}

impl TryFrom<TypedRecordValueExpr> for Record {
    type Error = CompileError;
    
    fn try_from(value: TypedRecordValueExpr) -> Result<Self, Self::Error> {
        trace!("FROM TYPED RECORD VALUE EXPR");
        
        let mut kvs: Vec<(ShortString, ElementTypeValue)> = vec![];
        for (s, t) in value.key_values.iter() {
            match ElementTypeValue::try_from(t.clone()) {
                Ok(t) => { kvs.push((s.ident.clone(),t)) },
                Err(e) => { return Err(e); }
            };
        }

        Ok(Record(kvs))
    }
}

impl From<Record> for TypeValue {
    fn from(value: Record) -> Self {
        TypeValue::Record(value)
    }
}

impl From<Record> for Vec<(ShortString, TypeDef)> {
    fn from(value: Record) -> Self {
        value
            .0
            .iter()
            .map(|ty| (ty.0.clone(), (&ty.1).into()))
            .collect::<Vec<(_, TypeDef)>>()
    }
}

#[derive(Debug)]
#[repr(u8)]
pub enum RecordToken {
    Get = 0,
    GetAll = 1,
    Contains = 2,
    Set = 3,
}

impl TryFrom<usize> for RecordToken {
    type Error = VmError;

    fn try_from(i: usize) -> Result<Self, VmError> {
        match i {
            0 => Ok(RecordToken::Get),
            1 => Ok(RecordToken::GetAll),
            2 => Ok(RecordToken::Contains),
            t => {
                error!("Cannot find method on Record for token: {}", t);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<RecordToken> for usize {
    fn from(t: RecordToken) -> Self {
        t as usize
    }
}

//------------ EnumBytesRecord trait ----------------------------------------

// This trait is used for BytesRecord types that are enums themselves,
// currently that is only the BytesRecord<BmpMessage> type. This allows the
// roto user to create match patterns for these BytesRecord instances.

// Unlike a normal record, a bytes record need to have its recursive field
// resolved in one go, there can be no intermediary methods that return a
// (sub)-field value and then other methods can take that as argument for the
// next recursion IN THE VM, because that would mean having to clone the
// (sub-)field and probably the whole bytes message. This would defy the
// point of lazy evaluation. Therefore this method takes the bytes record AND
// the complete field index vec to go to do all the recursion in this method.
// The data-fields of the variants in this enum are handled as closely as
// possible to actual lazy fields. Note that we're still copying bytes out
// into the actual variant. TODO.

pub trait EnumBytesRecord {
    fn get_variant(&self) -> LazyRecordTypeDef;

    // Returns the TypeValue for a variant and field_index on this
    // bytes_record. Returns a TypeValue::Unknown if the requested
    // variant does not match the bytes record. Returns an error if
    // no field_index was specified.
    fn get_field_index_for_variant(
        &self,
        variant_token: LazyRecordTypeDef,
        field_index: &FieldIndex,
    ) -> Result<TypeValue, VmError>;

    fn is_variant(
        &self,
        variant_token: Token
    ) -> bool;
}

pub trait RecordType: AsRef<[u8]> {
    fn get_field_num() -> usize;
}

//------------ BytesRecord type ---------------------------------------------

// A wrapper around routecore types, used to store into a TypeValue. The
// actual mapping between routecore methods and Roto record field names and
// values does not happen here, but in the LazyRecord type.

#[derive(Debug, Serialize)]
pub struct BytesRecord<T: RecordType>(pub T);

impl<T: RecordType + std::fmt::Debug> BytesRecord<T> {
    pub(crate) fn bytes_parser(&self) -> &T {
        &self.0
    }

    pub(crate) fn get_props_for_method(
        _type_def: TypeDef,
        method_name: &Identifier,
    ) -> Result<MethodProps, CompileError> {
        Err(CompileError::from(format!(
            "Cannot find field/method/\
        variant {} in this BytesRecord type",
            method_name
        )))
    }
}

impl<T: RecordType> AsRef<[u8]> for BytesRecord<T> {
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref()
    }
}

impl<T: RecordType> Eq for BytesRecord<T> {}

impl<T: RecordType> PartialEq for BytesRecord<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ref() == other.0.as_ref()
    }
}

impl<T: RecordType> std::hash::Hash for BytesRecord<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.as_ref().hash(state);
    }
}


//------------- LazyElementTypeValue type -----------------------------------

// The containing element of a LazyRecord, besides being able to store
// recursive and simple collections (like its counterpart the materialized
// LazyElementTypeValue), it can also host unevaluated expressions (as
// closures) of field values, to be called by the VM at runtime.

#[allow(clippy::type_complexity)]
pub enum LazyElementTypeValue<'a, T: RecordType> {
    LazyRecord(LazyRecord<'a, T>),
    Lazy(Box<dyn Fn(&BytesRecord<T>) -> ElementTypeValue + 'a>),
    Materialized(ElementTypeValue),
}

impl<T: RecordType + std::fmt::Debug> LazyElementTypeValue<'_, T> {
    pub(crate) fn _into_materialized(
        self,
        raw_bytes: &BytesRecord<T>,
    ) -> Result<Self, VmError> {
        match self {
            LazyElementTypeValue::LazyRecord(rec) => {
                let rec = Record::try_from((&rec, raw_bytes))?;

                Ok(LazyElementTypeValue::Materialized(
                    ElementTypeValue::Primitive(rec.into()),
                ))
            }
            LazyElementTypeValue::Lazy(elm) => {
                Ok(LazyElementTypeValue::Materialized(elm(raw_bytes)))
            }
            LazyElementTypeValue::Materialized(_) => Ok(self),
        }
    }

    fn _materialize(&mut self, raw_bytes: &BytesRecord<T>) {
        if let LazyElementTypeValue::Lazy(elm) = self {
            *self = LazyElementTypeValue::Materialized(elm(raw_bytes));
        }
    }

    fn as_mut_record(&mut self) -> Result<&mut LazyRecord<T>, VmError> {
        todo!()
    }
}

impl<T: RecordType + std::fmt::Debug>
    TryFrom<(LazyElementTypeValue<'_, T>, &BytesRecord<T>)>
    for ElementTypeValue
{
    type Error = VmError;

    fn try_from(value: (LazyElementTypeValue<'_, T>, &BytesRecord<T>)) -> Result<Self, VmError> {
        match value {
            (LazyElementTypeValue::LazyRecord(rec), _) => {
                ElementTypeValue::try_from(TypeValue::Record(Record::try_from((
                    &rec, value.1,
                ))?))
            }
            (LazyElementTypeValue::Lazy(elm), raw_bytes) => Ok(elm(raw_bytes)),
            (LazyElementTypeValue::Materialized(elm), _) => Ok(elm),
        }
    }
}

impl<T: std::fmt::Debug + RecordType>
    TryFrom<(&LazyElementTypeValue<'_, T>, &BytesRecord<T>)>
    for ElementTypeValue
{
    type Error = VmError;

    fn try_from(
        (value, raw_bytes): (&LazyElementTypeValue<'_, T>, &BytesRecord<T>),
    ) -> Result<Self, VmError> {
        match (value, raw_bytes) {
            (LazyElementTypeValue::LazyRecord(rec), _) => {
                ElementTypeValue::try_from(TypeValue::Record(Record::try_from((
                    rec, raw_bytes,
                ))?))
            }
            (LazyElementTypeValue::Lazy(elm), raw_bytes) => Ok(elm(raw_bytes)),
            (LazyElementTypeValue::Materialized(elm), _) => Ok(elm.clone()),
        }
    }
}

impl<T: RecordType + std::fmt::Debug> std::fmt::Debug
    for LazyElementTypeValue<'_, T>
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LazyElementTypeValue::Materialized(mat_v) => {
                write!(f, "{:?}", mat_v)
            }
            LazyElementTypeValue::LazyRecord(rec) => {
                write!(f, "unresolved lazy record {:?}", rec)
            }
            LazyElementTypeValue::Lazy(_lazy_elm) => {
                write!(f, "unresolved lazy value")
            }
        }
    }
}

impl<T: RecordType> std::fmt::Display for LazyElementTypeValue<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let LazyElementTypeValue::Materialized(mat_v) = self {
            write!(f, "{}", mat_v)
        } else {
            write!(f, "unresolved lazy value")
        }
    }
}

impl<'a, T: RecordType + std::fmt::Debug> LazyElementTypeValue<'a, T> {
    pub(crate) fn _as_materialized(
        &'a self,
        raw_bytes: BytesRecord<T>,
    ) -> Result<ElementTypeValue, VmError> {
        match self {
            LazyElementTypeValue::LazyRecord(ref rec) => {
                let rec = Record::try_from((rec, &raw_bytes))?;
                Ok(ElementTypeValue::Primitive(rec.into()))
            }
            LazyElementTypeValue::Lazy(elm) => Ok(elm(&raw_bytes)),
            LazyElementTypeValue::Materialized(elm) => Ok(elm.clone()),
        }
    }
}

//------------ LazyRecord type ----------------------------------------------

// The LazyRecord type is the mapping between the routecore parser methods
// and the roto record (field_name, value) pairs that we communicate to a
// roto user. It does contains neither the parser, nor the original bytes of
// the message, instead each method of LazyRecord requires the caller (the
// roto VM) to pass in a reference to the BytesRecord. A value whether lazily
// evaluated or not, can be modified by the caller. The result of the
// modification should be materialized into a (regular) roto record though,
// the LazyRecord itself cannot be stored into a TypeValue variant.

#[derive(Debug)]
pub struct LazyRecord<'a, T: RecordType> {
    value: LazyNamedTypeDef<'a, T>,
    _is_materialized: bool,
    _raw_message: PhantomData<T>,
}

impl<'a, T: std::fmt::Debug + RecordType> LazyRecord<'a, T> {
    pub(crate) fn new(value: LazyNamedTypeDef<'a, T>) -> Self {
        LazyRecord {
            value,
            _is_materialized: false,
            _raw_message: PhantomData,
        }
    }

    pub(crate) fn from_type_def(
        ty: LazyNamedTypeDef<'a, T>,
    ) -> Result<LazyRecord<'a, T>, VmError> {
        Ok(Self {
            value: ty,
            _is_materialized: false,
            _raw_message: PhantomData,
        })
    }

    pub fn get_field_by_index(
        &self,
        field_index: &FieldIndex,
        raw_bytes: &BytesRecord<T>,
    ) -> Result<ElementTypeValue, VmError> {
        trace!(
            "get_field_by_index for {:?} w/ field index {:?}",
            self.value,
            field_index
        );
        let index = field_index.first().map_err(|_| VmError::InvalidMemoryAccess(0))?;
        self.value.get(index).and_then(|f| match &f.1 {
            LazyElementTypeValue::Lazy(l_value) => Some(l_value(raw_bytes)),
            LazyElementTypeValue::Materialized(m_value) => Some(m_value.clone()),
            LazyElementTypeValue::LazyRecord(rec) => rec
                .get_field_by_index(&field_index.skip_first().into(), raw_bytes).ok(),
        }).ok_or(VmError::InvalidFieldAccess)
    }

    pub fn get_field_by_index_as_owned(
        &self,
        field_index: &FieldIndex,
        raw_bytes: &BytesRecord<T>
    ) -> Result<StackValue, VmError> {
        let v = self
            .get_field_by_index(field_index, raw_bytes)?;

        Ok(StackValue::Owned(v.try_into()?))
    }

    pub fn exec_value_method(
        &self,
        _method: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
        _raw_bytes: impl AsRef<[u8]>,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    pub fn set_value_on_field_index(
        &'a mut self,
        field_index: FieldIndex,
        value: TypeValue,
    ) -> Result<(), VmError> {
        let mut elm: Option<&mut (ShortString, LazyElementTypeValue<'_, T>)> =
            self.value.get_mut(field_index.first()?);

        for index in field_index.skip_first() {
            elm = elm
                .ok_or(VmError::MemOutOfBounds)?
                .1
                .as_mut_record()?
                .value
                .get_mut(*index)
        }

        let e_tv = elm.ok_or(VmError::MemOutOfBounds)?;
        let new_field = &mut (
            e_tv.0.clone(),
            LazyElementTypeValue::Materialized(ElementTypeValue::try_from(value)?),
        );
        std::mem::swap(e_tv, new_field);

        Ok(())
    }
}

impl<T: RecordType> Display for LazyRecord<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (i, (field, elm)) in self.value.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "\n\t{}: ", field)?;
            write!(f, "{}", elm)?;
        }
        write!(f, "\n   }}")
    }
}

impl<T: std::fmt::Debug + RecordType>
    TryFrom<(&LazyRecord<'_, T>, &BytesRecord<T>)> for Record
{
    type Error = VmError;

    fn try_from(value: (&LazyRecord<T>, &BytesRecord<T>)) -> Result<Self, VmError> {
        let mut fields = vec![];
        for (field, elm) in &value.0.value {
            fields.push((field.clone(), ElementTypeValue::try_from((elm, value.1))?));
        }

        Ok(Record(fields))
    }
}

impl<T: std::fmt::Debug + RecordType>
    TryFrom<(LazyRecord<'_, T>, &BytesRecord<T>)> for TypeValue
{
    type Error = VmError;

    fn try_from(value: (LazyRecord<T>, &BytesRecord<T>)) -> Result<Self, VmError> {
        let mut rec = vec![];
        for field in value.0.value {
            rec.push((field.0, ElementTypeValue::try_from((field.1, value.1))?));
        }
        Ok(TypeValue::Record(Record::new(rec)))
    }
}

#[derive(Debug)]
#[repr(u8)]
pub enum BytesRecordToken {
    Get = 0,
    GetAll = 1,
    Contains = 2,
    Set = 3,
}

impl TryFrom<usize> for BytesRecordToken {
    type Error = VmError;

    fn try_from(i: usize) -> Result<Self, VmError> {
        match i {
            0 => Ok(BytesRecordToken::Get),
            1 => Ok(BytesRecordToken::GetAll),
            2 => Ok(BytesRecordToken::Contains),
            t => {
                error!("Cannot find method on BytesRecord for token: {}", t);
                Err(VmError::InvalidMethodCall)
            }
        }
    }
}

impl From<BytesRecordToken> for usize {
    fn from(t: BytesRecordToken) -> Self {
        t as usize
    }
}
