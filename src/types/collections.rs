use log::trace;
use serde::Serialize;
use smallvec::SmallVec;

use crate::ast::{
    AnonymousRecordValueExpr, Identifier, ListValueExpr, ShortString,
    TypedRecordValueExpr, ValueExpr,
};
use crate::compile::CompileError;
use crate::traits::RotoType;
use crate::vm::{StackValue, VmError};
use std::fmt::{Display, Formatter};
use std::marker::PhantomData;
use std::sync::Arc;

use super::builtin::{BuiltinTypeValue, U32};
use super::typedef::{LazyNamedTypeDef, MethodProps, TypeDef};
use super::typevalue::TypeValue;

//============ Collections ==================================================

//------------ ElementType -------------------------------------

// This enum is used to differentiate between recursive collections and simple
// collections (that only contain primitive types). The latter do not need to
// be boxed, while the former do.

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize)]
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

impl From<TypeValue> for ElementTypeValue {
    fn from(t: TypeValue) -> Self {
        match t {
            TypeValue::Builtin(v) => ElementTypeValue::Primitive(v.into()),
            TypeValue::List(ty) => {
                ElementTypeValue::Nested(Box::new(TypeValue::List(ty)))
            }
            TypeValue::Record(kv_list) => {
                ElementTypeValue::Nested(Box::new(TypeValue::Record(kv_list)))
            }
            ty => panic!("1. Unknown type {}", ty),
        }
    }
}

impl From<ElementTypeValue> for TypeValue {
    fn from(t: ElementTypeValue) -> Self {
        match t {
            ElementTypeValue::Primitive(v) => v,
            ElementTypeValue::Nested(ty) => match *ty {
                TypeValue::List(ty) => TypeValue::List(ty),
                TypeValue::Record(kv_list) => TypeValue::Record(kv_list),
                ty => panic!("2. Unknown type {}", ty),
            },
        }
    }
}

impl<'a> From<&'a ElementTypeValue> for &'a TypeValue {
    fn from(t: &'a ElementTypeValue) -> Self {
        match t {
            ElementTypeValue::Primitive(v) => v,
            ElementTypeValue::Nested(ty) => match ty.as_ref() {
                TypeValue::List(_li) => ty,
                TypeValue::Record(_kv_list) => ty,
                ty => panic!("3. Unknown type {}", ty),
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
// provided by the user in a a Roto script need to be converted.
impl From<ValueExpr> for ElementTypeValue {
    fn from(value: ValueExpr) -> Self {
        match value {
            ValueExpr::StringLiteral(s_lit) => {
                ElementTypeValue::Primitive(s_lit.into())
            }
            ValueExpr::IntegerLiteral(i_lit) => {
                ElementTypeValue::Primitive(i_lit.into())
            }
            ValueExpr::PrefixLengthLiteral(pl_lit) => {
                ElementTypeValue::Primitive(pl_lit.into())
            }
            ValueExpr::AsnLiteral(asn_lit) => {
                ElementTypeValue::Primitive(asn_lit.into())
            }
            ValueExpr::HexLiteral(hex_lit) => {
                ElementTypeValue::Primitive(hex_lit.into())
            }
            ValueExpr::BooleanLit(bool_lit) => {
                ElementTypeValue::Primitive(bool_lit.into())
            }
            ValueExpr::PrefixMatchExpr(_) => todo!(),
            ValueExpr::ComputeExpr(_) => todo!(),
            ValueExpr::BuiltinMethodCallExpr(_) => todo!(),
            ValueExpr::AnonymousRecordExpr(rec) => {
                ElementTypeValue::Nested(Box::new(rec.into()))
            }
            ValueExpr::TypedRecordExpr(rec) => {
                ElementTypeValue::Nested(Box::new(rec.into()))
            }
            ValueExpr::ListExpr(list) => {
                ElementTypeValue::Nested(Box::new(list.into()))
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

//------------ List type ----------------------------------------------------

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

    pub fn get_field_by_index(
        &self,
        field_index: SmallVec<[usize; 8]>,
    ) -> Option<&ElementTypeValue> {
        let mut elm = self.0.get(field_index[0]);

        for index in &field_index[1..] {
            elm = elm
                .or(None)?
                .as_record()
                .unwrap()
                .0
                .get(*index)
                .map(|f| &f.1)
        }

        elm
    }

    pub fn get_field_by_index_owned(
        &mut self,
        field_index: SmallVec<[usize; 8]>,
    ) -> Option<ElementTypeValue> {
        let mut elm = self.0.get_mut(field_index[0]).map(std::mem::take);

        for index in &field_index[1..] {
            elm = elm
                .or(None)?
                .into_record()
                .unwrap()
                .0
                .get_mut(*index)
                .map(|f| std::mem::take(&mut f.1));
        }
        elm
    }

    pub fn set_field_for_index(
        &mut self,
        field_index: SmallVec<[usize; 8]>,
        value: TypeValue,
    ) -> Result<(), VmError> {
        let mut elm = self.0.get_mut(field_index[0]);

        for index in &field_index[1..] {
            elm = elm
                .ok_or(VmError::MemOutOfBounds)?
                .as_mut_record()?
                .0
                .get_mut(*index)
                .map(|f| &mut f.1)
        }

        let e_tv = elm.ok_or(VmError::MemOutOfBounds)?;
        let new_field = &mut ElementTypeValue::from(value);
        std::mem::swap(e_tv, new_field);
        Ok(())
    }

    pub fn prepend_value(&mut self, value: TypeValue) -> Result<(), VmError> {
        let exist_v = std::mem::take(&mut self.0);
        let mut new_v = Vec::with_capacity(exist_v.len() + 1);
        new_v.push(ElementTypeValue::from(value));
        new_v.extend(exist_v);
        self.0 = new_v;

        Ok(())
    }

    pub fn append_value(&mut self, value: TypeValue) -> Result<(), VmError> {
        self.0.push(ElementTypeValue::from(value));
        Ok(())
    }

    pub fn insert_value(
        &mut self,
        index: usize,
        value: TypeValue,
    ) -> Result<(), VmError> {
        let mut new_v = Vec::with_capacity(self.0.len() + 1);

        new_v.extend_from_slice(&self.0[..index]);
        new_v.push(ElementTypeValue::from(value));
        new_v.extend_from_slice(&self.0[index..]);
        self.0 = new_v;

        Ok(())
    }

    pub fn prepend_vec(
        &mut self,
        value: Vec<TypeValue>,
    ) -> Result<(), VmError> {
        let exist_v = std::mem::take(&mut self.0);
        let mut new_v = Vec::with_capacity(exist_v.len() + value.len());
        new_v.extend(
            value
                .into_iter()
                .map(ElementTypeValue::from)
                .collect::<Vec<_>>(),
        );
        new_v.extend(exist_v);
        self.0 = new_v;

        Ok(())
    }

    pub fn append_vec(
        &mut self,
        value: Vec<TypeValue>,
    ) -> Result<(), VmError> {
        self.0.extend(
            value
                .into_iter()
                .map(ElementTypeValue::from)
                .collect::<Vec<_>>(),
        );

        Ok(())
    }

    pub fn insert_vec(
        &mut self,
        index: usize,
        value: Vec<TypeValue>,
    ) -> Result<(), VmError> {
        let mut new_v = Vec::with_capacity(self.0.len() + value.len());

        new_v.extend_from_slice(&self.0[..index]);
        new_v.extend(
            value
                .into_iter()
                .map(ElementTypeValue::from)
                .collect::<Vec<_>>(),
        );
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
                    vec![],
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
        match method.into() {
            ListToken::Len => Ok(TypeValue::Builtin(BuiltinTypeValue::U32(
                U32(self.0.len() as u32),
            ))),
            ListToken::Contains => {
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
        match method.into() {
            ListToken::Get => {
                Ok(match self.0.into_iter().find(|e| *e == args[0]) {
                    Some(e) => e.into(),
                    None => TypeValue::Unknown,
                })
            }
            ListToken::Push => {
                self.0.push((args.remove(0)).into());
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

impl From<ListValueExpr> for List {
    fn from(value: ListValueExpr) -> Self {
        List(
            value
                .values
                .iter()
                .map(|v| v.clone().into())
                .collect::<Vec<_>>(),
        )
    }
}

impl From<List> for TypeValue {
    fn from(value: List) -> Self {
        TypeValue::List(value)
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

impl From<usize> for ListToken {
    fn from(i: usize) -> Self {
        match i {
            0 => ListToken::Len,
            1 => ListToken::Contains,
            2 => ListToken::Get,
            3 => ListToken::Push,
            4 => ListToken::Pop,
            5 => ListToken::Remove,
            6 => ListToken::Insert,
            7 => ListToken::Clear,
            _ => panic!("Unknown ListToken"),
        }
    }
}

impl From<ListToken> for usize {
    fn from(t: ListToken) -> Self {
        t as usize
    }
}

//---------------- Record type ----------------------------------------------

#[derive(Debug, PartialEq, Eq, Default, Clone, Hash, Serialize)]
pub struct Record(pub(crate) Vec<(ShortString, ElementTypeValue)>);

impl<'a> Record {
    pub(crate) fn new(
        elems: Vec<(ShortString, ElementTypeValue)>,
    ) -> Result<Self, CompileError> {
        Ok(Self(elems))
    }

    pub fn create_instance(
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
                Err(CompileError::new(
                    format!("Record fields do not match record type, expected instance of type {}, but got {:?}", ty, shortstring_vec)
                ))
            }
        } else {
            Err(CompileError::new("Not a record type".into()))
        }
    }

    pub fn get_value_for_field(
        &self,
        field: &'a str,
    ) -> Option<&ElementTypeValue> {
        self.0.iter().find(|(f, _)| f == &field).map(|(_, v)| v)
    }

    pub fn get_field_by_index(
        &'a self,
        field_index: SmallVec<[usize; 8]>,
    ) -> Option<&'a ElementTypeValue> {
        let mut elm = self.0.get(field_index[0]).map(|f| &f.1);

        for index in &field_index[1..] {
            elm = elm?.as_record().unwrap().0.get(*index).map(|f| &f.1)
        }
        elm
    }

    pub fn get_field_by_index_owned(
        &mut self,
        field_index: SmallVec<[usize; 8]>,
    ) -> ElementTypeValue {
        let mut elm = self
            .0
            .get_mut(field_index[0])
            .map(|f| std::mem::take(&mut f.1));

        for index in &field_index[1..] {
            elm = elm
                .or(None)
                .unwrap()
                .into_record()
                .unwrap()
                .0
                .get_mut(*index)
                .map(|f| std::mem::take(&mut f.1))
        }

        elm.unwrap()
    }

    pub fn set_value_on_field_index(
        &mut self,
        field_index: SmallVec<[usize; 8]>,
        value: TypeValue,
    ) -> Result<(), VmError> {
        let mut elm = self.0.get_mut(field_index[0]);

        for index in &field_index[1..] {
            elm = elm
                .ok_or(VmError::MemOutOfBounds)?
                .1
                .as_mut_record()
                .unwrap()
                .0
                .get_mut(*index)
        }

        let e_tv = elm.ok_or(VmError::MemOutOfBounds)?;
        let new_field = &mut (e_tv.0.clone(), ElementTypeValue::from(value));
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
        match into_type {
            TypeDef::Record(_) => {
                if into_type == &TypeValue::Record(self.clone()) {
                    Ok(TypeValue::Record(self))
                } else {
                    match into_type {
                        TypeDef::OutputStream(rec) => Ok(TypeValue::OutputStreamMessage(
                            Arc::new(super::outputs::OutputStreamMessage {
                                name: "".into(),
                                topic: "".into(),
                                record_type: (**rec).clone(),
                                record: self.into(),
                            }),
                        )),
                        _ => Err("Record type cannot be converted into another type"
                        .to_string()
                        .into())
                    }
                }
            }
            _ => Err("Record type cannot be converted into another type"
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

// Value Expressions that containt a Record parsed as a pair of
// (field_name, value) pairs. This turns it into an actual Record.
impl From<AnonymousRecordValueExpr> for Record {
    fn from(value: AnonymousRecordValueExpr) -> Self {
        Record(
            value
                .key_values
                .iter()
                .map(|(s, t)| (s.ident.clone(), t.clone().into()))
                .collect::<Vec<_>>(),
        )
    }
}

impl From<TypedRecordValueExpr> for Record {
    fn from(value: TypedRecordValueExpr) -> Self {
        Record(
            value
                .key_values
                .iter()
                .map(|(s, t)| (s.ident.clone(), t.clone().into()))
                .collect::<Vec<_>>(),
        )
    }
}

impl From<Record> for TypeValue {
    fn from(value: Record) -> Self {
        TypeValue::Record(value)
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

impl From<usize> for RecordToken {
    fn from(i: usize) -> Self {
        match i {
            0 => RecordToken::Get,
            1 => RecordToken::GetAll,
            2 => RecordToken::Contains,
            _ => panic!("Unknown RecordToken"),
        }
    }
}

impl From<RecordToken> for usize {
    fn from(t: RecordToken) -> Self {
        t as usize
    }
}

//------------ bytesRecord --------------------------------------------------

#[derive(Debug, Serialize)]
pub struct BytesRecord<T>(pub(crate) T);

impl<T: AsRef<[u8]>> BytesRecord<T> {
    pub(crate) fn bytes_parser(&self) -> &T {
        &self.0
    }

    pub(crate) fn get_props_for_method(
        _ty: TypeDef,
        _method_name: &Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }
}

impl<T: AsRef<[u8]>> Eq for BytesRecord<T> {}

impl<T: AsRef<[u8]>> PartialEq for BytesRecord<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ref() == other.0.as_ref()
    }
}

impl<T: AsRef<[u8]>> std::hash::Hash for BytesRecord<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.as_ref().hash(state);
    }
}

//------------- LazyElementTypeValue ----------------------------------------
pub enum LazyElementTypeValue<'a, T> {
    LazyRecord(LazyRecord<'a, T>),
    Lazy(Box<dyn Fn(&BytesRecord<T>) -> ElementTypeValue + 'a>),
    Materialized(ElementTypeValue),
}

impl<T: std::fmt::Debug> LazyElementTypeValue<'_, T> {
    fn into_materialized(self, raw_bytes: &BytesRecord<T>) -> Self {
        match self {
            LazyElementTypeValue::LazyRecord(rec) => {
                let rec = Record::from((&rec, raw_bytes));

                LazyElementTypeValue::Materialized(
                    ElementTypeValue::Primitive(rec.into()),
                )
            }
            LazyElementTypeValue::Lazy(elm) => {
                LazyElementTypeValue::Materialized(elm(raw_bytes))
            }
            LazyElementTypeValue::Materialized(_) => self,
        }
    }

    fn materialize(&mut self, raw_bytes: &BytesRecord<T>) {
        if let LazyElementTypeValue::Lazy(elm) = self {
            *self = LazyElementTypeValue::Materialized(elm(raw_bytes));
        }
    }

    fn as_mut_record(&mut self) -> Result<&mut LazyRecord<T>, VmError> {
        todo!()
    }
}

impl<T: std::fmt::Debug> From<(LazyElementTypeValue<'_, T>, &BytesRecord<T>)>
    for ElementTypeValue
{
    fn from(value: (LazyElementTypeValue<'_, T>, &BytesRecord<T>)) -> Self {
        match value {
            (LazyElementTypeValue::LazyRecord(rec), _) => {
                ElementTypeValue::from(TypeValue::Record(Record::from((
                    &rec, value.1,
                ))))
            }
            (LazyElementTypeValue::Lazy(elm), raw_bytes) => elm(raw_bytes),
            (LazyElementTypeValue::Materialized(elm), _) => elm,
        }
    }
}

impl<T: std::fmt::Debug> From<(&LazyElementTypeValue<'_, T>, &BytesRecord<T>)>
    for ElementTypeValue
{
    fn from(
        (value, raw_bytes): (&LazyElementTypeValue<'_, T>, &BytesRecord<T>),
    ) -> Self {
        match (value, raw_bytes) {
            (LazyElementTypeValue::LazyRecord(rec), _) => {
                ElementTypeValue::from(TypeValue::Record(Record::from((
                    rec, raw_bytes,
                ))))
            }
            (LazyElementTypeValue::Lazy(elm), raw_bytes) => elm(raw_bytes),
            (LazyElementTypeValue::Materialized(elm), _) => elm.clone(),
        }
    }
}

// impl<T> Serialize for LazyElementTypeValue<'_, T> {
//     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
//     where
//         S: Serializer,
//     {
//         ElementTypeValue::from(self).serialize(serializer)
//     }
// }

impl<T: std::fmt::Debug> std::fmt::Debug for LazyElementTypeValue<'_, T> {
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

impl<T> std::fmt::Display for LazyElementTypeValue<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let LazyElementTypeValue::Materialized(mat_v) = self {
            write!(f, "{}", mat_v)
        } else {
            write!(f, "unresolved lazy value")
        }
    }
}

// impl<T> Eq for LazyElementTypeValue<'_, T> {}

// impl<T> PartialEq for LazyElementTypeValue<'_, T> {
//     fn eq(&self, other: &Self) -> bool {
//         ElementTypeValue::from(self) == ElementTypeValue::from(other)
//     }
// }

// impl<T> Clone for LazyElementTypeValue<'_, T> {
//     fn clone(&self) -> Self {
//         match self {
//             LazyElementTypeValue::LazyRecord(elm) => LazyElementTypeValue::LazyRecord(elm.clone()),
//             LazyElementTypeValue::Lazy(elm) => {
//                 Self::Materialized(elm())
//             }
//             LazyElementTypeValue::Materialized(elm) => {
//                 Self::Materialized(elm.clone())
//             }
//         }
//     }
// }

//------------ LazyRecord type ---------------------------------------------

// The LazyRecord is a Chimaera
#[derive(Debug)]
pub struct LazyRecord<'a, T> {
    value: LazyNamedTypeDef<'a, T>,
    is_materialized: bool,
    _raw_message: PhantomData<T>,
}

// impl<T> Eq for LazyRecord<'_, T> {}

impl<'a, T: std::fmt::Debug> LazyRecord<'a, T> {
    pub(crate) fn new(
        value: LazyNamedTypeDef<'a, T>,
        // raw_message: Arc<T>,
    ) -> Result<Self, VmError> {
        Ok(LazyRecord {
            value,
            is_materialized: false,
            _raw_message: PhantomData,
        })
    }

    pub(crate) fn from_type_def(
        ty: LazyNamedTypeDef<'a, T>,
    ) -> LazyRecord<'a, T> {
        Self {
            value: ty,
            is_materialized: false,
            _raw_message: PhantomData,
        }
    }

    // pub(crate) fn materialize_record(&mut self, raw_bytes: &BytesRecord<T>) {
    //     for field in &mut self.value {
    //         field.1.materialize(raw_bytes);
    //     }
    //     self.is_materialized = true;
    // }

    // pub(crate) fn into_materialized_record(self, raw_bytes: &BytesRecord<T>) -> Self {
    //     if self.is_materialized {
    //         return self;
    //     }

    //     let mut s = vec![];
    //     for field in self.value {
    //         s.push((field.0, field.1.into_materialized(raw_bytes)));
    //     }

    //     Self {
    //         value: s,
    //         is_materialized: true,
    //         _raw_message: PhantomData
    //     }
    // }

    // fn as_materialized_record(&self, raw_bytes: &RawBytes<T>) -> Self {
    //     self.clone().into_materialized_record(raw_bytes)
    // }

    // pub fn create_instance(
    //     ty: &TypeDef,
    //     kvs: Vec<(&str, TypeValue)>,
    // ) -> Result<Record, CompileError> {
    //     if kvs.is_empty() {
    //         return Err(CompileError::new(
    //             "Can't create empty instance.".into(),
    //         ));
    //     }

    //     let shortstring_vec = kvs
    //         .iter()
    //         .map(|(name, ty)| (ShortString::from(*name), ty))
    //         .collect::<Vec<_>>();
    //     if let TypeDef::Record(_rec) = ty {
    //         if ty._check_record_fields(shortstring_vec.as_slice()) {
    //             TypeValue::create_record(kvs)
    //         } else {
    //             Err(CompileError::new(
    //                 format!("Record fields do not match record type, expected instance of type {}, but got {:?}", ty, shortstring_vec)
    //             ))
    //         }
    //     } else {
    //         Err(CompileError::new("Not a record type".into()))
    //     }
    // }

    // pub fn get_value_for_field(
    //     &self,
    //     field: &str,
    // ) -> Option<ElementTypeValue> {
    //     self.value
    //         .iter()
    //         .find(|(f, _)| f == &field)
    //         .map(|f| ElementTypeValue::from(&f.1))
    // }

    pub fn get_field_by_index(
        &self,
        field_index: &[usize],
        raw_bytes: &BytesRecord<T>,
    ) -> Option<ElementTypeValue> {
        self.value.get(field_index[0]).map(|f| match &f.1 {
            LazyElementTypeValue::Lazy(l_value) => l_value(raw_bytes),
            LazyElementTypeValue::Materialized(m_value) => m_value.clone(),
            LazyElementTypeValue::LazyRecord(rec) => rec
                .get_field_by_index(&field_index[1..], raw_bytes)
                .unwrap(),
        })
    }

    pub fn exec_value_method(
        &self,
        _method: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
        raw_bytes: impl AsRef<[u8]>,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    // pub fn get_field_by_index_owned(
    //     &mut self,
    //     field_index: SmallVec<[usize; 8]>,
    // ) -> ElementTypeValue {
    //     let mut elm = self
    //         .value
    //         .get_mut(field_index[0])
    //         .map(|f| ElementTypeValue::from(f.1.clone()));

    //     for index in &field_index[1..] {
    //         elm = elm
    //             .or(None)
    //             .unwrap()
    //             .into_record()
    //             .unwrap()
    //             .0
    //             .get_mut(*index)
    //             .map(|f| std::mem::take(&mut f.1))
    //     }

    //     elm.unwrap()
    // }

    pub fn set_value_on_field_index(
        &'a mut self,
        field_index: SmallVec<[usize; 8]>,
        value: TypeValue,
    ) -> Result<(), VmError> {
        // let mut elm = self.0.get_mut(field_index[0]);

        // for index in &field_index[1..] {
        //     elm = elm
        //         .ok_or(VmError::MemOutOfBounds)?
        //         .1
        //         .as_mut_record()
        //         .unwrap()
        //         .0
        //         .get_mut(*index)
        // }

        // let e_tv = elm.ok_or(VmError::MemOutOfBounds)?;
        // let new_field = &mut (e_tv.0.clone(), ElementTypeValue::from(value));
        // std::mem::swap(e_tv, new_field);

        // Ok(())

        let mut elm: Option<&mut (ShortString, LazyElementTypeValue<'_, T>)> =
            self.value.get_mut(field_index[0]);

        for index in &field_index[1..] {
            elm = elm
                .ok_or(VmError::MemOutOfBounds)?
                .1
                .as_mut_record()
                .unwrap()
                .value
                .get_mut(*index)
        }

        let e_tv = elm.ok_or(VmError::MemOutOfBounds)?;
        let new_field = &mut (
            e_tv.0.clone(),
            LazyElementTypeValue::Materialized(ElementTypeValue::from(value)),
        );
        std::mem::swap(e_tv, new_field);

        Ok(())
    }
}

// impl<T> PartialEq for LazyRecord<'_, T> {
//     fn eq(&self, other: &Self) -> bool {
//         Record::from(self) == Record::from(other)
//     }
// }

// impl Eq for LazyRecord {}

// impl<T> Clone for LazyRecord<'_, T> {
//     fn clone(&self) -> Self {
//         self.as_materialized_record()
//     }
// }

// impl<T> std::hash::Hash for LazyRecord<'_, T> {
//     fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
//         todo!()
//     }
// }

impl<T> Display for LazyRecord<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (i, (field, elm)) in self.value.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "\n\t{}: ", field)?;
            write!(f, "{}", elm)?;
            // match *elm {
            //     ElementTypeValue::Primitive(v) => write!(f, "{}", v)?,
            //     ElementTypeValue::Nested(v) => write!(f, "{}", v)?,
            // }
        }
        write!(f, "\n   }}")
    }
}

// impl<T: std::fmt::Debug> RotoType for LazyRecord<T> {
//     fn get_props_for_method(
//         ty: TypeDef,
//         method_name: &crate::ast::Identifier,
//     ) -> Result<MethodProps, CompileError>
//     where
//         Self: std::marker::Sized,
//     {
//         match method_name.ident.as_str() {
//             "get" => Ok(MethodProps::new(
//                 ty,
//                 RecordToken::Get.into(),
//                 vec![TypeDef::U32],
//             )),
//             "get_all" => {
//                 Ok(MethodProps::new(ty, RecordToken::GetAll.into(), vec![]))
//             }
//             "contains" => Ok(MethodProps::new(
//                 TypeDef::Boolean,
//                 RecordToken::Contains.into(),
//                 vec![ty],
//             )),
//             _ => Err(format!(
//                 "Unknown method '{}' for Record type with fields {:?}",
//                 method_name.ident, ty
//             )
//             .into()),
//         }
//     }

//     fn into_type(
//         self,
//         into_type: &TypeDef,
//     ) -> Result<TypeValue, CompileError> {
//         // Converting from a Record into a Record is not as straight-forward
//         // as it is for primitive types, since we have to check whether the
//         // fields in both completely match.
//         match into_type {
//             TypeDef::Record(_) => {
//                 if into_type == &TypeValue::Record((&self).into()) {
//                     Ok(self.into())
//                 } else {
//                     match into_type {
//                         TypeDef::OutputStream(rec) => Ok(TypeValue::OutputStreamMessage(
//                             Arc::new(super::outputs::OutputStreamMessage {
//                                 name: "".into(),
//                                 topic: "".into(),
//                                 record_type: (**rec).clone(),
//                                 record: self.into(),
//                             }),
//                         )),
//                         _ => Err("Record type cannot be converted into another type"
//                         .to_string()
//                         .into())
//                     }
//                 }
//             }
//             _ => Err("Record type cannot be converted into another type"
//                 .to_string()
//                 .into()),
//         }
//     }

//     fn exec_value_method(
//         &self,
//         _method: usize,
//         _args: &[StackValue],
//         _res_type: TypeDef,
//     ) -> Result<TypeValue, VmError> {
//         todo!()
//     }

//     fn exec_consume_value_method(
//         self,
//         _method: usize,
//         _args: Vec<TypeValue>,
//         _res_type: TypeDef,
//     ) -> Result<TypeValue, VmError> {
//         todo!()
//     }

//     fn exec_type_method(
//         _method_token: usize,
//         _args: &[StackValue],
//         _res_type: TypeDef,
//     ) -> Result<TypeValue, VmError> {
//         todo!()
//     }
// }

// Value Expressions that contain a Record parsed as a pair of
// (field_name, value) pairs. This turns it into an actual Record.
// impl From<AnonymousRecordValueExpr> for LazyRecord {
//     fn from(value: AnonymousRecordValueExpr) -> Self {
//         LazyRecord(
//             value
//                 .key_values
//                 .iter()
//                 .map(|(s, t)| (s.ident.clone(), t.clone().into()))
//                 .collect::<Vec<_>>(),
//         )
//     }
// }

// impl From<TypedRecordValueExpr> for LazyRecord {
//     fn from(value: TypedRecordValueExpr) -> Self {
//         LazyRecord(
//             value
//                 .key_values
//                 .iter()
//                 .map(|(s, t)| (s.ident.clone(), t.clone().into()))
//                 .collect::<Vec<_>>(),
//         )
//     }
// }

impl<T: std::fmt::Debug> From<(&LazyRecord<'_, T>, &BytesRecord<T>)>
    for Record
{
    fn from(value: (&LazyRecord<T>, &BytesRecord<T>)) -> Self {
        Record(
            value
                .0
                .value
                .iter()
                .map(|(field, elm)| {
                    (field.clone(), ElementTypeValue::from((elm, value.1)))
                })
                .collect::<Vec<_>>(),
        )
    }
}

impl<T: std::fmt::Debug> From<(LazyRecord<'_, T>, &BytesRecord<T>)>
    for TypeValue
{
    fn from(value: (LazyRecord<T>, &BytesRecord<T>)) -> Self {
        let mut rec = vec![];
        for field in value.0.value {
            rec.push((field.0, ElementTypeValue::from((field.1, value.1))));
        }
        TypeValue::Record(Record::new(rec).unwrap())
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

impl From<usize> for BytesRecordToken {
    fn from(i: usize) -> Self {
        match i {
            0 => BytesRecordToken::Get,
            1 => BytesRecordToken::GetAll,
            2 => BytesRecordToken::Contains,
            _ => panic!("Unknown RecordToken"),
        }
    }
}

impl From<BytesRecordToken> for usize {
    fn from(t: BytesRecordToken) -> Self {
        t as usize
    }
}
