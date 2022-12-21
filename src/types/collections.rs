use std::fmt::{Display, Formatter};

use crate::ast::ShortString;
use crate::compile::CompileError;
use crate::traits::{MethodProps, RotoFilter, TokenConvert};
use crate::vm::Payload;

use super::builtin::{
    AsPath, Asn, Community, IpAddress,
    Prefix, PrefixLength, U32, U8,
};
use super::typedef::TypeDef;
use super::typevalue::TypeValue;

//============ Collections ==================================================

//------------ ElementType -------------------------------------

// This enum is used to differentiate between recursive collections and simple
// collections (that only contain primitive types). The latter do not need to
// be boxed, while the former do.

#[derive(Debug, PartialEq, Eq)]
pub enum ElementTypeValue {
    Primitive(TypeValue),
    Nested(Box<TypeValue>),
}

impl<'a> From<&'a TypeDef> for ElementTypeValue {
    fn from(t: &'a TypeDef) -> Self {
        match t {
            TypeDef::U32 => ElementTypeValue::Primitive(U32(None).into()),
            TypeDef::U8 => ElementTypeValue::Primitive(U8(None).into()),
            TypeDef::Prefix => {
                ElementTypeValue::Primitive(Prefix(None).into())
            }
            TypeDef::PrefixLength => {
                ElementTypeValue::Primitive(PrefixLength(None).into())
            }
            TypeDef::IpAddress => {
                ElementTypeValue::Primitive(IpAddress(None).into())
            }
            TypeDef::Asn => ElementTypeValue::Primitive(Asn(None).into()),
            TypeDef::AsPath => {
                ElementTypeValue::Primitive(AsPath(None).into())
            }
            TypeDef::Community => {
                ElementTypeValue::Primitive(Community(None).into())
            }
            TypeDef::List(ty) => ElementTypeValue::Nested(Box::new(
                TypeValue::List(ty.as_ref().into()),
            )),
            TypeDef::Record(kv_list) => {
                let def_ = kv_list
                    .iter()
                    .map(|(ident, ty)| (ident.clone(), ty.as_ref().into()))
                    .collect::<Vec<_>>();
                ElementTypeValue::Nested(Box::new(TypeValue::Record(
                    Record::new(def_).unwrap(),
                )))
            }
            _ => panic!("Unknown type"),
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
            _ => panic!("Unknown type"),
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
                _ => panic!("Unknown type"),
            },
        }
    }
}

impl<'a> From<&'a ElementTypeValue> for &'a TypeValue {
    fn from(t: &'a ElementTypeValue) -> Self {
        match t {
            ElementTypeValue::Primitive(v) => v,
            ElementTypeValue::Nested(ty) => match ty.as_ref() {
                TypeValue::List(li) => ty,
                TypeValue::Record(kv_list) => ty,
                _ => panic!("Unknown type"),
            },
        }
    }
}

impl PartialEq<TypeValue> for ElementTypeValue {
    fn eq(&self, other: &TypeValue) -> bool {
        match self {
            ElementTypeValue::Primitive(v) => v == other,
            _ => false
        }
    }
}

//------------ List type ----------------------------------------------------

#[derive(Debug, PartialEq, Eq)]
pub struct List(pub(crate) Vec<ElementTypeValue>);

impl List {
    pub fn new(elem_type: Vec<ElementTypeValue>) -> Self {
        List(elem_type)
    }

    pub(crate) fn iter(&self) -> std::slice::Iter<ElementTypeValue> {
        self.0.iter()
    }

    fn inner_from_typevalue(
        type_value: TypeValue,
    ) -> std::result::Result<
        Vec<ElementTypeValue>,
        CompileError,
    >
    where
        Self: std::marker::Sized,
    {
        match type_value {
            TypeValue::List(list) => Ok(list.0),
            _ => Err("Not a List type".into()),
        }
    }

    pub fn exec_method(
        &self,
        _method_token: usize,
        _args: Vec<&TypeValue>,
        _res_type: TypeDef,
    ) -> Result<
        Box<dyn FnOnce(TypeValue) -> TypeValue + '_>,
        CompileError,
    > {
        todo!()
    }

    pub fn get_field_by_index(
        &self,
        index: usize,
    ) -> Option<&(ShortString, ElementTypeValue)> {
        // self.0.get(index).ok_or("Index out of bounds".into()).into()
        todo!()
    }
}

impl<'a> From<&'a TypeDef> for List {
    fn from(t: &'a TypeDef) -> Self {
        List::new(vec![t.into()])
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

impl RotoFilter<ListToken> for List {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "get" => Ok(MethodProps::new(
                TypeValue::List(self),
                ListToken::Get.into(),
                vec![TypeDef::U32],
            )),
            "remove" => Ok(MethodProps::new(
                TypeValue::List(self),
                ListToken::Remove.into(),
                vec![TypeDef::U32],
            )),
            "push" => Ok(MethodProps::new(
                (&TypeDef::Boolean).into(),
                ListToken::Push.into(),
                vec![TypeDef::from(&self.0[0])],
            )),
            "contains" => Ok(MethodProps::new(
                (&TypeDef::Boolean).into(),
                ListToken::Contains.into(),
                vec![],
            )),
            _ => Err(format!(
                "Unknown method '{}' for list",
                method_name.ident
            )
            .into()),
        }
    }

    fn into_type(
        self,
        _type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        Err("List type cannot be converted into another type".into())
    }

    fn exec_value_method(
        &self,
        _method: usize,
        _args: &[&TypeValue],
        _res_type: TypeDef,
    ) -> Result<
        std::boxed::Box<(dyn FnOnce() -> TypeValue)>,
        CompileError,
    > {
        todo!()
    }

    fn exec_type_method<'a>(
        method_token: usize,
        args: &[&'a TypeValue],
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, CompileError>
    {
        todo!()
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

impl TokenConvert for ListToken {}

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

#[derive(Debug, PartialEq, Eq, Default)]
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
        let shortstring_vec = kvs
            .iter()
            .map(|(name, ty)| (ShortString::from(*name), ty))
            .collect::<Vec<_>>();
        if let TypeDef::Record(_rec) = ty {
            if ty._check_record_fields(shortstring_vec.as_slice()) {
                TypeValue::create_record(kvs)
            } else {
                Err(CompileError::new("Record fields do not match record type".into()))
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
        index: usize,
    ) -> Option<&'a (ShortString, ElementTypeValue)> {
        self.0.get(index)
    }

    fn inner_from_typevalue(
        ty: TypeValue,
    ) -> std::result::Result<
        Vec<(ShortString, ElementTypeValue)>,
        CompileError,
    > {
        match ty {
            TypeValue::Record(r) => Ok(r.0),
            _ => Err("Not a record type".into()),
        }
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

impl RotoFilter<RecordToken> for Record {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "longest_match" => Ok(MethodProps::new(
                TypeValue::Record(self),
                RecordToken::LongestMatch.into(),
                vec![TypeDef::Prefix],
            )),
            "get" => Ok(MethodProps::new(
                TypeValue::Record(self),
                RecordToken::Get.into(),
                vec![TypeDef::U32],
            )),
            "get_all" => Ok(MethodProps::new(
                TypeValue::Record(self),
                RecordToken::GetAll.into(),
                vec![],
            )),
            "contains" => Ok(MethodProps::new(
                (&TypeDef::Boolean).into(),
                RecordToken::Contains.into(),
                vec![(&TypeValue::Record(self)).into()],
            )),
            _ => Err(format!(
                "Unknown method '{}' for Record type with fields {:?}",
                method_name.ident, self
            )
            .into()),
        }
    }

    fn into_type(
        self,
        _type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        Err("Record type cannot be converted into another type".into())
    }

    fn exec_value_method(
        &self,
        _method: usize,
        _args: &[&TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + '_>, CompileError>
    {
        todo!()
    }

    fn exec_type_method<'a>(
        method_token: usize,
        args: &[&'a TypeValue],
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, CompileError>
    {
        todo!()
    }
}

impl From<Vec<(ShortString, Box<TypeDef>)>> for Record {
    fn from(st_vec: Vec<(ShortString, Box<TypeDef>)>) -> Self {
        Record(st_vec.iter().map(|(s, t)| (s.clone(), t.as_ref().into())).collect::<Vec<_>>())
    }
}

#[derive(Debug)]
#[repr(u8)]
pub enum RecordToken {
    Get = 0,
    GetAll = 1,
    Contains = 2,
    LongestMatch = 3,
    Set,
}

impl TokenConvert for RecordToken {}

impl From<usize> for RecordToken {
    fn from(i: usize) -> Self {
        match i {
            0 => RecordToken::Get,
            1 => RecordToken::GetAll,
            2 => RecordToken::Contains,
            3 => RecordToken::LongestMatch,
            _ => panic!("Unknown RecordToken"),
        }
    }
}

impl From<RecordToken> for usize {
    fn from(t: RecordToken) -> Self {
        t as usize
    }
}

impl Payload for Record {
    fn set(&mut self, field: ShortString, value: TypeValue) {
        todo!()
    }

    fn get(&self, field: ShortString) -> Option<&TypeValue> {
        todo!()
    }

    fn take_value(self) -> TypeValue {
        // let v = std::mem::take(self);
        TypeValue::Record(self)
    }
}
