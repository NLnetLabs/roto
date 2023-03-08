use std::fmt::{Display, Formatter};

use crate::ast::ShortString;
use crate::compile::CompileError;
use crate::traits::{RotoType, TokenConvert};
use crate::vm::VmError;

use super::builtin::{BuiltinTypeValue, U32};
use super::typedef::{MethodProps, TypeDef};
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
                TypeValue::List(_li) => ty,
                TypeValue::Record(_kv_list) => ty,
                _ => panic!("Unknown type"),
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

impl Default for ElementTypeValue {
    fn default() -> Self {
        Self::Primitive(TypeValue::Unknown)
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
    ) -> std::result::Result<Vec<ElementTypeValue>, CompileError>
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
    ) -> Result<Box<dyn FnOnce(TypeValue) -> TypeValue + '_>, CompileError>
    {
        todo!()
    }

    pub fn get_field_by_index(
        &self,
        index: usize,
    ) -> Option<&ElementTypeValue> {
        // self.0.get(index).ok_or("Index out of bounds".into())
        self.0.get(index)
    }

    pub fn get_field_by_index_owned(
        &mut self,
        index: usize,
    ) -> Option<ElementTypeValue> {
        self.0.get_mut(index).map(std::mem::take)
    }

    pub fn set_field_for_index(
        &mut self,
        index: usize,
        value: TypeValue,
    ) -> Result<(), VmError> {
        let e_tv = self.0.get_mut(index).ok_or(VmError::MemOutOfBounds)?;
        let new_field = &mut ElementTypeValue::from(value);
        std::mem::swap(e_tv, new_field);
        Ok(())
    }
}

impl<'a> From<&'a TypeDef> for List {
    fn from(t: &'a TypeDef) -> Self {
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
        args: &'a [&TypeValue],
        _res_type: TypeDef,
    ) -> Result<std::boxed::Box<(dyn FnOnce() -> TypeValue + '_)>, VmError>
    {
        match method.into() {
            ListToken::Len => Ok(Box::new(move || {
                TypeValue::Builtin(BuiltinTypeValue::U32(U32(
                    self.0.len() as u32,
                )))
            })),
            ListToken::Contains => {
                Ok(Box::new(move || self.iter().any(|e| e == args[0]).into()))
            }
            _ => {
                println!("can't exec method on {}", method);
                Err(VmError::InvalidMethodCall)
            }
        }
    }

    fn exec_consume_value_method(
        mut self,
        method: usize,
        mut args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        match method.into() {
            ListToken::Get => Ok(Box::new(move || {
                match self.0.into_iter().find(|e| *e == args[0]) {
                    Some(e) => e.into(),
                    None => TypeValue::Unknown,
                }
            })),
            ListToken::Push => Ok(Box::new(move || {
                self.0.push((args.remove(0)).into());
                TypeValue::List(self)
            })),
            ListToken::Pop => todo!(),
            ListToken::Remove => todo!(),
            ListToken::Insert => todo!(),
            ListToken::Clear => todo!(),
            m => {
                println!("Can't call method on {:?}", m);
                Err(VmError::InvalidMethodCall)
            }
        }
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
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
        if kvs.is_empty() {
            return Err(CompileError::new("Can't create empty instance.".into()));
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
                    "Record fields do not match record type".into(),
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
        index: usize,
    ) -> Option<&'a ElementTypeValue> {
        self.0.get(index).map(|f| &f.1)
    }

    pub fn get_field_by_index_owned(
        &mut self,
        index: usize,
    ) -> ElementTypeValue {
        self.0
            .get_mut(index)
            .map(|f| std::mem::take(&mut f.1))
            .unwrap()
    }

    pub fn set_field_for_index(
        &mut self,
        index: usize,
        value: TypeValue,
    ) -> Result<(), VmError> {
        let e_tv = self.0.get_mut(index).ok_or(VmError::MemOutOfBounds)?;
        let new_field = &mut (e_tv.0.clone(), ElementTypeValue::from(value));
        std::mem::swap(e_tv, new_field);
        Ok(())
    }

    fn inner_from_typevalue(
        ty: TypeValue,
    ) -> std::result::Result<Vec<(ShortString, ElementTypeValue)>, CompileError>
    {
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
        type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        match type_def {
            TypeDef::Record(_) => Ok(TypeValue::Record(self)),
            _ => Err("Record type cannot be converted into another type"
                .to_string()
                .into()),
        }
    }

    fn exec_value_method(
        &self,
        _method: usize,
        _args: &[&TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + '_>, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        mut self,
        method: usize,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }
}

// impl From<Vec<(ShortString, Box<TypeDef>)>> for Record {
//     fn from(st_vec: Vec<(ShortString, Box<TypeDef>)>) -> Self {
//         Record(
//             st_vec
//                 .iter()
//                 .map(|(s, t)| (s.clone(), t.as_ref().into()))
//                 .collect::<Vec<_>>(),
//         )
//     }
// }

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

impl TokenConvert for RecordToken {}

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
