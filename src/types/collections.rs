use crate::ast::ShortString;
use crate::symbols::{Symbol, self};
use crate::traits::{RotoFilter, MethodProps};

use super::builtin::{
    AsPath, Asn, Boolean, BuiltinTypeValue, Community, IpAddress, Prefix,
    U32, U8,
};
use super::typedef::TypeDef;
use super::typevalue::TypeValue;

//============ Collections ==================================================

//------------ ElementType -------------------------------------

// This enum is used to differentiate between recursive collections and simple
// collections (that only contain primitive types). The latter do not need to
// be boxed, while the former do.

#[derive(Debug, PartialEq)]
pub enum ElementTypeValue {
    Primitive(BuiltinTypeValue),
    Nested(Box<TypeValue>),
}

impl<'a> From<&'a TypeDef> for ElementTypeValue {
    fn from(t: &'a TypeDef) -> Self {
        match t {
            TypeDef::U32 => {
                ElementTypeValue::Primitive(BuiltinTypeValue::U32(U32(None)))
            }
            TypeDef::U8 => {
                ElementTypeValue::Primitive(BuiltinTypeValue::U8(U8(None)))
            }
            TypeDef::Prefix => ElementTypeValue::Primitive(
                BuiltinTypeValue::Prefix(Prefix(None)),
            ),
            TypeDef::IpAddress => ElementTypeValue::Primitive(
                BuiltinTypeValue::IpAddress(IpAddress(None)),
            ),
            TypeDef::Asn => {
                ElementTypeValue::Primitive(BuiltinTypeValue::Asn(Asn(None)))
            }
            TypeDef::AsPath => ElementTypeValue::Primitive(
                BuiltinTypeValue::AsPath(AsPath(None)),
            ),
            TypeDef::Community => ElementTypeValue::Primitive(
                BuiltinTypeValue::Community(Community(None)),
            ),
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
            TypeValue::Builtin(v) => ElementTypeValue::Primitive(v),
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
            ElementTypeValue::Primitive(v) => TypeValue::Builtin(v),
            ElementTypeValue::Nested(ty) => {
                match *ty {
                    TypeValue::List(ty) => {
                        TypeValue::List(ty)
                    }
                    TypeValue::Record(kv_list) => {
                        TypeValue::Record(kv_list)
                    }
                    _ => panic!("Unknown type"),
                }
            }
        }
    }
}

//------------ List type ----------------------------------------------------

#[derive(Debug, PartialEq)]
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
        std::boxed::Box<(dyn std::error::Error)>,
    >
    where
        Self: std::marker::Sized,
    {
        match type_value {
            TypeValue::List(list) => Ok(list.0),
            _ => Err("Not a List type".into()),
        }
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
    ) -> Result<MethodProps, Box<(dyn std::error::Error + 'static)>>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "get" => Ok(MethodProps {
                method_token: std::mem::size_of_val(&ListToken::Get) as u8,
                return_type_value: TypeValue::List(self),
                arg_types: vec![Symbol::new_argument_type(TypeDef::U32)],
        }),
            "remove" => Ok(MethodProps {
                method_token: std::mem::size_of_val(&ListToken::Remove) as u8,
                return_type_value: TypeValue::List(self),
                arg_types: vec![Symbol::new_argument_type(TypeDef::U32)],
        }),
            "contains" => Ok(MethodProps {
                method_token: std::mem::size_of_val(&ListToken::Contains) as u8,
                return_type_value: (&TypeDef::Boolean).into(),
                arg_types: vec![],
        }),
            _ => {
                Err(format!("Unknown method '{}'", method_name.ident).into())
            }
        }
    }

    fn exec_method(
        &self,
        _method: ListToken,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<
        std::boxed::Box<(dyn FnOnce(TypeValue) -> TypeValue)>,
        Box<dyn std::error::Error>,
    > {
        todo!()
    }
}

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

//---------------- Record type ----------------------------------------------

#[derive(Debug, PartialEq)]
pub struct Record(pub(crate) Vec<(ShortString, ElementTypeValue)>);

impl<'a> Record {
    pub(crate) fn new(
        elems: Vec<(ShortString, ElementTypeValue)>,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        Ok(Self(elems))
    }

    pub fn create_instance(
        ty: &TypeDef,
        kvs: Vec<(&str, TypeValue)>,
    ) -> Result<Record, Box<dyn std::error::Error>> {
        let shortstring_vec = kvs
            .iter()
            .map(|(name, ty)| (ShortString::from(*name), ty))
            .collect::<Vec<_>>();
        if let TypeDef::Record(_rec) = ty {
            if ty._check_record_fields(shortstring_vec.as_slice()) {
                TypeValue::create_record(kvs)
            } else {
                Err("Record fields do not match record type".into())
            }
        } else {
            Err("Not a record type".into())
        }
    }

    pub fn get_value_for_field(
        &self,
        field: &'a str,
    ) -> Option<&ElementTypeValue> {
        self.0.iter().find(|(f, _)| f == &field).map(|(_, v)| v)
    }

    fn inner_from_typevalue(
        ty: TypeValue,
    ) -> std::result::Result<
        Vec<(ShortString, ElementTypeValue)>,
        std::boxed::Box<(dyn std::error::Error)>,
    > {
        match ty {
            TypeValue::Record(r) => Ok(r.0),
            _ => Err("Not a record type".into()),
        }
    }
}

impl std::fmt::Display for Record {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (i, (field, elem)) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: ", field)?;
            match elem {
                ElementTypeValue::Primitive(v) => write!(f, "{}", v)?,
                ElementTypeValue::Nested(v) => write!(f, "{}", v)?,
            }
        }
        write!(f, "}}")
    }
}

impl RotoFilter<RecordToken> for Record {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, Box<(dyn std::error::Error + 'static)>>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "longest_match" => Ok(MethodProps {
                method_token: std::mem::size_of_val(&RecordToken::LongestMatch) as u8,
                return_type_value: TypeValue::Record(Record::new(vec![(
                    ShortString::from("prefix"),
                    ElementTypeValue::Nested(Box::new(TypeValue::Record(
                        self,
                    ))),
                )])?),
                arg_types: vec![Symbol::new_argument_type(TypeDef::Prefix)],
        }   ),
            "get" => Ok(MethodProps {
                method_token: std::mem::size_of_val(&RecordToken::Get) as u8,
                return_type_value: TypeValue::Record(self),
                arg_types: vec![Symbol::new_argument_type(TypeDef::U32)],
        }),
            "get_all" => Ok(MethodProps {
                method_token: std::mem::size_of_val(&RecordToken::GetAll) as u8,
                return_type_value: TypeValue::Record(self),
                arg_types: vec![],
        }),
            "contains" => Ok(MethodProps {
                method_token: std::mem::size_of_val(&RecordToken::Contains) as u8,
                return_type_value: (&TypeDef::Boolean).into(),
                arg_types: vec![Symbol::new_with_value("self_arg".into(), symbols::SymbolKind::Argument, TypeValue::Record(self), vec![])],
        }),
            "set" => Ok(MethodProps {
                method_token: std::mem::size_of_val(&RecordToken::Set) as u8,
                return_type_value: TypeValue::None,
                arg_types: vec![Symbol::new_with_value("self_arg".into(), symbols::SymbolKind::Argument, TypeValue::Record(self), vec![])],
        }),
            _ => {
                Err(format!("Unknown method '{}' for Record type with fields {:?}", method_name.ident, self).into())
            }
        }
    }

    fn exec_method(
        &self,
        _method: RecordToken,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<
        Box<dyn FnOnce(TypeValue) -> TypeValue + '_>,
        Box<dyn std::error::Error>,
    > {
        todo!()
    }
}

#[repr(u8)]
pub enum RecordToken {
    Get = 0,
    GetAll = 1,
    Contains = 2,
    LongestMatch = 3,
    Set
}
