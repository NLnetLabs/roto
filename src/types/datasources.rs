// =========== Data source Types ===========================================

// ----------- Rib Type ----------------------------------------------------

use crate::{
    ast::ShortString,
    compile::CompileError,
    traits::{RotoType, Token, TokenConvert},
    vm::{StackRefPos, VmError},
};

use super::{
    builtin::{Boolean, BuiltinTypeValue},
    collections::Record,
    typedef::{MethodProps, TypeDef},
    typevalue::TypeValue,
};

#[derive(Debug, PartialEq, Eq)]
pub struct Rib {
    pub(crate) ty: TypeDef,
    pub(crate) records: Vec<Record>,
}

impl Rib {
    pub(crate) fn exec_ref_value_method<'a>(
        &'a self,
        method: usize,
        args: &'a [&'a TypeValue],
        res_type: TypeDef,
    ) -> Box<dyn FnOnce() -> DataSourceMethodValue + 'a> {
        match RibToken::from(method) {
            RibToken::Match => {
                todo!()
            }
            RibToken::LongestMatch => Box::new(|| {
                self.records
                    .iter()
                    .enumerate()
                    .find(|v| {
                        if let Some(val) = v.1.get_field_by_index(0) {
                            val == args[0]
                        } else {
                            false
                        }
                    })
                    .map(|v| {
                        DataSourceMethodValue::Ref(StackRefPos::TablePos(
                            Token::Table(v.0),
                            0,
                        ))
                    })
                    .unwrap_or_else(|| {
                        println!("WRITING Empty record INTO RIB FIELD");
                        // We can't write TypeValue::Unknown directly in here
                        // because we would erase the type then. Since this
                        // method can be in the middle of a chain we need to
                        // pass on the type
                        DataSourceMethodValue::Empty(res_type)
                    })
            }),
            RibToken::Contains => {
                todo!()
            }
            RibToken::Get => todo!(),
        }
    }
}

impl RotoType for Rib {
    fn get_props_for_method(
        ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "match" => Ok(MethodProps::new(
                ty,
                RibToken::Match.into(),
                vec![TypeDef::Prefix],
            )),
            "longest_match" => Ok(MethodProps::new(
                ty,
                RibToken::LongestMatch.into(),
                vec![TypeDef::Prefix],
            )),
            "contains" => Ok(MethodProps::new(
                TypeDef::Boolean,
                RibToken::Contains.into(),
                vec![TypeDef::Prefix],
            )),
            _ => Err(format!(
                "Unknown method '{}' for data source",
                method_name.ident
            )
            .into()),
        }
    }

    fn into_type(self, _type_def: &TypeDef) -> Result<TypeValue, CompileError>
    where
        Self: std::marker::Sized,
    {
        Err("Rib type cannot be converted into another type".into())
    }

    fn exec_value_method<'a>(
        &'a self,
        _method: usize,
        _args: &[&TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        unimplemented!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method: usize,
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        unimplemented!()
    }
}

impl From<Rib> for TypeValue {
    fn from(_value: Rib) -> Self {
        unimplemented!()
    }
}

#[derive(Debug)]
pub enum RibToken {
    Match,
    LongestMatch,
    Get,
    Contains,
}

impl TokenConvert for RibToken {}

impl From<usize> for RibToken {
    fn from(token: usize) -> Self {
        match token {
            0 => RibToken::Match,
            1 => RibToken::LongestMatch,
            2 => RibToken::Get,
            3 => RibToken::Contains,
            _ => panic!("Unknown token"),
        }
    }
}

impl From<RibToken> for usize {
    fn from(t: RibToken) -> usize {
        t as usize
    }
}

impl std::fmt::Display for Rib {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Rib with record type {}", self.ty)
    }
}

// ----------- Table Type --------------------------------------------------

pub type NamedTypeDef = (ShortString, Box<TypeDef>);

pub(crate) enum DataSourceMethodValue {
    Ref(StackRefPos),
    TypeValue(TypeValue),
    Empty(TypeDef)
}

#[derive(Debug, PartialEq, Eq)]
pub struct Table {
    pub(crate) ty: TypeDef,
    pub(crate) records: Vec<Record>,
}

impl Table {
    // fn inner_from_typevalue(
    //     type_value: TypeValue,
    // ) -> Result<Table, CompileError>
    // where
    //     Self: std::marker::Sized,
    // {
    //     if let TypeValue::Table(t) = type_value {
    //         Ok(t)
    //     } else {
    //         Err("Not a table type".into())
    //     }
    // }

    pub fn get_at_field_index(
        &self,
        index: usize,
        field_index: Option<usize>,
    ) -> Option<&TypeValue> {
        match field_index {
            None => self
                .records
                .get(index)
                .and_then(|r| r.0.get(index).map(|v| (&v.1).into())),
            Some(field_index) => match self.records.get(index) {
                Some(r) => {
                    r.get_field_by_index(field_index).map(|v| v.into())
                }
                _ => None,
            },
        }
    }

    pub(crate) fn exec_ref_value_method<'a>(
        &'a self,
        method_token: usize,
        args: &'a [&'a TypeValue],
        _res_type: TypeDef,
    ) -> Box<dyn FnOnce() -> DataSourceMethodValue + 'a> {
        match method_token.into() {
            TableToken::Find => Box::new(|| {
                self.records
                    .iter()
                    .enumerate()
                    .find(|v| {
                        if let Some(val) = v.1.get_field_by_index(0) {
                            val == args[0]
                        } else {
                            false
                        }
                    })
                    .map(|v| {
                        DataSourceMethodValue::Ref(StackRefPos::TablePos(
                            Token::Table(v.0),
                            0,
                        ))
                    })
                    .unwrap_or_else(|| {
                        DataSourceMethodValue::TypeValue(TypeValue::Unknown)
                    })
            }),
            TableToken::Contains => Box::new(|| {
                self.records
                    .iter()
                    .enumerate()
                    .find(|v| {
                        if let Some(val) = v.1.get_field_by_index(0) {
                            val == args[0]
                        } else {
                            false
                        }
                    })
                    .map(|_v| {
                        DataSourceMethodValue::TypeValue(TypeValue::Builtin(
                            BuiltinTypeValue::Boolean(Boolean(true)),
                        ))
                    })
                    .unwrap_or_else(|| {
                        DataSourceMethodValue::TypeValue(TypeValue::Builtin(
                            BuiltinTypeValue::Boolean(Boolean(false)),
                        ))
                    })
            }),
        }
    }
}

impl RotoType for Table {
    fn get_props_for_method(
        ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "find" => Ok(MethodProps::new(
                ty,
                TableToken::Find.into(),
                vec![TypeDef::Asn],
            )),
            "contains" => Ok(MethodProps::new(
                TypeDef::Boolean,
                TableToken::Contains.into(),
                vec![TypeDef::Asn],
            )),
            _ => Err(format!(
                "Unknown method '{}' for table",
                method_name.ident
            )
            .into()),
        }
    }

    fn into_type(self, _type_def: &TypeDef) -> Result<TypeValue, CompileError>
    where
        Self: std::marker::Sized,
    {
        Err("Table type cannot be converted into another type".into())
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &[&TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        unimplemented!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        unimplemented!()
    }
}

impl From<Table> for TypeValue {
    fn from(_value: Table) -> Self {
        unimplemented!()
    }
}

#[derive(Debug)]
pub enum TableToken {
    Find,
    Contains,
}

impl TokenConvert for TableToken {}

impl From<usize> for TableToken {
    fn from(token: usize) -> Self {
        match token {
            0 => TableToken::Find,
            1 => TableToken::Contains,
            t => panic!("Unknown method with token {}", t),
        }
    }
}

impl From<TableToken> for usize {
    fn from(t: TableToken) -> usize {
        t as usize
    }
}

impl std::fmt::Display for Table {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Table with record type {:#?}", self.ty)
    }
}
