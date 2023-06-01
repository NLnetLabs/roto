// =========== Data source Types ===========================================

// ----------- Rib Type ----------------------------------------------------

use crate::{
    ast::ShortString,
    compile::CompileError,
    traits::{RotoType, Token, TokenConvert},
    vm::{StackRefPos, StackValue, VmError},
};

use super::{
    builtin::{Boolean, BuiltinTypeValue},
    collections::Record,
    typedef::{MethodProps, TypeDef},
    typevalue::TypeValue,
};

// This data-structure only exists to populate the static methods for the type
// `Rib`, e.g. the methods `Rib::method_name()` and their properties.
#[derive(Debug)]
pub struct Rib {
    pub(crate) ty: TypeDef,
}

impl Rib {
    pub(crate) fn get_props_for_method(
        ty: &TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError> {
        match method_name.ident.as_str() {
            "match" => Ok(MethodProps::new(
                ty.clone(),
                RibToken::Match.into(),
                vec![TypeDef::Prefix],
            )),
            "longest_match" => Ok(MethodProps::new(
                ty.clone(),
                RibToken::LongestMatch.into(),
                vec![TypeDef::Prefix],
            )),
            "contains" => Ok(MethodProps::new(
                TypeDef::Boolean,
                RibToken::Contains.into(),
                vec![TypeDef::Prefix],
            )),
            _ => Err(format!(
                "Unknown method '{}' for data source of type Rib",
                method_name.ident
            )
            .into()),
        }
    }

    pub(crate) fn exec_type_method<'a>(
        _method: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
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

pub enum DataSourceMethodValue {
    Ref(StackRefPos),
    TypeValue(TypeValue),
    Empty(TypeDef),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Table {
    pub(crate) ty: TypeDef,
    pub(crate) records: Vec<Record>,
}

impl Table {
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
        args: &'a [StackValue],
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
        _args: &[StackValue],
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
        _args: &[StackValue],
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
