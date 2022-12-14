// =========== Data source Types ===========================================

// ----------- Rib Type ----------------------------------------------------

use crate::{
    ast::ShortString,
    compile::CompileError,
    traits::{MethodProps, RotoFilter, Token, TokenConvert},
    vm::StackRefPos,
};

use super::{
    builtin::{Boolean, BuiltinTypeValue},
    collections::Record,
    typedef::TypeDef,
    typevalue::TypeValue,
};

#[derive(Debug, PartialEq, Eq)]
pub struct Rib {
    pub(crate) ty: TypeDef,
    pub(crate) records: Vec<Record>
}

impl Rib {
    fn inner_from_typevalue(
        type_value: TypeValue,
    ) -> Result<Rib, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

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
            RibToken::LongestMatch => {
                Box::new(|| self.records
                    .iter()
                    .enumerate()
                    .find(|v| {
                        if let Some(val) =
                            v.1.get_field_by_index(0).map(|v| &v.1)
                        {
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
                        DataSourceMethodValue::TypeValue(TypeValue::None)
                    }))
            }
            RibToken::Contains => {
                todo!()
            }
            RibToken::Get => todo!(),
        }
    }
}

impl RotoFilter<RibToken> for Rib {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "match" => Ok(MethodProps::new(
                TypeValue::Record(Record::create_empty_instance(&self.ty)?),
                RibToken::Match.into(),
                vec![TypeDef::Prefix],
            )),
            "longest_match" => Ok(MethodProps::new(
                TypeValue::Record(Record::create_empty_instance(&self.ty)?),
                RibToken::LongestMatch.into(),
                vec![TypeDef::Prefix],
            )),
            "contains" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(None))),
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
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, CompileError> {
        unimplemented!()
    }

    fn exec_type_method<'a>(
        _method: usize,
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, CompileError> {
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
}

#[derive(Debug, PartialEq, Eq)]
pub struct Table {
    pub(crate) ty: TypeDef,
    pub(crate) records: Vec<Record>,
}

impl Table {
    fn inner_from_typevalue(
        type_value: TypeValue,
    ) -> Result<Table, CompileError>
    where
        Self: std::marker::Sized,
    {
        if let TypeValue::Table(t) = type_value {
            Ok(t)
        } else {
            Err("Not a table type".into())
        }
    }

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
                    r.get_field_by_index(field_index).map(|v| (&v.1).into())
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
                        if let Some(val) =
                            v.1.get_field_by_index(0).map(|v| &v.1)
                        {
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
                        DataSourceMethodValue::TypeValue(TypeValue::None)
                    })
            }),
            TableToken::Contains => Box::new(|| {
                self.records
                    .iter()
                    .enumerate()
                    .find(|v| {
                        if let Some(val) =
                            v.1.get_field_by_index(0).map(|v| &v.1)
                        {
                            val == args[0]
                        } else {
                            false
                        }
                    })
                    .map(|_v| {
                        DataSourceMethodValue::TypeValue(TypeValue::Builtin(
                            BuiltinTypeValue::Boolean(Boolean(Some(true))),
                        ))
                    })
                    .unwrap_or_else(|| {
                        DataSourceMethodValue::TypeValue(TypeValue::Builtin(
                            BuiltinTypeValue::Boolean(Boolean(Some(false))),
                        ))
                    })
            }),
        }
    }
}

impl RotoFilter<TableToken> for Table {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "find" => Ok(MethodProps::new(
                (&self.ty).into(),
                TableToken::Find.into(),
                vec![TypeDef::Asn],
            )),
            "contains" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(None))),
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
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, CompileError> {
        unimplemented!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, CompileError> {
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
