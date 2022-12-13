// =========== Data source Types ===========================================

// ----------- Rib Type ----------------------------------------------------

use crate::traits::{MethodProps, RotoFilter, TokenConvert};

use super::{
    builtin::{Boolean, BuiltinTypeValue},
    collections::Record,
    typedef::TypeDef,
    typevalue::TypeValue,
};

#[derive(Debug, PartialEq, Eq)]
pub struct Rib {
    pub(crate) record: Record,
}

impl Rib {
    fn inner_from_typevalue(
        type_value: TypeValue,
    ) -> Result<Rib, Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }
}

impl RotoFilter<RibToken> for Rib {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, Box<(dyn std::error::Error)>>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "match" => Ok(MethodProps::new(
                TypeValue::Record(self.record),
                RibToken::Match.into(),
                vec![TypeDef::Prefix],
            )),
            "longest_match" => Ok(MethodProps::new(
                TypeValue::Record(self.record),
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

    fn into_type(
        self,
        _type_def: &TypeDef,
    ) -> Result<TypeValue, Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized,
    {
        Err("Rib type cannot be converted into another type".into())
    }

    fn exec_method<'a>(
        &'a self,
        _method: usize,
        _args: Vec<&'a TypeValue>,
        _res_type: TypeDef,
    ) -> Result<
        Box<dyn FnOnce(TypeValue) -> TypeValue + 'a>,
        Box<dyn std::error::Error>,
    > {
        todo!()
    }

    fn exec_type_method<'a>(
        _method: usize,
        _args: Vec<&'a TypeValue>,
        _res_type: TypeDef,
    ) -> Result<
        Box<dyn FnOnce() -> TypeValue + 'a>,
        Box<dyn std::error::Error>,
    > {
        todo!()
    }

    fn get_field_by_index(
        self,
        _field_index: usize,
    ) -> Result<TypeValue, Box<dyn std::error::Error>> {
        todo!()
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
        write!(f, "Rib with record type {}", self.record)
    }
}

// ----------- Table Type --------------------------------------------------

#[derive(Debug, PartialEq, Eq)]
pub struct Table {
    pub(crate) record: Record,
}

impl Table {
    fn inner_from_typevalue(
        type_value: TypeValue,
    ) -> Result<Table, Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized,
    {
        if let TypeValue::Table(t) = type_value {
            Ok(t)
        } else {
            Err("Not a table type".into())
        }
    }
}

impl RotoFilter<TableToken> for Table {
    fn get_props_for_method(
        self,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, Box<(dyn std::error::Error)>>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "get" => Ok(MethodProps::new(
                TypeValue::Record(self.record),
                TableToken::Get.into(),
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

    fn into_type(
        self,
        _type_def: &TypeDef,
    ) -> Result<TypeValue, Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized,
    {
        Err("Table type cannot be converted into another type".into())
    }

    fn exec_method<'a>(
        &'a self,
        _method: usize,
        _args: Vec<&'a TypeValue>,
        _res_type: TypeDef,
    ) -> Result<
        Box<dyn FnOnce(TypeValue) -> TypeValue + 'a>,
        Box<dyn std::error::Error>,
    > {
        todo!()
    }

    fn exec_type_method<'a>(
        _method: usize,
        _args: Vec<&'a TypeValue>,
        _res_type: TypeDef,
    ) -> Result<
        Box<dyn FnOnce() -> TypeValue + 'a>,
        Box<dyn std::error::Error>,
    > {
        todo!()
    }

    fn get_field_by_index(
            self,
            field_index: usize,
        ) -> Result<TypeValue, Box<dyn std::error::Error>> {
            todo!()
        }
    
}

#[derive(Debug)]
pub enum TableToken {
    Get,
    Contains,
}

impl TokenConvert for TableToken {}

impl From<usize> for TableToken {
    fn from(token: usize) -> Self {
        match token {
            0 => TableToken::Get,
            1 => TableToken::Contains,
            _ => panic!("Unknown token"),
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
        write!(f, "Table with record type {}", self.record)
    }
}
