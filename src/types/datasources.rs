// =========== Data source Types ===========================================

// ----------- Rib Type ----------------------------------------------------

use crate::traits::RotoFilter;

use super::{collections::Record, typevalue::TypeValue, builtin::{BuiltinTypeValue, Boolean}, typedef::TypeDef};

#[derive(Debug, PartialEq)]
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
    ) -> Result<(u8, TypeValue), Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "match" => Ok((
                std::mem::size_of_val(&RibToken::Match) as u8,
                TypeValue::Record(self.record),
            )),
            "longest_match" => Ok((
                std::mem::size_of_val(&RibToken::LongestMatch) as u8,
                TypeValue::Record(self.record),
            )),
            "contains" => Ok((
                std::mem::size_of_val(&RibToken::Contains) as u8,
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(
                    None,
                ))),
            )),
            _ => {
                Err(format!("Unknown method '{}'", method_name.ident).into())
            }
        }
    }

    fn exec_method<'a>(
        &'a self,
        method: RibToken,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> Result<
        Box<dyn FnOnce(TypeValue) -> TypeValue + 'a>,
        Box<dyn std::error::Error>,
    > {
        todo!()
    }
}

pub enum RibToken {
    Match,
    LongestMatch,
    Get,
    Contains,
}

impl std::fmt::Display for Rib {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Rib with record type {}", self.record)
    }
}

// ----------- Table Type --------------------------------------------------

#[derive(Debug, PartialEq)]
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
    ) -> Result<(u8, TypeValue), Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "get" => Ok((
                std::mem::size_of_val(&TableToken::Get) as u8,
                TypeValue::Record(self.record),
            )),
            "contains" => Ok((
                std::mem::size_of_val(&TableToken::Contains) as u8,
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(
                    None,
                ))),
            )),
            _ => {
                Err(format!("Unknown method '{}'", method_name.ident).into())
            }
        }
    }

    fn exec_method<'a>(
        &'a self,
        method: TableToken,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> Result<
        Box<dyn FnOnce(TypeValue) -> TypeValue + 'a>,
        Box<dyn std::error::Error>,
    > {
        todo!()
    }

}

pub enum TableToken {
    Get,
    Contains,
}

impl std::fmt::Display for Table {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Table with record type {}", self.record)
    }
}