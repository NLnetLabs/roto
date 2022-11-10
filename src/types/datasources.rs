// =========== Data source Types ===========================================

// ----------- Rib Type ----------------------------------------------------

use crate::{traits::{RotoFilter, MethodProps}, symbols::Symbol};

use super::{
    builtin::{Boolean, BuiltinTypeValue},
    collections::Record,
    typedef::TypeDef,
    typevalue::TypeValue,
};

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
    ) -> Result<MethodProps, Box<(dyn std::error::Error)>>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
            "match" => Ok(MethodProps {
                method_token: std::mem::size_of_val(&RibToken::Match) as u8,
                return_type_value: TypeValue::Record(self.record),
                arg_types: vec![Symbol::new_argument_type(TypeDef::Prefix)]
            }),
            "longest_match" => Ok(MethodProps {
                method_token: std::mem::size_of_val(&RibToken::LongestMatch) as u8,
                return_type_value: TypeValue::Record(self.record),
                arg_types: vec![Symbol::new_argument_type(TypeDef::Prefix)]
        }),
            "contains" => Ok(MethodProps {
                method_token: std::mem::size_of_val(&RibToken::Contains) as u8,
                return_type_value: TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(None))),
                arg_types: vec![Symbol::new_argument_type(TypeDef::Prefix)]
        }),
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

    // pub fn get_type(&self) -> Vec<(ShortString, Box<TypeDef>)> {
    //     (&self.record).into()
    // }
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
            "get" => Ok(MethodProps {
                method_token: std::mem::size_of_val(&TableToken::Get) as u8,
                return_type_value: TypeValue::Record(self.record),
                arg_types: vec![Symbol::new_argument_type(TypeDef::Prefix)]
        }),
            "contains" => Ok(MethodProps {
                method_token: std::mem::size_of_val(&TableToken::Contains) as u8,
                return_type_value: TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(None))),
                arg_types: vec![Symbol::new_argument_type(TypeDef::Asn)]
        }),
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
