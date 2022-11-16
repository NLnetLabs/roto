// =========== Data source Types ===========================================

// ----------- Rib Type ----------------------------------------------------

use crate::traits::{RotoFilter, MethodProps, TokenConvert};

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
            "match" => Ok(MethodProps::new(
                TypeValue::Record(self.record),
                RibToken::Match.into_u8(),
                vec![TypeDef::Prefix]
            )),
            "longest_match" => Ok(MethodProps::new(
                TypeValue::Record(self.record),
                RibToken::LongestMatch.into_u8(),
                vec![TypeDef::Prefix]
            )),
            "contains" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(None))),
                RibToken::Contains.into_u8(),
                vec![TypeDef::Prefix]
            )),
            _ => {
                Err(format!("Unknown method '{}' for data source", method_name.ident).into())
            }
        }
    }

    fn into_type(self, _type_def: &TypeDef) -> Result<TypeValue, Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized,
    {
        Err("Rib type cannot be converted into another type".into())
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

impl TokenConvert for RibToken {}

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
            "get" => Ok(MethodProps::new(
                TypeValue::Record(self.record),
                TableToken::Get.into_u8(),
                vec![TypeDef::Prefix]
            )),
            "contains" => Ok(MethodProps::new(
                TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(None))),
                TableToken::Contains.into_u8(),
                vec![TypeDef::Asn]
            )), 
            _ => {
                Err(format!("Unknown method '{}' for table", method_name.ident).into())
            }
        }
    }

    fn into_type(self, _type_def: &TypeDef) -> Result<TypeValue, Box<dyn std::error::Error>>
        where
            Self: std::marker::Sized {
        Err("Table type cannot be converted into another type".into())
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

impl TokenConvert for TableToken {}

impl std::fmt::Display for Table {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Table with record type {}", self.record)
    }
}
