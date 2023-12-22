// =========== Data source Types ===========================================

pub enum DataSource {
    Table(Table),
    Rib(Arc<dyn RotoRib>),
}

impl DataSource {
    pub fn table_from_records(
        name: &str,
        records: Vec<Record>,
    ) -> Result<Self, VmError> {
        match records.get(0) {
            Some(rec) => {
                let ty = TypeDef::from(&TypeValue::Record(rec.clone()));
                Ok(Self::Table(Table {
                    name: name.into(),
                    ty,
                    records,
                }))
            }
            None => Err(VmError::DataSourceEmpty(name.into())),
        }
    }

    pub fn rib_from_prefix_store<M: Meta + 'static>(
        name: &str,
        ty: TypeDef,
        store: rotonda_store::MultiThreadedStore<M>,
    ) -> Result<Self, VmError> {
        let rib = Rib::new(name, ty, store);
        Ok(DataSource::Rib(Arc::new(rib)))
    }

    pub fn get_at_field_index(
        &self,
        index: usize,
        field_index: FieldIndex,
    ) -> Option<&TypeValue> {
        match self {
            DataSource::Table(ref t) => {
                t.get_at_field_index(index, field_index).ok()
            }
            DataSource::Rib(ref _r) => {
                todo!()
            }
        }
    }

    // methods on a data source can indicate whether they are returning a
    // value created by the method or a reference to a value in the data
    // source itself, through the TableMethodValue enum.
    pub(crate) fn exec_method(
        &self,
        method_token: usize,
        args: &[StackValue],
        res_type: TypeDef,
    ) -> Result<DataSourceMethodValue, VmError> {
        match self {
            DataSource::Table(t) => {
                Ok(t.exec_ref_value_method(method_token, args, res_type)?())
            }
            DataSource::Rib(ref r) => {
                r.exec_ref_value_method(method_token, args, res_type)
            }
        }
    }

    pub fn get_name(&self) -> ShortString {
        match &self {
            DataSource::Table(t) => t.name.clone(),
            DataSource::Rib(r) => (**r).get_name(),
        }
    }

    pub fn get_type(&self) -> TypeDef {
        match &self {
            DataSource::Table(t) => t.ty.clone(),
            DataSource::Rib(r) => r.get_type(),
        }
    }

    pub fn is_empty(&self) -> bool {
        match &self {
            DataSource::Table(t) => t.records.is_empty(),
            DataSource::Rib(r) => r.is_empty(),
        }
    }
}

impl<M: Meta + 'static> From<Rib<M>> for DataSource {
    fn from(rib: Rib<M>) -> Self {
        DataSource::Rib(Arc::new(rib))
    }
}

// ----------- Rib Type ----------------------------------------------------

use std::sync::Arc;

use log::{error, trace};
use rotonda_store::{epoch, prelude::Meta, MatchOptions, MatchType};

use crate::{
    ast::ShortString,
    compiler::compile::CompileError,
    traits::{RotoRib, Token},
    vm::{StackRefPos, StackValue, VmError, FieldIndex}, first_into_vm_err,
};

use super::{
    builtin::BuiltinTypeValue,
    collections::Record,
    typedef::{MethodProps, TypeDef},
    typevalue::TypeValue,
};

// This data-structure only exists to populate the static methods for the type
// `Rib`, e.g. the methods `Rib::method_name()` and their properties.
#[derive(Debug)]
pub struct RibType {
    pub(crate) ty: TypeDef,
}

impl RibType {
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
                TypeDef::Bool,
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

    pub(crate) fn exec_type_method(
        _method: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
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

impl TryFrom<usize> for RibToken {
    type Error = VmError;

    fn try_from(token: usize) -> Result<Self, VmError> {
        match token {
            0 => Ok(RibToken::Match),
            1 => Ok(RibToken::LongestMatch),
            2 => Ok(RibToken::Get),
            3 => Ok(RibToken::Contains),
            _ => Err(VmError::InvalidDataSource)
        }
    }
}

impl From<RibToken> for usize {
    fn from(t: RibToken) -> usize {
        t as usize
    }
}

// Wrapper around a prefix store so that it can be used by Roto as an
// external source of data. See [DataSource] for further details of
// how it gets shared.
pub struct Rib<M: Meta> {
    // The definition of the Roto type as exposed to Roto scripts.
    // Doesn't necessarily have to be the Meta-data type that is stored in
    // the prefix store.
    pub name: ShortString,
    pub ty: TypeDef,
    pub store: rotonda_store::MultiThreadedStore<M>,
}

impl<M: Meta> Rib<M> {
    pub fn new(
        name: &str,
        ty: TypeDef,
        store: rotonda_store::MultiThreadedStore<M>,
    ) -> Self {
        Self {
            name: name.into(),
            ty,
            store,
        }
    }
}

impl<M: Meta> RotoRib for Rib<M> {
    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_ref_value_method<'a>(
        &'a self,
        method: usize,
        args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<DataSourceMethodValue, VmError> {
        match RibToken::try_from(method)? {
            RibToken::Match => {
                todo!()
            }
            RibToken::LongestMatch if args.len() == 1 => {
                trace!("longest match on rib");
                trace!("args {:?}", args);
                let guard = epoch::pin();
                let prefix = if let Ok(prefix) =
                    routecore::addr::Prefix::try_from(first_into_vm_err!(args, InvalidMethodCall)?.as_ref())
                {
                    prefix
                } else {
                    error!("Cannot convert Argument '{:?}' to Prefix while executing Data Source method.", args[0]);
                    return Ok(DataSourceMethodValue::TypeValue(
                        TypeValue::Unknown,
                    ));
                };
                Ok(self.store
                    .match_prefix(
                        &prefix,
                        &MatchOptions {
                            match_type: MatchType::LongestMatch,
                            include_all_records: false,
                            include_less_specifics: false,
                            include_more_specifics: false,
                        },
                        &guard,
                    )
                    .prefix
                    .map(|v| DataSourceMethodValue::TypeValue(v.into()))
                    .unwrap_or_else(|| {
                        DataSourceMethodValue::TypeValue(TypeValue::Unknown)
                    })
                )
            }
            RibToken::Contains => {
                trace!("contains on rib");
                todo!()
            }
            RibToken::Get => {
                trace!("get on rib");
                todo!()
            }
            _ => Err(VmError::InvalidMethodCall)
        }
    }

    fn get_by_key<'a>(&'a self, _key: &str) -> Option<&'a Record> {
        todo!()
    }

    fn len(&self) -> usize {
        todo!()
    }

    fn is_empty(&self) -> bool {
        self.store.prefixes_count() == 0
    }

    fn get_type(&self) -> TypeDef {
        self.ty.clone()
    }

    fn get_name(&self) -> ShortString {
        self.name.clone()
    }
}

impl std::fmt::Display for RibType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Rib with record type {}", self.ty)
    }
}

// ----------- Table Type --------------------------------------------------

pub enum DataSourceMethodValue {
    Ref(StackRefPos),
    TypeValue(TypeValue),
    Empty(TypeDef),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Table {
    pub(crate) name: ShortString,
    pub(crate) ty: TypeDef,
    pub(crate) records: Vec<Record>,
}

impl Table {
    pub fn get_at_field_index(
        &self,
        index: usize,
        field_index: FieldIndex,
    ) -> Result<&TypeValue, VmError> {
        match field_index {
            fi if fi.is_empty() => if let Some(r) = self.records.get(index) {
                if let Some((_, ref v)) = r.get_field_by_single_index(index) {
                    v.try_into()
                } else { Err(VmError::InvalidFieldAccess) }
            } else { Err(VmError::InvalidFieldAccess) },
            field_index => match self.records.get(index) {
                Some(r) => {
                    if let Some(v) = r.get_field_by_index(&field_index) {
                        v.try_into()
                    } else { Err(VmError::InvalidFieldAccess) }
                }
                _ => Ok(&TypeValue::Unknown),
            },
        }
    }

    pub(crate) fn exec_ref_value_method<'a>(
        &'a self,
        method_token: usize,
        args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> DataSourceMethodValue + 'a>, VmError> {
        match method_token.try_into()? {
            TableToken::Find if args.len() == 1 => Ok(Box::new(|| {
                self.records
                    .iter()
                    .enumerate()
                    .find(|v| {
                        if let Some(val) =
                            v.1.get_field_by_index(&FieldIndex::from(vec![0]))
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
                        DataSourceMethodValue::TypeValue(TypeValue::Unknown)
                    })
            })),
            TableToken::Contains if args.len() == 1 => Ok(Box::new(|| {
                self.records
                    .iter()
                    .enumerate()
                    .find(|v| {
                        if let Some(val) =
                            v.1.get_field_by_index(&FieldIndex::from(vec![0]))
                        {
                            val == args[0]
                        } else {
                            false
                        }
                    })
                    .map(|_v| {
                        DataSourceMethodValue::TypeValue(TypeValue::Builtin(
                            BuiltinTypeValue::Bool(true),
                        ))
                    })
                    .unwrap_or_else(|| {
                        DataSourceMethodValue::TypeValue(TypeValue::Builtin(
                            BuiltinTypeValue::Bool(false),
                        ))
                    })
            })),
            _ => Err(VmError::InvalidMethodCall)
        }
    }

    pub(crate) fn get_props_for_method(
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
                TypeDef::Bool,
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

    pub(crate) fn exec_type_method(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        unimplemented!()
    }
}

#[derive(Debug)]
pub enum TableToken {
    Find,
    Contains,
}

impl TryFrom<usize> for TableToken {
    type Error = VmError;

    fn try_from(token: usize) -> Result<Self, VmError> {
        match token {
            0 => Ok(TableToken::Find),
            1 => Ok(TableToken::Contains),
            t => {
                error!("Cannot find method on Table for token: {}", t);
                Err(VmError::InvalidDataSourceAccess)
            }
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
