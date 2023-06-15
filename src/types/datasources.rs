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
                Ok(Self::Table(Table { name: name.into(), ty, records }))
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
        field_index: SmallVec<[usize; 8]>,
    ) -> Option<&TypeValue> {
        match self {
            DataSource::Table(ref t) => {
                t.get_at_field_index(index, field_index)
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
    ) -> DataSourceMethodValue {
        match self {
            DataSource::Table(t) => {
                t.exec_ref_value_method(method_token, args, res_type)()
            }
            DataSource::Rib(ref r) => {
                r.exec_ref_value_method(method_token, args, res_type)
            }
        }
    }

    pub fn get_name(&self) -> ShortString {
        match &self {
            DataSource::Table(t) => t.name.clone(),
            DataSource::Rib(r) => (**r).get_name()
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

use log::trace;
use rotonda_store::{prelude::Meta, epoch, MatchOptions, MatchType};
use smallvec::SmallVec;

use crate::{
    ast::ShortString,
    compile::CompileError,
    traits::{Token, TokenConvert, RotoRib},
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

// Wrapper around a prefix store so that it can be used by Roto as an
// external source of data. See [DataSource] for further details of
// how it gets shared.
pub struct Rib<M: Meta> {
    // The definition of the Roto type as exposed to Roto scripts.
    // Doesn't necessarily have to be the Meta-data type that is stored in
    // the prefix store.
    pub name: ShortString,
    pub ty: TypeDef,
    pub store: rotonda_store::MultiThreadedStore<M>
}

impl<M: Meta> Rib<M> {
    pub fn new(name: &str, ty: TypeDef, store: rotonda_store::MultiThreadedStore<M>) -> Self {
        Self { name: name.into(), ty, store }
    }
}

impl<M: Meta> RotoRib for Rib<M> {
    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

    fn exec_ref_value_method<'a>(
        &'a self,
        method: usize,
        args: &'a [StackValue],
        _res_type: TypeDef,
    ) -> DataSourceMethodValue {
        match RibToken::from(method) {
            RibToken::Match => {
                todo!()
            }
            RibToken::LongestMatch => {
                trace!("longest match on rib");
                let guard = epoch::pin();
                self
                    .store
                    .match_prefix(
                        &routecore::addr::Prefix::try_from(args[0].as_ref())
                            .unwrap(),
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
            }
            RibToken::Contains => {
                trace!("contains on rib");
                todo!()
            }
            RibToken::Get => {
                trace!("get on rib");
                todo!()
            }
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

pub type NamedTypeDef = (ShortString, Box<TypeDef>);

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
        field_index: SmallVec<[usize; 8]>,
    ) -> Option<&TypeValue> {
        match field_index {
            fi if fi.is_empty() => self
                .records
                .get(index)
                .and_then(|r| r.0.get(index).map(|v| (&v.1).into())),
            field_index => match self.records.get(index) {
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
                        if let Some(val) = v.1.get_field_by_index(smallvec::smallvec![0]) {
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
                        if let Some(val) = v.1.get_field_by_index(smallvec::smallvec![0]) {
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

    pub(crate) fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
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
