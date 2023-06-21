// =========== RotoFilter trait ============================================

use crate::{
    ast::ShortString,
    compile::CompileError,
    types::{
        collections::Record,
        datasources::DataSourceMethodValue,
        typedef::{MethodProps, TypeDef},
        typevalue::TypeValue,
    },
    vm::{StackValue, VmError},
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum Token {
    Variable(usize),
    Method(usize),
    Argument(usize),
    // There can only ever be one RxType
    RxType,
    // There can only ever be one TxType too
    TxType,
    // External Data Sources
    Table(usize),
    Rib(usize),
    // A generic stream that can be used by Roto to send messages to and that
    // can be configured through a Roto script, e.g. a Kafka or a MQTT stream.
    OutputStream(usize),
    FieldAccess(Vec<u8>),
    Term(u8),
    Action(u8),
    MatchAction(u8),
    // None as data indicates a constant that wasn't stored (yet) in the
    // symbol table.
    Constant(Option<usize>),
    // An anonymous record, the fields live in the `args` vec of the
    // symbol.
    AnonymousRecord,
    TypedRecord,
    List,
    BuiltinType(u8),
    // Enum
    Enum(u32),
    ConstEnumVariant,
}

impl Token {
    pub fn new(ty: &str, value: usize) -> Self {
        match ty {
            "variable" => Token::Variable(value),
            "method" => Token::Method(value),
            "argument" => Token::Argument(value),
            _ => panic!("Unknown token type"),
        }
    }

    pub fn push(&mut self, value: u8) {
        match self {
            Token::FieldAccess(v) => v.push(value),
            _ => panic!("Cannot push to this token"),
        }
    }

    pub fn is_variable(&self) -> bool {
        matches!(self, Token::Variable(_))
    }

    pub fn is_argument(&self) -> bool {
        matches!(self, Token::Argument(_))
    }

    pub fn is_data_source(&self) -> bool {
        matches!(self, Token::Rib(_) | Token::Table(_))
    }
}

impl From<Token> for usize {
    fn from(token: Token) -> Self {
        match token {
            Token::Argument(v) => v,
            Token::Table(v) | Token::Rib(v) => v,
            Token::Method(v) => v,
            Token::Variable(v) => v,
            Token::RxType => 0,
            Token::TxType => 1,
            Token::Term(v) => v as usize,
            _ => {
                panic!(
                    "Cannot convert {:?} to usize, and that's fatal.",
                    token
                );
            }
        }
    }
}

pub trait RotoType: Into<TypeValue>
where
    Self: std::fmt::Debug + Sized,
{
    fn take_value(self) -> Self {
        self
    }

    fn get_props_for_method(
        ty: TypeDef,
        method_name: &super::ast::Identifier,
    ) -> Result<MethodProps, CompileError>
    where
        Self: std::marker::Sized;

    fn into_type(
        self,
        type_value: &TypeDef,
    ) -> Result<TypeValue, CompileError>
    where
        Self: std::marker::Sized;

    fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        args: &'a [StackValue],
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError>;

    fn exec_consume_value_method(
        self,
        method_token: usize,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError>;

    fn exec_type_method<'a>(
        method_token: usize,
        args: &[StackValue],
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError>;
}

pub(crate) trait TokenConvert
where
    Self: std::fmt::Debug + Sized,
{
    fn to_u8(&self) -> u8 {
        std::mem::size_of_val(self) as u8
    }
}

pub trait RotoRib {
    fn exec_value_method<'a>(
        &'a self,
        method_token: usize,
        args: &'a [StackValue],
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError>;

    fn exec_ref_value_method<'a>(
        &self,
        method_token: usize,
        args: &'a [StackValue],
        res_type: TypeDef,
    ) -> DataSourceMethodValue;

    fn get_by_key<'a>(&'a self, key: &str) -> Option<&'a Record>;

    fn len(&self) -> usize;

    fn is_empty(&self) -> bool;

    fn get_name(&self) -> ShortString;

    fn get_type(&self) -> TypeDef;
}
