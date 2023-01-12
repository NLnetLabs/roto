// =========== RotoFilter trait ============================================

use crate::{types::{
    typedef::TypeDef, typevalue::TypeValue,
}, compile::CompileError};

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
    FieldAccess(Vec<u8>),
    Term(u8),
    Action(u8),
    MatchAction(u8),
    // None as data indicates a constant that wasn't stored (yet) in the
    // symbol table.
    Constant(Option<usize>),
    BuiltinType(u8),
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
                println!("Cannot convert {:?} to usize.", token);
                panic!("..and that's fatal");
            }
        }
    }
}

#[derive(Debug)]
pub(crate) struct MethodProps {
    pub(crate) return_type_value: TypeValue,
    pub(crate) method_token: Token,
    pub(crate) arg_types: Vec<TypeDef>,
}

impl MethodProps {
    pub(crate) fn new(
        return_type_value: TypeValue,
        method_token: usize,
        arg_types: Vec<TypeDef>,
    ) -> Self {
        MethodProps {
            return_type_value,
            method_token: Token::Method(method_token),
            arg_types,
        }
    }
}

pub(crate) trait RotoFilter<T: TokenConvert>
where
    Self: std::fmt::Debug,
{
    fn get_props_for_method(
        self,
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
        args: &'a [&'a TypeValue],
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, CompileError>;

    fn exec_type_method<'a>(
        method_token: usize,
        args: &[&'a TypeValue],
        res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, CompileError>;
}

pub(crate) trait TokenConvert
where
    Self: std::fmt::Debug + Sized,
{
    fn to_u8(&self) -> u8 {
        std::mem::size_of_val(self) as u8
    }
}
