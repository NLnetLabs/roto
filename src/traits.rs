// =========== RotoFilter trait ============================================

use crate::types::{typedef::TypeDef, typevalue::TypeValue};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub(crate) enum Token {
    Variable(u8),
    Method(u8),
    Argument(usize),
    // There can only ever be one RxType
    RxType,
    // There can only ever be one TxType too
    TxType,
    DataSource(u8),
    FieldAccess(Vec<u8>),
    Term(u8),
    Action(u8),
    MatchAction(u8),
    Constant,
    BuiltinType(u8),
}

impl Token {
    pub fn new(ty: &str, value: u8) -> Self {
        match ty {
            "variable" => Token::Variable(value),
            "method" => Token::Method(value),
            "argument" => Token::Argument(value as usize),
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
        matches!(self, Token::DataSource(_))
    }
}

impl From<Token> for usize {
    fn from(token: Token) -> Self {
        match token {
            Token::Argument(v) => v,
            Token::DataSource(v) => v as usize,
            Token::Method(v) => v as usize,
            Token::RxType => 0,
            Token::TxType => 1,
            _ => {
                println!("Cannot convert to usize: {:?}", token);
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
        method_token: u8,
        arg_types: Vec<TypeDef>,
    ) -> Self {
        MethodProps {
            return_type_value,
            method_token: Token::Method(method_token),
            arg_types,
        }
    }
}

pub(crate) trait RotoFilter<T: TokenConvert> where Self: std::fmt::Debug {
    fn get_props_for_method(
        self,
        method_name: &super::ast::Identifier,
    ) -> Result<MethodProps, Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized;

    fn into_type(
        self,
        type_value: &TypeDef,
    ) -> Result<TypeValue, Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized;

    fn exec_method<'a>(
        &'a self,
        method_token: usize,
        args: Vec<&'a TypeValue>,
        res_type: TypeDef,
    ) -> Result<
        Box<dyn FnOnce(TypeValue) -> TypeValue + 'a>,
        Box<dyn std::error::Error>,
    >;

    fn exec_type_method<'a>(
        method_token: usize,
        args: Vec<&'a TypeValue>,
        res_type: TypeDef,
    ) -> Result<
        Box<dyn FnOnce() -> TypeValue + 'a>,
        Box<dyn std::error::Error>,
    >;

    fn get_field_by_index(
        self,
        field_index: usize,
    ) -> Result<TypeValue, Box<dyn std::error::Error>>;
}

pub(crate) trait TokenConvert where Self: std::fmt::Debug + Sized {
    fn to_u8(&self) -> u8 {
        std::mem::size_of_val(self) as u8
    }
}
