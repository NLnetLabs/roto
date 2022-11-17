// =========== RotoFilter trait ============================================

use crate::types::{typedef::TypeDef, typevalue::TypeValue};

#[derive(Debug, Clone)]
pub(crate) enum Token {
    Variable(u8),
    Method(u8),
    Argument(u8),
    DataSource(u8),
    FieldAccess(Vec<u8>),
    Constant,
    BuiltinType(u8)
}

impl Token {
    pub fn new(ty: &str, value: u8) -> Self {
        match ty {
            "variable" => Token::Variable(value),
            "method" => Token::Method(value),
            "argument" => Token::Argument(value),
            _ => panic!("Unknown token type")
        }
    }

    pub fn push(&mut self, value: u8) {
        match self {
            Token::FieldAccess(v) => v.push(value),
            _ => panic!("Cannot push to this token")
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
    pub(crate) fn new(return_type_value: TypeValue, method_token: u8, arg_types: Vec<TypeDef>) -> Self {
        MethodProps {
            return_type_value,
            method_token: Token::Method(method_token),
            arg_types,
        }
    }
}

pub(crate) trait RotoFilter<T: TokenConvert> {

    fn get_props_for_method(
        self,
        method_name: &super::ast::Identifier,
    ) -> Result<MethodProps, Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized;

    fn into_type(self, type_value: &TypeDef) -> Result<TypeValue, Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized;

    fn exec_method<'a>(
        &'a self,
        method_token: T,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> Result<
        Box<dyn FnOnce(TypeValue) -> TypeValue + 'a>,
        Box<dyn std::error::Error>,
    >;
}

pub(crate) trait TokenConvert {
    fn into_u8(&self) -> u8 {
        std::mem::size_of_val(self) as u8
    }
}
