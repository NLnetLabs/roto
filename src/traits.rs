// =========== RotoFilter trait ============================================

use crate::{types::{typedef::TypeDef, typevalue::TypeValue}, symbols::Symbol};

pub(crate) struct Token(u8);

impl Token {
    pub fn new(value: Token) -> Self {
        Token(std::mem::size_of_val(&Token) as u8)
    }
}

#[derive(Debug)]
pub(crate) struct MethodProps {
    pub(crate) return_type_value: TypeValue,
    pub(crate) method_token: u8,
    pub(crate) arg_types: Vec<TypeDef>,
}

impl MethodProps {
    pub(crate) fn new(return_type_value: TypeValue, method_token: u8, arg_types: Vec<TypeDef>) -> Self {
        MethodProps {
            return_type_value,
            method_token,
            arg_types,
        }
    }
}

pub(crate) trait RotoFilter<T> {
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
