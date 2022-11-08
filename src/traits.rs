// =========== RotoFilter trait ============================================

use crate::types::{typedef::TypeDef, typevalue::TypeValue};

pub(crate) struct MethodProps {
    pub(crate) return_type_value: TypeValue,
    pub(crate) method_token: u8,
    pub(crate) arg_types: Vec<TypeValue>,
}

impl MethodProps {
    pub(crate) fn new(return_type_value: TypeValue, method_token: u8, arg_types: Vec<TypeValue>) -> Self {
        MethodProps {
            return_type_value,
            method_token,
            arg_types,
        }
    }
}

pub(crate) trait RotoFilter<Token> {
    fn get_props_for_method(
        self,
        method_name: &super::ast::Identifier,
    ) -> Result<MethodProps, Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized;

    fn exec_method<'a>(
        &'a self,
        method_token: Token,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> Result<
        Box<dyn FnOnce(TypeValue) -> TypeValue + 'a>,
        Box<dyn std::error::Error>,
    >;
}
