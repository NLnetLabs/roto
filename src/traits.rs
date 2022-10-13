
// =========== RotoFilter trait ============================================

use crate::types::{typevalue::TypeValue, typedef::TypeDef};

pub(crate) trait RotoFilter<Token> {
    fn get_props_for_method(
        self,
        method_name: &super::ast::Identifier,
    ) -> Result<(u8, TypeValue), Box<dyn std::error::Error>>
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
