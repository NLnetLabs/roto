//------------ Global Methods type ----------------------------------------------

use crate::{
    compiler::compile::CompileError,
    traits::RotoType,
    types::{
        typedef::{MethodProps, TypeDef},
        typevalue::TypeValue,
    },
    vm::{StackValue, VmError},
};

#[derive(Debug, Eq, PartialEq, Copy, Clone, Default)]
pub struct GlobalMethods;
impl GlobalMethods {
    pub fn new() -> Self {
        GlobalMethods
    }
}

impl RotoType for GlobalMethods {
    fn get_props_for_method(
        ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError> {
        match method_name.ident.as_str() {
            "format" => Ok(MethodProps::new(
                ty.clone(),
                GlobalMethodsToken::Send.into(),
                vec![ty],
            )),
            _ => Err(format!(
                "Unknown method: '{}' for type Prefix",
                method_name.ident
            )
            .into()),
        }
    }

    fn into_type(
        self,
        _type_def: &TypeDef,
    ) -> Result<TypeValue, CompileError> {
        unimplemented!();
    }

    fn exec_value_method(
        &self,
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

impl std::fmt::Display for GlobalMethods {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Global Methods")
    }
}

#[derive(Debug)]
pub(crate) enum GlobalMethodsToken {
    Send,
}

impl From<GlobalMethodsToken> for usize {
    fn from(val: GlobalMethodsToken) -> Self {
        val as usize
    }
}

impl From<GlobalMethods> for TypeValue {
    fn from(_value: GlobalMethods) -> Self {
        unimplemented!()
    }
}
