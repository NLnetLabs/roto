//------------ Global Methods type ----------------------------------------------

use crate::{
    compile::CompileError,
    traits::{RotoType, TokenConvert},
    types::{
        typedef::{MethodProps, TypeDef},
        typevalue::TypeValue,
    },
    vm::VmError,
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

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &[&TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[&'a TypeValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
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

impl TokenConvert for GlobalMethods {}

impl From<usize> for GlobalMethodsToken {
    fn from(val: usize) -> Self {
        match val {
            0 => GlobalMethodsToken::Send,
            _ => panic!("Unknown token value: {}", val),
        }
    }
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
