use crate::{
    ast::ShortString,
    compile::CompileError,
    traits::{RotoType, TokenConvert},
    types::{
        typedef::{MethodProps, TypeDef},
        typevalue::TypeValue,
    },
    vm::{VmError, StackValue},
};

impl RotoType for OutputStream {
    fn get_props_for_method(
        ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError> {
        match method_name.ident.as_str() {
            "send" => Ok(MethodProps::new(
                ty.clone(),
                OutputStreamToken::Send.into(),
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
        _args: &[StackValue],
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
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, VmError> {
        todo!()
    }
}

impl std::fmt::Display for OutputStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Output stream with record type {:#?}", self.record_type)
    }
}

#[derive(Debug)]
pub(crate) enum OutputStreamToken {
    Send,
}

impl TokenConvert for OutputStream {}

impl From<usize> for OutputStreamToken {
    fn from(val: usize) -> Self {
        match val {
            0 => OutputStreamToken::Send,
            _ => panic!("Unknown token value: {}", val),
        }
    }
}

impl From<OutputStreamToken> for usize {
    fn from(val: OutputStreamToken) -> Self {
        val as usize
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct OutputStream {
    pub(crate) name: ShortString,
    pub(crate) topic: String,
    pub(crate) record_type: TypeDef,
    pub(crate) record: TypeValue,
}

impl From<OutputStream> for TypeValue {
    fn from(_value: OutputStream) -> Self {
        unimplemented!()
    }
}
