use log::trace;
use serde::Serialize;

use crate::{
    ast::ShortString,
    compile::CompileError,
    traits::RotoType,
    types::{
        typedef::{MethodProps, TypeDef},
        typevalue::TypeValue,
    },
    vm::{StackValue, VmError},
};

use super::collections::Record;

impl RotoType for OutputStreamMessage {
    fn get_props_for_method(
        ty: TypeDef,
        method_name: &crate::ast::Identifier,
    ) -> Result<MethodProps, CompileError> {
        trace!("GET PROPS FOR METHOD ON OUTPUTSTREAMMESSAGE");
        match method_name.ident.as_str() {
            "send" => Ok(MethodProps::new(
                ty.clone(),
                OutputStreamToken::Send.into(),
                vec![ty],
            )
            .consume_value()),
            _ => Err(format!(
                "Unknown method: '{}' for type OutputStreamMessage",
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
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        method_token: usize,
        mut args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        match method_token.into() {
            OutputStreamToken::Send => {
                trace!("Send: args for output stream message {}", args[0]);
                Ok(args.remove(0))
            }
        }
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[StackValue],
        _res_type: TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

impl std::fmt::Display for OutputStreamMessage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Output stream record {:#?}", self.record)
    }
}

#[derive(Debug, Clone)]
pub(crate) enum OutputStreamToken {
    Send,
}

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

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
pub struct OutputStreamMessage {
    name: ShortString,
    topic: String,
    record: TypeValue,
}

impl OutputStreamMessage {
    // pub fn new(name: ShortString, topic: String, record: TypeValue) -> Self {
    //     Self {
    //         name, topic, record
    //     }
    // }

    pub fn get_name(&self) -> ShortString {
        self.name.clone()
    }

    pub fn get_topic(&self) -> &String {
        &self.topic
    }

    pub fn get_record(&self) -> &TypeValue {
        &self.record
    }
}

impl From<OutputStreamMessage> for TypeValue {
    fn from(msg: OutputStreamMessage) -> Self {
        TypeValue::OutputStreamMessage(msg.into())
    }
}

// Temporary solution. Uses the From<&ElementTypeValue> impl for (Short)-
// String. Probably tokenization for topic at the very least makes more
// sense performance wise and logically.
impl From<Record> for OutputStreamMessage {
    fn from(value: Record) -> Self {
        trace!("CONVERT INTO OUTPUTSTREAMMESSAGE");
        trace!("{}", value);
        let name: ShortString = value
            .get_value_for_field("name")
            .map(|v| v.into())
            .unwrap_or_else(|| "".into());
        let topic: String = value
            .get_value_for_field("topic")
            .map(|v| v.into())
            .unwrap_or_else(|| "".into());

        Self {
            name,
            topic,
            record: value.into(),
        }
    }
}
