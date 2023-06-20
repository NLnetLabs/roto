use std::fmt::{Debug, Display};

use log::trace;
use routecore::bgp::types::{AFI, SAFI};

use crate::{
    ast::ShortString,
    eval::AccessReceiverError,
    symbols,
    traits::{RotoType, Token},
};

use super::{
    builtin::BuiltinTypeValue, typedef::TypeDef, typevalue::TypeValue,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct EnumVariant<T> {
    pub(crate) enum_name: ShortString,
    pub(crate) value: T,
}

impl RotoType for EnumVariant<u16> {
    fn get_props_for_method(
        _ty: super::typedef::TypeDef,
        _method_name: &crate::ast::Identifier,
    ) -> Result<super::typedef::MethodProps, crate::compile::CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn into_type(
        self,
        _type_value: &super::typedef::TypeDef,
    ) -> Result<super::typevalue::TypeValue, crate::compile::CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &'a [crate::vm::StackValue],
        _res_type: super::typedef::TypeDef,
    ) -> Result<
        Box<dyn FnOnce() -> super::typevalue::TypeValue + 'a>,
        crate::vm::VmError,
    > {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<super::typevalue::TypeValue>,
        _res_type: super::typedef::TypeDef,
    ) -> Result<
        Box<dyn FnOnce() -> super::typevalue::TypeValue>,
        crate::vm::VmError,
    > {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[crate::vm::StackValue],
        _res_type: super::typedef::TypeDef,
    ) -> Result<
        Box<dyn FnOnce() -> super::typevalue::TypeValue + 'a>,
        crate::vm::VmError,
    > {
        todo!()
    }
}

impl From<EnumVariant<u16>> for TypeValue {
    fn from(value: EnumVariant<u16>) -> Self {
        TypeValue::Builtin(BuiltinTypeValue::EnumVariant(value))
    }
}

impl<T> Display for EnumVariant<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.enum_name)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Enum {
    ty: TypeDef,
    token: Token,
    pub(crate) variants: Vec<EnumVariant<u16>>,
}

impl Enum {
    pub fn get_type(&self) -> TypeDef {
        self.ty.clone()
    }

    pub fn find(&self, variant: ShortString) -> Option<&EnumVariant<u16>> {
        self.variants.iter().find(|var| var.enum_name == variant)
    }
}

impl From<Enum> for TypeValue {
    fn from(value: Enum) -> Self {
        TypeValue::Enum(value)
    }
}

impl RotoType for Enum {
    fn get_props_for_method(
        _ty: TypeDef,
        _method_name: &crate::ast::Identifier,
    ) -> Result<super::typedef::MethodProps, crate::compile::CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn into_type(
        self,
        _type_value: &TypeDef,
    ) -> Result<TypeValue, crate::compile::CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn exec_value_method<'a>(
        &'a self,
        _method_token: usize,
        _args: &'a [crate::vm::StackValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, crate::vm::VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue>, crate::vm::VmError> {
        todo!()
    }

    fn exec_type_method<'a>(
        _method_token: usize,
        _args: &[crate::vm::StackValue],
        _res_type: TypeDef,
    ) -> Result<Box<dyn FnOnce() -> TypeValue + 'a>, crate::vm::VmError> {
        todo!()
    }
}

impl Display for Enum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (i, var) in self.variants.iter().enumerate() {
            if i > 0 {
                write!(f, "| ")?;
            }
            write!(f, "{}", var)?;
        }
        write!(f, "\n   }}")
    }
}

pub(crate) fn global_enums(
    var: &str,
) -> Result<symbols::Symbol, AccessReceiverError> {
    trace!("var {}", var);
    let mut args = vec![];

    for enum_name in GLOBAL_ENUMS {
        if let Ok(val) = match *enum_name {
            "AFI" => match var {
                "IPV4" => Ok(u16::from(AFI::Ipv4)),
                "IPV6" => Ok(u16::from(AFI::Ipv6)),
                _ => Err(AccessReceiverError::Global),
            },
            "SAFI" => match var {
                "UNICAST" => Ok(u8::from(SAFI::Unicast) as u16),
                "MULTICAST" => Ok(u8::from(SAFI::Multicast) as u16),
                _ => Err(AccessReceiverError::Global),
            },
            _ => Err(AccessReceiverError::Global),
        } {
            args.push(symbols::Symbol::new_with_value(
                (*enum_name).into(),
                symbols::SymbolKind::AccessReceiver,
                TypeValue::Builtin(BuiltinTypeValue::EnumVariant(EnumVariant::<u16> { enum_name: (*enum_name).into(), value: val })),
                vec![],
                Token::EnumVariant(val.try_into().unwrap()),
            ))
        }
    }

    match args {
        args if args.is_empty() => Err(AccessReceiverError::Global),
        mut args if args.len() == 1 => Ok(args.remove(0)),
        _ => Ok(symbols::Symbol::new(
            "anonymous_enum".into(),
            symbols::SymbolKind::AccessReceiver,
            TypeDef::Enum(Box::new(TypeDef::Unknown)),
            args,
            Some(Token::Enum(0)),
        )),
    }
}

const GLOBAL_ENUMS: &[&str] = &["AFI", "SAFI"];
