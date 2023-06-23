use std::fmt::{Debug, Display};
use std::str::FromStr;

use log::trace;
use routecore::bgp::communities::Wellknown;
use routecore::bgp::types::{AFI, SAFI};
use serde::Serialize;

use crate::{
    ast::ShortString,
    eval::AccessReceiverError,
    symbols,
    traits::{RotoType, Token},
};

use super::{
    builtin::BuiltinTypeValue, typedef::TypeDef, typevalue::TypeValue,
};

//------------ EnumVariant --------------------------------------------------

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize)]
pub struct EnumVariant<T> {
    pub(crate) enum_name: ShortString,
    pub(crate) value: T,
}

impl<T> Display for EnumVariant<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.enum_name)
    }
}

impl<T: Copy> EnumVariant<T> {
    fn _get_value(&self) -> T {
        self.value
    }
}

//------------ EnumVariant---------------------------------------------------

impl<T: Copy + Debug> RotoType for EnumVariant<T>
where
    BuiltinTypeValue: From<EnumVariant<T>>,
{
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
        type_def: &super::typedef::TypeDef,
    ) -> Result<super::typevalue::TypeValue, crate::compile::CompileError>
    where
        Self: std::marker::Sized,
    {
        match type_def {
            TypeDef::ConstEnumVariant(_) => {
                Ok(self.into())
            }
            _ => Err(format!(
                "Cannot convert type EnumVariant to type {:?}",
                type_def
            )
            .into()),
        }
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

impl<T: Copy + Debug> From<EnumVariant<T>> for TypeValue
where
    BuiltinTypeValue: From<EnumVariant<T>>,
{
    fn from(value: EnumVariant<T>) -> Self {
        TypeValue::Builtin(value.into())
    }
}

impl From<EnumVariant<u8>> for BuiltinTypeValue {
    fn from(value: EnumVariant<u8>) -> Self {
        BuiltinTypeValue::ConstU8EnumVariant(value)
    }
}

impl From<EnumVariant<u16>> for BuiltinTypeValue {
    fn from(value: EnumVariant<u16>) -> Self {
        BuiltinTypeValue::ConstU16EnumVariant(value)
    }
}

impl From<EnumVariant<u32>> for BuiltinTypeValue {
    fn from(value: EnumVariant<u32>) -> Self {
        BuiltinTypeValue::ConstU32EnumVariant(value)
    }
}

//------------ Enum ---------------------------------------------------------

#[derive(Debug, PartialEq, Eq, Clone, Hash, Serialize)]
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
        if let Ok(val) =
            match *enum_name {
                "AFI" => match var {
                    "IPV4" => Ok(BuiltinTypeValue::ConstU16EnumVariant(
                        EnumVariant::<u16> {
                            enum_name: (*enum_name).into(),
                            value: AFI::Ipv4.into(),
                        },
                    )),
                    "IPV6" => Ok(BuiltinTypeValue::ConstU16EnumVariant(
                        EnumVariant::<u16> {
                            enum_name: (*enum_name).into(),
                            value: AFI::Ipv6.into(),
                        },
                    )),
                    _ => Err(AccessReceiverError::Global),
                },
                "SAFI" => match var {
                    "UNICAST" => Ok(BuiltinTypeValue::ConstU8EnumVariant(
                        EnumVariant::<u8> {
                            enum_name: (*enum_name).into(),
                            value: SAFI::Unicast.into(),
                        },
                    )),
                    "MULTICAST" => Ok(BuiltinTypeValue::ConstU8EnumVariant(
                        EnumVariant::<u8> {
                            enum_name: (*enum_name).into(),
                            value: SAFI::Multicast.into(),
                        },
                    )),
                    _ => Err(AccessReceiverError::Global),
                },
                "WELL_KNOWN_COMMUNITIES" => {
                    Ok(BuiltinTypeValue::ConstU32EnumVariant(EnumVariant::<
                        u32,
                    > {
                        enum_name: (*enum_name).into(),
                        value: Wellknown::from_str(var)
                            .map_err(|_| AccessReceiverError::Arg)?
                            .to_u32(),
                    }))
                }
                _ => Err(AccessReceiverError::Global),
            }
        {
            args.push(symbols::Symbol::new_with_value(
                (*enum_name).into(),
                symbols::SymbolKind::AccessReceiver,
                TypeValue::Builtin(val.clone()),
                vec![],
                Token::ConstEnumVariant,
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
