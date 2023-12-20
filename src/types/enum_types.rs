use std::fmt::{Debug, Display};
use std::str::FromStr;

use log::trace;
use routecore::bgp::communities::{Wellknown, Community, StandardCommunity};
use routecore::bgp::types::{Afi, Safi};
use routecore::bmp::message::MessageType;
use serde::Serialize;

use crate::compiler::compile::CompileError;
use crate::types::builtin::BytesRecord;
use crate::types::lazyrecord_types::BmpMessage;
use crate::vm::VmError;
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
pub struct EnumVariant<T> where u32: From<T> {
    pub(crate) enum_name: ShortString,
    pub(crate) value: T,
}

impl<T> Display for EnumVariant<T> where u32: From<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.enum_name)
    }
}

impl<T: Copy> EnumVariant<T> where u32: From<T> {
    pub fn new(enum_tv: (ShortString, T)) -> Self {
        Self {
            enum_name: enum_tv.0,
            value: enum_tv.1,
        }
    }
}

//------------ EnumVariant---------------------------------------------------

impl<T: Copy + Debug> RotoType for EnumVariant<T>
where
    BuiltinTypeValue: From<EnumVariant<T>>,
    u32: From<T>
{
    fn get_props_for_method(
        _ty: super::typedef::TypeDef,
        _method_name: &crate::ast::Identifier,
    ) -> Result<super::typedef::MethodProps, CompileError>
    where
        Self: std::marker::Sized,
    {
        todo!()
    }

    fn into_type(
        self,
        type_def: &super::typedef::TypeDef,
    ) -> Result<super::typevalue::TypeValue, CompileError>
    where
        Self: std::marker::Sized,
    {
        trace!("CONVERT ENUM");
        match type_def {
            TypeDef::ConstEnumVariant(_) => Ok(self.into()),
            TypeDef::U32 => Ok(u32::from(self.value).into()),
            TypeDef::Community => {
                Ok(Community::Standard(StandardCommunity::from(<u32>::from(self.value))).into())
            },
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
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_consume_value_method(
        self,
        _method_token: usize,
        _args: Vec<super::typevalue::TypeValue>,
        _res_type: super::typedef::TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }

    fn exec_type_method(
        _method_token: usize,
        _args: &[crate::vm::StackValue],
        _res_type: super::typedef::TypeDef,
    ) -> Result<TypeValue, VmError> {
        todo!()
    }
}

impl<T: Copy + Debug> From<EnumVariant<T>> for TypeValue
where
    BuiltinTypeValue: From<EnumVariant<T>>,
    u32: From<T>
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

// Every possible enum type that can is available pre-defined should be
// registered here. This is used by the evaluator to figure out if the
// name of the enum type, what variants it has, etc. It's not possible
// for users to create enums currently (there's no syntax!).
#[derive(
    Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Serialize,
)]
pub enum GlobalEnumTypeDef {
    Afi,
    Safi,
    WellKnownCommunities,
    BmpMessageType,
}

impl GlobalEnumTypeDef {
    fn iter<'a>() -> impl Iterator<Item = &'a GlobalEnumTypeDef> + 'a {
        ([
            GlobalEnumTypeDef::Afi,
            GlobalEnumTypeDef::Safi,
            GlobalEnumTypeDef::WellKnownCommunities,
            GlobalEnumTypeDef::BmpMessageType,
        ])
        .iter()
    }

    // This method checks if the supplied variant exists as the name of a
    // GlobalEnum and returns it as Ok(GlobalEnumTypeDef) OR it returns a vec
    // with all the GlobalEnumTypeDefs that contain it as the name of a
    // variant, in the form of Err(Vec<GlobalEnumTypeDef>). If the Vec inside
    // the Err is empty, than the variant name wasn't found at all. The outer
    // result will return an Err if for some reason the name of the variant cannot be p
    pub(crate) fn get_enum_for_variant_as_token(
        variant: &str,
    ) -> Result<GlobalEnumTypeDef, Vec<&GlobalEnumTypeDef>> {
        trace!("find enum for variant {}", variant);
        (&ShortString::from(variant)).try_into().map_err(|_e| {
            Self::iter()
                .filter(|t| {
                    ShortString::from(*t) == variant
                        || t.get_value_for_variant(variant).is_ok()
                })
                .collect::<Vec<_>>()
        })
    }

    // This is the equivalent for a Lazy Enum of the `get_props_for_field`
    // method of a Lazy Record.
    pub(crate) fn get_props_for_variant(
        &self,
        field: &crate::ast::Identifier,
    ) -> Result<(TypeDef, Token), CompileError> {
        match self {
            GlobalEnumTypeDef::BmpMessageType => {
                trace!("BmpMessage w/ variant '{}'", field);
                BytesRecord::<BmpMessage>::get_props_for_variant(field)
            }
            _ => Err(CompileError::from(format!("{}", self))),
        }
    }

    // This method returns the data field of a Variant of the GlobalEnum that
    // is represented by self.
    pub(crate) fn get_value_for_variant(
        &self,
        variant: &str,
    ) -> Result<BuiltinTypeValue, AccessReceiverError> {
        trace!("get_value_for_variant {}", variant);

        match self {
            GlobalEnumTypeDef::Afi => {
                match variant {
                    "IPV4" => Ok(BuiltinTypeValue::ConstU16EnumVariant(
                        EnumVariant::<u16> {
                            enum_name: self.into(),
                            value: Afi::Ipv4.into(),
                        },
                    )),
                    "IPV6" => Ok(BuiltinTypeValue::ConstU16EnumVariant(
                        EnumVariant::<u16> {
                            enum_name: self.into(),
                            value: Afi::Ipv6.into(),
                        },
                    )),
                    _ => Err(AccessReceiverError::Global),
                }
            }
            GlobalEnumTypeDef::Safi => {
                match variant {
                    "UNICAST" => Ok(BuiltinTypeValue::ConstU8EnumVariant(
                        EnumVariant::<u8> {
                            enum_name: self.into(),
                            value: Safi::Unicast.into(),
                        },
                    )),
                    "MULTICAST" => Ok(BuiltinTypeValue::ConstU8EnumVariant(
                        EnumVariant::<u8> {
                            enum_name: self.into(),
                            value: Safi::Multicast.into(),
                        },
                    )),
                    _ => Err(AccessReceiverError::Global),
                }
            }
            GlobalEnumTypeDef::WellKnownCommunities => Ok(
                BuiltinTypeValue::ConstU32EnumVariant(EnumVariant::<u32> {
                    enum_name: self.into(),
                    value: Wellknown::from_str(variant)
                        .map_err(|_| AccessReceiverError::Arg)?
                        .to_u32(),
                }),
            ),
            GlobalEnumTypeDef::BmpMessageType => match variant {
                "ROUTE_MONITORING" => {
                    Ok(BuiltinTypeValue::ConstU8EnumVariant(EnumVariant {
                        enum_name: self.into(),
                        value: MessageType::RouteMonitoring.into(),
                    }))
                }
                "STATISTICS_REPORT" => {
                    Ok(BuiltinTypeValue::ConstU8EnumVariant(EnumVariant {
                        enum_name: self.into(),
                        value: MessageType::StatisticsReport.into(),
                    }))
                }
                "PEER_DOWN_NOTIFICATION" => {
                    Ok(BuiltinTypeValue::ConstU8EnumVariant(EnumVariant {
                        enum_name: self.into(),
                        value: MessageType::PeerDownNotification.into(),
                    }))
                }
                "PEER_UP_NOTIFICATION" => {
                    Ok(BuiltinTypeValue::ConstU8EnumVariant(EnumVariant {
                        enum_name: self.into(),
                        value: MessageType::PeerUpNotification.into(),
                    }))
                }
                "INITATION_MESSAGE" => {
                    Ok(BuiltinTypeValue::ConstU8EnumVariant(EnumVariant {
                        enum_name: self.into(),
                        value: MessageType::InitiationMessage.into(),
                    }))
                }
                "TERMINATION_MESSAGE" => {
                    Ok(BuiltinTypeValue::ConstU8EnumVariant(EnumVariant {
                        enum_name: self.into(),
                        value: MessageType::TerminationMessage.into(),
                    }))
                }
                "ROUTE_MIRRORING" => {
                    Ok(BuiltinTypeValue::ConstU8EnumVariant(EnumVariant {
                        enum_name: self.into(),
                        value: MessageType::RouteMirroring.into(),
                    }))
                }
                _ => Err(AccessReceiverError::Global),
            },
        }
    }

    // This method checks if any variant of any enum of any global enum itself
    // has the name `variant`, if so it creates and returns a symbol, that
    // may have some arguments.
    pub(crate) fn any_variant_as_symbol(
        variant: &str,
    ) -> Result<symbols::Symbol, AccessReceiverError> {
        trace!("search enum for variant {}", variant);
        let enum_token =
            GlobalEnumTypeDef::get_enum_for_variant_as_token(variant);

        trace!("found enum {:?}", enum_token);
        let args = match enum_token {
            // The supplied variant is NOT a global enum name, AND it is NOT
            // a name of a variant. We're done.
            Err(t) if t.is_empty() => {
                return Err(AccessReceiverError::Global);
            }
            // The supplied variant is the name of a variant of a global enum
            // let's create a vec of symbols, one symbol for each encountered
            // variant. The caller should decide which one it needs.
            Err(t) => t
                .iter()
                .filter_map(|t| {
                    t.get_value_for_variant(variant)
                        .map(|tv| {
                            symbols::Symbol::new_with_value(
                                ShortString::from(*t),
                                symbols::SymbolKind::AccessReceiver,
                                TypeValue::Builtin(tv),
                                vec![],
                                Token::ConstEnumVariant,
                            )
                        })
                        .ok()
                })
                .collect::<Vec<_>>(),
            // The supplied variant is the name of a Global Enum, we will
            // create a Symbol for, without args. The caller will have to
            // fill in the `args` field.
            Ok(t) => {
                return Ok(symbols::Symbol::new(
                    t.into(),
                    symbols::SymbolKind::AccessReceiver,
                    TypeDef::GlobalEnum(t),
                    vec![],
                    Token::Enum(t),
                ));
            }
        };

        match args {
            args if args.is_empty() => Err(AccessReceiverError::Global),
            mut args if args.len() == 1 => Ok(args.remove(0)),
            _ => Ok(symbols::Symbol::new(
                "anonymous_enum".into(),
                symbols::SymbolKind::AccessReceiver,
                TypeDef::Unknown,
                args,
                Token::AnonymousEnum,
            )),
        }
    }
}

impl std::fmt::Display for GlobalEnumTypeDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GlobalEnumTypeDef::Afi => write!(f, "AFI"),
            GlobalEnumTypeDef::Safi => write!(f, "SAFI"),
            GlobalEnumTypeDef::WellKnownCommunities => {
                write!(f, "WELL_KNOWN_COMMUNITIES")
            }
            GlobalEnumTypeDef::BmpMessageType => {
                write!(f, "BMP_MESSAGE_TYPE")
            }
        }
    }
}

impl From<&GlobalEnumTypeDef> for ShortString {
    fn from(value: &GlobalEnumTypeDef) -> Self {
        match value {
            GlobalEnumTypeDef::Afi => ShortString::from("AFI"),
            GlobalEnumTypeDef::Safi => ShortString::from("SAFI"),
            GlobalEnumTypeDef::WellKnownCommunities => {
                ShortString::from("WELL_KNOWN_COMMUNITIES")
            }
            GlobalEnumTypeDef::BmpMessageType => {
                ShortString::from("BMP_MESSAGE_TYPE")
            }
        }
    }
}

impl From<GlobalEnumTypeDef> for ShortString {
    fn from(value: GlobalEnumTypeDef) -> Self {
        match value {
            GlobalEnumTypeDef::Afi => ShortString::from("AFI"),
            GlobalEnumTypeDef::Safi => ShortString::from("SAFI"),
            GlobalEnumTypeDef::WellKnownCommunities => {
                ShortString::from("WELL_KNOWN_COMMUNITIES")
            }
            GlobalEnumTypeDef::BmpMessageType => {
                ShortString::from("BMP_MESSAGE_TYPE")
            }
        }
    }
}

impl TryFrom<&ShortString> for GlobalEnumTypeDef {
    type Error = CompileError;
    fn try_from(value: &ShortString) -> Result<Self, CompileError> {
        match value.as_str() {
            "AFI" => Ok(GlobalEnumTypeDef::Afi),
            "SAFI" => Ok(GlobalEnumTypeDef::Safi),
            "WELL_KNOWN_COMMUNITIES" => {
                Ok(GlobalEnumTypeDef::WellKnownCommunities)
            }
            "BMP_MESSAGE_TYPE" => Ok(GlobalEnumTypeDef::BmpMessageType),
            _ => Err(CompileError::from(format!(
                "Unknown variant name {} in global enums",
                value
            ))),
        }
    }
}

impl FromStr for GlobalEnumTypeDef {
    type Err = CompileError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "AFI" => Ok(GlobalEnumTypeDef::Afi),
            "SAFI" => Ok(GlobalEnumTypeDef::Safi),
            "WELL_KNOWN_COMMUNITIES" => {
                Ok(GlobalEnumTypeDef::WellKnownCommunities)
            }
            "BMP_MESSAGE_TYPE" => Ok(GlobalEnumTypeDef::BmpMessageType),
            _ => Err(CompileError::from(format!(
                "Unknown variant name {} in global enums",
                s
            ))),
        }
    }
}

impl TryFrom<&str> for GlobalEnumTypeDef {
    type Error = CompileError;
    fn try_from(value: &str) -> Result<Self, CompileError> {
        match value {
            "AFI" => Ok(GlobalEnumTypeDef::Afi),
            "SAFI" => Ok(GlobalEnumTypeDef::Safi),
            "WELL_KNOWN_COMMUNITIES" => {
                Ok(GlobalEnumTypeDef::WellKnownCommunities)
            }
            "BMP_MESSAGE_TYPE" => Ok(GlobalEnumTypeDef::BmpMessageType),
            _ => Err(CompileError::from(format!(
                "Unknown variant name {} in global enums",
                value
            ))),
        }
    }
}

impl From<GlobalEnumTypeDef> for TypeDef {
    fn from(value: GlobalEnumTypeDef) -> Self {
        match value {
            GlobalEnumTypeDef::Afi => TypeDef::U16,
            GlobalEnumTypeDef::Safi => TypeDef::U8,
            GlobalEnumTypeDef::WellKnownCommunities => TypeDef::U8,
            GlobalEnumTypeDef::BmpMessageType => TypeDef::LazyRecord(
                super::lazyrecord_types::LazyRecordTypeDef::RouteMirroring,
            ),
        }
    }
}
