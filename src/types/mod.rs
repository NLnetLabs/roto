use self::typedef::TypeDef;
use crate::ast::ShortString;

pub mod builtin;
pub mod collections;
pub mod datasources;
pub mod typedef;
pub mod typevalue;

pub type NamedTypeDef = (ShortString, Box<TypeDef>);
