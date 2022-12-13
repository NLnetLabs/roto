use crate::ast::ShortString;
use self::typedef::TypeDef;

pub mod builtin;
pub mod collections;
pub mod datasources;
pub mod typedef;
pub mod typevalue;

pub type NamedTypeDef = (ShortString, Box<TypeDef>);
