//------------ TypeDef -----------------------------------------------------

// These are all the types the user can create. This enum is used to create
// `user defined` types.

use crate::{ast::{ShortString, AcceptReject}, traits::{RotoFilter, MethodProps}};
use crate::types::collections::ElementTypeValue;

use super::{
    builtin::BuiltinTypeValue,
    collections::List,
    typevalue::TypeValue,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypeDef {
    // Data Sources
    Rib(Box<TypeDef>),
    Table(Box<TypeDef>),
    // Collection Types
    List(Box<TypeDef>),
    Record(Vec<(ShortString, Box<TypeDef>)>),
    // Builtin Types
    U32,
    U8,
    Boolean,
    String, // used for fieldname in method calls
    Prefix,
    PrefixRecord, // A Record with a prefix as key
    PrefixLengthLiteral, // A u8 prefixes by a /
    IpAddress,
    Asn,
    AsnLiteral,
    AsPath,
    Community,
    Route,
    HexLiteral,
    IntegerLiteral,
    AcceptReject(AcceptReject), // used in the apply section
    None,
}

impl TypeDef {
    pub(crate) fn new_record_type_from_short_string(
        type_ident_pairs: Vec<(ShortString, Box<TypeDef>)>,
    ) -> Result<TypeDef, Box<dyn std::error::Error>> {
        Ok(TypeDef::Record(type_ident_pairs))
    }

    pub fn has_field(&self, field: &str) -> bool {
        match self {
            TypeDef::Record(fields) => {
                fields.iter().any(|(ident, _)| ident == &field)
            }
            _ => false,
        }
    }

    pub fn new_record_type(
        type_ident_pairs: Vec<(&str, Box<TypeDef>)>,
    ) -> Result<TypeDef, Box<dyn std::error::Error>> {
        Ok(TypeDef::Record(
            type_ident_pairs
                .iter()
                .map(|(k, v)| (ShortString::from(*k), v.clone()))
                .collect(),
        ))
    }

    pub fn has_fields_chain(
        &self,
        fields: &[crate::ast::Identifier],
    ) -> Result<TypeDef, Box<dyn std::error::Error>> {
        println!("has_fields_chain: {:?}", fields);
        println!("self: {:?}", self);
        let mut current_type = self;
        for field in fields {
            if let TypeDef::Record(_fields) = current_type {
                if let Some((_, ty)) = _fields
                    .iter()
                    .find(|(ident, _)| ident == &field.ident.as_str())
                {
                    current_type = ty;
                    println!("UwU {}", field.ident);
                } else {
                    println!("AA none fields:: {}", field.ident.as_str());
                    return Err(Box::new(std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        format!("No field named '{}'", field.ident.as_str()),
                    )));
                }
            } else {
                println!("BB none fields:: {}", field.ident.as_str());
                return Err(Box::new(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!("No field named '{}'", field.ident.as_str()),
                )));
            }
        }
        Ok(current_type.clone())
    }

    pub(crate) fn _check_record_fields(
        &self,
        fields: &[(ShortString, &TypeValue)],
    ) -> bool {
        if let TypeDef::Record(rec) = self {
            for (name, ty) in fields {
                if !rec.iter().any(|(k, v)| k == name && v.as_ref() == *ty) {
                    return false;
                }
            }
            true
        } else {
            false
        }
    }

    pub(crate) fn _check_record_fields_for_ref(
        &self,
        fields: &[(ShortString, TypeValue)],
    ) -> bool {
        if let TypeDef::Record(rec) = self {
            for (name, ty) in fields {
                if !rec.iter().any(|(k, v)| k == name && v.as_ref() == ty) {
                    return false;
                }
            }
            true
        } else {
            false
        }
    }

    pub(crate) fn get_props_for_method(
        &self,
        method: &crate::ast::Identifier,
    ) -> Result<MethodProps, Box<dyn std::error::Error>> {
        let parent_ty: TypeValue = self.into();
        match parent_ty {
            TypeValue::Record(rec_type) => {
                rec_type.get_props_for_method(method)
            }
            TypeValue::List(list) => list.get_props_for_method(method),
            TypeValue::Builtin(BuiltinTypeValue::AsPath(as_path)) => {
                as_path.get_props_for_method(method)
            }
            TypeValue::Builtin(BuiltinTypeValue::Prefix(prefix)) => {
                prefix.get_props_for_method(method)
            }
            TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(lit_int)) => {
                lit_int.get_props_for_method(method)
            }
            TypeValue::Builtin(BuiltinTypeValue::Asn(asn)) => {
                asn.get_props_for_method(method)
            }
            TypeValue::Rib(rib) => rib.get_props_for_method(method),
            TypeValue::Table(rec) => rec.get_props_for_method(method),
            _ => Err(format!(
                "No method named '{}' found for {}.",
                method.ident, parent_ty
            )
            .into()),
        }
    }
}

impl PartialEq<BuiltinTypeValue> for TypeDef {
    fn eq(&self, other: &BuiltinTypeValue) -> bool {
        match self {
            TypeDef::U32 => {
                matches!(other, BuiltinTypeValue::U32(_))
            }
            TypeDef::U8 => {
                matches!(other, BuiltinTypeValue::U8(_))
            }
            TypeDef::IntegerLiteral => {
                matches!(other, BuiltinTypeValue::IntegerLiteral(_))
            }
            TypeDef::Prefix => {
                matches!(other, BuiltinTypeValue::Prefix(_))
            }
            TypeDef::PrefixLengthLiteral => {
                matches!(other, BuiltinTypeValue::PrefixLengthLiteral(_))
            }
            TypeDef::IpAddress => {
                matches!(other, BuiltinTypeValue::IpAddress(_))
            }
            TypeDef::Asn => {
                matches!(other, BuiltinTypeValue::Asn(_))
            }
            TypeDef::AsPath => {
                matches!(other, BuiltinTypeValue::AsPath(_))
            }
            TypeDef::Community => {
                matches!(other, BuiltinTypeValue::Community(_))
            }
            _ => false,
        }
    }
}

impl PartialEq<TypeValue> for TypeDef {
    fn eq(&self, other: &TypeValue) -> bool {
        match (self, other) {
            (a, TypeValue::Builtin(b)) => a == b,
            (TypeDef::List(a), TypeValue::List(b)) => match (a.as_ref(), b) {
                (TypeDef::List(aa), List(bb)) => match &bb[0] {
                    ElementTypeValue::Nested(bb) => {
                        return aa.as_ref() == bb.as_ref()
                    }
                    ElementTypeValue::Primitive(bb) => {
                        return aa.as_ref() == bb
                    }
                },
                _ => false,
            },
            (TypeDef::Record(a), TypeValue::Record(_b)) => self
                ._check_record_fields_for_ref(
                    a.iter()
                        .map(|ty| (ty.0.clone(), ty.1.as_ref().into()))
                        .collect::<Vec<_>>()
                        .as_slice(),
                ),
            _ => false,
        }
    }
}

// This From impl creates the link between the AST and the TypeDef enum
// for built-in types.
impl TryFrom<crate::ast::TypeIdentifier> for TypeDef {
    type Error = Box<dyn std::error::Error>;
    fn try_from(
        ty: crate::ast::TypeIdentifier,
    ) -> Result<TypeDef, std::boxed::Box<dyn std::error::Error>> {
        match ty.ident.as_str() {
            "U32" => Ok(TypeDef::U32),
            "U8" => Ok(TypeDef::U8),
            "IntegerLiteral" => Ok(TypeDef::IntegerLiteral),
            "Prefix" => Ok(TypeDef::Prefix),
            "PrefixLengthLiteral" => Ok(TypeDef::PrefixLengthLiteral),
            "PrefixRecord" => Ok(TypeDef::PrefixRecord),
            "IpAddress" => Ok(TypeDef::IpAddress),
            "Asn" => Ok(TypeDef::Asn),
            "AsPath" => Ok(TypeDef::AsPath),
            "Community" => Ok(TypeDef::Community),
            "Route" => Ok(TypeDef::Route),
            "HexLiteral" => Ok(TypeDef::HexLiteral),
            _ => Err(format!("Undefined type: {}", ty.ident).into()),
        }
    }
}

impl From<&BuiltinTypeValue> for TypeDef {
    fn from(ty: &BuiltinTypeValue) -> TypeDef {
        match ty {
            BuiltinTypeValue::U32(_) => TypeDef::U32,
            BuiltinTypeValue::U8(_) => TypeDef::U8,
            BuiltinTypeValue::IntegerLiteral(_) => TypeDef::IntegerLiteral,
            BuiltinTypeValue::Boolean(_) => TypeDef::Boolean,
            BuiltinTypeValue::Prefix(_) => TypeDef::Prefix,
            BuiltinTypeValue::PrefixLengthLiteral(_) => TypeDef::PrefixLengthLiteral,
            BuiltinTypeValue::PrefixRecord(_) => TypeDef::PrefixRecord,
            BuiltinTypeValue::IpAddress(_) => TypeDef::IpAddress,
            BuiltinTypeValue::Asn(_) => TypeDef::Asn,
            BuiltinTypeValue::AsPath(_) => TypeDef::AsPath,
            BuiltinTypeValue::Community(_) => TypeDef::Community,
            BuiltinTypeValue::Route(_) => TypeDef::Route,
            BuiltinTypeValue::HexLiteral(_) => TypeDef::HexLiteral,
        }
    }
}

impl From<BuiltinTypeValue> for TypeDef {
    fn from(ty: BuiltinTypeValue) -> TypeDef {
        match ty {
            BuiltinTypeValue::U32(_) => TypeDef::U32,
            BuiltinTypeValue::U8(_) => TypeDef::U8,
            BuiltinTypeValue::IntegerLiteral(_) => TypeDef::IntegerLiteral,
            BuiltinTypeValue::Boolean(_) => TypeDef::Boolean,
            BuiltinTypeValue::Prefix(_) => TypeDef::Prefix,
            BuiltinTypeValue::PrefixLengthLiteral(_) => TypeDef::PrefixLengthLiteral,
            BuiltinTypeValue::PrefixRecord(_) => TypeDef::PrefixRecord,
            BuiltinTypeValue::IpAddress(_) => TypeDef::IpAddress,
            BuiltinTypeValue::Asn(_) => TypeDef::Asn,
            BuiltinTypeValue::AsPath(_) => TypeDef::AsPath,
            BuiltinTypeValue::Community(_) => TypeDef::Community,
            BuiltinTypeValue::Route(_) => TypeDef::Route,
            BuiltinTypeValue::HexLiteral(_) => TypeDef::HexLiteral,
        }
    }
}
