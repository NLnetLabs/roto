use std::{cell::RefCell, rc::Rc};

use crate::ast::ShortString;

/// Roto Types
///
/// This module contains the types offered by the Roto languages.

pub type GlobalSymbolTable<'a> = Rc<
    RefCell<
        std::collections::HashMap<
            super::symbols::Scope,
            super::symbols::SymbolTable,
        >,
    >,
>;

//------------ TypeDef -----------------------------------------------------

// These are all the types the user can create. This enum is used to create
// `user defined` types.

#[derive(Clone, Debug)]
pub enum TypeDef {
    List(Box<TypeDef>),
    Record(Vec<(ShortString, Box<TypeDef>)>),
    U32,
    U8,
    Boolean,
    String, // used for fieldname in method calls
    Prefix,
    IpAddress,
    Asn,
    AsPath,
    Community,
    Route,
    None,
}

impl<'a> TypeDef {
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
        Ok(TypeDef::Record(type_ident_pairs.iter().map(
            |(k,v)| (ShortString::from(*k), v.clone())
        
        ).collect()
        )
    )
    }

    pub fn has_fields_chain(
        &self,
        fields: &[super::ast::Identifier],
    ) -> Option<TypeDef> {
        let mut current_type = self;
        for field in fields {
            if let TypeDef::Record(fields) = current_type {
                if let Some((_, ty)) = fields
                    .iter()
                    .find(|(ident, _)| ident == &field.ident.as_str())
                {
                    current_type = ty;
                } else {
                    return None;
                }
            } else {
                return None;
            }
        }
        Some(current_type.clone())
    }

    fn _check_record_fields(&self, fields: &[(ShortString, &TypeValue)]) -> bool {
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

    fn _check_record_fields2(&self, fields: &[(ShortString, TypeValue)]) -> bool {
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
}

//     pub fn get_props_for_method(
//         self,
//         method: super::ast::Identifier,
//     ) -> Result<(Token, TypeValue), Box<dyn std::error::Error>>
//     where
//         TypeDef: RotoFilter,
//     {
//         match self {
//             TypeDef::List(list_type) => {
//                 <TypeDef as RotoFilter>::get_props_for_method(
//                     TypeDef::List(list_type),
//                     method,
//                 )
//             }
//             TypeDef::Record(rec_type) => {
//                 <TypeDef as RotoFilter>::get_props_for_method(
//                     TypeDef::Record(rec_type),
//                     method,
//                 )
//             }
//             TypeDef::AsPath => {
//                 <TypeDef as RotoFilter>::get_props_for_method(
//                     TypeDef::AsPath,
//                     method,
//                 )
//             }
//             _ => Err(format!(
//                 "Type {:?} does not have method {}",
//                 self, method
//             )
//             .into()),
//         }
//     }
// }

impl PartialEq<BuiltinTypeValue> for TypeDef {
    fn eq(&self, other: &BuiltinTypeValue) -> bool {
        match self {
            TypeDef::U32 => {
                matches!(other, BuiltinTypeValue::U32(_))
            }
            TypeDef::U8 => {
                matches!(other, BuiltinTypeValue::U8(_))
            }
            TypeDef::Prefix => {
                matches!(other, BuiltinTypeValue::Prefix(_))
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
            (a, TypeValue::Primitive(b)) => a == b,
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
                ._check_record_fields2(
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
impl<'a> TryFrom<crate::ast::TypeIdentifier> for TypeDef {
    type Error = Box<dyn std::error::Error>;
    fn try_from(
        ty: crate::ast::TypeIdentifier,
    ) -> Result<TypeDef, std::boxed::Box<dyn std::error::Error>> {
        match ty.ident.as_str() {
            "U32" => Ok(TypeDef::U32),
            "U8" => Ok(TypeDef::U8),
            "Prefix" => Ok(TypeDef::Prefix),
            "IpAddress" => Ok(TypeDef::IpAddress),
            "Asn" => Ok(TypeDef::Asn),
            "AsPath" => Ok(TypeDef::AsPath),
            "Community" => Ok(TypeDef::Community),
            "Route" => Ok(TypeDef::Route),
            _ => Err(format!("Undefined type: {}", ty.ident).into()),
        }
    }
}

impl<'a> From<BuiltinTypeValue> for TypeDef {
    fn from(ty: BuiltinTypeValue) -> TypeDef {
        match ty {
            BuiltinTypeValue::U32(_) => TypeDef::U32,
            BuiltinTypeValue::U8(_) => TypeDef::U8,
            BuiltinTypeValue::Boolean(_) => TypeDef::Boolean,
            BuiltinTypeValue::Prefix(_) => TypeDef::Prefix,
            BuiltinTypeValue::IpAddress(_) => TypeDef::IpAddress,
            BuiltinTypeValue::Asn(_) => TypeDef::Asn,
            BuiltinTypeValue::AsPath(_) => TypeDef::AsPath,
            BuiltinTypeValue::Community(_) => TypeDef::Community,
            BuiltinTypeValue::Route(_) => TypeDef::Route,
        }
    }
}

//------------ TypeValue ------------------------------------------------

/// These are the actual types that are used in the Roto language. This enum
/// holds both the type-level information and the value. The collection
/// variants can hold multiple values recursively, e.g. a List of Records.

#[derive(Debug, PartialEq)]
pub enum TypeValue {
    // All the built-in scalars
    Primitive(BuiltinTypeValue),
    // An ordered list of one type
    List(List),
    // A map of (key, value) pairs, where value can be any of the other types
    Record(Record),
    None,
}

impl TypeValue {
    pub fn is_empty(&self) -> bool {
        matches!(self, TypeValue::None)
    }

    pub fn from_literal(s: &str) -> Result<Self, Box<dyn std::error::Error>> {
        match s {
            "U32" => {
                Ok(TypeValue::Primitive(BuiltinTypeValue::U32(U32(None))))
            }
            "U8" => Ok(TypeValue::Primitive(BuiltinTypeValue::U8(U8(None)))),
            "prefix" => Ok(TypeValue::Primitive(BuiltinTypeValue::Prefix(
                Prefix(None),
            ))),
            "IpAddress" => Ok(TypeValue::Primitive(
                BuiltinTypeValue::IpAddress(IpAddress(None)),
            )),
            "Asn" => {
                Ok(TypeValue::Primitive(BuiltinTypeValue::Asn(Asn(None))))
            }
            "AsPath" => Ok(TypeValue::Primitive(BuiltinTypeValue::AsPath(
                AsPath(None),
            ))),
            "Community" => Ok(TypeValue::Primitive(
                BuiltinTypeValue::Community(Community(None)),
            )),
            _ => Err(Box::new(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!("Unknown type: {}", s),
            ))),
        }
    }

    fn create_record(
        type_ident_pairs: Vec<(&str, TypeValue)>,
    ) -> Result<Record, Box<dyn std::error::Error>> {
        let def_ = type_ident_pairs
            .into_iter()
            .map(|(ident, ty)| (ShortString::from(ident), ty.into()))
            .collect::<Vec<_>>();
        Record::new(def_)
    }
}

impl<'a> From<&'a TypeDef> for Box<TypeValue> {
    fn from(t: &'a TypeDef) -> Self {
        match t {
            TypeDef::U32 => Box::new(TypeValue::Primitive(
                BuiltinTypeValue::U32(U32(None)),
            )),
            TypeDef::U8 => {
                Box::new(TypeValue::Primitive(BuiltinTypeValue::U8(U8(None))))
            }
            TypeDef::Prefix => Box::new(TypeValue::Primitive(
                BuiltinTypeValue::Prefix(Prefix(None)),
            )),
            TypeDef::IpAddress => Box::new(TypeValue::Primitive(
                BuiltinTypeValue::IpAddress(IpAddress(None)),
            )),
            TypeDef::Asn => Box::new(TypeValue::Primitive(
                BuiltinTypeValue::Asn(Asn(None)),
            )),
            TypeDef::AsPath => Box::new(TypeValue::Primitive(
                BuiltinTypeValue::AsPath(AsPath(None)),
            )),
            TypeDef::Community => Box::new(TypeValue::Primitive(
                BuiltinTypeValue::Community(Community(None)),
            )),
            TypeDef::List(ty) => {
                Box::new(TypeValue::List(ty.as_ref().into()))
            }
            TypeDef::Record(kv_list) => {
                let def_ = kv_list
                    .iter()
                    .map(|(ident, ty)| (ident.clone(), ty.as_ref().into()))
                    .collect::<Vec<_>>();
                Box::new(TypeValue::Record(Record::new(def_).unwrap()))
            }
            _ => panic!("Unknown type"),
        }
    }
}

impl<'a> From<&'a TypeDef> for TypeValue {
    fn from(t: &'a TypeDef) -> Self {
        match t {
            TypeDef::U32 => {
                TypeValue::Primitive(BuiltinTypeValue::U32(U32(None)))
            }
            TypeDef::U8 => {
                TypeValue::Primitive(BuiltinTypeValue::U8(U8(None)))
            }
            TypeDef::Prefix => {
                TypeValue::Primitive(BuiltinTypeValue::Prefix(Prefix(None)))
            }
            TypeDef::IpAddress => TypeValue::Primitive(
                BuiltinTypeValue::IpAddress(IpAddress(None)),
            ),
            TypeDef::Asn => {
                TypeValue::Primitive(BuiltinTypeValue::Asn(Asn(None)))
            }
            TypeDef::AsPath => {
                TypeValue::Primitive(BuiltinTypeValue::AsPath(AsPath(None)))
            }
            TypeDef::Community => TypeValue::Primitive(
                BuiltinTypeValue::Community(Community(None)),
            ),
            TypeDef::List(ty) => TypeValue::List(ty.as_ref().into()),
            TypeDef::Record(kv_list) => {
                let def_ = kv_list
                    .iter()
                    .map(|(ident, ty)| (ident.clone(), ty.as_ref().into()))
                    .collect::<Vec<_>>();
                TypeValue::Record(Record::new(def_).unwrap())
            }
            _ => panic!("Unknown type"),
        }
    }
}

impl std::fmt::Display for TypeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TypeValue::Primitive(p) => write!(f, "primitive type {}", p),
            TypeValue::List(l) => write!(f, "list that contains type {}", l),
            TypeValue::Record(r) => write!(f, "record that contains type {}", r),
            TypeValue::None => write!(f, "None"),
        }
    }
}

//------------ PrimitiveTypeValue -------------------------------------------

// The built-in types

#[derive(Debug, PartialEq)]
pub enum BuiltinTypeValue {
    U32(U32),
    U8(U8),
    Prefix(Prefix),
    Community(Community),
    IpAddress(IpAddress),
    Asn(Asn),
    AsPath(AsPath),
    Route(Route),
    Boolean(Boolean),
}

impl BuiltinTypeValue {
    pub fn get_type_name(&self) -> &'static str {
        match self {
            BuiltinTypeValue::U32(_) => "U32",
            BuiltinTypeValue::U8(_) => "U8",
            BuiltinTypeValue::Boolean(_) => "Boolean",
            BuiltinTypeValue::Prefix(_) => "Prefix",
            BuiltinTypeValue::Community(_) => "Community",
            BuiltinTypeValue::IpAddress(_) => "IpAddress",
            BuiltinTypeValue::Asn(_) => "Asn",
            BuiltinTypeValue::AsPath(_) => "AsPath",
            BuiltinTypeValue::Route(_) => "Route",
        }
    }

    pub fn get_value(&self) -> &dyn std::any::Any {
        match self {
            BuiltinTypeValue::U32(val) => val,
            BuiltinTypeValue::U8(val) => val,
            BuiltinTypeValue::Boolean(val) => val,
            BuiltinTypeValue::Prefix(val) => val,
            BuiltinTypeValue::Community(val) => val,
            BuiltinTypeValue::IpAddress(val) => val,
            BuiltinTypeValue::Asn(val) => val,
            BuiltinTypeValue::AsPath(val) => val,
            BuiltinTypeValue::Route(val) => val,
        }
    }

    pub fn exists(ty: &'_ str) -> bool {
        matches!(
            ty,
            "U32"
                | "U8"
                | "Prefix"
                | "Community"
                | "IpAddress"
                | "Asn"
                | "AsPath"
                | "Route"
        )
    }

    pub fn create_instance(
        ty: TypeDef,
        value: impl Into<BuiltinTypeValue>,
    ) -> Result<TypeValue, Box<dyn std::error::Error>> {
        let var = match ty {
            TypeDef::U32 => {
                if let BuiltinTypeValue::U32(v) = value.into() {
                    BuiltinTypeValue::U32(v)
                } else {
                    return Err("Not a u32".into());
                }
            }
            TypeDef::U8 => {
                if let BuiltinTypeValue::U8(v) = value.into() {
                    BuiltinTypeValue::U8(v)
                } else {
                    return Err("Not a u8".into());
                }
            }
            TypeDef::Prefix => {
                if let BuiltinTypeValue::Prefix(v) = value.into() {
                    BuiltinTypeValue::Prefix(v)
                } else {
                    return Err("Not a prefix".into());
                }
            }
            TypeDef::IpAddress => {
                if let BuiltinTypeValue::IpAddress(v) = value.into() {
                    BuiltinTypeValue::IpAddress(v)
                } else {
                    return Err("Not an IP address".into());
                }
            }
            TypeDef::Asn => {
                if let BuiltinTypeValue::Asn(v) = value.into() {
                    BuiltinTypeValue::Asn(v)
                } else {
                    return Err("Not an ASN".into());
                }
            }
            TypeDef::AsPath => {
                if let BuiltinTypeValue::AsPath(v) = value.into() {
                    BuiltinTypeValue::AsPath(v)
                } else {
                    return Err("Not an AS Path".into());
                }
            }
            TypeDef::Community => {
                if let BuiltinTypeValue::Community(v) = value.into() {
                    BuiltinTypeValue::Community(v)
                } else {
                    return Err("Not a community".into());
                }
            }
            _ => return Err("Not a primitive type".into()),
        };
        Ok(TypeValue::Primitive(var))
    }
}

// These From impls allow the user to use the create_instance function with
// simple types like u32, u8, etc. (without the nested variants).

impl From<Asn> for BuiltinTypeValue {
    fn from(val: Asn) -> Self {
        BuiltinTypeValue::Asn(val)
    }
}

impl From<u32> for BuiltinTypeValue {
    fn from(val: u32) -> Self {
        BuiltinTypeValue::U32(U32(Some(val)))
    }
}

impl From<std::net::IpAddr> for BuiltinTypeValue {
    fn from(val: std::net::IpAddr) -> Self {
        BuiltinTypeValue::IpAddress(IpAddress(Some(val)))
    }
}

impl From<routecore::addr::Prefix> for BuiltinTypeValue {
    fn from(val: routecore::addr::Prefix) -> Self {
        BuiltinTypeValue::Prefix(Prefix(Some(val)))
    }
}

impl TryFrom<&'_ str> for BuiltinTypeValue {
    type Error = Box<dyn std::error::Error>;

    fn try_from(val: &'_ str) -> Result<Self, Self::Error> {
        match val {
            "U32" => Ok(BuiltinTypeValue::U32(U32(None))),
            "U8" => Ok(BuiltinTypeValue::U8(U8(None))),
            "Prefix" => Ok(BuiltinTypeValue::Prefix(Prefix(None))),
            "Community" => Ok(BuiltinTypeValue::Community(Community(None))),
            "IpAddress" => Ok(BuiltinTypeValue::IpAddress(IpAddress(None))),
            "Asn" => Ok(BuiltinTypeValue::Asn(Asn(None))),
            "AsPath" => Ok(BuiltinTypeValue::AsPath(AsPath(None))),
            "Route" => Ok(BuiltinTypeValue::Route(Route {
                prefix: None,
                bgp: None,
            })),
            _ => Err(format!("Unknown type: {}", val).into()),
        }
    }
}

impl TryFrom<&TypeDef> for BuiltinTypeValue {
    type Error = Box<dyn std::error::Error>;

    fn try_from(ty: &TypeDef) -> Result<Self, Self::Error> {
        match ty {
            TypeDef::U32 => Ok(BuiltinTypeValue::U32(U32(None))),
            TypeDef::U8 => Ok(BuiltinTypeValue::U8(U8(None))),
            TypeDef::Prefix => Ok(BuiltinTypeValue::Prefix(Prefix(None))),
            TypeDef::Community => {
                Ok(BuiltinTypeValue::Community(Community(None)))
            }
            TypeDef::IpAddress => {
                Ok(BuiltinTypeValue::IpAddress(IpAddress(None)))
            }
            TypeDef::Asn => Ok(BuiltinTypeValue::Asn(Asn(None))),
            TypeDef::AsPath => Ok(BuiltinTypeValue::AsPath(AsPath(None))),
            TypeDef::Route => Ok(BuiltinTypeValue::Route(Route {
                prefix: None,
                bgp: None,
            })),
            _ => Err(format!("Unknown type: {:?}", ty).into()),
        }
    }
}

impl std::fmt::Display for BuiltinTypeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            BuiltinTypeValue::U32(_) => write!(f, "unsigned 32-bits integer"),
            BuiltinTypeValue::U8(_) => write!(f, "unsigned 8-bits integer"),
            BuiltinTypeValue::Prefix(_) => write!(f, "prefix"),
            BuiltinTypeValue::Community(_) => write!(f, "community"),
            BuiltinTypeValue::IpAddress(_) => write!(f, "ip address"),
            BuiltinTypeValue::Asn(_) => write!(f, "Autonomous System Number"),
            BuiltinTypeValue::AsPath(_) => write!(f, "AsPath (BGP AS_PATH attribute)"),
            BuiltinTypeValue::Route(_) => write!(f, "Route (BGP Route)"),
            BuiltinTypeValue::Boolean(_) => write!(f, "Boolean"),
        }
    }
}

// ----------- A simple u32 type --------------------------------------------

#[derive(Debug, PartialEq)]
pub struct U32(Option<u32>);

impl U32 {
    pub fn new(val: u32) -> Self {
        U32(Some(val))
    }
}

// ----------- A simple u8 type ---------------------------------------------

#[derive(Debug, PartialEq)]
pub struct U8(Option<u8>);

impl U8 {
    pub fn new(val: u8) -> Self {
        U8(Some(val))
    }
}

// ----------- Boolean type -------------------------------------------------

#[derive(Debug, PartialEq)]
pub struct Boolean(Option<bool>);
impl Boolean {
    pub fn new(val: bool) -> Self {
        Boolean(Some(val))
    }
}

// ----------- Prefix type --------------------------------------------------

#[derive(Debug, PartialEq)]
pub struct Prefix(Option<routecore::addr::Prefix>);

impl Prefix {
    pub fn new(prefix: routecore::addr::Prefix) -> Self {
        Self(Some(prefix))
    }
}

#[derive(Debug, PartialEq)]
pub enum CommunityType {
    Normal,
    Extended,
    Large,
}

#[derive(Debug, PartialEq)]
pub struct Community(Option<CommunityType>);

impl Community {
    pub fn new(community_type: CommunityType) -> Self {
        Self(Some(community_type))
    }
}

// ----------- IpAddress type -----------------------------------------------

#[derive(Debug, PartialEq)]
pub struct IpAddress(Option<std::net::IpAddr>);

impl IpAddress {
    pub fn new(addr: std::net::IpAddr) -> Self {
        IpAddress(Some(addr))
    }
}

// ----------- Asn type -----------------------------------------------------

#[derive(Debug, PartialEq)]
pub struct Asn(Option<routecore::asn::Asn>);

impl Asn {
    pub fn new(asn: routecore::asn::Asn) -> Self {
        Asn(Some(asn))
    }

    pub fn from_u32(asn: u32) -> Self {
        Asn(Some(routecore::asn::Asn::from(asn)))
    }

    pub fn get_asn(&self) -> routecore::asn::Asn {
        self.0.unwrap()
    }
}


// ----------- AsPath type --------------------------------------------------

#[derive(Debug, PartialEq)]
pub struct AsPath(Option<Vec<Asn>>);

impl AsPath {
    pub fn new(as_path: Vec<Asn>) -> Self {
        AsPath(Some(as_path))
    }

    pub fn from_vec_u32(as_path: Vec<u32>) -> Self {
        let as_path = as_path
            .into_iter()
            .map(|asn| Asn::new(routecore::asn::Asn::from(asn)))
            .collect();
        AsPath::new(as_path)
    }
}

impl RotoFilter for AsPath {
    fn get_props_for_method(
        self,
        method_name: crate::ast::Identifier,
    ) -> Result<(Token, TypeValue), Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
                "origin" => Ok((Token::RecordLongestMatch, TypeValue::Primitive(BuiltinTypeValue::AsPath(AsPath(None))))),
                "len" => Ok((Token::RecordGet, TypeValue::Primitive(BuiltinTypeValue::U8(U8(None))))),
                _ => {
                    Err(format!("Unknown method '{}'", method_name.ident)
                        .into())
                }
        }
    }

    fn exec_method(
        &self,
        method: Token,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> TypeDef {
        todo!()
    }
}

enum AsPathToken {
    Origin,
    Contains,
}

// impl<'a> RotoFilter<'a> for AsPathToken {
//     type Token = AsPathToken;

//     fn get_props_for_method(
//             ty: Vec<(&'a str, Box<TypeDef<'a>>)>,
//             method_name: crate::ast::Identifier,
//         ) -> Result<(Self, TypeDef<'a>), Box<dyn std::error::Error>> where Self: std::marker::Sized {
//         match method_name.ident.as_str() {
//             "origin" => Ok((AsPathToken::Origin, TypeDef::Asn)),
//             "contains" => Ok((AsPathToken::Contains, TypeDef::Boolean)),
//             _ => Err(format!("Unknown method: {}", method_name).into()),
//         }
//     }

//     fn exec_method(
//             &self,
//             method: Self,
//             args: Vec<TypeValue>,
//             res_type: TypeDef,
//         ) -> TypeDef {
//         todo!()
//     }
// }

//------------ RFC4271 Route type -------------------------------------------

#[derive(Debug, PartialEq)]
pub struct Route {
    pub prefix: Option<Prefix>,
    pub bgp: Option<BgpRecord>,
}

#[derive(Debug, PartialEq)]
pub struct BgpRecord {
    pub as_path: AsPath,
    pub communities: Vec<Community>,
}

//------------ Collections: ElementType -------------------------------------

// This enum is used to differentiate between recursive collections and simple
// collections (that only contain primitive types). The latter do not need to
// be boxed, while the former do.

#[derive(Debug, PartialEq)]
pub enum ElementTypeValue {
    Primitive(BuiltinTypeValue),
    Nested(Box<TypeValue>),
}

impl<'a> From<&'a TypeDef> for ElementTypeValue {
    fn from(t: &'a TypeDef) -> Self {
        match t {
            TypeDef::U32 => {
                ElementTypeValue::Primitive(BuiltinTypeValue::U32(U32(None)))
            }
            TypeDef::U8 => {
                ElementTypeValue::Primitive(BuiltinTypeValue::U8(U8(None)))
            }
            TypeDef::Prefix => ElementTypeValue::Primitive(
                BuiltinTypeValue::Prefix(Prefix(None)),
            ),
            TypeDef::IpAddress => ElementTypeValue::Primitive(
                BuiltinTypeValue::IpAddress(IpAddress(None)),
            ),
            TypeDef::Asn => {
                ElementTypeValue::Primitive(BuiltinTypeValue::Asn(Asn(None)))
            }
            TypeDef::AsPath => ElementTypeValue::Primitive(
                BuiltinTypeValue::AsPath(AsPath(None)),
            ),
            TypeDef::Community => ElementTypeValue::Primitive(
                BuiltinTypeValue::Community(Community(None)),
            ),
            TypeDef::List(ty) => ElementTypeValue::Nested(Box::new(
                TypeValue::List(ty.as_ref().into()),
            )),
            TypeDef::Record(kv_list) => {
                let def_ = kv_list
                    .iter()
                    .map(|(ident, ty)| (ident.clone(), ty.as_ref().into()))
                    .collect::<Vec<_>>();
                ElementTypeValue::Nested(Box::new(TypeValue::Record(
                    Record::new(def_).unwrap(),
                )))
            }
            _ => panic!("Unknown type"),
        }
    }
}

impl<'a> From<TypeValue> for ElementTypeValue {
    fn from(t: TypeValue) -> Self {
        match t {
            TypeValue::Primitive(v) => ElementTypeValue::Primitive(v),
            TypeValue::List(ty) => {
                ElementTypeValue::Nested(Box::new(TypeValue::List(ty)))
            }
            TypeValue::Record(kv_list) => {
                ElementTypeValue::Nested(Box::new(TypeValue::Record(kv_list)))
            }
            _ => panic!("Unknown type"),
        }
    }
}

//============ Collections ==================================================

//------------ List type ----------------------------------------------------

#[derive(Debug, PartialEq)]
pub struct List(Vec<ElementTypeValue>);

impl<'a> List {
    pub fn new(elem_type: Vec<ElementTypeValue>) -> Self {
        List(elem_type)
    }

    pub fn iter(&self) -> std::slice::Iter<ElementTypeValue> {
        self.0.iter()
    }
}

impl<'a> From<&'a TypeDef> for List {
    fn from(t: &'a TypeDef) -> Self {
        List::new(vec![t.into()])
    }
}

impl std::fmt::Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[")?;
        for (i, elem) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            match elem {
                ElementTypeValue::Primitive(v) => write!(f, "{}", v)?,
                ElementTypeValue::Nested(v) => write!(f, "{}", v)?,
            }
        }
        write!(f, "]")
    }
}

impl RotoFilter for List{
    fn get_props_for_method(
        self,
        method_name: crate::ast::Identifier,
    ) -> Result<(Token, TypeValue), Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
                "get" => Ok((Token::RecordGet, TypeValue::List(self))),
                "get_all" => Ok((Token::RecordGetAll, TypeValue::List(self))),
                "contains" => Ok((Token::RecordContains, TypeValue::Primitive(BuiltinTypeValue::Boolean(Boolean(None))))),
                _ => {
                    Err(format!("Unknown method '{}'", method_name.ident)
                        .into())
                }
        }
    }

    fn exec_method(
        &self,
        _method: Token,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> TypeDef {
        todo!()
    }
}

pub enum ListToken {
    Len,
    Contains,
    Get,
    Push,
    Pop,
    Remove,
    Insert,
    Clear,
}

//---------------- Record type ----------------------------------------------

#[derive(Debug, PartialEq)]
pub struct Record(Vec<(ShortString, ElementTypeValue)>);

impl<'a> Record {
    pub fn new(
        elems: Vec<(ShortString, ElementTypeValue)>,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        Ok(Self(elems))
    }

    pub fn create_instance(
        ty: &TypeDef,
        kvs: Vec<(&str, TypeValue)>,
    ) -> Result<Record, Box<dyn std::error::Error>> {
        let shortstring_vec = kvs.iter().map(|(name, ty)| {
            (ShortString::from(*name), ty)
        }).collect::<Vec<_>>();
        if let TypeDef::Record(_rec) = ty {
            if ty._check_record_fields(shortstring_vec.as_slice()) {
                TypeValue::create_record(kvs)
            } else {
                Err("Record fields do not match record type".into())
            }
        } else {
            Err("Not a record type".into())
        }
    }

    pub fn get_value_for_field(
        &self,
        field: &'a str,
    ) -> Option<&ElementTypeValue> {
        self.0.iter().find(|(f, _)| f == &field).map(|(_, v)| v)
    }
}

impl std::fmt::Display for Record {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (i, (field, elem)) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: ", field)?;
            match elem {
                ElementTypeValue::Primitive(v) => write!(f, "{}", v)?,
                ElementTypeValue::Nested(v) => write!(f, "{}", v)?,
            }
        }
        write!(f, "}}")
    }
}

impl<'a> RotoFilter for Record {
    fn get_props_for_method(
        self,
        method_name: crate::ast::Identifier,
    ) -> Result<(Token, TypeValue), Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized,
    {
        match method_name.ident.as_str() {
                "longest_match" => Ok((Token::RecordLongestMatch, TypeValue::Record(self))),
                "get" => Ok((Token::RecordGet, TypeValue::Record(self))),
                "get_all" => Ok((Token::RecordGetAll, TypeValue::Record(self))),
                "contains" => Ok((Token::RecordContains, TypeValue::Primitive(BuiltinTypeValue::Boolean(Boolean(None))))),
                _ => {
                    Err(format!("Unknown method '{}'", method_name.ident)
                        .into())
                }
        }
    }

    fn exec_method(
        &self,
        _method: Token,
        _args: Vec<TypeValue>,
        _res_type: TypeDef,
    ) -> TypeDef {
        todo!()
    }
}

pub enum Token {
    RecordLongestMatch,
    RecordGet,
    RecordGetAll,
    RecordContains,
    AsPathOrigin,
    AsPathContains,
}

pub trait RotoFilter {
    fn get_props_for_method(
        self,
        method_name: super::ast::Identifier,
    ) -> Result<(Token, TypeValue), Box<dyn std::error::Error>>
    where
        Self: std::marker::Sized;
    fn exec_method(
        &self,
        method: Token,
        args: Vec<TypeValue>,
        res_type: TypeDef,
    ) -> TypeDef;
}
