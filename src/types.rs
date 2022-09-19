/// Roto Types
///
/// This module contains the types offered by the Roto languages.

//------------ TypeDef -----------------------------------------------------

// These are all the types the user can create. This enum is used to create
// `user defined` types.

use std::ops::Deref;

#[derive(Clone, Debug)]
pub enum TypeDef<'a> {
    List(Box<TypeDef<'a>>),
    Record(Vec<(&'a str, Box<TypeDef<'a>>)>),
    U32,
    U8,
    Prefix,
    IpAddress,
    Asn,
    AsPath,
    Community,
    None,
}

impl<'a> TypeDef<'a> {
    pub fn new_record_type(
        type_ident_pairs: Vec<(&'a str, Box<TypeDef<'a>>)>,
    ) -> Result<TypeDef<'a>, Box<dyn std::error::Error>> {
        // let def_ = type_ident_pairs
        //     .iter()
        //     .map(move |(ident, ty)| (*ident, ty))
        //     .collect::<Vec<_>>();
        Ok(TypeDef::Record(type_ident_pairs))
    }

    fn _check_record_fields(&self, fields: &[(&str, TypeValue)]) -> bool {
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

impl PartialEq<PrimitiveTypeValue> for TypeDef<'_> {
    fn eq(&self, other: &PrimitiveTypeValue) -> bool {
        match self {
            TypeDef::U32 => {
                matches!(other, PrimitiveTypeValue::U32(_))
            }
            TypeDef::U8 => {
                matches!(other, PrimitiveTypeValue::U8(_))
            }
            TypeDef::Prefix => {
                matches!(other, PrimitiveTypeValue::Prefix(_))
            }
            TypeDef::IpAddress => {
                matches!(other, PrimitiveTypeValue::IpAddress(_))
            }
            TypeDef::Asn => {
                matches!(other, PrimitiveTypeValue::Asn(_))
            }
            TypeDef::AsPath => {
                matches!(other, PrimitiveTypeValue::AsPath(_))
            }
            TypeDef::Community => {
                matches!(other, PrimitiveTypeValue::Community(_))
            }
            _ => false,
        }
    }
}

impl PartialEq<TypeValue<'_>> for TypeDef<'_> {
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
                ._check_record_fields(
                    a.iter()
                        .map(|ty| (ty.0, ty.1.as_ref().into()))
                        .collect::<Vec<_>>()
                        .as_slice(),
                ),
            _ => false,
        }
    }
}

// This From impl creates the link between the AST and the TypeDef enum.
impl<'a> TryFrom<crate::ast::TypeIdentifier> for TypeDef<'a> {
    type Error = Box<dyn std::error::Error>;
    fn try_from(ty: crate::ast::TypeIdentifier) -> Result<TypeDef<'a>, std::boxed::Box<dyn std::error::Error>> {
        match ty.ident.as_str() {
            "U32" => Ok(TypeDef::U32),
            "U8" => Ok(TypeDef::U8),
            "Prefix" => Ok(TypeDef::Prefix),
            "IpAddress" => Ok(TypeDef::IpAddress),
            "Asn" => Ok(TypeDef::Asn),
            "AsPath" => Ok(TypeDef::AsPath),
            "Community" => Ok(TypeDef::Community),
            _ => Err(format!("Undefined type: {}", ty.ident).into()),
        }
    }
}

//------------ TypeValue ------------------------------------------------

/// These are the actual types that are used in the Roto language. This enum
/// holds both the type-level information and the value. The collection
/// variants can hold multiple values recursively, e.g. a List of Records.

#[derive(Debug, PartialEq)]
pub enum TypeValue<'a> {
    // All the built-in scalars
    Primitive(PrimitiveTypeValue),
    // An ordered list of one type
    List(List<'a>),
    // A map of (key, value) pairs, where value can be any of the other types
    Record(Record<'a>),
    None,
}

impl<'a> TypeValue<'a> {
    pub fn is_empty(&self) -> bool {
        matches!(self, TypeValue::None)
    }

    pub fn from_literal(s: &str) -> Result<Self, Box<dyn std::error::Error>> {
        match s {
            "U32" => {
                Ok(TypeValue::Primitive(PrimitiveTypeValue::U32(U32(None))))
            }
            "U8" => {
                Ok(TypeValue::Primitive(PrimitiveTypeValue::U8(U8(None))))
            }
            "prefix" => Ok(TypeValue::Primitive(PrimitiveTypeValue::Prefix(
                Prefix(None),
            ))),
            "IpAddress" => Ok(TypeValue::Primitive(
                PrimitiveTypeValue::IpAddress(IpAddress(None)),
            )),
            "Asn" => {
                Ok(TypeValue::Primitive(PrimitiveTypeValue::Asn(Asn(None))))
            }
            "AsPath" => Ok(TypeValue::Primitive(PrimitiveTypeValue::AsPath(
                AsPath(None),
            ))),
            "Community" => Ok(TypeValue::Primitive(
                PrimitiveTypeValue::Community(Community(None)),
            )),
            _ => Err(Box::new(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!("Unknown type: {}", s),
            ))),
        }
    }

    fn create_record(
        type_ident_pairs: Vec<(&'a str, TypeValue<'a>)>,
    ) -> Result<Record<'a>, Box<dyn std::error::Error>> {
        let def_ = type_ident_pairs
            .into_iter()
            .map(|(ident, ty)| (ident, ty.into()))
            .collect();
        Record::new(def_)
    }
}

impl<'a> From<&'a TypeDef<'_>> for Box<TypeValue<'a>> {
    fn from(t: &'a TypeDef<'_>) -> Self {
        match t {
            TypeDef::U32 => Box::new(TypeValue::Primitive(
                PrimitiveTypeValue::U32(U32(None)),
            )),
            TypeDef::U8 => Box::new(TypeValue::Primitive(
                PrimitiveTypeValue::U8(U8(None)),
            )),
            TypeDef::Prefix => Box::new(TypeValue::Primitive(
                PrimitiveTypeValue::Prefix(Prefix(None)),
            )),
            TypeDef::IpAddress => Box::new(TypeValue::Primitive(
                PrimitiveTypeValue::IpAddress(IpAddress(None)),
            )),
            TypeDef::Asn => Box::new(TypeValue::Primitive(
                PrimitiveTypeValue::Asn(Asn(None)),
            )),
            TypeDef::AsPath => Box::new(TypeValue::Primitive(
                PrimitiveTypeValue::AsPath(AsPath(None)),
            )),
            TypeDef::Community => Box::new(TypeValue::Primitive(
                PrimitiveTypeValue::Community(Community(None)),
            )),
            TypeDef::List(ty) => {
                Box::new(TypeValue::List(ty.as_ref().into()))
            }
            TypeDef::Record(kv_list) => {
                let def_ = kv_list
                    .iter()
                    .map(|(ident, ty)| (*ident, ty.as_ref().into()))
                    .collect::<Vec<_>>();
                Box::new(TypeValue::Record(Record::new(def_).unwrap()))
            }
            _ => panic!("Unknown type"),
        }
    }
}

impl<'a> From<&'a TypeDef<'_>> for TypeValue<'a> {
    fn from(t: &'a TypeDef<'_>) -> Self {
        match t {
            TypeDef::U32 => {
                TypeValue::Primitive(PrimitiveTypeValue::U32(U32(None)))
            }
            TypeDef::U8 => {
                TypeValue::Primitive(PrimitiveTypeValue::U8(U8(None)))
            }
            TypeDef::Prefix => {
                TypeValue::Primitive(PrimitiveTypeValue::Prefix(Prefix(None)))
            }
            TypeDef::IpAddress => TypeValue::Primitive(
                PrimitiveTypeValue::IpAddress(IpAddress(None)),
            ),
            TypeDef::Asn => {
                TypeValue::Primitive(PrimitiveTypeValue::Asn(Asn(None)))
            }
            TypeDef::AsPath => {
                TypeValue::Primitive(PrimitiveTypeValue::AsPath(AsPath(None)))
            }
            TypeDef::Community => TypeValue::Primitive(
                PrimitiveTypeValue::Community(Community(None)),
            ),
            TypeDef::List(ty) => TypeValue::List(ty.as_ref().into()),
            TypeDef::Record(kv_list) => {
                let def_ = kv_list
                    .iter()
                    .map(|(ident, ty)| (*ident, ty.as_ref().into()))
                    .collect::<Vec<_>>();
                TypeValue::Record(Record::new(def_).unwrap())
            }
            _ => panic!("Unknown type"),
        }
    }
}

//------------ PrimitiveTypeValue -------------------------------------------

// The built-in types

#[derive(Debug, PartialEq)]
pub enum PrimitiveTypeValue {
    U32(U32),
    U8(U8),
    Prefix(Prefix),
    Community(Community),
    IpAddress(IpAddress),
    Asn(Asn),
    AsPath(AsPath),
}

impl PrimitiveTypeValue {
    pub fn get_type_name(&self) -> &'static str {
        match self {
            PrimitiveTypeValue::U32(_) => "U32",
            PrimitiveTypeValue::U8(_) => "U8",
            PrimitiveTypeValue::Prefix(_) => "Prefix",
            PrimitiveTypeValue::Community(_) => "Community",
            PrimitiveTypeValue::IpAddress(_) => "IpAddress",
            PrimitiveTypeValue::Asn(_) => "Asn",
            PrimitiveTypeValue::AsPath(_) => "AsPath",
        }
    }

    pub fn get_value(&self) -> &dyn std::any::Any {
        match self {
            PrimitiveTypeValue::U32(val) => val,
            PrimitiveTypeValue::U8(val) => val,
            PrimitiveTypeValue::Prefix(val) => val,
            PrimitiveTypeValue::Community(val) => val,
            PrimitiveTypeValue::IpAddress(val) => val,
            PrimitiveTypeValue::Asn(val) => val,
            PrimitiveTypeValue::AsPath(val) => val,
        }
    }

    pub fn create_instance<'a>(
        ty: TypeDef,
        value: impl Into<PrimitiveTypeValue>,
    ) -> Result<TypeValue<'a>, Box<dyn std::error::Error>> {
        let var = match ty {
            TypeDef::U32 => {
                if let PrimitiveTypeValue::U32(v) = value.into() {
                    PrimitiveTypeValue::U32(v)
                } else {
                    return Err("Not a u32".into());
                }
            }
            TypeDef::U8 => {
                if let PrimitiveTypeValue::U8(v) = value.into() {
                    PrimitiveTypeValue::U8(v)
                } else {
                    return Err("Not a u8".into());
                }
            }
            TypeDef::Prefix => {
                if let PrimitiveTypeValue::Prefix(v) = value.into() {
                    PrimitiveTypeValue::Prefix(v)
                } else {
                    return Err("Not a prefix".into());
                }
            }
            TypeDef::IpAddress => {
                if let PrimitiveTypeValue::IpAddress(v) = value.into() {
                    PrimitiveTypeValue::IpAddress(v)
                } else {
                    return Err("Not an IP address".into());
                }
            }
            TypeDef::Asn => {
                if let PrimitiveTypeValue::Asn(v) = value.into() {
                    PrimitiveTypeValue::Asn(v)
                } else {
                    return Err("Not an ASN".into());
                }
            }
            TypeDef::AsPath => {
                if let PrimitiveTypeValue::AsPath(v) = value.into() {
                    PrimitiveTypeValue::AsPath(v)
                } else {
                    return Err("Not an AS Path".into());
                }
            }
            TypeDef::Community => {
                if let PrimitiveTypeValue::Community(v) = value.into() {
                    PrimitiveTypeValue::Community(v)
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

impl From<Asn> for PrimitiveTypeValue {
    fn from(val: Asn) -> Self {
        PrimitiveTypeValue::Asn(val)
    }
}

impl From<u32> for PrimitiveTypeValue {
    fn from(val: u32) -> Self {
        PrimitiveTypeValue::U32(U32(Some(val)))
    }
}

impl From<std::net::IpAddr> for PrimitiveTypeValue {
    fn from(val: std::net::IpAddr) -> Self {
        PrimitiveTypeValue::IpAddress(IpAddress(Some(val)))
    }
}

impl From<routecore::addr::Prefix> for PrimitiveTypeValue {
    fn from(val: routecore::addr::Prefix) -> Self {
        PrimitiveTypeValue::Prefix(Prefix(Some(val)))
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

//------------ RFC4271 Route type -------------------------------------------

pub struct Route<Meta: routecore::record::Meta> {
    pub prefix: Prefix,
    pub bgp: Option<BgpRecord>,
    pub meta: Meta,
}

pub struct BgpRecord {
    pub as_path: AsPath,
    pub communities: Vec<Community>,
}

//------------ Collections: ElementType -------------------------------------

// This enum is used to differentiate between recursive collections and simple
// collections (that only contain primitive types). The latter do not need to
// be boxed, while the former do.

#[derive(Debug, PartialEq)]
pub enum ElementTypeValue<'a> {
    Primitive(PrimitiveTypeValue),
    Nested(Box<TypeValue<'a>>),
}

impl<'a> From<&'a TypeDef<'_>> for ElementTypeValue<'a> {
    fn from(t: &'a TypeDef<'_>) -> Self {
        match t {
            TypeDef::U32 => ElementTypeValue::Primitive(
                PrimitiveTypeValue::U32(U32(None)),
            ),
            TypeDef::U8 => {
                ElementTypeValue::Primitive(PrimitiveTypeValue::U8(U8(None)))
            }
            TypeDef::Prefix => ElementTypeValue::Primitive(
                PrimitiveTypeValue::Prefix(Prefix(None)),
            ),
            TypeDef::IpAddress => ElementTypeValue::Primitive(
                PrimitiveTypeValue::IpAddress(IpAddress(None)),
            ),
            TypeDef::Asn => ElementTypeValue::Primitive(
                PrimitiveTypeValue::Asn(Asn(None)),
            ),
            TypeDef::AsPath => ElementTypeValue::Primitive(
                PrimitiveTypeValue::AsPath(AsPath(None)),
            ),
            TypeDef::Community => ElementTypeValue::Primitive(
                PrimitiveTypeValue::Community(Community(None)),
            ),
            TypeDef::List(ty) => ElementTypeValue::Nested(Box::new(
                TypeValue::List(ty.as_ref().into()),
            )),
            TypeDef::Record(kv_list) => {
                let def_ = kv_list
                    .iter()
                    .map(|(ident, ty)| (*ident, ty.as_ref().into()))
                    .collect::<Vec<_>>();
                ElementTypeValue::Nested(Box::new(TypeValue::Record(
                    Record::new(def_).unwrap(),
                )))
            }
            _ => panic!("Unknown type"),
        }
    }
}

impl<'a> From<TypeValue<'a>> for ElementTypeValue<'a> {
    fn from(t: TypeValue<'a>) -> Self {
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

//------------ Collections: List type ------------------------------------------------

#[derive(Debug, PartialEq)]
pub struct List<'a>(Vec<ElementTypeValue<'a>>);

impl<'a> List<'a> {
    pub fn new(elem_type: Vec<ElementTypeValue<'a>>) -> Self {
        List(elem_type)
    }

    pub fn iter(&self) -> std::slice::Iter<ElementTypeValue<'a>> {
        self.0.iter()
    }
}

impl<'a> From<&'a TypeDef<'a>> for List<'a> {
    fn from(t: &'a TypeDef) -> Self {
        List::new(vec![t.into()])
    }
}

//---------------- Collections: Record type --------------------------------------------

#[derive(Debug, PartialEq)]
pub struct Record<'a>(Vec<(&'a str, ElementTypeValue<'a>)>);

impl<'a> Record<'a> {
    pub fn new(
        elems: Vec<(&'a str, ElementTypeValue<'a>)>,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        Ok(Self(elems))
    }

    pub fn create_instance(
        ty: &TypeDef,
        kvs: Vec<(&'a str, TypeValue<'a>)>,
    ) -> Result<Record<'a>, Box<dyn std::error::Error>> {
        if let TypeDef::Record(_rec) = ty {
            if ty._check_record_fields(kvs.as_slice()) {
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
