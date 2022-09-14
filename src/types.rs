pub enum TypeName<'a> {
    Primitive,
    List(Box<TypeName<'a>>),
    Record(Vec<(&'a str, Box<TypeName<'a>>)>),
    U32,
    U8,
    Prefix,
    IpAddress,
    Asn,
    AsPath,
    Community,
    None,
}

impl<'a> TypeName<'a> {
    pub fn new_record(
        self,
        kvs: Vec<(&'a str, RotoType<'a>)>,
    ) -> Result<Record<'a>, Box<dyn std::error::Error>> {

        // TODO: Actually check whether the value fits the
        // record type.
        if let TypeName::Record(_) = self {
            RotoType::define_record(kvs)
        } else {
            Err("Not a record type".into())
        }
    }
}

/// Roto Types
///
/// This module contains the types offered by the Roto languages.

#[derive(Debug)]
pub enum RotoType<'a> {
    Primitive(RotoPrimitiveType),
    List(List<'a>),
    Record(Record<'a>),
    None,
}

impl<'a> RotoType<'a> {
    pub fn is_empty(&self) -> bool {
        matches!(self, RotoType::None)
    }

    pub fn from_literal(s: &str) -> Result<Self, Box<dyn std::error::Error>> {
        match s {
            "U32" => {
                Ok(RotoType::Primitive(RotoPrimitiveType::U32(U32(None))))
            }
            "U8" => Ok(RotoType::Primitive(RotoPrimitiveType::U8(U8(None)))),
            "prefix" => Ok(RotoType::Primitive(RotoPrimitiveType::Prefix(
                Prefix(None),
            ))),
            "IpAddress" => Ok(RotoType::Primitive(
                RotoPrimitiveType::IpAddress(IpAddress(None)),
            )),
            "Asn" => {
                Ok(RotoType::Primitive(RotoPrimitiveType::Asn(Asn(None))))
            }
            "AsPath" => Ok(RotoType::Primitive(RotoPrimitiveType::AsPath(
                AsPath(None),
            ))),
            "Community" => Ok(RotoType::Primitive(
                RotoPrimitiveType::Community(Community(None)),
            )),
            _ => Err(Box::new(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!("Unknown type: {}", s),
            ))),
        }
    }

    pub fn define_record(
        type_ident_pairs: Vec<(&'a str, RotoType<'a>)>,
    ) -> Result<Record<'a>, Box<dyn std::error::Error>> {
        let def_ = type_ident_pairs
            .into_iter()
            .map(|(ident, ty)| (ident, ty.into()))
            .collect();
        Record::new(def_)
    }

    pub fn new_record_type(
        type_ident_pairs: Vec<(&'a str, TypeName<'a>)>,
    ) -> Result<TypeName<'a>, Box<dyn std::error::Error>> {
        let def_ = type_ident_pairs
            .into_iter()
            .map(|(ident, ty)| (ident, Box::new(ty)))
            .collect();
        Ok(TypeName::Record(def_))
    }
}

impl<'a> From<&'a TypeName<'_>> for Box<RotoType<'a>> {
    fn from(t: &'a TypeName<'_>) -> Self {
        match t {
            TypeName::U32 => Box::new(RotoType::Primitive(
                RotoPrimitiveType::U32(U32(None)),
            )),
            TypeName::U8 => {
                Box::new(RotoType::Primitive(RotoPrimitiveType::U8(U8(None))))
            }
            TypeName::Prefix => Box::new(RotoType::Primitive(
                RotoPrimitiveType::Prefix(Prefix(None)),
            )),
            TypeName::IpAddress => Box::new(RotoType::Primitive(
                RotoPrimitiveType::IpAddress(IpAddress(None)),
            )),
            TypeName::Asn => Box::new(RotoType::Primitive(
                RotoPrimitiveType::Asn(Asn(None)),
            )),
            TypeName::AsPath => Box::new(RotoType::Primitive(
                RotoPrimitiveType::AsPath(AsPath(None)),
            )),
            TypeName::Community => Box::new(RotoType::Primitive(
                RotoPrimitiveType::Community(Community(None)),
            )),
            TypeName::List(ty) => {
                Box::new(RotoType::List(ty.as_ref().into()))
            }
            TypeName::Record(kv_list) => {
                let def_ = kv_list
                    .iter()
                    .map(|(ident, ty)| (*ident, ty.as_ref().into()))
                    .collect::<Vec<_>>();
                Box::new(RotoType::Record(Record::new(def_).unwrap()))
            }
            _ => panic!("Unknown type"),
        }
    }
}

/// primitive types
///
///
///
#[derive(Debug)]
pub enum RotoPrimitiveType {
    U32(U32),
    U8(U8),
    Prefix(Prefix),
    Community(Community),
    IpAddress(IpAddress),
    Asn(Asn),
    AsPath(AsPath),
}

impl RotoPrimitiveType {
    pub fn get_type_name(&self) -> &'static str {
        match self {
            RotoPrimitiveType::U32(_) => "U32",
            RotoPrimitiveType::U8(_) => "U8",
            RotoPrimitiveType::Prefix(_) => "Prefix",
            RotoPrimitiveType::Community(_) => "Community",
            RotoPrimitiveType::IpAddress(_) => "IpAddress",
            RotoPrimitiveType::Asn(_) => "Asn",
            RotoPrimitiveType::AsPath(_) => "AsPath",
        }
    }

    pub fn get_value(&self) -> &dyn std::any::Any {
        match self {
            RotoPrimitiveType::U32(val) => val,
            RotoPrimitiveType::U8(val) => val,
            RotoPrimitiveType::Prefix(val) => val,
            RotoPrimitiveType::Community(val) => val,
            RotoPrimitiveType::IpAddress(val) => val,
            RotoPrimitiveType::Asn(val) => val,
            RotoPrimitiveType::AsPath(val) => val,
        }
    }
}

#[derive(Debug)]
pub struct U32(Option<u32>);

impl U32 {
    pub fn new(val: u32) -> Self {
        U32(Some(val))
    }
}

#[derive(Debug)]
pub struct U8(Option<u8>);

impl U8 {
    pub fn new(val: u8) -> Self {
        U8(Some(val))
    }
}

#[derive(Debug)]
pub struct Prefix(Option<routecore::addr::Prefix>);

impl Prefix {
    pub fn new(prefix: routecore::addr::Prefix) -> Self {
        Self(Some(prefix))
    }
}

#[derive(Debug)]
pub enum CommunityType {
    Normal,
    Extended,
    Large,
}

#[derive(Debug)]
pub struct Community(Option<CommunityType>);

impl Community {
    pub fn new(community_type: CommunityType) -> Self {
        Self(Some(community_type))
    }
}

#[derive(Debug)]
pub struct IpAddress(Option<std::net::IpAddr>);

impl IpAddress {
    pub fn new(addr: std::net::IpAddr) -> Self {
        IpAddress(Some(addr))
    }
}

#[derive(Debug)]
pub struct Asn(Option<routecore::asn::Asn>);

impl Asn {
    pub fn new(asn: routecore::asn::Asn) -> Self {
        Asn(Some(asn))
    }
}

#[derive(Debug)]
pub struct AsPath(Option<Vec<Asn>>);

impl AsPath {
    pub fn new(as_path: Vec<Asn>) -> Self {
        AsPath(Some(as_path))
    }
}

/// Collection types
///

pub struct Route<Meta: routecore::record::Meta> {
    pub prefix: Prefix,
    pub bgp: Option<BgpRecord>,
    pub meta: Meta,
}

pub struct BgpRecord {
    pub as_path: AsPath,
    pub communities: Vec<Community>,
}

#[derive(Debug)]
pub enum ElementType<'a> {
    Primitive(RotoPrimitiveType),
    Nested(Box<RotoType<'a>>),
}

impl<'a> From<&'a TypeName<'_>> for ElementType<'a> {
    fn from(t: &'a TypeName<'_>) -> Self {
        match t {
            TypeName::U32 => {
                ElementType::Primitive(RotoPrimitiveType::U32(U32(None)))
            }
            TypeName::U8 => {
                ElementType::Primitive(RotoPrimitiveType::U8(U8(None)))
            }
            TypeName::Prefix => ElementType::Primitive(
                RotoPrimitiveType::Prefix(Prefix(None)),
            ),
            TypeName::IpAddress => ElementType::Primitive(
                RotoPrimitiveType::IpAddress(IpAddress(None)),
            ),
            TypeName::Asn => {
                ElementType::Primitive(RotoPrimitiveType::Asn(Asn(None)))
            }
            TypeName::AsPath => ElementType::Primitive(
                RotoPrimitiveType::AsPath(AsPath(None)),
            ),
            TypeName::Community => ElementType::Primitive(
                RotoPrimitiveType::Community(Community(None)),
            ),
            TypeName::List(ty) => ElementType::Nested(Box::new(
                RotoType::List(ty.as_ref().into()),
            )),
            TypeName::Record(kv_list) => {
                let def_ = kv_list
                    .iter()
                    .map(|(ident, ty)| (*ident, ty.as_ref().into()))
                    .collect::<Vec<_>>();
                ElementType::Nested(Box::new(RotoType::Record(
                    Record::new(def_).unwrap(),
                )))
            }
            _ => panic!("Unknown type"),
        }
    }
}

impl<'a> From<RotoType<'a>> for ElementType<'a> {
    fn from(t: RotoType<'a>) -> Self {
        match t {
            RotoType::Primitive(v) => {
                ElementType::Primitive(v)
            }
            RotoType::List(ty) => ElementType::Nested(Box::new(
                RotoType::List(ty),
            )),
            RotoType::Record(kv_list) => {
                ElementType::Nested(Box::new(RotoType::Record(kv_list)))
            }
            _ => panic!("Unknown type"),
        }
    }
}

#[derive(Debug)]
pub struct List<'a>(Vec<ElementType<'a>>);

impl<'a> List<'a> {
    pub fn new(elem_type: Vec<ElementType<'a>>) -> Self {
        List(elem_type)
    }

    pub fn iter(&self) -> std::slice::Iter<ElementType<'a>> {
        self.0.iter()
    }
}

impl<'a> From<&'a TypeName<'a>> for List<'a> {
    fn from(t: &'a TypeName) -> Self {
        List::new(vec![t.into()])
    }
}

#[derive(Debug)]
pub struct Record<'a>(Vec<(&'a str, ElementType<'a>)>);

impl<'a> Record<'a> {
    pub fn new(
        elems: Vec<(&'a str, ElementType<'a>)>,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        Ok(Self(elems))
    }

    pub fn get_value_for_field(
        &self,
        field: &'a str,
    ) -> Option<&ElementType> {
        self.0.iter().find(|(f, _)| f == &field).map(|(_, v)| v)
    }
}
