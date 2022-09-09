/// Roto Types
///
/// This module contains the types offered by the Roto languages.

#[derive(Debug)]
pub enum RotoType<'a, const T: usize> {
    Primitive(RotoPrimitiveType),
    List(List<'a, T>),
    Record(Record<'a, T>),
    Empty,
}

impl<'a, const T: usize> RotoType<'a, T> {
    pub fn is_empty(&self) -> bool {
        matches!(self, RotoType::Empty)
    }

    pub fn from_literal(s: &str) -> Result<Self, Box<dyn std::error::Error>> {
        match s {
            "u32" => {
                Ok(RotoType::Primitive(RotoPrimitiveType::U32(U32(None))))
            }
            "u8" => Ok(RotoType::Primitive(RotoPrimitiveType::U8(U8(None)))),
            "prefix" => Ok(RotoType::Primitive(RotoPrimitiveType::Prefix(
                Prefix(None),
            ))),
            "ip_address" => Ok(RotoType::Primitive(
                RotoPrimitiveType::IpAddress(IpAddress(None)),
            )),
            "asn" => {
                Ok(RotoType::Primitive(RotoPrimitiveType::Asn(Asn(None))))
            }
            "as_path" => Ok(RotoType::Primitive(RotoPrimitiveType::AsPath(
                AsPath(None),
            ))),
            "community" => Ok(RotoType::Primitive(
                RotoPrimitiveType::Community(Community::Normal),
            )),
            _ => Err(Box::new(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!("Unknown type: {}", s),
            ))),
        }
    }

    pub fn define(
        type_ident_pairs: Vec<(&'static str, &'a str)>,
    ) -> Result<Record<'a, T>, Box<dyn std::error::Error>> {
        let def_ = type_ident_pairs
            .into_iter()
            .map(|(ident, ty)| {
                (ident, Box::new(RotoType::from_literal(ty).unwrap()))
            })
            .collect();
        Record::new(def_)
    }
}

/// primitive types
///
///
///
#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct U32(Option<u32>);

impl U32 {
    pub fn new(val: u32) -> Self {
        U32(Some(val))
    }
}

#[derive(Debug, Clone)]
pub struct U8(Option<u8>);

impl U8 {
    pub fn new(val: u8) -> Self {
        U8(Some(val))
    }
}

#[derive(Debug, Clone)]
pub struct Prefix(Option<routecore::addr::Prefix>);

impl Prefix {
    pub fn new(prefix: routecore::addr::Prefix) -> Self {
        Self(Some(prefix))
    }
}

#[derive(Debug, Clone)]
pub enum Community {
    Normal,
    Extended,
    Large,
}

#[derive(Debug, Clone)]
pub struct IpAddress(Option<std::net::IpAddr>);

impl IpAddress {
    pub fn new(addr: std::net::IpAddr) -> Self {
        IpAddress(Some(addr))
    }
}

#[derive(Debug, Clone)]
pub struct Asn(Option<routecore::asn::Asn>);

impl Asn {
    pub fn new(asn: routecore::asn::Asn) -> Self {
        Asn(Some(asn))
    }
}

#[derive(Debug, Clone)]
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
pub struct List<'a, const T: usize>(Vec<RotoType<'a, T>>);

impl<'a, const T: usize> List<'a, T> {
    pub fn new(list: Vec<RotoType<'a, T>>) -> Self {
        List(list)
    }

    pub fn iter(&self) -> std::slice::Iter<RotoType<T>> {
        self.0.iter()
    }
}

#[derive(Debug)]
pub struct Record<'a, const T: usize>([(&'a str, Box<RotoType<'a, T>>); T]);

impl<'a, const T: usize> Record<'a, T> {
    fn new(
        elems: Vec<(&'a str, Box<RotoType<'a, T>>)>,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        if elems.len() != T {
            return Err(Box::new(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!(
                    "wrong number of elements, got {} expected {}",
                    elems.len(),
                    T
                ),
            )));
        }

        Ok(Self(elems.try_into().unwrap()))
    }

    pub fn get_value_by_field(&self, field: &'a str) -> Option<&Box<RotoType<T>>> {
        self.0.iter().find(|(f, _)| f == &field).map(|(_, v)| v)
    }
}
