/// Roto Types
///
/// This module contains the types offered by the Roto languages.

//------------ RotoType -----------------------------------------------------

// These are all the types the user can create. This enum is used to create 
// `user defined` types.

#[derive(Debug)]
pub enum RotoType<'a> {
    List(Box<RotoType<'a>>),
    Record(Vec<(&'a str, Box<RotoType<'a>>)>),
    U32,
    U8,
    Prefix,
    IpAddress,
    Asn,
    AsPath,
    Community,
    None,
}

impl<'a> RotoType<'a> {
    pub fn new_record_type(
        type_ident_pairs: Vec<(&'a str, RotoType<'a>)>,
    ) -> Result<RotoType<'a>, Box<dyn std::error::Error>> {
        let def_ = type_ident_pairs
            .into_iter()
            .map(|(ident, ty)| (ident, Box::new(ty)))
            .collect();
        Ok(RotoType::Record(def_))
    }

    fn _check_record_fields(&self, fields: &[(&str, RotoTypeValue)]) -> bool {
        if let RotoType::Record(rec) = self {
            for (name, ty) in fields {
                if !rec.iter().any(|(k, v)| {
                    k == name && v.as_ref() == ty
                }) {
                    return false;
                }
            }
            true
        } else {
            false
        }
    }
}

impl PartialEq<RotoPrimitiveType> for RotoType<'_> {
    fn eq(&self, other: &RotoPrimitiveType) -> bool {
        match self {
            RotoType::U32 => {
                matches!(other, RotoPrimitiveType::U32(_))
            }
            RotoType::U8 => {
                matches!(other, RotoPrimitiveType::U8(_))
            }
            RotoType::Prefix => {
                matches!(other, RotoPrimitiveType::Prefix(_))
            }
            RotoType::IpAddress => {
                matches!(other, RotoPrimitiveType::IpAddress(_))
            }
            RotoType::Asn => {
                matches!(other, RotoPrimitiveType::Asn(_))
            }
            RotoType::AsPath => {
                matches!(other, RotoPrimitiveType::AsPath(_))
            }
            RotoType::Community => {
                matches!(other, RotoPrimitiveType::Community(_))
            }
            _ => false,
        }
    }
}

impl PartialEq<RotoTypeValue<'_>> for RotoType<'_> {
    fn eq(&self, other: &RotoTypeValue) -> bool {
        match (self, other) {
            (a, RotoTypeValue::Primitive(b)) => a == b,
            (RotoType::List(a), RotoTypeValue::List(b)) => {
                match (a.as_ref(), b) {
                    (RotoType::List(aa), List(bb)) => match &bb[0] {
                        ElementType::Nested(bb) => {
                            return aa.as_ref() == bb.as_ref()
                        }
                        ElementType::Primitive(bb) => {
                            return aa.as_ref() == bb
                        }
                    },
                    _ => false,
                }
            }
            (RotoType::Record(a), RotoTypeValue::Record(_b)) => {
                self._check_record_fields(
                    a.iter()
                        .map(|ty| (ty.0, ty.1.as_ref().into()))
                        .collect::<Vec<_>>()
                        .as_slice(),
                )
            }
            _ => false,
        }
    }
}


//------------ RotoTypeValue ------------------------------------------------

/// These are the actual types that are used in the Roto language. This enum
/// holds both the type-level information and the value. The collection
/// variants can hold multiple values recursively, e.g. a List of Records.

#[derive(Debug, PartialEq)]
pub enum RotoTypeValue<'a> {
     // All the built-in scalars
    Primitive(RotoPrimitiveType),
    // An ordered list of one type
    List(List<'a>),
    // A map of (key, value) pairs, where value can be any of the other types
    Record(Record<'a>),
    None,
}

impl<'a> RotoTypeValue<'a> {
    pub fn is_empty(&self) -> bool {
        matches!(self, RotoTypeValue::None)
    }

    pub fn from_literal(s: &str) -> Result<Self, Box<dyn std::error::Error>> {
        match s {
            "U32" => Ok(RotoTypeValue::Primitive(RotoPrimitiveType::U32(
                U32(None),
            ))),
            "U8" => {
                Ok(RotoTypeValue::Primitive(RotoPrimitiveType::U8(U8(None))))
            }
            "prefix" => Ok(RotoTypeValue::Primitive(
                RotoPrimitiveType::Prefix(Prefix(None)),
            )),
            "IpAddress" => Ok(RotoTypeValue::Primitive(
                RotoPrimitiveType::IpAddress(IpAddress(None)),
            )),
            "Asn" => Ok(RotoTypeValue::Primitive(RotoPrimitiveType::Asn(
                Asn(None),
            ))),
            "AsPath" => Ok(RotoTypeValue::Primitive(
                RotoPrimitiveType::AsPath(AsPath(None)),
            )),
            "Community" => Ok(RotoTypeValue::Primitive(
                RotoPrimitiveType::Community(Community(None)),
            )),
            _ => Err(Box::new(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!("Unknown type: {}", s),
            ))),
        }
    }

    fn create_record(
        type_ident_pairs: Vec<(&'a str, RotoTypeValue<'a>)>,
    ) -> Result<Record<'a>, Box<dyn std::error::Error>> {
        let def_ = type_ident_pairs
            .into_iter()
            .map(|(ident, ty)| (ident, ty.into()))
            .collect();
        Record::new(def_)
    }
    
}

impl<'a> From<&'a RotoType<'_>> for Box<RotoTypeValue<'a>> {
    fn from(t: &'a RotoType<'_>) -> Self {
        match t {
            RotoType::U32 => Box::new(RotoTypeValue::Primitive(
                RotoPrimitiveType::U32(U32(None)),
            )),
            RotoType::U8 => Box::new(RotoTypeValue::Primitive(
                RotoPrimitiveType::U8(U8(None)),
            )),
            RotoType::Prefix => Box::new(RotoTypeValue::Primitive(
                RotoPrimitiveType::Prefix(Prefix(None)),
            )),
            RotoType::IpAddress => Box::new(RotoTypeValue::Primitive(
                RotoPrimitiveType::IpAddress(IpAddress(None)),
            )),
            RotoType::Asn => Box::new(RotoTypeValue::Primitive(
                RotoPrimitiveType::Asn(Asn(None)),
            )),
            RotoType::AsPath => Box::new(RotoTypeValue::Primitive(
                RotoPrimitiveType::AsPath(AsPath(None)),
            )),
            RotoType::Community => Box::new(RotoTypeValue::Primitive(
                RotoPrimitiveType::Community(Community(None)),
            )),
            RotoType::List(ty) => {
                Box::new(RotoTypeValue::List(ty.as_ref().into()))
            }
            RotoType::Record(kv_list) => {
                let def_ = kv_list
                    .iter()
                    .map(|(ident, ty)| (*ident, ty.as_ref().into()))
                    .collect::<Vec<_>>();
                Box::new(RotoTypeValue::Record(Record::new(def_).unwrap()))
            }
            _ => panic!("Unknown type"),
        }
    }
}

impl<'a> From<&'a RotoType<'_>> for RotoTypeValue<'a> {
    fn from(t: &'a RotoType<'_>) -> Self {
        match t {
            RotoType::U32 => {
                RotoTypeValue::Primitive(RotoPrimitiveType::U32(U32(None)))
            }
            RotoType::U8 => {
                RotoTypeValue::Primitive(RotoPrimitiveType::U8(U8(None)))
            }
            RotoType::Prefix => RotoTypeValue::Primitive(
                RotoPrimitiveType::Prefix(Prefix(None)),
            ),
            RotoType::IpAddress => RotoTypeValue::Primitive(
                RotoPrimitiveType::IpAddress(IpAddress(None)),
            ),
            RotoType::Asn => {
                RotoTypeValue::Primitive(RotoPrimitiveType::Asn(Asn(None)))
            }
            RotoType::AsPath => RotoTypeValue::Primitive(
                RotoPrimitiveType::AsPath(AsPath(None)),
            ),
            RotoType::Community => RotoTypeValue::Primitive(
                RotoPrimitiveType::Community(Community(None)),
            ),
            RotoType::List(ty) => RotoTypeValue::List(ty.as_ref().into()),
            RotoType::Record(kv_list) => {
                let def_ = kv_list
                    .iter()
                    .map(|(ident, ty)| (*ident, ty.as_ref().into()))
                    .collect::<Vec<_>>();
                RotoTypeValue::Record(Record::new(def_).unwrap())
            }
            _ => panic!("Unknown type"),
        }
    }
}

//------------ RotoPrimitiveType --------------------------------------------

// The built-in types

#[derive(Debug, PartialEq)]
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

    pub fn create_instance<'a>(
        ty: RotoType,
        value: impl Into<RotoPrimitiveType>,
    ) -> Result<RotoTypeValue<'a>, Box<dyn std::error::Error>> {
        let var = match ty {
            RotoType::U32 => {
                if let RotoPrimitiveType::U32(v) = value.into() {
                    RotoPrimitiveType::U32(v)
                } else {
                    return Err("Not a u32".into());
                }
            }
            RotoType::U8 => {
                if let RotoPrimitiveType::U8(v) = value.into() {
                    RotoPrimitiveType::U8(v)
                } else {
                    return Err("Not a u8".into());
                }
            }
            RotoType::Prefix => {
                if let RotoPrimitiveType::Prefix(v) = value.into() {
                    RotoPrimitiveType::Prefix(v)
                } else {
                    return Err("Not a prefix".into());
                }
            }
            RotoType::IpAddress => {
                if let RotoPrimitiveType::IpAddress(v) = value.into() {
                    RotoPrimitiveType::IpAddress(v)
                } else {
                    return Err("Not an IP address".into());
                }
            }
            RotoType::Asn => {
                if let RotoPrimitiveType::Asn(v) = value.into() {
                    RotoPrimitiveType::Asn(v)
                } else {
                    return Err("Not an ASN".into());
                }
            }
            RotoType::AsPath => {
                if let RotoPrimitiveType::AsPath(v) = value.into() {
                    RotoPrimitiveType::AsPath(v)
                } else {
                    return Err("Not an AS Path".into());
                }
            }
            RotoType::Community => {
                if let RotoPrimitiveType::Community(v) = value.into() {
                    RotoPrimitiveType::Community(v)
                } else {
                    return Err("Not a community".into());
                }
            }
            _ => return Err("Not a primitive type".into()),
        };
        Ok(RotoTypeValue::Primitive(var))
    }
}

// These From impls allow the user to use the create_instance function with
// simple types like u32, u8, etc. (without the nested variants).

impl From<Asn> for RotoPrimitiveType {
    fn from(val: Asn) -> Self {
        RotoPrimitiveType::Asn(val)
    }
}

impl From<u32> for RotoPrimitiveType {
    fn from(val: u32) -> Self {
        RotoPrimitiveType::U32(U32(Some(val)))
    }
}

impl From<std::net::IpAddr> for RotoPrimitiveType {
    fn from(val: std::net::IpAddr) -> Self {
        RotoPrimitiveType::IpAddress(IpAddress(Some(val)))
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
pub enum ElementType<'a> {
    Primitive(RotoPrimitiveType),
    Nested(Box<RotoTypeValue<'a>>),
}

impl<'a> From<&'a RotoType<'_>> for ElementType<'a> {
    fn from(t: &'a RotoType<'_>) -> Self {
        match t {
            RotoType::U32 => {
                ElementType::Primitive(RotoPrimitiveType::U32(U32(None)))
            }
            RotoType::U8 => {
                ElementType::Primitive(RotoPrimitiveType::U8(U8(None)))
            }
            RotoType::Prefix => ElementType::Primitive(
                RotoPrimitiveType::Prefix(Prefix(None)),
            ),
            RotoType::IpAddress => ElementType::Primitive(
                RotoPrimitiveType::IpAddress(IpAddress(None)),
            ),
            RotoType::Asn => {
                ElementType::Primitive(RotoPrimitiveType::Asn(Asn(None)))
            }
            RotoType::AsPath => ElementType::Primitive(
                RotoPrimitiveType::AsPath(AsPath(None)),
            ),
            RotoType::Community => ElementType::Primitive(
                RotoPrimitiveType::Community(Community(None)),
            ),
            RotoType::List(ty) => ElementType::Nested(Box::new(
                RotoTypeValue::List(ty.as_ref().into()),
            )),
            RotoType::Record(kv_list) => {
                let def_ = kv_list
                    .iter()
                    .map(|(ident, ty)| (*ident, ty.as_ref().into()))
                    .collect::<Vec<_>>();
                ElementType::Nested(Box::new(RotoTypeValue::Record(
                    Record::new(def_).unwrap(),
                )))
            }
            _ => panic!("Unknown type"),
        }
    }
}

impl<'a> From<RotoTypeValue<'a>> for ElementType<'a> {
    fn from(t: RotoTypeValue<'a>) -> Self {
        match t {
            RotoTypeValue::Primitive(v) => ElementType::Primitive(v),
            RotoTypeValue::List(ty) => {
                ElementType::Nested(Box::new(RotoTypeValue::List(ty)))
            }
            RotoTypeValue::Record(kv_list) => {
                ElementType::Nested(Box::new(RotoTypeValue::Record(kv_list)))
            }
            _ => panic!("Unknown type"),
        }
    }
}


//------------ Collections: List type ------------------------------------------------

#[derive(Debug, PartialEq)]
pub struct List<'a>(Vec<ElementType<'a>>);

impl<'a> List<'a> {
    pub fn new(elem_type: Vec<ElementType<'a>>) -> Self {
        List(elem_type)
    }

    pub fn iter(&self) -> std::slice::Iter<ElementType<'a>> {
        self.0.iter()
    }
}

impl<'a> From<&'a RotoType<'a>> for List<'a> {
    fn from(t: &'a RotoType) -> Self {
        List::new(vec![t.into()])
    }
}

//---------------- Collections: Record type --------------------------------------------

#[derive(Debug, PartialEq)]
pub struct Record<'a>(Vec<(&'a str, ElementType<'a>)>);

impl<'a> Record<'a> {
    pub fn new(
        elems: Vec<(&'a str, ElementType<'a>)>,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        Ok(Self(elems))
    }

    pub fn create_instance(
        ty: &RotoType,
        kvs: Vec<(&'a str, RotoTypeValue<'a>)>,
    ) -> Result<Record<'a>, Box<dyn std::error::Error>> {
        if let RotoType::Record(_rec) = ty {
            if ty._check_record_fields(kvs.as_slice()) {
                RotoTypeValue::create_record(kvs)
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
    ) -> Option<&ElementType> {
        self.0.iter().find(|(f, _)| f == &field).map(|(_, v)| v)
    }
}
