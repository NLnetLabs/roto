#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Var(usize),
    IntVar(usize),
    U32,
    U16,
    U8,
    String,
    Bool,
    Prefix,
    PrefixLength,
    AsNumber,
    IpAddress,
    Table(Box<Type>),
    OutputStream(Box<Type>),
    Rib(Box<Type>),
    Record(Vec<(String, Type)>),
    NamedRecord(String, Vec<(String, Type)>),
    Name(String),
}

pub struct Method {
    pub receiver_type: Type,
    pub name: &'static str,
    pub argument_types: &'static [Type],
    pub return_type: Type,
}

impl Method {
    const fn new(
        receiver_type: Type,
        name: &'static str,
        argument_types: &'static [Type],
        return_type: Type,
    ) -> Self {
        Self {
            receiver_type,
            name,
            argument_types,
            return_type,
        }
    }
}

pub static METHODS: &[Method] = &[
    Method::new(Type::Prefix, "address", &[], Type::IpAddress),
    Method::new(Type::Prefix, "exists", &[], Type::Bool),
];
