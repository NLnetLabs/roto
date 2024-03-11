#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Var(usize),
    ExplicitVar(&'static str),
    IntVar(usize),
    U32,
    U16,
    U8,
    Unit,
    String,
    Bool,
    Prefix,
    PrefixLength,
    AsNumber,
    IpAddress,
    List(Box<Type>),
    Table(Box<Type>),
    OutputStream(Box<Type>),
    Rib(Box<Type>),
    Record(Vec<(String, Type)>),
    NamedRecord(String, Vec<(String, Type)>),
    Name(String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Arrow {
    pub rec: Type,
    pub args: Vec<Type>,
    pub ret: Type,
}

impl Type {
    pub fn substitute(&self, from: &Self, to: &Self) -> Self {
        if self == from {
            return to.clone();
        }

        let f = |x: &Self| x.substitute(from, to);

        match self {
            Type::List(x) => Type::List(Box::new(f(x))),
            Type::Table(x) => Type::Table(Box::new(f(x))),
            Type::OutputStream(x) => Type::OutputStream(Box::new(f(x))),
            Type::Rib(x) => Type::Rib(Box::new(f(x))),
            Type::Record(fields) => Type::Record(
                fields.into_iter().map(|(n, t)| (n.clone(), f(t))).collect(),
            ),
            Type::NamedRecord(n, fields) => Type::NamedRecord(
                n.clone(),
                fields.into_iter().map(|(n, t)| (n.clone(), f(t))).collect(),
            ),
            other => other.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Method {
    pub receiver_type: Type,
    pub name: &'static str,
    pub vars: Vec<&'static str>,
    pub argument_types: Vec<Type>,
    pub return_type: Type,
}

impl Method {
    fn new(
        receiver_type: Type,
        name: &'static str,
        vars: &[&'static str],
        argument_types: &[Type],
        return_type: Type,
    ) -> Self {
        Self {
            receiver_type,
            name,
            vars: vars.to_vec(),
            argument_types: argument_types.to_vec(),
            return_type,
        }
    }
}

pub fn default_methods() -> Vec<Method> {
    vec![
        Method::new(Type::Prefix, "address", &[], &[], Type::IpAddress),
        Method::new(Type::Prefix, "exists", &[], &[], Type::Bool),
        Method::new(
            Type::OutputStream(Box::new(Type::ExplicitVar("T"))),
            "send",
            &["T"],
            &[Type::ExplicitVar("T")],
            Type::Unit,
        ),
    ]
}
