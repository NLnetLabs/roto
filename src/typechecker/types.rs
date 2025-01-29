use crate::{
    ast::Identifier,
    parser::meta::{Meta, MetaId},
    runtime::RuntimeFunctionRef,
    typechecker::scope::ScopeRef,
};
use std::{
    any::TypeId,
    fmt::{Debug, Display},
};

use super::scope::ResolvedName;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Var(usize),
    ExplicitVar(Identifier),
    IntVar(usize),
    RecordVar(usize, Vec<(Meta<Identifier>, Type)>),
    Never,
    Primitive(Primitive),
    BuiltIn(ResolvedName, TypeId),
    Verdict(Box<Type>, Box<Type>),
    List(Box<Type>),
    Record(Vec<(Meta<Identifier>, Type)>),
    NamedRecord(ResolvedName, Vec<(Meta<Identifier>, Type)>),
    Enum(ResolvedName, Vec<(Identifier, Option<Type>)>),
    Function(Vec<(Meta<Identifier>, Type)>, Box<Type>),
    Filter(Vec<(Meta<Identifier>, Type)>),
    FilterMap(Vec<(Meta<Identifier>, Type)>),
    Name(ResolvedName),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Primitive {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    Unit,
    String,
    Bool,
    Asn,
    IpAddr,
    Prefix,
}

impl From<Primitive> for Type {
    fn from(value: Primitive) -> Self {
        Type::Primitive(value)
    }
}

impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Primitive::U8 => "u8",
                Primitive::U16 => "u16",
                Primitive::U32 => "u32",
                Primitive::U64 => "u64",
                Primitive::I8 => "i8",
                Primitive::I16 => "i16",
                Primitive::I32 => "i32",
                Primitive::I64 => "i64",
                Primitive::Unit => "Unit",
                Primitive::String => "String",
                Primitive::Bool => "bool",
                Primitive::Asn => "Asn",
                Primitive::IpAddr => "IpAddr",
                Primitive::Prefix => "Prefix",
            }
        )
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fmt_args = |args: &[(_, Type)]| {
            use std::fmt::Write;
            let mut iter = args.iter();
            let mut s = String::new();
            if let Some((_, i)) = iter.next() {
                write!(s, "{i}")?;
            }
            for (_, i) in iter {
                write!(s, ", {i}")?;
            }
            Ok(s)
        };

        match self {
            Type::Var(_) => write!(f, "{{unknown}}"),
            Type::ExplicitVar(s) => write!(f, "{s}"),
            Type::IntVar(_) => write!(f, "{{integer}}"),
            Type::RecordVar(_, fields) | Type::Record(fields) => write!(
                f,
                "{{ {} }}",
                fields
                    .iter()
                    .map(|(s, t)| { format!("\"{s}\": {t}") })
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Type::Verdict(a, r) => {
                write!(f, "Verdict<{a}, {r}>",)
            }
            Type::Never => write!(f, "!"),
            Type::Primitive(p) => write!(f, "{p}"),
            Type::BuiltIn(name, _) => write!(f, " {}", name.ident),
            Type::List(t) => write!(f, "List<{t}>"),
            Type::NamedRecord(x, _) => write!(f, "{}", x.ident),
            Type::Enum(x, _) => write!(f, "{}", x.ident),
            Type::Function(args, ret) => {
                write!(f, "function({}) -> {ret}", fmt_args(args)?)
            }
            Type::Filter(args) => write!(f, "filter({})", fmt_args(args)?),
            Type::FilterMap(args) => {
                write!(f, "filter-map({})", fmt_args(args)?)
            }
            Type::Name(x) => write!(f, "{}", x.ident),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Signature {
    pub kind: FunctionKind,
    pub parameter_types: Vec<Type>,
    pub return_type: Type,
}

impl Type {
    pub fn substitute(&self, from: &Self, to: &Self) -> Self {
        if self == from {
            return to.clone();
        }

        let f = |x: &Self| x.substitute(from, to);

        match self {
            Type::List(x) => Type::List(Box::new(f(x))),
            Type::Verdict(a, r) => {
                Type::Verdict(Box::new(f(a)), Box::new(f(r)))
            }
            Type::RecordVar(x, fields) => Type::RecordVar(
                *x,
                fields.iter().map(|(n, t)| (n.clone(), f(t))).collect(),
            ),
            Type::Record(fields) => Type::Record(
                fields.iter().map(|(n, t)| (n.clone(), f(t))).collect(),
            ),
            Type::NamedRecord(n, fields) => Type::NamedRecord(
                *n,
                fields.iter().map(|(n, t)| (n.clone(), f(t))).collect(),
            ),
            other => other.clone(),
        }
    }
}

impl Primitive {
    /// Size of the type in bytes
    pub fn size(&self) -> u32 {
        use Primitive::*;
        match self {
            U8 | I8 | Bool => 1,
            U16 | I16 => 2,
            U32 | I32 | Asn => 4,
            U64 | I64 => 8,
            Unit => 0,
            String => 4,
            IpAddr => std::mem::size_of::<std::net::IpAddr>() as u32,
            Prefix => std::mem::size_of::<inetnum::addr::Prefix>() as u32,
        }
    }
}

/// The definition of a function from several different sources.
///
/// This is used to extract the function pointer and any other information
/// required to generate the code to call this function.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum FunctionDefinition {
    Runtime(RuntimeFunctionRef),
    Roto,
}

/// A function that can be called from Roto
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
    /// The type signature of this function
    pub signature: Signature,

    /// Function name
    pub name: ResolvedName,

    /// Type variables of this function
    pub vars: Vec<Identifier>,

    /// The source of this function
    pub definition: FunctionDefinition,
}

/// How a function should be called in Roto: as a free function or a
/// (static) method.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FunctionKind {
    Free,
    Method(Type),
    StaticMethod(Type),
}

impl Function {
    pub fn new<T: Into<Type>>(
        kind: FunctionKind,
        name: ResolvedName,
        vars: &[Identifier],
        parameter_types: impl IntoIterator<Item = T>,
        return_type: impl Into<Type>,
        definition: FunctionDefinition,
    ) -> Self {
        Self {
            name,
            vars: vars.to_vec(),
            signature: Signature {
                kind,
                parameter_types: parameter_types
                    .into_iter()
                    .map(Into::into)
                    .collect(),
                return_type: return_type.into(),
            },
            definition,
        }
    }
}

pub fn default_types() -> Vec<(Identifier, Type)> {
    use Primitive::*;

    let primitives = vec![
        ("u8", U8),
        ("u16", U16),
        ("u32", U32),
        ("u64", U64),
        ("i8", I8),
        ("i16", I16),
        ("i32", I32),
        ("i64", I64),
        ("bool", Bool),
        ("String", String),
        ("Unit", Unit),
        ("Asn", Asn),
        ("IpAddr", IpAddr),
        ("Prefix", Prefix),
    ];

    let mut types = Vec::new();

    for (n, p) in primitives {
        let name = Identifier::from(n);
        types.push((name, Type::Primitive(p)))
    }

    enum RecordOrEnum {
        Record(&'static str, Vec<(&'static str, &'static str)>),
        Enum(&'static str, Vec<(&'static str, Option<&'static str>)>),
    }

    use RecordOrEnum::*;

    let compound_types = vec![
        Enum(
            "Afi",
            vec![
                ("IpV4", None),
                ("IpV6", None),
                ("VpnV4", None),
                ("VpnV6", None),
            ],
        ),
        Enum("Safi", vec![("Unicast", None), ("Multicast", None)]),
        Record("Nlris", vec![("afi", "Afi"), ("safi", "Safi")]),
    ];

    for c in compound_types {
        match c {
            Record(n, fields) => {
                let ident = Identifier::from(n);
                let name = ResolvedName {
                    scope: ScopeRef::GLOBAL,
                    ident,
                };
                let fields = fields
                    .iter()
                    .map(|(field_name, field_type)| {
                        let field_name = Identifier::from(*field_name);

                        // Little hack to get list types for now, until that is in the
                        // actual syntax and we can use a real type parser here.
                        let is_list = field_type.starts_with('[')
                            && field_type.ends_with(']');

                        let s = if is_list {
                            &field_type[1..(field_type.len() - 1)]
                        } else {
                            field_type
                        };

                        let s = Identifier::from(s);

                        let mut ty = types
                            .iter()
                            .find(|(n, _)| s == *n)
                            .unwrap_or_else(|| panic!("Not found: {s}",))
                            .1
                            .clone();

                        if is_list {
                            ty = Type::List(Box::new(ty));
                        }

                        (
                            Meta {
                                id: MetaId(0),
                                node: field_name,
                            },
                            ty,
                        )
                    })
                    .collect();
                types.push((ident, Type::NamedRecord(name, fields)))
            }
            Enum(n, variants) => {
                let ident = Identifier::from(n);
                let name = ResolvedName {
                    scope: ScopeRef::GLOBAL,
                    ident,
                };
                let variants = variants
                    .iter()
                    .map(|(variant_name, v)| {
                        let variant_name = Identifier::from(*variant_name);
                        let v = v.map(|t| {
                            let t = Identifier::from(t);
                            types
                                .iter()
                                .find(|(n, _)| t == *n)
                                .unwrap()
                                .1
                                .clone()
                        });
                        (variant_name, v)
                    })
                    .collect();
                types.push((ident, Type::Enum(name, variants)))
            }
        }
    }

    types
}
