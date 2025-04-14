//! Types used by the type checker

use crate::{
    ast::Identifier,
    parser::meta::Meta,
    runtime::{layout::Layout, RuntimeFunctionRef},
    typechecker::scope::{ScopeGraph, ScopeRef},
};
use core::fmt;
use std::{
    any::TypeId,
    borrow::Borrow,
    fmt::{Debug, Display, Write},
    sync::Arc,
};

use super::{scope::ResolvedName, scoped_display::ScopedDisplay};

impl Type {
    pub fn unit() -> Type {
        Type::named("Unit", Vec::new())
    }

    pub fn bool() -> Type {
        Type::named("bool", Vec::new())
    }

    pub fn u8() -> Type {
        Type::named("u8", Vec::new())
    }

    pub fn string() -> Type {
        Type::named("String", Vec::new())
    }

    pub fn asn() -> Type {
        Type::named("Asn", Vec::new())
    }

    pub fn ip_addr() -> Type {
        Type::named("IpAddr", Vec::new())
    }

    pub fn prefix() -> Type {
        Type::named("Prefix", Vec::new())
    }

    pub fn verdict(a: impl Borrow<Type>, b: impl Borrow<Type>) -> Type {
        Type::named("Verdict", vec![a.borrow().clone(), b.borrow().clone()])
    }

    pub fn optional(t: impl Borrow<Type>) -> Type {
        Type::named("Optional", vec![t.borrow().clone()])
    }

    pub fn list(t: impl Borrow<Type>) -> Type {
        Type::named("List", vec![t.borrow().clone()])
    }

    /// Create a named type in the global scope
    pub fn named(ident: impl Into<Identifier>, arguments: Vec<Type>) -> Type {
        Type::Name(TypeName {
            name: ResolvedName {
                scope: ScopeRef::GLOBAL,
                ident: ident.into(),
            },
            arguments,
        })
    }
}

/// Types that the type checker deals with
///
/// This might represent unconcrete types.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Var(usize),
    ExplicitVar(Identifier),
    IntVar(usize),
    FloatVar(usize),
    RecordVar(usize, Vec<(Meta<Identifier>, Type)>),
    Never,
    Record(Vec<(Meta<Identifier>, Type)>),
    Function(Vec<Type>, Box<Type>),
    Name(TypeName),
}

/// A definition of a named type
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeDefinition {
    Enum(TypeName, Vec<EnumVariant>),
    Record(TypeName, Vec<(Meta<Identifier>, Type)>),
    Runtime(ResolvedName, TypeId),
    Primitive(Primitive),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnumVariant {
    pub name: Identifier,
    pub fields: Vec<Type>,
}

/// Primitive Roto types
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Primitive {
    Int(IntKind, IntSize),
    Float(FloatSize),
    Unit,
    String,
    Bool,
    Asn,
    IpAddr,
    Prefix,
}

/// Size of an integer type
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum IntSize {
    I8,
    I16,
    I32,
    I64,
}

/// Whether an integer is signed
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum IntKind {
    Unsigned,
    Signed,
}

/// Size of a floating point type
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum FloatSize {
    F32,
    F64,
}

impl IntSize {
    const fn int(&self) -> u8 {
        match self {
            Self::I8 => 8,
            Self::I16 => 16,
            Self::I32 => 32,
            Self::I64 => 64,
        }
    }
}

impl IntKind {
    fn prefix(&self) -> char {
        match self {
            IntKind::Unsigned => 'u',
            IntKind::Signed => 'i',
        }
    }
}

impl FloatSize {
    const fn int(&self) -> u8 {
        match self {
            Self::F32 => 32,
            Self::F64 => 64,
        }
    }
}

impl From<Primitive> for TypeDefinition {
    fn from(value: Primitive) -> Self {
        TypeDefinition::Primitive(value)
    }
}

impl TypeDefinition {
    /// Get the type name that belongs to this type definition
    pub fn type_name(&self) -> TypeName {
        match self {
            TypeDefinition::Enum(type_name, _) => type_name.clone(),
            TypeDefinition::Record(type_name, _) => type_name.clone(),
            TypeDefinition::Runtime(resolved_name, _) => TypeName {
                name: *resolved_name,
                arguments: Vec::new(),
            },
            TypeDefinition::Primitive(primitive) => TypeName {
                name: ResolvedName {
                    scope: ScopeRef::GLOBAL,
                    ident: primitive.to_string().into(),
                },
                arguments: Vec::new(),
            },
        }
    }

    /// Instantiate the type definition with fresh type variables
    pub fn instantiate(&self, fresh_var: impl FnMut() -> Type) -> Type {
        self.type_name().instantiate(fresh_var)
    }

    /// Get the match patterns for this type definition instantatiated with the
    /// given type arguments.
    pub fn match_patterns(
        &self,
        type_args: &[Type],
    ) -> Option<Vec<EnumVariant>> {
        let TypeDefinition::Enum(type_name, variants) = self else {
            return None;
        };

        assert_eq!(type_name.arguments.len(), type_args.len());

        let subs: Vec<_> =
            type_name.arguments.iter().zip(type_args).collect();

        let mut new_variants = Vec::new();
        for variant in variants {
            new_variants.push(variant.substitute_many(&subs));
        }
        Some(new_variants)
    }
}

impl EnumVariant {
    pub fn substitute_many(&self, subs: &[(&Type, &Type)]) -> Self {
        let fields = self
            .fields
            .iter()
            .map(|t| t.substitute_many(subs))
            .collect();
        EnumVariant {
            name: self.name,
            fields,
        }
    }
}

impl TypeName {
    /// Instantiate a type name with fresh type variables
    pub fn instantiate(&self, mut fresh_var: impl FnMut() -> Type) -> Type {
        let TypeName { name, arguments } = self;
        let arguments = arguments
            .iter()
            .cloned()
            .map(|a| {
                if matches!(a, Type::ExplicitVar(_)) {
                    fresh_var()
                } else {
                    a
                }
            })
            .collect();
        Type::Name(TypeName {
            name: *name,
            arguments,
        })
    }
}

impl Primitive {
    pub fn i32() -> Self {
        Self::Int(IntKind::Signed, IntSize::I32)
    }

    pub fn f64() -> Self {
        Self::Float(FloatSize::F64)
    }
}

impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Primitive::Int(ty, size) =>
                    format!("{}{}", ty.prefix(), size.int()),
                Primitive::Float(size) => format!("f{}", size.int()),
                Primitive::Unit => "Unit".into(),
                Primitive::String => "String".into(),
                Primitive::Bool => "bool".into(),
                Primitive::Asn => "Asn".into(),
                Primitive::IpAddr => "IpAddr".into(),
                Primitive::Prefix => "Prefix".into(),
            }
        )
    }
}

impl ScopedDisplay for Type {
    fn fmt(
        &self,
        graph: &ScopeGraph,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let fmt_args = |args: &[Type]| {
            use std::fmt::Write;
            let mut iter = args.iter();
            let mut s = String::new();
            if let Some(i) = iter.next() {
                write!(s, "{}", i.display(graph))?;
            }
            for i in iter {
                write!(s, ", {}", i.display(graph))?;
            }
            Ok(s)
        };

        match self {
            Type::Var(_) => write!(f, "_"),
            Type::ExplicitVar(s) => write!(f, "{s}"),
            Type::IntVar(_) => write!(f, "{{integer}}"),
            Type::FloatVar(_) => write!(f, "{{float}}"),
            Type::RecordVar(_, fields) | Type::Record(fields) => {
                write!(
                    f,
                    "{{ {} }}",
                    fields
                        .iter()
                        .map(|(s, t)| {
                            format!("{s}: {}", t.display(graph))
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Type::Never => write!(f, "!"),
            Type::Function(args, ret) => {
                write!(
                    f,
                    "function({}) -> {}",
                    fmt_args(args)?,
                    ret.display(graph)
                )
            }
            Type::Name(x) => write!(f, "{}", x.display(graph)),
        }
    }
}

impl ScopedDisplay for TypeDefinition {
    fn fmt(
        &self,
        graph: &ScopeGraph,
        f: &mut std::fmt::Formatter<'_>,
    ) -> core::fmt::Result {
        match self {
            TypeDefinition::Enum(type_name, _) => {
                Display::fmt(&type_name.display(graph), f)
            }
            TypeDefinition::Record(type_name, _) => {
                Display::fmt(&type_name.display(graph), f)
            }
            TypeDefinition::Runtime(resolved_name, _) => {
                Display::fmt(&resolved_name.display(graph), f)
            }
            TypeDefinition::Primitive(primitive) => {
                Display::fmt(primitive, f)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeName {
    pub name: ResolvedName,
    pub arguments: Vec<Type>,
}

impl TypeName {
    fn substitute(&self, from: &Type, to: &Type) -> Self {
        Self {
            name: self.name,
            arguments: self
                .arguments
                .iter()
                .map(|x| x.substitute(from, to))
                .collect(),
        }
    }
}

impl ScopedDisplay for TypeName {
    fn fmt(
        &self,
        graph: &ScopeGraph,
        f: &mut std::fmt::Formatter<'_>,
    ) -> core::fmt::Result {
        Display::fmt(&self.name.display(graph), f)?;
        let mut args = self.arguments.iter();
        if let Some(arg) = args.next() {
            f.write_char('[')?;
            Display::fmt(&arg.display(graph), f)?;
            for arg in args {
                f.write_char(',')?;
                f.write_char(' ')?;
                Display::fmt(&arg.display(graph), f)?;
            }
            f.write_char(']')?;
        }
        Ok(())
    }
}

impl TypeDefinition {
    pub fn is_int(&self) -> bool {
        matches!(self, Self::Primitive(Primitive::Int(_, _)))
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Self::Primitive(Primitive::Float(_)))
    }

    pub fn type_parameters(&self) -> usize {
        match self {
            Self::Enum(type_name, _) | Self::Record(type_name, _) => {
                type_name.arguments.len()
            }
            Self::Runtime(..) | Self::Primitive(..) => 0,
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
            Type::RecordVar(x, fields) => Type::RecordVar(
                *x,
                fields.iter().map(|(n, t)| (n.clone(), f(t))).collect(),
            ),
            Type::Record(fields) => Type::Record(
                fields.iter().map(|(n, t)| (n.clone(), f(t))).collect(),
            ),
            Type::Name(name) => Type::Name(name.substitute(from, to)),
            other => other.clone(),
        }
    }

    pub fn substitute_many(&self, iter: &[(&Self, &Self)]) -> Self {
        let mut me = self.clone();
        for (from, to) in iter {
            me = me.substitute(from, to);
        }
        me
    }
}

impl Primitive {
    /// Layout of the primitive type
    ///
    /// This gives access to the size and alignment
    pub const fn layout(&self) -> Layout {
        use Primitive::*;
        match self {
            Int(_, size) => {
                let bytes = size.int() as usize / 8;
                Layout::new(bytes, bytes)
            }
            Float(size) => {
                let bytes = size.int() as usize / 8;
                Layout::new(bytes, bytes)
            }
            Bool => Layout::new(1, 1),
            Asn => Layout::new(4, 4),
            Unit => Layout::new(0, 1),
            String => Layout::of::<Arc<str>>(),
            IpAddr => Layout::of::<std::net::IpAddr>(),
            Prefix => Layout::of::<inetnum::addr::Prefix>(),
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

/// The list of built-in Roto types
pub fn default_types() -> Vec<(Identifier, TypeDefinition)> {
    use Primitive::*;

    let primitives = vec![
        ("u8", Int(IntKind::Unsigned, IntSize::I8)),
        ("u16", Int(IntKind::Unsigned, IntSize::I16)),
        ("u32", Int(IntKind::Unsigned, IntSize::I32)),
        ("u64", Int(IntKind::Unsigned, IntSize::I64)),
        ("i8", Int(IntKind::Signed, IntSize::I8)),
        ("i16", Int(IntKind::Signed, IntSize::I16)),
        ("i32", Int(IntKind::Signed, IntSize::I32)),
        ("i64", Int(IntKind::Signed, IntSize::I64)),
        ("f32", Float(FloatSize::F32)),
        ("f64", Float(FloatSize::F64)),
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
        types.push((name, TypeDefinition::Primitive(p)))
    }

    struct Enum {
        name: &'static str,
        params: Vec<&'static str>,
        variants: Vec<(&'static str, Vec<Type>)>,
    }

    let compound_types = vec![
        Enum {
            name: "Optional",
            params: vec!["T"],
            variants: vec![
                ("Some", vec![Type::ExplicitVar("T".into())]),
                ("None", vec![]),
            ],
        },
        Enum {
            name: "Verdict",
            params: vec!["A", "R"],
            variants: vec![
                ("Accept", vec![Type::ExplicitVar("A".into())]),
                ("Reject", vec![Type::ExplicitVar("R".into())]),
            ],
        },
        Enum {
            name: "Afi",
            params: vec![],
            variants: vec![
                ("IpV4", vec![]),
                ("IpV6", vec![]),
                ("VpnV4", vec![]),
                ("VpnV6", vec![]),
            ],
        },
        Enum {
            name: "Safi",
            params: vec![],
            variants: vec![("Unicast", vec![]), ("Multicast", vec![])],
        },
    ];

    for Enum {
        name,
        params,
        variants,
    } in compound_types
    {
        let ident = Identifier::from(name);
        let name = ResolvedName {
            scope: ScopeRef::GLOBAL,
            ident,
        };

        let params: Vec<_> =
            params.into_iter().map(Identifier::from).collect();

        let param_types =
            params.iter().map(|p| Type::ExplicitVar(*p)).collect();

        let variants = variants
            .into_iter()
            .map(|(variant_name, fields)| {
                let name = Identifier::from(variant_name);
                EnumVariant { name, fields }
            })
            .collect();

        let type_name = TypeName {
            name,
            arguments: param_types,
        };

        types.push((ident, TypeDefinition::Enum(type_name, variants)))
    }

    types
}
