use crate::{
    ast::Identifier,
    parser::meta::Meta,
    runtime::{wrap::WrappedFunction, Runtime},
};
use std::{
    any::TypeId,
    fmt::{Debug, Display},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Var(usize),
    ExplicitVar(&'static str),
    IntVar(usize),
    RecordVar(usize, Vec<(String, Type)>),
    Never,
    Primitive(Primitive),
    BuiltIn(&'static str, TypeId),
    Verdict(Box<Type>, Box<Type>),
    List(Box<Type>),
    Table(Box<Type>),
    OutputStream(Box<Type>),
    Rib(Box<Type>),
    Record(Vec<(String, Type)>),
    NamedRecord(String, Vec<(String, Type)>),
    Enum(String, Vec<(String, Option<Type>)>),
    Function(Vec<(Meta<Identifier>, Type)>, Box<Type>),
    Filter(Vec<(Meta<Identifier>, Type)>),
    FilterMap(Vec<(Meta<Identifier>, Type)>),
    Name(String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
}

impl From<Primitive> for Type {
    fn from(value: Primitive) -> Self {
        Type::Primitive(value)
    }
}

// Yes this is abusing Debug, but it's fine
impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fmt_args = |args: &[(_, Type)]| {
            args.iter()
                .map(|(_, t)| t.to_string())
                .collect::<Vec<_>>()
                .join(", ")
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
                write!(f, "Verdict<{a}, {r}>")
            }
            Type::Never => write!(f, "!"),
            Type::Primitive(p) => write!(f, "{p}"),
            Type::BuiltIn(name, _) => write!(f, "{name}"),
            Type::List(t) => write!(f, "List<{t}>"),
            Type::Table(t) => write!(f, "Table<{t}>"),
            Type::OutputStream(t) => write!(f, "OutputStream<{t}>"),
            Type::Rib(t) => write!(f, "Rib<{t}>"),
            Type::NamedRecord(x, _) => write!(f, "{x}"),
            Type::Enum(x, _) => write!(f, "{x}"),
            Type::Function(args, ret) => {
                write!(f, "function({}) -> {}", fmt_args(args), ret)
            }
            Type::Filter(args) => write!(f, "filter({})", fmt_args(args)),
            Type::FilterMap(args) => {
                write!(f, "filter-map({})", fmt_args(args))
            }
            Type::Name(x) => write!(f, "{x}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Arrow {
    pub rec: Type,
    pub args: Vec<Type>,
    pub ret: Type,
    pub function: Option<WrappedFunction>,
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
                n.clone(),
                fields.iter().map(|(n, t)| (n.clone(), f(t))).collect(),
            ),
            other => other.clone(),
        }
    }
}

impl Primitive {
    /// Size of the type in bytes
    pub fn size(&self) -> u32 {
        match self {
            Primitive::U8 => 1,
            Primitive::U16 => 2,
            Primitive::U32 => 4,
            Primitive::U64 => 8,
            Primitive::I8 => 1,
            Primitive::I16 => 2,
            Primitive::I32 => 4,
            Primitive::I64 => 8,
            Primitive::Unit => 0,
            Primitive::String => 4,
            Primitive::Bool => 1,
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
    pub function: Option<WrappedFunction>,
}

impl Method {
    fn new<'a, T>(
        receiver_type: impl Into<Type>,
        name: &'static str,
        vars: &[&'static str],
        argument_types: &'a [T],
        return_type: impl Into<Type>,
    ) -> Self
    where
        T: Into<Type> + Clone + 'a,
    {
        Self {
            receiver_type: receiver_type.into(),
            name,
            vars: vars.to_vec(),
            argument_types: argument_types
                .iter()
                .cloned()
                .map(Into::into)
                .collect(),
            return_type: return_type.into(),
            function: None,
        }
    }
}

pub fn globals() -> Vec<(String, Type)> {
    [
        ("BLACKHOLE", Type::Name("Community".into())),
        ("UNICAST", Type::Name("Safi".into())),
        ("MULTICAST", Type::Name("Safi".into())),
        ("IPV4", Type::Name("Afi".into())),
        ("IPV6", Type::Name("Afi".into())),
        ("VPNV4", Type::Name("Afi".into())),
        ("VPNV6", Type::Name("Afi".into())),
    ]
    .into_iter()
    .map(|(s, t)| (s.into(), t))
    .collect()
}

pub fn methods(rt: &Runtime) -> Vec<Method> {
    use self::Primitive::*;
    use Type::*;

    // All the method defined by the runtime are valid
    let mut m = Vec::new();
    for ty in &rt.types {
        for method in &ty.methods {
            m.push(Method {
                receiver_type: Type::Name(ty.name.into()),
                name: method.name,
                vars: Vec::new(),
                argument_types: method.parameter_types.to_vec(),
                return_type: method.return_type.clone(),
                function: Some(method.wrapped.clone()),
            })
        }
    }

    // TODO: These should be valid but are not defined yet
    let other = vec![
        Method::new(
            OutputStream(Box::new(ExplicitVar("T"))),
            "send",
            &["T"],
            &[ExplicitVar("T")],
            Unit,
        ),
        Method::new(
            ExplicitVar("T"),
            "set",
            &["T"],
            &[ExplicitVar("T")],
            Unit,
        ),
        Method::new(
            List(Box::new(ExplicitVar("T"))),
            "contains",
            &["T"],
            &[ExplicitVar("T")],
            Bool,
        ),
        Method::new(
            Table(Box::new(ExplicitVar("T"))),
            "contains",
            &["T"],
            &[ExplicitVar("T")],
            Bool,
        ),
    ];

    m.extend(other);
    m
}

pub fn static_methods() -> Vec<Method> {
    use self::Primitive::*;

    vec![
        Method::new(
            Type::Name("Prefix".into()),
            "from",
            &[],
            &[
                Type::Name("IpAddress".into()),
                Type::Primitive(Primitive::U8),
            ],
            Type::Name("Prefix".into()),
        ),
        Method::new(
            String,
            "format",
            &["T"],
            &[Type::Primitive(String), Type::ExplicitVar("T")],
            String,
        ),
    ]
}

pub fn default_types(runtime: &Runtime) -> Vec<(&'static str, Type)> {
    use Primitive::*;

    let primitives = vec![
        ("U32", U32),
        ("U16", U16),
        ("U8", U8),
        ("I32", I32),
        ("I16", I16),
        ("I8", I8),
        ("Bool", Bool),
        ("String", String),
        ("Unit", Unit),
    ];

    let mut types = Vec::new();

    for (n, p) in primitives {
        types.push((n, Type::Primitive(p)))
    }

    for ty in &runtime.types {
        types.push((ty.name, Type::BuiltIn(ty.name, ty.type_id)))
    }

    enum RecordOrEnum {
        Record(&'static str, Vec<(&'static str, &'static str)>),
        Enum(&'static str, Vec<(&'static str, Option<&'static str>)>),
    }

    use RecordOrEnum::*;

    let compound_types = vec![
        Record(
            "Header",
            vec![
                ("is_ipv6", "Bool"),
                ("is_ipv4", "Bool"),
                ("is_legacy_format", "Bool"),
                ("is_post_policy", "Bool"),
                ("is_pre_policy", "Bool"),
                ("peer_type", "U8"),
                ("asn", "U32"),
                ("address", "IpAddress"),
            ],
        ),
        Enum(
            "RouteStatus",
            vec![
                ("InConvergence", None),
                ("UpToDate", None),
                ("Stale", None),
                ("StartOfRouteRefresh", None),
                ("Withdrawn", None),
                ("Unparsable", None),
                ("Empty", None),
            ],
        ),
        Record(
            "Route",
            vec![
                ("prefix", "Prefix"),
                ("as-path", "AsPath"),
                ("origin-type", "OriginType"),
                ("next-hop", "NextHop"),
                ("multi-exit-disc", "MultiExitDisc"),
                ("local-pref", "LocalPref"),
                ("atomic-aggregate", "AtomicAggregate"),
                ("aggregator", "Aggregator"),
                ("communities", "[Community]"),
                ("status", "RouteStatus"),
                ("peer_ip", "IpAddress"),
                ("peer_asn", "U32"),
            ],
        ),
        Record("BmpInitiationMessage", vec![]),
        Record(
            "BmpRouteMonitoringMessage",
            vec![("per_peer_header", "Header")],
        ),
        Record(
            "BmpPeerUpNotification",
            vec![
                ("local_address", "IpAddress"),
                ("local_port", "U16"),
                ("remote_port", "U16"),
                // ("session_config", "TODO"),
                ("per_peer_header", "Header"),
            ],
        ),
        Record(
            "BmpPeerDownNotification",
            vec![("per_peer_header", "Header")],
        ),
        Record("BmpStatisticsReport", vec![("per_peer_header", "Header")]),
        Record("BmpTerminationMessage", vec![("per_peer_header", "Header")]),
        Enum(
            "BmpMessage",
            vec![
                ("InitiationMessage", Some("BmpInitiationMessage")),
                ("RouteMonitoring", Some("BmpRouteMonitoringMessage")),
                ("PeerUpNotification", Some("BmpPeerUpNotification")),
                ("PeerDownNotification", Some("BmpPeerDownNotification")),
                ("StatisticsReport", Some("BmpStatisticsReport")),
                ("TerminationMessage", Some("BmpTerminationMessage")),
            ],
        ),
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
        Record(
            "BgpUpdateMessage",
            vec![("nlris", "Nlris"), ("afi", "Afi"), ("safi", "Safi")],
        ),
    ];

    for c in compound_types {
        match c {
            Record(n, fields) => {
                let fields = fields
                    .iter()
                    .map(|(field_name, field_type)| {
                        // Little hack to get list types for now, until that is in the
                        // actual syntax and we can use a real type parser here.
                        let is_list = field_type.starts_with('[')
                            && field_type.ends_with(']');

                        let s = if is_list {
                            &field_type[1..(field_type.len() - 1)]
                        } else {
                            field_type
                        };

                        let mut ty = types
                            .iter()
                            .find(|(n, _)| &s == n)
                            .unwrap()
                            .1
                            .clone();

                        if is_list {
                            ty = Type::List(Box::new(ty));
                        }

                        (field_name.to_string(), ty)
                    })
                    .collect();
                types.push((n, Type::NamedRecord(n.into(), fields)))
            }
            Enum(n, variants) => {
                let variants = variants
                    .iter()
                    .map(|(variant_name, v)| {
                        let v = v.map(|t| {
                            types
                                .iter()
                                .find(|(n, _)| &t == n)
                                .unwrap()
                                .1
                                .clone()
                        });
                        (variant_name.to_string(), v)
                    })
                    .collect();
                types.push((n, Type::Enum(n.into(), variants)))
            }
        }
    }

    types
}
