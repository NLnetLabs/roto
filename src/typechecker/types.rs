use std::fmt::{Debug, Display};

use crate::types::lazyrecord_types::BgpUpdateMessage;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Var(usize),
    ExplicitVar(&'static str),
    IntVar(usize),
    RecordVar(usize, Vec<(String, Type)>),
    Primitive(Primitive),
    List(Box<Type>),
    Table(Box<Type>),
    OutputStream(Box<Type>),
    Rib(Box<Type>),
    Record(Vec<(String, Type)>),
    NamedRecord(String, Vec<(String, Type)>),
    Enum(String, Vec<(String, Option<Type>)>),
    Term(Vec<(String, Type)>),
    Action(Vec<(String, Type)>),
    Filter(Vec<(String, Type)>),
    FilterMap(Vec<(String, Type)>),
    Name(String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Primitive {
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
    AsPath,
    Community,
    OriginType,
    NextHop,
    MultiExitDisc,
    LocalPref,
    AtomicAggregate,
    Aggregator,
    Nlri,
    RouteStatus,
    BgpUpdateMessage
}

impl Into<Type> for Primitive {
    fn into(self) -> Type {
        Type::Primitive(self)
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
        let fmt_args = |args: &[(String, Type)]| {
            args.iter().map(|(_, t)| t.to_string()).collect::<Vec<_>>().join(", ")
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
            Type::Primitive(p) => write!(f, "{p}"),
            Type::List(t) => write!(f, "List<{t}>"),
            Type::Table(t) => write!(f, "Table<{t}>"),
            Type::OutputStream(t) => write!(f, "OutputStream<{t}>"),
            Type::Rib(t) => write!(f, "Rib<{t}>"),
            Type::NamedRecord(x, _) => write!(f, "{x}"),
            Type::Enum(x, _) => write!(f, "{x}"),
            Type::Term(args) => write!(f, "Term({})", fmt_args(args)),
            Type::Action(args) => write!(f, "Action({})", fmt_args(args)),
            Type::Filter(args) => write!(f, "Filter({})", fmt_args(args)),
            Type::FilterMap(args) => write!(f, "Filter({})", fmt_args(args)),
            Type::Name(x) => write!(f, "{x}"),
        }
    }
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
            Type::RecordVar(x, fields) => Type::RecordVar(
                *x,
                fields.into_iter().map(|(n, t)| (n.clone(), f(t))).collect(),
            ),
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
        }
    }
}

pub fn globals() -> Vec<(String, Type)> {
    [
        ("BLACKHOLE", Type::Primitive(Primitive::Community)),
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

pub fn methods() -> Vec<Method> {
    use self::Primitive::*;
    use Type::*;

    vec![
        Method::new(Prefix, "address", &[], &[] as &[Type], IpAddress),
        Method::new(Prefix, "exists", &[], &[] as &[Type], Bool),
        Method::new(Prefix, "len", &[], &[] as &[Type], PrefixLength),
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
            List(Box::new(ExplicitVar("T"))),
            "first",
            &["T"],
            &[] as &[Type],
            ExplicitVar("T")
        ),
        Method::new(
            Table(Box::new(ExplicitVar("T"))),
            "contains",
            &["T"],
            &[ExplicitVar("T")],
            Bool,
        ),
        Method::new(AsPath, "contains", &[], &[AsNumber], Bool),
        Method::new(Prefix, "contains", &[], &[IpAddress], Bool),
        Method::new(Prefix, "covers", &[], &[Prefix], Bool),
        Method::new(Prefix, "is_covered_by", &[], &[Prefix], Bool),
        Method::new(AsPath, "len", &[], &[] as &[Type], U32),
        Method::new(AsPath, "origin", &[], &[] as &[Type], AsNumber),
        Method::new(Nlri, "afi", &[], &[] as &[Type], Type::Name("Afi".into())),
        Method::new(Nlri, "safi", &[], &[] as &[Type], Type::Name("Safi".into())),
    ]
}

pub fn static_methods() -> Vec<Method> {
    use self::Primitive::*;

    vec![
        Method::new(Prefix, "from", &[], &[IpAddress, PrefixLength], Prefix),
        Method::new(
            String,
            "format",
            &["T"],
            &[Type::Primitive(String), Type::ExplicitVar("T")],
            String,
        ),
    ]
}

pub fn default_types() -> Vec<(&'static str, Type)> {
    use Primitive::*;

    let primitives = vec![
        ("U32", U32),
        ("U16", U16),
        ("U8", U8),
        ("Bool", Bool),
        ("String", String),
        ("Prefix", Prefix),
        ("IpAddress", IpAddress),
        ("Asn", AsNumber),
        ("AsPath", AsPath),
        ("OriginType", OriginType),
        ("NextHop", NextHop),
        ("MultiExitDisc", MultiExitDisc),
        ("LocalPref", LocalPref),
        ("AtomicAggregate", AtomicAggregate),
        ("Aggregator", Aggregator),
        ("Community", Community),
        ("Unit", Unit),
        ("Nlri", Nlri),
        ("RouteStatus", RouteStatus),
        ("BgpUpdateMessage", BgpUpdateMessage),
    ];

    let mut types = Vec::new();

    for (n, p) in primitives {
        types.push((n, Type::Primitive(p)))
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
                ("asn", "Asn"),
                ("address", "IpAddress"),
            ],
        ),
        Enum(
            "PeerRibType",
            vec![
                ("InPre", None),
                ("InPost", None),
                ("Loc", None),
                ("OutPre", None),
                ("OutPost", None)
            ]
        ),
        Record(
            "PeerId",
            vec![("addr", "IpAddress"), ("asn", "Asn")]
        ),
        Record(
            "Provenance",
            vec![
                ("timestamp", "U32"),
                ("connection_id", "U32"),
                ("peer-id", "PeerId"),
                ("peer-bgp-id", "U32"),
                ("peer-distuingisher", "U32"),
                ("peer-rib-type", "PeerRibType"),
            ]
        ),
        Record(
            "RouteContext",
            vec![
                ("bgp-msg", "BgpUpdateMessage"),
                ("provenance", "Provenance"),
                ("nlri-status", "RouteStatus")
            ]
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
                ("peer_asn", "Asn"),
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
                ("session_config", "Unit"),
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
        Record("Nlri", vec![("afi", "Afi"), ("safi", "Safi")]),
        Record(
            "BgpUpdateMessage",
            vec![
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
                ("peer_asn", "Asn"),
            ]
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
                        let is_list = field_type.starts_with("[")
                            && field_type.ends_with("]");

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
