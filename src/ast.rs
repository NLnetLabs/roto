use crate::parser::span::Spanned;

#[derive(Clone, Debug)]
pub struct SyntaxTree {
    pub expressions: Vec<Declaration>,
}

#[derive(Clone, Debug)]
pub enum Declaration {
    FilterMap(Box<FilterMap>),
    Rib(Rib),
    Table(Table),
    OutputStream(OutputStream),
    Record(RecordTypeDeclaration),
}

/// The value of a typed record
#[derive(Clone, Debug)]
pub struct RecordTypeDeclaration {
    pub ident: Spanned<Identifier>,
    pub record_type: RecordType,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FilterType {
    FilterMap,
    Filter,
}

#[derive(Clone, Debug)]
pub struct FilterMap {
    pub filter_type: FilterType,
    pub ident: Spanned<Identifier>,
    pub params: Spanned<Vec<(Spanned<Identifier>, Spanned<Identifier>)>>,
    pub body: FilterMapBody,
}

#[derive(Clone, Debug)]
pub struct FilterMapBody {
    pub define: Define,
    pub expressions: Vec<FilterMapExpr>,
    pub apply: Spanned<Block>,
}

/// These are the sections that can appear multiple times in a Filter(Map)
#[derive(Clone, Debug)]
pub enum FilterMapExpr {
    Term(TermDeclaration),
    Action(ActionDeclaration),
}

#[derive(Clone, Debug)]
pub struct Define {
    pub body: DefineBody,
}

#[derive(Clone, Debug)]
pub enum RxTxType {
    RxOnly(Spanned<Identifier>, Spanned<Identifier>),
    Split {
        rx: (Spanned<Identifier>, Spanned<Identifier>),
        tx: (Spanned<Identifier>, Spanned<Identifier>),
    },
}

#[derive(Clone, Debug)]
pub struct DefineBody {
    pub rx_tx_type: RxTxType,
    pub assignments: Vec<(Spanned<Identifier>, Spanned<Expr>)>,
}

#[derive(Clone, Debug)]
pub struct TermDeclaration {
    pub ident: Spanned<Identifier>,
    pub params: Spanned<Vec<(Spanned<Identifier>, Spanned<Identifier>)>>,
    pub body: Spanned<Block>,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub exprs: Vec<Spanned<Expr>>,
    pub last: Option<Box<Spanned<Expr>>>,
}

#[derive(Clone, Debug)]
pub enum Expr {
    /// a literal, or a chain of field accesses and/or methods on a literal,
    /// e.g. `10.0.0.0/8.covers(..)`
    Literal(Spanned<Literal>),
    Match(Box<Match>),
    /// a JunOS style prefix match expression, e.g. `0.0.0.0/0
    /// prefix-length-range /12-/16`
    PrefixMatch(PrefixMatchExpr),
    FunctionCall(Spanned<Identifier>, Spanned<Vec<Spanned<Expr>>>),
    MethodCall(
        Box<Spanned<Expr>>,
        Spanned<Identifier>,
        Spanned<Vec<Spanned<Expr>>>,
    ),
    Access(Box<Spanned<Expr>>, Spanned<Identifier>),
    Var(Spanned<Identifier>),
    /// a record that doesn't have a type mentioned in the assignment of it,
    /// e.g `{ value_1: 100, value_2: "bla" }`. This can also be a sub-record
    /// of a record that does have an explicit type.
    Record(Spanned<Record>),
    /// an expression of a record that does have a type, e.g. `MyType {
    /// value_1: 100, value_2: "bla" }`, where MyType is a user-defined Record
    /// Type.
    TypedRecord(Spanned<Identifier>, Spanned<Record>),
    /// An expression that yields a list of values, e.g. `[100, 200, 300]`
    List(Vec<Spanned<Expr>>),
    Not(Box<Spanned<Expr>>),
    BinOp(Box<Spanned<Expr>>, BinOp, Box<Spanned<Expr>>),
    Return(Box<Spanned<Expr>>),
    IfElse(Box<Spanned<Expr>>, Spanned<Block>, Option<Spanned<Block>>),
}

#[derive(Clone, Debug)]
pub struct Record {
    pub fields: Vec<(Spanned<Identifier>, Spanned<Expr>)>,
}

#[derive(Clone, Debug)]
pub struct Match {
    pub expr: Spanned<Expr>,
    pub arms: Vec<MatchArm>,
}

#[derive(Clone, Debug)]
pub struct MatchArm {
    pub variant_id: Spanned<Identifier>,
    pub data_field: Option<Spanned<Identifier>>,
    pub guard: Option<Spanned<Expr>>,
    pub body: Spanned<Block>,
}

#[derive(Clone, Debug)]
pub struct ActionDeclaration {
    pub ident: Spanned<Identifier>,
    pub params: Spanned<Vec<(Spanned<Identifier>, Spanned<Identifier>)>>,
    pub body: Spanned<Block>,
}

#[derive(Clone, Debug)]
pub struct Rib {
    pub ident: Spanned<Identifier>,
    pub contain_ty: Spanned<Identifier>,
    pub body: RibBody,
}

#[derive(Clone, Debug)]
pub struct RibBody {
    pub key_values: Spanned<Vec<(Identifier, RibFieldType)>>,
}

#[derive(Clone, Debug)]
pub enum RibFieldType {
    Identifier(Identifier),
    Record(RecordType),
    List(Box<RibFieldType>),
}

#[derive(Clone, Debug)]
pub struct Table {
    pub ident: Spanned<Identifier>,
    pub contain_ty: Spanned<Identifier>,
    pub body: RibBody,
}

#[derive(Clone, Debug)]
pub struct OutputStream {
    pub ident: Spanned<Identifier>,
    pub contain_ty: Spanned<Identifier>,
    pub body: RibBody,
}

/// An identifier is the name of variables or other things.
///
/// It is a word composed of a leading alphabetic Unicode character, followed
/// by alphanumeric Unicode characters or underscore or hyphen.
#[derive(Clone, Debug, Ord, PartialOrd, Hash)]
pub struct Identifier(pub String);

impl AsRef<str> for Identifier {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl std::borrow::Borrow<str> for Identifier {
    fn borrow(&self) -> &str {
        self.0.as_ref()
    }
}

impl<T: AsRef<str>> PartialEq<T> for Identifier {
    fn eq(&self, other: &T) -> bool {
        self.0 == other.as_ref()
    }
}

impl Eq for Identifier {}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

/// The user-defined type of a record. It's very similar to a RibBody (in EBNF
/// it's the same), but it simplifies creating the SymbolTable, because they're
/// semantically different.
#[derive(Clone, Debug)]
pub struct RecordType {
    pub key_values: Spanned<Vec<(Identifier, RibFieldType)>>,
}

#[derive(Clone, Debug)]
pub enum Literal {
    Accept,
    Reject,
    String(String),
    Prefix(Prefix),
    PrefixLength(u8),
    Asn(u32),
    IpAddress(IpAddress),
    ExtendedCommunity(routecore::bgp::communities::ExtendedCommunity),
    StandardCommunity(routecore::bgp::communities::StandardCommunity),
    LargeCommunity(routecore::bgp::communities::LargeCommunity),
    Integer(i64),
    Bool(bool),
}

#[derive(Clone, Debug)]
pub enum BinOp {
    And,
    Or,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    In,
    NotIn,
}

#[derive(Clone, Debug)]
pub enum PrefixMatchType {
    Exact,
    Longer,
    OrLonger,
    PrefixLengthRange(PrefixLengthRange),
    UpTo(u8),
    Through(u8),
    NetMask(IpAddress),
}

#[derive(Clone, Debug)]
pub struct PrefixMatchExpr {
    pub prefix: Prefix,
    pub ty: PrefixMatchType,
}

#[derive(Clone, Debug)]
pub enum IpAddress {
    Ipv4(std::net::Ipv4Addr),
    Ipv6(std::net::Ipv6Addr),
}

#[derive(Clone, Debug)]
pub struct Prefix {
    pub addr: Spanned<IpAddress>,
    pub len: Spanned<u8>,
}

#[derive(Clone, Debug)]
pub struct PrefixLengthRange {
    pub start: u8,
    pub end: u8,
}
