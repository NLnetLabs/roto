use string_interner::symbol::SymbolU32;

use crate::parser::meta::Meta;

#[derive(Clone, Debug)]
pub struct SyntaxTree {
    pub declarations: Vec<Declaration>,
}

#[derive(Clone, Debug)]
pub enum Declaration {
    FilterMap(Box<FilterMap>),
    Rib(Rib),
    Table(Table),
    OutputStream(OutputStream),
    Record(RecordTypeDeclaration),
    Function(FunctionDeclaration),
}

#[derive(Clone, Debug)]
pub struct Params(pub Vec<(Meta<Identifier>, Meta<Identifier>)>);

/// The value of a typed record
#[derive(Clone, Debug)]
pub struct RecordTypeDeclaration {
    pub ident: Meta<Identifier>,
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
    pub ident: Meta<Identifier>,
    pub params: Meta<Params>,
    pub body: FilterMapBody,
}

#[derive(Clone, Debug)]
pub struct FilterMapBody {
    pub define: Vec<(Meta<Identifier>, Meta<Expr>)>,
    pub apply: Meta<Block>,
}

#[derive(Clone, Debug)]
pub struct FunctionDeclaration {
    pub ident: Meta<Identifier>,
    pub params: Meta<Params>,
    pub ret: Option<Meta<Identifier>>,
    pub body: Meta<Block>,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub exprs: Vec<Meta<Expr>>,
    pub last: Option<Box<Meta<Expr>>>,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Return(ReturnKind, Option<Box<Meta<Expr>>>),
    /// a literal, or a chain of field accesses and/or methods on a literal,
    /// e.g. `10.0.0.0/8.covers(..)`
    Literal(Meta<Literal>),
    Match(Box<Meta<Match>>),
    FunctionCall(Meta<Identifier>, Meta<Vec<Meta<Expr>>>),
    MethodCall(Box<Meta<Expr>>, Meta<Identifier>, Meta<Vec<Meta<Expr>>>),
    Access(Box<Meta<Expr>>, Meta<Identifier>),
    Var(Meta<Identifier>),
    /// a record that doesn't have a type mentioned in the assignment of it,
    /// e.g `{ value_1: 100, value_2: "bla" }`. This can also be a sub-record
    /// of a record that does have an explicit type.
    Record(Meta<Record>),
    /// an expression of a record that does have a type, e.g. `MyType {
    /// value_1: 100, value_2: "bla" }`, where MyType is a user-defined Record
    /// Type.
    TypedRecord(Meta<Identifier>, Meta<Record>),
    /// An expression that yields a list of values, e.g. `[100, 200, 300]`
    List(Vec<Meta<Expr>>),
    Not(Box<Meta<Expr>>),
    BinOp(Box<Meta<Expr>>, BinOp, Box<Meta<Expr>>),
    IfElse(Box<Meta<Expr>>, Meta<Block>, Option<Meta<Block>>),
}

#[derive(Clone, Debug)]
pub enum ReturnKind {
    Return,
    Accept,
    Reject,
}

impl ReturnKind {
    pub fn str(&self) -> &'static str {
        match self {
            ReturnKind::Return => "return",
            ReturnKind::Accept => "accept",
            ReturnKind::Reject => "reject",
        }
    }
}

#[derive(Clone, Debug)]
pub struct Record {
    pub fields: Vec<(Meta<Identifier>, Meta<Expr>)>,
}

#[derive(Clone, Debug)]
pub struct Match {
    pub expr: Meta<Expr>,
    pub arms: Vec<MatchArm>,
}

#[derive(Clone, Debug)]
pub struct MatchArm {
    pub pattern: Meta<Pattern>,
    pub guard: Option<Meta<Expr>>,
    pub body: Meta<Block>,
}

#[derive(Clone, Debug)]
pub enum Pattern {
    Underscore,
    EnumVariant {
        variant: Meta<Identifier>,
        data_field: Option<Meta<Identifier>>,
    }
}

#[derive(Clone, Debug)]
pub struct Rib {
    pub ident: Meta<Identifier>,
    pub contain_ty: Meta<Identifier>,
    pub body: RibBody,
}

#[derive(Clone, Debug)]
pub struct RibBody {
    pub key_values: Meta<Vec<(Meta<Identifier>, RibFieldType)>>,
}

#[derive(Clone, Debug)]
pub enum RibFieldType {
    Identifier(Meta<Identifier>),
    Record(Meta<RecordType>),
    List(Meta<Box<RibFieldType>>),
}

#[derive(Clone, Debug)]
pub struct Table {
    pub ident: Meta<Identifier>,
    pub contain_ty: Meta<Identifier>,
    pub body: RibBody,
}

#[derive(Clone, Debug)]
pub struct OutputStream {
    pub ident: Meta<Identifier>,
    pub contain_ty: Meta<Identifier>,
    pub body: RibBody,
}

/// An identifier is the name of variables or other things.
///
/// It is a word composed of a leading alphabetic Unicode character, followed
/// by alphanumeric Unicode characters or underscore or hyphen.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier(pub SymbolU32);

#[derive(Clone, Debug)]
pub struct RecordType {
    pub key_values: Meta<Vec<(Meta<Identifier>, RibFieldType)>>,
}

#[derive(Clone, Debug)]
pub enum Literal {
    #[allow(dead_code)]
    String(String),
    Asn(u32),
    IpAddress(IpAddress),
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
    Add,
    Sub,
    Mul,
    Div,
}

impl std::fmt::Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::And => "&&",
                Self::Or => "||",
                Self::Eq => "==",
                Self::Ne => "!=",
                Self::Lt => "<=",
                Self::Le => "<",
                Self::Gt => ">=",
                Self::Ge => "<",
                Self::In => "in",
                Self::Add => "+",
                Self::Sub => "-",
                Self::Mul => "*",
                Self::Div => "/",
                Self::NotIn => "not in",
            }
        )
    }
}

#[derive(Clone, Debug)]
pub enum IpAddress {
    Ipv4(std::net::Ipv4Addr),
    Ipv6(std::net::Ipv6Addr),
}
