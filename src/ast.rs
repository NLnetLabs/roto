//! Abstract Syntax Tree (AST) for Roto
//!
//! A [`SyntaxTree`] is the output of the Roto parser. It contains a
//! representation of the Roto script as Rust types for further processing.

use std::fmt::Display;

use inetnum::asn::Asn;
use symbol_table::GlobalSymbol;

use crate::parser::meta::Meta;

#[derive(Clone, Debug)]
pub struct SyntaxTree {
    pub declarations: Vec<Declaration>,
}

#[derive(Clone, Debug)]
pub enum Declaration {
    FilterMap(Box<FilterMap>),
    Record(RecordTypeDeclaration),
    Function(FunctionDeclaration),
    Test(Test),
    Import(Meta<Path>),
}

#[derive(Clone, Debug)]
pub struct Params(pub Vec<(Meta<Identifier>, Meta<Path>)>);

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
    pub body: Meta<Block>,
}

/// A function declaration, including the [`Block`] forming its definition
#[derive(Clone, Debug)]
pub struct FunctionDeclaration {
    pub ident: Meta<Identifier>,
    pub params: Meta<Params>,
    pub ret: Option<Meta<Path>>,
    pub body: Meta<Block>,
}

#[derive(Clone, Debug)]
pub struct Test {
    pub ident: Meta<Identifier>,
    pub body: Meta<Block>,
}

/// A block of multiple statements
#[derive(Clone, Debug)]
pub struct Block {
    pub imports: Vec<Meta<Path>>,
    pub stmts: Vec<Meta<Stmt>>,
    pub last: Option<Box<Meta<Expr>>>,
}

/// A statement in a block
#[derive(Clone, Debug)]
pub enum Stmt {
    Let(Meta<Identifier>, Meta<Expr>),
    Expr(Meta<Expr>),
}

#[derive(Clone, Debug)]
pub struct Path {
    pub idents: Vec<Meta<Identifier>>,
}

/// A Roto expression
#[derive(Clone, Debug)]
pub enum Expr {
    /// Return from the current function or filtermap
    ///
    /// Optionally takes an expression for the value being returned.
    Return(ReturnKind, Option<Box<Meta<Expr>>>),

    /// A literal expression
    Literal(Meta<Literal>),

    /// A match expression,
    Match(Box<Meta<Match>>),

    /// A function call expression
    FunctionCall(Box<Meta<Expr>>, Meta<Vec<Meta<Expr>>>),

    /// A field access expression
    Access(Box<Meta<Expr>>, Meta<Identifier>),

    /// A variable use
    Path(Meta<Path>),

    /// A record that doesn't have a type mentioned in the assignment of it
    ///
    /// For example: `{ value_1: 100, value_2: "bla" }`. This can also be a
    /// sub-record of a record that does have an explicit type.
    Record(Meta<Record>),

    /// An expression of a record that does have a type
    ///
    /// For example: `MyType { value_1: 100, value_2: "bla" }`, where `MyType`
    /// is a user-defined Record Type.
    TypedRecord(Meta<Path>, Meta<Record>),

    /// An expression that yields a list of values, e.g. `[100, 200, 300]`
    List(Vec<Meta<Expr>>),

    /// A unary not expression
    Not(Box<Meta<Expr>>),

    /// A binary operator expression
    ///
    /// Takes a left operand, the operator and the right operand
    BinOp(Box<Meta<Expr>>, BinOp, Box<Meta<Expr>>),

    /// An if or if-else expression
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
    },
}

/// An identifier is the name of variables or other things.
///
/// It is a word composed of a leading alphabetic Unicode character, followed
/// by alphanumeric Unicode characters or underscore or hyphen.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier(GlobalSymbol);

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Identifier {
    pub fn as_str(&self) -> &'static str {
        self.0.as_str()
    }
}

impl From<&str> for Identifier {
    fn from(value: &str) -> Self {
        Self(value.into())
    }
}

impl From<&String> for Identifier {
    fn from(value: &String) -> Self {
        Self(value.into())
    }
}

impl From<String> for Identifier {
    fn from(value: String) -> Self {
        Self(value.into())
    }
}

#[derive(Clone, Debug)]
pub struct RecordType {
    pub key_values: Meta<Vec<(Meta<Identifier>, RecordFieldType)>>,
}

#[derive(Clone, Debug)]
pub enum RecordFieldType {
    Path(Meta<Path>),
    Record(Meta<RecordType>),
    List(Meta<Box<RecordFieldType>>),
}

#[derive(Clone, Debug)]
pub enum Literal {
    #[allow(dead_code)]
    String(String),
    Asn(Asn),
    IpAddress(std::net::IpAddr),
    Integer(i64),
    Bool(bool),
}

#[derive(Clone, Debug)]
pub enum BinOp {
    /// Logical and (`&&`)
    And,
    /// Logical or (`||`)
    Or,
    /// Equals (`==`)
    Eq,
    /// Not equals (`!=`)
    Ne,
    /// Less than (`<`)
    Lt,
    /// Less than or equal (`<=`)
    Le,
    /// Greater than (`>`)
    Gt,
    /// Greater than or equal (`>=`)
    Ge,
    /// In
    In,
    /// Not in
    NotIn,
    /// Addition (`+`)
    Add,
    /// Subtraction (`-`)
    Sub,
    /// Multiplication (`*`)
    Mul,
    /// Division (`/`)
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
