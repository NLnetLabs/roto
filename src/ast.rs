//! Abstract Syntax Tree (AST) for Roto
//!
//! A [`SyntaxTree`] is the output of the Roto parser. It contains a
//! representation of the Roto script as Rust types for further processing.

use std::fmt::Display;

use inetnum::asn::Asn;
use symbol_table::GlobalSymbol;

use crate::{
    parser::{
        ParseError,
        meta::{Meta, MetaId},
    },
    typechecker::types::TypeDefinition,
    yang::parser::{
        Keyword,
        ast::{Argument, YangStmtSeq},
    },
};

#[derive(Clone, Debug)]
pub struct SyntaxTree {
    pub declarations: Vec<Declaration>,
}

#[derive(Clone, Debug)]
pub enum Declaration {
    YangModule(YangModuleDeclaration),
    FilterMap(Box<FilterMap>),
    Const(ConstantDeclaration),
    Record(RecordTypeDeclaration),
    Enum(EnumTypeDeclaration),
    Function(FunctionDeclaration),
    Test(Test),
    Import(Vec<Meta<Path>>),
}

impl SyntaxTree {
    // pub fn walk_node_tests(&self) -> Vec<Meta<Identifier>> {
    //     let mut node_tests = vec![];
    //     for decl in &self.declarations {
    //         if let Declaration::XPath(xpath) = decl {
    //             for xpsn in &xpath.idents {
    //                 if let XPathStep::NodeTest(nt) = &xpsn.node {
    //                     node_tests.push(nt.clone());
    //                 }
    //             }
    //         }
    //     }
    //     node_tests
    // }

    /// Iterator over all module and submodule statements in an ast.
    pub fn yang_modules(
        &self,
    ) -> impl Iterator<Item = &YangModuleDeclaration> {
        self.declarations.iter().filter_map(|d| match d {
            Declaration::YangModule(y) => Some(y),
            _ => None,
        })
    }
}

pub struct Signature {
    pub type_params: Vec<Meta<Identifier>>,
    pub params: Vec<Meta<TypeExpr>>,
    pub ret: Option<Meta<TypeExpr>>,
}

#[derive(Clone, Debug)]
pub struct Params(pub Vec<(Meta<Identifier>, Meta<TypeExpr>)>);

/// The value of a typed record
#[derive(Clone, Debug)]
pub struct RecordTypeDeclaration {
    pub ident: Meta<Identifier>,
    pub type_params: Vec<Meta<Identifier>>,
    pub record_type: RecordType,
}

#[derive(Clone, Debug)]
pub struct EnumTypeDeclaration {
    pub ident: Meta<Identifier>,
    pub type_params: Vec<Meta<Identifier>>,
    pub variants: Meta<Vec<Variant>>,
}

#[derive(Clone, Debug)]
pub struct Variant {
    pub ident: Meta<Identifier>,
    pub fields: Vec<Meta<TypeExpr>>,
}

#[derive(Clone, Debug)]
pub struct ConstantDeclaration {
    pub ident: Meta<Identifier>,
    pub ty: Meta<TypeExpr>,
    pub expr: Meta<Expr>,
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
    pub ret: Option<Meta<TypeExpr>>,
    pub body: Meta<Block>,
}

/// A yang module declaration which both covers `module` and `submodule`
/// keywords. `prefix` and `namepspace` are mandatory for `module`, but do not
/// appear in `submodule`.
#[derive(Clone, Debug)]
pub struct YangModuleDeclaration {
    pub ident: Meta<Identifier>,
    pub prefix: Option<Meta<Identifier>>,
    pub namespace: Option<Meta<String>>,
    pub parent: Option<Meta<Identifier>>,
    pub body: Meta<Expr>,
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
    Let(Meta<Identifier>, Option<Meta<TypeExpr>>, Meta<Expr>),
    Expr(Meta<Expr>),
    YangStmtSeq(YangStmtSeq),
}

impl Stmt {
    pub fn argument(&self) -> Option<&Meta<Argument>> {
        if let Stmt::YangStmtSeq(seq) = self {
            seq.argument()
        } else {
            None
        }

        // if let Stmt::YangStmtSeq(YangStmtSeq { arg, .. }) = self {
        //     dbg!(arg);
        //     dbg!(self);
        //     arg.as_ref().and_then(|a| {
        //         dbg!(a.node.as_ident().map(|arg| Meta {
        //             id: a.id,
        //             node: arg,
        //         }))
        //     })
        // } else {
        //     None
        // }
    }

    pub fn arg_as_ident(&self) -> Option<Identifier> {
        if let Stmt::YangStmtSeq(YangStmtSeq { arg, .. }) = self {
            arg.as_ref().and_then(|a| a.node.as_ident())
        } else {
            None
        }
    }

    pub fn as_ident(&self) -> Option<Identifier> {
        if let Stmt::YangStmtSeq(YangStmtSeq { stmt, .. }) = self {
            Some(stmt.as_ident())
        } else {
            None
        }
    }

    pub fn is_keyword(&self, kw: Keyword) -> bool {
        if let Stmt::YangStmtSeq(YangStmtSeq { stmt, .. }) = self {
            stmt.node().is_some_and(|mk| mk.node == kw)
        } else {
            false
        }
    }

    pub fn description(&self) -> Option<String> {
        let Stmt::YangStmtSeq(yang_s) = &self else {
            return None;
        };

        yang_s.description()
    }

    pub fn argument_type_definition(&self) -> Option<TypeDefinition> {
        let Stmt::YangStmtSeq(yang_s) = &self else {
            return None;
        };

        yang_s.argument_type_definition()
    }
}

impl From<YangStmtSeq> for Stmt {
    fn from(value: YangStmtSeq) -> Self {
        Stmt::YangStmtSeq(value)
    }
}

#[derive(Clone, Debug)]
pub struct Path {
    pub idents: Vec<Meta<Identifier>>,
}

#[derive(Clone, Debug)]
pub enum TypeExpr {
    Option(Box<Meta<TypeExpr>>),
    Path(Meta<Path>, Option<Meta<Vec<Meta<TypeExpr>>>>),
    Never,
    Unit,
    Record(RecordType),
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

    /// A block expression
    Block(Meta<Block>),

    /// A yang argument from a yang sequence
    Argument(Meta<Argument>),

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

    /// An assignment expression
    // TODO: Arbitrary place expressions should be allowed at some point, but
    //       for now that's not supported.
    Assign(Meta<Path>, Box<Meta<Expr>>),

    /// An assignment expression
    // TODO: Arbitrary place expressions should be allowed at some point, but
    //       for now that's not supported.
    CompoundAssign(CompoundAssign),

    /// A binary operator expression
    ///
    /// Takes a left operand, the operator and the right operand
    BinOp(Box<Meta<Expr>>, BinOp, Box<Meta<Expr>>),

    Negate(Box<Meta<Expr>>),

    /// An if or if-else expression
    IfElse(Box<Meta<Expr>>, Meta<Block>, Option<Meta<Block>>),

    /// A while-loop expression
    While(Box<Meta<Expr>>, Meta<Block>),

    /// A for-loop expression
    For(Meta<Identifier>, Box<Meta<Expr>>, Meta<Block>),

    /// Question mark operator
    QuestionMark(Box<Meta<Expr>>),

    /// f-string
    FString(Vec<Meta<FStringPart>>),
}

impl Expr {
    pub fn iter_stmt(&self) -> impl Iterator<Item = &Meta<Stmt>> {
        if let Expr::Block(block) = self {
            Some(block.stmts.iter())
        } else {
            None
        }
        .into_iter()
        .flatten()
    }

    pub fn find_attr(&self, name: &str) -> Option<&Meta<Argument>> {
        self.iter_stmt()
            .find(|stmt| {
                dbg!(stmt.as_ident());
                dbg!(name);
                dbg!(
                    stmt.as_ident()
                        .map(|i| i.as_str() == name)
                        .unwrap_or(false)
                )
            })
            .and_then(|stmt| stmt.argument())
    }
}

#[derive(Clone, Debug)]
pub struct CompoundAssign {
    pub binop_id: MetaId,
    pub path_expr_id: MetaId,
    pub path: Meta<Path>,
    pub op: CompoundAssignOp,
    pub expr: Box<Meta<Expr>>,
}

#[derive(Clone, Debug)]
pub enum FStringPart {
    String(String),
    Expr(Meta<Expr>),
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
        fields: Option<Meta<Vec<Meta<Identifier>>>>,
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
    pub fields: Meta<Vec<(Meta<Identifier>, Meta<TypeExpr>)>>,
}

#[derive(Clone, Debug)]
pub enum Literal {
    String(String),
    Char(char),
    Asn(Asn),
    IpAddress(std::net::IpAddr),
    Integer(i64, Option<IntType>),
    Float(f64, Option<FloatType>),
    Bool(bool),
    Unit,
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::String(s) => write!(f, "{}", s),
            Literal::Char(c) => write!(f, "{}", c.to_string()),
            Literal::Asn(asn) => write!(f, "{}", asn),
            Literal::IpAddress(ip_addr) => write!(f, "{}", ip_addr),
            Literal::Integer(i, int_type) => write!(f, "{}", i),
            Literal::Float(fl, float_type) => write!(f, "{}", fl),
            Literal::Bool(b) => write!(f, "{}", b),
            Literal::Unit => write!(f, "()"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum IntType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
}

impl Display for IntType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ident = match self {
            Self::I8 => "i8",
            Self::I16 => "i16",
            Self::I32 => "i32",
            Self::I64 => "i64",
            Self::U8 => "u8",
            Self::U16 => "u16",
            Self::U32 => "u32",
            Self::U64 => "u64",
        };
        f.write_str(ident)
    }
}

#[derive(Clone, Debug)]
pub enum FloatType {
    F32,
    F64,
}

impl Display for FloatType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ident = match self {
            Self::F32 => "i32",
            Self::F64 => "i64",
        };
        f.write_str(ident)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CompoundAssignOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
    /// Addition (`+`)
    Add,
    /// Subtraction (`-`)
    Sub,
    /// Multiplication (`*`)
    Mul,
    /// Division (`/`)
    Div,
    /// Modulo ('%')
    Mod,
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
                Self::Add => "+",
                Self::Sub => "-",
                Self::Mul => "*",
                Self::Div => "/",
                Self::Mod => "%",
            }
        )
    }
}
