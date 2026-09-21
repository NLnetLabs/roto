//! Abstract Syntax Tree (AST) for Roto
//!
//! A [`SyntaxTree`] is the output of the Roto parser. It contains a
//! representation of the Roto script as Rust types for further processing.

use std::fmt::Display;

use inetnum::asn::Asn;
use symbol_table::GlobalSymbol;

use crate::{
    ast::{BinOp, Block, Expr, Identifier, Literal, Stmt},
    parser::meta::Meta,
    typechecker::types::TypeDefinition,
    yang::parser::{Keyword, YangStmt},
};

// #[derive(Clone, Debug)]
// pub struct SyntaxTree {
//     pub declarations: Vec<Declaration>,
// }

// impl SyntaxTree {
//     pub fn walk_node_tests(&self) -> Vec<Meta<Identifier>> {
//         let mut node_tests = vec![];
//         for decl in &self.declarations {
//             if let Declaration::XPath(xpath) = decl {
//                 for xpsn in &xpath.idents {
//                     if let XPathStep::NodeTest(nt) = &xpsn.node {
//                         node_tests.push(nt.clone());
//                     }
//                 }
//             }
//         }
//         node_tests
//     }

//     /// Iterator over all module and submodule statements in an ast.
//     pub fn modules(&self) -> impl Iterator<Item = Meta<Identifier>> {
//         self.declarations.iter().filter_map(|d| {
//             if let Declaration::Statement(yang_stmt_seq) = d {
//                 yang_stmt_seq.module_stmt()
//             } else {
//                 None
//             }
//         })
//     }
// }

// #[derive(Clone, Debug)]
// pub enum Declaration {
//     // Module(Box<ModuleDeclaration>),
//     // Container(ContainerDeclaration),
//     Statement(YangStmtSeq),
//     FilterMap(Box<FilterMap>),
//     Record(RecordTypeDeclaration),
//     Function(FunctionDeclaration),
//     Test(Test),
//     Import(Meta<Path>),
//     XPath(Meta<XPath>),
// }

#[derive(Clone, Debug)]
pub struct Params(pub Vec<(Meta<Identifier>, Meta<TypeExpr>)>);

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

#[derive(Clone, Debug)]
pub struct Module {
    pub ident: Meta<Identifier>,
    pub block: Meta<Block>,
}

/// A function declaration, including the [`Block`] forming its definition
#[derive(Clone, Debug)]
pub struct FunctionDeclaration {
    pub ident: Meta<Identifier>,
    pub params: Meta<Params>,
    pub ret: Option<Meta<TypeExpr>>,
    pub body: Meta<Block>,
}

// #[derive(Clone, Debug)]
// pub struct ModuleDeclaration {
//     pub ident: Meta<Identifier>,
//     // pub ret: Option<Meta<TypeExpr>>,
//     pub body: Meta<Block>,
// }

// #[derive(Clone, Debug)]
// pub struct ContainerDeclaration {
//     pub ident: Meta<Identifier>,
//     pub children: Vec<Meta<Block>>,
// }

#[derive(Clone, Debug)]
pub struct Test {
    pub ident: Meta<Keyword>,
    pub body: Meta<Block>,
}

/// A block of multiple statements
// #[derive(Clone, Debug)]
// pub struct Block {
//     // pub imports: Vec<Meta<Path>>,
//     pub stmts: Vec<Meta<YangStmtSeq>>,
//     // pub last: Option<Box<Meta<Expr>>>,
// }

/// A statement in a block
// #[derive(Clone, Debug)]
// pub enum Stmt {
//     Let(Meta<Identifier>, Meta<Expr>),
//     Expr(Meta<Expr>),
// }

/// A sequunce of yang statements
/// statement = keyword [argument] (";" / "{" *statement "}")
#[derive(Clone, Debug)]
pub struct YangStmtSeq {
    pub stmt: YangStmt,
    pub arg: Option<Meta<Argument>>,
    pub sub_stmts: Meta<Expr>,
}

impl YangStmtSeq {
    /// Return the yang module if it is one, otherwise none.
    pub fn is_module(&self) -> Option<(&Self, bool)> {
        match &self.stmt {
            YangStmt::Module(meta, is_sub) => Some((self, *is_sub)),
            _ => None,
        }
    }

    /// Return a the value of the sub-statement with key 'description'.
    ///
    /// Returns None if the sub-statement does not exist.
    pub fn description(&self) -> Option<String> {
        self.sub_stmts
            .node
            .find_attr("description")
            .map(|d| d.to_string())
    }

    /// docs
    pub fn iter_block_stmts(&self) -> impl Iterator<Item = &Meta<Stmt>> {
        self.sub_stmts.iter_stmt()
    }

    /// docs
    pub fn find_attr(&self, name: &str) -> Option<&Meta<Argument>> {
        self.iter_block_stmts()
            .find(|stmt| {
                stmt.as_ident().map(|i| i.as_str() == name).unwrap_or(false)
            })
            .and_then(|stmt| {
                stmt.argument()
                // stmt.argument().and_then(|a| {
                //     Some(Meta {
                //         id: a.id,
                //         node: a.as_str(),
                //     })
                // })
            })
    }

    /// Returns the argument type definition of this [`YangStmtSeq`].
    pub fn argument_type_definition(&self) -> Option<TypeDefinition> {
        match &self {
            &YangStmtSeq {
                stmt: YangStmt::Stmt(Meta { node: kw, .. }),
                arg: Some(Meta { node: arg, .. }),
                ..
            } => kw.test_arg_type(arg),
            _ => None,
        }
    }

    /// docs
    pub fn argument(&self) -> Option<&Meta<Argument>> {
        // let Stmt::YangStmtSeq(yang_s) = &self else {
        //     return None;
        // };
        // let yang_s = self;

        // if yang_s.arg.is_some() {
        //     return yang_s.arg.as_ref();
        // }

        if let Expr::Argument(arg) = &self.sub_stmts.node {
            Some(arg)
        } else {
            self.arg.as_ref()
        }

        // Some(Meta {
        //     id: arg.id,
        //     node: arg_ident,
        // })
        // let derive_type = yang_s.sub_stmts.iter_stmt().find(|stmt| {
        //     stmt.is_keyword(crate::yang::parser::Keyword::Type)
        // })?;

        // let Stmt::YangStmtSeq(YangStmtSeq { sub_stmts, .. }) =
        //     &derive_type.node
        // else {
        //     return None;
        // };

        // let Expr::Argument(arg) = &sub_stmts.node else {
        //     return None;
        // };

        // Some(arg)
    }
}

#[derive(Clone, Debug)]
pub enum Argument {
    UnquotedString(Literal),
    QuotedString(Literal),
    Ident(Identifier),
    // Empty,
}

impl Argument {
    /// Return the argument as identifier if it qualifies as one. The parser
    /// should already have picked the most specific fitting one, where ident
    /// is the most specific: identifiers are a subset of unquoted strings.
    pub fn as_ident(&self) -> Option<Identifier> {
        match self {
            Argument::UnquotedString(_literal) => None,
            Argument::QuotedString(_literal) => None,
            Argument::Ident(identifier) => Some(*identifier),
        }
    }

    pub fn as_str(&self) -> String {
        match self {
            Argument::UnquotedString(literal) => literal.to_string(),
            Argument::QuotedString(literal) => literal.to_string(),
            Argument::Ident(identifier) => identifier.to_string(),
        }
    }
}

impl std::fmt::Display for Argument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Argument::UnquotedString(literal) => write!(f, "{}", literal),
            Argument::QuotedString(literal) => write!(f, "{}", literal),
            Argument::Ident(identifier) => write!(f, "{}", identifier),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Path {
    pub idents: Vec<Meta<Identifier>>,
}

#[derive(Clone, Debug)]
pub enum WildCardIdentifier {
    Ident(Identifier),
    Star,
    DoubleSlash,
    Dot,
    DoubleDot,
}

#[derive(Clone, Debug)]
pub struct XPath {
    pub idents: Vec<Meta<XPathStep>>,
}

#[derive(Clone, Debug)]
pub enum XPathStep {
    AxisName(Meta<AxisName>, Meta<Identifier>),
    NodeTest(Meta<Identifier>),
    Predicates(XPathPredicate),
}

#[derive(Clone, Debug, PartialEq)]
pub enum AxisName {
    Ancestor,
    AncestorOrSelf,
    Attribute,
    Child,
    Descendant,
    DescendantOrSelf,
    Following,
    FollowingSibling,
    Namespace,
    Parent,
    Preceding,
    PrecedingSibling,
    // right, Self
    Zelf,
}

impl From<&AxisName> for &str {
    fn from(value: &AxisName) -> Self {
        match value {
            AxisName::Ancestor => "ancestor",
            AxisName::AncestorOrSelf => "ancestor-or-self",
            AxisName::Attribute => "attribute",
            AxisName::Child => "child",
            AxisName::Descendant => "descendant",
            AxisName::DescendantOrSelf => "descendant-or-self",
            AxisName::Following => "following",
            AxisName::FollowingSibling => "following-sibling",
            AxisName::Namespace => "namespace",
            AxisName::Parent => "parent",
            AxisName::Preceding => "preceding",
            AxisName::PrecedingSibling => "preceding-sibling",
            AxisName::Zelf => "self",
        }
    }
}

#[derive(Clone, Debug)]
pub enum XPathNodeTest {
    NameTest(Identifier),
    XPathNodeType,
    ProcessingInstruction(Literal),
}

#[derive(Clone, Debug)]
pub enum XPathNodeType {
    Comment,
    Text,
    ProcessingInstruction,
    Node,
}

#[derive(Clone, Debug)]
pub struct XPathPredicate {
    pub ident: Meta<Identifier>,
    pub expr: Vec<Meta<Expr>>,
}

#[derive(Clone, Debug)]
pub enum TypeExpr {
    Optional(Box<TypeExpr>),
    Path(Meta<Path>, Vec<Meta<TypeExpr>>),
    Never,
    Unit,
    Record(RecordType),
}

/// A Roto expression
// #[derive(Clone, Debug)]
// pub enum Expr {
//     /// Return from the current function or filtermap
//     ///
//     /// Optionally takes an expression for the value being returned.
//     Return(ReturnKind, Option<Box<Meta<Expr>>>),

//     /// A literal expression
//     Literal(Meta<Literal>),

//     /// A match expression,
//     Match(Box<Meta<Match>>),

//     /// A function call expression
//     FunctionCall(Box<Meta<Expr>>, Meta<Vec<Meta<Expr>>>),

//     /// A field access expression
//     Access(Box<Meta<Expr>>, Meta<Identifier>),

//     /// A variable use
//     Path(Meta<Path>),

//     /// XPath 1.0 expression
//     XPath(Meta<XPath>),

//     /// a Yang statement

//     /// A record that doesn't have a type mentioned in the assignment of it
//     ///
//     /// For example: `{ value_1: 100, value_2: "bla" }`. This can also be a
//     /// sub-record of a record that does have an explicit type.
//     Record(Meta<Record>),

//     /// An expression of a record that does have a type
//     ///
//     /// For example: `MyType { value_1: 100, value_2: "bla" }`, where `MyType`
//     /// is a user-defined Record Type.
//     TypedRecord(Meta<Path>, Meta<Record>),

//     /// An expression that yields a list of values, e.g. `[100, 200, 300]`
//     List(Vec<Meta<Expr>>),

//     /// A unary not expression
//     Not(Box<Meta<Expr>>),

//     /// A binary operator expression
//     ///
//     /// Takes a left operand, the operator and the right operand
//     BinOp(Box<Meta<Expr>>, BinOp, Box<Meta<Expr>>),

//     /// An if or if-else expression
//     IfElse(Box<Meta<Expr>>, Meta<Block>, Option<Meta<Block>>),
// }

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

// #[derive(Clone, Debug)]
// pub struct Record {
//     pub fields: Vec<(Meta<Identifier>, Meta<Expr>)>,
// }

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
// #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
// pub struct Identifier(GlobalSymbol);

// impl Display for Identifier {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         self.0.fmt(f)
//     }
// }

// impl Identifier {
//     pub fn as_str(&self) -> &'static str {
//         self.0.as_str()
//     }
// }

// impl From<&str> for Identifier {
//     fn from(value: &str) -> Self {
//         Self(value.into())
//     }
// }

// impl From<&String> for Identifier {
//     fn from(value: &String) -> Self {
//         Self(value.into())
//     }
// }

// impl From<String> for Identifier {
//     fn from(value: String) -> Self {
//         Self(value.into())
//     }
// }

#[derive(Clone, Debug)]
pub struct RecordType {
    pub fields: Meta<Vec<(Meta<Identifier>, Meta<TypeExpr>)>>,
}

// #[derive(Clone, Debug)]
// pub enum Literal {
//     #[allow(dead_code)]
//     String(String),
//     Asn(Asn),
//     IpAddress(std::net::IpAddr),
//     Integer(i64),
//     Float(f64),
//     Bool(bool),
// }

// #[derive(Clone, Debug, PartialEq, Eq)]
// pub enum BinOp {
//     /// Logical and (`&&`)
//     And,
//     /// Logical or (`||`)
//     Or,
//     /// Equals (`==`)
//     Eq,
//     /// Not equals (`!=`)
//     Ne,
//     /// Less than (`<`)
//     Lt,
//     /// Less than or equal (`<=`)
//     Le,
//     /// Greater than (`>`)
//     Gt,
//     /// Greater than or equal (`>=`)
//     Ge,
//     /// In
//     In,
//     /// Not in
//     NotIn,
//     /// Addition (`+`)
//     Add,
//     /// Subtraction (`-`)
//     Sub,
//     /// Multiplication (`*`)
//     Mul,
//     /// Division (`/`)
//     Div,
// }

// impl std::fmt::Display for BinOp {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(
//             f,
//             "{}",
//             match self {
//                 Self::And => "&&",
//                 Self::Or => "||",
//                 Self::Eq => "==",
//                 Self::Ne => "!=",
//                 Self::Lt => "<=",
//                 Self::Le => "<",
//                 Self::Gt => ">=",
//                 Self::Ge => "<",
//                 Self::In => "in",
//                 Self::Add => "+",
//                 Self::Sub => "-",
//                 Self::Mul => "*",
//                 Self::Div => "/",
//                 Self::NotIn => "not in",
//             }
//         )
//     }
// }
