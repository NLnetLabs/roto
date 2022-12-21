use std::{borrow, cmp, fmt, hash, ops, str};

use nom::branch::{alt, permutation};
use nom::bytes::complete::{is_not, take, take_while, take_while1};
use nom::character::complete::{char, digit1, multispace0, multispace1};
use nom::combinator::{all_consuming, cut, map_res, not, opt, recognize};
use nom::error::{context, ErrorKind, ParseError, VerboseError};
use nom::multi::{
    fold_many0, many0, many1, separated_list0, separated_list1,
};
use nom::sequence::{
    delimited, pair, preceded, separated_pair, terminated, tuple,
};
use nom::Finish;
use nom::{
    bytes::complete::{tag, take_until},
    character::complete::char as tag_char,
    combinator::map,
    IResult,
};
use smallvec::SmallVec;

use crate::compile::CompileError;
use crate::types::builtin::{Asn, Boolean};

/// ======== Root ===========================================================

/// The Root of the file.
///
// Root ::= RootExpr+

#[derive(Clone, Debug)]
pub struct Root {
    pub expressions: Vec<RootExpr>,
}

impl Root {
    pub fn parse_str(
        input: &str,
    ) -> Result<(&str, Self), VerboseError<&str>> {
        Self::parse_root(input).finish()
    }

    fn parse_root(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, expressions) = all_consuming(many1(preceded(
            skip_opt_ws,
            terminated(RootExpr::parse, skip_opt_ws),
        )))(input)?;
        Ok((input, Self { expressions }))
    }
}

//------------ RootExpr -----------------------------------------------------

// RootExpr ::= Module |
//     "rib" Identifier 'contains' TypeIdentifier '{' RibBody '}' |
//     "view" Identifier 'contains' TypeIdentifier '{' RibBody '}' |
//     "prefix-list" Identifier '{' PrefixListbody '}' |
//     "table" Identifier '{' TableBody '}' |
//     Comment

#[derive(Debug, Clone)]
pub enum RootExpr {
    Module(Box<Module>),
    Rib(Rib),
    // PrefixList(PrefixListExpr),
    Table(Table),
}

impl RootExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, expressions) = context(
            "root",
            alt((
                map(Rib::parse, Self::Rib),
                map(Table::parse, Self::Table),
                map(Module::parse, |m| Self::Module(Box::new(m))),
            )),
        )(input)?;
        Ok((input, expressions))
    }

    pub fn get_module(&self) -> Result<&Module, CompileError> {
        match self {
            Self::Module(m) => Ok(m),
            _ => Err(CompileError::new("not a module".into())),
        }
    }
}

//------------ Module --------------------------------------------------------

// Module ::= "module" Identifier "for" Identifier WithStatement  '{' ModuleBody '}'

#[derive(Clone, Debug)]
pub struct Module {
    pub ident: Identifier,
    pub for_ident: Option<TypeIdentField>,
    pub with_kv: Vec<TypeIdentField>,
    pub body: ModuleBody,
}

impl Module {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (ident, for_ident, with_kv, body)) = context(
            "module definition",
            tuple((
                context(
                    "module name",
                    preceded(
                        opt_ws(tag("module")),
                        opt_ws(Identifier::parse),
                    ),
                ),
                for_statement,
                with_statement,
                context(
                    "module body",
                    delimited(
                        opt_ws(char('{')),
                        cut(ModuleBody::parse),
                        opt_ws(char('}')),
                    ),
                ),
                // map(many0(char('\n')), |_| ()),
            )),
        )(input)?;

        Ok((
            input,
            Module {
                ident,
                body,
                for_ident,
                with_kv: with_kv.unwrap_or_default(),
            },
        ))
    }
}

//------------ ModuleBody ---------------------------------------------------

// ModuleBody ::= Define ModuleExpr+ Apply

#[derive(Clone, Debug)]
pub struct ModuleBody {
    pub define: Define,
    pub expressions: Vec<ModuleExpr>,
    pub apply: Option<Apply>,
}

impl ModuleBody {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (define, expressions, apply)) = permutation((
            Define::parse,
            context("module expressions", many1(ModuleExpr::parse)),
            opt(Apply::parse),
        ))(input)?;

        Ok((
            input,
            Self {
                define,
                expressions,
                apply,
            },
        ))
    }
}

#[derive(Debug, Clone)]
pub enum ModuleExpr {
    Term(Term),
    Action(Action),
    // Empty, // Import(ImportBody),
}

//------------ ModuleExpr ----------------------------------------------------
//
// ModuleExpr ::= Define |
//   Term+ |
//   Action+ |
//   'import' ForStatement '{' ImportBody '}'

impl ModuleExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, expressions) = context(
            "module expression",
            alt((
                map(Term::parse, Self::Term),
                map(Action::parse, Self::Action),
                // map(multispace1, |_| Self::Empty),
                // map(ImportBody::parse, ModuleBody::Import),
            )),
        )(input)?;
        Ok((input, expressions))
    }
}

//------------ Define -------------------------------------------------------
#[derive(Clone, Debug)]
pub struct Define {
    pub for_kv: Option<TypeIdentField>, // associated Rib record type
    pub with_kv: Vec<TypeIdentField>,   // arguments
    pub body: DefineBody,
}

impl Define {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (for_kv, with_kv, body)) = context(
            "define definition",
            preceded(
                opt_ws(tag("define")),
                tuple((
                    for_statement,
                    with_statement,
                    context(
                        "define block",
                        delimited(
                            opt_ws(char('{')),
                            DefineBody::parse,
                            opt_ws(char('}')),
                        ),
                    ),
                )),
            ),
        )(input)?;

        Ok((
            input,
            Self {
                for_kv,
                with_kv: with_kv.unwrap_or_default(),
                body,
            },
        ))
    }
}

//------------ DefineBody ---------------------------------------------------

// DefineBody ::=
//     (( 'use' Identifier ';' )? ( Identifier '=' ComputeExpr ';' )+ )+

#[derive(Clone, Debug)]
pub struct DefineBody {
    pub rx_type: TypeIdentField,
    pub tx_type: TypeIdentField,
    pub use_ext_data: Vec<(Identifier, Identifier)>,
    pub assignments: Vec<(Identifier, ValueExpr)>,
}

impl DefineBody {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, ((rx_type, tx_type), use_ext_data, assignments)) =
            tuple((
                permutation((
                    cut(delimited(
                        opt_ws(tag("rx")),
                        opt_ws(TypeIdentField::parse),
                        opt_ws(char(';')),
                    )),
                    cut(delimited(
                        opt_ws(tag("tx")),
                        opt_ws(TypeIdentField::parse),
                        opt_ws(char(';')),
                    )),
                )),
                many1(delimited(
                    opt_ws(tag("use")),
                    tuple((
                        opt_ws(Identifier::parse),
                        opt_ws(Identifier::parse),
                    )),
                    opt_ws(char(';')),
                )),
                many1(context(
                    "assignments",
                    separated_pair(
                        opt_ws(Identifier::parse),
                        preceded(multispace0, char('=')),
                        terminated(
                            opt_ws(ValueExpr::parse),
                            opt_ws(char(';')),
                        ),
                    ),
                )),
            ))(input)?;

        // let (use_ext_data, assignments) = statements.iter().cloned().unzip();

        Ok((
            input,
            Self {
                rx_type,
                tx_type,
                use_ext_data,
                assignments,
            },
        ))
    }
}

//------------ Term ---------------------------------------------------------

#[derive(Clone, Debug)]
pub struct Term {
    pub ident: Identifier,
    pub for_kv: Option<TypeIdentField>,
    pub with_kv: Vec<TypeIdentField>,
    pub body: TermBody,
}

impl Term {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (ident, for_kv, with_kv, body)) = context(
            "term definition",
            tuple((
                preceded(
                    opt_ws(tag("term")),
                    cut(context(
                        "term name",
                        delimited(
                            multispace1,
                            Identifier::parse,
                            multispace1,
                        ),
                    )),
                ),
                for_statement,
                with_statement,
                context(
                    "term block",
                    cut(delimited(
                        opt_ws(char('{')),
                        TermBody::parse,
                        opt_ws(char('}')),
                    )),
                ),
            )),
        )(input)?;

        Ok((
            input,
            Term {
                ident,
                for_kv,
                with_kv: with_kv.unwrap_or_default(),
                body,
            },
        ))
    }
}

//------------ TermBody -----------------------------------------------------

// TermBody ::= TermScope+

#[derive(Clone, Debug)]
pub struct TermBody {
    pub scopes: Vec<TermScope>,
}

impl TermBody {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, scopes) =
            context("term body", many0(TermScope::parse))(input)?;
        Ok((input, Self { scopes }))
    }
}

//------------ TermScope -----------------------------------------------------

// TermScope ::=
//      ('use' Identifier ';')?
//     ( MatchOperator '{' (ComputeExpr ';')+ '}' )+

#[derive(Clone, Debug)]
pub struct TermScope {
    pub scope: Option<Identifier>,
    pub operator: MatchOperator, // kinda' useless and kinda' incorrect.
    pub match_exprs: Vec<LogicalExpr>,
}

impl TermScope {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (scope, (operator, match_exprs))) = tuple((
            opt(opt_ws(context(
                "use scope",
                preceded(
                    opt_ws(tag("use")),
                    delimited(
                        multispace1,
                        Identifier::parse,
                        opt_ws(char(';')),
                    ),
                ),
            ))),
            context(
                "match expressions",
                tuple((
                    opt_ws(MatchOperator::parse),
                    delimited(
                        opt_ws(char('{')),
                        many0(context(
                            "match expression",
                            terminated(
                                opt_ws(LogicalExpr::parse),
                                opt_ws(char(';')),
                            ),
                        )),
                        opt_ws(char('}')),
                    ),
                )),
            ),
        ))(input)?;
        Ok((
            input,
            Self {
                scope,
                operator,
                match_exprs,
            },
        ))
    }
}

//------------ Action -------------------------------------------------------

// 'action' Identifier '{' ActionBody '}')* ForStatement WithStatement
//  '{' ActionBody '}'

#[derive(Clone, Debug)]
pub struct Action {
    pub ident: Identifier,
    // pub for_kv: Option<TypeIdentField>,
    // pub with_kv: Vec<TypeIdentField>,
    pub body: ActionBody,
}

impl Action {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (_, ident, body)) = context(
            "action definition",
            tuple((
                opt_ws(tag("action")),
                context(
                    "action name",
                    delimited(multispace1, Identifier::parse, multispace1),
                ),
                // for_statement,
                // with_statement,
                context(
                    "action block",
                    delimited(
                        opt_ws(char('{')),
                        ActionBody::parse,
                        opt_ws(char('}')),
                    ),
                ),
            )),
        )(input)?;

        Ok((
            input,
            Action {
                ident,
                // for_kv,
                // with_kv: with_kv.unwrap_or_default(),
                body,
            },
        ))
    }
}

//------------ ActionBody -----------------------------------------------------

// ActionBody ::= (ComputeExpr ';')+

#[derive(Clone, Debug)]
pub struct ActionBody {
    pub expressions: Vec<ComputeExpr>,
}

impl ActionBody {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, expressions) = context(
            "action body",
            many1(opt_ws(terminated(ComputeExpr::parse, opt_ws(char(';'))))),
        )(input)?;
        Ok((input, Self { expressions }))
    }
}

//------------ ImportBody -----------------------------------------------------

// #[derive(Clone, Debug)]
// pub struct ImportBody {}

//------------ Apply ---------------------------------------------------------

// Apply ::= 'apply' ForStatement WithStatement '{' ApplyBody '}'

#[derive(Clone, Debug)]
pub struct Apply {
    pub body: ApplyBody,
    pub for_kv: Option<TypeIdentField>,
    pub with_kv: Vec<TypeIdentField>,
}

impl Apply {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (for_kv, with_kv, body)) = context(
            "apply definition",
            preceded(
                opt_ws(tag("apply")),
                tuple((
                    for_statement,
                    with_statement,
                    context(
                        "apply block",
                        delimited(
                            opt_ws(char('{')),
                            ApplyBody::parse,
                            opt_ws(char('}')),
                        ),
                    ),
                )),
            ),
        )(input)?;

        Ok((
            input,
            Self {
                for_kv,
                with_kv: with_kv.unwrap_or_default(),
                body,
            },
        ))
    }
}

//------------ ApplyBody -----------------------------------------------------

// ApplyBody ::= ApplyScope+ (AcceptReject ';')?

#[derive(Clone, Debug)]
pub struct ApplyBody {
    pub scopes: Vec<ApplyScope>,
    pub accept_reject: Option<AcceptReject>,
}

impl ApplyBody {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (scopes, accept_reject)) = context(
            "apply body",
            tuple((
                many1(ApplyScope::parse),
                context("final accept reject", opt(opt_ws(accept_reject))),
            )),
        )(input)?;
        Ok((
            input,
            Self {
                scopes,
                accept_reject,
            },
        ))
    }
}

//------------ ApplyScope -----------------------------------------------------

// ApplyScope ::=
//      ( 'use' Identifier ';' )?
//      'filter' MatchOperator ( ComputeExpr | Identifier )
//      'not'? 'matching'
//      '{' ( ( ComputeExpr | Identifier ) ';' ( AcceptReject ';' )? )+ '}' ';'

#[derive(Clone, Debug)]
pub struct ApplyScope {
    pub scope: Identifier,
    pub operator: MatchOperator,
    pub filter_ident: ValueExpr,
    pub negate: bool,
    pub actions: Vec<(ValueExpr, Option<AcceptReject>)>,
}

impl ApplyScope {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (scope, operator, (filter_ident, negate, action_exprs))) =
            tuple((
                opt_ws(context(
                    "use scope",
                    preceded(
                        opt_ws(tag("use")),
                        delimited(
                            multispace1,
                            Identifier::parse,
                            opt_ws(char(';')),
                        ),
                    ),
                )),
                preceded(opt_ws(tag("filter")), opt_ws(MatchOperator::parse)),
                context(
                    "action expressions",
                    opt_ws(tuple((
                        ValueExpr::parse,
                        opt(terminated(
                            opt(opt_ws(tag("not"))),
                            opt_ws(tag("matching")),
                        )),
                        delimited(
                            opt_ws(char('{')),
                            many1(context(
                                "Call Expression",
                                tuple((
                                    opt_ws(terminated(
                                        ValueExpr::parse,
                                        opt_ws(char(';')),
                                    )),
                                    opt(opt_ws(accept_reject)),
                                )),
                            )),
                            terminated(opt_ws(char('}')), opt_ws(char(';'))),
                        ),
                    ))),
                ),
            ))(input)?;
        Ok((
            input,
            Self {
                scope,
                operator,
                negate: negate.is_none(),
                actions: action_exprs,
                filter_ident,
            },
        ))
    }
}

//------------ Rib -----------------------------------------------------------

// Rib ::= "rib" Identifier 'contains' TypeIdentifier '{' RibBody '}'

#[derive(Clone, Debug)]
pub struct Rib {
    pub ident: Identifier,
    pub contain_ty: TypeIdentifier,
    pub body: RibBody,
}

impl Rib {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (ident, contain_ty, body, _)) = context(
            "rib definition",
            tuple((
                preceded(
                    tag("rib"),
                    context(
                        "rib name",
                        delimited(
                            multispace1,
                            Identifier::parse,
                            multispace1,
                        ),
                    ),
                ),
                context(
                    "contains",
                    preceded(
                        opt_ws(tag("contains")),
                        delimited(
                            multispace1,
                            TypeIdentifier::parse,
                            multispace1,
                        ),
                    ),
                ),
                context(
                    "rib block",
                    cut(delimited(
                        opt_ws(char('{')),
                        RibBody::parse,
                        opt_ws(char('}')),
                    )),
                ),
                map(skip_opt_ws, |_| ()),
            )),
        )(input)?;

        Ok((
            input,
            Rib {
                ident,
                contain_ty,
                body,
            },
        ))
    }
}

#[derive(Clone, Debug)]
pub struct RibBody {
    pub key_values: Vec<RibField>,
}

#[derive(Clone, Debug)]
pub enum RibField {
    PrimitiveField(TypeIdentField),
    RecordField(Box<(Identifier, RecordTypeIdentifier)>),
    ListField(Box<(Identifier, ListTypeIdentifier)>),
}

//------------ RibBody -------------------------------------------------------

//
// The body of a Rib consists of an (optional) enumeration of
// (field_name, type) pairs.

// RibBody ::= ( Identifier ':' (
//                 TypeIdentifier |
//                 '{' RecordTypeIdentifier '}' |
//                 '[' ListTypeIdentifier ']' ) ','? )+

impl RibBody {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, key_values) = context(
            "items list",
            separated_list1(
                char(','),
                opt_ws(cut(alt((
                    map(TypeIdentField::parse, RibField::PrimitiveField),
                    map(
                        tuple((
                            terminated(
                                opt_ws(Identifier::parse),
                                opt_ws(char(':')),
                            ),
                            delimited(
                                opt_ws(char('{')),
                                RecordTypeIdentifier::parse,
                                opt_ws(char('}')),
                            ),
                        )),
                        |r| RibField::RecordField(Box::new(r)),
                    ),
                    map(
                        tuple((
                            terminated(
                                opt_ws(Identifier::parse),
                                opt_ws(char(':')),
                            ),
                            delimited(
                                opt_ws(char('[')),
                                ListTypeIdentifier::parse,
                                opt_ws(char(']')),
                            ),
                        )),
                        |l| RibField::ListField(Box::new(l)),
                    ),
                )))),
            ),
        )(input)?;
        Ok((input, RibBody { key_values }))
    }
}

//------------ Table -----------------------------------------------------

// Table ::= "table" Identifier 'contains' TypeIdentifier '{' TableBody '}'

#[derive(Clone, Debug)]
pub struct Table {
    pub ident: Identifier,
    pub contain_ty: TypeIdentifier,
    pub body: RibBody,
}

impl Table {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (ident, contain_ty, body, _)) = context(
            "table definition",
            tuple((
                preceded(
                    opt_ws(tag("table")),
                    context(
                        "table name",
                        delimited(
                            multispace1,
                            Identifier::parse,
                            multispace1,
                        ),
                    ),
                ),
                context(
                    "contains",
                    preceded(
                        opt_ws(tag("contains")),
                        delimited(
                            multispace1,
                            TypeIdentifier::parse,
                            multispace1,
                        ),
                    ),
                ),
                context(
                    "table block",
                    cut(delimited(
                        opt_ws(char('{')),
                        RibBody::parse,
                        opt_ws(char('}')),
                    )),
                ),
                map(skip_opt_ws, |_| ()),
            )),
        )(input)?;

        Ok((
            input,
            Table {
                ident,
                contain_ty,
                body,
            },
        ))
    }
}

//============ Separators ====================================================

/// Parses something preceded by optional white space.
fn opt_ws<'a, O, F>(
    parse: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, VerboseError<&str>>
where
    F: FnMut(&'a str) -> IResult<&str, O, VerboseError<&str>>,
{
    preceded(skip_opt_ws, parse)
}

/// Optional white space.
///
/// White space is all actual white space characters plus comments.
fn skip_opt_ws(input: &str) -> IResult<&str, (), VerboseError<&str>> {
    fold_many0(alt((map(multispace1, |_| ()), comment)), || (), |_, _| ())(
        input,
    )
}

/// Comments start with a hash and run to the end of a line.
fn comment(input: &str) -> IResult<&str, (), VerboseError<&str>> {
    let (input, _) = tuple((tag("//"), take_until("\n")))(input)?;
    map(tag_char('\n'), |_| ())(input)
}

//------------ Identifier ----------------------------------------------------

/// An identifier is the name of variables or other things.
///
/// It is a word composed of a leading alphabetic Unicode character, followed
/// by alphanumeric Unicode characters or underscore or hyphen.
///
/// Identifier ::= ([a-z]) ([0-9a-z-_])*
///
#[derive(Clone, Debug)]
pub struct Identifier {
    /// The actual identifier.
    pub ident: ShortString,
}

impl Identifier {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, _) = context(
            "no keyword",
            not(delimited(
                multispace1,
                tuple((
                    tag("for"),
                    tag("type"),
                    tag("with"),
                    tag("in"),
                    tag("define"),
                    tag("module"),
                    tag("import"),
                    tag("term"),
                    tag("filter"),
                    tag("match"),
                    tag("route"),
                    tag("matches"),
                    tag("return"),
                    tag("prefix"),
                    tag("true"),
                    tag("false"),
                    tag("apply"),
                    tag("use"),
                )),
                multispace1,
            )),
        )(input)?;
        let (input, ident) = context(
            "identifier",
            recognize(preceded(
                take_while1(|ch: char| {
                    ch.is_alphabetic() || ch == '_' || ch == '-'
                }),
                take_while(|ch: char| {
                    ch.is_alphanumeric() || ch == '_' || ch == '-'
                }),
            )),
        )(input)?;

        Ok((
            input,
            Identifier {
                ident: ident.into(),
            },
        ))
    }
}

impl AsRef<str> for Identifier {
    fn as_ref(&self) -> &str {
        self.ident.as_ref()
    }
}

impl borrow::Borrow<str> for Identifier {
    fn borrow(&self) -> &str {
        self.ident.as_ref()
    }
}

impl<T: AsRef<str>> PartialEq<T> for Identifier {
    fn eq(&self, other: &T) -> bool {
        self.ident == other.as_ref()
    }
}

impl Eq for Identifier {}

impl hash::Hash for Identifier {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.ident.hash(state)
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.ident.fmt(f)
    }
}

//------------ TypeIdentifier -----------------------------------------------

/// An identifier is the uniqur name of all expressions that we allow to be
/// named.
///
/// It is a word composed of a leading alphabetic Unicode character or an
/// underscore, followed by alphanumeric Unicode characters or underscore or
/// period.
#[derive(Clone, Debug)]
pub struct TypeIdentifier {
    /// The actual identifier.
    pub ident: ShortString,
}

/// A TypeIdentifier is the unique name of a type.

/// TypeIdentifier ::= [A-Z] ([0-9a-zA-Z])*

impl TypeIdentifier {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, ident) = recognize(pair(
            take_while1(|ch: char| ch.is_alphabetic() && ch.is_uppercase()),
            take_while(|ch: char| ch.is_alphanumeric()),
        ))(input)?;
        Ok((
            input,
            TypeIdentifier {
                ident: ident.into(),
            },
        ))
    }
}

impl AsRef<str> for TypeIdentifier {
    fn as_ref(&self) -> &str {
        self.ident.as_ref()
    }
}

impl borrow::Borrow<str> for TypeIdentifier {
    fn borrow(&self) -> &str {
        self.ident.as_ref()
    }
}

impl<T: AsRef<str>> PartialEq<T> for TypeIdentifier {
    fn eq(&self, other: &T) -> bool {
        self.ident == other.as_ref()
    }
}

impl Eq for TypeIdentifier {}

impl hash::Hash for TypeIdentifier {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.ident.hash(state)
    }
}

impl fmt::Display for TypeIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.ident.fmt(f)
    }
}

//------------ TypeIdentField ------------------------------------------------

/// A `field_name: Type` pair.

#[derive(Clone, Debug)]
pub struct TypeIdentField {
    /// The name of the field.
    pub field_name: Identifier,
    /// The type of the field.
    pub ty: TypeIdentifier,
}

impl TypeIdentField {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (field_name, ty)) = context(
            "key-value pair",
            separated_pair(
                preceded(multispace0, Identifier::parse),
                preceded(multispace0, char(':')),
                preceded(multispace0, TypeIdentifier::parse),
            ),
        )(input)?;

        Ok((input, Self { field_name, ty }))
    }
}

//------------ ListTypeIdentifier -------------------------------------------

// ListTypeIdentifier ::= '[' TypeIdentifier  ','? ']'+

#[derive(Clone, Debug)]
pub struct ListTypeIdentifier {
    pub inner_type: TypeIdentifier,
}

impl ListTypeIdentifier {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, inner_type) =
            context("List Value", TypeIdentifier::parse)(input)?;
        Ok((input, ListTypeIdentifier { inner_type }))
    }
}

//------------ StringLiteral -----------------------------------------------

// Our take on a literal string is just a Identifier wrapped in two double
// quotes. We don't do any escaping or anything like that.

// StringLiteral ::= '"' Identifier '"'
#[derive(Clone, Debug)]
pub struct StringLiteral(ShortString);

impl StringLiteral {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, ident) =
            delimited(char('"'), Identifier::parse, char('"'))(input)?;

        Ok((input, Self(ident.ident)))
    }
}

impl From<&'_ StringLiteral> for ShortString {
    fn from(literal: &StringLiteral) -> Self {
        literal.0.clone()
    }
}

//------------ RecordTypeIdentifier -----------------------------------------

// The value of a record. It's very similar to a RibBody (in EBNF it's the
// same), but it simplifies creating the SymbolTable, because they're
// semantically different.

// RecordIdentifier ::= '{' ( Identifier ':'
//                          TypeIdentifier | '{' RecordTypeIdentifier '}'
//                      ','? )+ '}'

#[derive(Clone, Debug)]
pub struct RecordTypeIdentifier {
    pub key_values: Vec<RibField>,
}

impl RecordTypeIdentifier {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, key_values) = context(
            "Record Value",
            separated_list1(
                char(','),
                opt_ws(cut(alt((
                    map(TypeIdentField::parse, RibField::PrimitiveField),
                    map(
                        tuple((
                            terminated(
                                opt_ws(Identifier::parse),
                                opt_ws(char(':')),
                            ),
                            delimited(
                                opt_ws(char('{')),
                                RecordTypeIdentifier::parse,
                                opt_ws(char('}')),
                            ),
                        )),
                        |r| RibField::RecordField(Box::new(r)),
                    ),
                )))),
            ),
        )(input)?;
        Ok((input, RecordTypeIdentifier { key_values }))
    }
}

//============= Literals ====================================================

//------------ IntegerLiteral -----------------------------------------------

/// An integer literal is a sequence of digits.
/// IntegerLiteral ::= [0-9]+
///
/// We parse it as a string and then convert it to an integer.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IntegerLiteral(pub i64);

impl IntegerLiteral {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, digits) = context(
            "integer literal",
            recognize(pair(
                opt(char('-')),
                take_while1(|ch: char| ch.is_ascii_digit()),
            )),
        )(input)?;

        let value = digits.parse().unwrap();
        Ok((input, Self(value)))
    }
}

impl From<&'_ IntegerLiteral> for ShortString {
    fn from(literal: &IntegerLiteral) -> Self {
        ShortString::from(literal.0.to_string().as_str())
    }
}

impl From<&'_ IntegerLiteral> for i64 {
    fn from(literal: &IntegerLiteral) -> Self {
        literal.0
    }
}

//------------ PrefixLengthLiteral ------------------------------------------

/// A prefix length literal is a sequence of digits preceded by a '/'.
/// PrefixLengthLiteral ::= /[0-9]+
///
/// We parse it as a string and then convert it to an integer.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PrefixLengthLiteral(pub usize);

impl PrefixLengthLiteral {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, digits) = context(
            "prefix length literal",
            preceded(char('/'), take_while1(|ch: char| ch.is_ascii_digit())),
        )(input)?;

        println!("digits: {}", digits);
        let value = digits.parse().unwrap();
        Ok((input, Self(value)))
    }
}

impl From<&'_ PrefixLengthLiteral> for ShortString {
    fn from(literal: &PrefixLengthLiteral) -> Self {
        ShortString::from(literal.0.to_string().as_str())
    }
}

impl From<&'_ PrefixLengthLiteral> for u8 {
    fn from(literal: &PrefixLengthLiteral) -> Self {
        literal.0 as u8
    }
}

//------------ HexLiteral ---------------------------------------------------

/// A hex literal is a sequence of hex digits, prefixed by '0x'
/// HexLiteral ::= '0x' [0-9a-fA-F]+
#[derive(Clone, Debug)]

pub struct HexLiteral(pub u64);

impl HexLiteral {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, digits) = context(
            "hex literal",
            recognize(pair(
                tag("0x"),
                take_while1(|ch: char| ch.is_ascii_hexdigit()),
            )),
        )(input)?;

        let value = u64::from_str_radix(&digits[2..], 16).unwrap();
        Ok((input, Self(value)))
    }
}

impl From<&'_ HexLiteral> for ShortString {
    fn from(literal: &HexLiteral) -> Self {
        ShortString::from(literal.0.to_string().as_str())
    }
}

impl From<&'_ HexLiteral> for u64 {
    fn from(literal: &HexLiteral) -> Self {
        literal.0
    }
}

//------------ AsnLiteral ---------------------------------------------------

/// An ASN literal is a sequence of hex digits, prefixed by 'AS'
///
///
#[derive(Clone, Debug)]

pub struct AsnLiteral(pub u32);

impl AsnLiteral {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, digits) = context(
            "ASN literal",
            recognize(pair(
                tag("AS"),
                take_while1(|ch: char| ch.is_ascii_hexdigit()),
            )),
        )(input)?;

        let value = digits[2..].parse::<u32>().unwrap();
        Ok((input, Self(value)))
    }
}

impl From<&'_ AsnLiteral> for ShortString {
    fn from(literal: &AsnLiteral) -> Self {
        ShortString::from(literal.0.to_string().as_str())
    }
}

impl From<&'_ AsnLiteral> for Asn {
    fn from(literal: &AsnLiteral) -> Self {
        Asn::from_u32(literal.0)
    }
}

//------------ FloatLiteral -------------------------------------------------

/// A float literal is a sequence of digits with a decimal point.
/// FloatLiteral ::= [0-9]+ '.' [0-9]+
///
#[derive(Clone, Debug)]
pub struct FloatLiteral(pub f64);

impl FloatLiteral {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, digits) = context(
            "float literal",
            recognize(tuple((
                opt(char('-')),
                take_while1(|ch: char| ch.is_ascii_digit()),
                char('.'),
                take_while1(|ch: char| ch.is_ascii_digit()),
            ))),
        )(input)?;

        let value = digits.parse().unwrap();
        Ok((input, Self(value)))
    }
}

//------------ BooleanLiteral -----------------------------------------------
/// A boolean literal is either `true` or `false`.
/// BooleanLiteral ::= 'true' | 'false'
#[derive(Clone, Debug)]

pub struct BooleanLiteral(pub bool);

impl BooleanLiteral {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, value) = alt((
            map(tag("true"), |_| Self(true)),
            map(tag("false"), |_| Self(false)),
        ))(input)?;

        Ok((input, value))
    }
}

impl From<&'_ BooleanLiteral> for ShortString {
    fn from(literal: &BooleanLiteral) -> Self {
        ShortString::from(literal.0.to_string().as_str())
    }
}

impl From<&'_ BooleanLiteral> for Boolean {
    fn from(literal: &BooleanLiteral) -> Self {
        Boolean(Some(literal.0))
    }
}

//------------ ByteStringLiteral --------------------------------------------
/// A byte string literal is a sequence of bytes, preceded by '0x'
/// ByteStringLiteral ::= 0x[0-9a-fA-F]+

#[derive(Clone, Debug)]
pub struct ByteStringLiteral(pub SmallVec<[u8; 24]>);

impl<'a> ByteStringLiteral {
    pub fn parse(input: &'a str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, value) = (preceded(
            tag("0x"),
            many1(terminated(take(2_usize), opt(char('_')))),
        ))(input)?;

        let mut result_value: SmallVec<[u8; 24]> = SmallVec::new();

        for v in &value {
            let v = u8::from_str_radix(v, 16);

            if let Ok(v) = v {
                result_value.push(v);
            } else {
                return Err(nom::Err::Error(VerboseError::from_error_kind(
                    input,
                    ErrorKind::Digit,
                )));
            }
        }

        Ok((input, Self(result_value)))
    }
}

//------------ AcceptReject -------------------------------------------------

// Every filter needs to return either a 'accept' or 'reject' statement.
// failing to set it properly ends in the whole thing being cancelled.

// AcceptReject ::= 'return'? ( 'accept' | 'reject' ) ';'

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AcceptReject {
    Accept,
    Reject,
    NoReturn,
}

fn accept_reject(
    input: &str,
) -> IResult<&str, AcceptReject, VerboseError<&str>> {
    context(
        "accept or reject",
        preceded(
            opt(opt_ws(tag("return"))),
            alt((
                map(
                    terminated(opt_ws(tag("accept")), opt_ws(char(';'))),
                    |_| AcceptReject::Accept,
                ),
                map(
                    terminated(opt_ws(tag("reject")), opt_ws(char(';'))),
                    |_| AcceptReject::Reject,
                ),
            )),
        ),
    )(input)
}

//------------ ValueExpr --------------------------------------------------

// ValueExpr ::=
//  *Literal |
//  ComputeExpr |
//  BuiltinMethodCallExpr

#[derive(Clone, Debug)]
pub enum ValueExpr {
    StringLiteral(StringLiteral),
    IntegerLiteral(IntegerLiteral),
    PrefixLengthLiteral(PrefixLengthLiteral),
    AsnLiteral(AsnLiteral),
    HexLiteral(HexLiteral),
    BooleanLit(BooleanLiteral),
    PrefixMatchExpr(PrefixMatchExpr),
    ComputeExpr(ComputeExpr),
    BuiltinMethodCallExpr(MethodComputeExpr),
}

impl ValueExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        alt((
            map(StringLiteral::parse, ValueExpr::StringLiteral),
            map(HexLiteral::parse, ValueExpr::HexLiteral),
            map(IntegerLiteral::parse, ValueExpr::IntegerLiteral),
            map(PrefixLengthLiteral::parse, ValueExpr::PrefixLengthLiteral),
            map(AsnLiteral::parse, ValueExpr::AsnLiteral),
            map(tag("true"), |_| ValueExpr::BooleanLit(BooleanLiteral(true))),
            map(tag("false"), |_| {
                ValueExpr::BooleanLit(BooleanLiteral(false))
            }),
            map(PrefixMatchExpr::parse, ValueExpr::PrefixMatchExpr),
            map(MethodComputeExpr::parse, ValueExpr::BuiltinMethodCallExpr),
            map(ComputeExpr::parse, ValueExpr::ComputeExpr),
        ))(input)
    }
}

//------------ ArgExprList -------------------------------------------------

// ArgExprList ::= ( ValueExpr ','? )*

#[derive(Clone, Debug)]
pub struct ArgExprList {
    pub args: Vec<ValueExpr>,
}

impl ArgExprList {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, args) = context(
            "argument expressions",
            separated_list0(
                preceded(multispace0, char(',')),
                opt_ws(ValueExpr::parse),
            ),
        )(input)?;

        Ok((input, Self { args }))
    }

    pub fn is_empty(&self) -> bool {
        self.args.is_empty()
    }
}

//------------ for & with statements ----------------------------------------

// ForStatement ::= ('for' Identifier':' TypeIdentifier)?

fn for_statement(
    input: &str,
) -> IResult<&str, Option<TypeIdentField>, VerboseError<&str>> {
    context(
        "for",
        opt(preceded(opt_ws(tag("for")), TypeIdentField::parse)),
    )(input)
}

// WithStatement ::= ('with' (Identifier':' TypeIdentifier)+)?

fn with_statement(
    input: &str,
) -> IResult<&str, Option<Vec<TypeIdentField>>, VerboseError<&str>> {
    context(
        "with",
        opt(preceded(
            opt_ws(tag("with")),
            separated_list1(char(','), TypeIdentField::parse),
        )),
    )(input)
}

// ============ Compound Expressions ========================================

// Compound expressions consist of field access of data structures or method
// calls on data structures.

//------------- AccessExpr --------------------------------------------------

// Either a method call or a field access. Used as part of a ComputeExpr.

// AccessExpr ::= MethodComputeExpr | AccessReceiver

#[derive(Clone, Debug)]
pub enum AccessExpr {
    MethodComputeExpr(MethodComputeExpr),
    FieldAccessExpr(FieldAccessExpr),
}

impl AccessExpr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        alt((
            map(MethodComputeExpr::parse, AccessExpr::MethodComputeExpr),
            map(FieldAccessExpr::parse, AccessExpr::FieldAccessExpr),
        ))(input)
    }
}

//------------- ComputeExpr ----------------------------------------------------

// It's complete EBNF would be:
// CompoundExpr ::= AccessReceiver?(.MethodComputeExpr)? | AccessReceiver

// A ComputeExpr is an expression that starts with a Sub Call Expression,
// optionally followed by one or more method calls, and/or access receivers,
// e.g. 'rib-rov.longest_match(route.prefix).prefix.len()`.
//
// Note that an expression ending in a field acces, i.e. not a method call,
// is also parsed as a ComputeExpr, but with an empty method call list.

#[derive(Clone, Debug)]
pub struct ComputeExpr {
    pub receiver: AccessReceiver,
    pub access_expr: Vec<AccessExpr>,
}

impl ComputeExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (receiver, access_expr)) = tuple((
            AccessReceiver::parse,
            many0(preceded(char('.'), AccessExpr::parse)),
        ))(input)?;
        Ok((
            input,
            Self {
                receiver,
                access_expr,
            },
        ))
    }

    pub fn get_receiver(&self) -> AccessReceiver {
        self.receiver.clone()
    }
}

//------------- AccessReceiver ------------------------------------------------

// The AccessReceiver is the specifier of a data structure that is being called
// (used as part of a ComputeExpr) or used to retrieve one of its fields. Can also
// be a stand-alone specifier.

// CallReceiver ::= Identifier

#[derive(Clone, Debug)]
pub struct AccessReceiver {
    // The identifier of the data structure.
    pub ident: Identifier,
}

impl AccessReceiver {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, receiver) =
            context("access receiver", Identifier::parse)(input)?;

        Ok((input, Self { ident: receiver }))
    }
}

impl AccessReceiver {
    pub fn get_ident(&self) -> &Identifier {
        &self.ident
    }
}

//------------- FieldAccessExpr ---------------------------------------------

// The chain of fields that are being accesed. The last field is the name of
// the field that is accessed.

// FieldAccessExpr ::= ( '.'? Identifier )+

#[derive(Clone, Debug)]
pub struct FieldAccessExpr {
    // The chain of fields that are being accessed. The last field is the
    // name of the field that is accessed.
    pub field_names: Vec<Identifier>,
}

impl FieldAccessExpr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, field_names) = context(
            "field expression",
            separated_list1(
                char('.'),
                terminated(Identifier::parse, not(char('('))),
            ),
        )(input)?;
        Ok((input, Self { field_names }))
    }
}

//------------- MethodComputeExpr  ---------------------------------------------

// The method that is being called on the data structure (directly or on one
// of its fields).

// MethodComputeExpr ::= Identifier '(' ArgExprList ')'

#[derive(Clone, Debug)]
pub struct MethodComputeExpr {
    // The name of the method.
    pub ident: Identifier,
    // The list with arguments
    pub args: ArgExprList,
}

impl MethodComputeExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (ident, args)) = context(
            "method call expression",
            tuple((
                Identifier::parse,
                delimited(
                    opt_ws(char('(')),
                    ArgExprList::parse,
                    opt_ws(char(')')),
                ),
            )),
        )(input)?;
        Ok((input, Self { ident, args }))
    }
}

//============ First-Order Logic ============================================

// "No, no, you're not thinking. You're just being logical." -- Niels Bohr

// A first-order logical formula is a sequence of (predicate) symbols,
// logical connectives (a.k.a. logical operators), comparison operators, and
// quantifiers. Logical formulas can be grouped (for operator precedence) and
// nested through the contained symbols. Nesting will raise the arity of the
// formula.
//
// We are only using a binary logical formula that consists of a tuple of
// (boolean expression, logical operator, boolean expression), where boolean
// expressions can be grouped with parentheses. Also, we are only the
// connectives listed in this enum, i.e. ∧ (and), ∨ (or), and ¬ (not).
//
// This hardly limits the logical expressiveness of our language, because the
// user can:
// - Create the missing connectives (material implication ('if-then'),
//   bi-directional implication ('if and only if') from the given connectives
//   (for an arity of 2 at least) or they can be constructed in code in the
//   `apply` section.
// - Use the logical connectives in a grouped fashion to reduce
//   any number of Boolean expressions down to a binary formula, e.g.
//   (A ∧ B) ∨ (C ∧ D) is equivalent to A ∧ B ∨ C ∧ D. This limits the number
//   of boolean functions we have to consider when evaluating the formula,
//   i.e. a fully complete set of boolean functions has 16 functions for an
//   arity of 2.
//
// The first point above reduces the cognitive overhead by simplifying the
// flow of the program and using familar constructs, like 'if..then' and
// early returns.
//
// The second point also reduces (perceicved) ambiguity because we do not
// allow using implicit logic operator precedence (that probably no one
// knows anyway).

// https://en.wikipedia.org/wiki/First-order_logic#Formulas

// The form in which roto users can express logical formulas, etc. is:

// term {
//      <logical formula>;
//      <logical formula>;
//      ...
// }
//
// where <logical formula> is one of:
//
// <boolean expression> || <boolean expression>;
// <boolean expression> && <boolean expression>;
// !<boolean expression>;
// <boolean expression>;

// Ex.:
// ┌──────────┐┌──┐┌──────────┐ ┌──┐ ┌──────────┐
// │(BoolExpr)││&&││(BoolExpr)│ │||│ │(BoolExpr)│
// └──────────┘└──┘└──────────┘ └──┘ └──────────┘
// ▲──────────LF::And─────────▲      ▲──LF::BE──▲
//
// ▲──────────BoolExpr────────▲      ▲─BoolExpr─▲
//
// ▲────────────────────LF::Or──────────────────▲

//------------ LogicalExpr --------------------------------------------------

// The Logical expression evaluates to a logical forumula, that is a tuple of
// (optional boolean expression, logical operator, boolean expression).
// The first boolean expression is optional, only in the case of a negation
// (not) operator. The second boolean expression is always present.

// LogicalExpr ::= OrExpr | AndExpr | NotExpr | BooleanExpr

#[derive(Clone, Debug)]
pub enum LogicalExpr {
    OrExpr(OrExpr),
    AndExpr(AndExpr),
    NotExpr(NotExpr),
    BooleanExpr(BooleanExpr),
}

impl LogicalExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        opt_ws(alt((
            map(AndExpr::parse, LogicalExpr::AndExpr),
            map(OrExpr::parse, LogicalExpr::OrExpr),
            map(NotExpr::parse, LogicalExpr::NotExpr),
            map(BooleanExpr::parse, LogicalExpr::BooleanExpr),
        )))(input)
    }
}

#[derive(Clone, Debug)]
pub enum CompareArg {
    // A "stand-alone" left|right-hand side argument of a comparison
    ValueExpr(ValueExpr),
    // A nested logical formula, e.g. (A && B) || (C && D) used as a left|
    // right-hand side argument of a comparison. Note that this can only
    // have the opposite hand be a boolean expression.
    GroupedLogicalExpr(GroupedLogicalExpr),
}

impl CompareArg {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        alt((
            map(GroupedLogicalExpr::parse, CompareArg::GroupedLogicalExpr),
            map(ValueExpr::parse, CompareArg::ValueExpr),
        ))(input)
    }
}

//------------ BooleanExpr --------------------------------------------------

// A Boolean expression is an expression that *may* evaluate to one of:
// - a Boolean-valued function, which is a fn : X → B, where X is an
//   arbitrary set and B is a boolean value. For example, an Integer
//   expresssion can never evaluate to a boolean value, but a method call
//   expression may evaluate to a method that returns a Boolean value.
// - a Literal Boolean value, "true" or "false"
// - a Boolean-typed variable, including boolean-typed record fields
// - an Expression containing a boolean-valued operator, such as '==', '!=',
//   ">=", "<="

// BooleanExpr ::= BooleanLiteral | ComputeExpr | CompareExpr
//          | AccessReceiver | SetcompareExpr | PrefixMatchExpr
//          | Identifier

#[derive(Clone, Debug)]
pub enum BooleanExpr {
    // A complete formula that is wrapped in parentheses is a Boolean-Valued
    // Function, since it will always return a Boolean value.
    GroupedLogicalExpr(GroupedLogicalExpr),
    // "true" | "false" literals
    BooleanLiteral(BooleanLiteral),
    // A syntactically correct comparison always evaluates to a
    // Boolean-Valued Function, since it will always return a Boolean value.
    CompareExpr(Box<CompareExpr>),
    // A ComputeExpression *may* evaluate to a function that returns a boolean
    ComputeExpr(ComputeExpr),
    // Set Compare expression, will *always* result in a boolean-valued
    // function. Syntactic sugar for a truth-function that performs
    // fn : a -> {a} ∩ B
    SetCompareExpr(Box<SetCompareExpr>),
    // syntactic sugar for a method on a prefix function that returns a
    // boolean.
    PrefixMatchExpr(PrefixMatchExpr),
}

impl BooleanExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        alt((
            map(GroupedLogicalExpr::parse, BooleanExpr::GroupedLogicalExpr),
            map(CompareExpr::parse, |e| {
                BooleanExpr::CompareExpr(Box::new(e))
            }),
            map(ComputeExpr::parse, BooleanExpr::ComputeExpr),
            map(SetCompareExpr::parse, |e| {
                BooleanExpr::SetCompareExpr(Box::new(e))
            }),
            map(PrefixMatchExpr::parse, BooleanExpr::PrefixMatchExpr),
            map(BooleanLiteral::parse, BooleanExpr::BooleanLiteral),
        ))(input)
    }
}

//------------ CompareExpr --------------------------------------------------

// CompareExpr ::= ValueExpr CompareOp ValueExpr

#[derive(Clone, Debug)]
pub struct CompareExpr {
    pub left: CompareArg,
    pub op: CompareOp,
    pub right: CompareArg,
}

impl CompareExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (left, op, right)) = context(
            "Compare Expression",
            tuple((
                opt_ws(CompareArg::parse),
                opt_ws(alt((
                    map(tag("=="), |_| CompareOp::Eq),
                    map(tag("!="), |_| CompareOp::Ne),
                    map(tag("<="), |_| CompareOp::Le),
                    map(tag("<"), |_| CompareOp::Lt),
                    map(tag(">="), |_| CompareOp::Ge),
                    map(tag(">"), |_| CompareOp::Gt),
                ))),
                opt_ws(CompareArg::parse),
            )),
        )(input)?;

        Ok((input, Self { left, op, right }))
    }
}

//------------ CompareOp ----------------------------------------------------

// CompareOp ::= '==' | '!=' | '<' | '<=' | '>' | '>='

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CompareOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

//------------ AndExpr ------------------------------------------------------

// AndExpr ::= MatchExpr '&&' MatchExpr

#[derive(Clone, Debug)]
pub struct AndExpr {
    pub left: BooleanExpr,
    pub right: BooleanExpr,
}

impl AndExpr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (left, right)) = tuple((
            opt_ws(BooleanExpr::parse),
            preceded(opt_ws(tag("&&")), opt_ws(BooleanExpr::parse)),
        ))(input)?;

        Ok((input, Self { left, right }))
    }
}

//------------ OrExpr -------------------------------------------------------

// OrExpr ::= MatchExpr '||' MatchExpr

#[derive(Clone, Debug)]
pub struct OrExpr {
    pub left: BooleanExpr,
    pub right: BooleanExpr,
}

impl OrExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (left, right)) = context(
            "or expr",
            tuple((
                opt_ws(BooleanExpr::parse),
                preceded(opt_ws(tag("||")), opt_ws(BooleanExpr::parse)),
            )),
        )(input)?;

        Ok((input, Self { left, right }))
    }
}

#[derive(Clone, Debug)]
pub struct NotExpr {
    pub expr: BooleanExpr,
}

impl NotExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, expr) =
            preceded(opt_ws(tag("!")), opt_ws(BooleanExpr::parse))(input)?;
        Ok((input, Self { expr }))
    }
}

//------------ SetCompareExpr -----------------------------------------------

// SetCompareExpr ::= ( ValueExpr ( 'in' | 'not in' ) ValueExpr )+

#[derive(Clone, Debug)]
pub struct SetCompareExpr {
    pub left: CompareArg,
    pub op: SetCompareOp,
    pub right: CompareArg,
}

impl SetCompareExpr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (left, op, right)) = tuple((
            opt_ws(CompareArg::parse),
            alt((
                map(opt_ws(tag("in")), |_| SetCompareOp::In),
                map(opt_ws(tag("not in")), |_| SetCompareOp::NotIn),
            )),
            opt_ws(CompareArg::parse),
        ))(input)?;

        Ok((input, Self { left, op, right }))
    }
}

//------------ SetCompareOp -------------------------------------------------

// SetCompareOp ::= 'in' | 'not in'

#[derive(Clone, Debug)]
pub enum SetCompareOp {
    In,
    NotIn,
}

//------------- GroupedFormulaExpr ------------------------------------------

// GroupedFormulaExpr ::= '(' LogicalExpr ')'

#[derive(Clone, Debug)]
pub struct GroupedLogicalExpr {
    pub expr: Box<LogicalExpr>,
}

impl GroupedLogicalExpr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, expr) = delimited(
            opt_ws(char('(')),
            LogicalExpr::parse,
            opt_ws(char(')')),
        )(input)?;
        Ok((
            input,
            Self {
                expr: Box::new(expr),
            },
        ))
    }
}

#[derive(Clone, Debug)]
pub enum MatchOperator {
    Match,
    Some,
    ExactlyOne,
    All,
}

impl MatchOperator {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        alt((
            map(tag("match"), |_| MatchOperator::Match),
            map(tag("some"), |_| MatchOperator::Some),
            map(tag("exactly-one"), |_| MatchOperator::ExactlyOne),
            map(tag("all"), |_| MatchOperator::All),
        ))(input)
    }
}

//------------ PrefixMatchType ------------------------------------------

// PrefixMatchType ::= ( 'exact' | 'longer' | 'orlonger' |
//      'prefix-length-range' | 'upto' | 'through' | 'netmask' )
//      ( PrefixLength | PrefixLengthRange | IpAddress )

#[derive(Clone, Debug)]
pub enum PrefixMatchType {
    Exact,
    Longer,
    OrLonger,
    PrefixLengthRange(PrefixLengthRange),
    UpTo(PrefixLength),
    Through(PrefixLength),
    NetMask(IpAddress),
}

impl PrefixMatchType {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, res) = alt((
            map(tag("exact"), |_| PrefixMatchType::Exact),
            map(tag("longer"), |_| PrefixMatchType::Longer),
            map(tag("orlonger"), |_| PrefixMatchType::OrLonger),
            map(
                preceded(
                    tag("prefix-length-range"),
                    opt_ws(PrefixLengthRange::parse),
                ),
                PrefixMatchType::PrefixLengthRange,
            ),
            map(
                preceded(tag("upto"), opt_ws(PrefixLength::parse)),
                PrefixMatchType::UpTo,
            ),
            map(
                preceded(tag("through"), opt_ws(PrefixLength::parse)),
                PrefixMatchType::Through,
            ),
            map(
                preceded(tag("netmask"), opt_ws(IpAddress::parse)),
                PrefixMatchType::NetMask,
            ),
        ))(input)?;

        Ok((input, res))
    }
}

//------------ PrefixMatchExpr ----------------------------------------------

// PrefixMatchExpr ::= Prefix PrefixMatchType

#[derive(Clone, Debug)]
pub struct PrefixMatchExpr {
    pub prefix: Prefix,
    pub ty: PrefixMatchType,
}

impl PrefixMatchExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (prefix, ty)) = tuple((
            opt_ws(Prefix::parse),
            opt_ws(PrefixMatchType::parse),
        ))(input)?;

        Ok((input, Self { prefix, ty }))
    }
}

//------------ IpAddress ----------------------------------------------------

// IpAddress ::= IpV4Address | IpV6Address

#[derive(Clone, Debug)]
pub enum IpAddress {
    Ipv4(Ipv4Addr),
    Ipv6(Ipv6Addr),
}

impl IpAddress {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, res) = alt((
            map(Ipv4Addr::parse, IpAddress::Ipv4),
            map(Ipv6Addr::parse, IpAddress::Ipv6),
        ))(input)?;

        Ok((input, res))
    }
}

//------------ Ipv4Addr -----------------------------------------------------

// Ipv4Addr ::= <ipv4 address>
#[derive(Clone, Debug)]
pub struct Ipv4Addr(std::net::Ipv4Addr);

impl Ipv4Addr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        input
            .parse::<std::net::Ipv4Addr>()
            .map(|addr| (input, Self(addr)))
            .map_err(|_| {
                nom::Err::Error(VerboseError::from_error_kind(
                    input,
                    ErrorKind::Digit,
                ))
            })
    }
}

//------------ Ipv6Addr -----------------------------------------------------

// Ipv6Addr ::= <ipv6 address>

#[derive(Clone, Debug)]
pub struct Ipv6Addr(std::net::Ipv6Addr);

impl Ipv6Addr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        input
            .parse::<std::net::Ipv6Addr>()
            .map(|addr| (input, Self(addr)))
            .map_err(|_| {
                nom::Err::Error(VerboseError::from_error_kind(
                    input,
                    ErrorKind::Digit,
                ))
            })
    }
}

//------------ Prefix -------------------------------------------------------

// Prefix ::= IpAddress '/' PrefixLength

#[derive(Clone, Debug)]
pub struct Prefix {
    pub addr: IpAddress,
    pub len: PrefixLength,
}

impl Prefix {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, ((_, addr), len)) = tuple((
            map_res(opt_ws(is_not("/")), IpAddress::parse),
            opt_ws(PrefixLength::parse),
        ))(input)?;

        Ok((input, Prefix { addr, len }))
    }
}

//------------ PrefixLength -------------------------------------------------

// PrefixLength ::= '/' <u8>

#[derive(Clone, Debug)]
pub struct PrefixLength(u8);

impl PrefixLength {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, length) = preceded(char('/'), opt_ws(digit1))(input)?;

        let length = length.parse::<u8>().map_err(|_| {
            nom::Err::Error(VerboseError::from_error_kind(
                input,
                ErrorKind::Digit,
            ))
        })?;

        Ok((input, Self(length)))
    }
}

//------------ PrefixLengthRange --------------------------------------------

// PrefixLengthRange ::= PrefixLength '-' PrefixLength

#[derive(Clone, Debug)]
pub struct PrefixLengthRange {
    pub start: PrefixLength,
    pub end: PrefixLength,
}

impl PrefixLengthRange {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (start, end)) = tuple((
            opt_ws(PrefixLength::parse),
            preceded(char('-'), opt_ws(PrefixLength::parse)),
        ))(input)?;

        Ok((input, Self { start, end }))
    }
}

//------------ ShortString ---------------------------------------------------

#[derive(Clone)]
pub struct ShortString {
    bytes: SmallVec<[u8; 24]>,
}

impl ShortString {
    pub fn as_bytes(&self) -> &[u8] {
        &self.bytes
    }

    pub fn as_str(&self) -> &str {
        unsafe { str::from_utf8_unchecked(&self.bytes) }
    }
}

impl<'a> From<&'a str> for ShortString {
    fn from(src: &'a str) -> ShortString {
        ShortString {
            bytes: SmallVec::from_slice(src.as_bytes()),
        }
    }
}

impl ops::Deref for ShortString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl AsRef<str> for ShortString {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl AsRef<[u8]> for ShortString {
    fn as_ref(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl borrow::Borrow<str> for ShortString {
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl borrow::Borrow<[u8]> for ShortString {
    fn borrow(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl<T: AsRef<str>> PartialEq<T> for ShortString {
    fn eq(&self, other: &T) -> bool {
        self.as_str().eq(other.as_ref())
    }
}

impl Eq for ShortString {}

impl<T: AsRef<str>> PartialOrd<T> for ShortString {
    fn partial_cmp(&self, other: &T) -> Option<cmp::Ordering> {
        self.as_str().partial_cmp(other.as_ref())
    }
}

impl Ord for ShortString {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl hash::Hash for ShortString {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}

impl fmt::Display for ShortString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl fmt::Debug for ShortString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self.as_str(), f)
    }
}
