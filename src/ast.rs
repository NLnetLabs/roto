use std::{borrow, cmp, fmt, hash, ops, str};

use nom::branch::alt;
use nom::bytes::complete::{take_while, take_while1};
use nom::character::complete::{
    char, multispace0, multispace1,
};
use nom::combinator::{cut, opt, recognize};
use nom::error::{context, VerboseError};
use nom::multi::{
    fold_many0, many0, many1, separated_list0, separated_list1,
};
use nom::sequence::{
    delimited, pair, preceded, separated_pair, terminated, tuple,
};
use nom::{
    bytes::complete::{tag, take_until},
    character::complete::char as tag_char,
    combinator::map,
    IResult,
};
use nom::Finish;
use smallvec::SmallVec;

/// ======== Root ===========================================================

/// The Root of the file.
///
/// Root ::= (
///     "module" ModuleIdentifier (ForStatement)?  (WithStatement)*  '{' ModuleBody '}' |
///     "rib" RibIdentifier 'contains' TypeIdentifier '{' RibBody '}' |
///     "prefix-list" PrefixListIdentifier '{' PrefixListBody '}' |
///     "table" TableIdentifier '{' TableBody '}' |
///     Comment
/// )+

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
        let (input, expressions) = cut(many1(preceded(
            opt(comment),
            terminated(RootExpr::parse, opt(comment)),
        )))(input)?;
        Ok((input, Self { expressions }))
    }
}

#[derive(Debug, Clone)]
pub enum RootExpr {
    Module(Module),
    Rib(Rib),
    // PrefixList(PrefixListExpr),
    // Table(TableExpr),
}

impl RootExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, expressions) = alt((
            map(Rib::parse, Self::Rib),
            map(Module::parse, Self::Module),
        ))(input)?;
        Ok((input, expressions))
    }
}

//------------ Module --------------------------------------------------------

#[derive(Clone, Debug)]
pub struct Module {
    pub ident: Identifier,
    pub for_kv: Option<TypeIdentField>,
    pub with_kv: Vec<TypeIdentField>,
    pub body: ModuleBody,
}

impl Module {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (_, ident, for_kv, with_kv, body, _)) = context(
            "module definition",
            tuple((
                opt_ws(tag("module")),
                context(
                    "module name",
                    delimited(multispace1, Identifier::parse, multispace1),
                ),
                for_statement,
                with_statement,
                context(
                    "module body",
                    delimited(
                        opt_ws(char('{')),
                        ModuleBody::parse,
                        opt_ws(char('}')),
                    ),
                ),
                map(many0(char('\n')), |_| ()),
            )),
        )(input)?;

        Ok((
            input,
            Module {
                ident,
                body,
                for_kv,
                with_kv: with_kv.unwrap_or_default(),
            },
        ))
    }
}

//------------ ModuleBody ---------------------------------------------------

// ModuleBody ::= (
//     'define' (ForStatement)?  (WithStatement)* '{' DefineBody '}' |
//     ('term' TermIdentifier '{' TermBody '}')* (ForStatement)?  (WithStatement)* '{' DefineBody '}' |
//     ('action' ActionIndentifier '{' ActionBody '}')* (ForStatement)?  (WithStatement)* '{' DefineBody '}' |
//     ('import' '{' ImportBody '}')* (ForStatement)? |
//     'apply' '{' ApplyBody '}' (ForStatement)?  (WithStatement)* '{' DefineBody '}'
// )+

#[derive(Clone, Debug)]
pub struct ModuleBody {
    pub define: Define,
    pub expressions: Vec<ModuleExpr>,
    pub apply: Option<Apply>,
}

impl ModuleBody {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (
            input,
            (
                define, //(define_ident, define_for_kv, define_with_kv, define),
                expressions,
                apply, //(apply_for_kv, apply_with_kv, apply),
            ),
        ) = tuple((
            context(
                "define definition",
                preceded(
                    opt_ws(tag("define")),
                    tuple((
                        context(
                            "define name",
                            opt(delimited(
                                multispace1,
                                Identifier::parse,
                                multispace1,
                            )),
                        ),
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
                        map(many0(char('\n')), |_| ()),
                    )),
                ),
            ),
            context(
                "module expressions",
                cut(many0(opt_ws(ModuleExpr::parse))),
            ),
            context(
                "apply definition",
                opt(preceded(
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
                )),
            ),
        ))(input)?;

        Ok((
            input,
            Self {
                define: Define {
                    ident: define.0,
                    for_kv: define.1,
                    with_kv: define.2.unwrap_or_default(),
                    body: define.3,
                },
                // define: {
                //     ident: define_ident,
                //     for_kv: define_for_kv,
                //     with_kv: define_with_kv.unwrap_or_default(),
                //     body: define,
                // },
                expressions,
                apply: apply.map(|a| Apply {
                    for_kv: a.0,
                    with_kv: a.1.unwrap_or_default(),
                    body: a.2,
                }),
            },
        ))
    }
}

#[derive(Debug, Clone)]
pub enum ModuleExpr {
    Term(Term),
    Action(Action),
    // Import(ImportBody),
}

impl ModuleExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, expressions) = alt((
            map(Term::parse, Self::Term),
            map(Action::parse, Self::Action),
            // map(ImportBody::parse, ModuleBody::Import),
            // map(ApplyBody::parse, |apply| { Self::Apply(apply) }),
        ))(input)?;
        Ok((input, expressions))
    }
}

//------------ Define -------------------------------------------------------
#[derive(Clone, Debug)]
pub struct Define {
    pub ident: Option<Identifier>,
    pub for_kv: Option<TypeIdentField>,
    pub with_kv: Vec<TypeIdentField>,
    pub body: DefineBody,
}

//------------ DefineBody ---------------------------------------------------

// DefineBody ::=
//  ('use' RibIdentifier ';')? ( VariableIdentifier '=' TypeIdentifier ';')+

#[derive(Clone, Debug)]
pub struct DefineBody {
    pub use_rib: Option<Identifier>,
    pub assignments: Vec<(Identifier, ArgExpr)>,
}

impl DefineBody {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (use_rib, assignments)) = tuple((
            opt(delimited(
                opt_ws(tag("use")),
                opt_ws(Identifier::parse),
                opt_ws(char(';')),
            )),
            many0(context(
                "assigments",
                separated_pair(
                    opt_ws(Identifier::parse),
                    preceded(multispace0, char('=')),
                    terminated(opt_ws(ArgExpr::parse), opt_ws(char(';'))),
                ),
            )),
        ))(input)?;
        Ok((
            input,
            Self {
                use_rib,
                assignments,
            },
        ))
    }
}

//------------ Term ---------------------------------------------------------

#[derive(Clone, Debug)]
pub struct Term {
    pub ident: Identifier,
    pub body: TermBody,
}

impl Term {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (_, ident, body)) = context(
            "term definition",
            tuple((
                opt_ws(tag("term")),
                context(
                    "term name",
                    delimited(multispace1, Identifier::parse, multispace1),
                ),
                context(
                    "term block",
                    delimited(
                        opt_ws(char('{')),
                        TermBody::parse,
                        opt_ws(char('}')),
                    ),
                ),
            )),
        )(input)?;

        Ok((input, Term { ident, body }))
    }
}

//------------ TermBody -----------------------------------------------------

// TermBody ::=
//  ('with' VariableIdentifier ';')? ('some' | 'match')
//  (MatchExpr ';')+

#[derive(Clone, Debug)]
pub struct TermBody {
    pub with: Option<Identifier>,
    pub operator: MatchOperator,
    pub match_exprs: Vec<CallExpr>,
}

impl TermBody {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (with, (operator, match_exprs), _)) = context(
            "term body",
            tuple((
                opt(opt_ws(context(
                    "with",
                    preceded(
                        opt_ws(tag("with")),
                        delimited(
                            multispace1,
                            Identifier::parse,
                            opt_ws(char(';')),
                        ),
                    ),
                ))),
                tuple((
                    opt_ws(MatchOperator::parse),
                    delimited(
                        opt_ws(char('{')),
                        many1(opt_ws(terminated(
                            CallExpr::parse,
                            opt_ws(char(';')),
                        ))),
                        opt_ws(char('}')),
                    ),
                )),
                map(char('\n'), |_| ()),
            )),
        )(input)?;
        Ok((
            input,
            Self {
                with,
                operator,
                match_exprs,
            },
        ))
    }
}

//------------ Action -------------------------------------------------------

#[derive(Clone, Debug)]
pub struct Action {
    pub ident: Identifier,
    pub for_kv: Option<TypeIdentField>,
    pub with_kv: Vec<TypeIdentField>,
    pub body: ActionBody,
}

impl Action {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (_, ident, for_kv, with_kv, body)) = context(
            "action definition",
            tuple((
                tag("action"),
                context(
                    "action name",
                    delimited(multispace1, Identifier::parse, multispace1),
                ),
                for_statement,
                with_statement,
                context(
                    "action block",
                    delimited(
                        opt_ws(char('{')),
                        ActionBody::parse,
                        tuple((opt(char(',')), opt_ws(char('}')))),
                    ),
                ),
            )),
        )(input)?;

        Ok((
            input,
            Action {
                ident,
                body,
                for_kv,
                with_kv: with_kv.unwrap_or_default(),
            },
        ))
    }
}

//------------ ActionBody -----------------------------------------------------

// ActionBody ::= (ActionExpr ';')+

#[derive(Clone, Debug)]
pub struct ActionBody {}

impl ActionBody {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        unimplemented!()
    }
}

//------------ ImportBody -----------------------------------------------------

// #[derive(Clone, Debug)]
// pub struct ImportBody {}

//------------ Apply ---------------------------------------------------------

// Apply ::= 'apply' '{' ApplyBody '}'

#[derive(Clone, Debug)]
pub struct Apply {
    pub body: ApplyBody,
    pub for_kv: Option<TypeIdentField>,
    pub with_kv: Vec<TypeIdentField>,
}

//------------ ApplyBody -----------------------------------------------------

// ApplyBody ::=
//     (
//        'filter' FilterIdentifier? MatchOperator?
//            TermIdentifier('(' VariableIdentifier ')')?
//        (
//            ('matching' '{' MatchBody '}') |
//            ('not matching' '{' MatchBody '}')
//        )+
//     )
//     AcceptReject

#[derive(Clone, Debug)]
pub struct ApplyBody {}

impl ApplyBody {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        unimplemented!()
    }
}

//------------ Rib -----------------------------------------------------------

#[derive(Clone, Debug)]
pub struct Rib {
    pub ident: Identifier,
    pub body: RibBody,
}

impl Rib {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (_, ident, body, _)) = context(
            "rib definition",
            tuple((
                opt_ws(tag("rib")),
                context(
                    "rib name",
                    cut(delimited(
                        multispace1,
                        Identifier::parse,
                        multispace1,
                    )),
                ),
                context(
                    "rib block",
                    cut(delimited(
                        opt_ws(char('{')),
                        RibBody::parse,
                        opt_ws(char('}')),
                    )),
                ),
                map(char('\n'), |_| ()),
            )),
        )(input)?;

        Ok((input, Rib { ident, body }))
    }
}

#[derive(Clone, Debug)]
pub struct RibBody {
    pub key_values: Vec<TypeIdentField>,
}

//------------ RibBody -------------------------------------------------------

//
// The body of a Rib consists of an (optional) enumeration of
// (field_name, type) pairs.

// RibBody  ::= (VariableIdentifier ':' TypeIdentifier ',')+

impl RibBody {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, key_values) = cut(separated_list0(
            char(','),
            opt_ws(TypeIdentField::parse),
        ))(input)?;

        Ok((input, RibBody { key_values }))
    }
}

//============ Separators ====================================================

//
// This is everything that doesn’t end up as actual output.

/// Parses something preceded by mandatory white space.

fn ws<'a, O, F>(
    parse: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, VerboseError<&str>>
where
    F: FnMut(&'a str) -> IResult<&str, O, VerboseError<&str>>,
{
    preceded(skip_ws, parse)
}

/// Parses something preceded by optional white space.
fn opt_ws<'a, O, F>(
    parse: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, VerboseError<&str>>
where
    F: FnMut(&'a str) -> IResult<&str, O, VerboseError<&str>>,
{
    preceded(skip_opt_ws, parse)
}

/// Mandatory white space.
///
/// White space is all actual white space characters plus comments.
fn skip_ws(input: &str) -> IResult<&str, (), VerboseError<&str>> {
    fold_many0(alt((map(multispace1, |_| ()), comment)), || (), |_, _| ())(
        input,
    )
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
/// It is a word composed of a leading alphabetic Unicode character or an
/// underscore, followed by alphanumeric Unicode characters or underscore or
/// period.
#[derive(Clone, Debug)]
pub struct Identifier {
    /// The actual identifier.
    pub ident: ShortString,
}

impl Identifier {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, ident) = context(
            "identifier",
            recognize(preceded(
                take_while1(|ch: char| ch.is_alphabetic() || ch == '_'),
                take_while(|ch: char| {
                    ch.is_alphanumeric() || ch == '_' || ch == '.'
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

/// An identifier is the name of variables or other things.
///
/// It is a word composed of a leading alphabetic Unicode character or an
/// underscore, followed by alphanumeric Unicode characters or underscore or
/// period.
#[derive(Clone, Debug)]
pub struct TypeIdentifier {
    /// The actual identifier.
    pub ident: ShortString,
}

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
                cut(preceded(multispace0, char(':'))),
                preceded(multispace0, TypeIdentifier::parse),
            ),
        )(input)?;

        Ok((input, Self { field_name, ty }))
    }
}

//------------ StringLiteral -----------------------------------------------

// Our take on a literal string is just a Identifier wrapped in two double
// quotes. We don't do any escaping or anything like that.
#[derive(Clone, Debug)]
pub struct StringLiteral(ShortString);

impl StringLiteral {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, ident) =
            delimited(char('"'), Identifier::parse, char('"'))(input)?;

        Ok((input, Self(ident.ident)))
    }
}

//------------ AcceptReject -------------------------------------------------

// Every filter needs to return either a 'accept' or 'reject' statement.
// failing to set it properly ends in the whole thing being cancelled.

// AcceptReject ::= 'return'? ( 'accept' | 'reject' ) ';'

#[derive(Clone, Debug)]
pub enum AcceptReject {
    Accept,
    Reject,
}

fn accept_reject(
    input: &str,
) -> IResult<&str, AcceptReject, VerboseError<&str>> {
    context(
        "accept or reject",
        cut(preceded(
            opt(opt_ws(tag("return"))),
            alt((
                map(tag("accept"), |_| AcceptReject::Accept),
                map(tag("reject"), |_| AcceptReject::Reject),
            )),
        )),
    )(input)
}

//------------ ArgExpr --------------------------------------------------
#[derive(Clone, Debug)]
pub enum ArgExpr {
    Identifier(Identifier),
    TypeIdentifier(TypeIdentifier),
    StringLiteral(StringLiteral),
    Bool(bool),
    CallExpr(CallExpr),
    FieldExpr(FieldExpr),
}

impl ArgExpr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        alt((
            map(Identifier::parse, ArgExpr::Identifier),
            map(TypeIdentifier::parse, ArgExpr::TypeIdentifier),
            map(StringLiteral::parse, ArgExpr::StringLiteral),
            map(tag("true"), |_| ArgExpr::Bool(true)),
            map(tag("false"), |_| ArgExpr::Bool(false)),
            map(CallExpr::parse, ArgExpr::CallExpr),
            map(FieldExpr::parse, ArgExpr::FieldExpr),
        ))(input)
    }
}

//------------ ExprList -------------------------------------------------

// ArgExprList ::=
//      ((Identifier|StringLiteral|TypeIdentifier)','?)+

#[derive(Clone, Debug)]
pub struct ArgExprList {
    pub args: Vec<ArgExpr>,
}

impl ArgExprList {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, args) = context(
            "argument expressions",
            separated_list1(preceded(multispace0, char(',')), ArgExpr::parse),
        )(input)?;
        Ok((input, Self { args }))
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

//------------- FieldExpr --------------------------------------------------

// FieldExpr ::= Identifier'.'Identifier

#[derive(Clone, Debug)]
pub struct FieldExpr {
    pub ident: Identifier,
    pub field_name: Identifier,
}

impl FieldExpr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (ident, field_name)) = tuple((
            Identifier::parse,
            preceded(char('.'), Identifier::parse),
        ))(input)?;
        Ok((input, Self { ident, field_name }))
    }
}

//------------- CallReceiver ------------------------------------------------

// CallReceiver ::= Identifier | FieldExpr

#[derive(Clone, Debug)]
pub enum CallReceiver {
    Identifier(Identifier),
    FieldExpr(FieldExpr),
}

impl CallReceiver {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        alt((
            map(Identifier::parse, |ident| CallReceiver::Identifier(ident)),
            map(FieldExpr::parse, |field| CallReceiver::FieldExpr(field)),
        ))(input)
    }
}

//------------- CallExpr ----------------------------------------------------

// CallExpr ::= CallReceiver'('ExprList?')'
#[derive(Clone, Debug)]
pub struct CallExpr {
    /// The name of the function.
    pub receiver: CallReceiver,
    /// The arguments to the function.
    pub expr_list: ArgExprList,
}

impl CallExpr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (receiver, expr_list)) = tuple((
            CallReceiver::parse,
            delimited(char('('), ArgExprList::parse, char(')')),
        ))(input)?;

        Ok((
            input,
            Self {
                receiver,
                expr_list,
            },
        ))
    }
}

//------------ MatchExpr ----------------------------------------------------

// MatchExpr ::=
//      Identifier(
//          ('.' Identifier)+("." MethodCallIdentifier)
//      ?)?
// MatchBody ::=
//      (ActionExpr ';')+ AcceptReject?

// MatchOperator ::=
//      'match' | 'some' | 'exactly-one' | 'all'

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
