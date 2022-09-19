use std::{borrow, cmp, fmt, hash, ops, str};

use nom::branch::{alt, permutation};
use nom::bytes::complete::{is_not, take, take_while, take_while1};
use nom::character::complete::{char, digit1, multispace0, multispace1};
use nom::combinator::{all_consuming, cut, map_res, opt, recognize};
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
    // Table(TableExpr),
}

impl RootExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, expressions) = context(
            "root",
            alt((
                map(Rib::parse, Self::Rib),
                map(Module::parse, |m| Self::Module(Box::new(m))),
            )),
        )(input)?;
        Ok((input, expressions))
    }

    pub fn get_module(&self) -> Result<&Module, &str> {
        match self {
            Self::Module(m) => Ok(m),
            _ => Err("not a module"),
        }
    }
}

//------------ Module --------------------------------------------------------

// Module ::= "module" Identifier "for" Identifier WithStatement  '{' ModuleBody '}'

#[derive(Clone, Debug)]
pub struct Module {
    pub ident: Identifier,
    pub for_ident: Option<Identifier>,
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
                opt(preceded(opt_ws(tag("for")), opt_ws(Identifier::parse))),
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
    pub ident: Option<Identifier>,
    pub for_kv: Option<TypeIdentField>,
    pub with_kv: Vec<TypeIdentField>,
    pub body: DefineBody,
}

impl Define {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (ident, for_kv, with_kv, body, _)) = context(
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
        )(input)?;

        Ok((
            input,
            Self {
                ident,
                for_kv,
                with_kv: with_kv.unwrap_or_default(),
                body,
            },
        ))
    }
}

//------------ DefineBody ---------------------------------------------------

// DefineBody ::=
//     (( 'use' Identifier ';' )? ( Identifier '=' CallExpr ';' )+ )+

#[derive(Clone, Debug)]
pub struct DefineBody {
    pub use_rib: Option<Identifier>,
    pub assignments: Vec<(Identifier, CallExpr)>,
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
                    terminated(opt_ws(CallExpr::parse), opt_ws(char(';'))),
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
//     ( MatchOperator '{' (CallExpr ';')+ '}' )+

#[derive(Clone, Debug)]
pub struct TermScope {
    pub scope: Option<Identifier>,
    pub operator: MatchOperator,
    pub match_exprs: Vec<MatchExpr>,
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
                                opt_ws(MatchExpr::parse),
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

// 'action' Identifier '{' ActionBody '}')* ForStatement WithStatement '{' ActionBody '}'

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
                opt_ws(tag("action")),
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
                        opt_ws(char('}')),
                    ),
                ),
            )),
        )(input)?;

        Ok((
            input,
            Action {
                ident,
                for_kv,
                with_kv: with_kv.unwrap_or_default(),
                body,
            },
        ))
    }
}

//------------ ActionBody -----------------------------------------------------

// ActionBody ::= (CallExpr ';')+

#[derive(Clone, Debug)]
pub struct ActionBody {
    pub expressions: Vec<CallExpr>,
}

impl ActionBody {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, expressions) = context(
            "action body",
            many1(opt_ws(terminated(CallExpr::parse, opt_ws(char(';'))))),
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
//      'filter' MatchOperator ( CallExpr | Identifier )
//      'not'? 'matching'
//      '{' ( ( CallExpr | Identifier ) ';' ( AcceptReject ';' )? )+ '}' ';'

#[derive(Clone, Debug)]
pub struct ApplyScope {
    pub scope: Identifier,
    pub operator: MatchOperator,
    pub filter_ident: ArgExpr,
    pub negate: bool,
    pub actions: Vec<(ArgExpr, Option<AcceptReject>)>,
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
                        ArgExpr::parse,
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
                                        ArgExpr::parse,
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
                    opt_ws(tag("rib")),
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
    RecordField(Box<(Identifier, RibBody)>),
}

// impl RibField {
//     pub fn get_field(&self) -> &TypeIdentField {
//         match self {
//             RibField::PrimitiveField(field) => field,
//             RibField::RecordField(body) => body.1.get_field(),
//         }
//     }
// }

//------------ RibBody -------------------------------------------------------

//
// The body of a Rib consists of an (optional) enumeration of
// (field_name, type) pairs.

// RibBody ::= ( Identifier ':' ( TypeIdentifier | RibBody ) ','?)+

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
                                RibBody::parse,
                                opt_ws(char('}')),
                            ),
                        )),
                        |r| RibField::RecordField(Box::new(r)),
                    ),
                )))),
            ),
        )(input)?;
        Ok((input, RibBody { key_values }))
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

//------------ IntegerLiteral -----------------------------------------------

/// An integer literal is a sequence of digits.
/// IntegerLiteral ::= [0-9]+
///
/// We parse it as a string and then convert it to an integer.
#[derive(Clone, Debug)]
pub struct IntegerLiteral(pub u64);

impl IntegerLiteral {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, digits) = context(
            "integer literal",
            recognize(pair(
                opt(char('-')),
                take_while1(|ch: char| ch.is_digit(10)),
            )),
        )(input)?;

        let value = digits.parse().unwrap();
        Ok((input, Self(value)))
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
                take_while1(|ch: char| ch.is_digit(10)),
                char('.'),
                take_while1(|ch: char| ch.is_digit(10)),
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

//------------ ArgExpr --------------------------------------------------

// ArgExpr ::= Identifier |
//  TypeIdentifier |
//  StringLiteral |
//  Bool |
//  CallExpr |
//  FieldExpr

#[derive(Clone, Debug)]
pub enum ArgExpr {
    Identifier(Identifier),
    TypeIdentifier(TypeIdentifier),
    StringLiteral(StringLiteral),
    Bool(bool),
    CallExpr(CallExpr),
    FieldExpr(FieldExpr),
    PrefixMatchExpr(PrefixMatchExpr),
}

impl ArgExpr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        alt((
            map(CallExpr::parse, ArgExpr::CallExpr),
            map(FieldExpr::parse, ArgExpr::FieldExpr),
            map(TypeIdentifier::parse, ArgExpr::TypeIdentifier),
            map(Identifier::parse, ArgExpr::Identifier),
            map(StringLiteral::parse, ArgExpr::StringLiteral),
            map(tag("true"), |_| ArgExpr::Bool(true)),
            map(tag("false"), |_| ArgExpr::Bool(false)),
            map(PrefixMatchExpr::parse, ArgExpr::PrefixMatchExpr),
        ))(input)
    }
}

//------------ ArgExprList -------------------------------------------------

// ArgExprList ::= ( ArgExpr ','? )*

#[derive(Clone, Debug)]
pub struct ArgExprList {
    pub args: Vec<ArgExpr>,
}

impl ArgExprList {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, args) = context(
            "argument expressions",
            separated_list0(preceded(multispace0, char(',')), ArgExpr::parse),
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

// FieldExpr ::= Identifier ( '.' Identifier )+

#[derive(Clone, Debug)]
pub struct FieldExpr {
    pub ident: Identifier,
    pub field_names: Vec<Identifier>,
}

impl FieldExpr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (ident, field_names)) = context(
            "field expression",
            tuple((
                Identifier::parse,
                many1(preceded(char('.'), Identifier::parse)),
            )),
        )(input)?;
        Ok((input, Self { ident, field_names }))
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
            map(FieldExpr::parse, CallReceiver::FieldExpr),
            map(Identifier::parse, CallReceiver::Identifier),
        ))(input)
    }
}

//------------- CallExpr ----------------------------------------------------

// CallExpr ::= CallReceiver'('ArgExprList?')'

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

// MatchExpr ::= ArgExpr | GroupedExpr | CompareExpr | AndExpr | OrExpr | SetCompareExpr

#[derive(Clone, Debug)]
pub enum MatchExpr {
    OrExpr(OrExpr),
    CompareExpr(CompareExpr),
    AndExpr(AndExpr),
    SetCompareExpr(SetCompareExpr),
    PrefixMatchExpr(PrefixMatchExpr),
    GroupedMatchExpr(GroupedMatchExpr),
    ArgExpr(ArgExpr),
}

impl MatchExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        opt_ws(alt((
            map(CompareExpr::parse, MatchExpr::CompareExpr),
            map(AndExpr::parse, MatchExpr::AndExpr),
            map(OrExpr::parse, MatchExpr::OrExpr),
            map(SetCompareExpr::parse, MatchExpr::SetCompareExpr),
            map(PrefixMatchExpr::parse, MatchExpr::PrefixMatchExpr),
            map(GroupedMatchExpr::parse, MatchExpr::GroupedMatchExpr),
            map(ArgExpr::parse, MatchExpr::ArgExpr),
        )))(input)
    }
}

#[derive(Clone, Debug)]
pub enum NestedMatchExpr {
    NArgExpr(ArgExpr),
    NGroupedMatchExpr(GroupedMatchExpr),
}

impl NestedMatchExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        alt((
            map(GroupedMatchExpr::parse, NestedMatchExpr::NGroupedMatchExpr),
            map(ArgExpr::parse, NestedMatchExpr::NArgExpr),
        ))(input)
    }
}

//------------ CompareExpr --------------------------------------------------

// CompareExpr ::= ArgExpr CompareOp ArgExpr

#[derive(Clone, Debug)]
pub struct CompareExpr {
    pub left: NestedMatchExpr,
    pub op: CompareOp,
    pub right: NestedMatchExpr,
}

impl CompareExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (left, op, right)) = context(
            "Compare Expression",
            tuple((
                opt_ws(NestedMatchExpr::parse),
                opt_ws(alt((
                    map(tag("=="), |_| CompareOp::Eq),
                    map(tag("!="), |_| CompareOp::Ne),
                    map(tag("<"), |_| CompareOp::Lt),
                    map(tag("<="), |_| CompareOp::Le),
                    map(char('>'), |_| CompareOp::Gt),
                    map(tag(">="), |_| CompareOp::Ge),
                ))),
                opt_ws(NestedMatchExpr::parse),
            )),
        )(input)?;

        Ok((input, Self { left, op, right }))
    }
}

//------------ CompareOp ----------------------------------------------------

// CompareOp ::= '==' | '!=' | '<' | '<=' | '>' | '>='

#[derive(Clone, Debug)]
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
    pub left: NestedMatchExpr,
    pub right: NestedMatchExpr,
}

impl AndExpr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (left, right)) = tuple((
            opt_ws(NestedMatchExpr::parse),
            preceded(opt_ws(tag("&&")), opt_ws(NestedMatchExpr::parse)),
        ))(input)?;

        Ok((input, Self { left, right }))
    }
}

//------------ OrExpr -------------------------------------------------------

// OrExpr ::= MatchExpr '||' MatchExpr

#[derive(Clone, Debug)]
pub struct OrExpr {
    pub left: NestedMatchExpr,
    pub right: NestedMatchExpr,
}

impl OrExpr {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (left, right)) = context(
            "or expr",
            tuple((
                opt_ws(NestedMatchExpr::parse),
                preceded(opt_ws(tag("||")), opt_ws(NestedMatchExpr::parse)),
            )),
        )(input)?;

        Ok((input, Self { left, right }))
    }
}

//------------ SetCompareExpr -----------------------------------------------

// SetCompareExpr ::= ( ArgExpr ( 'in' | 'not in' ) ArgExpr )+

#[derive(Clone, Debug)]
pub struct SetCompareExpr {
    pub left: NestedMatchExpr,
    pub op: SetCompareOp,
    pub right: NestedMatchExpr,
}

impl SetCompareExpr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, (left, op, right)) = tuple((
            opt_ws(NestedMatchExpr::parse),
            alt((
                map(opt_ws(tag("in")), |_| SetCompareOp::In),
                map(opt_ws(tag("not in")), |_| SetCompareOp::NotIn),
            )),
            opt_ws(NestedMatchExpr::parse),
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

//------------- GroupedMatchExpr -------------------------------------------------

// GroupedMatchExpr ::= '(' MatchExpr ')'

#[derive(Clone, Debug)]
pub struct GroupedMatchExpr {
    pub expr: Box<MatchExpr>,
}

impl GroupedMatchExpr {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, expr) = delimited(
            opt_ws(char('(')),
            MatchExpr::parse,
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
