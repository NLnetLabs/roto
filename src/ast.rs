use std::{borrow, cmp, fmt, hash, ops, str};

use nom::branch::alt;
use nom::bytes::complete::{take_while, take_while1};
use nom::character::complete::{char, multispace0, multispace1, alphanumeric1};
use nom::combinator::{all_consuming, cut, opt, recognize};
use nom::error::{
    context, convert_error, ContextError, ParseError, VerboseError, VerboseErrorKind,
};
use nom::multi::{fold_many0, many0, many1, separated_list0, separated_list1};
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated, tuple};
use nom::{
    bytes::complete::{tag, take_until},
    character::complete::char as tag_char,
    combinator::map,
    IResult,
};
use nom::{ErrorConvert, Finish};
use smallvec::SmallVec;

use nom::error::{Error as NomError, ErrorKind};

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
    pub fn parse_str(input: &str) -> Result<(&str, Self), VerboseError<&str>> {
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
            map(Rib::parse, RootExpr::Rib),
            map(Module::parse, RootExpr::Module),
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
        let (input, (_, ident, for_kv, with_kv, body)) = context(
            "module definition",
            tuple((
                tag("module"),
                context(
                    "module name",
                    delimited(multispace1, Identifier::parse, multispace1),
                ),
                for_statement,
                with_statement,
                context(
                    "module block",
                    delimited(
                        opt_ws(char('{')),
                        ModuleBody::parse,
                        tuple((opt(char(',')), opt_ws(char('}')))),
                    ),
                ),
            )),
        )(input)?;

        Ok((
            input,
            Module {
                ident,
                body,
                for_kv,
                with_kv,
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
pub enum ModuleBody {
    Define(DefineBody),
    Term(TermBody),
    Action(ActionBody),
    // Import(ImportBody),
    Apply(ApplyBody),
}

impl ModuleBody {
    pub fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, ((ident, for_kv, with_kv, define), expressions)) = tuple((
            context(
                "define definition",
                preceded(
                    opt_ws(tag("define")),
                    tuple((
                        context(
                            "module name",
                            delimited(multispace1, Identifier::parse, multispace1),
                        ),
                        opt(for_statement),
                        opt(with_statement),
                        context(
                            "define block",
                            delimited(opt_ws(char('{')), DefineBody::parse, opt_ws(char('}'))),
                        ),
                    )),
                ),
            ),
            many0(alt((
                map(TermBody::parse, ModuleBody::Term),
                map(ActionBody::parse, ModuleBody::Action),
                // map(ImportBody::parse, ModuleBody::Import),
                map(ApplyBody::parse, ModuleBody::Apply),
            ))),
        ))(input)?;
        Ok((input, expressions.into_iter().next().unwrap()))
    }
}
//------------ DefineBody ---------------------------------------------------

// DefineBody ::=
//  ('use' RibIdentifier ';')? ( VariableIdentifier '=' TypeIdentifier ';')+

#[derive(Clone, Debug)]
pub struct DefineBody {}

impl DefineBody {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        let (input, _) = tuple((
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
                    opt_ws(Identifier::parse),
                ),
            )),
        ))(input)?;
        Ok((input, Self {}))
    }
}

//------------ TermBody -----------------------------------------------------

// TermBody ::=
//  ('with' VariableIdentifier ';')? ('some' | 'match')
//     '{' (MatchExpr ';')+ '}' ';'

#[derive(Clone, Debug)]
pub struct TermBody {}

impl TermBody {
    fn parse(input: &str) -> IResult<&str, Self, VerboseError<&str>> {
        unimplemented!()
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
                tag("rib"),
                context(
                    "rib name",
                    cut(delimited(multispace1, Identifier::parse, multispace1)),
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

        let (input, key_values) =
            cut(separated_list0(char(','), opt_ws(TypeIdentField::parse)))(input)?;

        Ok((
            input,
            RibBody {
                key_values,
            },
        ))
    }
}

//============ Separators ====================================================

//
// This is everything that doesn’t end up as actual output.

/// Parses something preceded by mandatory white space.

fn ws<'a, O, F>(parse: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, VerboseError<&str>>
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
    fold_many0(alt((map(multispace1, |_| ()), comment)), || (), |_, _| ())(input)
}

/// Optional white space.
///
/// White space is all actual white space characters plus comments.
fn skip_opt_ws(input: &str) -> IResult<&str, (), VerboseError<&str>> {
    fold_many0(alt((map(multispace1, |_| ()), comment)), || (), |_, _| ())(input)
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
                take_while(|ch: char| ch.is_alphanumeric() || ch == '_' || ch == '.'),
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

    // fn key_value<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (&'a str, JsonValue), E> {
    //     separated_pair(preceded(sp, string), cut(preceded(sp, char(':'))), value)(i)
    //     }
}

//------------ ArgumentList -------------------------------------------------

// ArgumentList ::= 
//      ((VariableIdentifier|StringLiteral|EnumVariantIdentifier)','?)+

//------------ for & with statements ----------------------------------------

// ForStatement ::= 'for' VariableIdentifier':' TypeIdentifier

fn for_statement(input: &str) -> IResult<&str, Option<TypeIdentField>, VerboseError<&str>> {
    opt(preceded(opt_ws(tag("for")), TypeIdentField::parse))(input)
}

// WithStatement ::= 'with' VariableIdentifier':' TypeIdentifier

fn with_statement(input: &str) -> IResult<&str, Vec<TypeIdentField>, VerboseError<&str>> {
    preceded(
        opt_ws(tag("with")),
        separated_list0(char(','), TypeIdentField::parse),
    )(input)
}

// ------------ MethodCall --------------------------------------------------
// MethodCall ::= 
//      VariableIdentifier(('.'FieldIdentifier)+('.'MethodCallIdentifier)?)?
// MethodCallIdentifier ::= 
//      ([a-z] ([0-9a-z_])*)'('ArgumentList?')'

//------------ MatchExpr -----------------------------------------------------
// MatchExpr ::= 
//      VariableIdentifier(
//          ('.'VariableIdentifier)+("." MethodCallIdentifier)
//      ?)?
// MatchBody ::= 
//      (ActionExpr ';')+ AcceptReject?
// MatchOperator ::= 
//      'match' | 'some' | 'exactly-one' | 'all


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
