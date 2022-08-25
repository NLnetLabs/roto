use std::{borrow, cmp, fmt, hash, ops, str};

use nom::branch::alt;
use nom::bytes::complete::{take_while, take_while1};
use nom::character::complete::{multispace0, multispace1};
use nom::combinator::{all_consuming, cut, opt, recognize};
use nom::error::{
    context, convert_error, ContextError, ParseError, VerboseError, VerboseErrorKind,
};
use nom::multi::{fold_many0, many0, many1, separated_list0};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::{
    bytes::complete::{tag, take_until},
    character::complete::char as tag_char,
    combinator::map,
    IResult,
};
use nom::{ErrorConvert, Finish};
use nom_locate::LocatedSpan;
use smallvec::SmallVec;

use nom::error::{Error as NomError, ErrorKind};

type Span<'a> = nom_locate::LocatedSpan<&'a str>;

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
    pub pos: Pos,
}

impl Root {
    pub fn parse_str<'a>(input: &'a str) -> Result<(Span, Self), VerboseError<Span<'a>>> {
        Self::parse_root(Span::new(input)).finish()
    }

    fn parse_root(input: Span) -> IResult<Span, Self, VerboseError<Span<'_>>> {
        let pos = Pos::capture(&input);
        let (input, (_, expressions, _)) = context(
            "root",
            tuple((skip_opt_ws, many1(RootExpr::parse), skip_opt_ws)),
        )(input)?;
        Ok((input, Self { expressions, pos }))
    }
}

#[derive(Debug, Clone)]
pub enum RootExpr {
    Module(Module),
    Rib(Rib),
    // PrefixList(PrefixListExpr),
    // Table(TableExpr),
    // Comment(CommentExpr),
}

impl RootExpr {
    pub fn parse(input: Span) -> IResult<Span, Self, VerboseError<Span<'_>>> {
        let (input, expressions) = context(
            "root expression",
            alt((
                map(Module::parse, RootExpr::Module),
                map(Rib::parse, RootExpr::Rib),
            )),
        )(input)?;
        Ok((input, expressions))
    }
}

//------------ Module --------------------------------------------------------

#[derive(Clone, Debug)]
pub struct Module {
    pub ident: Identifier,
    pub pos: Pos, // pub for_statement: Option<ForStatement>,
                  // pub with_statements: Vec<WithStatement>,
                  // pub body: ModuleBody
}

impl Module {
    pub fn parse(input: Span) -> IResult<Span, Self, VerboseError<Span<'_>>> {
        let pos = Pos::capture(&input);
        let (input, _) = opt_ws(tag("module"))(input)?;
        let (input, ident) = opt_ws(Identifier::parse)(input)?;
        // let (input, body) = opt_ws(ModuleBody::parse)(input)?;

        Ok((input, Module { ident, pos }))
    }
}

//------------ Rib -----------------------------------------------------------

#[derive(Clone, Debug)]
pub struct Rib {
    pub ident: Identifier,
    pub body: RibBody,
    pub pos: Pos,
}

impl Rib {
    pub fn parse(input: Span) -> IResult<Span, Self, VerboseError<Span<'_>>> {
        let pos = Pos::capture(&input);

        let (input, (_, ident, body)) = context(
            "rib definition",
            tuple((
                tag("rib"),
                context(
                    "rib name",
                    cut(delimited(multispace1, Identifier::parse, multispace1)),
                ),
                context(
                    "rib block",
                    delimited(
                        opt_ws(tag("{")),
                        RibBody::parse,
                        tuple((opt(tag(",")), multispace0, opt_ws(tag("}")))),
                    ),
                ),
            )),
        )(input)?;

        Ok((input, Rib { ident, body, pos }))
    }
}

#[derive(Clone, Debug)]
pub struct RibBody {
    pub key_values: Vec<TypeIdentField>,
    pub pos: Pos,
}

//------------ RibBody -------------------------------------------------------

//
// The body of a Rib consists of an (optional) enumeration of
// (field_name, type) pairs.

// RibBody  ::= VariableIdentifier ':' TypeIdentifier ','?

impl RibBody {
    pub fn parse(input: Span) -> IResult<Span, Self, VerboseError<Span<'_>>> {
        let pos = Pos::capture(&input);

        let (input, key_values) = context(
            "rib body",
            opt(separated_list0(tag(","), opt_ws(TypeIdentField::parse))),
        )(input)?;

        Ok((
            input,
            RibBody {
                key_values: key_values.unwrap_or_default(),
                pos,
            },
        ))
    }
}

//============ Separators ====================================================

//
// This is everything that doesn’t end up as actual output.

/// Parses something preceded by mandatory white space.

fn ws<'a, O, F>(parse: F) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, O, VerboseError<Span<'_>>>
where
    F: FnMut(Span<'a>) -> IResult<Span, O, VerboseError<Span<'_>>>,
{
    preceded(skip_ws, parse)
}

/// Parses something preceded by optional white space.
fn opt_ws<'a, O, F>(
    parse: F,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, O, VerboseError<Span<'_>>>
where
    F: FnMut(Span<'a>) -> IResult<Span, O, VerboseError<Span<'_>>>,
{
    preceded(skip_opt_ws, parse)
}

/// Mandatory white space.
///
/// White space is all actual white space characters plus comments.
fn skip_ws(input: Span) -> IResult<Span, (), VerboseError<Span<'_>>> {
    fold_many0(alt((map(multispace1, |_| ()), comment)), || (), |_, _| ())(input)
}

/// Optional white space.
///
/// White space is all actual white space characters plus comments.
fn skip_opt_ws(input: Span) -> IResult<Span, (), VerboseError<Span<'_>>> {
    fold_many0(alt((map(multispace1, |_| ()), comment)), || (), |_, _| ())(input)
}

/// Comments start with a hash and run to the end of a line.
fn comment(input: Span) -> IResult<Span, (), VerboseError<Span<'_>>> {
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

    /// The start of the identifier in the source.
    pub pos: Pos,
}

impl Identifier {
    fn parse(input: Span) -> IResult<Span, Self, VerboseError<Span<'_>>> {
        let pos = Pos::capture(&input);

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
                ident: (*ident.fragment()).into(),
                pos,
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

//------------ Identifier ----------------------------------------------------

/// An identifier is the name of variables or other things.
///
/// It is a word composed of a leading alphabetic Unicode character or an
/// underscore, followed by alphanumeric Unicode characters or underscore or
/// period.
#[derive(Clone, Debug)]
pub struct TypeIdentifier {
    /// The actual identifier.
    pub ident: ShortString,

    /// The start of the identifier in the source.
    pub pos: Pos,
}

impl TypeIdentifier {
    fn parse(input: Span) -> IResult<Span, Self, VerboseError<Span<'_>>> {
        let pos = Pos::capture(&input);

        let (input, ident) = recognize(pair(
            take_while1(|ch: char| ch.is_alphabetic() && ch.is_uppercase()),
            take_while(|ch: char| ch.is_alphanumeric()),
        ))(input)?;
        Ok((
            input,
            TypeIdentifier {
                ident: (*ident.fragment()).into(),
                pos,
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
    /// The start of the field in the source.
    pub pos: Pos,
}

impl TypeIdentField {
    pub fn parse(input: Span) -> IResult<Span, Self, VerboseError<Span<'_>>> {
        let pos = Pos::capture(&input);

        let (input, (field_name, ty)) = context(
            "type",
            tuple((
                opt_ws(Identifier::parse),
                preceded(opt_ws(tag(":")), opt_ws(TypeIdentifier::parse)),
            )),
        )(input)?;

        Ok((
            input,
            Self {
                field_name,
                ty,
                pos,
            },
        ))
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

//------------ Pos -----------------------------------------------------------

/// The position of an item within input.
#[derive(Clone, Copy, Debug, Default)]
pub struct Pos {
    pub offset: usize,
    pub line: u32,
    pub col: usize,
}

impl Pos {
    fn capture(span: &Span) -> Self {
        Pos {
            offset: span.location_offset(),
            line: span.location_line(),
            col: span.get_utf8_column(),
        }
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

//============ Error ====================================================

#[derive(Clone, Debug)]
pub struct Error {
    pub pos: Pos,
    pub kind: VerboseErrorKind,
    pub fragment: String,
}

impl<'a> From<VerboseError<Span<'a>>> for Error {
    fn from(err: VerboseError<Span>) -> Self {
        let last_error = err.errors.last().unwrap();
        Error {
            pos: Pos::capture(&last_error.0),
            kind: last_error.1.clone(),
            fragment: last_error.0.fragment().to_string(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {:?}", self.pos, self.kind)
    }
}
