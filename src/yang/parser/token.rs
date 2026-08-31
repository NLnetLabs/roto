//! Lexer for Roto scripts

use core::{ops::Range, str};
use std::{fmt::Display, ops::ControlFlow};

use icu::properties::sets::CodePointSetDataBorrowed;

use crate::{
    allowed_subs2,
    yang::parser::ast::AxisName,
    yang::parser::expr::{ArgConstraints, Cardinality},
};

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'s> {
    Ident(&'s str),

    // === Punctuation ===
    AmpAmp,
    AngleLeftEq,
    AngleRightEq,
    Arrow,
    At,
    Bang,
    BangEq,
    // Colon,
    Comma,
    DoubleSlash,
    DoubleColon,
    DoubleDot,
    Eq,
    EqEq,
    // Hyphen,
    // Period,
    Pipe,
    PipePipe,
    Plus,
    QuestionMark,
    SemiColon,
    Slash,
    Star,

    // === Delimiters ===
    AngleLeft,
    AngleRight,
    CurlyLeft,
    CurlyRight,
    RoundLeft,
    RoundRight,
    SquareLeft,
    SquareRight,

    // === Keywords ===
    Keyword(Keyword),

    // XPath Axis names are of the form <AN>::Ident,
    // the <AN> is hardcoded
    AxisName(AxisName),
    // === Literals ===
    QuotedString(&'s str),
    UnquotedString(&'s str),
    Integer(&'s str),
    Float(&'s str),
    Hex(&'s str),
    Asn(&'s str),
    IpV4(&'s str),
    IpV6(&'s str),
    Bool(bool),

    Test,
}

const XID_START: CodePointSetDataBorrowed<'static> =
    icu::properties::sets::xid_start();
const XID_CONTINUE: CodePointSetDataBorrowed<'static> =
    icu::properties::sets::xid_continue();

pub struct Lexer<'a> {
    input: &'a str,
    original_length: usize,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Result<Token<'a>, ()>, Range<usize>);

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            ControlFlow::Continue(()) => {
                if self.input.is_empty() {
                    None
                } else {
                    let start = self.original_length - self.input.len();
                    let end = start + 1;
                    Some((Err(()), start..end))
                }
            }
            ControlFlow::Break((tok, span)) => Some((Ok(tok), span)),
        }
    }
}

impl<'s> Lexer<'s> {
    pub fn new(input: &'s str) -> Self {
        Self {
            input,
            original_length: input.len(),
        }
    }

    fn bump(&mut self, n: usize) -> (&'s str, Range<usize>) {
        let start = self.original_length - self.input.len();
        let (a, b) = self.input.split_at(n);
        self.input = b;
        let end = self.original_length - self.input.len();
        (a, start..end)
    }

    fn is_empty(&self) -> bool {
        self.input.is_empty()
    }

    fn next_token(&mut self) -> ControlFlow<(Token<'s>, Range<usize>)> {
        self.skip_comments();

        if self.is_empty() {
            return ControlFlow::Continue(());
        }

        self.xpath_axis_name()?;
        // self.ipv6()?;
        // self.ipv4()?;
        // self.two_char_punctuation()?;
        self.one_char_punctuation()?;
        // self.as_number()?;
        // self.hex_number()?;
        // self.float()?;
        // self.integer()?;
        self.double_quoted_string()?;
        self.single_quoted_string()?;
        self.unquoted_string_or_identifier_ref()?;
        // self.unquoted_string()?;

        ControlFlow::Continue(())
    }

    fn skip_comments(&mut self) {
        loop {
            self.input = self.input.trim_start();
            if self.input.as_bytes().first_chunk::<2>() == Some(b"//") {
                let n = self.input.find('\n').unwrap_or(self.input.len());
                self.bump(n);
            } else if self.input.as_bytes().first_chunk::<2>() == Some(b"/*")
            {
                let n = self.input.find("*/").unwrap_or(self.input.len());
                self.bump(n + 2);
            } else {
                return;
            }
        }
    }

    fn two_char_punctuation(
        &mut self,
    ) -> ControlFlow<(Token<'s>, Range<usize>)> {
        let Some(x) = self.input.as_bytes().first_chunk() else {
            return ControlFlow::Continue(());
        };

        let tok = match *x {
            [b'=', b'='] => Token::EqEq,
            [b'!', b'='] => Token::BangEq,
            [b'&', b'&'] => Token::AmpAmp,
            [b'|', b'|'] => Token::PipePipe,
            [b'>', b'='] => Token::AngleRightEq,
            [b'<', b'='] => Token::AngleLeftEq,
            [b'-', b'>'] => Token::Arrow,
            [b':', b':'] => Token::DoubleColon,
            [b'/', b'/'] => Token::DoubleSlash,
            [b'.', b'.'] => Token::DoubleDot,
            _ => return ControlFlow::Continue(()),
        };

        let (_, span) = self.bump(2);

        ControlFlow::Break((tok, span))
    }

    fn one_char_punctuation(
        &mut self,
    ) -> ControlFlow<(Token<'s>, Range<usize>)> {
        let Some(x) = self.input.as_bytes().first() else {
            return ControlFlow::Continue(());
        };

        let tok = match x {
            // b'=' => Token::Eq,
            // b'|' => Token::Pipe,
            // b'-' => Token::Hyphen,
            // b':' => Token::Colon,
            b';' => Token::SemiColon,
            // b',' => Token::Comma,
            // b'.' => Token::Period,
            // b'+' => Token::Plus,
            // b'*' => Token::Star,
            b'/' => Token::Slash,
            // b'!' => Token::Bang,
            b'{' => Token::CurlyLeft,
            b'}' => Token::CurlyRight,
            // b'?' => Token::QuestionMark,
            // b'[' => Token::SquareLeft,
            // b']' => Token::SquareRight,
            // b'(' => Token::RoundLeft,
            // b')' => Token::RoundRight,
            // b'<' => Token::AngleLeft,
            // b'>' => Token::AngleRight,
            // b'@' => Token::At,
            _ => return ControlFlow::Continue(()),
        };

        let (_, span) = self.bump(1);

        ControlFlow::Break((tok, span))
    }

    fn xpath_axis_name(&mut self) -> ControlFlow<(Token<'s>, Range<usize>)> {
        // println!("xpath_axis_name {}", s);
        let an = self.input.split("::").collect::<Vec<_>>();
        let end = an.first().map(|s| s.len()).unwrap_or(self.input.len());

        // An axis name must have exactly one '::'
        // let left = self.input.split("::");
        // if left.count() != 2 {
        //     return ControlFlow::Continue(());
        // }

        let an = if let Some(a) = an.first() {
            match *a {
                "ancestor" => AxisName::Ancestor,
                "ancestor-or-self" => AxisName::AncestorOrSelf,
                "attribute" => AxisName::Attribute,
                "child" => AxisName::Child,
                "descendant" => AxisName::Descendant,
                "descendant-or-self" => AxisName::Descendant,
                "following" => AxisName::Following,
                "following-or-sibling" => AxisName::FollowingSibling,
                "namespace" => AxisName::Namespace,
                "parent" => AxisName::Parent,
                "preceding" => AxisName::Preceding,
                "preceding-sibling" => AxisName::PrecedingSibling,
                "self" => AxisName::Zelf,
                _t => {
                    return ControlFlow::Continue(());
                }
            }
        } else {
            return ControlFlow::Continue(());
        };

        println!("an {:?}", an);

        let (_, span) = self.bump(end + 2);

        ControlFlow::Break((Token::AxisName(an), span))
    }

    fn ipv6(&mut self) -> ControlFlow<(Token<'s>, Range<usize>)> {
        let end = self
            .input
            .find(|c: char| !c.is_ascii_hexdigit() && c != ':')
            .unwrap_or(self.input.len());

        // An IPv6 literal must have at least 2 colons
        if self.input[..end].chars().filter(|&c| c == ':').count() < 2 {
            return ControlFlow::Continue(());
        }

        // if self.input.get(..=end).is_none_or(|c| !c.ends_with(' ')) {
        //     return ControlFlow::Continue(());
        // }

        let (tok, span) = self.bump(end);
        ControlFlow::Break((Token::IpV6(tok), span))
    }

    fn ipv4(&mut self) -> ControlFlow<(Token<'s>, Range<usize>)> {
        let mut start_idx = 0;

        let mut count = 0;
        while count < 3 {
            let rest = &self.input[start_idx..];
            let digit_idx = rest
                .find(|c: char| !c.is_ascii_digit())
                .unwrap_or(rest.len());
            if digit_idx == 0 {
                return ControlFlow::Continue(());
            }
            start_idx += digit_idx;
            if Some(&b'.') != self.input.as_bytes().get(start_idx) {
                return ControlFlow::Continue(());
            }
            start_idx += 1;
            count += 1;
        }

        let rest = &self.input[start_idx..];
        let digit_idx = rest
            .find(|c: char| !c.is_ascii_digit())
            .unwrap_or(rest.len());

        let final_idx = start_idx + digit_idx;
        let (tok, span) = self.bump(final_idx);
        ControlFlow::Break((Token::IpV4(tok), span))
    }

    fn as_number(&mut self) -> ControlFlow<(Token<'s>, Range<usize>)> {
        let Some(rest) = self.input.strip_prefix("AS") else {
            return ControlFlow::Continue(());
        };

        let digit_idx = rest
            .find(|c: char| !c.is_ascii_digit())
            .unwrap_or(rest.len());

        if digit_idx == 0 {
            return ControlFlow::Continue(());
        }

        let (tok, span) = self.bump(digit_idx + 2);

        ControlFlow::Break((Token::Asn(tok), span))
    }

    fn hex_number(&mut self) -> ControlFlow<(Token<'s>, Range<usize>)> {
        let Some(rest) = self.input.strip_prefix("0x") else {
            return ControlFlow::Continue(());
        };

        let digit_idx = rest
            .find(|c: char| !c.is_ascii_hexdigit())
            .unwrap_or(rest.len());

        let (tok, span) = self.bump(2 + digit_idx);
        ControlFlow::Break((Token::Hex(tok), span))
    }

    fn float(&mut self) -> ControlFlow<(Token<'s>, Range<usize>)> {
        let mut current_idx = self
            .input
            .find(|c: char| !c.is_ascii_digit())
            .unwrap_or(self.input.len());

        if current_idx == 0 {
            return ControlFlow::Continue(());
        }

        let mut rest = &self.input[current_idx..];
        if rest.starts_with('.') {
            current_idx += 1;
            rest = &self.input[current_idx..];

            // If we have `10..` or `10._hello` or `10.hello` we should treat this as an integer
            if let Some(c) = rest.chars().next() {
                if XID_START.contains(c)
                    || c == ':'
                    || c == '.'
                    || c == '_'
                    || c == '-'
                {
                    return ControlFlow::Continue(());
                }
            }

            current_idx += rest
                .find(|c: char| !c.is_ascii_digit())
                .unwrap_or(rest.len());
            rest = &self.input[current_idx..];

            if rest.starts_with(['e', 'E']) {
                current_idx += 1;
                rest = &self.input[current_idx..];
                if rest.starts_with(['+', '-']) {
                    current_idx += 1;
                    rest = &self.input[current_idx..];
                }
                current_idx += rest
                    .find(|c: char| !c.is_ascii_digit())
                    .unwrap_or(rest.len());
            }
        } else if rest.starts_with(['e', 'E']) {
            current_idx += 1;
            rest = &self.input[current_idx..];
            if rest.starts_with(['+', '-']) {
                current_idx += 1;
                rest = &self.input[current_idx..];
            }
            current_idx += rest
                .find(|c: char| !c.is_ascii_digit())
                .unwrap_or(rest.len());
        } else {
            return ControlFlow::Continue(());
        }

        let (tok, span) = self.bump(current_idx);
        ControlFlow::Break((Token::Float(tok), span))
    }

    fn integer(&mut self) -> ControlFlow<(Token<'s>, Range<usize>)> {
        let non_numeric_idx = self
            .input
            .find(|c: char| !c.is_ascii_digit())
            .unwrap_or(self.input.len());

        if non_numeric_idx == 0 {
            return ControlFlow::Continue(());
        }

        let (tok, span) = self.bump(non_numeric_idx);
        ControlFlow::Break((Token::Integer(tok), span))
    }

    fn double_quoted_string(
        &mut self,
    ) -> ControlFlow<(Token<'s>, Range<usize>)> {
        let Some(rest) = self.input.strip_prefix('"') else {
            return ControlFlow::Continue(());
        };

        let mut last_is_backslash = false;
        let end_quote = rest.find(|c| {
            if last_is_backslash {
                last_is_backslash = false;
                return false;
            }

            match c {
                '"' => true,
                '\\' => {
                    last_is_backslash = true;
                    false
                }
                _ => false,
            }
        });

        let Some(end_quote) = end_quote else {
            return ControlFlow::Continue(());
        };

        let (tok, span) = self.bump(2 + end_quote);
        ControlFlow::Break((Token::QuotedString(tok), span))
    }

    fn single_quoted_string(
        &mut self,
    ) -> ControlFlow<(Token<'s>, Range<usize>)> {
        let Some(rest) = self.input.strip_prefix("'") else {
            return ControlFlow::Continue(());
        };

        let mut last_is_backslash = false;
        let end_quote = rest.find(|c| {
            if last_is_backslash {
                last_is_backslash = false;
                return false;
            }

            match c {
                '\'' => true,
                '\\' => {
                    last_is_backslash = true;
                    false
                }
                _ => false,
            }
        });

        let Some(end_quote) = end_quote else {
            return ControlFlow::Continue(());
        };

        let (tok, span) = self.bump(2 + end_quote);
        ControlFlow::Break((Token::QuotedString(tok), span))
    }

    // identifier          = (ALPHA / "_")
    //                       *(ALPHA / DIGIT / "_" / "-" / ".")
    //
    // identifier-ref      = [prefix ":"] identifier
    // prefix              = identifier
    //
    // An unquoted string is any sequence of characters that does not contain
    // any space, tab, carriage return, or line feed characters, a single
    // or double quote character, a semicolon (";"), braces ("{" or "}"), or
    // comment sequences ("//", "/*", or "*/").
    fn unquoted_string_or_identifier_ref(
        &mut self,
    ) -> ControlFlow<(Token<'s>, Range<usize>)> {
        let Some(c) = self.input.chars().next() else {
            return ControlFlow::Continue(());
        };

        // index of the end character of an identifier
        let non_ident_idx = self
            .input
            .find(|c: char| {
                !(XID_CONTINUE.contains(c)
                    || c == ':'
                    || c == '.'
                    || c == '-'
                    || c == '_')
            })
            .unwrap_or(self.input.len());

        let mut non_unq_s_idx = self
            .input
            .find(|c: char| {
                c.is_whitespace() || c == '{' || c == '}' || c == ';'
            })
            .unwrap_or(self.input.len());

        non_unq_s_idx = usize::min(
            self.input.find("/*").unwrap_or(self.input.len()),
            non_unq_s_idx,
        );
        non_unq_s_idx = usize::min(
            self.input.find("//").unwrap_or(self.input.len()),
            non_unq_s_idx,
        );
        non_unq_s_idx = usize::min(
            self.input.find("/*").unwrap_or(self.input.len()),
            non_unq_s_idx,
        );
        non_unq_s_idx = usize::min(
            self.input.find("*/").unwrap_or(self.input.len()),
            non_unq_s_idx,
        );

        // we have trailing garbage, this is not an identifier!
        if non_unq_s_idx > non_ident_idx {
            let (tok, span) = self.bump(non_unq_s_idx);
            return ControlFlow::Break((Token::UnquotedString(tok), span));
        }

        // start character of an identifier
        if !c.is_ascii_alphanumeric() && c != '_' {
            return ControlFlow::Continue(());
        }

        let (ident, span) = self.bump(non_ident_idx);

        let kw = Keyword::try_from(ident)
            .map(Token::Keyword)
            .unwrap_or(Token::Ident(ident));

        ControlFlow::Break((kw, span))
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Token::Ident(s) => s,

            // Punctuation
            Token::AmpAmp => "&&",
            Token::AngleLeftEq => "<=",
            Token::AngleRightEq => ">=",
            Token::Arrow => "->",
            Token::At => "@",
            Token::Bang => "!",
            Token::BangEq => "!=",
            // Token::Colon => ":",
            Token::Comma => ",",
            Token::DoubleDot => "..",
            Token::DoubleColon => "::",
            Token::DoubleSlash => "//",
            Token::Eq => "=",
            Token::EqEq => "==",
            // Token::Hyphen => "-",
            // Token::Period => ".",
            Token::Pipe => "|",
            Token::PipePipe => "||",
            Token::Plus => "+",
            Token::QuestionMark => "?",
            Token::SemiColon => ";",
            Token::Slash => "/",
            Token::Star => "*",

            // Delimiters
            Token::AngleLeft => "<",
            Token::AngleRight => ">",
            Token::CurlyLeft => "{",
            Token::CurlyRight => "}",
            Token::RoundLeft => "(",
            Token::RoundRight => ")",
            Token::SquareLeft => "[",
            Token::SquareRight => "]",

            Token::AxisName(s) => s.into(),
            Token::Keyword(k) => k.as_str(),

            // Literals
            Token::QuotedString(s) => s,
            Token::UnquotedString(s) => s,
            Token::Integer(s) => s,
            Token::Float(s) => s,
            Token::Hex(s) => s,
            Token::Asn(s) => s,
            Token::IpV4(s) => s,
            Token::IpV6(s) => s,
            Token::Bool(true) => "true",
            Token::Bool(false) => "false",

            Token::Test => "test",
        };
        write!(f, "{s}")
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Keyword {
    Module,
    YangVersion,
    NameSpace,
    Prefix,
    Import,
    RevisionDate,
    Include,
    Organization,
    Contact,
    Revision,
    SubModule,
    BelongsTo,
    Units,
    TypeDef,
    Type,
    Bit,
    Position,
    Enum,
    FractionDigits,
    Length,
    Path,
    Pattern,
    Modifier,
    Range,
    RequireInstance,
    Container,
    Must,
    Presence,
    ErrorMessage,
    ErrorAppTag,
    Leaf,
    Mandatory,
    LeafList,
    MinElements,
    MaxElements,
    OrderedBy,
    List,
    Unique,
    Key,
    Choice,
    Case,
    AnyData,
    AnyXml,
    Grouping,
    Uses,
    Refine,
    Rpc,
    Input,
    Output,
    Action,
    Notification,
    Augment,
    Feature,
    Identity,
    Base,
    Extension,
    Argument,
    YinElement,
    IfFeature,
    Deviation,
    Deviate,
    // common statements
    Config,
    Status,
    Description,
    Default,
    Reference,
    When,
    // builtin types sub-statements
    Value,
}

impl Keyword {
    pub fn as_str(&self) -> &'static str {
        match self {
            Keyword::Module => "module",
            Keyword::YangVersion => "yang-version",
            Keyword::NameSpace => "namespace",
            Keyword::Prefix => "prefix",
            Keyword::Import => "import",
            Keyword::RevisionDate => "revision-date",
            Keyword::Include => "include",
            Keyword::Organization => "organization",
            Keyword::Contact => "contact",
            Keyword::Revision => "revision",
            Keyword::SubModule => "submodule",
            Keyword::BelongsTo => "belongs-to",
            Keyword::Units => "units",
            Keyword::TypeDef => "typedef",
            Keyword::Type => "type",
            Keyword::Base => "base",
            Keyword::Bit => "bit",
            Keyword::Position => "position",
            Keyword::Enum => "enum",
            Keyword::FractionDigits => "fraction-digits",
            Keyword::Length => "length",
            Keyword::Path => "path",
            Keyword::Range => "range",
            Keyword::RequireInstance => "require-instance",
            Keyword::Pattern => "pattern",
            Keyword::Modifier => "modifier",
            Keyword::Container => "container",
            Keyword::Must => "must",
            Keyword::Presence => "presence",
            Keyword::ErrorAppTag => "error-app-tag",
            Keyword::ErrorMessage => "error-message",
            Keyword::Leaf => "leaf",
            Keyword::Mandatory => "mandatory",
            Keyword::LeafList => "leaf-list",
            Keyword::MinElements => "min-elements",
            Keyword::MaxElements => "max-elements",
            Keyword::OrderedBy => "ordered-by",
            Keyword::List => "list",
            Keyword::Unique => "unique",
            Keyword::Key => "key",
            Keyword::Choice => "choice",
            Keyword::Case => "case",
            Keyword::AnyData => "anydata",
            Keyword::AnyXml => "anyxml",
            Keyword::Grouping => "grouping",
            Keyword::Uses => "uses",
            Keyword::Refine => "refine",
            Keyword::Rpc => "rpc",
            Keyword::Input => "input",
            Keyword::Output => "output",
            Keyword::Action => "action",
            Keyword::Notification => "notification",
            Keyword::Augment => "augment",
            Keyword::Feature => "feature",
            Keyword::Identity => "identity",
            Keyword::Extension => "extension",
            Keyword::Argument => "argument",
            Keyword::YinElement => "yin-element",
            Keyword::IfFeature => "if-feature",
            Keyword::Deviation => "deviation",
            Keyword::Deviate => "deviate",
            Keyword::Config => "config",
            Keyword::Status => "status",
            Keyword::Default => "default",
            Keyword::Description => "description",
            Keyword::Reference => "reference",
            Keyword::When => "when",
            Keyword::Value => "value",
        }
    }

    pub(crate) fn argument_constraints(&self) -> (ArgConstraints, bool) {
        match self {
            Keyword::Module => (ArgConstraints::NoKeyword, true),
            Keyword::Import => (ArgConstraints::NoKeyword, true),
            Keyword::Include => (ArgConstraints::NoKeyword, true),
            Keyword::Organization => todo!(),
            Keyword::Contact => todo!(),
            Keyword::Revision => todo!(),
            Keyword::SubModule => todo!(),
            Keyword::BelongsTo => todo!(),
            Keyword::Units => todo!(),
            Keyword::TypeDef => todo!(),
            Keyword::Type => todo!(),
            Keyword::Bit => todo!(),
            Keyword::Position => todo!(),
            Keyword::Enum => todo!(),
            Keyword::FractionDigits => todo!(),
            Keyword::Length => todo!(),
            Keyword::Path => todo!(),
            Keyword::Pattern => todo!(),
            Keyword::Range => todo!(),
            Keyword::RequireInstance => todo!(),
            Keyword::Container => todo!(),
            Keyword::Must => todo!(),
            Keyword::Presence => todo!(),
            Keyword::ErrorMessage => todo!(),
            Keyword::ErrorAppTag => todo!(),
            Keyword::Leaf => todo!(),
            Keyword::Mandatory => todo!(),
            Keyword::LeafList => todo!(),
            Keyword::MinElements => todo!(),
            Keyword::MaxElements => todo!(),
            Keyword::OrderedBy => todo!(),
            Keyword::List => todo!(),
            Keyword::Unique => todo!(),
            Keyword::Key => todo!(),
            Keyword::Choice => todo!(),
            Keyword::Case => todo!(),
            Keyword::AnyData => todo!(),
            Keyword::AnyXml => todo!(),
            Keyword::Grouping => todo!(),
            Keyword::Uses => todo!(),
            Keyword::Refine => todo!(),
            Keyword::Rpc => todo!(),
            Keyword::Input => todo!(),
            Keyword::Output => todo!(),
            Keyword::Action => todo!(),
            Keyword::Notification => todo!(),
            Keyword::Augment => todo!(),
            Keyword::Feature => todo!(),
            Keyword::Identity => todo!(),
            Keyword::Base => todo!(),
            Keyword::Extension => todo!(),
            Keyword::Argument => todo!(),
            Keyword::YinElement => todo!(),
            Keyword::IfFeature => todo!(),
            Keyword::Deviation => todo!(),
            Keyword::Deviate => todo!(),
            Keyword::Config => todo!(),
            Keyword::Status => todo!(),
            Keyword::Description => todo!(),
            Keyword::Default => todo!(),
            Keyword::Reference => todo!(),
            Keyword::When => todo!(),
            Keyword::Value => todo!(),
            // by default we assume a string without sub statements
            _ => (ArgConstraints::NoKeyword, false),
        }
    }

    pub(crate) fn allowed_sub_stmts(&self) -> Vec<(Keyword, Cardinality)> {
        match self {
            Keyword::Module => allowed_subs2!(
                (AnyData, Inf),
                (AnyXml, Inf),
                (Augment, Inf),
                (Choice, Inf),
                (Contact, ZeroOrOne),
                (Container, Inf),
                (Description, ZeroOrOne),
                (Deviation, Inf),
                (Extension, Inf),
                (Feature, Inf),
                (Grouping, Inf),
                (Identity, Inf),
                (Import, Inf),
                (RevisionDate, Inf),
                (Include, Inf),
                (Leaf, Inf),
                (Mandatory, Inf),
                (LeafList, Inf),
                (List, Inf),
                (NameSpace, ExactlyOne),
                (Notification, Inf),
                (Organization, Inf),
                (Prefix, ExactlyOne),
                (Reference, Inf),
                (Revision, Inf),
                (Rpc, Inf),
                (TypeDef, Inf),
                (Uses, Inf),
                (YangVersion, ExactlyOne),
            ),
            Keyword::YangVersion => vec![],
            Keyword::NameSpace => vec![],
            Keyword::Prefix => vec![],
            Keyword::Import => allowed_subs2!(
                (Description, ZeroOrOne),
                (Prefix, ExactlyOne),
                (RevisionDate, ZeroOrOne),
                (Reference, ZeroOrOne),
            ),

            Keyword::RevisionDate => vec![],
            Keyword::Include => {
                allowed_subs2!(
                    (Description, ZeroOrOne),
                    (Reference, ZeroOrOne),
                    (RevisionDate, ZeroOrOne),
                )
            }
            Keyword::Organization => vec![],
            Keyword::Contact => vec![],
            Keyword::Revision => allowed_subs2!(
                (Description, ZeroOrOne),
                (Reference, ZeroOrOne),
            ),
            Keyword::SubModule => allowed_subs2!(
                (AnyData, Inf),
                (AnyXml, Inf),
                (Augment, Inf),
                (BelongsTo, ExactlyOne),
                (Choice, Inf),
                (Contact, ZeroOrOne),
                (Container, Inf),
                (Description, ZeroOrOne),
                (Deviation, Inf),
                (Extension, Inf),
                (Feature, Inf),
                (Grouping, Inf),
                (Identity, Inf),
                (Import, Inf),
                (Include, Inf),
                (Leaf, Inf),
                (LeafList, Inf),
                (List, Inf),
                (Notification, Inf),
                (Organization, Inf),
                (Revision, Inf),
                (Rpc, Inf),
                (TypeDef, Inf),
                (Uses, Inf),
                (YangVersion, ExactlyOne),
            ),
            Keyword::BelongsTo => allowed_subs2!((Prefix, ExactlyOne),),
            Keyword::Units => vec![],
            Keyword::TypeDef => {
                allowed_subs2!(
                    (Default, ZeroOrOne),
                    (Description, ZeroOrOne),
                    (Reference, ZeroOrOne),
                    (Status, ZeroOrOne),
                    (Type, ExactlyOne),
                    (Units, ZeroOrOne),
                )
            }
            Keyword::Type => allowed_subs2!(
                (Base, Inf),
                (Bit, Inf),
                (Enum, Inf),
                (FractionDigits, ZeroOrOne),
                (Length, ZeroOrOne),
                (Path, ZeroOrOne),
                (Pattern, Inf),
                (Range, ZeroOrOne),
                (RequireInstance, ZeroOrOne),
                (Type, Inf),
            ),
            Keyword::Base => vec![],
            Keyword::Bit => allowed_subs2!(
                (Description, ZeroOrOne),
                (IfFeature, Inf),
                (Position, ZeroOrOne),
                (Reference, ZeroOrOne),
                (Status, ZeroOrOne),
            ),
            Keyword::Position => vec![],
            Keyword::FractionDigits => vec![],
            Keyword::Length => allowed_subs2!(
                (Description, ZeroOrOne),
                (ErrorAppTag, ZeroOrOne),
                (ErrorMessage, ZeroOrOne),
                (Reference, ZeroOrOne),
            ),
            Keyword::Path => vec![],
            Keyword::Range => vec![],
            Keyword::RequireInstance => vec![],
            Keyword::Pattern => allowed_subs2!(
                (Description, ZeroOrOne),
                (ErrorAppTag, ZeroOrOne),
                (ErrorMessage, ZeroOrOne),
                (Modifier, ZeroOrOne),
                (Reference, ZeroOrOne),
            ),
            Keyword::Modifier => vec![],
            Keyword::Container => allowed_subs2!(
                (Action, Inf),
                (AnyData, Inf),
                (AnyXml, Inf),
                (Choice, Inf),
                (Config, ZeroOrOne),
                (Container, Inf),
                (Description, ZeroOrOne),
                (Grouping, Inf),
                (IfFeature, Inf),
                (Leaf, Inf),
                (LeafList, Inf),
                (List, Inf),
                (Must, Inf),
                (Notification, Inf),
                (Presence, ZeroOrOne),
                (Reference, ZeroOrOne),
                (Status, ZeroOrOne),
                (TypeDef, Inf),
                (Uses, Inf),
                (When, ZeroOrOne),
            ),
            Keyword::Must => allowed_subs2!(
                (Description, ZeroOrOne),
                (ErrorAppTag, ZeroOrOne),
                (ErrorMessage, ZeroOrOne),
                (Reference, ZeroOrOne),
            ),
            Keyword::ErrorAppTag => vec![],
            Keyword::ErrorMessage => vec![],
            Keyword::Presence => vec![],
            Keyword::Leaf => allowed_subs2!(
                (Config, ZeroOrOne),
                (Default, ZeroOrOne),
                (Description, ZeroOrOne),
                (IfFeature, Inf),
                (Mandatory, ZeroOrOne),
                (Must, Inf),
                (Reference, ZeroOrOne),
                (Status, ZeroOrOne),
                (Type, ExactlyOne),
                (Units, ZeroOrOne),
                (When, ZeroOrOne),
            ),
            Keyword::Mandatory => vec![],
            Keyword::LeafList => allowed_subs2!(
                (Config, ZeroOrOne),
                (Default, ZeroOrOne),
                (Description, ZeroOrOne),
                (IfFeature, ZeroOrOne),
                (MaxElements, ZeroOrOne),
                (MinElements, ZeroOrOne),
                (Must, Inf),
                (OrderedBy, ZeroOrOne),
                (Reference, ZeroOrOne),
                (Status, ZeroOrOne),
                (Type, ExactlyOne),
                (Units, ZeroOrOne),
                (When, ZeroOrOne),
            ),
            Keyword::MinElements => vec![],
            Keyword::MaxElements => vec![],
            Keyword::OrderedBy => vec![],
            Keyword::List => allowed_subs2!(
                (Action, Inf),
                (AnyData, Inf),
                (AnyXml, Inf),
                (Choice, Inf),
                (Config, ZeroOrOne),
                (Container, Inf),
                (Description, ZeroOrOne),
                (Grouping, Inf),
                (IfFeature, Inf),
                (Key, ZeroOrOne),
                (Leaf, Inf),
                (LeafList, Inf),
                (List, Inf),
                (MaxElements, Inf),
                (MinElements, Inf),
                (Must, Inf),
                (Notification, Inf),
                (OrderedBy, ZeroOrOne),
                (Reference, ZeroOrOne),
                (Status, ZeroOrOne),
                (TypeDef, ZeroOrOne),
                (Unique, ZeroOrOne),
                (Uses, Inf),
                (When, ZeroOrOne),
            ),
            Keyword::Key => vec![],
            Keyword::Unique => vec![],
            Keyword::Choice => allowed_subs2!(
                (AnyData, Inf),
                (AnyXml, Inf),
                (Case, Inf),
                (Choice, Inf),
                (Config, ZeroOrOne),
                (Container, Inf),
                (Default, ZeroOrOne),
                (Description, ZeroOrOne),
                (IfFeature, Inf),
                (Leaf, Inf),
                (LeafList, Inf),
                (List, Inf),
                (Mandatory, ZeroOrOne),
                (Reference, ZeroOrOne),
                (Status, ZeroOrOne),
                (When, ZeroOrOne),
            ),
            Keyword::Case => allowed_subs2!(
                (AnyData, Inf),
                (AnyXml, Inf),
                (Choice, Inf),
                (Config, ZeroOrOne),
                (Container, Inf),
                (Description, ZeroOrOne),
                (IfFeature, Inf),
                (Leaf, Inf),
                (LeafList, Inf),
                (List, Inf),
                (Reference, ZeroOrOne),
                (Status, ZeroOrOne),
                (Uses, Inf),
                (When, ZeroOrOne),
            ),
            Keyword::AnyData => allowed_subs2!(
                (Config, ZeroOrOne),
                (Description, ZeroOrOne),
                (IfFeature, Inf),
                (Mandatory, ZeroOrOne),
                (Must, Inf),
                (Reference, ZeroOrOne),
                (Status, ZeroOrOne),
                (When, ZeroOrOne),
            ),
            Keyword::AnyXml => vec![],
            Keyword::Grouping => allowed_subs2!(
                (Action, Inf),
                (AnyData, Inf),
                (AnyXml, Inf),
                (Choice, Inf),
                (Container, Inf),
                (Description, ZeroOrOne),
                (Grouping, Inf),
                (Leaf, Inf),
                (LeafList, Inf),
                (List, Inf),
                (Notification, Inf),
                (Reference, ZeroOrOne),
                (Status, ZeroOrOne),
                (TypeDef, Inf),
                (Uses, Inf),
            ),
            Keyword::Uses => allowed_subs2!(
                (Augment, Inf),
                (Description, ZeroOrOne),
                (IfFeature, Inf),
                (Reference, ZeroOrOne),
                (Refine, Inf),
                (Status, ZeroOrOne),
                (When, ZeroOrOne),
            ),
            Keyword::Refine => allowed_subs2!(
                (Default, ZeroOrOne),
                (Description, ZeroOrOne),
                (Reference, ZeroOrOne),
                (Config, ZeroOrOne),
                (Mandatory, ZeroOrOne),
                (Presence, ZeroOrOne),
                (Must, Inf),
                (MaxElements, ZeroOrOne),
                (IfFeature, Inf),
                (Refine, Inf),
                (Status, ZeroOrOne),
            ),
            Keyword::Rpc => allowed_subs2!(
                (Description, ZeroOrOne),
                (Grouping, Inf),
                (IfFeature, Inf),
                (Input, ZeroOrOne),
                (Output, ZeroOrOne),
                (Reference, ZeroOrOne),
                (Status, ZeroOrOne),
                (TypeDef, ZeroOrOne),
            ),
            Keyword::Input => allowed_subs2!(
                (AnyData, Inf),
                (AnyXml, Inf),
                (Choice, Inf),
                (Container, Inf),
                (Grouping, Inf),
                (Leaf, Inf),
                (LeafList, Inf),
                (List, Inf),
                (Must, Inf),
                (TypeDef, ZeroOrOne),
                (Uses, Inf),
            ),
            Keyword::Output => allowed_subs2!(
                (AnyData, Inf),
                (AnyXml, Inf),
                (Choice, Inf),
                (Container, Inf),
                (Grouping, Inf),
                (Leaf, Inf),
                (LeafList, Inf),
                (List, Inf),
                (Must, Inf),
                (TypeDef, ZeroOrOne),
                (Uses, Inf),
            ),
            Keyword::Action => allowed_subs2!(
                (Description, ZeroOrOne),
                (Grouping, Inf),
                (IfFeature, Inf),
                (Input, ZeroOrOne),
                (Output, ZeroOrOne),
                (Reference, ZeroOrOne),
                (Status, ZeroOrOne),
                (TypeDef, ZeroOrOne),
            ),
            Keyword::Notification => allowed_subs2!(
                (AnyData, Inf),
                (AnyXml, Inf),
                (Choice, Inf),
                (Container, Inf),
                (Description, ZeroOrOne),
                (Grouping, Inf),
                (IfFeature, Inf),
                (Leaf, Inf),
                (LeafList, Inf),
                (List, Inf),
                (Must, Inf),
                (TypeDef, ZeroOrOne),
                (Reference, ZeroOrOne),
                (Status, ZeroOrOne),
                (Uses, Inf),
            ),
            Keyword::Augment => allowed_subs2!(
                (Action, Inf),
                (AnyData, Inf),
                (AnyXml, Inf),
                (Case, Inf),
                (Choice, Inf),
                (Container, Inf),
                (Description, ZeroOrOne),
                (IfFeature, Inf),
                (Leaf, Inf),
                (LeafList, Inf),
                (List, Inf),
                (Notification, Inf),
                (Reference, ZeroOrOne),
                (Status, ZeroOrOne),
                (Uses, Inf),
                (When, ZeroOrOne),
            ),
            Keyword::Identity => {
                allowed_subs2!(
                    (Base, Inf),
                    (Description, ZeroOrOne),
                    (IfFeature, Inf),
                    (Reference, ZeroOrOne),
                    (Status, ZeroOrOne),
                )
            }
            Keyword::Extension => {
                allowed_subs2!(
                    (Argument, ZeroOrOne),
                    (Description, ZeroOrOne),
                    (Reference, ZeroOrOne),
                    (Status, ZeroOrOne),
                )
            }
            Keyword::Argument => allowed_subs2!((YinElement, ZeroOrOne),),
            Keyword::Feature => {
                allowed_subs2!(
                    (Description, ZeroOrOne),
                    (IfFeature, Inf),
                    (Reference, ZeroOrOne),
                    (Status, ZeroOrOne),
                )
            }
            Keyword::YinElement => vec![],
            Keyword::IfFeature => vec![],
            Keyword::Deviation => {
                allowed_subs2!(
                    (Description, ZeroOrOne),
                    (Deviate, OneOrMore),
                    (Reference, ZeroOrOne),
                )
            }
            Keyword::Deviate => vec![],
            Keyword::Config => vec![],
            Keyword::Status => vec![],
            Keyword::Description => vec![],
            Keyword::Default => vec![],
            Keyword::Reference => vec![],
            // RFC7950 doesn't mention sub-statements for 'when', but it does
            // appear in RFCs, e.g. RFC8022, so whatever
            Keyword::When => allowed_subs2!((Description, ZeroOrOne),),
            Keyword::Enum => allowed_subs2!(
                (Description, ZeroOrOne),
                (IfFeature, Inf),
                (Reference, ZeroOrOne),
                (Status, ZeroOrOne),
                (Value, ZeroOrOne),
            ),
            Keyword::Value => vec![],
        }
    }
}

pub struct KeywordError;

impl TryFrom<&str> for Keyword {
    type Error = KeywordError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "module" => Ok(Keyword::Module),
            "yang-version" => Ok(Keyword::YangVersion),
            "namespace" => Ok(Keyword::NameSpace),
            "prefix" => Ok(Keyword::Prefix),
            "import" => Ok(Keyword::Import),
            "revision-date" => Ok(Keyword::RevisionDate),
            "include" => Ok(Keyword::Include),
            "organization" => Ok(Keyword::Organization),
            "contact" => Ok(Keyword::Contact),
            "revision" => Ok(Keyword::Revision),
            "submodule" => Ok(Keyword::SubModule),
            "belongs-to" => Ok(Keyword::BelongsTo),
            "units" => Ok(Keyword::Units),
            "typedef" => Ok(Keyword::TypeDef),
            "type" => Ok(Keyword::Type),
            "base" => Ok(Keyword::Base),
            "bit" => Ok(Keyword::Bit),
            "position" => Ok(Keyword::Position),
            "enum" => Ok(Keyword::Enum),
            "fraction-digits" => Ok(Keyword::FractionDigits),
            "length" => Ok(Keyword::Length),
            "path" => Ok(Keyword::Path),
            "range" => Ok(Keyword::Range),
            "require-instance" => Ok(Keyword::RequireInstance),
            "pattern" => Ok(Keyword::Pattern),
            "modifier" => Ok(Keyword::Modifier),
            "container" => Ok(Keyword::Container),
            "must" => Ok(Keyword::Must),
            "presence" => Ok(Keyword::Presence),
            "error-app-tag" => Ok(Keyword::ErrorAppTag),
            "error-message" => Ok(Keyword::ErrorMessage),
            "leaf" => Ok(Keyword::Leaf),
            "mandatory" => Ok(Keyword::Mandatory),
            "leaf-list" => Ok(Keyword::LeafList),
            "min-elements" => Ok(Keyword::MinElements),
            "max-elements" => Ok(Keyword::MaxElements),
            "ordered-by" => Ok(Keyword::OrderedBy),
            "list" => Ok(Keyword::List),
            "unique" => Ok(Keyword::Unique),
            "key" => Ok(Keyword::Key),
            "choice" => Ok(Keyword::Choice),
            "case" => Ok(Keyword::Case),
            "anydata" => Ok(Keyword::AnyData),
            "anyxml" => Ok(Keyword::AnyXml),
            "grouping" => Ok(Keyword::Grouping),
            "uses" => Ok(Keyword::Uses),
            "refine" => Ok(Keyword::Refine),
            "rpc" => Ok(Keyword::Rpc),
            "input" => Ok(Keyword::Input),
            "output" => Ok(Keyword::Output),
            "action" => Ok(Keyword::Action),
            "notification" => Ok(Keyword::Notification),
            "augment" => Ok(Keyword::Augment),
            "feature" => Ok(Keyword::Feature),
            "identity" => Ok(Keyword::Identity),
            "extension" => Ok(Keyword::Extension),
            "argument" => Ok(Keyword::Argument),
            "yin-element" => Ok(Keyword::YinElement),
            "if-feature" => Ok(Keyword::IfFeature),
            "deviation" => Ok(Keyword::Deviation),
            "deviate" => Ok(Keyword::Deviate),
            "config" => Ok(Keyword::Config),
            "status" => Ok(Keyword::Status),
            "default" => Ok(Keyword::Default),
            "description" => Ok(Keyword::Description),
            "reference" => Ok(Keyword::Reference),
            "when" => Ok(Keyword::When),
            "value" => Ok(Keyword::Value),
            _ => Err(KeywordError),
        }
    }
}
