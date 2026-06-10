//! Tokens produced by the lexer

use std::fmt::Display;

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'s> {
    Ident(&'s str),

    // === Punctuation ===
    AmpAmp,
    AngleLeftEq,
    AngleRightEq,
    Arrow,
    Bang,
    BangEq,
    Colon,
    Comma,
    Eq,
    EqEq,
    FatArrow,
    Hash,
    Hyphen,
    HyphenHyphen,
    Period,
    Pipe,
    PipePipe,
    Plus,
    QuestionMark,
    SemiColon,
    Slash,
    SlashStar,
    Star,
    Percent,
    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,
    PercentEq,

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

    // === Literals ===
    String(&'s str),
    Char(&'s str),
    Integer(&'s str),
    Float(&'s str),
    Hex(&'s str),
    Asn(&'s str),
    IpV4(&'s str),
    IpV6(&'s str),
    Bool(bool),

    /// An f-string start token signals to the parser that an f-string is coming up
    FStringStart,
}

pub enum FStringToken<'s> {
    /// The final part of a string.
    ///
    /// This is the token from the current position to the end of the string.
    /// For non-f-strings, this will be the entire string.
    StringEnd(&'s str),

    /// An intermediate part of an f-string, until the next `{` token.
    StringIntermediate(&'s str),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Keyword {
    Accept,
    Const,
    Dep,
    Else,
    Enum,
    Filter,
    FilterMap,
    For,
    Fn,
    If,
    Import,
    In,
    Let,
    Match,
    Pkg,
    Record,
    Reject,
    Return,
    Std,
    Super,
    Test,
    While,
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
            Token::Bang => "!",
            Token::BangEq => "!=",
            Token::Colon => ":",
            Token::Comma => ",",
            Token::Eq => "=",
            Token::EqEq => "==",
            Token::FatArrow => "=>",
            Token::Hash => "#",
            Token::Hyphen => "-",
            Token::HyphenHyphen => "--",
            Token::Period => ".",
            Token::Pipe => "|",
            Token::PipePipe => "||",
            Token::Plus => "+",
            Token::QuestionMark => "?",
            Token::SemiColon => ";",
            Token::Slash => "/",
            Token::SlashStar => "/*",
            Token::Star => "*",
            Token::Percent => "%",
            Token::PlusEq => "+=",
            Token::MinusEq => "-=",
            Token::StarEq => "*=",
            Token::SlashEq => "/=",
            Token::PercentEq => "%=",

            // Delimiters
            Token::AngleLeft => "<",
            Token::AngleRight => ">",
            Token::CurlyLeft => "{",
            Token::CurlyRight => "}",
            Token::RoundLeft => "(",
            Token::RoundRight => ")",
            Token::SquareLeft => "[",
            Token::SquareRight => "]",

            // Keywords
            Token::Keyword(k) => k.as_str(),

            // Literals
            Token::String(s) => s,
            Token::Char(s) => s,
            Token::Integer(s) => s,
            Token::Float(s) => s,
            Token::Hex(s) => s,
            Token::Asn(s) => s,
            Token::IpV4(s) => s,
            Token::IpV6(s) => s,
            Token::Bool(true) => "true",
            Token::Bool(false) => "false",

            Token::FStringStart => "f\"",
        };

        f.write_str(s)
    }
}

impl Keyword {
    fn as_str(&self) -> &'static str {
        match self {
            Keyword::Accept => "accept",
            Keyword::Const => "const",
            Keyword::Dep => "dep",
            Keyword::Else => "else",
            Keyword::Enum => "enum",
            Keyword::Filter => "filter",
            Keyword::FilterMap => "filtermap",
            Keyword::For => "for",
            Keyword::Fn => "fn",
            Keyword::If => "if",
            Keyword::Import => "import",
            Keyword::In => "in",
            Keyword::Let => "let",
            Keyword::Match => "match",
            Keyword::Pkg => "pkg",
            Keyword::Record => "record",
            Keyword::Reject => "reject",
            Keyword::Return => "return",
            Keyword::Std => "std",
            Keyword::Super => "super",
            Keyword::Test => "test",
            Keyword::While => "while",
        }
    }
}
