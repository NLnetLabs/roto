use std::fmt::Display;

use logos::Logos;

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"([ \t\n\f]|(#[^\n]*))+")]
pub enum Token<'s> {
    #[regex("[a-zA-Z_][a-zA-Z0-9_-]*")]
    Ident(&'s str),

    // === Punctuation ===
    #[token("==")]
    EqEq,
    #[token("=")]
    Eq,
    #[token("!=")]
    BangEq,
    #[token("&&")]
    AmpAmp,
    #[token("|")]
    Pipe,
    #[token("||")]
    PipePipe,
    #[token(">=")]
    AngleRightEq,
    #[token("<=")]
    AngleLeftEq,
    #[token("->")]
    Arrow,
    #[token("-")]
    Hyphen,
    #[token(":")]
    Colon,
    #[token(";")]
    SemiColon,
    #[token(",")]
    Comma,
    #[token(".")]
    Period,
    #[token("/")]
    Slash,
    #[token("!")]
    Bang,
    #[token("+")]
    Plus,
    #[token("*")]
    Star,

    // === Delimiters ===
    #[token("{")]
    CurlyLeft,
    #[token("}")]
    CurlyRight,
    #[token("[")]
    SquareLeft,
    #[token("]")]
    SquareRight,
    #[token("(")]
    RoundLeft,
    #[token(")")]
    RoundRight,
    #[token("<")]
    AngleLeft,
    #[token(">")]
    AngleRight,

    // === Keywords ===
    #[token("accept")]
    Accept,
    #[token("apply")]
    Apply,
    #[token("contains")]
    Contains,
    #[token("define")]
    Define,
    #[token("else")]
    Else,
    #[token("exact")]
    Exact,
    #[token("filter-map")]
    FilterMap,
    #[token("filter")]
    Filter,
    #[token("function")]
    Function,
    #[token("import")]
    Import,
    #[token("if")]
    If,
    #[token("in")]
    In,
    #[token("longer")]
    Longer,
    #[token("match")]
    Match,
    #[token("matching")]
    Matching,
    #[token("module")]
    Module,
    #[token("netmask")]
    NetMask,
    #[token("not")]
    Not,
    #[token("orlonger")]
    OrLonger,
    #[token("output-stream")]
    OutputStream,
    #[token("prefix-length-range")]
    PrefixLengthRange,
    #[token("reject")]
    Reject,
    #[token("return")]
    Return,
    #[token("rib")]
    Rib,
    #[token("some")]
    Some,
    #[token("table")]
    Table,
    #[token("through")]
    Through,
    #[token("type")]
    Type,
    #[token("upto")]
    UpTo,
    #[token("use")]
    Use,

    // === Literals ===
    // String literal with escape sequences would look like this:
    // #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#)]
    // For now, keep it simple and just lex until the next quote.
    #[regex(r#""[^"]*""#)]
    String(&'s str),
    // Integers can contain underscores, but cannot start with them.
    #[regex(r"[0-9][0-9_]*")]
    Integer(&'s str),

    #[regex(r"0x[0-9A-Fa-f]+")]
    Hex(&'s str),
    #[regex(r"AS[0-9]+")]
    Asn(&'s str),
    #[regex(r"[0-9]+\.[0-9]*")]
    Float,

    #[regex(r"([0-9]+\.){3}[0-9]+")]
    IpV4(&'s str),
    #[regex(r"[0-9a-zA-Z]*(:[0-9a-zA-Z]*){2,6}")]
    IpV6(&'s str),

    // This regex is a super set of all the forms of communities:
    // standard, large and extended.
    #[regex(r"([0-9a-zA-Z]+:)?(0x)?[0-9a-fA-F]+:(0x)?[0-9a-fA-F]+")]
    Community(&'s str),

    #[token("true", |_| true)]
    #[token("false", |_| false)]
    Bool(bool),
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Token::Ident(s) => s,
            Token::EqEq => "==",
            Token::Eq => "=",
            Token::BangEq => "!=",
            Token::AmpAmp => "&&",
            Token::Pipe => "|",
            Token::PipePipe => "||",
            Token::AngleRightEq => ">=",
            Token::AngleLeftEq => "<=",
            Token::Arrow => "->",
            Token::Hyphen => "-",
            Token::Colon => ":",
            Token::SemiColon => ";",
            Token::Comma => ",",
            Token::Period => ".",
            Token::Plus => "+",
            Token::Star => "*",
            Token::Slash => "/",
            Token::Bang => "!",
            Token::CurlyLeft => "{",
            Token::CurlyRight => "}",
            Token::SquareLeft => "[",
            Token::SquareRight => "]",
            Token::RoundLeft => "(",
            Token::RoundRight => ")",
            Token::AngleLeft => "<",
            Token::AngleRight => ">",
            Token::Accept => "accept",
            Token::Apply => "apply",
            Token::Contains => "contains",
            Token::Define => "define",
            Token::Else => "else",
            Token::Exact => "exact",
            Token::FilterMap => "filter-map",
            Token::Filter => "filter",
            Token::Function => "function",
            Token::Import => "import",
            Token::If => "if",
            Token::In => "in",
            Token::Longer => "longer",
            Token::Match => "match",
            Token::Matching => "matching",
            Token::Module => "module",
            Token::NetMask => "net-mask",
            Token::Not => "not",
            Token::OrLonger => "or-longer",
            Token::OutputStream => "output-stream",
            Token::PrefixLengthRange => "prefix-length-range",
            Token::Reject => "reject",
            Token::Return => "return",
            Token::Rib => "rib",
            Token::Some => "some",
            Token::Table => "table",
            Token::Through => "through",
            Token::Type => "type",
            Token::UpTo => "up-to",
            Token::Use => "use",
            Token::String(s) => s,
            Token::Integer(s) => s,
            Token::Hex(s) => s,
            Token::Asn(s) => s,
            Token::Float => "float",
            Token::IpV4(s) => s,
            Token::IpV6(s) => s,
            Token::Community(s) => s,
            Token::Bool(true) => "true",
            Token::Bool(false) => "false",
        };
        write!(f, "{s}")
    }
}
