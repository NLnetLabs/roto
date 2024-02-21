use logos::Logos;

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"([ \t\n\f]|(//[^\n]*))+")]
pub enum Token<'s> {
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
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
    #[token("||")]
    PipePipe,
    #[token(">=")]
    AngleRightEq,
    #[token("<=")]
    AngleLeftEq,
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
    #[token("action")]
    Action,
    #[token("all")]
    All,
    #[token("apply")]
    Apply,
    #[token("contains")]
    Contains,
    #[token("define")]
    Define,
    #[token("exact")]
    Exact,
    #[token("exactly-one")]
    ExactlyOne,
    #[token("filter-map")]
    FilterMap,
    #[token("filter")]
    Filter,
    #[token("for")]
    For,
    #[token("import")]
    Import,
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
    #[token("rx")]
    Rx,
    #[token("rx_tx")]
    RxTx,
    #[token("some")]
    Some,
    #[token("table")]
    Table,
    #[token("term")]
    Term,
    #[token("tx")]
    Tx,
    #[token("through")]
    Through,
    #[token("type")]
    Type,
    #[token("upto")]
    UpTo,
    #[token("use")]
    Use,
    #[token("with")]
    With,

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
    #[regex(r"([0-9a-zA-Z]+:){6}[0-9a-zA-Z]+")]
    IpV6(&'s str),

    #[token("true", |_| true)]
    #[token("false", |_| false)]
    Bool(bool),
}
