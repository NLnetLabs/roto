//! Lexer for Roto scripts

use core::{ops::Range, str};
use std::{
    any::TypeId,
    collections::HashSet,
    fmt::{Debug, Display},
    ops::ControlFlow,
    sync::Arc,
};

use unicode_ident::{is_xid_continue, is_xid_start};

use crate::{ast::Identifier, runtime::Rt};

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
    Hyphen,
    HyphenHyphen,
    Period,
    Pipe,
    PipePipe,
    Plus,
    QuestionMark,
    SemiColon,
    Slash,
    SlashSlash,
    SlashStar,
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

    /// A custom Runtime-specified lexer hook.
    Custom(CustomToken),
}

/// A token created by a lexer hook.
#[derive(Clone)]
pub struct CustomToken {
    pub(crate) val: Arc<dyn Send + Sync + 'static>,
    id: TypeId,
}

impl Debug for CustomToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("CustomToken")
            .field(&Arc::as_ptr(&self.val))
            .finish()
    }
}
impl CustomToken {
    /// Create a new token
    pub fn new<T: Send + Sync + 'static>(x: T) -> Self {
        Self {
            val: Arc::new(x),
            id: TypeId::of::<T>(),
        }
    }

    /// Get the type id of the value this token was constructed with.
    pub fn type_id(&self) -> TypeId {
        self.id
    }
}

impl PartialEq for CustomToken {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
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
    Dep,
    Else,
    Filter,
    FilterMap,
    For,
    Fn,
    If,
    Import,
    In,
    Let,
    Match,
    Not,
    Pkg,
    Record,
    Reject,
    Return,
    Std,
    Super,
    Test,
    Variant,
    While,
}

/// Roto's lexer
pub struct Lexer<'a> {
    input: &'a str,
    original_length: usize,
    peeked: Option<(Result<Token<'a>, ()>, Range<usize>)>,
    pub(crate) almost_keyword:
        Option<(Identifier, Range<usize>, Option<&'static str>)>,
    hook: Option<LexerHook>,
    custom_type_ids: HashSet<TypeId>,
}

/// A function to give to the runtime to augment the lexer for custom literals.
pub type LexerHook = Arc<
    dyn for<'b> Fn(
        &mut Lexer<'b>,
    ) -> ControlFlow<(CustomToken, Range<usize>)>,
>;

impl<'a> Lexer<'a> {
    pub(crate) fn next(
        &mut self,
    ) -> Option<(Result<Token<'a>, ()>, Range<usize>)> {
        if self.peeked.is_some() {
            return self.peeked.take();
        }
        self.next_inner()
    }

    pub(crate) fn peek(
        &mut self,
    ) -> &Option<(Result<Token<'a>, ()>, Range<usize>)> {
        if self.peeked.is_none() {
            self.peeked = self.next_inner();
        }
        &self.peeked
    }

    fn next_inner(
        &mut self,
    ) -> Option<(Result<Token<'a>, ()>, Range<usize>)> {
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
    pub(crate) fn new(input: &'s str, rt: &Rt) -> Self {
        let hook = rt.lexer_hook();
        let custom_type_ids = rt.custom_type_ids();
        Self {
            input,
            original_length: input.len(),
            peeked: None,
            almost_keyword: None,
            hook,
            custom_type_ids,
        }
    }

    pub(crate) fn new_simple(input: &'s str) -> Self {
        Self {
            input,
            original_length: input.len(),
            peeked: None,
            almost_keyword: None,
            hook: None,
            custom_type_ids: HashSet::new(),
        }
    }

    /// Get the _remaining_ input string of the lexer
    pub fn input(&self) -> &str {
        self.input
    }

    /// Move the lexer forward by `n` bytes.
    ///
    /// Returns the string and its corresponding range.
    pub fn bump(&mut self, n: usize) -> (&'s str, Range<usize>) {
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
        self.skip_whitespace();

        if self.is_empty() {
            return ControlFlow::Continue(());
        }

        // This is doing a clone that should be unnecessary, but as a proof of
        // concept, this works.
        if let Some(f) = self.hook.as_ref().map(|x| x.clone()) {
            f(self)
                .map_break(|t| {
                    if !self.custom_type_ids.contains(&t.0.type_id()) {
                        // There's not much we can do here. The implementer of
                        // the lexer hook made a mistake. Panicking is the best
                        // option.
                        panic!("Custom lexer returned an invalid type.");
                    }
                    t
                })
                .map_break(|(t, r)| (Token::Custom(t), r))?;
        }

        self.ipv6()?;
        self.ipv4()?;
        self.two_char_punctuation()?;
        self.one_char_punctuation()?;
        self.as_number()?;
        self.hex_number()?;
        self.float()?;
        self.integer()?;
        self.f_string()?;
        self.string()?;
        self.char()?;
        self.keyword_or_ident()?;

        ControlFlow::Continue(())
    }

    fn skip_whitespace(&mut self) {
        loop {
            self.input = self.input.trim_start();
            if self.input.as_bytes().first() == Some(&b'#') {
                let n = self.input.find('\n').unwrap_or(self.input.len());
                self.bump(n);
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

            // These are added for better diagnostics
            [b'/', b'/'] => Token::SlashSlash,
            [b'/', b'*'] => Token::SlashStar,
            [b'-', b'-'] => Token::HyphenHyphen,

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
            b'=' => Token::Eq,
            b'|' => Token::Pipe,
            b'-' => Token::Hyphen,
            b':' => Token::Colon,
            b';' => Token::SemiColon,
            b',' => Token::Comma,
            b'.' => Token::Period,
            b'+' => Token::Plus,
            b'*' => Token::Star,
            b'/' => Token::Slash,
            b'!' => Token::Bang,
            b'{' => Token::CurlyLeft,
            b'}' => Token::CurlyRight,
            b'?' => Token::QuestionMark,
            b'[' => Token::SquareLeft,
            b']' => Token::SquareRight,
            b'(' => Token::RoundLeft,
            b')' => Token::RoundRight,
            b'<' => Token::AngleLeft,
            b'>' => Token::AngleRight,
            _ => return ControlFlow::Continue(()),
        };

        let (_, span) = self.bump(1);

        ControlFlow::Break((tok, span))
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
            if let Some(c) = rest.chars().next()
                && (is_xid_start(c) || c == '.' || c == '_')
            {
                return ControlFlow::Continue(());
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

    fn f_string(&mut self) -> ControlFlow<(Token<'s>, Range<usize>)> {
        let Some(_rest) = self.input.strip_prefix("f\"") else {
            return ControlFlow::Continue(());
        };
        let (_tok, span) = self.bump(2);
        ControlFlow::Break((Token::FStringStart, span))
    }

    pub(crate) fn f_string_part(
        &mut self,
    ) -> Option<(FStringToken<'s>, Range<usize>)> {
        let mut chars = self.input.chars().enumerate();
        'outer: while let Some((i, c)) = chars.next() {
            match c {
                '\\' => {
                    let (_, c) = chars.next()?;
                    if c == 'u' || c == 'U' {
                        let (_, c) = chars.next()?;
                        if c != '{' {
                            // We need a `{` after `\u` and `\U`
                            return None;
                        }
                        for (_, c) in chars.by_ref() {
                            if c == '}' {
                                continue 'outer;
                            }
                        }
                        return None;
                    }
                    continue 'outer;
                }
                '{' => {
                    let (i, c) = chars.next()?;
                    if c == '{' {
                        continue 'outer;
                    } else {
                        // We bump to _before_ the curly
                        let (tok, span) = self.bump(i - 1);
                        return Some((
                            FStringToken::StringIntermediate(tok),
                            span,
                        ));
                    }
                }
                '"' => {
                    // Check for the end of the string, which is an unescaped quote
                    let (tok, span) = self.bump(i);
                    // Eat the `"`
                    self.bump(1);
                    return Some((FStringToken::StringEnd(tok), span));
                }
                _ => {}
            }
        }

        // Reached the end of the input, but were still in an f-string!
        None
    }

    fn char(&mut self) -> ControlFlow<(Token<'s>, Range<usize>)> {
        let Some(rest) = self.input.strip_prefix('\'') else {
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
        ControlFlow::Break((Token::Char(tok), span))
    }

    fn string(&mut self) -> ControlFlow<(Token<'s>, Range<usize>)> {
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
        ControlFlow::Break((Token::String(tok), span))
    }

    fn keyword_or_ident(&mut self) -> ControlFlow<(Token<'s>, Range<usize>)> {
        let Some(c) = self.input.chars().next() else {
            return ControlFlow::Continue(());
        };

        if !(is_xid_start(c) || c == '_') {
            return ControlFlow::Continue(());
        }

        let non_ident_idx = self
            .input
            .find(|c: char| !is_xid_continue(c))
            .unwrap_or(self.input.len());

        let (ident, span) = self.bump(non_ident_idx);

        let kw = match ident {
            "accept" => Keyword::Accept,
            "dep" => Keyword::Dep,
            "else" => Keyword::Else,
            "filter" => Keyword::Filter,
            "filtermap" => Keyword::FilterMap,
            "for" => Keyword::For,
            "fn" => Keyword::Fn,
            "if" => Keyword::If,
            "import" => Keyword::Import,
            "in" => Keyword::In,
            "let" => Keyword::Let,
            "match" => Keyword::Match,
            "not" => Keyword::Not,
            "pkg" => Keyword::Pkg,
            "record" => Keyword::Record,
            "reject" => Keyword::Reject,
            "return" => Keyword::Return,
            "std" => Keyword::Std,
            "super" => Keyword::Super,
            "test" => Keyword::Test,
            "variant" => Keyword::Variant,
            "while" => Keyword::While,
            // ----
            "true" => return ControlFlow::Break((Token::Bool(true), span)),
            "false" => return ControlFlow::Break((Token::Bool(false), span)),
            x => {
                self.record_almost_keyword(x, span.clone());
                return ControlFlow::Break((Token::Ident(x), span));
            }
        };
        ControlFlow::Break((Token::Keyword(kw), span))
    }

    fn record_almost_keyword(&mut self, x: &str, span: Range<usize>) {
        let suggestion = match x {
            "loop" => None,
            "enum" => Some("variant"),
            "struct" => Some("record"),
            "class" => Some("record"),
            "data" => Some("record"),
            "use" => Some("import"),
            "switch" => Some("match"),
            "var" => Some("let"),
            "local" => Some("let"),
            "function" => Some("fn"),
            "fun" => Some("fn"),
            "func" => Some("fn"),
            "def" => Some("fn"),
            _ => return,
        };

        let ident = Identifier::from(x);
        self.almost_keyword = Some((ident, span, suggestion));
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
            Token::Bang => "!",
            Token::BangEq => "!=",
            Token::Colon => ":",
            Token::Comma => ",",
            Token::Eq => "=",
            Token::EqEq => "==",
            Token::Hyphen => "-",
            Token::HyphenHyphen => "--",
            Token::Period => ".",
            Token::Pipe => "|",
            Token::PipePipe => "||",
            Token::Plus => "+",
            Token::QuestionMark => "?",
            Token::SemiColon => ";",
            Token::Slash => "/",
            Token::SlashSlash => "//",
            Token::SlashStar => "/*",
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

            Token::Custom(x) => &format!("Custom({x:?})"),
        };

        f.write_str(s)
    }
}

impl Keyword {
    fn as_str(&self) -> &'static str {
        match self {
            Keyword::Accept => "accept",
            Keyword::Dep => "dep",
            Keyword::Else => "else",
            Keyword::Filter => "filter",
            Keyword::FilterMap => "filtermap",
            Keyword::For => "for",
            Keyword::Fn => "fn",
            Keyword::If => "if",
            Keyword::Import => "import",
            Keyword::In => "in",
            Keyword::Let => "let",
            Keyword::Match => "match",
            Keyword::Not => "not",
            Keyword::Pkg => "pkg",
            Keyword::Record => "record",
            Keyword::Reject => "reject",
            Keyword::Return => "return",
            Keyword::Std => "std",
            Keyword::Super => "super",
            Keyword::Test => "test",
            Keyword::Variant => "variant",
            Keyword::While => "while",
        }
    }
}
