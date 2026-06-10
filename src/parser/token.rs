//! Lexer for Roto scripts

use core::{ops::Range, str};
use std::{collections::VecDeque, fmt::Display, ops::ControlFlow};

use unicode_ident::{is_xid_continue, is_xid_start};

use crate::ast::Identifier;

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

pub struct Lexer<'a> {
    input: &'a str,
    original_length: usize,
    peeked: VecDeque<(Result<Token<'a>, ()>, Range<usize>)>,
    pub almost_keyword:
        Option<(Identifier, Range<usize>, Option<&'static str>)>,
}

impl<'a> Lexer<'a> {
    pub fn next(&mut self) -> Option<(Result<Token<'a>, ()>, Range<usize>)> {
        if let Some(t) = self.peeked.pop_front() {
            return Some(t);
        }
        self.next_inner()
    }

    pub fn peek(&mut self) -> Option<&(Result<Token<'a>, ()>, Range<usize>)> {
        if self.peeked.is_empty()
            && let Some(t) = self.next_inner()
        {
            self.peeked.push_back(t);
        }
        self.peeked.front()
    }

    pub fn peek_many<const N: usize>(&mut self) -> Option<[&Token<'a>; N]> {
        for _ in 0..N - self.peeked.len() {
            if let Some(t) = self.next_inner() {
                self.peeked.push_back(t);
            } else {
                return None;
            }
        }

        // Start with some random tokens that we will override.
        let mut tokens = [const { &Token::AmpAmp }; N];
        for (i, token) in tokens.iter_mut().enumerate() {
            let (Ok(t), _) = self.peeked.get(i).unwrap() else {
                return None;
            };
            *token = t;
        }

        Some(tokens)
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
    pub fn new(input: &'s str) -> Self {
        Self {
            input,
            original_length: input.len(),
            peeked: VecDeque::new(),
            almost_keyword: None,
        }
    }

    fn bump(&mut self, n: usize) -> (&'s str, Range<usize>) {
        let start = self.original_length - self.input.len();
        let (a, b) = self.input.split_at(n);
        self.input = b;
        let end = self.original_length - self.input.len();
        (a, start..end)
    }

    fn bump_to(&mut self, tail: &str) -> (&'s str, Range<usize>) {
        self.bump(self.input.len() - tail.len())
    }

    fn is_empty(&self) -> bool {
        self.input.is_empty()
    }

    fn next_token(&mut self) -> ControlFlow<(Token<'s>, Range<usize>)> {
        self.skip_whitespace();

        if self.is_empty() {
            return ControlFlow::Continue(());
        }

        self.ipv6()?;
        self.ipv4()?;
        self.two_char_punctuation()?;
        self.one_char_punctuation()?;
        self.as_number()?;
        self.hex_number()?;
        self.number()?;
        self.f_string()?;
        self.string()?;
        self.char()?;
        self.keyword_or_ident()?;

        ControlFlow::Continue(())
    }

    pub fn skip_shebang(&mut self) {
        let mut tail = self.input;
        if tail.eat_str("#!")
            && tail.starts_with(|c: char| !c.is_whitespace())
        {
            tail.eat_until('\n');
            self.bump_to(tail);
        }
    }

    fn skip_whitespace(&mut self) {
        let mut tail = self.input;
        loop {
            tail.eat_whitespace();
            if tail.eat_str("//") {
                tail.eat_until('\n');
            } else {
                break;
            }
        }
        if tail.len() < self.input.len() {
            self.bump_to(tail);
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
            [b'=', b'>'] => Token::FatArrow,
            [b'+', b'='] => Token::PlusEq,
            [b'-', b'='] => Token::MinusEq,
            [b'*', b'='] => Token::StarEq,
            [b'/', b'='] => Token::SlashEq,
            [b'%', b'='] => Token::PercentEq,

            // These are added for better diagnostics
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
            b'%' => Token::Percent,
            b'#' => Token::Hash,
            _ => return ControlFlow::Continue(()),
        };

        let (_, span) = self.bump(1);

        ControlFlow::Break((tok, span))
    }

    fn ipv6(&mut self) -> ControlFlow<(Token<'s>, Range<usize>)> {
        let mut tail = self.input;

        let mut count = 0;
        while count < 2 {
            tail.eat_while(char::is_ascii_hexdigit);
            if !tail.eat_char(':') {
                return ControlFlow::Continue(());
            }
            count += 1;
        }

        tail.eat_while(|c| char::is_ascii_hexdigit(c) || *c == ':');

        let (tok, span) = self.bump_to(tail);
        ControlFlow::Break((Token::IpV6(tok), span))
    }

    fn ipv4(&mut self) -> ControlFlow<(Token<'s>, Range<usize>)> {
        let mut tail = self.input;

        let mut count = 0;
        while count < 3 {
            if !tail.eat_while(char::is_ascii_digit) {
                return ControlFlow::Continue(());
            }
            if !tail.eat_char('.') {
                return ControlFlow::Continue(());
            }
            count += 1;
        }

        tail.eat_while(char::is_ascii_digit);

        let (tok, span) = self.bump_to(tail);
        ControlFlow::Break((Token::IpV4(tok), span))
    }

    fn as_number(&mut self) -> ControlFlow<(Token<'s>, Range<usize>)> {
        let mut tail = self.input;

        if !tail.eat_str("AS") {
            return ControlFlow::Continue(());
        };

        let ate_digits = tail.eat_while(char::is_ascii_digit);

        if !ate_digits {
            return ControlFlow::Continue(());
        }

        let (tok, span) = self.bump_to(tail);
        ControlFlow::Break((Token::Asn(tok), span))
    }

    fn hex_number(&mut self) -> ControlFlow<(Token<'s>, Range<usize>)> {
        let mut tail = self.input;

        if !tail.eat_str("0x") {
            return ControlFlow::Continue(());
        };

        tail.eat_while(char::is_ascii_hexdigit);

        let (tok, span) = self.bump_to(tail);
        ControlFlow::Break((Token::Hex(tok), span))
    }

    fn number(&mut self) -> ControlFlow<(Token<'s>, Range<usize>)> {
        let mut tail = self.input;

        // Get the first set of digits. We require at least one digit for both
        // integers and floats.
        let ate_digits = tail.eat_while(char::is_ascii_digit);

        if !ate_digits {
            return ControlFlow::Continue(());
        }

        // The literal is a float if we encounter a '.', 'e' or 'E'.
        let mut is_float = false;

        if tail.starts_with('.') {
            // Edge case: if we have `10..` or `10._hello` or `10.hello` we
            // should treat this as an integer
            if let Some(c) = tail.chars().nth(1)
                && (is_xid_start(c) || c == '.' || c == '_')
            {
                let (tok, span) = self.bump_to(tail);
                return ControlFlow::Break((Token::Integer(tok), span));
            }

            is_float = true;
            tail.eat_char('.');
            tail.eat_while(char::is_ascii_digit);
        }

        if tail.eat_one_of(['e', 'E']) {
            is_float = true;
            tail.eat_one_of(['+', '-']);
            tail.eat_while(char::is_ascii_digit);
        }

        let (tok, span) = self.bump_to(tail);
        let tok = if is_float {
            Token::Float(tok)
        } else {
            Token::Integer(tok)
        };
        ControlFlow::Break((tok, span))
    }

    fn f_string(&mut self) -> ControlFlow<(Token<'s>, Range<usize>)> {
        let mut tail = self.input;

        if !tail.eat_str("f\"") {
            return ControlFlow::Continue(());
        };

        let (_tok, span) = self.bump_to(tail);
        ControlFlow::Break((Token::FStringStart, span))
    }

    pub fn f_string_part(
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
        let mut tail = self.input;

        if !tail.eat_char('\'') {
            return ControlFlow::Continue(());
        };

        let mut last_is_backslash = false;
        tail.eat_until_fn(|c| {
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

        if tail.is_empty() {
            return ControlFlow::Continue(());
        };

        let (tok, span) = self.bump_to(tail);
        ControlFlow::Break((Token::Char(tok), span))
    }

    fn string(&mut self) -> ControlFlow<(Token<'s>, Range<usize>)> {
        let mut tail = self.input;
        if !tail.eat_char('\"') {
            return ControlFlow::Continue(());
        };

        let mut last_is_backslash = false;
        tail.eat_until_fn(|c| {
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

        if tail.is_empty() {
            return ControlFlow::Continue(());
        };

        let (tok, span) = self.bump_to(tail);
        ControlFlow::Break((Token::String(tok), span))
    }

    fn keyword_or_ident(&mut self) -> ControlFlow<(Token<'s>, Range<usize>)> {
        let mut tail = self.input;

        let Some(c) = tail.chars().next() else {
            return ControlFlow::Continue(());
        };

        if !(is_xid_start(c) || c == '_') {
            return ControlFlow::Continue(());
        }

        tail = &tail[1..];
        tail.eat_while(|c: &char| is_xid_continue(*c));

        let (ident, span) = self.bump_to(tail);

        let kw = match ident {
            "accept" => Keyword::Accept,
            "const" => Keyword::Const,
            "dep" => Keyword::Dep,
            "else" => Keyword::Else,
            "enum" => Keyword::Enum,
            "filter" => Keyword::Filter,
            "filtermap" => Keyword::FilterMap,
            "for" => Keyword::For,
            "fn" => Keyword::Fn,
            "if" => Keyword::If,
            "import" => Keyword::Import,
            "in" => Keyword::In,
            "let" => Keyword::Let,
            "match" => Keyword::Match,
            "pkg" => Keyword::Pkg,
            "record" => Keyword::Record,
            "reject" => Keyword::Reject,
            "return" => Keyword::Return,
            "std" => Keyword::Std,
            "super" => Keyword::Super,
            "test" => Keyword::Test,
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

trait StrExt {
    fn eat_char(&mut self, c: char) -> bool;
    fn eat_str(&mut self, s: &str) -> bool;
    fn eat_one_of<const N: usize>(&mut self, options: [char; N]) -> bool;
    fn eat_until(&mut self, c: char);
    fn eat_until_fn(&mut self, c: impl FnMut(&char) -> bool);
    fn eat_while(&mut self, pat: impl FnMut(&char) -> bool) -> bool;
    fn eat_whitespace(&mut self);
}

impl StrExt for &str {
    fn eat_char(&mut self, s: char) -> bool {
        if let Some(new) = self.strip_prefix(s) {
            *self = new;
            true
        } else {
            false
        }
    }

    fn eat_str(&mut self, s: &str) -> bool {
        if let Some(new) = self.strip_prefix(s) {
            *self = new;
            true
        } else {
            false
        }
    }

    fn eat_one_of<const N: usize>(&mut self, options: [char; N]) -> bool {
        if let Some(new) = self.strip_prefix(options) {
            *self = new;
            true
        } else {
            false
        }
    }

    fn eat_until(&mut self, c: char) {
        *self = if let Some((_, tail)) = self.split_once(c) {
            tail
        } else {
            ""
        };
    }

    fn eat_until_fn(&mut self, mut f: impl FnMut(&char) -> bool) {
        *self = if let Some((_, tail)) = self.split_once(|c: char| f(&c)) {
            tail
        } else {
            ""
        };
    }

    fn eat_while(&mut self, mut pat: impl FnMut(&char) -> bool) -> bool {
        let new = self.trim_start_matches(|c| pat(&c));
        if self.len() != new.len() {
            *self = new;
            true
        } else {
            false
        }
    }

    fn eat_whitespace(&mut self) {
        *self = self.trim_start();
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
