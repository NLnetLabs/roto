use core::{ops::Range, str};
use std::{fmt::Display, ops::ControlFlow};

use icu::properties::sets::CodePointSetDataBorrowed;

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
    Period,
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
    Accept,
    Dep,
    Else,
    Filter,
    FilterMap,
    Function,
    If,
    Import,
    In,
    Let,
    Match,
    Not,
    Pkg,
    Reject,
    Return,
    Std,
    Super,
    Test,
    Type,

    // === Literals ===
    String(&'s str),
    Integer(&'s str),
    Hex(&'s str),
    Asn(&'s str),
    IpV4(&'s str),
    IpV6(&'s str),
    Bool(bool),
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
        self.numeric()?;
        self.string()?;
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
            .find(|c: char| !c.is_ascii_digit())
            .unwrap_or(rest.len());

        let (tok, span) = self.bump(2 + digit_idx);
        ControlFlow::Break((Token::Hex(tok), span))
    }

    fn numeric(&mut self) -> ControlFlow<(Token<'s>, Range<usize>)> {
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

        if !(XID_START.contains(c) || c == '_') {
            return ControlFlow::Continue(());
        }

        let non_ident_idx = self
            .input
            .find(|c: char| !XID_CONTINUE.contains(c))
            .unwrap_or(self.input.len());

        let (ident, span) = self.bump(non_ident_idx);
        let tok = match ident {
            "accept" => Token::Accept,
            "else" => Token::Else,
            "false" => Token::Bool(false),
            "filter" => Token::Filter,
            "filtermap" => Token::FilterMap,
            "function" => Token::Function,
            "if" => Token::If,
            "import" => Token::Import,
            "in" => Token::In,
            "let" => Token::Let,
            "dep" => Token::Dep,
            "match" => Token::Match,
            "not" => Token::Not,
            "pkg" => Token::Pkg,
            "reject" => Token::Reject,
            "return" => Token::Return,
            "std" => Token::Std,
            "super" => Token::Super,
            "test" => Token::Test,
            "true" => Token::Bool(true),
            "type" => Token::Type,
            x => Token::Ident(x),
        };
        ControlFlow::Break((tok, span))
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
            Token::Period => ".",
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

            // Keywords
            Token::Accept => "accept",
            Token::Else => "else",
            Token::Filter => "filter",
            Token::FilterMap => "filtermap",
            Token::Function => "function",
            Token::If => "if",
            Token::Import => "import",
            Token::In => "in",
            Token::Let => "let",
            Token::Dep => "dep",
            Token::Match => "match",
            Token::Not => "not",
            Token::Pkg => "pkg",
            Token::Reject => "reject",
            Token::Return => "return",
            Token::Std => "std",
            Token::Super => "super",
            Token::Test => "test",
            Token::Type => "type",

            // Literals
            Token::String(s) => s,
            Token::Integer(s) => s,
            Token::Hex(s) => s,
            Token::Asn(s) => s,
            Token::IpV4(s) => s,
            Token::IpV6(s) => s,
            Token::Bool(true) => "true",
            Token::Bool(false) => "false",
        };
        write!(f, "{s}")
    }
}
