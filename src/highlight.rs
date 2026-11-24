use std::ops::Range;

use rotoline::highlight::{Style, StyledStr};

use crate::parser::{
    lexer::Lexer,
    token::{FStringToken, Token},
};

pub fn highlight(source: &str) -> StyledStr {
    let mut h = Highlighter {
        segments: Vec::new(),
        lexer: Lexer::new(0, source),
        source,
        last_end: 0,
        delimiter_stack: Vec::new(),
    };

    h.normal();

    if h.last_end < source.len() {
        h.emit(ansi::COMMENT, h.last_end..source.len());
    }

    StyledStr {
        segments: h.segments,
    }
}

#[allow(unused)]
mod ansi {
    pub const BLACK: &str = "\x1b[0;30m";
    pub const RED: &str = "\x1b[0;31m";
    pub const GREEN: &str = "\x1b[0;32m";
    pub const YELLOW: &str = "\x1b[0;33m";
    pub const BLUE: &str = "\x1b[0;34m";
    pub const PURPLE: &str = "\x1b[0;35m";
    pub const CYAN: &str = "\x1b[0;36m";
    pub const WHITE: &str = "\x1b[0;37m";
    pub const GRAY: &str = "\x1b[38;5;248m";
    pub const RESET: &str = "\x1b[0m";
    pub const ITALIC: &str = "\x1b[3m";

    // Gray and italic
    pub const COMMENT: &str = "\x1b[38;5;248m\x1b[3m";
    pub const ERROR: &str = "\x1b[0;41m";
}

struct Highlighter<'a> {
    last_end: usize,
    delimiter_stack: Vec<char>,
    segments: Vec<(Style, String)>,
    source: &'a str,
    lexer: Lexer<'a>,
}

impl Highlighter<'_> {
    fn emit(&mut self, style: &'static str, range: Range<usize>) {
        let s = &self.source[range.start..range.end];
        self.segments.push((Style(style), s.into()));
    }

    fn normal(&mut self) {
        while let Some((tok, span)) = self.lexer.next() {
            if span.start > self.last_end {
                self.emit(ansi::COMMENT, self.last_end..span.start);
            }
            self.last_end = span.end;

            let Ok(tok) = tok else {
                self.emit(ansi::ERROR, span.start..span.end);
                continue;
            };

            let style = match tok {
                Token::Ident(_) => {
                    if let Some((Ok(Token::RoundLeft), _)) = self.lexer.peek()
                    {
                        ansi::YELLOW
                    } else {
                        ansi::WHITE
                    }
                }
                Token::Hash | Token::HyphenHyphen => ansi::RED,
                Token::Colon
                | Token::Comma
                | Token::Period
                | Token::SemiColon => ansi::GRAY,
                Token::CurlyLeft | Token::RoundLeft | Token::SquareLeft => {
                    self.delimiter_stack.push(
                        self.source[span.start..].chars().next().unwrap(),
                    );
                    ansi::GRAY
                }
                Token::RoundRight => delim(&mut self.delimiter_stack, '('),
                Token::SquareRight => delim(&mut self.delimiter_stack, '['),
                Token::CurlyRight => {
                    let style = delim(&mut self.delimiter_stack, '{');
                    self.emit(style, span.start..span.end);
                    if self.delimiter_stack.last() == Some(&'f') {
                        return;
                    } else {
                        continue;
                    }
                }
                Token::AmpAmp
                | Token::QuestionMark
                | Token::Pipe
                | Token::PipePipe
                | Token::FatArrow
                | Token::Eq
                | Token::EqEq
                | Token::AngleLeftEq
                | Token::AngleRightEq
                | Token::Arrow
                | Token::Bang
                | Token::BangEq
                | Token::Hyphen
                | Token::Plus
                | Token::Slash
                | Token::SlashStar
                | Token::Star
                | Token::StarStar
                | Token::Percent
                | Token::PlusEq
                | Token::MinusEq
                | Token::StarEq
                | Token::SlashEq
                | Token::AngleLeft
                | Token::AngleRight
                | Token::PercentEq => ansi::YELLOW,
                Token::Keyword(_) => ansi::BLUE,
                Token::String(_) => ansi::GREEN,
                Token::Char(_) => ansi::GREEN,
                Token::Integer(_, _) => ansi::PURPLE,
                Token::Float(_, _) => ansi::PURPLE,
                Token::Hex(_) => ansi::PURPLE,
                Token::Asn(_) => ansi::PURPLE,
                Token::IpV4(_) => ansi::PURPLE,
                Token::IpV6(_) => ansi::PURPLE,
                Token::Bool(_) => ansi::PURPLE,
                Token::FStringStart => {
                    self.emit(ansi::GREEN, span.start..span.end);
                    self.f_string();
                    continue;
                }
            };

            self.emit(style, span.start..span.end);
        }
    }

    fn f_string(&mut self) {
        while let Some((tok, span)) = self.lexer.f_string_part() {
            if span.start > self.last_end {
                self.emit(ansi::COMMENT, self.last_end..span.start);
            }

            self.last_end = span.end;

            match tok {
                FStringToken::StringEnd(_) => {
                    self.emit(ansi::GREEN, span.start..span.end);
                    return;
                }
                FStringToken::StringIntermediate(_) => {
                    self.emit(ansi::GREEN, span.start..span.end);
                    self.delimiter_stack.push('f');
                    self.normal();
                    self.delimiter_stack.pop_if(|c| *c == 'f');
                }
            }
        }
    }
}

fn delim(delimiter_stack: &mut Vec<char>, d: char) -> &'static str {
    if delimiter_stack.last() == Some(&d) {
        delimiter_stack.pop();
        ansi::GRAY
    } else if let Some(idx) = delimiter_stack.iter().rposition(|c| *c == d) {
        delimiter_stack.truncate(idx);
        ansi::ERROR
    } else {
        ansi::ERROR
    }
}
