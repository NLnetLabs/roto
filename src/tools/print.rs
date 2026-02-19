use crate::parser::token::{Lexer, Token};

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
}

/// Print highlighted Roto code to the terminal
///
/// The syntax highlighting is simple and based solely on the lexer.
pub fn print_highlighted(s: &str) {
    let mut lexer = Lexer::new(s);
    let mut last_end = 0;
    for (token, range) in std::iter::from_fn(|| lexer.next()) {
        if range.start > last_end {
            print!(
                "{}{}{}{}",
                ansi::GRAY,
                ansi::ITALIC,
                &s[last_end..range.start],
                ansi::RESET
            );
        }
        last_end = range.end;
        let token = token.unwrap();
        let color = match token {
            Token::Ident(_) => ansi::WHITE,
            Token::AmpAmp
            | Token::AngleLeftEq
            | Token::AngleRightEq
            | Token::Arrow
            | Token::Bang
            | Token::BangEq
            | Token::Colon
            | Token::Comma
            | Token::Eq
            | Token::EqEq
            | Token::Hyphen
            | Token::HyphenHyphen
            | Token::Period
            | Token::Pipe
            | Token::PipePipe
            | Token::Plus
            | Token::QuestionMark
            | Token::SemiColon
            | Token::Slash
            | Token::SlashSlash
            | Token::SlashStar
            | Token::Star
            | Token::AngleLeft
            | Token::AngleRight
            | Token::CurlyLeft
            | Token::CurlyRight
            | Token::RoundLeft
            | Token::RoundRight
            | Token::SquareLeft
            | Token::SquareRight 
            | Token::Percent => ansi::GRAY,
            Token::Keyword(_) => ansi::BLUE,
            Token::String(_) => ansi::GREEN,
            Token::Char(_) => ansi::GREEN,
            Token::Integer(_) => ansi::PURPLE,
            Token::Float(_) => ansi::PURPLE,
            Token::Hex(_) => ansi::PURPLE,
            Token::Asn(_) => ansi::PURPLE,
            Token::IpV4(_) => ansi::PURPLE,
            Token::IpV6(_) => ansi::PURPLE,
            Token::Bool(_) => ansi::PURPLE,
            Token::FStringStart => todo!(),
        };
        print!("{color}{}{}", &s[range.start..range.end], ansi::RESET);
    }
}
