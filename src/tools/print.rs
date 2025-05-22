use crate::parser::token::{Lexer, TokenClassification};

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

pub fn print_highlighted(s: &str) {
    let lexer = Lexer::new(s);
    let mut last_end = 0;
    for (token, range) in lexer {
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
        let color = match token.classify() {
            TokenClassification::Variable => ansi::WHITE,
            TokenClassification::Punctuation => ansi::GRAY,
            TokenClassification::Keyword => ansi::BLUE,
            TokenClassification::Number => ansi::PURPLE,
            TokenClassification::String => ansi::GREEN,
        };
        print!("{color}{}{}", &s[range.start..range.end], ansi::RESET);
    }
}
