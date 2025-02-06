use crate::{ast::Declaration, parser::Parser};

use super::{meta::Spans, ParseResult};

fn parse_function(s: &str) -> ParseResult<Declaration> {
    let mut spans = Spans::default();
    Parser::run_parser(Parser::root, 0, &mut spans, s)
}

#[test]
fn function_1() {
    let s = "
        function myfunction() {
            send-to(a,b);
        }
    ";
    parse_function(s).unwrap();
}

#[test]
fn function_2() {
    let s = "
        function myfunction() {
            send_to(a,b);
            pph_asn.asn.set(AS200);
        }
    ";
    parse_function(s).unwrap();
}

#[test]
fn block_with_if() {
    let s = "
        function myfunction() {
            if true { send_to(a,b); }
            pph_asn.asn.set(AS200);
        }
    ";
    parse_function(s).unwrap();
}

#[test]
fn block_with_if_with_semicolon() {
    let s = "
        function myfunction() {
            if true { send_to(a,b); };
            pph_asn.asn.set(AS200);
        }
    ";
    parse_function(s).unwrap();
}
