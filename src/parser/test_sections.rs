use string_interner::StringInterner;

use crate::{ast::Declaration, parser::Parser};

use super::{meta::Spans, ParseResult};

fn parse_function(s: &str) -> ParseResult<Declaration> {
    let mut spans = Spans::default();
    let mut identifiers = StringInterner::default();
    Parser::run_parser(Parser::root, 0, &mut identifiers, &mut spans, s)
}

#[test]
fn function_1() {
    let s = "
        function my-function() {
            send-to(a,b);
        }
    ";
    parse_function(s).unwrap();
}

#[test]
fn function_2() {
    let s = "
        function my-function() {
            send_to(a,b);
            pph_asn.asn.set(AS200);
        }
    ";
    parse_function(s).unwrap();
}

#[test]
fn block_with_if() {
    let s = "
        function my-function() {
            if true { send_to(a,b); }
            pph_asn.asn.set(AS200);
        }
    ";
    parse_function(s).unwrap();
}

#[test]
fn block_with_if_with_semicolon() {
    let s = "
        function my-function() {
            if true { send_to(a,b); };
            pph_asn.asn.set(AS200);
        }
    ";
    parse_function(s).unwrap();
}
