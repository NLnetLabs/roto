use crate::{ast::Declaration, parser::Parser};

use super::{meta::Spans, ParseResult};

fn parse_action(s: &str) -> ParseResult<Declaration> {
    let mut spans = Spans::default();
    Parser::run_parser(Parser::root, 0, &mut spans, s)
}

#[test]
fn action_section_1() {
    let s = "
        action my-action() {
            send-to(a,b);
        }
    ";
    parse_action(s).unwrap();
}

#[test]
fn action_section_2() {
    let s = "
        action my-action() {
            send_to(a,b);
            pph_asn.asn.set(AS200);
        }
    ";
    parse_action(s).unwrap();
}

#[test]
fn block_with_if() {
    let s = "
        action my-action() {
            if true { send_to(a,b); }
            pph_asn.asn.set(AS200);
        }
    ";
    parse_action(s).unwrap();
}

#[test]
fn block_with_if_with_semicolon() {
    let s = "
        action my-action() {
            if true { send_to(a,b); };
            pph_asn.asn.set(AS200);
        }
    ";
    parse_action(s).unwrap();
}
