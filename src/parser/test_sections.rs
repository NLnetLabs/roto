use crate::{ast::ActionDeclaration, parser::Parser};

use super::{meta::Spans, ParseResult};

fn parse_action(s: &str) -> ParseResult<ActionDeclaration> {
    let mut spans = Spans::new();
    Parser::run_parser(Parser::action, 0, &mut spans, s)
}

#[test]
fn test_logical_expr_1() {
    let s = "
        action my-action() {
            send-to(a,b);
        }
    ";
    parse_action(s).unwrap();
}

#[test]
fn test_logical_expr_2() {
    let s = "
        action my-action() {
            send_to(a,b);
            pph_asn.asn.set(AS200);
        }
    ";
    parse_action(s).unwrap();
}
