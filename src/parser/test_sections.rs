use crate::{
    ast::{Declaration, SyntaxTree},
    parser::Parser,
};

use super::{meta::Spans, ParseResult};

fn parse_function(s: &str) -> ParseResult<Declaration> {
    let mut spans = Spans::default();
    Parser::run_parser(Parser::root, 0, &mut spans, s)
}

fn parse(s: &str) -> ParseResult<SyntaxTree> {
    let mut spans = Spans::default();
    Parser::run_parser(Parser::tree, 0, &mut spans, s)
}

#[test]
fn function_1() {
    let s = "
        fn myfunction() {
            send-to(a,b);
        }
    ";
    parse_function(s).unwrap();
}

#[test]
fn function_2() {
    let s = "
        fn myfunction() {
            send_to(a,b);
            pph_asn.asn.set(AS200);
        }
    ";
    parse_function(s).unwrap();
}

#[test]
fn block_with_if() {
    let s = "
        fn myfunction() {
            if true { send_to(a,b); }
            pph_asn.asn.set(AS200);
        }
    ";
    parse_function(s).unwrap();
}

#[test]
fn block_with_if_with_semicolon() {
    let s = "
        fn myfunction() {
            if true { send_to(a,b); };
            pph_asn.asn.set(AS200);
        }
    ";
    parse_function(s).unwrap();
}

#[test]
fn top_level_import() {
    let s = "
        import foo.bar;

        fn myfunction() {
            1 + 1;
        }
    ";
    parse(s).unwrap();
}

#[test]
fn block_import_1() {
    let s = "
        fn myfunction() {
            import foo.bar;
            1 + 1;
        }
    ";
    parse(s).unwrap();
}

#[test]
fn block_import_2() {
    let s = "
        fn myfunction() {
            1 + 1;
            import foo.bar;
        }
    ";
    parse(s).unwrap();
}
