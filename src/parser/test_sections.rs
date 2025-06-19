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

#[test]
fn top_level_import() {
    let s = "
        import foo.bar;

        function myfunction() {
            1 + 1;
        }
    ";
    parse(s).unwrap();
}

#[test]
fn block_import_1() {
    let s = "
        function myfunction() {
            import foo.bar;
            1 + 1;
        }
    ";
    parse(s).unwrap();
}

#[test]
fn block_import_2() {
    let s = "
        function myfunction() {
            1 + 1;
            import foo.bar;
        }
    ";
    parse(s).unwrap();
}

#[test]
fn list_import() {
    let s = "
        function myfunction() {
            import foo.{bar1, bar2};
            1 + 1;
        }
    ";

    parse(s).unwrap();
}

#[test]
fn nested_list_import() {
    let s = "
        function myfunction() {
            import foo.{baz.{fn1, fn2, fn3}, baz.{bar1, bar2}};
            1 + 1;
        }
    ";

    parse(s).unwrap();
}

#[test]
fn sublists_only_import() {
    let s = "
        function myfunction() {
            import foo.{{fn1, fn2, fn3}, {bar1, bar2}};
            1 + 1;
        }
    ";

    parse(s).unwrap();
}

#[test]
fn sublist_sublist_import() {
    let s = "
        function myfunction() {
            import foo.{item1.{fn1, fn2}, item2.{item3.fn3, item4.{fn5, fn6}}};
            1 + 1;
        }
    ";

    parse(s).unwrap();
}
