use crate::{ast::Expr, parser::Parser};

use super::{
    meta::{Meta, Spans},
    ParseResult,
};

fn parse_expr(s: &str) -> ParseResult<Meta<Expr>> {
    let mut spans = Spans::new();
    Parser::run_parser(Parser::expr, 0, &mut spans, s)
}

#[test]
fn test_logical_expr_1() {
    let s =
        "( blaffer.waf().contains(my_set) ) || ( blaffer.blaf() < bop() )";
    parse_expr(s).unwrap();
}

#[test]
fn test_logical_expr_2() {
    let s = r#"(0.0.0.0/0 prefix-length-range /12-/16)"#;
    parse_expr(s).unwrap();
}

#[test]
fn test_logical_expr_3() {
    let s = r#"blaffer.blaf.contains(something,"somewhat") > blaf()"#;
    parse_expr(s).unwrap();
}

#[test]
fn test_logical_expr_4() {
    let s = r#"( my_set.contains(bla.bla()) ) || ( my_other_set.contains(bla.bla()) )"#;
    parse_expr(s).unwrap();
}

#[test]
fn test_logical_expr_5() {
    let s ="(found_prefix.prefix.exists() && found_prefix.prefix.exists()) || route_in_table";
    parse_expr(s).unwrap();
}

//------------ Compute Expressions parsing ----------------------------------

#[test]
fn test_compute_expr_1() {
    let s = r#"source_asns.contains("asn", route.as_path.origin)"#;
    parse_expr(s).unwrap();
}

#[test]
fn test_compute_expr_2() {
    let s = "a.b.c.d(x,y,z).e.f(o.p()).g";
    parse_expr(s).unwrap();
}

#[test]
fn test_compute_expr_3() {
    let s = "send-to(a, b)";
    parse_expr(s).unwrap();
}

#[test]
fn test_compute_expr_4() {
    let s = "global_record.field";
    parse_expr(s).unwrap();
}

#[test]
fn test_compute_expr_5() {
    let s = "pph_asn.asn.set(AS200)";
    parse_expr(s).unwrap();
}

//------------ Other Expressions --------------------------------------------

#[test]
fn test_value_expr() {
    let s = "globlaf(bla)";
    parse_expr(s).unwrap();
}

//------------ Prefix Match Expressions -------------------------------------

// This SHOULD be syntactic sugar for, but they're not correct right now.
// They're supposed to be boolean expressions, not expressions that yield
// prefixes. TODO

#[test]
fn test_prefix_expr_1() {
    let s = "129.23.0.0/16 upto /18";
    parse_expr(s).unwrap();
}

#[test]
fn test_prefix_expr_2() {
    let s = r"2001::1/48 orlonger";
    parse_expr(s).unwrap();
}

#[test]
fn test_prefix_expr_3() {
    let s = r"0.0.0.0/0 prefix-length-range /24-/32";
    parse_expr(s).unwrap();
}

#[test]
fn test_match() {
    let s = "
        match x {
            A(x) -> b(),
            C(y) -> d(),
        }
    ";
    parse_expr(s).unwrap();
}

#[test]
fn test_match_block() {
    let s= "
        match x {
            A(x) -> {
                a == b;
                a && b;
            }
            C(y) -> d(),
        }
    ";
    parse_expr(s).unwrap();
}

#[test]
fn test_and_and_and() {
    let s = "a && b && c && d";
    parse_expr(s).unwrap();
}

#[test]
fn test_or_or_or() {
    let s = "a || b || c || d";
    parse_expr(s).unwrap();
}

#[test]
fn test_and_or_and() {
    let s = "a && b || c && d";
    parse_expr(s).unwrap_err();
}

#[test]
fn test_if() {
    let s = "if true { 0 }";
    parse_expr(s).unwrap();
}

#[test]
fn test_if_else() {
    let s = "if true { 0 } else { 1 }";
    parse_expr(s).unwrap();
}

#[test]
fn test_if_else_if_else() {
    let s = "if true { 0 } else if false { 1 } else { 2 }";
    parse_expr(s).unwrap();
}
