use crate::{ast::Expr, parser::Parser};

use super::{
    meta::{Meta, Spans},
    ParseResult,
};

fn parse_expr(s: &str) -> ParseResult<Meta<Expr>> {
    let mut spans = Spans::default();
    Parser::run_parser(Parser::expr, 0, &mut spans, s)
}

#[test]
fn assign_expr_1() {
    let s = "a = 4";
    parse_expr(s).unwrap();
}

#[test]
fn assign_expr_2() {
    let s = "a + 3 = 4";
    parse_expr(s).unwrap_err();
}

#[test]
fn assign_expr_3() {
    let s = "a = 4 + 3";
    parse_expr(s).unwrap();
}

#[test]
fn test_logical_expr_1() {
    let s =
        "( blaffer.waf().contains(my_set) ) || ( blaffer.blaf() < bop() )";
    parse_expr(s).unwrap();
}

#[test]
fn test_logical_expr_2() {
    let s = r#"blaffer.blaf.contains(something,"somewhat") > blaf()"#;
    parse_expr(s).unwrap();
}

#[test]
fn test_logical_expr_3() {
    let s = r#"( my_set.contains(bla.bla()) ) || ( my_other_set.contains(bla.bla()) )"#;
    parse_expr(s).unwrap();
}

#[test]
fn test_logical_expr_4() {
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
    let s = "
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

#[test]
fn test_not_true() {
    let s = "not true";
    parse_expr(s).unwrap();
}

#[test]
fn test_not_true_is_true() {
    let s = "not true == true";
    parse_expr(s).unwrap();
}

#[test]
fn hex_number() {
    let s = "0xffff029";
    parse_expr(s).unwrap();
}

#[test]
fn match_without_trailing_comma() {
    let s = "
      match s {
          Foo -> 10,
          Bar -> 20
      }
    ";
    parse_expr(s).unwrap();
}

#[test]
fn match_with_trailing_comma() {
    let s = "
      match s {
          Foo -> 10,
          Bar -> 20,
      }
    ";
    parse_expr(s).unwrap();
}

#[test]
fn match_with_braces_and_commas() {
    let s = "
      match s {
          Foo -> { 10 },
          Bar -> { 20 },
      }
    ";
    parse_expr(s).unwrap();
}
#[test]
fn match_with_braces_without_commas() {
    let s = "
      match s {
          Foo -> { 10 }
          Bar -> { 20 }
      }
    ";
    parse_expr(s).unwrap();
}
