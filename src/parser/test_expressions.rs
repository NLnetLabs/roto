use crate::parser::Parser;

//------------ Logical Expressions parsing ----------------------------------

#[test]
fn test_logical_expr_1() {
    let r = Parser::run_parser(
        Parser::expr,
        0,
        "( blaffer.waf().contains(my_set) ) || ( blaffer.blaf() < bop() )",
    );
    assert!(r.is_ok());
}

#[test]
fn test_logical_expr_2() {
    Parser::run_parser(
        Parser::expr,
        0,
        r#"(0.0.0.0/0 prefix-length-range /12-/16)"#,
    )
    .unwrap();
}

#[test]
fn test_logical_expr_3() {
    Parser::run_parser(
        Parser::expr,
        0,
        r#"blaffer.blaf.contains(something,"somewhat") > blaf()"#,
    )
    .unwrap();
}

#[test]
fn test_logical_expr_4() {
    Parser::run_parser(
        Parser::expr,
        0,
        r#"( my_set.contains(bla.bla()) ) || ( my_other_set.contains(bla.bla()) )"#,
    ).unwrap();
}

#[test]
fn test_logical_expr_5() {
    Parser::run_parser(
        Parser::expr,
        0,
        "(found_prefix.prefix.exists() && found_prefix.prefix.exists()) || route_in_table"
    ).unwrap();
}

//------------ Compute Expressions parsing ----------------------------------

#[test]
fn test_compute_expr_1() {
    Parser::run_parser(
        Parser::expr,
        0,
        r#"source_asns.contains("asn", route.as_path.origin)"#,
    )
    .unwrap();
}

#[test]
fn test_compute_expr_2() {
    Parser::run_parser(Parser::expr, 0, "a.b.c.d(x,y,z).e.f(o.p()).g")
        .unwrap();
}

#[test]
fn test_compute_expr_3() {
    Parser::run_parser(Parser::expr, 0, "send-to(a, b)").unwrap();
}

#[test]
fn test_compute_expr_4() {
    Parser::run_parser(Parser::expr, 0, "global_record.field").unwrap();
}

#[test]
fn test_compute_expr_5() {
    Parser::run_parser(Parser::expr, 0, "pph_asn.asn.set(AS200)").unwrap();
}

//------------ Other Expressions --------------------------------------------

#[test]
fn test_value_expr() {
    Parser::run_parser(Parser::expr, 0, r###"globlaf(bla)"###).unwrap();
}

//------------ Prefix Match Expressions -------------------------------------

// This SHOULD be syntactic sugar for, but they're not correct right now.
// They're supposed to be boolean expressions, not expressions that yield
// prefixes. TODO

#[test]
fn test_prefix_expr_1() {
    Parser::run_parser(Parser::expr, 0, r###"129.23.0.0/16 upto /18"###)
        .unwrap();
}

#[test]
fn test_prefix_expr_2() {
    Parser::run_parser(Parser::expr, 0, r###"2001::1/48 orlonger"###)
        .unwrap();
}

#[test]
fn test_prefix_expr_3() {
    Parser::run_parser(
        Parser::expr,
        0,
        r###"0.0.0.0/0 prefix-length-range /24-/32"###,
    )
    .unwrap();
}

#[test]
fn test_match() {
    Parser::run_parser(
        Parser::expr,
        0,
        "match x {
            A(x) -> b(),
            C(y) -> d(),
        }",
    )
    .unwrap();
}

#[test]
fn test_match_block() {
    Parser::run_parser(
        Parser::expr,
        0,
        "match x {
            A(x) -> {
                a == b;
                a && b;
            }
            C(y) -> d(),
        }",
    )
    .unwrap();
}

#[test]
fn test_and_and_and() {
    Parser::run_parser(Parser::expr, 0, "a && b && c && d").unwrap();
}

#[test]
fn test_or_or_or() {
    Parser::run_parser(Parser::expr, 0, "a || b || c || d").unwrap();
}

#[test]
fn test_and_or_and() {
    Parser::run_parser(Parser::expr, 0, "a && b || c && d").unwrap_err();
}
