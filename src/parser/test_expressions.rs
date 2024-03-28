use crate::parser::Parser;

//------------ Logical Expressions parsing ----------------------------------

#[test]
fn test_logical_expr_1() {
    let r = Parser::run_parser(
        Parser::logical_expr,
        0,
        "( blaffer.waf().contains(my_set) ) || ( blaffer.blaf() < bop() )",
    );
    assert!(r.is_ok());
}

#[test]
fn test_logical_expr_2() {
    let r = Parser::run_parser(
        Parser::logical_expr,
        0,
        r#"(0.0.0.0/0 prefix-length-range /12-/16)"#,
    );
    assert!(r.is_ok());
}

#[test]
fn test_logical_expr_3() {
    let r = Parser::run_parser(
        Parser::logical_expr,
        0,
        r#"blaffer.blaf.contains(something,"somewhat") > blaf()"#,
    );
    assert!(r.is_ok());
}

#[test]
fn test_logical_expr_4() {
    let r = Parser::run_parser(
        Parser::logical_expr,
        0,
        r#"( my_set.contains(bla.bla()) ) || ( my_other_set.contains(bla.bla()) )"#,
    );
    assert!(r.is_ok());
}

#[test]
fn test_logical_expr_5() {
    let r = Parser::run_parser(
        Parser::logical_expr,
        0,
        "(found_prefix.prefix.exists() && found_prefix.prefix.exists()) || route_in_table"
    );
    assert!(r.is_ok());
}

//------------ Compute Expressions parsing ----------------------------------

#[test]
fn test_compute_expr_1() {
    let cm = Parser::run_parser(
        Parser::value_expr,
        0,
        r#"source_asns.contains("asn", route.as_path.origin)"#,
    );
    assert!(cm.is_ok());
}

#[test]
fn test_compute_expr_2() {
    let r =
        Parser::run_parser(Parser::value_expr, 0, "a.b.c.d(x,y,z).e.f(o.p()).g");
    assert!(r.is_ok());
}

#[test]
fn test_compute_expr_3() {
    let r = Parser::run_parser(Parser::value_expr, 0, "send-to(a, b)");
    assert!(r.is_ok());
}

#[test]
fn test_compute_expr_4() {
    let r = Parser::run_parser(Parser::value_expr, 0, "global_record.field");
    assert!(r.is_ok());
}

#[test]
fn test_compute_expr_5() {
    let r = Parser::run_parser(Parser::value_expr, 0, "pph_asn.asn.set(AS200)");
    assert!(r.is_ok());
}

//------------ Other Expressions --------------------------------------------

#[test]
fn test_value_expr() {
    let mm = Parser::run_parser(Parser::value_expr, 0, r###"globlaf(bla)"###);
    assert!(mm.is_ok());
}

//------------ Prefix Match Expressions -------------------------------------

// This SHOULD be syntactic sugar for, but they're not correct right now.
// They're supposed to be boolean expressions, not expressions that yield
// prefixes. TODO

#[test]
fn test_prefix_expr_1() {
    let s = Parser::run_parser(Parser::value_expr, 0, r###"129.23.0.0/16 upto /18"###);
    assert!(s.is_ok());
}

#[test]
fn test_prefix_expr_2() {
    let s = Parser::run_parser(Parser::value_expr, 0, r###"2001::1/48 orlonger"###);
    assert!(s.is_ok());
}

#[test]
fn test_prefix_expr_3() {
    let s = Parser::run_parser(Parser::value_expr,
        0,
        r###"0.0.0.0/0 prefix-length-range /24-/32"###,
    );
    assert!(s.is_ok());
}
