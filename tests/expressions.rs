use roto::ast::*;

//------------ Logical Expressions parsing ----------------------------------

#[test]
fn test_logical_expr_1() {
    let r = LogicalExpr::parse(
        "( blaffer.waf().contains(my_set) ) || ( blaffer.blaf() < bop() )",
    );
    assert!(r.is_ok());
    if let Ok(expr) = r {
        assert_eq!(expr.0, "");
    }
}

#[test]
fn test_logical_expr_2() {
    let r = LogicalExpr::parse(
        r#"(0.0.0.0/0 prefix-length-range /12-/16)"#
    );
    println!("{:#?}", r);
    assert!(r.is_ok());
    if let Ok(expr) = r {
        assert_eq!(expr.0, "");
    }
}

#[test]
fn test_logical_expr_3() {
    let r = LogicalExpr::parse(
        r###"blaffer.blaf.contains(something,"somewhat") > blaf()"###,
    );
    println!("{:#?}", r);
    assert!(r.is_ok());
        if let Ok(expr) = r {
        assert_eq!(expr.0, "");
    }
}

#[test]
fn test_logical_expr_4() {
    let r = LogicalExpr::parse(
        r###"( my_set.contains(bla.bla()) ) || ( my_other_set.contains(bla.bla()) )"###,
    );
    println!("{:#?}", r);
    assert!(r.is_ok());
    if let Ok(expr) = r {
        assert_eq!(expr.0, "");
    }
}

#[test]
fn test_logical_expr_5() {
    let r = LogicalExpr::parse(
        "(found_prefix.prefix.exists() && found_prefix.prefix.exists()) || route_in_table"
    );
    println!("{:#?}", r);
    assert!(r.is_ok());
    if let Ok(expr) = r {
        assert_eq!(expr.0, "");
    }
}

//------------ Compute Expressions parsing ----------------------------------

#[test]
fn test_compute_expr_1() {
    let cm = ComputeExpr::parse(
        r###"source_asns.contains("asn", route.as_path.origin)"###,
    );

    println!("{}, {:#?}", cm.is_ok(), cm);
    assert!(cm.is_ok());
    if let Ok(expr) = cm {
        assert_eq!(expr.0, "");
    }
}

#[test]
fn test_compute_expr_2() {
    let r = ComputeExpr::parse("a.b.c.d(x,y,z).e.f(o.p()).g");
    assert!(r.is_ok());
    if let Ok(expr) = r {
        assert_eq!(expr.0, "");
    }
}

#[test]
fn test_compute_expr_3() {
    let r = OptionalGlobalComputeExpr::parse("send-to(a, b)");
    assert!(r.is_ok());
    if let Ok(expr) = r {
        assert_eq!(expr.0, "");
    }
}

#[test]
fn test_compute_expr_4() {
    let r = OptionalGlobalComputeExpr::parse("global_record.field");
    assert!(r.is_ok());
    if let Ok(expr) = r {
        assert_eq!(expr.0, "");
    }
}

#[test]
fn test_compute_expr_5() {
    let r = OptionalGlobalComputeExpr::parse("pph_asn.asn.set(AS200)");
    assert!(r.is_ok());
    if let Ok(expr) = r {
        assert_eq!(expr.0, "");
    }
}

#[test]
fn test_action_body() {
    let r = ActionSectionBody::parse(r###"
        send_to(a,b);
        pph_asn.asn.set(AS200);"###);
    assert!(r.is_ok());
    if let Ok(expr) = r {
        assert_eq!(expr.0, "");
    }
}

//------------ Other Expressions --------------------------------------------

#[test]
fn test_value_expr() {
    let mm = ValueExpr::parse(r###"globlaf(bla)"###);
    println!("{}, {:#?}", mm.is_ok(), mm);
    assert!(mm.is_ok());
    if let Ok(expr) = mm {
        assert_eq!(expr.0, "");
    }
}

#[test]
fn test_bytestring_literal_expr() {
    let r = ByteStringLiteral::parse("0xZZZZ_AE9");
    println!("{:#?}", r);
    assert!(r.is_err());
    if let Ok(expr) = r {
        assert_eq!(expr.0, "");
    }
}

//------------ Prefix Match Expressions -------------------------------------

// This SHOULD be syntactic sugar for, but they're not correct right now.
// They're supposed to be boolean expressions, not expressions that yield
// prefixes. TODO

#[test]
fn test_prefix_expr_1() {
    let s = PrefixMatchExpr::parse(
        r###"129.23.0.0/16 upto /18"###,
    );
    println!("{:#?}", s);
    assert!(s.is_ok());
    if let Ok(expr) = s {
        assert_eq!(expr.0, "");
    }
}

#[test]
fn test_prefix_expr_2() {
    let s = PrefixMatchExpr::parse(
        r###"2001::1/48 orlonger"###,
    );
    println!("{:#?}", s);
    assert!(s.is_ok());
    if let Ok(expr) = s {
        assert_eq!(expr.0, "");
    }
}

#[test]
fn test_prefix_expr_3() {
    let s = PrefixMatchExpr::parse(
        r###"0.0.0.0/0 prefix-length-range /24-/32"###,
    );
    println!("{:#?}", s);
    assert!(s.is_ok());
        if let Ok(expr) = s {
        assert_eq!(expr.0, "");
    }
}
