use roto::ast::*;

//------------ Logical Expressions parsing ----------------------------------

#[test]
fn test_logical_expr_1() {
    let r = ActionSection::parse(
        r###"
        action my-action {
            send-to(a,b);
        }"###,
    );

    if r.is_err() {
        println!("{:?}", r);
    }
    assert!(r.is_ok());
    if let Ok(expr) = r {
        println!("{:?}", expr.1);
        assert_eq!(expr.0, "");
    }
}
