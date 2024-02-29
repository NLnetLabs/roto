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
    assert!(r.is_ok());
}

#[test]
fn test_logical_expr_1() {
    let r = ActionSection::parse(
        r###"
                action my-action {
                    send_to(a,b);
                    pph_asn.asn.set(AS200);
                }"###,
    );
    assert!(r.is_ok());
}
