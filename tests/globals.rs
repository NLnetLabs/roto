mod helpers;
mod common;

//------------ RIB parse ----------------------------------------------------

#[test]
fn test_rib_invalid_1() {
    common::init();

    let compiler = helpers::TestCompiler::create(
        "test_rib_invalid_1",
        r###"
            rib my_rib contains Blaffer { 
                bla: Bla, blow_up 
            }
            // comment
            "###,
    );

    compiler.test_parse(false);
}

#[test]
fn test_rib_invalid_2() {
    common::init();

    let compiler = helpers::TestCompiler::create(
        "invalid-rib-with-comment-2",
        r###"
            rib my_rib contains Blaffer { 
                bla: Bla; blow: up
            }
            // comment
            "###
    );

    compiler.test_parse(false);
}

#[test]
fn test_rib_without_name_1() {
    common::init();
    
    let compiler = helpers::TestCompiler::create(
        "rib-without-a-name",
        r###"
        // comment
        rib {}
    "###);
    compiler.test_parse(false);
}

#[test]
fn test_empty() {
    common::init();
    
    let compiler = helpers::TestCompiler::create(
        "invalid-rib-without-name-2",
        r#"
            // some comment
            // bl alba  bcomment"
        "#
    );

    compiler.test_parse(false);
}

#[test]
fn test_interspersed_comments() {
    common::init();
    
    let compiler = helpers::TestCompiler::create(
        "interspersed-comments",
        r###"
        rib my_rib contains SomeCrap { prefix: Prefix, as-path: AsPath }
        // comment
        rib unrib contains Blaffer { _ip: IpAddress }
        "###,
    );

    let _p = compiler.test_parse(true);
    let _e = _p.test_eval(true);
}


#[test]
fn test_stream_1() {
    common::init();
    
    let compiler = helpers::TestCompiler::create(
        "stream_1",
        r###"
        output-stream mqtt contains Message {
            message: String,
            asn: Asn
        }
        "###,
    );

    let _p = compiler.test_parse(true);
    let _e = _p.test_eval(true);
}
