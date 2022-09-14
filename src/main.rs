use nom::error::convert_error;
use roto::ast::*;

fn test_data(name: &str, data: &str, expect_success: bool) {
    println!("test {}", name);
    let parsed_data = Root::parse_str(data);
    println!("{} {:#?}", name, parsed_data);
    if let Err(e) = parsed_data.clone() {
        println!("{}", convert_error(data, e));
    }

    match expect_success {
        false => assert!(parsed_data.is_err()),
        true => assert!(parsed_data.is_ok()),
    }
}

fn main() {
    let r = ByteStringLiteral::parse(
 "0xZZZZ_AE9",
    );
    println!("{:#?}", r);
    assert!(r.is_ok());

    let mut r = MatchExpr::parse(
        "( blaffer.waf() in my_set ) || ( blaffer.blaf() < bop() )",
    );
    assert!(r.is_ok());

    panic!("STOP!");

    r = MatchExpr::parse(
        r###"
        route.prefix in 0.0.0.0/0 prefix-length-range /12-/16;
    "###,
    );
    println!("{:#?}", r);
    assert!(r.is_ok());

    r = MatchExpr::parse(
        r###"
        // blaffer.blaf.contains(something,"somewhat") > blaf();
        ( bla.bla() in my_set ) || ( bla.bla() in my_other_set );
    "###,
    );
    println!("{:#?}", r);
    assert!(r.is_ok());

    // let r = Module::parse(r###"
    //     module my_module for rib-in with bla: Blaffer {
    //         define { use bla; }
    //         term filter2 {
    //             use bla;
    //             match { blaffer.blaf.contains(something); }
    //         }
    //     }
    // "###);
    // println!("{:#?}", r);
    // assert!(r.is_ok());

    let s = PrefixMatchExpr::parse(
        r###"
        129.23.0.0/16 upto /18;
    "###,
    );
    println!("{:#?}", s);
    assert!(s.is_ok());

    let s = PrefixMatchExpr::parse(
        r###"
        2001::1/48 orlonger;
    "###,
    );
    println!("{:#?}", s);
    assert!(s.is_ok());

    let s = PrefixMatchExpr::parse(
        r###"
        0.0.0.0/0 prefix-length-range /24-/32;
    "###,
    );
    println!("{:#?}", s);
    assert!(s.is_ok());

    test_data("random-crap", "##@#@#kdflfk!  abc  \n  ", false);

    test_data("empty-rib", "rib my_rib {}\n// comment\n", false);

    test_data("crab-rib", "rib [] ribribribmodule\n", false);

    test_data(
        "valid-rib-with-comment",
        r###"
            rib my_rib3 contains Bla { 
                bla: Bla, 
                bloo: Bloo 
            }
            // comment
            "###,
        true,
    );

    test_data(
        "invalid-rib-with-comment-1",
        r###"
            rib my_rib contains Blaffer { 
                bla: Bla, blow_up 
            }
            // comment
            "###,
        false,
    );

    test_data(
        "invalid-rib-with-comment-2",
        r###"
            rib my_rib contains Blaffer { 
                bla: Bla; blow: up
            }
            // comment
            "###,
        false,
    );

    test_data("rib-without-a-name", "// comment\nrib {}\n", false);

    test_data(
        "comments-only",
        "// some comment\n// bl alba  bcomment\n",
        false,
    );

    test_data(
        "interspersed-comments",
        r###"
        rib my_rib contains SomeCrap { bla: Bla, bloo: Bloo }
        // comment
        rib unrib contains Blaffer { blaffer: Blaf }
        "###,
        true,
    );

    test_data(
        "module_1",
        r###"
        module my_module for rib-loc with bla: Blaffer { 
            define my_def { 
                use bla;
            }
            term my_term {
                match { bazooka; }
            }
        }

        // comment
        rib unrib contains Blaffer { blaffer: Blaf }
        "###,
        true,
    );

    test_data(
        "module-more-with",
        r###"
        module my_module for rib-in with bla: Blaffer {
            define { use bla; }
            term filter2 {
                use bla;
                match { blaffer.blaf.contains(something); }
            }
        }
            
        // comment
        rib unrib contains Blaffer { blaffer: Blaf }
        "###,
        true,
    );

    test_data(
        "module_2",
        r###"
            module my_module for rib-in with bla: Blaffer {
               define { use bla; }
               term filter3 {}
            }
            // comment
            rib unrib contains Blaffer { blaffer: Blaf }
        "###,
        true,
    );

    test_data(
        "module_with_assignments_1",
        r###"
            module my_module for rib-in with bla: Blaffer {
               define {
                   use bla;
                   bla = blaf(Bla);
               }
            
               term blaffer_filter {
                   use bla;
                   match { blaffer.blaf.contains(something); }
               }
            }

            // comment
            rib unrib contains Blaffer { blaffer: Blaf }
        "###,
        true,
    );

    test_data(
        "module_with_assignments_2",
        r###"
            module my_module for rib-in with bla: Blaffer {
               define {
                   use bla;
                   bla = bla2(Bla);
               }
            
               term filter_te#$%^$$%st_1 {
                   match { blaffer.blaf.contains(something,"somewhat"); }
               }
               
               action blaffer {
                   blaffer.blaf(bla);
               }
            }
            // comment
            rib unrib contains Blaffer { blaffer: Blaf }
        "###,
        false,
    );

    test_data(
        "module_with_assignments_3",
        r###"
            module my_module with bla: Blaffer {
               define {
                   use bla;
                   bla = bla3(Bla);
               }
            
               term blaffer_filter {
                   match { blaffer.blaf.contains(something,"somewhat"); }
               }
               
               action blaffer {
                   blaffer.blaf(bla);
               }
            }
            // comment
            rib unrib contains Blaffer { blaffer: Blaf }
        "###,
        true,
    );

    test_data(
        "module_with_apply_1",
        r###"
            module my_module for my-rib with bla: Blaffer {
               define {
                   use bla;
                   bla = bla4(Bla);
               }
            
               term blaffer_filter {
                   match {
                        blaffer.waf() > gruf;
                        route.prefix in 0.0.0.0/0 prefix-length-range /12-/16;
                    }
               }
               
               action blaffer {
                   blaffer.blaf(bla);
               }

               apply {
                    use best-path;
                    filter exactly-one exists(found_prefix) matching { set-best(route); return accept; };
               }
            }
            // comment
            rib unrib contains Blaffer { blaffer: Blaf }
        "###,
        true,
    );

    test_data(
        "module_with_apply_2",
        r###"
            module my_module for my-rib with bla: Blaffer {
               define {
                   use bla;
                   bla = bla();
               }
            
               term blaffer_filter {
                   match { 
                        blaffer.blaf.contains(something,"somewhat") > blaf();
                        ( bla.bla() > some_external_set );
                   }
               }
               
               action blaffer {
                   blaffer.blaf(bla);
               }

               apply {
                    use best-path;
                    filter exactly-one exists(found_prefix) matching { set-best(route); return accept; };
                    use backup-path;
                    filter match rov-invalid-asn matching { set-rov-invalid-asn-community; return reject; };
               }
            }
            // comment
            rib unrib contains Blaffer { blaffer: Blaf }
        "###,
        true,
    );

    test_data(
        "module_with_nested_match_expressions",
        r###"
            module my_module for my-rib with bla: Blaffer {
               define {
                   use bla;
                   bla = bla();
               }
            
               term blaffer_filter {
                   match { 
                        // blaffer.blaf.contains(something,"somewhat") > blaf();
                        ( bla.bla() in my_set ) || ( bla.bla() in my_other_set );
                   }
               }
               
               action blaffer {
                   blaffer.blaf(bla);
               }

               apply {
                    use best-path;
                    filter exactly-one exists(found_prefix) matching { set-best(route); return accept; };
                    use backup-path;
                    filter match rov-invalid-asn matching { set-rov-invalid-asn-community; return reject; };
               }
            }
            // comment
            rib unrib contains Blaffer { blaffer: Blaf }
        "###,
        true,
    );
}
