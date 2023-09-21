use nom::error::convert_error;
use roto::ast::*;

fn test_data(name: &str, data: &str, expect_success: bool) {
    println!("test {}", name);
    let parsed_data = SyntaxTree::parse_str(data);
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
    let cm = ComputeExpr::parse(
        r#"source_asns.contains("asn", route.as_path.origin)"#,
    );

    println!("{}, {:#?}", cm.is_ok(), cm);
    assert!(cm.is_ok());

    let mm = ValueExpr::parse(r###"globlaf(bla)"###);
    println!("{}, {:#?}", mm.is_ok(), mm);
    assert!(mm.is_ok());

    let r = ByteStringLiteral::parse("0xZZZZ_AE9");
    println!("{:#?}", r);
    assert!(r.is_err());

    let mut r = LogicalExpr::parse(
        "( blaffer.waf().contains(my_set) ) || ( blaffer.blaf() < bop() )",
    );
    assert!(r.is_ok());

    r = LogicalExpr::parse(
        r###"
        route.prefix in 0.0.0.0/0 prefix-length-range /12-/16;
    "###,
    );
    println!("{:#?}", r);
    assert!(r.is_ok());

    r = LogicalExpr::parse(
        r#"
        blaffer.blaf.contains(something,"somewhat") > blaf();
        // ( bla.bla() in my_set ) || ( bla.bla() in my_other_set );
    "#,
    );
    println!("{:#?}", r);
    assert!(r.is_ok());

    // let r = FilterMap::parse(r###"
    //     filter-map my_filter_map for rib-in with bla: Blaffer {
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

    test_data("crab-rib", "rib [] ribribribfilter_map\n", false);

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
        "filter-map-1",
        r###"
        filter-map filter-map-1 with my_asn: Asn {
            define for ext_r: ExtRoute with extra_asn: Asn {
                // specify the types of that this filter receives
                // and sends.
                // rx_tx route: StreamRoute;
                rx route: StreamRoute;
                tx ext_route: ExtRoute;

                // specify additional external data sets that will be consulted.
                use table source_asns;

                // syntactically correct, but semantically wrong: these
                // identifiers are not defined.
                // bullshitter = a.b.c.d(x,y,z).e.f(o.p()).g;

                my_false = false;
            }
        
            term rov-valid for route: Route {
                match {
                    found_prefix_pref == route.local-pref;  
                }
            }

            term on-my-terms for route: Route {
                match {
                    my_false;
                }
            }
           
            action set-best {
               route.origin.set(AS300);
            }

            action set-rov-invalid-asn-community {
                route.community.push(ROV_INVALID_AS);
            }

            apply {
                use best-path;
                filter exactly-one rov-valid matching { 
                    set-best; 
                    set-rov-invalid-asn-community; 
                    return accept; 
                };
                return accept;
            }
        }

        // comment
        rib rib-extra contains ExtRoute { 
            blaffer: U32, 
            blooper: Prefix,
            blixer: { 
                bla: U8, 
                salt: { 
                    pp: Prefix 
                } 
            }  
        }

        table source_asns contains AsnLines { 
            asn: Asn
        }

        // yo, rib
        rib rib-rov contains StreamRoute {
            prefix: Prefix, // this is shit: it's the key
            as-path: AsPath,
            origin: Asn,
            next-hop: IpAddress,
            med: U32,
            local-pref: U32,
            community: [Community]
        }
        "###,
        true,
    );

    test_data(
        "filter-map-more-with",
        r###"
        filter-map filter-map-more-with for route_in: Route with bla: Blaffer {
            define { 
                rx route: Route;
                tx ext_route: Route;
                use rib bla; 
            }
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
        "filter_map_2",
        r###"
            filter-map filter_map_2 for route_in: Route with bla: Blaffer {
                define { 
                    rx route: Route;
                    tx ext_route: Route;
                    use rib bla; 
                }
                term filter3 {}
                }
                // comment
                rib unrib contains Blaffer { blaffer: Blaf }
        "###,
        true,
    );

    test_data(
        "filter-map_with_assignments_1",
        r###"
            filter-map filter-map_with_assignments_1 for route_in: Route with bla: Blaffer {
               define {
                   rx route: Route;
                   tx ext_route: Route;
                   use rib bla;
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
        "filter-map_with_assignments_2",
        r#"
            filter-map filter-map_with_assignments_2 for route_in: Route with bla: Blaffer {
               define {
                   rx route: Route;
                   tx ext_route: Route;
                   use rib bla;
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
        "#,
        false,
    );

    test_data(
        "filter_map_with_assignments_3",
        r#"
            filter-map filter-map_with_assignments_3 for route_in: Route with bla: Blaffer {
               define {
                   rx route: Route;
                   tx ext_route: Route;
                   use rib bla;
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
        "#,
        true,
    );

    test_data(
        "filter_map_with_apply_1",
        r###"
            filter-map filter_map_with_apply_1 for route_in: Route with bla: Blaffer {
               define {
                    rx route: StreamRoute;
                    tx ext_route: ExtRoute;
                    use rib bla;
                    bla = bla4(Bla);
               }
            
               term blaffer_filter {
                    match {
                        blaffer.waf() > gruf;
                        // route.prefix in 0.0.0.0/0 prefix-length-range /12-/16;
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
        "filter_map_with_apply_2",
        r#"
            filter-map filter_map_with_apply_2 for route_in: Route with bla: Blaffer {
               define {
                   rx route: Route;
                   tx ext_route: Route;
                   use rib bla;
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
        "#,
        true,
    );

    test_data(
        "filter-map_with_nested_match_expressions",
        r#"
            filter-map filter-map_with_nested_match_expressions for route_in: Route with bla: Blaffer {
               define {
                   rx route: Route;
                   tx ext_route: Route;
                   use rib bla;
                   bla = bla();
               }
            
               term blaffer_filter {
                   match { 
                        blaffer.blaf.contains(something,"somewhat") > blaf();
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
            rib unrib contains Blaffer { blaffer: Blaf, blixer: { bloop: U32, blap: U8 } }
        "#,
        true,
    );
}
