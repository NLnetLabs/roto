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
    test_data("random-crap", "##@#@#kdflfk!  abc  \n  ", false);

    test_data("empty-rib", "rib my_rib {}\n// comment\n", true);

    test_data("crab-rib", "rib [] ribribribmodule\n", false);

    test_data(
        "valid-rib-with-comment",
        "rib my_rib3 { bla: Bla, bloo: Bloo }\n// comment\n",
        true,
    );

    test_data(
        "invalid-rib-with-comment",
        "rib my_rib { bla: Bla, blow_up }\n// comment\n",
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
        rib my_rib { bla: Bla, bloo: Bloo }
        // comment\nrib unrib { blaffer: Blaf }
        "###,
        true,
    );

    test_data(
        "module",
        r###"
        module my_module for rib-loc with bla: Blaffer { define my_def { use bla; } }
            // comment
            rib unrib { blaffer: Blaf }
        "###,
        true,
    );

    test_data(
        "module-more-with",
        r###"
        module my_module for rib-in with bla: Blaffer {
            define { use bla; }
        }
            
        // comment
        rib unrib { blaffer: Blaf }
        "###,
        true,
    );

    test_data(
        "module",
        r###"
            module my_module for rib-in with bla: Blaffer {
               define { use bla; }
            }
            // comment
            rib unrib { blaffer: Blaf }
        "###,
        true,
    );

    test_data(
        "module_with_assignments",
        r###"
            module my_module for rib-in with bla: Blaffer {
               define {
                   use bla;
                   bla = Bla;
               }
            
               term blaffer_filter {
                   with bla;
                   match { blaffer.blaf.contains(something); }
               }
            }
            // comment
            rib unrib { blaffer: Blaf }
        "###,
        true,
    );

    test_data(
        "module_with_assignments_2",
            r###"
            module my_module for rib-in with bla: Blaffer {
               define {
                   use bla;
                   bla = Bla;
               }
            
               term blaffer_filter {
                   match { blaffer.blaf.contains(something,\"somewhat"); }
               }
               
               action blaffer {
                   blaffer.blaf(bla);
               }
            }
            // comment
            rib unrib { blaffer: Blaf }
        "###,
        false,
    );

    test_data(
        "module_with_assignments_2",
            r###"
            module my_module with bla: Blaffer {
               define {
                   use bla;
                   bla = Bla;
               }
            
               term blaffer_filter {
                   match { blaffer.blaf.contains(something,"somewhat"); }
               }
               
               action blaffer {
                   blaffer.blaf(bla);
               }
            }
            // comment
            rib unrib { blaffer: Blaf }
        "###,
        true,
    );

    test_data(
        "module_with_apply_2",
            r###"
            module my_module for my-rib with bla: Blaffer {
               define {
                   use bla;
                   bla = Bla;
               }
            
               term blaffer_filter {
                   match { blaffer.blaf.contains(something,"somewhat"); }
               }
               
               action blaffer {
                   blaffer.blaf(bla);
               }

               apply {
                    with best-path;
                    filter exactly-one exists(found_prefix) matching { set-best(route); return accept; };
               }
            }
            // comment
            rib unrib { blaffer: Blaf }
        "###,
        true,
    );
}
