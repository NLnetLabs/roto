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

    test_data("empty-rib", "rib my_rib {}\n// comment\n", false);

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
        "rib my_rib { bla: Bla, bloo: Bloo }\n// comment\nrib unrib { blaffer: Blaf }\n",
        true,
    );

    test_data(
        "module",
        concat!(
            "module my_module for route: Route with bla: Blaffer { define my_def { use bla; } }\n",
            "// comment\n",
            "rib unrib { blaffer: Blaf }\n"
        ),
        true,
    );

    test_data(
        "module-more-with",
        concat!(
            "module my_module for route: Route with bla: Blaffer {\n",
            "   define { use bla; }\n",
            "}\n",
            "\n",
            "// comment\n",
            "rib unrib { blaffer: Blaf }\n"
        ),
        true,
    );

    test_data(
        "module",
        concat!(
            "module my_module for route: Route with bla: Blaffer {\n",
            "   define { use bla; }\n",
            "}\n",
            "// comment\n",
            "rib unrib { blaffer: Blaf }\n"
        ),
        true,
    );
}
