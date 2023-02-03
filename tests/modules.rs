#[cfg(test)]
use nom::error::convert_error;
use roto::ast::*;

fn test_data_parse(name: &str, data: &str, expect_success: bool) {
    println!("test {}", name);
    let parsed_data = SyntaxTree::parse_str(data);
    println!("{} {:#?}", name, parsed_data);
    if let Err(e) = parsed_data.clone() {
        println!("{}", convert_error(data, e));
    }

    match expect_success {
        false => assert!(parsed_data.is_err()),
        true => assert!(parsed_data.is_ok()),
    };
}

#[test]
fn test_module_parse_1() {
    test_data_parse(
        "module_1",
        r###"
        module in-module {
            define {
                // specify the types of that this filter receives
                // and sends.
                // rx route: StreamRoute;
                // tx ext_route: ExtRoute;
                rx_tx route: StreamRoute;
  
            }

            term on-my-terms {
                match {
                    route.as-path.origin() == AS211321;
                }
            }
        }
        "###,
        true,
    )
}

#[test]
fn test_module_parse_2() {
    test_data_parse(
        "module_1",
        r###"
        module in-module {
            define {
                // specify the types of that this filter receives
                // and sends.
                rx route: StreamRoute;
                tx ext_route: ExtRoute;  
            }

            term on-my-terms {
                match {
                    route.as-path.origin() == AS211321;
                }
            }
        }
        "###,
        true,
    )
}