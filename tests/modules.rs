#[cfg(test)]
use nom::error::convert_error;
use nom::error::VerboseError;
use roto::ast::*;
use roto::compile::{CompileError, Compiler};

fn test_source_code_parse<'a>(
    name: &'a str,
    source_code: &'a str,
    expect_success: bool,
) -> Result<(&'a str, SyntaxTree), VerboseError<&'a str>> {
    println!("test {}", name);
    let parsed_data = SyntaxTree::parse_str(source_code);
    println!("{} {:#?}", name, parsed_data);
    if let Err(e) = parsed_data.clone() {
        println!("{}", convert_error(source_code, e));
    }

    match expect_success {
        false => assert!(parsed_data.is_err()),
        true => assert!(parsed_data.is_ok()),
    };

    parsed_data
}

fn test_source_code_eval(
    name: &str,
    source_code: &str,
    expect_success: bool,
) -> Result<(), CompileError> {
    println!("test eval {}", name);
    let mut compiler = Compiler::new();
    compiler.parse_source_code(source_code).unwrap();
    match compiler.eval_ast() {
        Err(e) => {
            println!("{}", e);
        }
        Ok(_res) => {
            match expect_success {
                false => panic!("Source Code was successfully compiled, however an invalid result was requested."),
                true => {}
            };
        }
    }

    Ok(())
}

#[test]
fn test_module_parse_1() {
    test_source_code_parse(
        "module_1",
        r###"
        module in-module {
            define {
                rx_tx route: Route;
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
    .unwrap();
}

#[test]
fn test_module_eval_1() {
    test_source_code_eval(
        "module_1",
        r###"
        module in-module {
            define {
                rx_tx route: Route;
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
    .unwrap();
}

#[test]
fn test_module_parse_2() {
    test_source_code_parse(
        "module_1",
        r###"
        module in-module {
            define {
                rx route: Route;
                tx ext_route: ExtRoute;  
            }

            term on-my-terms {
                match {
                    route.as-path.origin() == AS211321;
                }
            }
        }

        rib ext-rib contains ExtRoute {
            prefix: Prefix,
            as-path: AsPath
        }
        "###,
        true,
    )
    .unwrap();
}

#[test]
fn test_module_eval_2() {
    test_source_code_eval(
        "module_1",
        r###"
        module in-module {
            define {
                rx route: Route;
                tx ext_route: ExtRoute;  
            }

            term on-my-terms {
                match {
                    route.as-path.origin() == AS211321;
                }
            }
        }

        rib ext-rib contains ExtRoute {
            prefix: Prefix,
            as-path: AsPath
        }
        "###,
        true,
    )
    .unwrap();
}
