use crate::parser::Parser;

use super::{TypeChecker, TypeResult};

#[track_caller]
fn typecheck(s: &str) -> TypeResult<()> {
    let tree = match Parser::parse(s) {
        Ok(ast) => ast,
        Err(e) => {
            let report =
                miette::Report::new(e).with_source_code(s.to_string());
            println!("{report:?}");
            panic!("Parse error, see above");
        }
    };
    let res = TypeChecker::new().check(tree);
    if let Err(e) = &res {
        eprintln!("{e}");
    }
    res
}

#[test]
fn one_record() {
    let src = "type Foo { a: U32 }";
    assert!(typecheck(src).is_ok());
}

#[test]
fn declared_multiple_times() {
    let src = "
        type Foo { a: U32 }
        type Foo { a: U32 }
    ";
    assert!(typecheck(src).is_err());
}

#[test]
fn double_field() {
    let src = "
        type Foo { a: U32, a: U8 }
    ";
    assert!(typecheck(src).is_err());
}

#[test]
fn undeclared_type_in_record() {
    let src = "type Bar { f: Foo }";
    assert!(typecheck(src).is_err());
}

#[test]
fn nested_record() {
    let src = "
        type Foo { a: { b: U32, c: U32 } }
    ";
    assert!(typecheck(src).is_ok());
}

#[test]
fn two_records() {
    let src = "
        type Foo { a: U32 }
        type Bar { f: Foo }
    ";
    assert!(typecheck(src).is_ok());

    let src = "
        type Bar { f: Foo }
        type Foo { a: U32 }
    ";
    assert!(typecheck(src).is_ok());
}

#[test]
fn record_cycle() {
    let src = "
        type Foo { f: Foo }
    ";
    assert!(typecheck(src).is_err());

    let src = "
        type Bar { f: Foo }
        type Foo { b: Bar }
    ";
    assert!(typecheck(src).is_err());

    let src = "
        type A { x: B }
        type B { x: C }
        type C { x: D }
        type D { x: A }
    ";
    assert!(typecheck(src).is_err());

    let src = "
        type A { x: B }
        type B { x: C }
        type C { x: { y: D } }
        type D { x: B }
    ";
    assert!(typecheck(src).is_err());
}

#[test]
fn record_diamond() {
    let src = "
        type A { x: B, y: C }
        type B { x: D }
        type C { x: D }
        type D { }
    ";
    assert!(typecheck(src).is_ok());
}

#[test]
fn table_contains_record() {
    let src = "
        table t contains A { b: B }
        type B { x: U32 }
    ";
    assert!(typecheck(src).is_ok());
}

#[test]
fn output_stream_contains_record() {
    let src = "
        output-stream o contains A { b: B }
        type B { x: U32 }
    ";
    assert!(typecheck(src).is_ok());
}

#[test]
fn rib_contains_record() {
    let src = "
        rib r contains A { b: B }
        type B { x: U32 }
    ";
    assert!(typecheck(src).is_ok());
}

#[test]
fn filter_map() {
    let src = r#"
        filter-map blabla {
            define {
                rx foo: U32;
                a = "hello";
                b = 0.0.0.0/10;
                c = 192.168.0.0;
            }
        }
    "#;
    assert!(typecheck(src).is_ok());
}

#[test]
fn filter_map_double_definition() {
    let src = r#"
        filter-map blabla {
            define {
                rx foo: U32;
                a = "hello";
                a = 0.0.0.0/10;
            }
        }
    "#;
    assert!(typecheck(src).is_err());
}

#[test]
fn using_records() {
    let src = r#"
        type Foo { a: String }

        filter-map bar {
            define {
                rx r: U32;
                a = Foo { a: "hello" };
            }
        }
    "#;
    typecheck(src).unwrap();

    let src = r#"
        type Foo { a: String }

        filter-map bar {
            define {
                rx r: U32;
                a = Foo { a: 0.0.0.0 };
            }
        }
    "#;
    assert!(typecheck(src).is_err());

    let src = r#"
        type Foo { a: string }

        filter-map bar {
            define {
                rx r: U32;
                a = Foo { };
            }
        }
    "#;
    assert!(typecheck(src).is_err());
}

#[test]
fn integer_inference() {
    let src = "
        type Foo { x: U8 }

        filter-map test {
            define {
                rx r: U32;
                foo = Foo { x: 5 };
            }
        }
    ";
    typecheck(src).unwrap();

    let src = "
        type Foo { x: U8 }
        type Bar { x: U8 }

        filter-map test {
            define {
                rx r: U32;
                a = 5;
                foo = Foo { x: a };
            }
        }
    ";
    assert!(typecheck(src).is_ok());

    let src = "
        type Foo { x: U8 }
        type Bar { x: U8 }

        filter-map test {
            define {
                rx r: U32;
                a = 5;
                foo = Foo { x: a };
                bar = Bar { x: a };
            }
        }
    ";
    assert!(typecheck(src).is_ok());

    let src = "
        type Foo { x: U8 }
        type Bar { x: U32 }

        filter-map test {
            define {
                rx r: U32;
                a = 5;
                foo = Foo { x: a };
                bar = Bar { x: a };
            }
        }
    ";
    assert!(typecheck(src).is_err());

    let src = "
        type Foo { x: U8 }
        type Bar { x: U32 }

        filter-map test {
            define {
                rx r: U32;
                foo = Foo { x: 5 };
                bar = Bar { x: 5 };
            }
        }
    ";
    assert!(typecheck(src).is_ok());
}

#[test]
fn assign_field_to_other_record() {
    let src = "
        type Foo { x: U8 }
        type Bar { x: U8 }

        filter-map test {
            define {
                rx r: U32;
                foo = Foo { x: 5 };
                bar = Bar { x: foo.x };
            }
        }
    ";
    typecheck(src).unwrap();

    let src = "
        type Foo { x: U8 }
        type Bar { x: U8 }

        filter-map test {
            define {
                rx r: U32;
                foo = Foo { x: 5 };
                bar = Bar { x: foo.y };
            }
        }
    ";
    assert!(typecheck(src).is_err());

    let src = "
        type Foo { x: U8 }
        type Bar { x: U32 }

        filter-map test {
            define {
                rx r: U32;
                foo = Foo { x: 5 };
                bar = Bar { x: foo.x };
            }
        }
    ";
    assert!(typecheck(src).is_err());
}

#[test]
fn prefix_method() {
    let src = "
        filter-map test {
            define {
                rx r: U32;
                p = 10.10.10.10/20;
                add = p.address();
            }
        }
    ";
    assert!(typecheck(src).is_ok());
}

#[test]
fn logical_expr() {
    let src = "
        filter-map test {
            define {
                rx r: U32;
                p = 10.10.10.10/10;
            }

            term foo {
                match {
                    (10 == 10) || (10 == 11);
                }
            }
        }
    ";
    assert!(typecheck(src).is_ok());

    let src = r#"
        filter-map test {
            define {
                rx r: U32;
                p = 10.10.10.10/10;
            }

            term foo {
                match {
                    (10 == 10) || ("hello" == 11);
                }
            }
        }
    "#;
    assert!(typecheck(src).is_err());
}

#[test]
fn send_output_stream() {
    let src = r#"
        output-stream stream contains Msg {
            foo: String
        }

        filter-map test {
            define {
                rx r: U32;
            }

            action hello {
                stream.send(Msg {
                    foo: "hello",
                });
            }
        }
    "#;
    typecheck(src).unwrap();

    let src = r#"
        output-stream stream contains Foo {
            foo: String
        }

        type Bar { bar: String }

        filter-map test {
            define {
                rx r: U32;
            }

            action hello {
                stream.send(Bar {
                    bar: "hello",
                });
            }
        }
    "#;
    assert!(typecheck(src).is_err());

    let src = r#"
        output-stream foos contains Foo {
            foo: String
        }

        output-stream bars contains Bar {
            bar: String
        }

        filter-map test {
            define {
                rx r: U32;
            }

            action hello {
                foos.send(Foo {
                    foo: "hello",
                });
                bars.send(Bar {
                    bar: "world",
                });
            }
        }
    "#;
    typecheck(src).unwrap();

    let src = r#"
        output-stream foos contains Foo {
            foo: String
        }

        output-stream bars contains Bar {
            bar: String
        }

        filter-map test {
            define {
                rx r: U32;
            }

            action hello {
                foos.send(Foo {
                    foo: "hello",
                });
                bars.send(Foo {
                    foo: "world",
                });
            }
        }
    "#;
    assert!(typecheck(src).is_err());
}

#[test]
fn term_overrides_var() {
    let src = r#"
        filter-map test {
            define {
                rx r: U32;
                a = true;
            }

            term a {
                match {
                    a;
                }
            }
        }
    "#;
    assert!(typecheck(src).is_err());
}
