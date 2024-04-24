use crate::pipeline::{self, RotoReport};

#[track_caller]
fn typecheck(s: &str) -> Result<(), RotoReport> {
    let res = pipeline::test_file(s).parse();

    let res = match res {
        Ok(res) => res,
        Err(err) => {
            println!("{err}");
            panic!("Parse Error");
        }
    };

    // Unwrap on parse because a parse error in this file is never correct.
    // We only want to test for type errors.
    if let Err(e) = res.typecheck() {
        println!("{e}");
        Err(e)
    } else {
        Ok(())
    }
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
        filter-map blabla(foo: U32) {
            define {
                a = "hello";
                b = 0.0.0.0/10;
                c = 192.168.0.0;
            }

            apply { accept }
        }
    "#;
    assert!(typecheck(src).is_ok());
}

#[test]
fn filter_map_double_definition() {
    let src = r#"
        filter-map blabla(foo: U32) {
            define {
                a = "hello";
                a = 0.0.0.0/10;
            }

            apply { accept }
        }
    "#;
    assert!(typecheck(src).is_err());
}

#[test]
fn using_records() {
    let src = r#"
        type Foo { a: String }

        filter-map bar(r: U32) {
            define {
                a = Foo { a: "hello" };
            }

            apply { accept }
        }
    "#;
    typecheck(src).unwrap();

    let src = r#"
        type Foo { a: String }

        filter-map bar(r: U32) {
            define {
                a = Foo { a: 0.0.0.0 };
            }

            apply { accept }
        }
    "#;
    assert!(typecheck(src).is_err());

    let src = r#"
        type Foo { a: string }

        filter-map bar(r: U32) {
            define {
                a = Foo { };
            }

            apply { accept }
        }
    "#;
    assert!(typecheck(src).is_err());
}

#[test]
fn integer_inference() {
    let src = "
        type Foo { x: U8 }

        filter-map test(r: U32) {
            define {
                foo = Foo { x: 5 };
            }

            apply { accept }
        }
    ";
    typecheck(src).unwrap();

    let src = "
        type Foo { x: U8 }
        type Bar { x: U8 }

        filter-map test(r: U32) {
            define {
                a = 5;
                foo = Foo { x: a };
            }

            apply { accept }
        }
    ";
    assert!(typecheck(src).is_ok());

    let src = "
        type Foo { x: U8 }
        type Bar { x: U8 }

        filter-map test(r: U32) {
            define {
                a = 5;
                foo = Foo { x: a };
                bar = Bar { x: a };
            }

            apply { accept }
        }
    ";
    assert!(typecheck(src).is_ok());

    let src = "
        type Foo { x: U8 }
        type Bar { x: U32 }

        filter-map test(r: U32) {
            define {
                a = 5;
                foo = Foo { x: a };
                bar = Bar { x: a };
            }

            apply { accept }
        }
    ";
    assert!(typecheck(src).is_err());

    let src = "
        type Foo { x: U8 }
        type Bar { x: U32 }

        filter-map test(r: U32) {
            define {
                foo = Foo { x: 5 };
                bar = Bar { x: 5 };
            }

            apply { accept }
        }
    ";
    assert!(typecheck(src).is_ok());
}

#[test]
fn assign_field_to_other_record() {
    let src = "
        type Foo { x: U8 }
        type Bar { x: U8 }

        filter-map test(r: U32) {
            define {
                foo = Foo { x: 5 };
                bar = Bar { x: foo.x };
            }

            apply { accept }
        }
    ";
    typecheck(src).unwrap();

    let src = "
        type Foo { x: U8 }
        type Bar { x: U8 }

        filter-map test(r: U32) {
            define {
                foo = Foo { x: 5 };
                bar = Bar { x: foo.y };
            }

            apply { accept }
        }
    ";
    assert!(typecheck(src).is_err());

    let src = "
        type Foo { x: U8 }
        type Bar { x: U32 }

        filter-map test(r: U32) {
            define {
                foo = Foo { x: 5 };
                bar = Bar { x: foo.x };
            }

            apply { accept }
        }
    ";
    assert!(typecheck(src).is_err());
}

#[test]
fn prefix_method() {
    let src = "
        filter-map test(r: U32) {
            define {
                p = 10.10.10.10/20;
                add = p.address();
            }

            apply { accept }
        }
    ";
    assert!(typecheck(src).is_ok());
}

#[test]
fn logical_expr() {
    let src = "
        filter-map test(r: U32) {
            define {
                p = 10.10.10.10/10;
            }

            term foo() {
                (10 == 10) || (10 == 11)
            }

            apply { accept }
        }
    ";
    typecheck(src).unwrap();

    let src = r#"
        filter-map test(r: U32) {
            define {
                p = 10.10.10.10/10;
            }

            term foo() {
                (10 == 10) || ("hello" == 11)
            }

            apply { accept }
        }
    "#;
    typecheck(src).unwrap_err();
}

#[test]
fn send_output_stream() {
    let src = r#"
        output-stream stream contains Msg {
            foo: String
        }

        filter-map test(r: U32) {
            action hello() {
                stream.send(Msg {
                    foo: "hello",
                });
            }

            apply { accept }
        }
    "#;
    typecheck(src).unwrap();

    let src = r#"
        output-stream stream contains Foo {
            foo: String
        }

        type Bar { bar: String }

        filter-map test(r: U32) {
            action hello() {
                stream.send(Bar {
                    bar: "hello",
                });
            }

            apply { accept }
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

        filter-map test(r: U32) {
            action hello() {
                foos.send(Foo {
                    foo: "hello",
                });
                bars.send(Bar {
                    bar: "world",
                });
            }

            apply { accept }
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

        filter-map test(r: U32) {
            action hello() {
                foos.send(Foo {
                    foo: "hello",
                });
                bars.send(Foo {
                    foo: "world",
                });
            }

            apply { accept }
        }
    "#;
    assert!(typecheck(src).is_err());
}

#[test]
fn term_overrides_var() {
    let src = r#"
        filter-map test(r: U32) {
            define {
                a = true;
            }

            term a() {
                a
            }

            apply { accept }
        }
    "#;
    assert!(typecheck(src).is_err());
}

#[test]
fn record_inference() {
    let src = "
        output-stream s contains Msg { a: U32 }

        filter-map foo(r: U32) { 
            define {
                a = { a: 8 };
            }

            action bla() {
                s.send(a);
            }

            apply { accept }
        }
    ";
    typecheck(src).unwrap();

    let src = "
        output-stream s contains Msg { a: U32 }

        filter-map foo(r: U32) { 
            define {
                a = { b: 8 };
            }

            action bla() {
                s.send(a);
            }

            apply { accept }
        }
    ";
    assert!(typecheck(src).is_err());

    let src = "
        type A { a: U32 }

        filter-map foo(r: U32) { 
            define {
                a = { a: 8 };
                b = A { a: 8 };
            }

            term bla() {
                a == b
            }

            apply { accept }
        }
    ";
    typecheck(src).unwrap();

    let src = "
        type A { a: U32 }
        type B { a: U32 }

        filter-map foo(r: U32) { 
            define {
                a = { a: 8 };
                b = A { a: 8 };
                c = B { a: 8 };
            }

            term bla() {
                a == b && a == c
            }

            apply { accept }
        }
    ";
    assert!(typecheck(src).is_err());
}

#[test]
fn return_keyword() {
    let src = "
        filter-map foo(r: U32) { 
            term foo() {
                return true
            }

            apply { accept }
        }
    ";
    typecheck(src).unwrap();

    let src = "
        filter-map foo(r: U32) { 
            term foo() {
                return 2
            }

            apply { accept }
        }
    ";
    typecheck(src).unwrap_err();
}

#[test]
fn unit_block() {
    let src = "
        filter-map foo(r: U32) { 
            // workaround for not having a ()
            action unit() {}

            term foo() {
                unit();
            }

            apply { accept }
        }
    ";
    typecheck(src).unwrap_err();
}

#[test]
fn unreachable_expression() {
    let src = "
        filter-map foo(r: U32) { 
            term foo() {
                return true;
                return false;
            }

            apply { accept }
        }
    ";
    typecheck(src).unwrap_err();
}

#[test]
fn enum_values() {
    let src = "
        filter-map main(r: U32) { 
            define {
                x = Afi.IpV4;
            }

            apply {
                if x == Afi.IpV4 {
                    accept
                } else {
                    reject
                }
            }
        }
    ";
    typecheck(src).unwrap();
}

#[test]
fn bmp_message() {
    let src = "
        filter-map main(x: BmpMessage) { 
            define {
                a = BmpMessage.InitiationMessage(BmpInitiationMessage {});
            }

            apply {
                if x == a {
                    accept
                } else {
                    reject
                }
            }
        }
    ";
    typecheck(src).unwrap();
}

#[test]
fn enum_match() {
    let src = "
        filter-map foo(r: U32) { 
            define {
                x = Afi.IpV4;
            }

            apply {
                match x {
                    IpV4 -> accept,
                    IpV6 -> accept,
                    _ -> reject,
                }
            }
        }
    ";
    typecheck(src).unwrap();
}

#[test]
fn bmp_message_4() {
    let src = "
        filter-map main(x: BmpMessage) { 
            apply {
                match x {
                    PeerUpNotification(x) | x.local_port == 80 -> accept,
                    PeerUpNotification(x) | x.local_port == 12 -> accept,
                    PeerUpNotification(x) -> {
                        if x.local_port == 70 {
                            accept
                        }
                    }
                    _ -> {},
                }
                reject
            }
        }
    ";
    typecheck(src).unwrap();
}
