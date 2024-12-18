use crate::runtime::tests::routecore_runtime;
use crate::src;
use crate::{pipeline::RotoReport, Files};

#[track_caller]
fn typecheck(loaded: Files) -> Result<(), RotoReport> {
    let res = loaded.parse();

    let res = match res {
        Ok(res) => res,
        Err(err) => {
            println!("{err}");
            panic!("Parse Error");
        }
    };

    let runtime = routecore_runtime().unwrap();
    let pointer_bytes = usize::BITS / 8;

    // Unwrap on parse because a parse error in this file is never correct.
    // We only want to test for type errors.
    if let Err(e) = res.typecheck(runtime, pointer_bytes) {
        println!("{e}");
        Err(e)
    } else {
        Ok(())
    }
}

#[test]
fn one_record() {
    let s = src!("type Foo { a: u32 }");
    assert!(typecheck(s).is_ok());
}

#[test]
fn declared_multiple_times() {
    let s = src!(
        "
        type Foo { a: u32 }
        type Foo { a: u32 }
    "
    );
    assert!(typecheck(s).is_err());
}

#[test]
fn double_field() {
    let s = src!(
        "
        type Foo { a: u32, a: u8 }
    "
    );
    assert!(typecheck(s).is_err());
}

#[test]
fn undeclared_type_in_record() {
    let s = src!("type Bar { f: Foo }");
    assert!(typecheck(s).is_err());
}

#[test]
fn nested_record() {
    let s = src!(
        "
        type Foo { a: { b: u32, c: u32 } }
    "
    );
    assert!(typecheck(s).is_ok());
}

#[test]
fn two_records() {
    let s = src!(
        "
        type Foo { a: u32 }
        type Bar { f: Foo }
    "
    );
    assert!(typecheck(s).is_ok());

    let s = src!(
        "
        type Bar { f: Foo }
        type Foo { a: u32 }
    "
    );
    assert!(typecheck(s).is_ok());
}

#[test]
fn record_cycle() {
    let s = src!(
        "
        type Foo { f: Foo }
    "
    );
    assert!(typecheck(s).is_err());

    let s = src!(
        "
        type Bar { f: Foo }
        type Foo { b: Bar }
    "
    );
    assert!(typecheck(s).is_err());

    let s = src!(
        "
        type A { x: B }
        type B { x: C }
        type C { x: D }
        type D { x: A }
    "
    );
    assert!(typecheck(s).is_err());

    let s = src!(
        "
        type A { x: B }
        type B { x: C }
        type C { x: { y: D } }
        type D { x: B }
    "
    );
    assert!(typecheck(s).is_err());
}

#[test]
fn record_diamond() {
    let s = src!(
        "
        type A { x: B, y: C }
        type B { x: D }
        type C { x: D }
        type D { }
    "
    );
    assert!(typecheck(s).is_ok());
}

#[test]
fn table_contains_record() {
    let s = src!(
        "
        table t contains A { b: B }
        type B { x: u32 }
    "
    );
    assert!(typecheck(s).is_ok());
}

#[test]
fn output_stream_contains_record() {
    let s = src!(
        "
        output-stream o contains A { b: B }
        type B { x: u32 }
    "
    );
    assert!(typecheck(s).is_ok());
}

#[test]
fn rib_contains_record() {
    let s = src!(
        "
        rib r contains A { b: B }
        type B { x: u32 }
    "
    );
    assert!(typecheck(s).is_ok());
}

#[test]
#[ignore = "prefixes not supported yet"]
fn filter_map() {
    let s = src!(
        r#"
        filter-map blabla(foo: u32) {
            define {
                a = "hello";
                b = 0.0.0.0/10;
                c = 192.168.0.0;
            }

            apply { accept }
        }
    "#
    );
    assert!(typecheck(s).is_ok());
}

#[test]
fn filter_map_double_definition() {
    let s = src!(
        r#"
        filter-map blabla(foo: u32) {
            define {
                a = "hello";
                a = 0.0.0.0/10;
            }

            apply { accept }
        }
    "#
    );
    assert!(typecheck(s).is_err());
}

#[test]
fn using_records() {
    let s = src!(
        r#"
        type Foo { a: String }

        filter-map bar(r: u32) {
            define {
                a = Foo { a: "hello" };
            }

            apply { accept }
        }
    "#
    );
    typecheck(s).unwrap();

    let s = src!(
        r#"
        type Foo { a: String }

        filter-map bar(r: u32) {
            define {
                a = Foo { a: 0.0.0.0 };
            }

            apply { accept }
        }
    "#
    );
    assert!(typecheck(s).is_err());

    let s = src!(
        r#"
        type Foo { a: string }

        filter-map bar(r: u32) {
            define {
                a = Foo { };
            }

            apply { accept }
        }
    "#
    );
    assert!(typecheck(s).is_err());
}

#[test]
fn integer_inference() {
    let s = src!(
        "
        type Foo { x: u8 }

        filter-map test(r: u32) {
            define {
                foo = Foo { x: 5 };
            }

            apply { accept }
        }
    "
    );
    typecheck(s).unwrap();

    let s = src!(
        "
        type Foo { x: u8 }
        type Bar { x: u8 }

        filter-map test(r: u32) {
            define {
                a = 5;
                foo = Foo { x: a };
            }

            apply { accept }
        }
    "
    );
    assert!(typecheck(s).is_ok());

    let s = src!(
        "
        type Foo { x: u8 }
        type Bar { x: u8 }

        filter-map test(r: u32) {
            define {
                a = 5;
                foo = Foo { x: a };
                bar = Bar { x: a };
            }

            apply { accept }
        }
    "
    );
    assert!(typecheck(s).is_ok());

    let s = src!(
        "
        type Foo { x: u8 }
        type Bar { x: u32 }

        filter-map test(r: u32) {
            define {
                a = 5;
                foo = Foo { x: a };
                bar = Bar { x: a };
            }

            apply { accept }
        }
    "
    );
    assert!(typecheck(s).is_err());

    let s = src!(
        "
        type Foo { x: u8 }
        type Bar { x: u32 }

        filter-map test(r: u32) {
            define {
                foo = Foo { x: 5 };
                bar = Bar { x: 5 };
            }

            apply { accept }
        }
    "
    );
    assert!(typecheck(s).is_ok());
}

#[test]
fn assign_field_to_other_record() {
    let s = src!(
        "
        type Foo { x: u8 }
        type Bar { x: u8 }

        filter-map test(r: u32) {
            define {
                foo = Foo { x: 5 };
                bar = Bar { x: foo.x };
            }

            apply { accept }
        }
    "
    );
    typecheck(s).unwrap();

    let s = src!(
        "
        type Foo { x: u8 }
        type Bar { x: u8 }

        filter-map test(r: u32) {
            define {
                foo = Foo { x: 5 };
                bar = Bar { x: foo.y };
            }

            apply { accept }
        }
    "
    );
    assert!(typecheck(s).is_err());

    let s = src!(
        "
        type Foo { x: u8 }
        type Bar { x: u32 }

        filter-map test(r: u32) {
            define {
                foo = Foo { x: 5 };
                bar = Bar { x: foo.x };
            }

            apply { accept }
        }
    "
    );
    assert!(typecheck(s).is_err());
}

#[test]
fn ip_addr_method() {
    let s = src!(
        "
        filter-map test(r: u32) {
            define {
                p = 10.10.10.10;
                is_four = 1 + p.is_ipv4();
            }

            apply { accept }
        }
    "
    );
    assert!(typecheck(s).is_err());

    let s = src!(
        "
        filter-map test(r: u32) {
            define {
                p = 10.10.10.10;
                is_four = true && p.is_ipv4();
            }

            apply { accept }
        }
    "
    );
    assert!(typecheck(s).is_ok());
}

#[test]
#[ignore = "to_canonical doesn't work for now"]
fn ip_addr_method_of_method_return_type() {
    let s = src!(
        "
        filter-map test(r: u32) {
            define {
                p = 10.10.10.10;
                x = p.to_canonical().is_ipv4();
            }

            apply { accept }
        }
    "
    );
    assert!(typecheck(s).is_ok());

    let s = src!(
        "
        filter-map test(r: u32) {
            define {
                p = 10.10.10.10;
                x = p.is_ipv4().to_canonical();
            }

            apply { accept }
        }
    "
    );
    assert!(typecheck(s).is_err());
}

#[test]
#[ignore = "prefixes not supported yet"]
fn prefix_method() {
    let s = src!(
        "
        filter-map test(r: u32) {
            define {
                p = 10.10.10.10/20;
                add = p.address();
            }

            apply { accept }
        }
    "
    );
    assert!(typecheck(s).is_ok());
}

#[test]
#[ignore = "prefixes not supported yet"]
fn logical_expr() {
    let s = src!(
        "
        function foo() -> Bool {
            (10 == 10) || (10 == 11)
        }

        filter-map test(r: u32) {
            define {
                p = 10.10.10.10/10;
            }


            apply { accept }
        }
    "
    );
    typecheck(s).unwrap();

    let s = src!(
        r#"
        function foo() -> Bool {
            (10 == 10) || ("hello" == 11)
        }

        filter-map test(r: u32) {
            define {
                p = 10.10.10.10/10;
            }

            apply { accept }
        }
    "#
    );
    typecheck(s).unwrap_err();
}

#[test]
#[ignore = "sending messages not supported yet"]
fn send_output_stream() {
    let s = src!(
        r#"
        output-stream stream contains Msg {
            foo: String
        }

        function hello() {
            stream.send(Msg {
                foo: "hello",
            });
        }
        
        filter-map test(r: u32) {
            apply { accept }
        }
    "#
    );
    typecheck(s).unwrap();

    let s = src!(
        r#"
        output-stream stream contains Foo {
            foo: String
        }

        type Bar { bar: String }

        function hello() {
            stream.send(Bar {
                bar: "hello",
            });
        }

        filter-map test(r: u32) {
            apply { accept }
        }
    "#
    );
    assert!(typecheck(s).is_err());

    let s = src!(
        r#"
        output-stream foos contains Foo {
            foo: String
        }

        output-stream bars contains Bar {
            bar: String
        }
        
        function hello() {
            foos.send(Foo {
                foo: "hello",
            });
            bars.send(Bar {
                bar: "world",
            });
        }

        filter-map test(r: u32) {
            apply { accept }
        }
    "#
    );
    typecheck(s).unwrap();

    let s = src!(
        r#"
        output-stream foos contains Foo {
            foo: String
        }

        output-stream bars contains Bar {
            bar: String
        }

        function hello() {
            foos.send(Foo {
                foo: "hello",
            });
            bars.send(Foo {
                foo: "world",
            });
        }
        
        filter-map test(r: u32) {
            apply { accept }
        }
    "#
    );
    assert!(typecheck(s).is_err());
}

#[test]
#[ignore = "sending messages is not supported yet"]
fn record_inference() {
    let s = src!(
        "
        output-stream s contains Msg { a: u32 }
        
        function bar(a: Msg) {
            s.send(a);
        }
        
        filter-map foo(r: u32) { 
            define {
                a = { a: 8 };
            }

            apply {
                bar(a);
                accept
            }
        }
    "
    );
    typecheck(s).unwrap();

    let s = src!(
        "
        output-stream s contains Msg { a: u32 }
        
        function bar(a: Msg) {
            s.send(a);
        }

        filter-map foo(r: u32) { 
            define {
                a = { b: 8 };
            }

            apply {
                bar(a);
                accept
            }
        }
    "
    );
    assert!(typecheck(s).is_err());

    let s = src!(
        "
        type A { a: u32 }

        function bla(a: A, b: A) -> Bool {
            a == b
        }
        
        filter-map foo(r: u32) { 
            define {
                a = { a: 8 };
                b = A { a: 8 };
            }

            apply { 
                if bla(a, b) {
                    accept
                }
                reject
            }
        }
    "
    );
    typecheck(s).unwrap();

    let s = src!(
        "
        type A { a: u32 }
        type B { a: u32 }

        function bla() -> Bool {
            a == b && a == c
        }

        filter-map foo(r: u32) { 
            define {
                a = { a: 8 };
                b = A { a: 8 };
                c = B { a: 8 };
            }

            apply { accept }
        }
    "
    );
    assert!(typecheck(s).is_err());
}

#[test]
fn return_keyword() {
    let s = src!(
        "
        function bar() -> bool {
            return true
        }
        
        filter-map foo(r: u32) { 
            apply { accept }
        }
    "
    );
    typecheck(s).unwrap();

    let s = src!(
        "
        function bar() -> bool {
            return 2
        }
        
        filter-map foo(r: u32) { 
            apply { accept }
        }
    "
    );
    typecheck(s).unwrap_err();
}

#[test]
fn unit_block() {
    let s = src!(
        "
        # workaround for not having a ()
        function unit() {}

        function bar() -> Bool {
            unit();
        }

        filter-map foo(r: u32) { 
            apply { accept }
        }
    "
    );
    typecheck(s).unwrap_err();
}

#[test]
fn unreachable_expression() {
    let s = src!(
        "
        function bar() -> Bool {
            return true;
            return false;
        }

        filter-map foo(r: u32) { 
            apply { accept }
        }
    "
    );
    typecheck(s).unwrap_err();
}

#[test]
fn enum_values() {
    let s = src!(
        "
        filter-map main(r: u32) { 
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
    "
    );
    typecheck(s).unwrap();
}

#[test]
fn enum_match() {
    let s = src!(
        "
        filter-map foo(r: u32) { 
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
    "
    );
    typecheck(s).unwrap();
}

#[test]
fn runtime_function() {
    let s = src!(
        "
        filter-map main(x: u32) {
            apply {
                if pow(x, 2) > 100 {
                    accept
                } else {
                    reject
                }
            }
        }
    "
    );
    typecheck(s).unwrap();

    let s = src!(
        "
        filter-map main(x: u32) {
            define {
                pow = 3;
            }
            apply {
                if pow(x, 2) > 100 {
                    accept
                } else {
                    reject
                }
            }
        }
    "
    );
    typecheck(s).unwrap_err();
}

#[test]
fn issue_51() {
    let s = src!(
        "
        filter-map main() {
            apply {
                if true {
                    foo;
                }
                accept
            }
        }
    "
    );

    typecheck(s).unwrap_err();
}

#[test]
fn use_globals() {
    let s = src!(
        "
        filter-map main() {
            apply {
                accept BLACKHOLE
            }
        }
        "
    );

    typecheck(s).unwrap();
}
