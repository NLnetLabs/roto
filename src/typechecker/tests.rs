use crate::file_tree::FileTree;
use crate::pipeline::RotoReport;
use crate::runtime::tests::routecore_runtime;
use crate::{src, Context, Runtime};

#[track_caller]
fn typecheck(loaded: FileTree) -> Result<(), RotoReport> {
    typecheck_with_runtime(loaded, routecore_runtime().unwrap())
}

#[track_caller]
fn typecheck_with_runtime(
    loaded: FileTree,
    rt: Runtime,
) -> Result<(), RotoReport> {
    let res = loaded.parse();

    let res = match res {
        Ok(res) => res,
        Err(err) => {
            println!("{err}");
            panic!("Parse Error");
        }
    };

    let pointer_bytes = usize::BITS / 8;

    // Unwrap on parse because a parse error in this file is never correct.
    // We only want to test for type errors.
    if let Err(e) = res.typecheck(rt, pointer_bytes) {
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
#[ignore = "prefixes not supported yet"]
fn filter_map() {
    let s = src!(
        r#"
        filter-map blabla(foo: u32) {
            let a = "hello";
            let b = 0.0.0.0/10;
            let c = 192.168.0.0;

            accept
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
            let a = "hello";
            let a = 0.0.0.0/10;

            accept
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
            let a = Foo { a: "hello" };
            accept
        }
    "#
    );
    typecheck(s).unwrap();

    let s = src!(
        r#"
        type Foo { a: String }

        filter-map bar(r: u32) {
            let a = Foo { a: 0.0.0.0 };
            accept
        }
    "#
    );
    assert!(typecheck(s).is_err());

    let s = src!(
        r#"
        type Foo { a: string }

        filter-map bar(r: u32) {
            let a = Foo { };
            accept
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

        filter-map my_map(r: u32) {
            let foo = Foo { x: 5 };
            accept
        }
    "
    );
    typecheck(s).unwrap();

    let s = src!(
        "
        type Foo { x: u8 }
        type Bar { x: u8 }

        filter-map my_map(r: u32) {
            let a = 5;
            let foo = Foo { x: a };
            accept
        }
    "
    );
    assert!(typecheck(s).is_ok());

    let s = src!(
        "
        type Foo { x: u8 }
        type Bar { x: u8 }

        filter-map my_map(r: u32) {
            let a = 5;
            let foo = Foo { x: a };
            let bar = Bar { x: a };
            accept
        }
    "
    );
    assert!(typecheck(s).is_ok());

    let s = src!(
        "
        type Foo { x: u8 }
        type Bar { x: u32 }

        filter-map my_map(r: u32) {
            let a = 5;
            let foo = Foo { x: a };
            let bar = Bar { x: a };
            accept
        }
    "
    );
    assert!(typecheck(s).is_err());

    let s = src!(
        "
        type Foo { x: u8 }
        type Bar { x: u32 }

        filter-map my_map(r: u32) {
            let foo = Foo { x: 5 };
            let bar = Bar { x: 5 };
            accept
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

        filter-map my_map(r: u32) {
            let foo = Foo { x: 5 };
            let bar = Bar { x: foo.x };
            accept
        }
    "
    );
    typecheck(s).unwrap();

    let s = src!(
        "
        type Foo { x: u8 }
        type Bar { x: u8 }

        filter-map my_map(r: u32) {
            let foo = Foo { x: 5 };
            let bar = Bar { x: foo.y };
            accept
        }
    "
    );
    assert!(typecheck(s).is_err());

    let s = src!(
        "
        type Foo { x: u8 }
        type Bar { x: u32 }

        filter-map my_map(r: u32) {
            let foo = Foo { x: 5 };
            let bar = Bar { x: foo.x };
            accept
        }
    "
    );
    assert!(typecheck(s).is_err());
}

#[test]
fn ip_addr_method() {
    let s = src!(
        "
        filter-map my_map(r: u32) {
            let p = 10.10.10.10;
            let is_four = 1 + p.is_ipv4();
            accept
        }
    "
    );
    assert!(typecheck(s).is_err());

    let s = src!(
        "
        filter-map my_map(r: u32) {
            let p = 10.10.10.10;
            let is_four = true && p.is_ipv4();
            accept
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
        filter-map my_map(r: u32) {
            let p = 10.10.10.10;
            let x = p.to_canonical().is_ipv4();
            accept
        }
    "
    );
    assert!(typecheck(s).is_ok());

    let s = src!(
        "
        filter-map my_map(r: u32) {
            let p = 10.10.10.10;
            let x = p.is_ipv4().to_canonical();
            accept
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
        filter-map my_map(r: u32) {
            let p = 10.10.10.10/20;
            let add = p.address();
            accept
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

        filter-map my_map(r: u32) {
            let p = 10.10.10.10/10;
            accept
        }
    "
    );
    typecheck(s).unwrap();

    let s = src!(
        r#"
        function foo() -> Bool {
            (10 == 10) || ("hello" == 11)
        }

        filter-map my_map(r: u32) {
            let p = 10.10.10.10/10;
            accept
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
        
        filter-map my_map(r: u32) {
            accept
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

        filter-map my_map(r: u32) {
            accept
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

        filter-map my_map(r: u32) {
            accept
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
        
        filter-map my_map(r: u32) {
            accept
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
            let a = { a: 8 };
            bar(a);
            accept
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
            let a = { b: 8 };
            bar(a);
            accept
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
            let a = { a: 8 };
            let b = A { a: 8 };
            if bla(a, b) {
                accept
            }
            reject
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
            let a = { a: 8 };
            let b = A { a: 8 };
            let c = B { a: 8 };
            accept
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
            accept
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
            accept
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
            accept
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
            accept
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
            let x = Afi.IpV4;
            if x == Afi.IpV4 {
                accept
            } else {
                reject
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
            let x = Afi.IpV4;
            match x {
                IpV4 -> accept,
                IpV6 -> accept,
                _ -> reject,
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
            if pow(x, 2) > 100 {
                accept
            } else {
                reject
            }
        }
    "
    );
    typecheck(s).unwrap();

    let s = src!(
        "
        filter-map main(x: u32) {
            let pow = 3;
            if pow(x, 2) > 100 {
                accept
            } else {
                reject
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
            if true {
                foo;
            }
            accept
        }
    "
    );

    typecheck(s).unwrap_err();
}

#[test]
fn self_referential_variable() {
    let s = src!(
        "
        filter-map main() {
            let a = a;
            accept
        }
    "
    );

    typecheck(s).unwrap_err();
}

#[test]
fn reference_variable_later_declared() {
    let s = src!(
        "
        filter-map main() {
            let a = b;
            let b = 12;
            accept
        }
    "
    );

    typecheck(s).unwrap_err();
}

#[test]
fn reference_variable_from_parent_scope() {
    let s = src!(
        "
        filter-map main() {
            let a = 10;
            if true {
                accept a
            }
            let b = 20;
            accept b
        }
    "
    );

    typecheck(s).unwrap();
}

#[test]
fn reference_variable_from_child_scope() {
    let s = src!(
        "
        filter-map main() {
            if true {
                let a = 10;
            }
            let b = a;
            accept
        }
    "
    );

    typecheck(s).unwrap_err();
}

#[test]
fn shadow_variable() {
    let s = src!(
        "
        filter-map main() {
            let a = 10;
            let a = a + 1;
            accept
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
            accept BLACKHOLE
        }
        "
    );

    typecheck(s).unwrap();
}

#[test]
fn use_context() {
    let mut rt = Runtime::basic().unwrap();

    #[derive(Context)]
    struct Ctx {
        /// boop
        pub foo: u8,
    }

    rt.register_context_type::<Ctx>().unwrap();

    let s = src!(
        "
        filter-map main() {
            accept foo;
        }
        "
    );

    typecheck_with_runtime(s, rt).unwrap();
}
