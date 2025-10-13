use crate::file_tree::{FileSpec, FileTree};
use crate::pipeline::RotoReport;
use crate::runtime::OptionCtx;
use crate::{Context, Runtime, Val, library, source_file, src};

#[track_caller]
fn typecheck(loaded: FileTree) -> Result<(), RotoReport> {
    typecheck_with_runtime(loaded, Runtime::new())
}

#[track_caller]
fn typecheck_with_runtime(
    loaded: FileTree,
    rt: Runtime<impl OptionCtx>,
) -> Result<(), RotoReport> {
    let res = loaded.parse();

    let res = match res {
        Ok(res) => res,
        Err(err) => {
            println!("{err}");
            panic!("Parse Error");
        }
    };

    // Unwrap on parse because a parse error in this file is never correct.
    // We only want to test for type errors.
    if let Err(e) = res.typecheck(&rt) {
        println!("{e}");
        Err(e)
    } else {
        Ok(())
    }
}

#[test]
fn empty() {
    let s = src!("");
    assert!(typecheck(s).is_ok());
}

#[test]
fn one_record() {
    let s = src!("record Foo { a: u32 }");
    assert!(typecheck(s).is_ok());
}

#[test]
fn declared_multiple_times() {
    let s = src!(
        "
        record Foo { a: u32 }
        record Foo { a: u32 }
    "
    );
    assert!(typecheck(s).is_err());
}

#[test]
fn double_field() {
    let s = src!(
        "
        record Foo { a: u32, a: u8 }
    "
    );
    assert!(typecheck(s).is_err());
}

#[test]
fn undeclared_type_in_record() {
    let s = src!("record Bar { f: Foo }");
    assert!(typecheck(s).is_err());
}

#[test]
fn nested_record() {
    let s = src!(
        "
        record Foo { a: { b: u32, c: u32 } }
    "
    );
    assert!(typecheck(s).is_ok());
}

#[test]
fn two_records() {
    let s = src!(
        "
        record Foo { a: u32 }
        record Bar { f: Foo }
    "
    );
    assert!(typecheck(s).is_ok());

    let s = src!(
        "
        record Bar { f: Foo }
        record Foo { a: u32 }
    "
    );
    assert!(typecheck(s).is_ok());
}

#[test]
fn record_cycle() {
    let s = src!(
        "
        record Foo { f: Foo }
    "
    );
    assert!(typecheck(s).is_err());

    let s = src!(
        "
        record Bar { f: Foo }
        record Foo { b: Bar }
    "
    );
    assert!(typecheck(s).is_err());

    let s = src!(
        "
        record A { x: B }
        record B { x: C }
        record C { x: D }
        record D { x: A }
    "
    );
    assert!(typecheck(s).is_err());

    let s = src!(
        "
        record A { x: B }
        record B { x: C }
        record C { x: { y: D } }
        record D { x: B }
    "
    );
    assert!(typecheck(s).is_err());
}

#[test]
fn record_diamond() {
    let s = src!(
        "
        record A { x: B, y: C }
        record B { x: D }
        record C { x: D }
        record D { }
    "
    );
    assert!(typecheck(s).is_ok());
}

#[test]
fn negation_on_unsigned_int_literal() {
    let s = src!(
        "
        fn negate() -> u32 {
            -5
        }
        "
    );

    assert!(typecheck(s).is_err());
}

#[test]
fn negation_unify_with_unsigned() {
    let s = src!(
        "
        fn negate(x: u32) -> u32 {
            let y = -5;
            x + y
        }
        "
    );

    assert!(typecheck(s).is_err());
}

#[test]
fn negation_on_unsigned_int() {
    let s = src!(
        "
        fn negate(x: u32) -> u32 {
            -x
        }
        "
    );

    assert!(typecheck(s).is_err());
}

#[test]
fn negation_on_string() {
    let s = src!(
        "
        fn negate(x: String) -> String {
            -x
        }
        "
    );

    assert!(typecheck(s).is_err());
}

#[test]
fn filter_map() {
    let s = src!(
        r#"
        filtermap blabla(foo: u32) {
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
        filtermap blabla(foo: u32) {
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
        record Foo { a: String }

        filtermap bar(r: u32) {
            let a = Foo { a: "hello" };
            accept
        }
    "#
    );
    typecheck(s).unwrap();

    let s = src!(
        r#"
        record Foo { a: String }

        filtermap bar(r: u32) {
            let a = Foo { a: 0.0.0.0 };
            accept
        }
    "#
    );
    assert!(typecheck(s).is_err());

    let s = src!(
        r#"
        record Foo { a: string }

        filtermap bar(r: u32) {
            let a = Foo { };
            accept
        }
    "#
    );
    assert!(typecheck(s).is_err());
}

#[test]
fn let_type_annotation() {
    let s = src!(
        "
        fn foo() {
            let foo: u32 = 5;
        }
    "
    );
    typecheck(s).unwrap();

    let s = src!(
        "
        fn foo() {
            let foo: u32 = true;
        }
    "
    );
    typecheck(s).unwrap_err();

    let s = src!(
        "
        fn foo() {
            let foo: String = \"abc\";
        }
    "
    );
    typecheck(s).unwrap();

    let s = src!(
        "
        fn foo() {
            let foo: String = 5;
        }
    "
    );
    typecheck(s).unwrap_err();
}

#[test]
fn integer_inference() {
    let s = src!(
        "
        record Foo { x: u8 }

        filtermap my_map(r: u32) {
            let foo = Foo { x: 5 };
            accept
        }
    "
    );
    typecheck(s).unwrap();

    let s = src!(
        "
        record Foo { x: u8 }
        record Bar { x: u8 }

        filtermap my_map(r: u32) {
            let a = 5;
            let foo = Foo { x: a };
            accept
        }
    "
    );
    assert!(typecheck(s).is_ok());

    let s = src!(
        "
        record Foo { x: u8 }
        record Bar { x: u8 }

        filtermap my_map(r: u32) {
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
        record Foo { x: u8 }
        record Bar { x: u32 }

        filtermap my_map(r: u32) {
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
        record Foo { x: u8 }
        record Bar { x: u32 }

        filtermap my_map(r: u32) {
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
        record Foo { x: u8 }
        record Bar { x: u8 }

        filtermap my_map(r: u32) {
            let foo = Foo { x: 5 };
            let bar = Bar { x: foo.x };
            accept
        }
    "
    );
    typecheck(s).unwrap();

    let s = src!(
        "
        record Foo { x: u8 }
        record Bar { x: u8 }

        filtermap my_map(r: u32) {
            let foo = Foo { x: 5 };
            let bar = Bar { x: foo.y };
            accept
        }
    "
    );
    assert!(typecheck(s).is_err());

    let s = src!(
        "
        record Foo { x: u8 }
        record Bar { x: u32 }

        filtermap my_map(r: u32) {
            let foo = Foo { x: 5 };
            let bar = Bar { x: foo.x };
            accept
        }
    "
    );
    assert!(typecheck(s).is_err());
}

#[test]
fn if_without_body() {
    let s = src!(
        "
           fn foo() {
               if true {}
           }
        "
    );

    assert!(typecheck(s).is_ok());
}

#[test]
fn ip_addr_method() {
    let s = src!(
        "
        filtermap my_map(r: u32) {
            let p = 10.10.10.10;
            let is_four = 1 + p.is_ipv4();
            accept
        }
    "
    );
    assert!(typecheck(s).is_err());

    let s = src!(
        "
        filtermap my_map(r: u32) {
            let p = 10.10.10.10;
            let is_four = true && p.is_ipv4();
            accept
        }
    "
    );
    assert!(typecheck(s).is_ok());
}

#[test]
fn ip_addr_method_of_method_return_type() {
    let s = src!(
        "
        filtermap my_map(r: u32) {
            let p = 10.10.10.10;
            let x = p.to_canonical().is_ipv4();
            accept
        }
    "
    );
    assert!(typecheck(s).is_ok());

    let s = src!(
        "
        filtermap my_map(r: u32) {
            let p = 10.10.10.10;
            let x = p.is_ipv4().to_canonical();
            accept
        }
    "
    );
    assert!(typecheck(s).is_err());
}

#[test]
fn prefix_method() {
    let s = src!(
        "
        filtermap my_map(r: u32) {
            let p = 10.10.10.10/20;
            let add = p.addr();
            accept
        }
    "
    );
    assert!(typecheck(s).is_ok());
}

#[test]
fn logical_expr() {
    let s = src!(
        "
        fn foo() -> bool {
            (10 == 10) || (10 == 11)
        }

        filtermap my_map(r: u32) {
            let p = 10.10.10.10/10;
            accept
        }
    "
    );
    typecheck(s).unwrap();

    let s = src!(
        r#"
        fn foo() -> Bool {
            (10 == 10) || ("hello" == 11)
        }

        filtermap my_map(r: u32) {
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

        fn hello() {
            stream.send(Msg {
                foo: "hello",
            });
        }
        
        filtermap my_map(r: u32) {
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

        record Bar { bar: String }

        fn hello() {
            stream.send(Bar {
                bar: "hello",
            });
        }

        filtermap my_map(r: u32) {
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
        
        fn hello() {
            foos.send(Foo {
                foo: "hello",
            });
            bars.send(Bar {
                bar: "world",
            });
        }

        filtermap my_map(r: u32) {
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

        fn hello() {
            foos.send(Foo {
                foo: "hello",
            });
            bars.send(Foo {
                foo: "world",
            });
        }
        
        filtermap my_map(r: u32) {
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
        
        fn bar(a: Msg) {
            s.send(a);
        }
        
        filtermap foo(r: u32) { 
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
        
        fn bar(a: Msg) {
            s.send(a);
        }

        filtermap foo(r: u32) { 
            let a = { b: 8 };
            bar(a);
            accept
        }
    "
    );
    assert!(typecheck(s).is_err());

    let s = src!(
        "
        record A { a: u32 }

        fn bla(a: A, b: A) -> Bool {
            a == b
        }
        
        filtermap foo(r: u32) { 
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
        record A { a: u32 }
        record B { a: u32 }

        fn bla() -> Bool {
            a == b && a == c
        }

        filtermap foo(r: u32) { 
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
        fn bar() -> bool {
            return true
        }
        
        filtermap foo(r: u32) { 
            accept
        }
    "
    );
    typecheck(s).unwrap();

    let s = src!(
        "
        fn bar() -> bool {
            return 2
        }
        
        filtermap foo(r: u32) { 
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
        fn unit() {}

        fn bar() -> Bool {
            unit();
        }

        filtermap foo(r: u32) { 
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
        fn bar() -> Bool {
            return true;
            return false;
        }

        filtermap foo(r: u32) { 
            accept
        }
    "
    );
    typecheck(s).unwrap_err();
}

#[test]
fn runtime_function() {
    let rt = Runtime::from_lib(library! {
        fn pow(x: u32, y: u32) -> u32 {
            x.pow(y)
        }
    })
    .unwrap();

    let s = src!(
        "
        filtermap main(x: u32) {
            if pow(x, 2) > 100 {
                accept
            } else {
                reject
            }
        }
    "
    );
    typecheck_with_runtime(s, rt.clone()).unwrap();

    let s = src!(
        "
        filtermap main(x: u32) {
            let pow = 3;
            if pow(x, 2) > 100 {
                accept
            } else {
                reject
            }
        }
    "
    );
    typecheck_with_runtime(s, rt).unwrap_err();
}

#[test]
fn issue_51() {
    let s = src!(
        "
        filtermap main() {
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
        filtermap main() {
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
        filtermap main() {
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
        filtermap main() {
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
        filtermap main() {
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
        filtermap main() {
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
    use routecore::bgp::communities::{Community, Wellknown};

    let s = src!(
        "
        filtermap main() {
            accept BLACKHOLE
        }
        "
    );

    let rt = Runtime::from_lib(library! {
        #[clone] type Community = Val<Community>;

        /// The well-known BLACKHOLE community
        const BLACKHOLE: Val<Community> = Val(Community::from(Wellknown::Blackhole));
    }).unwrap();

    typecheck_with_runtime(s, rt).unwrap();
}

#[test]
fn use_context() {
    #[derive(Clone, Context)]
    struct Ctx {
        /// boop
        pub foo: u8,
    }

    let rt = Runtime::new().with_context_type::<Ctx>().unwrap();

    let s = src!(
        "
        filtermap main() {
            accept foo;
        }
        "
    );

    typecheck_with_runtime(s, rt).unwrap();
}

#[test]
fn too_many_leading_super() {
    let s = src!(
        "
        import super.super;
        "
    );

    typecheck(s).unwrap_err();
}

#[test]
fn expected_module() {
    let pkg = source_file!(
        "pkg",
        "
            import foo.bar.baz;
        "
    );
    let foo = source_file!(
        "foo",
        "
            fn bar() {}
        "
    );

    let tree = FileTree::file_spec(FileSpec::Directory(
        pkg,
        vec![FileSpec::File(foo)],
    ));
    typecheck(tree).unwrap_err();
}

#[test]
fn silly_import_loop() {
    let pkg = source_file!(
        "pkg",
        "
            fn main(x: i32) -> i32 {
                foo.super.bar(x)    
            }

            fn bar(x: i32) -> i32 {
                2 * x
            }
        "
    );
    let foo = source_file!(
        "foo",
        "
        "
    );

    let tree = FileTree::file_spec(FileSpec::Directory(
        pkg,
        vec![FileSpec::File(foo)],
    ));
    typecheck(tree).unwrap_err();
}

#[test]
fn filtermap_calling_filtermap() {
    let s = src!(
        "
            filtermap foo() {
                accept 5
            }

            filtermap bar() {
                foo()
            }
        "
    );

    typecheck(s).unwrap();

    let s = src!(
        "
            filtermap bar() {
                foo()
            }

            filtermap foo() {
                accept 5
            }
        "
    );

    typecheck(s).unwrap();
}

#[test]
fn assignment() {
    let s = src!(
        "
            fn foo() -> i32 {
                let x = 4;
                x = x + 3;
                x
            }
        "
    );

    typecheck(s).unwrap();

    let s = src!(
        "
            fn foo() -> i32 {
                let x = false;
                x = x + 3;
                x
            }
        "
    );

    typecheck(s).unwrap_err();
}

#[test]
fn assignment_to_record() {
    let s = src!(
        "
            fn foo() -> i32 {
                let x = { bar: 4 };
                x.bar = x.bar + 3;
                x.bar
            }
        "
    );

    typecheck(s).unwrap();
}

#[test]
fn invalid_type_in_f_string() {
    let s = src!(
        r#"
            record Foo {
                x: i32,
            }

            fn foo() -> String {
                let y = Foo { x: 10 };
                f"Value is {y}"
            }
        "#
    );

    typecheck(s).unwrap_err();
}

#[test]
fn two_variants() {
    let s = src!(
        r#"
          variant Foo { Bar }
          variant Foo { Baz }  
        "#
    );
    typecheck(s).unwrap_err();
}

#[test]
fn assign_to_variant_with_different_type_param() {
    let s = src!(
        r#"
          variant Foo[T] { Bar(T) }

          fn foo() {
              let x: Foo[i32] = Foo.Bar(10);
              let y: Foo[f32] = x; # should error!
          }
        "#
    );
    typecheck(s).unwrap_err();
}

#[test]
fn variant_with_unused_type_param() {
    let s = src!(
        r#"
          variant Foo[T] { Bar }

          fn foo() {
              let x = Foo.Bar;
          }
        "#
    );
    typecheck(s).unwrap();
}

#[test]
fn variant_with_never_type_param() {
    let s = src!(
        r#"
          variant Foo[T] { Bar }

          fn foo() {
              let x: Foo[!] = Foo.Bar;
          }
        "#
    );
    typecheck(s).unwrap();
}

#[test]
fn generic_record_contains_itself_but_not_quite() {
    let s = src!(
        "
        record Foo[T] {
            x: Foo[i32],
        }
    "
    );

    typecheck(s).expect_err("type cycle!");
}

#[test]
fn assign_generic_to_another() {
    let s = src!(
        "
        record Foo[T] {
            x: T,
        }

        fn main() {
            let x = Foo { x: 10 };
            let y: Foo[u64] = Foo { x: 20 };
            y = x;
        }
    "
    );

    typecheck(s).unwrap();
}

#[test]
fn assign_generic_to_another_type() {
    let s = src!(
        "
        record Foo[T] {
            x: T,
        }

        fn main() {
            let x: Foo[i32] = Foo { x: 10 };
            let y: Foo[u64] = Foo { x: 20 };
            y = x;
        }
    "
    );

    typecheck(s).unwrap_err();
}
