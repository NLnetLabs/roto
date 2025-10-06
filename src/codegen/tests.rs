use core::f32;
use std::{
    net::IpAddr,
    sync::{
        atomic::{AtomicI32, AtomicUsize, Ordering},
        Arc,
    },
};

use crate::{
    file_tree::FileSpec, library, pipeline::Package, source_file, src,
    Context, FileTree, Runtime, Val, Verdict,
};
use inetnum::{addr::Prefix, asn::Asn};

#[track_caller]
fn compile(f: FileTree) -> Package {
    let runtime = Runtime::new();
    compile_with_runtime(f, runtime)
}

#[track_caller]
fn compile_with_runtime(f: FileTree, runtime: Runtime) -> Package {
    #[cfg(feature = "logger")]
    let _ = env_logger::try_init();

    let res = f.parse().and_then(|x| x.typecheck(&runtime)).map(|x| {
        let x = x.lower_to_mir().lower_to_lir();
        x.codegen()
    });

    match res {
        Ok(x) => x,
        Err(err) => {
            println!("{err}");
            panic!("Compilation error: see above");
        }
    }
}

#[test]
fn unit() {
    let s = src!(
        "
        fn unit_expression() {
            if true {
                ()
            } else {
                ()
            }
        }
    "
    );
    let mut p = compile(s);
    let f = p
        .get_function::<(), fn() -> ()>("unit_expression")
        .expect("No function found (or mismatched types)");

    let _res = f.call(&mut ());
}

#[test]
fn unit_type() {
    let s = src!(
        "
        fn unit_type() -> () {
            let x: () = ();
            x
        }
    "
    );
    let mut p = compile(s);
    let f = p
        .get_function::<(), fn() -> ()>("unit_type")
        .expect("No function found (or mismatched types)");

    let _res = f.call(&mut ());
}

#[test]
fn accept() {
    let s = src!(
        "
        filtermap main() {
            accept
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn() -> Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut ());
    assert_eq!(res, Verdict::Accept(()));
}

#[test]
fn accept_with_semicolon() {
    let s = src!(
        "
        filtermap main() {
            accept;
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn() -> Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut ());
    assert_eq!(res, Verdict::Accept(()));
}

#[test]
fn reject() {
    let s = src!(
        "
        filtermap main() {
            reject
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn() -> Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut ());
    assert_eq!(res, Verdict::Reject(()));
}

#[test]
fn equal_to_10() {
    let s = src!(
        "
        filtermap main(x: u32) {
            if x == 10 {
                accept
            } else {
                reject
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(u32) -> Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), 5);
    assert_eq!(res, Verdict::Reject(()));

    let res = f.call(&mut (), 10);
    assert_eq!(res, Verdict::Accept(()));
}

#[test]
fn equal_to_10_with_function() {
    let s = src!(
        "
        fn is_10(x: i32) -> bool {
            x == 10
        }
        
        filtermap main(x: i32) {
            if is_10(x) {
                accept
            } else {
                reject
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(i32) -> Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), 5);
    assert_eq!(res, Verdict::Reject(()));

    let res = f.call(&mut (), 10);
    assert_eq!(res, Verdict::Accept(()));
}

#[test]
fn equal_to_10_with_two_functions() {
    let s = src!(
        "
        fn equals(x: u32, y: u32) -> bool {
            x == y
        }

        fn is_10(x: u32) -> bool {
            equals(x, 10)
        }

        filtermap main(x: u32) {
            if is_10(x) {
                accept
            } else if equals(x, 20) {
                accept
            } else {
                reject
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(u32) -> Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    assert_eq!(f.call(&mut (), 5), Verdict::Reject(()));
    assert_eq!(f.call(&mut (), 10), Verdict::Accept(()));
    assert_eq!(f.call(&mut (), 15), Verdict::Reject(()));
    assert_eq!(f.call(&mut (), 20), Verdict::Accept(()));
}

#[test]
fn negated_literal() {
    let s = src!(
        "
        fn negate() -> i32 {
            -5
        }
        "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn() -> i32>("negate")
        .expect("No function found (or mismatched types)");
    let res = f.call(&mut ());
    assert_eq!(res, -5)
}

#[test]
fn negation() {
    let s = src!(
        "
        fn negate(x: i32) -> i32 {
            -x
        }
        "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(i32) -> i32>("negate")
        .expect("No function found (or mismatched types)");
    let res = f.call(&mut (), 5);
    assert_eq!(res, -5)
}

#[test]
fn inversion() {
    let s = src!(
        "
        filtermap main(x: i32) {
            if not (x == 10) {
                accept
            } else {
                reject
            }
        } 
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(i32) -> Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    for x in 0..20 {
        let res = f.call(&mut (), x);
        let exp = if x != 10 {
            Verdict::Accept(())
        } else {
            Verdict::Reject(())
        };
        assert_eq!(res, exp, "{x}");
    }
}

#[test]
fn not_not() {
    let s = src!(
        "
        fn main(x: i32) -> bool {
            not not (x == 10)
        } 
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(i32) -> bool>("main")
        .expect("No function found (or mismatched types)");

    for x in 0..20 {
        let res = f.call(&mut (), x);
        assert_eq!(res, x == 10, "{x}");
    }
}

#[test]
fn a_bunch_of_comparisons() {
    let s = src!(
        "
        filtermap main(x: i32) {
            if (
                (x > 10 && x < 20)
                || (x >= 30 && x <= 40)
                || x == 55
            ){
                accept
            } else {
                reject
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(i32) -> Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    for x in 0..100 {
        #[allow(clippy::manual_range_contains)]
        let expected =
            if (x > 10 && x < 20) || (x >= 30 && x <= 40) || x == 55 {
                Verdict::Accept(())
            } else {
                Verdict::Reject(())
            };

        let res = f.call(&mut (), x);
        assert_eq!(res, expected);
    }
}

#[test]
fn record() {
    let s = src!(
        "
        type Foo { a: i32, b: i32 }

        filtermap main(x: i32) {
            let foo = Foo { a: x, b: 20 };
            if foo.a == foo.b {
                accept
            } else {
                reject
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(i32) -> Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    for x in 0..100 {
        let expected = if x == 20 {
            Verdict::Accept(())
        } else {
            Verdict::Reject(())
        };
        let res = f.call(&mut (), x);
        assert_eq!(res, expected);
    }
}

#[test]
fn record_with_fields_flipped() {
    let s = src!(
        "
        type Foo { a: i32, b: i32 }

        filtermap main(x: i32) {
            # These are flipped, to ensure that the order in which
            # the fields are given doesn't matter:
            let foo = Foo { b: 20, a: x };
            if foo.a == foo.b {
                accept
            } else {
                reject
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(i32) -> Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)")
        .into_func();

    for x in 0..100 {
        let expected = if x == 20 {
            Verdict::Accept(())
        } else {
            Verdict::Reject(())
        };
        let res = f(&mut (), x);
        assert_eq!(res, expected);
    }
}

#[test]
fn nested_record() {
    let s = src!(
        "
        type Foo { x: Bar, y: Bar }
        type Bar { a: i32, b: i32 }

        filtermap main(x: i32) {
            let bar = Bar { a: 20, b: x };
            let foo = Foo { x: bar, y: bar };
            if foo.x.a == foo.y.b {
                accept
            } else {
                reject
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(i32) -> Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    for x in 0..100 {
        let expected = if x == 20 {
            Verdict::Accept(())
        } else {
            Verdict::Reject(())
        };
        let res = f.call(&mut (), x);
        assert_eq!(res, expected, "for {x}");
    }
}

#[test]
fn misaligned_fields() {
    // A record where the second field should be aligned
    let s = src!(
        "
        type Foo { a: i16, b: i32 }

        filtermap main(x: i32) {
            let foo = Foo { a: 10, b: x };
            if foo.b == 20 {
                accept
            } else {
                reject
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(i32) -> Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    for x in 0..100 {
        let expected = if x == 20 {
            Verdict::Accept(())
        } else {
            Verdict::Reject(())
        };
        let res = f.call(&mut (), x);
        assert_eq!(res, expected, "for {x}");
    }
}

#[test]
fn arithmetic() {
    let s = src!(
        "
        filtermap main(x: i32) {
            if x + 10 * 20 < 250 {
                accept
            } else {
                reject
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(i32) -> Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), 5);
    assert_eq!(res, Verdict::Accept(()));

    let res = f.call(&mut (), 20);
    assert_eq!(res, Verdict::Accept(()));

    let res = f.call(&mut (), 100);
    assert_eq!(res, Verdict::Reject(()));
}

#[test]
fn call_runtime_function() {
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

    let rt = Runtime::from_lib(library! {
        fn pow(x: u32, y: u32) -> u32 {
            x.pow(y)
        }
    })
    .unwrap();

    let mut p = compile_with_runtime(s, rt);
    let f = p
        .get_function::<(), fn(u32) -> Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    for (value, expected) in
        [(5, Verdict::Reject(())), (11, Verdict::Accept(()))]
    {
        let res = f.call(&mut (), value);
        assert_eq!(res, expected);
    }
}

#[test]
fn call_runtime_method() {
    let s = src!(
        "
        filtermap main(x: u32) {
            if x.is_even() {
                accept
            } else {
                reject
            }
        }
    "
    );

    let rt = Runtime::from_lib(library! {
        impl u32 {
            fn is_even(x: u32) -> bool {
                x % 2 == 0
            }
        }
    })
    .unwrap();

    let mut p = compile_with_runtime(s, rt);
    let f = p
        .get_function::<(), fn(u32) -> Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    for (value, expected) in
        [(5, Verdict::Reject(())), (10, Verdict::Accept(()))]
    {
        let res = f.call(&mut (), value);
        assert_eq!(res, expected);
    }
}

#[test]
fn int_var() {
    let s = src!(
        "
        filtermap main() {
            accept 32
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn() -> Verdict<i32, ()>>("main")
        .expect("No function found (or mismatched types)");

    assert_eq!(f.call(&mut ()), Verdict::Accept(32));
}

#[test]
fn issue_52() {
    #[derive(Clone)]
    struct Foo {
        _x: i32,
    }

    let rt = Runtime::from_lib(library! {
        /// A Foo!
        #[clone] type Foo = Val<Foo>;

        impl Val<Foo> {
            fn bar(_x: u32) -> u32 {
                2
            }
        }
    })
    .unwrap();

    let s = src!(
        "
        filtermap main(foo: Foo) {
            Foo.bar(1);
            accept
        }
    "
    );

    let _p = compile_with_runtime(s, rt);
}

#[test]
fn register_with_non_registered_type() {
    #[derive(Clone)]
    struct Foo {
        _x: i32,
    }

    Runtime::from_lib(library! {
        fn bar(_foo: Val<Foo>, _x: u32) {}
    })
    .unwrap_err();
}

#[test]
fn asn() {
    let s = src!(
        "
        filtermap main(x: Asn) {
            if x == AS1000 {
                accept x
            } else {
                reject x
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(Asn) -> Verdict<Asn, Asn>>("main")
        .expect("No function found (or mismatched types)");

    assert_eq!(
        f.call(&mut (), Asn::from_u32(1000)),
        Verdict::Accept(Asn::from_u32(1000))
    );
    assert_eq!(
        f.call(&mut (), Asn::from_u32(2000)),
        Verdict::Reject(Asn::from_u32(2000))
    );
}

#[test]
fn mismatched_types() {
    let s = src!(
        "
        filtermap main(x: i32) {
            accept x
        }
    "
    );

    let mut p = compile(s);

    let err = p
        .get_function::<(), fn(i8) -> Verdict<i8, ()>>("main")
        .unwrap_err();

    eprintln!("{err}");
    assert!(err.to_string().contains("do not match"));
}

#[test]
fn multiply() {
    let s = src!(
        "
        filtermap main(x: u8) {
            if x > 10 {
                accept 2 * x
            } else {
                reject
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(u8) -> Verdict<u8, ()>>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), 20);
    assert_eq!(res, Verdict::Accept(40));
}

#[test]
fn remainder() {
    let s = src!(
        "
        fn remainder(x: u64, y: u64) -> u64 {
            while x > y {
                x = x - y;
            }
            x
        }     
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(u64, u64) -> u64>("remainder")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), 55, 10);
    assert_eq!(res, 5);
}

#[test]
fn factorial() {
    let s = src!(
        "
        fn factorial(x: u64) -> u64 {
            let i = 1;
            let n = 1;
            while i <= x {
                n = n * i;
                i = i + 1;
            }
            n
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(u64) -> u64>("factorial")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), 5);
    assert_eq!(res, 120);
}

#[test]
fn nested_while_loop() {
    let s = src!(
        "
        fn nested_while_loop() -> u64 {
            let res = 0;
            let x = 0;
            while x < 10 {
                let y = 0;
                while y < 10 {
                    res = res + 1;
                    y = y + 1;
                }
                x = x + 1;
            }
            res
        }
        "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn() -> u64>("nested_while_loop")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut ());
    assert_eq!(res, 100);
}

#[test]
fn repeat_string_manually() {
    let s = src!(
        "
        fn repeat(s: String, n: u64) -> String {
            let res = \"\";
            while n > 0 {
                res = res + s;
                n = n - 1;
            }
            res
        }
        "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(Arc<str>, u64) -> Arc<str>>("repeat")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), "foo".into(), 6);
    assert_eq!(res, "foofoofoofoofoofoo".into());
}

#[test]
fn float_mul() {
    let s = src!(
        "
        filtermap main(x: f32) {
            accept 2.0 * x
        }
        "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(f32) -> Verdict<f32, ()>>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), 20.0);
    assert_eq!(res, Verdict::Accept(40.0));
}

#[test]
fn float_add() {
    let s = src!(
        "
        filtermap main(x: f32) {
            accept 2.0 + x
        }
        "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(f32) -> Verdict<f32, ()>>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), 20.0);
    assert_eq!(res, Verdict::Accept(22.0));
}

#[test]
fn float_sub() {
    let s = src!(
        "
        filtermap main(x: f32) {
            accept 20.0 - x
        }
        "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(f32) -> Verdict<f32, ()>>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), 2.0);
    assert_eq!(res, Verdict::Accept(18.0));
}

#[test]
fn float_cmp() {
    let s = src!(
        "
        filtermap main(x: f32) {
            accept x == 20.0
        }
        "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(f32) -> Verdict<bool, ()>>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), 20.0);
    assert_eq!(res, Verdict::Accept(true));
}

#[test]
fn float_div_zero() {
    let s = src!(
        "
        filtermap main(x: f32) {
            accept x / 0.0
        }
        "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(f32) -> Verdict<f32, ()>>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), 20.0);
    assert_eq!(res, Verdict::Accept(f32::INFINITY));

    let res = f.call(&mut (), -20.0);
    assert_eq!(res, Verdict::Accept(-f32::INFINITY));

    let Verdict::Accept(res) = f.call(&mut (), 0.0) else {
        panic!("should have returned accept")
    };
    assert!(res.is_nan());
}

#[test]
fn float_floor() {
    let s = src!("fn floor(x: f32) -> f32 { x.floor() }");
    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(f32) -> f32>("floor")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), 20.5);
    assert_eq!(res, 20.0);
    let res = f.call(&mut (), -20.5);
    assert_eq!(res, -21.0);
}

#[test]
fn float_ceil() {
    let s = src!("fn ceil(x: f32) -> f32 { x.ceil() }");
    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(f32) -> f32>("ceil")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), 20.5);
    assert_eq!(res, 21.0);
    let res = f.call(&mut (), -20.5);
    assert_eq!(res, -20.0);
}

#[test]
fn float_round() {
    let s = src!("fn round(x: f32) -> f32 { x.round() }");
    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(f32) -> f32>("round")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), 20.6);
    assert_eq!(res, 21.0);
    let res = f.call(&mut (), 20.4);
    assert_eq!(res, 20.0);
}

#[test]
fn float_pow() {
    let s = src!("fn pow(x: f32, y: f32) -> f32 { x.pow(y) }");
    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(f32, f32) -> f32>("pow")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), 2.0, 2.0);
    assert_eq!(res, 4.0);
    let res = f.call(&mut (), 25.0, 0.5);
    assert_eq!(res, 5.0);
}

#[test]
fn float_scientific_notation_one() {
    let s = src!("fn main() -> f32 { 20.0e4 }");

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn() -> f32>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut ());
    assert_eq!(res, 20.0e4);
}

#[test]
fn float_scientific_notation_two() {
    let s = src!("fn main() -> f32 { 20.0e-4 }");

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn() -> f32>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut ());
    assert_eq!(res, 20.0e-4);
}

#[test]
fn float_add_f64() {
    let s = src!(
        "
        filtermap main(x: f64) {
            accept x + 20.0
        }
        "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(f64) -> Verdict<f64, ()>>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), 20.0);
    assert_eq!(res, Verdict::Accept(40.0));
}

#[test]
fn ip_output() {
    let s = src!(
        "
        filtermap main() {
            accept 1.2.3.4
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn() -> Verdict<IpAddr, ()>>("main")
        .expect("No function found (or mismatched types)");

    let ip = IpAddr::from([1, 2, 3, 4]);
    let res = f.call(&mut ());
    assert_eq!(res, Verdict::Accept(ip));
}

#[test]
fn ip_passthrough() {
    let s = src!(
        "
        filtermap main(x: IpAddr) {
            accept x
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(IpAddr) -> Verdict<IpAddr, ()>>("main")
        .expect("No function found (or mismatched types)");

    let ip = IpAddr::from([1, 2, 3, 4]);
    let res = f.call(&mut (), ip);
    assert_eq!(res, Verdict::Accept(ip));
}

#[test]
fn ipv4_compare() {
    let s = src!(
        "
        filtermap main(x: IpAddr) {
            if x == 0.0.0.0 {
                accept x
            } else if x == 192.168.0.0 {
                accept x
            } else {
                reject x
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(IpAddr) -> Verdict<IpAddr, IpAddr>>("main")
        .expect("No function found (or mismatched types)");

    let ip = IpAddr::from([0, 0, 0, 0]);
    let res = f.call(&mut (), ip);
    assert_eq!(res, Verdict::Accept(ip));
    let ip = IpAddr::from([192, 168, 0, 0]);
    let res = f.call(&mut (), ip);
    assert_eq!(res, Verdict::Accept(ip));

    let ip = IpAddr::from([1, 2, 3, 4]);
    let res = f.call(&mut (), ip);
    assert_eq!(res, Verdict::Reject(ip));

    let ip = IpAddr::from([0, 0, 0, 0, 0, 0, 0, 0]);
    let res = f.call(&mut (), ip);
    assert_eq!(res, Verdict::Reject(ip));
}

#[test]
fn ipv6_compare() {
    let s = src!(
        "
        filtermap main(x: IpAddr) {
            if x == :: {
                accept x
            } else if x == 192.168.0.0 {
                accept x
            } else if x == ::1 {
                accept x
            } else {
                reject x
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(IpAddr) -> Verdict<IpAddr, IpAddr>>("main")
        .expect("No function found (or mismatched types)");

    let ip = IpAddr::from([0, 0, 0, 0, 0, 0, 0, 0]);
    let res = f.call(&mut (), ip);
    assert_eq!(res, Verdict::Accept(ip));

    let ip = IpAddr::from([0, 0, 0, 0, 0, 0, 0, 1]);
    let res = f.call(&mut (), ip);
    assert_eq!(res, Verdict::Accept(ip));

    let ip = IpAddr::from([192, 168, 0, 0]);
    let res = f.call(&mut (), ip);
    assert_eq!(res, Verdict::Accept(ip));

    let ip = IpAddr::from([1, 2, 3, 4]);
    let res = f.call(&mut (), ip);
    assert_eq!(res, Verdict::Reject(ip));
}

#[test]
fn construct_prefix() {
    let s = src!(
        "
        filtermap main() {
            accept 192.168.0.0 / 16
        }
    "
    );
    let mut p = compile(s);
    let f = p
        .get_function::<(), fn() -> Verdict<Prefix, ()>>("main")
        .expect("No function found (or mismatched types)");

    let p = Prefix::new("192.168.0.0".parse().unwrap(), 16).unwrap();
    let res = f.call(&mut ());
    assert_eq!(res, Verdict::Accept(p));
}

#[test]
fn function_returning_unit() {
    let rt = Runtime::from_lib(library! {
        fn unit_unit() {}
    })
    .unwrap();

    let s = src!(
        "
        filtermap main() {
            accept unit_unit()
        }
    "
    );

    let mut p = compile_with_runtime(s, rt);
    let f = p
        .get_function::<(), fn() -> Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut ());
    assert_eq!(res, Verdict::Accept(()));
}

#[test]
fn to_string() {
    let s = src!(
        r#"
        fn foo() -> String {
            let a: u8 = 10;
            let b: i32 = 20;
            let c: f64 = 15.5;
            let d = false;
            let e = 1.1.1.1;
            let f = 1.1.0.0 / 16;
            let g = AS1000;
            let h = "foo";

            a.to_string()
            + " "
            + b.to_string()
            + " "
            + c.to_string()
            + " "
            + d.to_string()
            + " "
            + e.to_string()
            + " "
            + f.to_string()
            + " "
            + g.to_string()
            + " "
            + h.to_string()
        }     
    "#
    );

    let mut p = compile(s);
    let f = p.get_function::<(), fn() -> Arc<str>>("foo").unwrap();

    let res = f.call(&mut ());
    assert_eq!(res, "10 20 15.5 false 1.1.1.1 1.1.0.0/16 AS1000 foo".into());
}

#[test]
fn simple_f_string() {
    let s = src!(
        r#"
        fn foo(name: String) -> String {
            f"Hello {name}!"
        }
    "#
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(Arc<str>) -> Arc<str>>("foo")
        .unwrap();

    let res = f.call(&mut (), "John".into());
    assert_eq!(res, "Hello John!".into());
}

#[test]
fn simple_f_string_number() {
    let s = src!(
        r#"
        fn foo(x: i32) -> String {
            f"Hello {x}!"
        }
    "#
    );

    let mut p = compile(s);
    let f = p.get_function::<(), fn(i32) -> Arc<str>>("foo").unwrap();

    let res = f.call(&mut (), 10);
    assert_eq!(res, "Hello 10!".into());
}

#[test]
fn complex_f_string() {
    let s = src!(
        r#"
        fn foo() -> String {
            f"This is a string with { f"another string which prints {true}" }, isn't that wonderful?"
        }
        "#
    );

    let mut p = compile(s);
    let f = p.get_function::<(), fn() -> Arc<str>>("foo").unwrap();

    let res = f.call(&mut ());
    assert_eq!(res, "This is a string with another string which prints true, isn't that wonderful?".into());
}

#[test]
fn escape_curly_in_f_string() {
    let s = src!(
        r#"
        fn foo() -> String {
            f"Here is a single curly {{ and a closing one }}"
        }
        "#
    );

    let mut p = compile(s);
    let f = p.get_function::<(), fn() -> Arc<str>>("foo").unwrap();

    let res = f.call(&mut ());
    assert_eq!(res, "Here is a single curly { and a closing one }".into());
}

#[test]
fn unicode_val_in_f_string() {
    let s = src!(
        r#"
        fn foo() -> String {
            f"Here is an uppercase \u{41} and a lowercase \u{61}."
        }
        "#
    );

    let mut p = compile(s);
    let f = p.get_function::<(), fn() -> Arc<str>>("foo").unwrap();

    let res = f.call(&mut ());
    assert_eq!(res, "Here is an uppercase A and a lowercase a.".into());
}

#[test]
fn arc_type() {
    use std::sync::atomic::Ordering;

    static CLONES: AtomicUsize = AtomicUsize::new(0);
    static DROPS: AtomicUsize = AtomicUsize::new(0);

    #[derive(Debug)]
    struct CloneDrop {
        clones: &'static AtomicUsize,
        drops: &'static AtomicUsize,
    }

    impl Clone for CloneDrop {
        fn clone(&self) -> Self {
            self.clones.fetch_add(1, Ordering::Relaxed);
            Self {
                clones: self.clones,
                drops: self.drops,
            }
        }
    }

    impl Drop for CloneDrop {
        fn drop(&mut self) {
            self.drops.fetch_add(1, Ordering::Relaxed);
        }
    }

    let rt = Runtime::from_lib(library! {
        /// A special type for which we track the number of clones and drops
        #[clone] type CloneDrop = Val<CloneDrop>;
    })
    .unwrap();

    let s = src!(
        "
        filtermap main(choose: bool, x: CloneDrop, y: CloneDrop) {
            if choose {
                accept x
            } else {
                accept y
            }
        }
    "
    );

    let mut p = compile_with_runtime(s, rt);
    let f = p
        .get_function::<(), fn(
            bool,
            Val<CloneDrop>,
            Val<CloneDrop>,
        ) -> Verdict<Val<CloneDrop>, ()>>("main")
        .unwrap();

    let input = CloneDrop {
        clones: &CLONES,
        drops: &DROPS,
    };

    println!("{input:?}");

    let output = f.call(&mut (), true, Val(input.clone()), Val(input));

    let output = output.into_result().unwrap().0;
    println!("{output:?}");
    assert_eq!(
        output.clones.load(Ordering::Relaxed),
        output.drops.load(Ordering::Relaxed)
    );
}

#[test]
fn use_constant() {
    let s = src!(
        "
        filtermap main() {
            let safi = 127.0.0.1;
            if safi == LOCALHOSTV4 {
                reject
            }
            accept
        }"
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn() -> Verdict<(), ()>>("main")
        .unwrap();
    let output = f.call(&mut ());
    assert_eq!(output, Verdict::Reject(()));
}

#[test]
fn use_context() {
    #[derive(Context)]
    struct Ctx {
        pub foo: i32,
        pub bar: bool,
    }

    let mut rt = Runtime::new();
    rt.register_context_type::<Ctx>().unwrap();

    let s = src!(
        "
        filtermap main() {
            if bar {
                accept foo + 1
            } else {
                accept foo
            } 
        }"
    );

    let mut p = compile_with_runtime(s, rt);

    // Try getting the wrong ctx, which should fail
    p.get_function::<(), fn() -> Verdict<i32, ()>>("main")
        .unwrap_err();

    // And then with the correct ctx type
    let f = p
        .get_function::<Ctx, fn() -> Verdict<i32, ()>>("main")
        .unwrap();

    let mut ctx = Ctx { foo: 9, bar: false };
    let output = f.call(&mut ctx);
    assert_eq!(output, Verdict::Accept(9));

    let mut ctx = Ctx { foo: 10, bar: true };
    let output = f.call(&mut ctx);
    assert_eq!(output, Verdict::Accept(11));
}

#[test]
fn use_a_roto_function() {
    let s = src!(
        "
        fn double(x: i32) -> i32 {
            2 * x
        }"
    );

    let mut p = compile(s);
    let f = p.get_function::<(), fn(i32) -> i32>("double").unwrap();
    let output = f.call(&mut (), 2);
    assert_eq!(output, 4);

    let output = f.call(&mut (), 16);
    assert_eq!(output, 32);
}

#[test]
fn use_a_test() {
    let s = src!(
        "
        fn double(x: i32) -> i32 {
            x # oops! not correct
        }
        
        test check_double {
            if double(4) != 8 {
                reject;
            }
            if double(16) != 32 {
                reject;
            }
            accept
        }
        "
    );

    let mut p = compile(s);
    p.run_tests(()).unwrap_err();

    let s = src!(
        "
        fn double(x: i32) -> i32 {
            2 * x
        }
        
        test check_double {
            if double(4) != 8 {
                reject;
            }
            if double(16) != 32 {
                reject;
            }
            accept
        }
        "
    );

    let mut p = compile(s);
    p.run_tests(()).unwrap();
}

#[test]
fn get_tests() {
    let s = src!(
        "
        test check_output {
            accept
        }
        "
    );

    let mut p = compile(s);
    let tests: Vec<_> = p.get_tests::<()>().collect();

    assert!(!tests.is_empty());
    assert_eq!(tests[0].name(), "pkg.check_output");
    tests[0].run(&mut ()).unwrap();
}

#[test]
fn get_no_tests() {
    let s = src!(
        "
        fn double(x: i32) -> i32 {
          2 * x
        }
        "
    );

    let mut p = compile(s);
    let tests = p.get_tests::<()>();
    assert_eq!(tests.count(), 0);
}

#[test]
fn string() {
    let s = src!(
        r#"
        filtermap main() {
            accept "hello" 
        }
    "#
    );

    let mut p = compile(s);

    let f = p
        .get_function::<(), fn() -> Verdict<Arc<str>, ()>>("main")
        .unwrap();

    let res = f.call(&mut ());
    assert_eq!(res, Verdict::Accept("hello".into()));
}

#[test]
fn escape_string() {
    let s = src!(
        r#"
        fn main() -> String {
            "\t\tfoo" 
        }
    "#
    );

    let mut p = compile(s);

    let f = p.get_function::<(), fn() -> Arc<str>>("main").unwrap();

    let res = f.call(&mut ());
    assert_eq!(res, "\t\tfoo".into());
}

#[test]
fn string_append() {
    let s = src!(
        r#"
        filtermap main(name: String) {
            accept "Hello ".append(name).append("!")
        }
    "#
    );

    let mut p = compile(s);

    let f = p
        .get_function::<(), fn(Arc<str>) -> Verdict<Arc<str>, ()>>("main")
        .unwrap();

    let res = f.call(&mut (), "Martin".into());
    assert_eq!(res, Verdict::Accept("Hello Martin!".into()));
}

#[test]
fn string_append_as_function_call() {
    let s = src!(
        r#"
        fn main(name: String) -> String {
            String.append("Hello ", name)
        }
    "#
    );

    let mut p = compile(s);

    let f = p
        .get_function::<(), fn(Arc<str>) -> Arc<str>>("main")
        .unwrap();

    let res = f.call(&mut (), "Martin".into());
    assert_eq!(res, "Hello Martin".into());
}

#[test]
fn string_append_as_imported_function() {
    let s = src!(
        r#"
        import String.append;

        fn main(name: String) -> String {
            append("Hello ", name)
        }
    "#
    );

    let mut p = compile(s);

    let f = p
        .get_function::<(), fn(Arc<str>) -> Arc<str>>("main")
        .unwrap();

    let res = f.call(&mut (), "Martin".into());
    assert_eq!(res, "Hello Martin".into());
}

#[test]
fn string_plus_operator() {
    let s = src!(
        r#"
        filtermap main(name: String) {
            accept "Hello " + name + "!"
        }
    "#
    );

    let mut p = compile(s);

    let f = p
        .get_function::<(), fn(Arc<str>) -> Verdict<Arc<str>, ()>>("main")
        .unwrap();

    let res = f.call(&mut (), "Martin".into());
    assert_eq!(res, Verdict::Accept("Hello Martin!".into()));
}

#[test]
fn string_contains() {
    let s = src!(
        r#"
        filtermap main(s: String) {
            if "incomprehensibilities".contains(s) {
                accept
            } else {
                reject
            }
        }
    "#
    );

    let mut p = compile(s);

    let f = p
        .get_function::<(), fn(Arc<str>) -> Verdict<(), ()>>("main")
        .unwrap();

    let res = f.call(&mut (), "incompre".into());
    assert_eq!(res, Verdict::Accept(()));

    let res = f.call(&mut (), "hensi".into());
    assert_eq!(res, Verdict::Accept(()));

    let res = f.call(&mut (), "bilities".into());
    assert_eq!(res, Verdict::Accept(()));

    let res = f.call(&mut (), "nananana".into());
    assert_eq!(res, Verdict::Reject(()));
}

#[test]
fn string_starts_with() {
    let s = src!(
        r#"
        filtermap main(s: String) {
            if "incomprehensibilities".starts_with(s) {
                accept
            } else {
                reject
            }
        }
    "#
    );

    let mut p = compile(s);

    let f = p
        .get_function::<(), fn(Arc<str>) -> Verdict<(), ()>>("main")
        .unwrap();

    let res = f.call(&mut (), "incompre".into());
    assert_eq!(res, Verdict::Accept(()));

    let res = f.call(&mut (), "hensi".into());
    assert_eq!(res, Verdict::Reject(()));

    let res = f.call(&mut (), "bilities".into());
    assert_eq!(res, Verdict::Reject(()));

    let res = f.call(&mut (), "nananana".into());
    assert_eq!(res, Verdict::Reject(()));
}

#[test]
fn string_ends_with() {
    let s = src!(
        r#"
        filtermap main(s: String) {
            if "incomprehensibilities".ends_with(s) {
                accept
            } else {
                reject
            }
        }
    "#
    );

    let mut p = compile(s);

    let f = p
        .get_function::<(), fn(Arc<str>) -> Verdict<(), ()>>("main")
        .unwrap();

    let res = f.call(&mut (), "incompre".into());
    assert_eq!(res, Verdict::Reject(()));

    let res = f.call(&mut (), "hensi".into());
    assert_eq!(res, Verdict::Reject(()));

    let res = f.call(&mut (), "bilities".into());
    assert_eq!(res, Verdict::Accept(()));

    let res = f.call(&mut (), "nananana".into());
    assert_eq!(res, Verdict::Reject(()));
}

#[test]
fn string_to_lowercase_and_uppercase() {
    let s = src!(
        r#"
        filtermap main(lower: bool, s: String) {
            if lower { 
                accept s.to_lowercase()
            } else {
                accept s.to_uppercase()
            }
        }
    "#
    );

    let mut p = compile(s);

    let f = p
        .get_function::<(), fn(bool, Arc<str>) -> Verdict<Arc<str>, ()>>(
            "main",
        )
        .unwrap();

    let res = f.call(&mut (), true, "WHISPER THIS!".into());
    assert_eq!(res, Verdict::Accept("whisper this!".into()));

    let res = f.call(&mut (), false, "now shout this!".into());
    assert_eq!(res, Verdict::Accept("NOW SHOUT THIS!".into()));
}

#[test]
fn string_repeat() {
    let s = src!(
        r#"
        filtermap main(s: String) {
            let exclamation = (s + "!").to_uppercase();
            accept (exclamation + " ").repeat(4) + exclamation 
        }
    "#
    );

    let mut p = compile(s);

    let f = p
        .get_function::<(), fn(Arc<str>) -> Verdict<Arc<str>, ()>>("main")
        .unwrap();

    let res = f.call(&mut (), "boo".into());
    assert_eq!(res, Verdict::Accept("BOO! BOO! BOO! BOO! BOO!".into()));
}

#[test]
fn match_option_value() {
    let s = src!(
        "
        fn or_fortytwo(x: u32?) -> u32 {
            match x {
                Some(x) -> x,
                None -> 42,
            }
        }
        "
    );

    let mut p = compile(s);

    let f = p
        .get_function::<(), fn(Option<u32>) -> u32>("or_fortytwo")
        .unwrap();

    let res = f.call(&mut (), Some(10));
    assert_eq!(res, 10);
    let res = f.call(&mut (), None);
    assert_eq!(res, 42);
}

#[test]
fn match_option_string() {
    let s = src!(
        "
        fn foo() -> String? {
            Option.Some(\"Foobar\")
        }

        fn bar() -> Verdict[String, String] {
            match foo() {
                Some(v) -> accept v,
                _ -> reject \"nope\",
            }
        }
        "
    );

    let mut p = compile(s);

    let f = p
        .get_function::<(), fn() -> Verdict<Arc<str>, Arc<str>>>("bar")
        .unwrap();

    let res = f.call(&mut ());
    assert_eq!(res, Verdict::Accept("Foobar".into()));
}

#[test]
fn match_on_string_with_guards() {
    let s = src!(
        r#"
        fn foo(s: String?, x: i32) -> String {
            match s {
                Some(s) | s == "hello" -> "hey!",
                _ | x == 5 ->  "x is 5",
                Some(s) | s.starts_with("lorem ipsum") -> {
                    "You've generated lorem ipsum: " + s
                }
                _ -> "Can't recognize",
            }
        }
        "#
    );

    let mut p = compile(s);

    let f = p
        .get_function::<(), fn(Option<Arc<str>>, i32) -> Arc<str>>("foo")
        .unwrap();

    let res = f.call(&mut (), Some("hello".into()), 5);
    assert_eq!(&*res, "hey!");
    let res = f.call(&mut (), Some("hello".into()), 4);
    assert_eq!(&*res, "hey!");

    let res = f.call(&mut (), Some("lorem ipsum dolor".into()), 5);
    assert_eq!(&*res, "x is 5");
    let res = f.call(&mut (), Some("lorem ipsum dolor".into()), 4);
    assert_eq!(&*res, "You've generated lorem ipsum: lorem ipsum dolor");

    let res = f.call(&mut (), Some("nothing".into()), 5);
    assert_eq!(&*res, "x is 5");
    let res = f.call(&mut (), Some("nothing".into()), 4);
    assert_eq!(&*res, "Can't recognize");
}

#[test]
fn construct_option_value() {
    let s = src!(
        "
        fn sub_one(x: u32) -> u32? {
            if x == 0 {
                Option.None
            } else {
                Option.Some(x - 1)
            }
        }
        "
    );

    let mut p = compile(s);

    let f = p
        .get_function::<(), fn(u32) -> Option<u32>>("sub_one")
        .unwrap();

    let res = f.call(&mut (), 0);
    assert_eq!(res, None);
    let res = f.call(&mut (), 2);
    assert_eq!(res, Some(1));
}

#[test]
fn construct_imported_option() {
    let s = src!(
        "
        import Option.{Some, None};

        fn sub_one(x: u32) -> u32? {
            if x == 0 {
                None
            } else {
                Some(x - 1)
            }
        }
        "
    );

    let mut p = compile(s);

    let f = p
        .get_function::<(), fn(u32) -> Option<u32>>("sub_one")
        .unwrap();

    let res = f.call(&mut (), 0);
    assert_eq!(res, None);
    let res = f.call(&mut (), 2);
    assert_eq!(res, Some(1));
}

#[test]
fn construct_option_value_from_prelude() {
    let s = src!(
        "
        fn sub_one(x: u32) -> u32? {
            if x == 0 {
                None
            } else {
                Some(x - 1)
            }
        }
        "
    );

    let mut p = compile(s);

    let f = p
        .get_function::<(), fn(u32) -> Option<u32>>("sub_one")
        .unwrap();

    let res = f.call(&mut (), 0);
    assert_eq!(res, None);
    let res = f.call(&mut (), 2);
    assert_eq!(res, Some(1));
}

#[test]
fn filter_map_with_manual_verdict() {
    let s = src!(
        "
        filtermap foo(x: i32) {
            if x < 10 {
                return Verdict.Reject(10)
            } else {
                return Verdict.Accept(x)
            }
        }
        "
    );

    let mut p = compile(s);

    let f = p
        .get_function::<(), fn(i32) -> Verdict<i32, i32>>("foo")
        .unwrap();

    let res = f.call(&mut (), 0);
    assert_eq!(res, Verdict::Reject(10));
    let res = f.call(&mut (), 12);
    assert_eq!(res, Verdict::Accept(12));
}

/// This tests the type checker because it needs to infer a never type for Reject
#[test]
fn unused_accept() {
    let s = src!(
        "
        fn foo() {
            Verdict.Accept(5);
        }    
        "
    );

    let mut p = compile(s);

    let f = p.get_function::<(), fn() -> ()>("foo").unwrap();

    let _res = f.call(&mut ());
}

#[test]
fn match_on_verdict() {
    let s = src!(
        "
        filtermap over_10(x: i32) {
            if x > 10 {
                accept x
            } else {
                reject x
            }
        }

        filtermap foo(x: i32) {
            match over_10(x) {
                Accept(x) -> accept x + 1,
                Reject(x) -> reject x,
            };
        }
        "
    );

    let mut p = compile(s);

    let f = p
        .get_function::<(), fn(i32) -> Verdict<i32, i32>>("foo")
        .unwrap();

    let res = f.call(&mut (), 5);
    assert_eq!(res, Verdict::Reject(5));
    let res = f.call(&mut (), 15);
    assert_eq!(res, Verdict::Accept(16));
    let res = f.call(&mut (), 25);
    assert_eq!(res, Verdict::Accept(26));
}

#[test]
fn non_sugar_option() {
    let s = src!(
        "
        fn foo() -> Option[u32] {
            Option.Some(2)
        }
        "
    );

    let mut p = compile(s);
    let f = p.get_function::<(), fn() -> Option<u32>>("foo").unwrap();

    let res = f.call(&mut ());
    assert_eq!(res, Some(2));
}

#[test]
fn none_with_unknown_type() {
    let s = src!(
        "
        fn foo() {
            Option.None;
        }
        "
    );

    let mut p = compile(s);
    let f = p.get_function::<(), fn() -> ()>("foo").unwrap();

    // We don't care about the output
    let _ = f.call(&mut ());
}

#[test]
fn question_mark() {
    let s = src!(
        "
        fn bar() -> u32? {
            Option.None
        }

        fn foo() -> u32? {
            bar()?;
            Option.Some(3)
        }
        "
    );

    let mut p = compile(s);
    let f = p.get_function::<(), fn() -> Option<u32>>("foo").unwrap();

    let res = f.call(&mut ());
    assert_eq!(res, None);
}

#[test]
fn question_mark_in_chain() {
    let s = src!(
        "
        fn is_finite(x: f64) -> f64? {
            if x.is_finite() {
                Option.Some(x)
            } else {
                Option.None
            }
        }

        fn foo(x: f64) -> f64? {
            let y = is_finite(x)?.pow(2.0);
            Option.Some(y)
        }
        "
    );

    let mut p = compile(s);
    let f = p.get_function::<(), fn(f64) -> Option<f64>>("foo").unwrap();

    let res = f.call(&mut (), 10.0);
    assert_eq!(res, Some(100.0));
    let res = f.call(&mut (), f64::NAN);
    assert_eq!(res, None);
}

#[test]
fn question_unit() {
    let s = src!(
        "
        fn maybe_a_unit(x: bool) -> ()? {
            if x {
                Some(())
            } else {
                None
            }
        }    
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(bool) -> Option<()>>("maybe_a_unit")
        .unwrap();

    let res = f.call(&mut (), false);
    assert_eq!(res, None);

    let res = f.call(&mut (), true);
    assert_eq!(res, Some(()));
}

#[test]
fn question_record() {
    let s = src!(
        "
        fn maybe_an_i32(x: bool) -> i32? {
            Some(maybe_a_record(x)?.a)
        }

        fn maybe_a_record(x: bool) -> { a: i32 }? {
            if x {
                Some({ a: 5 })
            } else {
                None
            }
        }    
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(bool) -> Option<i32>>("maybe_an_i32")
        .unwrap();

    let res = f.call(&mut (), false);
    assert_eq!(res, None);

    let res = f.call(&mut (), true);
    assert_eq!(res, Some(5));
}

#[test]
fn add_options() {
    let s = src!(
        "
        fn small(x: i32) -> i32? {
            if x < 10 {
                Option.Some(x)
            } else {
                Option.None
            }
        }

        fn foo(x: i32, y: i32) -> i32? {
            let z = small(x)? + small(y)?;
            Option.Some(z)
        }
        "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(i32, i32) -> Option<i32>>("foo")
        .unwrap();

    let res = f.call(&mut (), 2, 2);
    assert_eq!(res, Some(4));
    let res = f.call(&mut (), 5, 5);
    assert_eq!(res, Some(10));
    let res = f.call(&mut (), 15, 5);
    assert_eq!(res, None);
    let res = f.call(&mut (), 5, 10);
    assert_eq!(res, None);
}

#[test]
fn question_question() {
    let s = src!(
        "
        fn bar() -> u32?? {
            Option.Some(Option.None)
        }

        fn foo() -> u32? {
            bar()??;
            Option.Some(4)
        }
        "
    );

    let mut p = compile(s);
    let f = p.get_function::<(), fn() -> Option<u32>>("foo").unwrap();

    let res = f.call(&mut ());
    assert_eq!(res, None);
}

#[test]
fn question_mark_none() {
    let s = src!(
        "
        fn foo() -> u32? {
            Option.None?;
            Option.Some(3)
        }
        "
    );

    let mut p = compile(s);
    let f = p.get_function::<(), fn() -> Option<u32>>("foo").unwrap();

    let res = f.call(&mut ());
    assert_eq!(res, None);
}

#[test]
fn top_level_import() {
    let pkg = source_file!(
        "pkg",
        "
            import foo.bar;
            fn main(x: i32) -> i32 {
                bar(x)    
            }
        "
    );
    let foo = source_file!(
        "foo",
        "
            fn bar(x: i32) -> i32 {
                2 * x
            }
        "
    );

    let tree = FileTree::file_spec(FileSpec::Directory(
        pkg,
        vec![FileSpec::File(foo)],
    ));
    let mut p = compile(tree);
    let main = p.get_function::<(), fn(i32) -> i32>("main").unwrap();
    let res = main.call(&mut (), 4);
    assert_eq!(res, 8);
}

#[test]
fn local_import() {
    let pkg = source_file!(
        "pkg",
        "
            fn main(x: i32) -> i32 {
                import foo.bar;
                bar(x)    
            }
        "
    );
    let foo = source_file!(
        "foo",
        "
            fn bar(x: i32) -> i32 {
                2 * x
            }
        "
    );

    let tree = FileTree::file_spec(FileSpec::Directory(
        pkg,
        vec![FileSpec::File(foo)],
    ));
    let mut p = compile(tree);
    let main = p.get_function::<(), fn(i32) -> i32>("main").unwrap();
    let res = main.call(&mut (), 4);
    assert_eq!(res, 8);
}

#[test]
fn parent_import() {
    let pkg = source_file!(
        "pkg",
        "
            import foo.quadruple;
            fn main(x: i32) -> i32 {
                quadruple(x)
            }

            fn double(x: i32) -> i32 {
                2 * x
            }
        "
    );
    let foo = source_file!(
        "foo",
        "
            import super.double;
            fn quadruple(x: i32) -> i32 {
                double(double(x))
            }
        "
    );

    let tree = FileTree::file_spec(FileSpec::Directory(
        pkg,
        vec![FileSpec::File(foo)],
    ));
    let mut p = compile(tree);
    let main = p.get_function::<(), fn(i32) -> i32>("main").unwrap();
    let res = main.call(&mut (), 4);
    assert_eq!(res, 16);
}

#[test]
fn package_import() {
    let pkg = source_file!(
        "pkg",
        "
            import foo.quadruple;
            fn main(x: i32) -> i32 {
                quadruple(x)
            }

            fn double(x: i32) -> i32 {
                2 * x
            }
        "
    );
    let foo = source_file!(
        "foo",
        "
            import pkg.double;
            fn quadruple(x: i32) -> i32 {
                double(double(x))
            }
        "
    );

    let tree = FileTree::file_spec(FileSpec::Directory(
        pkg,
        vec![FileSpec::File(foo)],
    ));
    let mut p = compile(tree);
    let main = p.get_function::<(), fn(i32) -> i32>("main").unwrap();
    let res = main.call(&mut (), 4);
    assert_eq!(res, 16);
}

#[test]
fn import_via_super() {
    let pkg = source_file!(
        "pkg",
        "
            import foo.a;
            fn main(x: i32) -> i32 {
                a(x)  
            }
        "
    );
    let foo = source_file!(
        "foo",
        "
            import super.bar.b;
            fn a(x: i32) -> i32 {
                b(x)
            }
        "
    );
    let bar = source_file!(
        "bar",
        "
            fn b(x: i32) -> i32 {
                2 * x
            }
        "
    );

    let tree = FileTree::file_spec(FileSpec::Directory(
        pkg,
        vec![FileSpec::File(foo), FileSpec::File(bar)],
    ));
    let mut p = compile(tree);
    let main = p.get_function::<(), fn(i32) -> i32>("main").unwrap();
    let res = main.call(&mut (), 4);
    assert_eq!(res, 8);
}

#[test]
fn import_module_first() {
    let pkg = source_file!(
        "pkg",
        "
            import foo.a;
            fn main(x: i32) -> i32 {
                a(x)  
            }
        "
    );
    let foo = source_file!(
        "foo",
        "
            import super.bar;
            import bar.b;
            fn a(x: i32) -> i32 {
                b(x)
            }
        "
    );
    let bar = source_file!(
        "bar",
        "
            fn b(x: i32) -> i32 {
                2 * x
            }
        "
    );

    let tree = FileTree::file_spec(FileSpec::Directory(
        pkg,
        vec![FileSpec::File(foo), FileSpec::File(bar)],
    ));
    let mut p = compile(tree);
    let main = p.get_function::<(), fn(i32) -> i32>("main").unwrap();
    let res = main.call(&mut (), 4);
    assert_eq!(res, 8);
}

#[test]
fn import_module_second() {
    let pkg = source_file!(
        "pkg",
        "
            import foo.a;
            fn main(x: i32) -> i32 {
                a(x)  
            }
        "
    );
    let foo = source_file!(
        "foo",
        "
            import bar.b;
            import super.bar;
            fn a(x: i32) -> i32 {
                b(x)
            }
        "
    );
    let bar = source_file!(
        "bar",
        "
            fn b(x: i32) -> i32 {
                2 * x
            }
        "
    );

    let tree = FileTree::file_spec(FileSpec::Directory(
        pkg,
        vec![FileSpec::File(foo), FileSpec::File(bar)],
    ));
    let mut p = compile(tree);
    let main = p.get_function::<(), fn(i32) -> i32>("main").unwrap();
    let res = main.call(&mut (), 4);
    assert_eq!(res, 8);
}

#[test]
fn use_type_from_module() {
    let pkg = source_file!(
        "pkg",
        "
            fn main(x: i32) -> i32 {
                let foofoo = foo.Foo { bar: x };
                foofoo.bar
            }
        "
    );
    let foo = source_file!(
        "foo",
        "
            type Foo {
                bar: i32,
            }
        "
    );

    let tree = FileTree::file_spec(FileSpec::Directory(
        pkg,
        vec![FileSpec::File(foo)],
    ));
    let mut p = compile(tree);
    let main = p.get_function::<(), fn(i32) -> i32>("main").unwrap();
    let res = main.call(&mut (), 4);
    assert_eq!(res, 4);
}

#[test]
fn use_imported_type() {
    let pkg = source_file!(
        "pkg",
        "
            import foo.Foo;
            fn main(x: i32) -> i32 {
                let foofoo = Foo { bar: x };
                foofoo.bar
            }
        "
    );
    let foo = source_file!(
        "foo",
        "
            type Foo {
                bar: i32,
            }
        "
    );

    let tree = FileTree::file_spec(FileSpec::Directory(
        pkg,
        vec![FileSpec::File(foo)],
    ));
    let mut p = compile(tree);
    let main = p.get_function::<(), fn(i32) -> i32>("main").unwrap();
    let res = main.call(&mut (), 4);
    assert_eq!(res, 4);
}

#[test]
fn use_type_in_function_argument() {
    let pkg = source_file!(
        "pkg",
        "
            fn main(x: i32) -> i32 {
                get_bar(foo.Foo { bar: x }) 
            }

            fn get_bar(f: foo.Foo) -> i32 {
                f.bar
            }
        "
    );
    let foo = source_file!(
        "foo",
        "
            type Foo {
                bar: i32,
            }
        "
    );

    let tree = FileTree::file_spec(FileSpec::Directory(
        pkg,
        vec![FileSpec::File(foo)],
    ));
    let mut p = compile(tree);
    let main = p.get_function::<(), fn(i32) -> i32>("main").unwrap();
    let res = main.call(&mut (), 4);
    assert_eq!(res, 4);
}

#[test]
fn import_list() {
    let pkg = source_file!(
        "pkg",
        "
            import foo.{double, triple};
            fn main(x: i32) -> i32 {
                triple(double(x))
            }
        "
    );
    let foo = source_file!(
        "foo",
        "
            fn double(x: i32) -> i32 {
                2 * x
            }

            fn triple(x: i32) -> i32 {
                3 * x
            }
        "
    );

    let tree = FileTree::file_spec(FileSpec::Directory(
        pkg,
        vec![FileSpec::File(foo)],
    ));
    let mut p = compile(tree);
    let main = p.get_function::<(), fn(i32) -> i32>("main").unwrap();
    let res = main.call(&mut (), 1);
    assert_eq!(res, 6);
}

#[test]
fn use_type_in_function_return_type() {
    let pkg = source_file!(
        "pkg",
        "
            fn main(x: i32) -> i32 {
                make_foo(x).bar
            }

            fn make_foo(x: i32) -> foo.Foo {
                foo.Foo { bar: x }
            }
        "
    );
    let foo = source_file!(
        "foo",
        "
            type Foo {
                bar: i32,
            }
        "
    );

    let tree = FileTree::file_spec(FileSpec::Directory(
        pkg,
        vec![FileSpec::File(foo)],
    ));
    let mut p = compile(tree);
    let main = p.get_function::<(), fn(i32) -> i32>("main").unwrap();
    let res = main.call(&mut (), 4);
    assert_eq!(res, 4);
}

#[test]
fn use_type_from_other_module_in_type() {
    let pkg = source_file!(
        "pkg",
        "
            type Bli {
                bla: foo.Bla,
            }

            fn main(x: i32) -> i32 {
                Bli { bla: foo.Bla { blubb: x }}.bla.blubb
            }
        "
    );
    let foo = source_file!(
        "foo",
        "
            type Bla {
                blubb: i32,
            }
        "
    );

    let tree = FileTree::file_spec(FileSpec::Directory(
        pkg,
        vec![FileSpec::File(foo)],
    ));
    let mut p = compile(tree);
    let main = p.get_function::<(), fn(i32) -> i32>("main").unwrap();
    let res = main.call(&mut (), 4);
    assert_eq!(res, 4);
}

#[test]
fn mutate() {
    #[derive(Copy, Clone, Debug)]
    struct MyType {
        i: i16,
    }

    let rt = Runtime::from_lib(library! {
        #[copy] type MyType = Val<*mut MyType>;

        impl Val<*mut MyType> {
            fn increase(mut t: Val<*mut MyType>) {
                eprintln!("increase, pre: {}", unsafe { (**t).i });
                unsafe { (**t).i += 1 };
                eprintln!("increase, post: {}", unsafe { (**t).i });
            }
        }
    })
    .unwrap();

    let s = src!(
        "
        filter main(t: MyType) {
            t.increase();
            accept t;
        }
    "
    );

    let mut p = compile_with_runtime(s, rt);
    let f = p
        .get_function::<(), fn(Val<*mut MyType>) -> Verdict<Val<*mut MyType>, ()>>("main")
        .expect("No function found (or mismatched types)");

    let mut t = MyType { i: 0 };
    let res = f.call(&mut (), Val(&mut t));

    match res {
        Verdict::Accept(val) => {
            let val = unsafe { &*val.0 };
            assert_eq!(val.i, 1, "returned value should be 1")
        }
        Verdict::Reject(_) => todo!(),
    }

    assert_eq!(t.i, 1, "mutated value should be 1");
}

#[test]
fn return_vec() {
    #[derive(Clone, Debug, Default)]
    #[allow(dead_code)]
    struct MyType {
        v: Vec<u8>,
    }

    let rt = Runtime::from_lib(library! {
        #[clone] type MyType = Val<MyType>;
    })
    .unwrap();

    let s = src!(
        "
        fn main(t: MyType) -> MyType {
            t
        }
    "
    );

    let mut p = compile_with_runtime(s, rt);
    let f = p
        .get_function::<(), fn(Val<MyType>) -> Val<MyType>>("main")
        .expect("No function found (or mismatched types)");

    let t = MyType { v: vec![0x01] };
    let res = f.call(&mut (), roto::Val(t));
    println!("Returned with {:?}", res.0.v.as_ptr());
}

#[test]
fn name_collision() {
    #[derive(Clone)]
    struct A {
        _x: u32,
    }

    #[derive(Clone)]
    struct B {
        _x: u32,
    }

    for _ in 0..100 {
        let rt = Runtime::from_lib(library! {
            #[clone] type A = Val<A>;

            impl Val<A> {
                fn foo(_a: Val<A>) -> bool {
                    true
                }
            }

            #[clone] type B = Val<B>;

            impl Val<B> {
                fn foo(_b: Val<B>) -> bool {
                    unreachable!()
                }
            }
        })
        .unwrap();

        let s = src!(
            "
           filter foo(t: B) {
               accept t.foo()
           }

           filter main(t: A) {
               accept t.foo()
           }
       "
        );

        let mut p = compile_with_runtime(s, rt);
        let f = p
            .get_function::<(), fn(Val<A>) -> Verdict<bool, ()>>("main")
            .expect("No function found (or mismatched types)");

        let t = A { _x: 1 };
        let res = f.call(&mut (), roto::Val(t));
        assert_eq!(res, Verdict::Accept(true));
        eprint!(".");
    }
}

#[test]
fn refcounting_in_a_recursive_function() {
    #[derive(Debug, Clone)]
    struct Foo;

    let rt = Runtime::from_lib(library! {
        #[clone] type Foo = Val<Arc<Foo>>;
    })
    .unwrap();

    let s = src!(
        r##"
        fn f(foo: Foo, idx: i32) {
            if idx > 0 { f(foo, idx - 1); }
        }

        fn main(foo: Foo) -> Verdict[i32, i32] {
            f(foo, 1);
            reject 3
        }           
    "##
    );

    let mut p = compile_with_runtime(s, rt);
    let f = p
        .get_function::<(), fn(Val<Arc<Foo>>) -> Verdict<i32, i32>>("main")
        .unwrap();

    let v = Arc::new(Foo);
    let res = f.call(&mut (), Val(v.clone()));
    assert_eq!(res, Verdict::Reject(3));

    assert_eq!(Arc::strong_count(&v), 1);
}

#[test]
fn str_equals() {
    let s = src!(
        r#"
        fn is_slash(s: String) -> bool {
            s == "/"
        }
        "#
    );

    let mut compiled = compile(s);
    let func = compiled
        .get_function::<(), fn(Arc<str>) -> bool>("is_slash")
        .unwrap();

    assert!(func.call(&mut (), "/".into()));
    assert!(!func.call(&mut (), "foo".into()));
}

#[test]
fn str_not_equals() {
    let s = src!(
        r#"
        fn is_not_slash(s: String) -> bool {
            s != "/"
        }
        "#
    );

    let mut compiled = compile(s);
    let func = compiled
        .get_function::<(), fn(Arc<str>) -> bool>("is_not_slash")
        .unwrap();

    assert!(func.call(&mut (), "foo".into()));
    assert!(!func.call(&mut (), "/".into()));
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

    let mut compiled = compile(s);
    let func = compiled.get_function::<(), fn() -> i32>("foo").unwrap();

    assert_eq!(func.call(&mut ()), 7);
}

#[test]
fn assignment_record_field() {
    let s = src!(
        "
            fn foo() -> i32 {
                let x = { bar: 4 };
                x.bar = x.bar + 3;
                x.bar
            }
        "
    );

    let mut compiled = compile(s);
    let func = compiled.get_function::<(), fn() -> i32>("foo").unwrap();

    assert_eq!(func.call(&mut ()), 7);
}

#[test]
fn assignment_record() {
    let s = src!(
        "
            fn foo() -> i32 {
                let x = { bar: 4 };
                x = { bar: x.bar + 3 };
                x.bar
            }
        "
    );

    let mut compiled = compile(s);
    let func = compiled.get_function::<(), fn() -> i32>("foo").unwrap();

    assert_eq!(func.call(&mut ()), 7);
}

#[test]
fn assignment_record_is_by_value() {
    let s = src!(
        "
            fn foo() -> i32 {
                let x = { bar: 4 };
                let y = { bar: 5 };
                x = y;
                x.bar = 6;
                y.bar
            }
        "
    );

    let mut compiled = compile(s);
    let func = compiled.get_function::<(), fn() -> i32>("foo").unwrap();

    assert_eq!(func.call(&mut ()), 5);
}

#[test]
fn assignment_nested_record_1() {
    let s = src!(
        "
            fn foo() -> i32 {
                let x = { bar: { baz: 1 } };
                x.bar.baz = 6;
                x.bar.baz
            }
        "
    );

    let mut compiled = compile(s);
    let func = compiled.get_function::<(), fn() -> i32>("foo").unwrap();

    assert_eq!(func.call(&mut ()), 6);
}

#[test]
fn assignment_nested_record_2() {
    let s = src!(
        "
            fn foo() -> i32 {
                let x = { bar: { baz: 1 } };
                x.bar = { baz: 6 };
                x.bar.baz
            }
        "
    );

    let mut compiled = compile(s);
    let func = compiled.get_function::<(), fn() -> i32>("foo").unwrap();

    assert_eq!(func.call(&mut ()), 6);
}

#[test]
fn assignment_string() {
    let s = src!(
        "
            fn foo() -> String {
                let x = \"foo\";
                x = x + x;
                x = x + x;
                x
            }
        "
    );

    let mut compiled = compile(s);
    let func = compiled
        .get_function::<(), fn() -> Arc<str>>("foo")
        .unwrap();

    assert_eq!(func.call(&mut ()), "foofoofoofoo".into());
}

#[test]
fn let_declaration_is_by_value() {
    let s = src!(
        "
            fn foo() -> i32 {
                let y = { bar: 5 };
                let x = y;
                x.bar = 6;
                y.bar
            }
        "
    );

    let mut compiled = compile(s);
    let func = compiled.get_function::<(), fn() -> i32>("foo").unwrap();

    assert_eq!(func.call(&mut ()), 5);
}

#[test]
fn sigill() {
    struct Arcane(Arc<()>);

    impl Clone for Arcane {
        fn clone(&self) -> Self {
            assert!(Arc::strong_count(&self.0) > 0);
            Arcane(self.0.clone())
        }
    }

    impl Drop for Arcane {
        fn drop(&mut self) {
            assert!(Arc::strong_count(&self.0) > 0);
        }
    }

    let rt = Runtime::from_lib(library! {
        #[clone] type Arcane = Val<Arcane>;

        impl Val<Arcane> {
            fn get(a: Val<Arcane>) -> u64 {
                Arc::strong_count(&a.0.0) as u64
            }
        }

        fn make_arcane() -> Val<Arcane> {
            Val(Arcane(Arc::new(())))
        }
    })
    .unwrap();

    let s = src!(
        "
        fn bar(a: Arcane) -> u64 {
            a.get()
        }


        fn foo() -> Arcane {
            let a = make_arcane();
            bar(a);
            bar(a);
            a
        }
    "
    );

    let mut compiled = compile_with_runtime(s, rt);
    let func = compiled
        .get_function::<(), fn() -> Val<Arcane>>("foo")
        .unwrap();
    let Val(a) = func.call(&mut ());
    assert_eq!(Arc::strong_count(&a.0), 1);
}

/// The normal Rust string type will fail more often than `Arc<str>`
/// if we treat it wrong, so this test stress-tests Roto a bit with that
/// type.
#[test]
fn rust_string_string() {
    let rt = Runtime::from_lib(library! {
        #[clone] type RustString = Val<String>;

        impl Val<String> {
            fn new(s: Arc<str>) -> Val<String> {
                Val(s.as_ref().into())
            }
        }
    })
    .unwrap();

    let s = src!(
        "
            fn foo() -> RustString {
                let s = RustString.new(\"hello\");
                s = s;
                s
            }

            fn bar() -> RustString {
                let s = RustString.new(\"hello\");
                let a = { str: s };
                a.str = a.str;
                a.str
            }
        "
    );

    let mut compiled = compile_with_runtime(s, rt);
    let func = compiled
        .get_function::<(), fn() -> Val<String>>("foo")
        .unwrap();
    assert_eq!(func.call(&mut ()), Val("hello".into()));

    let func = compiled
        .get_function::<(), fn() -> Val<String>>("bar")
        .unwrap();
    assert_eq!(func.call(&mut ()), Val("hello".into()));
}

#[test]
fn return_verdict_from_runtime_function() {
    let rt = Runtime::from_lib(library! {
        fn foo() -> Verdict<(), ()> {
            Verdict::Accept(())
        }
    })
    .unwrap();

    let s = src!(
        "
        filtermap bar() {
            foo()
        }
    "
    );

    let mut compiled = compile_with_runtime(s, rt);
    let func = compiled
        .get_function::<(), fn() -> Verdict<(), ()>>("bar")
        .unwrap();

    assert_eq!(Verdict::Accept(()), func.call(&mut ()));
}

#[test]
fn string_global() {
    let s = src!(
        "fn use_foo() -> String {
            FOO
        }"
    );

    let rt = Runtime::from_lib(library! {
        const FOO: Arc<str> = "BAR".into();
    })
    .unwrap();

    let mut p = compile_with_runtime(s, rt);
    let f = p.get_function::<(), fn() -> Arc<str>>("use_foo").unwrap();

    assert_eq!(f.call(&mut ()), "BAR".into());
}

// Originally from issue #234
#[test]
fn layered_option_matching_none() {
    let s = src!(
        "
        fn reproducer() -> String? {
          let s = match None {
            Some(v) -> v,
            None -> { return None; },
          };
          Some(s)
        }
        "
    );
    let mut p = compile(s);
    let f = p
        .get_function::<(), fn() -> Option<Arc<str>>>("reproducer")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut ());
    assert_eq!(res, None);
}

#[test]
fn strings_from_if() {
    let s = src!(
        r#"
        fn foo(x: bool) -> String {
            if x { "true" } else { "false" }
        }
        "#
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(bool) -> Arc<str>>("foo")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), false);
    assert_eq!(res, "false".into());

    let res = f.call(&mut (), true);
    assert_eq!(res, "true".into());
}

#[test]
fn bool_is_true() {
    let s = src!(
        r#"
        fn foo(x: bool) -> bool {
            x == true
        }
        "#
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(bool) -> bool>("foo")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), false);
    assert_eq!(res, false);

    let res = f.call(&mut (), true);
    assert_eq!(res, true);
}

#[test]
fn bool_is_not_true() {
    let s = src!(
        r#"
        fn foo(x: bool) -> bool {
            x != true
        }
        "#
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), fn(bool) -> bool>("foo")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), false);
    assert_eq!(res, true);

    let res = f.call(&mut (), true);
    assert_eq!(res, false);
}

#[test]
fn register_on_optstr() {
    let rt = Runtime::from_lib(library! {
        #[clone] type OptStr = Val<Option<Arc<str>>>;

        impl Val<Option<Arc<str>>> {
            fn unwrap_or_empty(x: Val<Option<Arc<str>>>) -> Arc<str> {
                x.0.unwrap_or_default()
            }
        }
    })
    .unwrap();

    let s = src!(
        r#"
        fn foo(x: OptStr) -> String {
            x.unwrap_or_empty()
        }
    "#
    );

    let mut p = compile_with_runtime(s, rt);
    let f = p
        .get_function::<(), fn(Val<Option<Arc<str>>>) -> Arc<str>>("foo")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), Val(None));
    assert_eq!(res, "".into());

    let res = f.call(&mut (), Val(Some("hello".into())));
    assert_eq!(res, "hello".into());
}

#[test]
fn register_closure() {
    let some_number = 10;

    let rt = Runtime::from_lib(library! {
        let get = move || { some_number };
    })
    .unwrap();

    let s = src!(
        r#"
        fn foo() -> i32 {
            get()
        }
        "#
    );

    let mut p = compile_with_runtime(s, rt);
    let f = p
        .get_function::<(), fn() -> i32>("foo")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut ());
    assert_eq!(res, 10);
}

#[test]
fn increment_via_closure() {
    static COUNTER: AtomicI32 = AtomicI32::new(0);

    let runtime = Runtime::from_lib(library! {
        let inc = || {
            COUNTER.fetch_add(1, Ordering::Relaxed)
        };
    })
    .unwrap();

    let s = src!(
        r#"
        fn foo() {
            inc();
            inc();
        }
        "#
    );

    let mut p = compile_with_runtime(s, runtime);
    let f = p
        .get_function::<(), fn()>("foo")
        .expect("No function found (or mismatched types)");

    f.call(&mut ());
    assert_eq!(COUNTER.load(Ordering::Relaxed), 2);
}

#[test]
fn call_runtime_function_in_f_string() {
    let s = src!(
        r#"
       fn foo() -> String {
           f"foo{gimme_an_asn()}bar"
       }     
    "#
    );

    let rt = Runtime::from_lib(library! {
        fn gimme_an_asn() -> Asn {
            Asn::from_u32(2)
        }
    })
    .unwrap();

    let mut p = compile_with_runtime(s, rt);
    let f = p
        .get_function::<(), fn() -> Arc<str>>("foo")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut ());
    assert_eq!(res, "fooAS2bar".into());
}

#[test]
fn f32_issue() {
    let s = src!(
        r#"
       fn foo() {
           let x = 5.0;
           let y: f32 = 2.0;
           let z: f32 = x * y;
       }
    "#
    );

    let rt = Runtime::from_lib(library! {
        fn rand() -> f32 {
            10.0
        }
    })
    .unwrap();

    let mut p = compile_with_runtime(s, rt);
    let f = p
        .get_function::<(), fn()>("foo")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut ());
}
