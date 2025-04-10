use core::f32;
use std::{
    net::IpAddr,
    sync::{atomic::AtomicUsize, Arc},
};

use inetnum::{addr::Prefix, asn::Asn};
use roto_macros::{roto_function, roto_method, roto_static_method};

use crate::{
    file_tree::FileSpec, pipeline::Compiled,
    runtime::tests::routecore_runtime, source_file, src, Context, FileTree,
    Runtime, Val, Verdict,
};

#[track_caller]
fn compile(f: FileTree) -> Compiled {
    let runtime = routecore_runtime().unwrap();
    compile_with_runtime(f, runtime)
}

#[track_caller]
fn compile_with_runtime(f: FileTree, runtime: Runtime) -> Compiled {
    let _ = env_logger::try_init();

    let res = f.parse().and_then(|x| x.typecheck(runtime)).map(|x| {
        let x = x.lower();
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
        .get_function::<(), (), Verdict<(), ()>>("main")
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
        .get_function::<(), (), Verdict<(), ()>>("main")
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
        .get_function::<(), (), Verdict<(), ()>>("main")
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
        .get_function::<(), (u32,), Verdict<(), ()>>("main")
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
        function is_10(x: i32) -> bool {
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
        .get_function::<(), (i32,), Verdict<(), ()>>("main")
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
        function equals(x: u32, y: u32) -> bool {
            x == y
        }

        function is_10(x: u32) -> bool {
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
        .get_function::<(), (u32,), Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    assert_eq!(f.call(&mut (), 5), Verdict::Reject(()));
    assert_eq!(f.call(&mut (), 10), Verdict::Accept(()));
    assert_eq!(f.call(&mut (), 15), Verdict::Reject(()));
    assert_eq!(f.call(&mut (), 20), Verdict::Accept(()));
}

#[test]
fn negation() {
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
        .get_function::<(), (i32,), Verdict<(), ()>>("main")
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
        .get_function::<(), (i32,), Verdict<(), ()>>("main")
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
        .get_function::<(), (i32,), Verdict<(), ()>>("main")
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
        .get_function::<(), (i32,), Verdict<(), ()>>("main")
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
        .get_function::<(), (i32,), Verdict<(), ()>>("main")
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
        .get_function::<(), (i32,), Verdict<(), ()>>("main")
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
fn enum_match() {
    let s = src!(
        "
        filtermap main(r: bool) { 
            let x = if r {
                Afi.IpV4
            } else {
                Afi.IpV6
            };
            match x {
                IpV4 -> accept,
                _ -> reject,
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), (bool,), Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    assert_eq!(f.call(&mut (), true), Verdict::Accept(()));
    assert_eq!(f.call(&mut (), false), Verdict::Reject(()));
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
        .get_function::<(), (i32,), Verdict<(), ()>>("main")
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

    let mut p = compile(s);
    let f = p
        .get_function::<(), (u32,), Verdict<(), ()>>("main")
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

    let mut p = compile(s);
    let f = p
        .get_function::<(), (u32,), Verdict<(), ()>>("main")
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
        .get_function::<(), (), Verdict<i32, ()>>("main")
        .expect("No function found (or mismatched types)");

    assert_eq!(f.call(&mut ()), Verdict::Accept(32));
}

#[test]
fn issue_52() {
    #[derive(Clone)]
    struct Foo {
        _x: i32,
    }

    let mut rt = Runtime::new();
    rt.register_clone_type::<Foo>("A Foo!").unwrap();

    #[roto_static_method(rt, Foo)]
    fn bar(_x: u32) -> u32 {
        2
    }

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
fn issue_54() {
    let mut rt = Runtime::new();

    struct Foo {
        _x: i32,
    }
    extern "C" fn bar(_foo: *mut Foo, _x: u32) {}

    // We 'forget' to register type Foo:
    // rt.register_type::<Foo>().unwrap();

    // But we do register a method on it:
    rt.register_method::<Foo, _, _>("bar", bar as extern "C" fn(_, _) -> _)
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
        .get_function::<(), (Asn,), Verdict<Asn, Asn>>("main")
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
        .get_function::<(), (i8,), Verdict<i8, ()>>("main")
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
        .get_function::<(), (u8,), Verdict<u8, ()>>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), 20);
    assert_eq!(res, Verdict::Accept(40));
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
        .get_function::<(), (f32,), Verdict<f32, ()>>("main")
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
        .get_function::<(), (f32,), Verdict<f32, ()>>("main")
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
        .get_function::<(), (f32,), Verdict<f32, ()>>("main")
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
        .get_function::<(), (f32,), Verdict<bool, ()>>("main")
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
        .get_function::<(), (f32,), Verdict<f32, ()>>("main")
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
    let s = src!("function floor(x: f32) -> f32 { x.floor() }");
    let mut p = compile(s);
    let f = p
        .get_function::<(), (f32,), f32>("floor")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), 20.5);
    assert_eq!(res, 20.0);
    let res = f.call(&mut (), -20.5);
    assert_eq!(res, -21.0);
}

#[test]
fn float_ceil() {
    let s = src!("function ceil(x: f32) -> f32 { x.ceil() }");
    let mut p = compile(s);
    let f = p
        .get_function::<(), (f32,), f32>("ceil")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), 20.5);
    assert_eq!(res, 21.0);
    let res = f.call(&mut (), -20.5);
    assert_eq!(res, -20.0);
}

#[test]
fn float_round() {
    let s = src!("function round(x: f32) -> f32 { x.round() }");
    let mut p = compile(s);
    let f = p
        .get_function::<(), (f32,), f32>("round")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), 20.6);
    assert_eq!(res, 21.0);
    let res = f.call(&mut (), 20.4);
    assert_eq!(res, 20.0);
}

#[test]
fn float_pow() {
    let s = src!("function pow(x: f32, y: f32) -> f32 { x.pow(y) }");
    let mut p = compile(s);
    let f = p
        .get_function::<(), (f32, f32), f32>("pow")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut (), 2.0, 2.0);
    assert_eq!(res, 4.0);
    let res = f.call(&mut (), 25.0, 0.5);
    assert_eq!(res, 5.0);
}

#[test]
fn float_scientific_notation_one() {
    let s = src!("function main() -> f32 { 20.0e4 }");

    let mut p = compile(s);
    let f = p
        .get_function::<(), (), f32>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut ());
    assert_eq!(res, 20.0e4);
}

#[test]
fn float_scientific_notation_two() {
    let s = src!("function main() -> f32 { 20.0e-4 }");

    let mut p = compile(s);
    let f = p
        .get_function::<(), (), f32>("main")
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
        .get_function::<(), (f64,), Verdict<f64, ()>>("main")
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
        .get_function::<(), (), Verdict<IpAddr, ()>>("main")
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
        .get_function::<(), (IpAddr,), Verdict<IpAddr, ()>>("main")
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
        .get_function::<(), (IpAddr,), Verdict<IpAddr, IpAddr>>("main")
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
        .get_function::<(), (IpAddr,), Verdict<IpAddr, IpAddr>>("main")
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
        .get_function::<(), (), Verdict<Prefix, ()>>("main")
        .expect("No function found (or mismatched types)");

    let p = Prefix::new("192.168.0.0".parse().unwrap(), 16).unwrap();
    let res = f.call(&mut ());
    assert_eq!(res, Verdict::Accept(p));
}

#[test]
fn function_returning_unit() {
    let mut runtime = Runtime::new();

    #[roto_function(runtime)]
    fn unit_unit() {}

    let s = src!(
        "
        filtermap main() {
            accept unit_unit()
        }
    "
    );

    let mut p = compile_with_runtime(s, runtime);
    let f = p
        .get_function::<(), (), Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call(&mut ());
    assert_eq!(res, Verdict::Accept(()));
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

    let mut rt = Runtime::new();

    rt.register_clone_type::<CloneDrop>("A CloneDrop type")
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

    let mut p = s.compile(rt).unwrap();
    let f = p.get_function::<(), (bool, Val<CloneDrop>, Val<CloneDrop>), Verdict<Val<CloneDrop>, ()>>(
        "main",
    ).unwrap();

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
    let f = p.get_function::<(), (), Verdict<(), ()>>("main").unwrap();
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
    p.get_function::<(), (), Verdict<i32, ()>>("main")
        .unwrap_err();

    // And then with the correct ctx type
    let f = p.get_function::<Ctx, (), Verdict<i32, ()>>("main").unwrap();

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
        function double(x: i32) -> i32 {
            2 * x
        }"
    );

    let mut p = compile(s);
    let f = p.get_function::<(), (i32,), i32>("double").unwrap();
    let output = f.call(&mut (), 2);
    assert_eq!(output, 4);

    let output = f.call(&mut (), 16);
    assert_eq!(output, 32);
}

#[test]
fn use_a_test() {
    let s = src!(
        "
        function double(x: i32) -> i32 {
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
        function double(x: i32) -> i32 {
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
        .get_function::<(), (), Verdict<Arc<str>, ()>>("main")
        .unwrap();

    let res = f.call(&mut ());
    assert_eq!(res, Verdict::Accept("hello".into()));
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
        .get_function::<(), (Arc<str>,), Verdict<Arc<str>, ()>>("main")
        .unwrap();

    let res = f.call(&mut (), "Martin".into());
    assert_eq!(res, Verdict::Accept("Hello Martin!".into()));
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
        .get_function::<(), (Arc<str>,), Verdict<Arc<str>, ()>>("main")
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
        .get_function::<(), (Arc<str>,), Verdict<(), ()>>("main")
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
        .get_function::<(), (Arc<str>,), Verdict<(), ()>>("main")
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
        .get_function::<(), (Arc<str>,), Verdict<(), ()>>("main")
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
        .get_function::<(), (bool, Arc<str>), Verdict<Arc<str>, ()>>("main")
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
        .get_function::<(), (Arc<str>,), Verdict<Arc<str>, ()>>("main")
        .unwrap();

    let res = f.call(&mut (), "boo".into());
    assert_eq!(res, Verdict::Accept("BOO! BOO! BOO! BOO! BOO!".into()));
}

#[test]
fn match_optional_value() {
    let s = src!(
        "
        function or_fortytwo(x: u32?) -> u32 {
            match x {
                Some(x) -> x,
                None -> 42,
            }
        }
        "
    );

    let mut p = compile(s);

    let f = p
        .get_function::<(), (Option<u32>,), u32>("or_fortytwo")
        .unwrap();

    let res = f.call(&mut (), Some(10));
    assert_eq!(res, 10);
    let res = f.call(&mut (), None);
    assert_eq!(res, 42);
}

#[test]
fn construct_optional_value() {
    let s = src!(
        "
        function sub_one(x: u32) -> u32? {
            if x == 0 {
                Optional.None
            } else {
                Optional.Some(x - 1)
            }
        }
        "
    );

    let mut p = compile(s);

    let f = p
        .get_function::<(), (u32,), Option<u32>>("sub_one")
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
        .get_function::<(), (i32,), Verdict<i32, i32>>("foo")
        .unwrap();

    let res = f.call(&mut (), 0);
    assert_eq!(res, Verdict::Reject(10));
    let res = f.call(&mut (), 12);
    assert_eq!(res, Verdict::Accept(12));
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
        .get_function::<(), (i32,), Verdict<i32, i32>>("foo")
        .unwrap();

    let res = f.call(&mut (), 5);
    assert_eq!(res, Verdict::Reject(5));
    let res = f.call(&mut (), 15);
    assert_eq!(res, Verdict::Accept(16));
    let res = f.call(&mut (), 25);
    assert_eq!(res, Verdict::Accept(26));
}

#[test]
fn non_sugar_optional() {
    let s = src!(
        "
        function foo() -> Optional[u32] {
            Optional.Some(2)
        }
        "
    );

    let mut p = compile(s);
    let f = p.get_function::<(), (), Option<u32>>("foo").unwrap();

    let res = f.call(&mut ());
    assert_eq!(res, Some(2));
}

#[test]
fn top_level_import() {
    let pkg = source_file!(
        "pkg",
        "
            import foo.bar;
            function main(x: i32) -> i32 {
                bar(x)    
            }
        "
    );
    let foo = source_file!(
        "foo",
        "
            function bar(x: i32) -> i32 {
                2 * x
            }
        "
    );

    let tree = FileTree::file_spec(FileSpec::Directory(
        pkg,
        vec![FileSpec::File(foo)],
    ));
    let mut p = compile(tree);
    let main = p.get_function::<(), (i32,), i32>("main").unwrap();
    let res = main.call(&mut (), 4);
    assert_eq!(res, 8);
}

#[test]
fn local_import() {
    let pkg = source_file!(
        "pkg",
        "
            function main(x: i32) -> i32 {
                import foo.bar;
                bar(x)    
            }
        "
    );
    let foo = source_file!(
        "foo",
        "
            function bar(x: i32) -> i32 {
                2 * x
            }
        "
    );

    let tree = FileTree::file_spec(FileSpec::Directory(
        pkg,
        vec![FileSpec::File(foo)],
    ));
    let mut p = compile(tree);
    let main = p.get_function::<(), (i32,), i32>("main").unwrap();
    let res = main.call(&mut (), 4);
    assert_eq!(res, 8);
}

#[test]
fn parent_import() {
    let pkg = source_file!(
        "pkg",
        "
            import foo.quadruple;
            function main(x: i32) -> i32 {
                quadruple(x)
            }

            function double(x: i32) -> i32 {
                2 * x
            }
        "
    );
    let foo = source_file!(
        "foo",
        "
            import super.double;
            function quadruple(x: i32) -> i32 {
                double(double(x))
            }
        "
    );

    let tree = FileTree::file_spec(FileSpec::Directory(
        pkg,
        vec![FileSpec::File(foo)],
    ));
    let mut p = compile(tree);
    let main = p.get_function::<(), (i32,), i32>("main").unwrap();
    let res = main.call(&mut (), 4);
    assert_eq!(res, 16);
}

#[test]
fn package_import() {
    let pkg = source_file!(
        "pkg",
        "
            import foo.quadruple;
            function main(x: i32) -> i32 {
                quadruple(x)
            }

            function double(x: i32) -> i32 {
                2 * x
            }
        "
    );
    let foo = source_file!(
        "foo",
        "
            import pkg.double;
            function quadruple(x: i32) -> i32 {
                double(double(x))
            }
        "
    );

    let tree = FileTree::file_spec(FileSpec::Directory(
        pkg,
        vec![FileSpec::File(foo)],
    ));
    let mut p = compile(tree);
    let main = p.get_function::<(), (i32,), i32>("main").unwrap();
    let res = main.call(&mut (), 4);
    assert_eq!(res, 16);
}

#[test]
fn import_via_super() {
    let pkg = source_file!(
        "pkg",
        "
            import foo.a;
            function main(x: i32) -> i32 {
                a(x)  
            }
        "
    );
    let foo = source_file!(
        "foo",
        "
            import super.bar.b;
            function a(x: i32) -> i32 {
                b(x)
            }
        "
    );
    let bar = source_file!(
        "bar",
        "
            function b(x: i32) -> i32 {
                2 * x
            }
        "
    );

    let tree = FileTree::file_spec(FileSpec::Directory(
        pkg,
        vec![FileSpec::File(foo), FileSpec::File(bar)],
    ));
    let mut p = compile(tree);
    let main = p.get_function::<(), (i32,), i32>("main").unwrap();
    let res = main.call(&mut (), 4);
    assert_eq!(res, 8);
}

#[test]
fn import_module_first() {
    let pkg = source_file!(
        "pkg",
        "
            import foo.a;
            function main(x: i32) -> i32 {
                a(x)  
            }
        "
    );
    let foo = source_file!(
        "foo",
        "
            import super.bar;
            import bar.b;
            function a(x: i32) -> i32 {
                b(x)
            }
        "
    );
    let bar = source_file!(
        "bar",
        "
            function b(x: i32) -> i32 {
                2 * x
            }
        "
    );

    let tree = FileTree::file_spec(FileSpec::Directory(
        pkg,
        vec![FileSpec::File(foo), FileSpec::File(bar)],
    ));
    let mut p = compile(tree);
    let main = p.get_function::<(), (i32,), i32>("main").unwrap();
    let res = main.call(&mut (), 4);
    assert_eq!(res, 8);
}

#[test]
fn import_module_second() {
    let pkg = source_file!(
        "pkg",
        "
            import foo.a;
            function main(x: i32) -> i32 {
                a(x)  
            }
        "
    );
    let foo = source_file!(
        "foo",
        "
            import bar.b;
            import super.bar;
            function a(x: i32) -> i32 {
                b(x)
            }
        "
    );
    let bar = source_file!(
        "bar",
        "
            function b(x: i32) -> i32 {
                2 * x
            }
        "
    );

    let tree = FileTree::file_spec(FileSpec::Directory(
        pkg,
        vec![FileSpec::File(foo), FileSpec::File(bar)],
    ));
    let mut p = compile(tree);
    let main = p.get_function::<(), (i32,), i32>("main").unwrap();
    let res = main.call(&mut (), 4);
    assert_eq!(res, 8);
}

#[test]
fn use_type_from_module() {
    let pkg = source_file!(
        "pkg",
        "
            function main(x: i32) -> i32 {
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
    let main = p.get_function::<(), (i32,), i32>("main").unwrap();
    let res = main.call(&mut (), 4);
    assert_eq!(res, 4);
}

#[test]
fn use_imported_type() {
    let pkg = source_file!(
        "pkg",
        "
            import foo.Foo;
            function main(x: i32) -> i32 {
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
    let main = p.get_function::<(), (i32,), i32>("main").unwrap();
    let res = main.call(&mut (), 4);
    assert_eq!(res, 4);
}

#[test]
fn use_type_in_function_argument() {
    let pkg = source_file!(
        "pkg",
        "
            function main(x: i32) -> i32 {
                get_bar(foo.Foo { bar: x }) 
            }

            function get_bar(f: foo.Foo) -> i32 {
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
    let main = p.get_function::<(), (i32,), i32>("main").unwrap();
    let res = main.call(&mut (), 4);
    assert_eq!(res, 4);
}

#[test]
fn use_type_in_function_return_type() {
    let pkg = source_file!(
        "pkg",
        "
            function main(x: i32) -> i32 {
                make_foo(x).bar
            }

            function make_foo(x: i32) -> foo.Foo {
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
    let main = p.get_function::<(), (i32,), i32>("main").unwrap();
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

            function main(x: i32) -> i32 {
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
    let main = p.get_function::<(), (i32,), i32>("main").unwrap();
    let res = main.call(&mut (), 4);
    assert_eq!(res, 4);
}

#[test]
fn mutate() {
    let mut runtime = Runtime::new();

    #[derive(Copy, Clone, Debug)]
    struct MyType {
        i: i16,
    }

    runtime
        .register_copy_type::<*mut MyType>("my type")
        .unwrap();

    #[roto_method(runtime, *mut MyType)]
    fn increase(t: &*mut MyType) {
        eprintln!("increase, pre: {}", unsafe { (**t).i });
        unsafe { (**t).i += 1 };
        eprintln!("increase, post: {}", unsafe { (**t).i });
    }

    let s = src!(
        "
        filter main(t: MyType) {
            t.increase();
            accept t;
        }
    "
    );

    let mut p = compile_with_runtime(s, runtime);
    let f = p
        .get_function::<(), (roto::Val<*mut MyType>,) ,Verdict<roto::Val<*mut MyType>, ()>>("main")
        .expect("No function found (or mismatched types)");

    let mut t = MyType { i: 0 };
    let res = f.call(&mut (), roto::Val(&mut t));

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
    let mut runtime = Runtime::new();

    #[derive(Clone, Debug, Default)]
    #[allow(dead_code)]
    struct MyType {
        v: Vec<u8>,
    }

    runtime.register_clone_type::<MyType>("my type").unwrap();

    let s = src!(
        "
        function main(t: MyType) -> MyType {
            t
        }
    "
    );

    let mut p = compile_with_runtime(s, runtime);
    let f = p
        .get_function::<(), (roto::Val<MyType>,), roto::Val<MyType>>("main")
        .expect("No function found (or mismatched types)");

    let t = MyType { v: vec![0x01] };
    let res = f.call(&mut (), roto::Val(t));
    println!("Returned with {:?}", res.0.v.as_ptr());
}
