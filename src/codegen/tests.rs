use std::{net::IpAddr, sync::atomic::AtomicUsize};

use inetnum::{addr::Prefix, asn::Asn};
use roto_macros::{roto_function, roto_static_method};

use crate::{
    pipeline::Compiled, runtime::tests::routecore_runtime, src, Files,
    Runtime, Val, Verdict,
};

#[track_caller]
fn compile(f: Files) -> Compiled {
    let runtime = routecore_runtime().unwrap();
    compile_with_runtime(f, runtime)
}

#[track_caller]
fn compile_with_runtime(f: Files, runtime: Runtime) -> Compiled {
    let _ = env_logger::try_init();

    let pointer_bytes = usize::BITS / 8;

    let res = f
        .parse()
        .and_then(|x| x.typecheck(runtime, pointer_bytes))
        .map(|x| {
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
        filter-map main() {
            apply {
                accept
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call();
    dbg!(std::mem::size_of::<Verdict<(), ()>>());
    assert_eq!(res, Verdict::Accept(()));
}

#[test]
fn reject() {
    let s = src!(
        "
        filter-map main() {
            apply {
                reject
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call();
    assert_eq!(res, Verdict::Reject(()));
}

#[test]
fn equal_to_10() {
    let s = src!(
        "
        filter-map main(x: u32) {
            apply {
                if x == 10 {
                    accept
                } else {
                    reject
                }
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(u32,), Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call(5);
    assert_eq!(res, Verdict::Reject(()));

    let res = f.call(10);
    assert_eq!(res, Verdict::Accept(()));
}

#[test]
fn equal_to_10_with_function() {
    let s = src!(
        "
        function is_10(x: i32) -> bool {
            x == 10
        }
        
        filter-map main(x: i32) {
            apply {
                if is_10(x) {
                    accept
                } else {
                    reject
                }
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(i32,), Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call(5);
    assert_eq!(res, Verdict::Reject(()));

    let res = f.call(10);
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

        filter-map main(x: u32) {
            apply {
                if is_10(x) {
                    accept
                } else if equals(x, 20) {
                    accept
                } else {
                    reject
                }
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(u32,), Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    assert_eq!(f.call(5), Verdict::Reject(()));
    assert_eq!(f.call(10), Verdict::Accept(()));
    assert_eq!(f.call(15), Verdict::Reject(()));
    assert_eq!(f.call(20), Verdict::Accept(()));
}

#[test]
fn negation() {
    let s = src!(
        "
        filter-map main(x: i32) {
            apply {
                if not (x == 10) {
                    accept
                } else {
                    reject
                }
            }
        } 
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(i32,), Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    for x in 0..20 {
        let res = f.call(x);
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
        filter-map main(x: i32) {
            apply {
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
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(i32,), Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    for x in 0..100 {
        #[allow(clippy::manual_range_contains)]
        let expected =
            if (x > 10 && x < 20) || (x >= 30 && x <= 40) || x == 55 {
                Verdict::Accept(())
            } else {
                Verdict::Reject(())
            };

        let res = f.call(x);
        assert_eq!(res, expected);
    }
}

#[test]
fn record() {
    let s = src!(
        "
        type Foo { a: i32, b: i32 }

        filter-map main(x: i32) {
            define {
                foo = Foo { a: x, b: 20 };
            }
            apply {
                if foo.a == foo.b {
                    accept
                } else {
                    reject
                }
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(i32,), Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    for x in 0..100 {
        let expected = if x == 20 {
            Verdict::Accept(())
        } else {
            Verdict::Reject(())
        };
        let res = f.call(x);
        assert_eq!(res, expected);
    }
}

#[test]
fn record_with_fields_flipped() {
    let s = src!(
        "
        type Foo { a: i32, b: i32 }

        filter-map main(x: i32) {
            define {
                # These are flipped, to ensure that the order in which
                # the fields are given doesn't matter:
                foo = Foo { b: 20, a: x };
            }
            apply {
                if foo.a == foo.b {
                    accept
                } else {
                    reject
                }
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(i32,), Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)")
        .into_func();

    for x in 0..100 {
        let expected = if x == 20 {
            Verdict::Accept(())
        } else {
            Verdict::Reject(())
        };
        let res = f(x);
        assert_eq!(res, expected);
    }
}

#[test]
fn nested_record() {
    let s = src!(
        "
        type Foo { x: Bar, y: Bar }
        type Bar { a: i32, b: i32 }

        filter-map main(x: i32) {
            define {
                bar = Bar { a: 20, b: x };
                foo = Foo { x: bar, y: bar };
            }
            apply {
                if foo.x.a == foo.y.b {
                    accept
                } else {
                    reject
                }
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(i32,), Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    for x in 0..100 {
        let expected = if x == 20 {
            Verdict::Accept(())
        } else {
            Verdict::Reject(())
        };
        let res = f.call(x);
        assert_eq!(res, expected, "for {x}");
    }
}

#[test]
fn misaligned_fields() {
    // A record where the second field should be aligned
    let s = src!(
        "
        type Foo { a: i16, b: i32 }

        filter-map main(x: i32) {
            define {
                foo = Foo { a: 10, b: x };
            }
            apply {
                if foo.b == 20 {
                    accept
                } else {
                    reject
                }
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(i32,), Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    for x in 0..100 {
        let expected = if x == 20 {
            Verdict::Accept(())
        } else {
            Verdict::Reject(())
        };
        let res = f.call(x);
        assert_eq!(res, expected, "for {x}");
    }
}

#[test]
fn enum_match() {
    let s = src!(
        "
        filter-map main(r: bool) { 
            define {
                x = if r {
                    Afi.IpV4
                } else {
                    Afi.IpV6
                };
            }

            apply {
                match x {
                    IpV4 -> accept,
                    _ -> reject,
                }
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(bool,), Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    assert_eq!(f.call(true), Verdict::Accept(()));
    assert_eq!(f.call(false), Verdict::Reject(()));
}

#[test]
fn arithmetic() {
    let s = src!(
        "
        filter-map main(x: i32) {
            apply {
                if x + 10 * 20 < 250 {
                    accept
                } else {
                    reject
                }
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(i32,), Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call(5);
    assert_eq!(res, Verdict::Accept(()));

    let res = f.call(20);
    assert_eq!(res, Verdict::Accept(()));

    let res = f.call(100);
    assert_eq!(res, Verdict::Reject(()));
}

#[test]
fn call_runtime_function() {
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

    let mut p = compile(s);
    let f = p
        .get_function::<(u32,), Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    for (value, expected) in
        [(5, Verdict::Reject(())), (11, Verdict::Accept(()))]
    {
        let res = f.call(value);
        assert_eq!(res, expected);
    }
}

#[test]
fn call_runtime_method() {
    let s = src!(
        "
        filter-map main(x: u32) {
            apply {
                if x.is_even() {
                    accept
                } else {
                    reject
                }
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(u32,), Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    for (value, expected) in
        [(5, Verdict::Reject(())), (10, Verdict::Accept(()))]
    {
        let res = f.call(value);
        assert_eq!(res, expected);
    }
}

#[test]
fn int_var() {
    let s = src!(
        "
        filter-map main() {
            apply {
                accept 32
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), Verdict<i32, ()>>("main")
        .expect("No function found (or mismatched types)");

    assert_eq!(f.call(), Verdict::Accept(32));
}

#[test]
fn issue_52() {
    #[derive(Clone)]
    struct Foo {
        _x: i32,
    }

    let mut rt = Runtime::basic().unwrap();
    rt.register_clone_type::<Foo>("A Foo!").unwrap();

    #[roto_static_method(rt, Foo)]
    fn bar(_x: u32) -> u32 {
        2
    }

    let s = src!(
        "
        filter-map main(foo: Foo) {
            apply {
                Foo.bar(1);
                accept
            }
        }
    "
    );

    let _p = compile_with_runtime(s, rt);
}

#[test]
fn issue_54() {
    let mut rt = Runtime::basic().unwrap();

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
        filter-map main(x: Asn) {
            apply {
                if x == AS1000 {
                    accept x
                } else {
                    reject x
                }
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(Asn,), Verdict<Asn, Asn>>("main")
        .expect("No function found (or mismatched types)");

    assert_eq!(
        f.call(Asn::from_u32(1000)),
        Verdict::Accept(Asn::from_u32(1000))
    );
    assert_eq!(
        f.call(Asn::from_u32(2000)),
        Verdict::Reject(Asn::from_u32(2000))
    );
}

#[test]
fn mismatched_types() {
    let s = src!(
        "
        filter-map main(x: i32) {
            apply {
                accept x
            }
        }
    "
    );

    let mut p = compile(s);

    let err = p
        .get_function::<(i8,), Verdict<i8, ()>>("main")
        .unwrap_err();

    eprintln!("{err}");
    assert!(err.to_string().contains("do not match"));
}

#[test]
fn multiply() {
    let s = src!(
        "
        filter-map main(x: u8) {
            apply {
                if x > 10 {
                    accept 2 * x
                } else {
                    reject
                }
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(u8,), Verdict<u8, ()>>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call(20);
    assert_eq!(res, Verdict::Accept(40));
}

#[test]
fn ip_output() {
    let s = src!(
        "
        filter-map main() {
            apply { accept 1.2.3.4 }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(), Verdict<IpAddr, ()>>("main")
        .expect("No function found (or mismatched types)");

    let ip = IpAddr::from([1, 2, 3, 4]);
    let res = f.call();
    assert_eq!(res, Verdict::Accept(ip));
}

#[test]
fn ip_passthrough() {
    let s = src!(
        "
        filter-map main(x: IpAddr) {
            apply { accept x }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(IpAddr,), Verdict<IpAddr, ()>>("main")
        .expect("No function found (or mismatched types)");

    let ip = IpAddr::from([1, 2, 3, 4]);
    let res = f.call(ip);
    assert_eq!(res, Verdict::Accept(ip));
}

#[test]
fn ipv4_compare() {
    let s = src!(
        "
        filter-map main(x: IpAddr) {
            apply { 
                if x == 0.0.0.0 {
                    accept x
                } else if x == 192.168.0.0 {
                    accept x
                } else {
                    reject x
                }
            }
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(IpAddr,), Verdict<IpAddr, IpAddr>>("main")
        .expect("No function found (or mismatched types)");

    let ip = IpAddr::from([0, 0, 0, 0]);
    let res = f.call(ip);
    assert_eq!(res, Verdict::Accept(ip));
    let ip = IpAddr::from([192, 168, 0, 0]);
    let res = f.call(ip);
    assert_eq!(res, Verdict::Accept(ip));

    let ip = IpAddr::from([1, 2, 3, 4]);
    let res = f.call(ip);
    assert_eq!(res, Verdict::Reject(ip));

    let ip = IpAddr::from([0, 0, 0, 0, 0, 0, 0, 0]);
    let res = f.call(ip);
    assert_eq!(res, Verdict::Reject(ip));
}

#[test]
fn ipv6_compare() {
    let s = src!(
        "
        filter-map main(x: IpAddr) {
            apply { 
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
        }
    "
    );

    let mut p = compile(s);
    let f = p
        .get_function::<(IpAddr,), Verdict<IpAddr, IpAddr>>("main")
        .expect("No function found (or mismatched types)");

    let ip = IpAddr::from([0, 0, 0, 0, 0, 0, 0, 0]);
    let res = f.call(ip);
    assert_eq!(res, Verdict::Accept(ip));

    let ip = IpAddr::from([0, 0, 0, 0, 0, 0, 0, 1]);
    let res = f.call(ip);
    assert_eq!(res, Verdict::Accept(ip));

    let ip = IpAddr::from([192, 168, 0, 0]);
    let res = f.call(ip);
    assert_eq!(res, Verdict::Accept(ip));

    let ip = IpAddr::from([1, 2, 3, 4]);
    let res = f.call(ip);
    assert_eq!(res, Verdict::Reject(ip));
}

#[test]
fn construct_prefix() {
    let s = src!(
        "
        filter-map main() {
            apply { 
                accept 192.168.0.0 / 16
            }
        }
    "
    );
    let mut p = compile(s);
    let f = p
        .get_function::<(), Verdict<Prefix, ()>>("main")
        .expect("No function found (or mismatched types)");

    let p = Prefix::new("192.168.0.0".parse().unwrap(), 16).unwrap();
    let res = f.call();
    assert_eq!(res, Verdict::Accept(p));
}

#[test]
fn function_returning_unit() {
    let mut runtime = Runtime::basic().unwrap();

    #[roto_function(runtime)]
    fn unit_unit() {}

    let s = src!(
        "
        filter-map main() {
            apply { 
                accept unit_unit()
            }
        }
    "
    );

    let mut p = compile_with_runtime(s, runtime);
    let f = p
        .get_function::<(), Verdict<(), ()>>("main")
        .expect("No function found (or mismatched types)");

    let res = f.call();
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

    let mut rt = Runtime::basic().unwrap();

    rt.register_clone_type::<CloneDrop>("A CloneDrop type")
        .unwrap();

    let s = src!(
        "
        filter-map main(choose: bool, x: CloneDrop, y: CloneDrop) {
            apply { 
                if choose {
                    accept x
                } else {
                    accept y
                }
            }
        }
    "
    );

    let mut p = s.compile(rt, usize::BITS / 8).unwrap();
    let f = p.get_function::<(bool, Val<CloneDrop>, Val<CloneDrop>), Verdict<Val<CloneDrop>, ()>>(
        "main",
    ).unwrap();

    let input = CloneDrop {
        clones: &CLONES,
        drops: &DROPS,
    };

    println!("{input:?}");

    let output = f.call(true, Val(input.clone()), Val(input));

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
        filter-map main() {
            define {
                safi = 127.0.0.1;
            }
            apply {
                if safi == LOCALHOSTV4 {
                    reject
                }
                accept
            }
        }"
    );

    let mut p = compile(s);
    let f = p.get_function::<(), Verdict<(), ()>>("main").unwrap();
    let output = f.call();
    assert_eq!(output, Verdict::Reject(()));
}
