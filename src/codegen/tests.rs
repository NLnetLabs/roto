use crate::{
    pipeline::{test_file, Compiled},
    runtime::tests::routecore_runtime,
};

#[track_caller]
fn compile(p: &'static str) -> Compiled {
    let _ = env_logger::try_init();

    let runtime = routecore_runtime().unwrap();
    let pointer_bytes = usize::BITS / 8;

    let res = test_file(file!(), p, line!() as usize)
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
    let s = "
        filter-map main() {
            apply {
                accept
            }
        }
    ";

    let p = compile(s);
    let f = p
        .module
        .get_function::<(*mut u8,), ()>("main")
        .expect("No function found (or mismatched types)");

    let mut verdict: u8 = 0;
    f.call((&mut verdict as *mut u8,));
    assert_eq!(verdict, 1);
}

#[test]
fn reject() {
    let s = "
        filter-map main() {
            apply {
                reject
            }
        }
    ";

    let p = compile(s);
    let f = p
        .module
        .get_function::<(*mut u8,), ()>("main")
        .expect("No function found (or mismatched types)");

    let mut verdict: u8 = 0;
    f.call((&mut verdict as *mut u8,));
    assert_eq!(verdict, 0);
}

#[test]
fn equal_to_10() {
    let s = "
        filter-map main(x: u32) {
            apply {
                if x == 10 {
                    accept
                } else {
                    reject
                }
            }
        }
    ";

    let p = compile(s);
    let f = p
        .module
        .get_function::<(*mut u8, u32), ()>("main")
        .expect("No function found (or mismatched types)");

    let mut verdict: u8 = 0;
    f.call((&mut verdict as *mut u8, 5));
    assert_eq!(verdict, 0);

    let mut verdict: u8 = 0;
    f.call((&mut verdict as *mut u8, 10));
    assert_eq!(verdict, 1);
}

#[test]
fn equal_to_10_with_function() {
    let s = "
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
    ";

    let p = compile(s);
    let f = p
        .module
        .get_function::<(*mut u8, i32), ()>("main")
        .expect("No function found (or mismatched types)");

    let mut verdict: u8 = 0;
    f.call((&mut verdict as *mut u8, 5));
    assert_eq!(verdict, 0);

    let mut verdict: u8 = 0;
    f.call((&mut verdict as *mut u8, 10));
    assert_eq!(verdict, 1);
}

#[test]
fn equal_to_10_with_two_functions() {
    let s = "
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
    ";

    let p = compile(s);
    let f = p
        .module
        .get_function::<(*mut u8, u32), ()>("main")
        .expect("No function found (or mismatched types)");

    let mut verdict: u8 = 0;
    f.call((&mut verdict as *mut u8, 5));
    assert_eq!(verdict, 0);

    let mut verdict: u8 = 0;
    f.call((&mut verdict as *mut u8, 10));
    assert_eq!(verdict, 1);

    let mut verdict: u8 = 0;
    f.call((&mut verdict as *mut u8, 15));
    assert_eq!(verdict, 0);

    let mut verdict: u8 = 0;
    f.call((&mut verdict as *mut u8, 20));
    assert_eq!(verdict, 1);
}

#[test]
fn negation() {
    let s = "
        filter-map main(x: i32) {
            apply {
                if not (x == 10) {
                    accept
                } else {
                    reject
                }
            }
        } 
    ";

    let p = compile(s);
    let f = p
        .module
        .get_function::<(*mut u8, i32), ()>("main")
        .expect("No function found (or mismatched types)");

    for x in 0..20 {
        let mut verdict: u8 = 0;
        f.call((&mut verdict as *mut _, x));
        assert_eq!(verdict, (x != 10) as u8, "{x}");
    }
}

#[test]
fn a_bunch_of_comparisons() {
    let s = "
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
    ";

    let p = compile(s);
    let f = p
        .module
        .get_function::<(*mut u8, i32), ()>("main")
        .expect("No function found (or mismatched types)");

    for x in 0..100 {
        #[allow(clippy::manual_range_contains)]
        let expected = (x > 10 && x < 20) || (x >= 30 && x <= 40) || x == 55;

        let mut verdict: u8 = 0;
        f.call((&mut verdict as *mut _, x));
        assert_eq!(verdict, expected as u8);
    }
}

#[test]
fn record() {
    let s = "
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
    ";

    let p = compile(s);
    let f = p
        .module
        .get_function::<(*mut u8, i32), ()>("main")
        .expect("No function found (or mismatched types)");

    for x in 0..100 {
        let expected = x == 20;
        let mut verdict: u8 = 0;
        f.call((&mut verdict as *mut _, x));
        assert_eq!(verdict, expected as u8);
    }
}

#[test]
fn record_with_fields_flipped() {
    let s = "
        type Foo { a: i32, b: i32 }

        filter-map main(x: i32) {
            define {
                // These are flipped, to ensure that the order in which
                // the fields are given doesn't matter:
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
    ";

    let p = compile(s);
    let f = p
        .module
        .get_function::<(*mut u8, i32), ()>("main")
        .expect("No function found (or mismatched types)");

    for x in 0..100 {
        let expected = x == 20;
        let mut verdict: u8 = 0;
        f.call((&mut verdict as *mut _, x));
        assert_eq!(verdict, expected as u8);
    }
}

#[test]
fn nested_record() {
    let s = "
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
    ";

    let p = compile(s);
    let f = p
        .module
        .get_function::<(*mut u8, i32), ()>("main")
        .expect("No function found (or mismatched types)");

    for x in 0..100 {
        let expected = x == 20;
        let mut verdict: u8 = 0;
        f.call((&mut verdict as *mut _, x));
        assert_eq!(verdict, expected as u8, "for {x}");
    }
}

#[test]
fn misaligned_fields() {
    // A record where the second field should be aligned
    let s = "
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
    ";

    let p = compile(s);
    let f = p
        .module
        .get_function::<(*mut u8, i32), ()>("main")
        .expect("No function found (or mismatched types)");

    for x in 0..100 {
        let expected = x == 20;
        let mut verdict: u8 = 0;
        f.call((&mut verdict as *mut _, x));
        assert_eq!(verdict, expected as u8);
    }
}

#[test]
fn enum_match() {
    let s = "
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
    ";

    let p = compile(s);
    let f = p
        .module
        .get_function::<(*mut u8, bool), ()>("main")
        .expect("No function found (or mismatched types)");

    let mut verdict: u8 = 0;
    f.call((&mut verdict as *mut _, true));
    assert_eq!(verdict, true as u8);

    let mut verdict: u8 = 0;
    f.call((&mut verdict as *mut _, false));
    assert_eq!(verdict, false as u8);
}

#[test]
fn arithmetic() {
    let s = "
        filter-map main(x: i32) {
            apply {
                if x + 10 * 20 < 250 {
                    accept
                } else {
                    reject
                }
            }
        }
    ";

    let p = compile(s);
    let f = p
        .module
        .get_function::<(*mut u8, i32), ()>("main")
        .expect("No function found (or mismatched types)");

    let mut verdict: u8 = 0;
    f.call((&mut verdict as *mut _, 5));
    assert_eq!(verdict, true as u8);

    let mut verdict: u8 = 0;
    f.call((&mut verdict as *mut _, 20));
    assert_eq!(verdict, true as u8);

    let mut verdict: u8 = 0;
    f.call((&mut verdict as *mut _, 100));
    assert_eq!(verdict, false as u8);
}

#[test]
fn call_runtime_function() {
    let s = "
        filter-map main(x: u32) {
            apply {
                if pow(x, 2) > 100 {
                    accept
                } else {
                    reject
                }
            }
        }
    ";

    let p = compile(s);
    let f = p
        .module
        .get_function::<(*mut u8, u32), ()>("main")
        .expect("No function found (or mismatched types)");

    for (value, expected) in [(5, 0), (11, 1)] {
        let mut verdict: u8 = 0;
        f.call((&mut verdict as *mut _, value));
        assert_eq!(verdict, expected);
    }
}

#[test]
fn call_runtime_method() {
    let s = "
        filter-map main(x: u32) {
            apply {
                if x.is_even() {
                    accept
                } else {
                    reject
                }
            }
        }
    ";

    let p = compile(s);
    let f = p
        .module
        .get_function::<(*mut u8, u32), ()>("main")
        .expect("No function found (or mismatched types)");

    for (value, expected) in [(5, 0), (10, 1)] {
        let mut verdict: u8 = 0;
        f.call((&mut verdict as *mut _, value));
        assert_eq!(verdict, expected);
    }
}

// #[test]
// fn bmp_message() {
//     let s = "
//     filter-map main(a: i32) {
//         define {
//             header = {
//                 is_ipv6: true,
//                 is_ipv4: true,
//                 is_legacy_format: false,
//                 is_post_policy: false,
//                 is_pre_policy: false,
//                 peer_type: 0,
//                 asn: 0,
//                 address: 1.1.1.1,
//             };
//             bmp = if a == 1 {
//                 BmpMessage.PeerUpNotification({
//                     local_port: 80,
//                     local_address: 1.1.1.1,
//                     remote_port: 80,
//                     per_peer_header: header,
//                 })
//             } else if a == 2 {
//                 BmpMessage.PeerUpNotification({
//                     local_port: 10,
//                     local_address: 1.1.1.1,
//                     remote_port: 80,
//                     per_peer_header: header,
//                 })
//             } else {
//                 BmpMessage.InitiationMessage({})
//             };
//         }

//         apply {
//             match bmp {
//                 PeerUpNotification(x) -> {
//                     if x.local_port == 80 {
//                         accept
//                     }
//                 },
//                 InitiationMessage(x) -> {},
//                 RouteMonitoring(x) -> {},
//                 PeerDownNotification(x) -> {},
//                 StatisticsReport(x) -> {},
//                 TerminationMessage(x) -> {},
//             }
//             reject
//         }
//     }
//     ";

//     let p = compile(s);
//     let f = p
//         .module
//         .get_function::<(*mut u8, i32), ()>("main")
//         .expect("No function found (or mismatched types)");

//     let mut verdict: u8 = 0;
//     f.call((&mut verdict as *mut _, 1));
//     assert_eq!(verdict, true as u8);

//     let mut verdict: u8 = 0;
//     f.call((&mut verdict as *mut _, 2));
//     assert_eq!(verdict, false as u8);

//     let mut verdict: u8 = 0;
//     f.call((&mut verdict as *mut _, 3));
//     assert_eq!(verdict, false as u8);
// }

// #[test]
// fn can_we_misalign_stack_slots() {
//     let s = "
//     type Foo { x: I8 }
//     type Bar { x: i32, y: I16 }

//     filter-map main() {
//         define {
//             foo = Foo { x: 1 };
//             bar = Bar { x: 2, y: 3 };
//         }

//         apply {
//             if foo.x == 1 && bar.x == 2 && bar.y == 3 {
//                 accept
//             } else {
//                 reject
//             }
//         }
//     }
//     ";

//     let p = compile(s);
//     let f = p
//         .module
//         .get_function::<(), i8>("main")
//         .expect("No function found (or mismatched types)");

//     assert_eq!(f.call(()), true as i8);
// }

// #[test]
// fn returning_a_record() {
//     let s = "
//         type Foo { x: i32, y: i32, z: i32 }

//         function make_foo(x: i32) -> Foo {
//             Foo { x: x, y: 1, z: 2 }
//         }

//         filter-map main(rx: i32) {
//             define {
//                 x = make_foo(rx);
//                 y = make_foo(1);
//             }
//             apply {
//                 if rx == x.x {
//                     accept
//                 } else {
//                     reject
//                 }
//             }
//         }
//     ";

//     let p = compile(s);
//     let f = p
//         .module
//         .get_function::<(i32,), i8>("main")
//         .expect("No function found (or mismatched types)");

//     assert_eq!(f.call((5,)), true as i8);
//     assert_eq!(f.call((4,)), true as i8);
// }
