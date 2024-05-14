use crate::pipeline::*;

fn compile(p: &'static str) -> Compiled {
    let res = test_file(p).parse().and_then(|x| x.typecheck()).map(|x| {
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
        .get_function::<(), i8>("main")
        .expect("No function found (or mismatched types)");

    assert_eq!(f.call(()), 1);
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
        .get_function::<(), i8>("main")
        .expect("No function found (or mismatched types)");

    assert_eq!(f.call(()), 0);
}

#[test]
fn equal_to_10() {
    let s = "
        filter-map main(x: U32) {
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
        .get_function::<(i32,), i8>("main")
        .expect("No function found (or mismatched types)");

    assert_eq!(f.call((5,)), 0);
    assert_eq!(f.call((10,)), 1);
}

#[test]
fn equal_to_10_with_term() {
    let s = "
        term is_10(x: U32) {
            x == 10
        }
        
        filter-map main(x: U32) {
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
        .get_function::<(i32,), i8>("main")
        .expect("No function found (or mismatched types)");

    assert_eq!(f.call((5,)), 0);
    assert_eq!(f.call((10,)), 1);
}

#[test]
fn equal_to_10_with_two_terms() {
    let s = "
        term equals(x: U32, y: U32) {
            x == y
        }

        term is_10(x: U32) {
            equals(x, 10)
        }

        filter-map main(x: U32) {
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
        .get_function::<(i32,), i8>("main")
        .expect("No function found (or mismatched types)");

    assert_eq!(f.call((5,)), 0);
    assert_eq!(f.call((10,)), 1);
    assert_eq!(f.call((15,)), 0);
    assert_eq!(f.call((20,)), 1);
}

#[test]
fn negation() {
    let s = "
        filter-map main(x: I32) {
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
        .get_function::<(i32,), i8>("main")
        .expect("No function found (or mismatched types)");

    for x in 0..20 {
        assert_eq!(f.call((x,)), (x != 10) as i8, "{x}");
    }
}

#[test]
fn a_bunch_of_comparisons() {
    let s = "
        filter-map main(x: I32) {
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
        .get_function::<(i32,), i8>("main")
        .expect("No function found (or mismatched types)");

    for x in 0..100 {
        #[allow(clippy::manual_range_contains)]
        let expected = (x > 10 && x < 20) || (x >= 30 && x <= 40) || x == 55;
        assert_eq!(f.call((x,)), expected as i8);
    }
}

#[test]
fn record() {
    let s = "
        type Foo { a: I32, b: I32 }

        filter-map main(x: I32) {
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
        .get_function::<(i32,), i8>("main")
        .expect("No function found (or mismatched types)");

    for x in 0..100 {
        let expected = x == 20;
        assert_eq!(f.call((x,)), expected as i8);
    }
}

#[test]
fn record_with_fields_flipped() {
    let s = "
        type Foo { a: I32, b: I32 }

        filter-map main(x: I32) {
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
        .get_function::<(i32,), i8>("main")
        .expect("No function found (or mismatched types)");

    for x in 0..100 {
        let expected = x == 20;
        assert_eq!(f.call((x,)), expected as i8);
    }
}

#[test]
fn nested_record() {
    let s = "
        type Foo { x: Bar, y: Bar }
        type Bar { a: I32, b: I32 }

        filter-map main(x: I32) {
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
        .get_function::<(i32,), i8>("main")
        .expect("No function found (or mismatched types)");

    for x in 0..100 {
        let expected = x == 20;
        assert_eq!(f.call((x,)), expected as i8);
    }
}
