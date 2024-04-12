use crate::pipeline;

use super::ir::SafeValue;

fn compile(s: &str) -> impl Fn(SafeValue) -> SafeValue {
    // We run this multiple times and only want to init the
    // first time, so ignore failures.
    let _ = env_logger::builder()
        .format_timestamp(None)
        .format_target(false)
        .try_init();

    let p = pipeline::test_file(s)
        .parse()
        .unwrap()
        .typecheck()
        .unwrap()
        .lower();

    move |v| p.eval(v)
}

#[test]
fn accept() {
    let p = compile(
        "
        filter-map main() {
            define {
                rx msg: U32;
            }

            apply { accept }
        }
    ",
    );
    assert_eq!(p(SafeValue::U32(0)), SafeValue::Bool(true));
}

#[test]
fn reject() {
    let p = compile(
        "
        filter-map main() {
            define {
                rx msg: U32;
            }

            apply { reject }
        }
    ",
    );
    assert_eq!(p(SafeValue::U32(0)), SafeValue::Bool(false));
}

#[test]
fn if_else() {
    let p = compile(
        "
        filter-map main() {
            define {
                rx msg: U32;
            }
        
            apply { 
                if true && true {
                    accept
                } else {
                    reject
                }
            }
        }      
    ",
    );
    assert_eq!(p(SafeValue::U32(0)), SafeValue::Bool(true));
}

#[test]
fn react_to_rx() {
    let p = compile(
        "
        filter-map main() {
            define {
                rx x: U32;
            }
        
            apply {
                if x <= 4 {
                    accept
                } else {
                    reject
                }
            }
        }
    ",
    );
    assert_eq!(p(SafeValue::U32(0)), SafeValue::Bool(true));
    assert_eq!(p(SafeValue::U32(1)), SafeValue::Bool(true));
    assert_eq!(p(SafeValue::U32(2)), SafeValue::Bool(true));
    assert_eq!(p(SafeValue::U32(3)), SafeValue::Bool(true));
    assert_eq!(p(SafeValue::U32(4)), SafeValue::Bool(true));
    assert_eq!(p(SafeValue::U32(5)), SafeValue::Bool(false));
}

#[test]
fn variable() {
    let p = compile(
        "
    filter-map main() {
        define {
            rx msg: U32;
            a = 5;
        }
    
        apply {
            if a == 5 {
                accept
            } else {
                reject
            }
        }
    }
    ",
    );
    assert_eq!(p(SafeValue::U32(0)), SafeValue::Bool(true));
}

#[test]
fn calling_term() {
    let p = compile(
        "
        filter-map main() {
            define {
                rx msg: U32;
                c = 10;
                d = 20;
            }
        
            term smaller_than(a: U32, b: U32) {
                a < b
            }
        
            term small(x: U32) {
                smaller_than(c, x) && smaller_than(x, d)
            }
        
            apply {
                if small(msg) { accept }
                reject
            }
        }
    ",
    );

    for x in 0..30 {
        assert_eq!(p(SafeValue::U32(x)), SafeValue::Bool(10 < x && x < 20));
    }
}
