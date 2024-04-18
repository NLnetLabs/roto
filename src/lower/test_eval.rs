use std::fmt::Debug;
use std::str::FromStr as _;

use routecore::addr::Prefix;

use crate::pipeline;

use super::value::SafeValue;

fn compile<T: Into<SafeValue>, U: TryFrom<SafeValue>>(
    s: &str,
) -> impl Fn(T) -> U
where
    <U as TryFrom<SafeValue>>::Error: Debug,
{
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

    move |v| p.eval(v.into()).try_into().unwrap()
}

#[test]
fn accept() {
    let p = compile::<u32, bool>(
        "
        filter-map main() {
            define {
                rx msg: U32;
            }

            apply { accept }
        }
    ",
    );
    assert_eq!(p(0), true);
}

#[test]
fn reject() {
    let p = compile::<u32, bool>(
        "
        filter-map main() {
            define {
                rx msg: U32;
            }

            apply { reject }
        }
    ",
    );
    assert_eq!(p(0), false);
}

#[test]
fn if_else() {
    let p = compile::<u32, bool>(
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
    assert_eq!(p(0), true);
}

#[test]
fn react_to_rx() {
    let p = compile::<u32, bool>(
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
    assert_eq!(p(0), true);
    assert_eq!(p(1), true);
    assert_eq!(p(2), true);
    assert_eq!(p(3), true);
    assert_eq!(p(4), true);
    assert_eq!(p(5), false);
}

#[test]
fn variable() {
    let p = compile::<u32, bool>(
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
    assert_eq!(p(0), true);
}

#[test]
fn calling_term() {
    let p = compile::<u32, bool>(
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
        assert_eq!(p(x), 10 < x && x < 20);
    }
}

#[test]
fn anonymous_record() {
    let p = compile::<u32, bool>(
        "
        filter-map main() {
            define {
                rx msg: U32;
                a = { low: 10, high: 20 };
            }

            term in_range(x: U32) {
                a.low < x && x < a.high
            }

            apply {
                if in_range(msg) { accept }
                reject
            }
        }
    ",
    );

    for x in 0..30 {
        assert_eq!(p(x), 10 < x && x < 20);
    }
}

#[test]
fn typed_record() {
    let p = compile::<u32, bool>(
        "
        type Range {
            low: U32,
            high: U32,
        }

        filter-map main() {
            define {
                rx msg: U32;
                a = Range { low: 10, high: 20 };
                b = Range { low: a.low, high: a.high };
                c = b;
            }
            
            term in_range(x: U32) {
                c.low < x && x < c.high
            }
            
            apply {
                if in_range(msg) { accept }
                reject
            }
        }
    ",
    );

    for x in 0..1 {
        assert_eq!(p(x), 10 < x && x < 20);
    }
}

#[test]
fn enum_values() {
    let p = compile::<SafeValue, bool>(
        "
        filter-map main() { 
            define {
                rx x: Afi;
            }

            apply {
                if x == Afi.IpV4 {
                    accept
                } else {
                    reject
                }
            }
        }
    ",
    );

    // IpV4 -> accepted
    assert_eq!(
        p(SafeValue::Enum(0, Box::new(SafeValue::Unit))),
        true
    );

    // IpV6 -> rejected
    assert_eq!(
        p(SafeValue::Enum(1, Box::new(SafeValue::Unit))),
        false
    );
}

#[test]
fn bmp_message() {
    let p = compile::<SafeValue, bool>(
        "
        filter-map main() {
            define {
                rx x: BmpMessage;
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
    ",
    );
    assert_eq!(
        p(SafeValue::Enum(0, Box::new(SafeValue::Record(Vec::new())))),
        true
    );

    assert_eq!(
        p(SafeValue::Enum(1, Box::new(SafeValue::Record(Vec::new())))),
        false
    );
}

#[test]
fn bmp_message_2() {
    let p = compile::<SafeValue, bool>(
        "
        filter-map main() { 
            define {
                rx x: BmpMessage;
            }

            apply {
                match x {
                    PeerUpNotification(x) -> {
                        if x.local_port == 80 {
                            accept
                        }
                    },
                    InitiationMessage(x) -> {},
                    RouteMonitoring(x) -> {},
                    PeerDownNotification(x) -> {},
                    StatisticsReport(x) -> {},
                    TerminationMessage(x) -> {},
                }
                reject
            }
        }
    ",
    );

    assert_eq!(
        p(SafeValue::Enum(
            2,
            Box::new(SafeValue::Record(vec![(
                "local_port".into(),
                SafeValue::U16(80)
            )]))
        )),
        true
    );

    assert_eq!(
        p(SafeValue::Enum(
            2,
            Box::new(SafeValue::Record(vec![(
                "local_port".into(),
                SafeValue::U16(10)
            )]))
        )),
        false
    );

    assert_eq!(
        p(SafeValue::Enum(1, Box::new(SafeValue::Record(Vec::new())))),
        false
    );
}

#[test]
fn bmp_message_3() {
    let p = compile::<SafeValue, bool>(
        "
        filter-map main() { 
            define {
                rx x: BmpMessage;
            }

            apply {
                match x {
                    PeerUpNotification(x) -> {
                        if x.local_port == 80 {
                            accept
                        }
                    }
                    _ -> {},
                }
                reject
            }
        }
    ",
    );

    assert_eq!(
        p(SafeValue::Enum(
            2,
            Box::new(SafeValue::Record(vec![(
                "local_port".into(),
                SafeValue::U16(80)
            )]))
        )),
        true
    );

    assert_eq!(
        p(SafeValue::Enum(
            2,
            Box::new(SafeValue::Record(vec![(
                "local_port".into(),
                SafeValue::U16(10)
            )]))
        )),
        false
    );

    assert_eq!(
        p(SafeValue::Enum(1, Box::new(SafeValue::Record(Vec::new())))),
        false
    );
}

#[test]
fn bmp_message_4() {
    let p = compile::<SafeValue, bool>(
        "
        filter-map main() { 
            define {
                rx x: BmpMessage;
            }

            apply {
                match x {
                    PeerUpNotification(x) | x.local_port == 80 -> accept,
                    PeerUpNotification(x) | x.local_port == 12 -> accept,
                    PeerUpNotification(x) -> {
                        if x.local_port == 70 {
                            accept
                        }
                    }
                    _ -> {}
                }
                reject
            }
        }
    ",
    );

    assert_eq!(
        p(SafeValue::Enum(
            2,
            Box::new(SafeValue::Record(vec![(
                "local_port".into(),
                SafeValue::U16(80)
            )]))
        )),
        true
    );

    assert_eq!(
        p(SafeValue::Enum(
            2,
            Box::new(SafeValue::Record(vec![(
                "local_port".into(),
                SafeValue::U16(12)
            )]))
        )),
        true
    );

    assert_eq!(
        p(SafeValue::Enum(
            2,
            Box::new(SafeValue::Record(vec![(
                "local_port".into(),
                SafeValue::U16(70)
            )]))
        )),
        true
    );

    assert_eq!(
        p(SafeValue::Enum(
            2,
            Box::new(SafeValue::Record(vec![(
                "local_port".into(),
                SafeValue::U16(10)
            )]))
        )),
        false
    );

    assert_eq!(
        p(SafeValue::Enum(1, Box::new(SafeValue::Record(Vec::new())))),
        false
    );
}

#[test]
fn bmp_message_5() {
    let p = compile::<SafeValue, bool>(
        "
        filter-map main() { 
            define {
                rx x: BmpMessage;
            }

            apply {
                match x {
                    PeerUpNotification(x) | x.local_port == 80 -> accept,
                    _ | true -> reject, // everything below is useless!
                    PeerUpNotification(x) | x.local_port == 12 -> accept,
                    PeerUpNotification(x) -> {
                        if x.local_port == 70 {
                            accept
                        }
                    }
                    _ -> {}
                }
                reject
            }
        }
    ",
    );

    assert_eq!(
        p(SafeValue::Enum(
            2,
            Box::new(SafeValue::Record(vec![(
                "local_port".into(),
                SafeValue::U16(80)
            )]))
        )),
        true
    );

    assert_eq!(
        p(SafeValue::Enum(
            2,
            Box::new(SafeValue::Record(vec![(
                "local_port".into(),
                SafeValue::U16(12)
            )]))
        )),
        false
    );

    assert_eq!(
        p(SafeValue::Enum(
            2,
            Box::new(SafeValue::Record(vec![(
                "local_port".into(),
                SafeValue::U16(70)
            )]))
        )),
        false
    );

    assert_eq!(
        p(SafeValue::Enum(
            2,
            Box::new(SafeValue::Record(vec![(
                "local_port".into(),
                SafeValue::U16(10)
            )]))
        )),
        false
    );

    assert_eq!(
        p(SafeValue::Enum(1, Box::new(SafeValue::Record(Vec::new())))),
        false
    );
}

#[test]
fn prefix_addr() {
    let p = compile::<SafeValue, bool>(
        "
        filter-map main() { 
            define {
                rx x: Prefix;
            }

            apply {
                if x.address() == 0.0.0.0 {
                    accept
                }
                reject
            }
        }
        ",
    );

    assert_eq!(
        p(Prefix::from_str("0.0.0.0/8").unwrap().into()),
        true
    );

    assert_eq!(
        p(Prefix::from_str("127.0.0.0/8").unwrap().into()),
        false
    );
}
