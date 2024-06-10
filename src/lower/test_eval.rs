use std::fmt::Debug;
use std::str::FromStr as _;

use routecore::addr::Prefix;

use crate::pipeline;

use super::value::SafeValue;

#[track_caller]
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
    let p = compile::<u32, Result<(), ()>>(
        "
        filter-map main(msg: U32) {
            apply { accept }
        }
    ",
    );
    assert_eq!(p(0), Ok(()));
}

#[test]
fn reject() {
    let p = compile::<u32, Result<(), ()>>(
        "
        filter-map main(msg: U32) {
            apply { reject }
        }
    ",
    );
    assert_eq!(p(0), Err(()));
}

#[test]
fn if_else() {
    let p = compile::<u32, Result<(), ()>>(
        "
        filter-map main(msg: U32) {
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
    assert_eq!(p(0), Ok(()));
}

#[test]
fn react_to_rx() {
    let p = compile::<u32, Result<(), ()>>(
        "
        filter-map main(x: U32) {
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
    assert_eq!(p(0), Ok(()));
    assert_eq!(p(1), Ok(()));
    assert_eq!(p(2), Ok(()));
    assert_eq!(p(3), Ok(()));
    assert_eq!(p(4), Ok(()));
    assert_eq!(p(5), Err(()));
}

#[test]
fn variable() {
    let p = compile::<u32, Result<(), ()>>(
        "
    filter-map main(msg: U32) {
        define {
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
    assert_eq!(p(0), Ok(()));
}

#[test]
fn calling_function() {
    let p = compile::<u32, Result<(), ()>>(
        "
        function smaller_than(a: U32, b: U32) -> Bool {
            a < b
        }
    
        function small(x: U32) -> Bool {
            smaller_than(10, x) && smaller_than(x, 20)
        }

        filter-map main(msg: U32) {        
            apply {
                if small(msg) { accept }
                reject
            }
        }
    ",
    );

    for x in 0..30 {
        assert_eq!(p(x), if 10 < x && x < 20 { Ok(()) } else { Err(()) });
    }
}

#[test]
fn anonymous_record() {
    let p = compile::<u32, Result<(), ()>>(
        "
        function in_range(x: U32, low: U32, high: U32) -> Bool {
            low < x && x < high
        }

        filter-map main(msg: U32) {
            define {
                a = { low: 10, high: 20 };
            }


            apply {
                if in_range(msg, a.low, a.high) { accept }
                reject
            }
        }
    ",
    );

    for x in 0..30 {
        assert_eq!(p(x), if 10 < x && x < 20 { Ok(()) } else { Err(()) });
    }
}

#[test]
fn typed_record() {
    let p = compile::<u32, Result<(), ()>>(
        "
        type Range {
            low: U32,
            high: U32,
        }

        function in_range(x: U32, c: Range) -> Bool {
            c.low < x && x < c.high
        }

        filter-map main(msg: U32) {
            define {
                a = Range { low: 10, high: 20 };
                b = Range { low: a.low, high: a.high };
                c = b;
            }
            
            
            apply {
                if in_range(msg, c) { accept }
                reject
            }
        }
    ",
    );

    for x in 0..1 {
        assert_eq!(p(x), if 10 < x && x < 20 { Ok(()) } else { Err(()) });
    }
}

#[test]
fn enum_values() {
    let p = compile::<SafeValue, Result<(), ()>>(
        "
        filter-map main(x: Afi) { 
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
    assert_eq!(p(SafeValue::Enum(0, None)), Ok(()));

    // IpV6 -> rejected
    assert_eq!(p(SafeValue::Enum(1, None)), Err(()));
}

#[test]
fn bmp_message() {
    let p = compile::<SafeValue, Result<(), ()>>(
        "
        filter-map main(x: BmpMessage) {
            define {
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
        p(SafeValue::Enum(
            0,
            Some(Box::new(SafeValue::Record(Vec::new())))
        )),
        Ok(())
    );

    assert_eq!(
        p(SafeValue::Enum(
            1,
            Some(Box::new(SafeValue::Record(Vec::new())))
        )),
        Err(())
    );
}

#[test]
fn bmp_message_2() {
    let p = compile::<SafeValue, Result<(), ()>>(
        "
        filter-map main(x: BmpMessage) { 
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
            Some(Box::new(SafeValue::Record(vec![(
                "local_port".into(),
                SafeValue::U16(80)
            )])))
        )),
        Ok(())
    );

    assert_eq!(
        p(SafeValue::Enum(
            2,
            Some(Box::new(SafeValue::Record(vec![(
                "local_port".into(),
                SafeValue::U16(10)
            )])))
        )),
        Err(())
    );

    assert_eq!(
        p(SafeValue::Enum(
            1,
            Some(Box::new(SafeValue::Record(Vec::new())))
        )),
        Err(())
    );
}

#[test]
fn bmp_message_3() {
    let p = compile::<SafeValue, Result<(), ()>>(
        "
        filter-map main(x: BmpMessage) { 
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
            Some(Box::new(SafeValue::Record(vec![(
                "local_port".into(),
                SafeValue::U16(80)
            )])))
        )),
        Ok(())
    );

    assert_eq!(
        p(SafeValue::Enum(
            2,
            Some(Box::new(SafeValue::Record(vec![(
                "local_port".into(),
                SafeValue::U16(10)
            )])))
        )),
        Err(())
    );

    assert_eq!(
        p(SafeValue::Enum(
            1,
            Some(Box::new(SafeValue::Record(Vec::new())))
        )),
        Err(())
    );
}

#[test]
fn bmp_message_4() {
    let p = compile::<SafeValue, Result<(), ()>>(
        "
        filter-map main(x: BmpMessage) { 
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
            Some(Box::new(SafeValue::Record(vec![(
                "local_port".into(),
                SafeValue::U16(80)
            )])))
        )),
        Ok(())
    );

    assert_eq!(
        p(SafeValue::Enum(
            2,
            Some(Box::new(SafeValue::Record(vec![(
                "local_port".into(),
                SafeValue::U16(12)
            )])))
        )),
        Ok(())
    );

    assert_eq!(
        p(SafeValue::Enum(
            2,
            Some(Box::new(SafeValue::Record(vec![(
                "local_port".into(),
                SafeValue::U16(70)
            )])))
        )),
        Ok(())
    );

    assert_eq!(
        p(SafeValue::Enum(
            2,
            Some(Box::new(SafeValue::Record(vec![(
                "local_port".into(),
                SafeValue::U16(10)
            )])))
        )),
        Err(())
    );

    assert_eq!(
        p(SafeValue::Enum(
            1,
            Some(Box::new(SafeValue::Record(Vec::new())))
        )),
        Err(())
    );
}

#[test]
fn bmp_message_5() {
    let p = compile::<SafeValue, Result<(), ()>>(
        "
        filter-map main(x: BmpMessage) { 
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
            Some(Box::new(SafeValue::Record(vec![(
                "local_port".into(),
                SafeValue::U16(80)
            )])))
        )),
        Ok(())
    );

    assert_eq!(
        p(SafeValue::Enum(
            2,
            Some(Box::new(SafeValue::Record(vec![(
                "local_port".into(),
                SafeValue::U16(12)
            )])))
        )),
        Err(())
    );

    assert_eq!(
        p(SafeValue::Enum(
            2,
            Some(Box::new(SafeValue::Record(vec![(
                "local_port".into(),
                SafeValue::U16(70)
            )])))
        )),
        Err(())
    );

    assert_eq!(
        p(SafeValue::Enum(
            2,
            Some(Box::new(SafeValue::Record(vec![(
                "local_port".into(),
                SafeValue::U16(10)
            )])))
        )),
        Err(())
    );

    assert_eq!(
        p(SafeValue::Enum(
            1,
            Some(Box::new(SafeValue::Record(Vec::new())))
        )),
        Err(())
    );
}

#[test]
fn prefix_addr() {
    let p = compile::<SafeValue, Result<(), ()>>(
        "
        filter-map main(x: Prefix) { 
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
        p(SafeValue::from_any(Box::new(
            Prefix::from_str("0.0.0.0/8").unwrap()
        ))),
        Ok(())
    );

    assert_eq!(
        p(SafeValue::from_any(Box::new(
            Prefix::from_str("127.0.0.0/8").unwrap()
        ))),
        Err(())
    );
}
