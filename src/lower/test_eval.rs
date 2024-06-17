use crate::{pipeline, Lowered};

use super::{eval::Memory, value::IrValue};

#[track_caller]
fn compile(s: &str) -> Lowered {
    // We run this multiple times and only want to init the
    // first time, so ignore failures.
    let _ = env_logger::builder()
        .format_timestamp(None)
        .format_target(false)
        .try_init();

    pipeline::test_file(s)
        .parse()
        .unwrap()
        .typecheck()
        .unwrap()
        .lower()
}

#[test]
fn accept() {
    let s = "
        filter-map main(msg: U32) {
            apply { accept }
        }
    ";

    let mut mem = Memory::new();
    let program = compile(s);
    let pointer = mem.allocate(1);
    program.eval(&mut mem, vec![IrValue::Pointer(pointer), IrValue::U32(0)]);
    let res = mem.read(pointer, 1);
    assert_eq!(&[1], res);
}

#[test]
fn reject() {
    let s = "
        filter-map main() {
            apply { reject }
        }
    ";

    let mut mem = Memory::new();
    let program = compile(s);
    let pointer = mem.allocate(1);
    program.eval(&mut mem, vec![IrValue::Pointer(pointer)]);
    let res = mem.read(pointer, 1);
    assert_eq!(&[0], res);
}

#[test]
fn if_else() {
    let s = "
        filter-map main() {
            apply { 
                if true && true {
                    accept
                } else {
                    reject
                }
            }
        }      
    ";
    let mut mem = Memory::new();
    let program = compile(s);
    let pointer = mem.allocate(1);
    program.eval(&mut mem, vec![IrValue::Pointer(pointer)]);
    let res = mem.read(pointer, 1);
    assert_eq!(&[1], res);
}

#[test]
fn react_to_rx() {
    let s = "
        filter-map main(x: U32) {
            apply {
                if x <= 4 {
                    accept
                } else {
                    reject
                }
            }
        }
    ";

    let program = compile(s);

    for i in 0..6 {
        let mut mem = Memory::new();
        let pointer = mem.allocate(1);
        program
            .eval(&mut mem, vec![IrValue::Pointer(pointer), IrValue::U32(i)]);
        let res = mem.read(pointer, 1);
        assert_eq!(res, &[(i <= 4) as u8], "failed at: {i}");
    }
}

#[test]
fn variable() {
    let s = "
    filter-map main() {
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
    ";

    let mut mem = Memory::new();
    let program = compile(s);
    let pointer = mem.allocate(1);
    program.eval(&mut mem, vec![IrValue::Pointer(pointer)]);
    let res = mem.read(pointer, 1);
    assert_eq!(&[1], res);
}

#[test]
fn calling_function() {
    let s = "
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
    ";

    for x in 0..30 {
        let mut mem = Memory::new();
        let program = compile(s);
        let pointer = mem.allocate(1);
        program
            .eval(&mut mem, vec![IrValue::Pointer(pointer), IrValue::U32(x)]);
        let res = mem.read(pointer, 1);
        assert_eq!(&[(10 < x && x < 20) as u8], res);
    }
}

#[test]
fn anonymous_record() {
    let s = "
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
    ";

    for x in 0..30 {
        let mut mem = Memory::new();
        let program = compile(s);
        let pointer = mem.allocate(1);
        program
            .eval(&mut mem, vec![IrValue::Pointer(pointer), IrValue::U32(x)]);
        let res = mem.read(pointer, 1);
        assert_eq!(&[(10 < x && x < 20) as u8], res);
    }
}

#[test]
fn typed_record() {
    let s = "
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
    ";

    for x in 0..1 {
        let mut mem = Memory::new();
        let program = compile(s);
        let pointer = mem.allocate(1);
        program
            .eval(&mut mem, vec![IrValue::Pointer(pointer), IrValue::U32(x)]);
        let res = mem.read(pointer, 1);
        assert_eq!(&[(10 < x && x < 20) as u8], res);
    }
}

// #[test]
// fn enum_values() {
//     let s = "
//         filter-map main(x: Afi) {
//             apply {
//                 if x == Afi.IpV4 {
//                     accept
//                 } else {
//                     reject
//                 }
//             }
//         }
//     ";

//     // IpV4 -> accepted
//     assert_eq!(p(IrValue::Enum(0, None)), Ok(()));

//     // IpV6 -> rejected
//     assert_eq!(p(IrValue::Enum(1, None)), Err(()));
// }

// #[test]
// fn bmp_message() {
//     let p = compile::<IrValue>(
//         "
//         filter-map main(x: BmpMessage) {
//             define {
//                 a = BmpMessage.InitiationMessage(BmpInitiationMessage {});
//             }

//             apply {
//                 if x == a {
//                     accept
//                 } else {
//                     reject
//                 }
//             }
//         }
//     ",
//     );
//     assert_eq!(
//         p(IrValue::Enum(
//             0,
//             Some(Box::new(IrValue::Record(Vec::new())))
//         )),
//         Ok(())
//     );

//     assert_eq!(
//         p(IrValue::Enum(
//             1,
//             Some(Box::new(IrValue::Record(Vec::new())))
//         )),
//         Err(())
//     );
// }

// #[test]
// fn bmp_message_2() {
//     let p = compile::<IrValue, Result<(), ()>>(
//         "
//         filter-map main(x: BmpMessage) {
//             apply {
//                 match x {
//                     PeerUpNotification(x) -> {
//                         if x.local_port == 80 {
//                             accept
//                         }
//                     },
//                     InitiationMessage(x) -> {},
//                     RouteMonitoring(x) -> {},
//                     PeerDownNotification(x) -> {},
//                     StatisticsReport(x) -> {},
//                     TerminationMessage(x) -> {},
//                 }
//                 reject
//             }
//         }
//     ",
//     );

//     assert_eq!(
//         p(IrValue::Enum(
//             2,
//             Some(Box::new(IrValue::Record(vec![(
//                 "local_port".into(),
//                 IrValue::U16(80)
//             )])))
//         )),
//         Ok(())
//     );

//     assert_eq!(
//         p(IrValue::Enum(
//             2,
//             Some(Box::new(IrValue::Record(vec![(
//                 "local_port".into(),
//                 IrValue::U16(10)
//             )])))
//         )),
//         Err(())
//     );

//     assert_eq!(
//         p(IrValue::Enum(
//             1,
//             Some(Box::new(IrValue::Record(Vec::new())))
//         )),
//         Err(())
//     );
// }

// #[test]
// fn bmp_message_3() {
//     let p = compile::<IrValue, Result<(), ()>>(
//         "
//         filter-map main(x: BmpMessage) {
//             apply {
//                 match x {
//                     PeerUpNotification(x) -> {
//                         if x.local_port == 80 {
//                             accept
//                         }
//                     }
//                     _ -> {},
//                 }
//                 reject
//             }
//         }
//     ",
//     );

//     assert_eq!(
//         p(IrValue::Enum(
//             2,
//             Some(Box::new(IrValue::Record(vec![(
//                 "local_port".into(),
//                 IrValue::U16(80)
//             )])))
//         )),
//         Ok(())
//     );

//     assert_eq!(
//         p(IrValue::Enum(
//             2,
//             Some(Box::new(IrValue::Record(vec![(
//                 "local_port".into(),
//                 IrValue::U16(10)
//             )])))
//         )),
//         Err(())
//     );

//     assert_eq!(
//         p(IrValue::Enum(
//             1,
//             Some(Box::new(IrValue::Record(Vec::new())))
//         )),
//         Err(())
//     );
// }

// #[test]
// fn bmp_message_4() {
//     let p = compile::<IrValue, Result<(), ()>>(
//         "
//         filter-map main(x: BmpMessage) {
//             apply {
//                 match x {
//                     PeerUpNotification(x) | x.local_port == 80 -> accept,
//                     PeerUpNotification(x) | x.local_port == 12 -> accept,
//                     PeerUpNotification(x) -> {
//                         if x.local_port == 70 {
//                             accept
//                         }
//                     }
//                     _ -> {}
//                 }
//                 reject
//             }
//         }
//     ",
//     );

//     assert_eq!(
//         p(IrValue::Enum(
//             2,
//             Some(Box::new(IrValue::Record(vec![(
//                 "local_port".into(),
//                 IrValue::U16(80)
//             )])))
//         )),
//         Ok(())
//     );

//     assert_eq!(
//         p(IrValue::Enum(
//             2,
//             Some(Box::new(IrValue::Record(vec![(
//                 "local_port".into(),
//                 IrValue::U16(12)
//             )])))
//         )),
//         Ok(())
//     );

//     assert_eq!(
//         p(IrValue::Enum(
//             2,
//             Some(Box::new(IrValue::Record(vec![(
//                 "local_port".into(),
//                 IrValue::U16(70)
//             )])))
//         )),
//         Ok(())
//     );

//     assert_eq!(
//         p(IrValue::Enum(
//             2,
//             Some(Box::new(IrValue::Record(vec![(
//                 "local_port".into(),
//                 IrValue::U16(10)
//             )])))
//         )),
//         Err(())
//     );

//     assert_eq!(
//         p(IrValue::Enum(
//             1,
//             Some(Box::new(IrValue::Record(Vec::new())))
//         )),
//         Err(())
//     );
// }

// #[test]
// fn bmp_message_5() {
//     let p = compile::<IrValue, Result<(), ()>>(
//         "
//         filter-map main(x: BmpMessage) {
//             apply {
//                 match x {
//                     PeerUpNotification(x) | x.local_port == 80 -> accept,
//                     _ | true -> reject, // everything below is useless!
//                     PeerUpNotification(x) | x.local_port == 12 -> accept,
//                     PeerUpNotification(x) -> {
//                         if x.local_port == 70 {
//                             accept
//                         }
//                     }
//                     _ -> {}
//                 }
//                 reject
//             }
//         }
//     ",
//     );

//     assert_eq!(
//         p(IrValue::Enum(
//             2,
//             Some(Box::new(IrValue::Record(vec![(
//                 "local_port".into(),
//                 IrValue::U16(80)
//             )])))
//         )),
//         Ok(())
//     );

//     assert_eq!(
//         p(IrValue::Enum(
//             2,
//             Some(Box::new(IrValue::Record(vec![(
//                 "local_port".into(),
//                 IrValue::U16(12)
//             )])))
//         )),
//         Err(())
//     );

//     assert_eq!(
//         p(IrValue::Enum(
//             2,
//             Some(Box::new(IrValue::Record(vec![(
//                 "local_port".into(),
//                 IrValue::U16(70)
//             )])))
//         )),
//         Err(())
//     );

//     assert_eq!(
//         p(IrValue::Enum(
//             2,
//             Some(Box::new(IrValue::Record(vec![(
//                 "local_port".into(),
//                 IrValue::U16(10)
//             )])))
//         )),
//         Err(())
//     );

//     assert_eq!(
//         p(IrValue::Enum(
//             1,
//             Some(Box::new(IrValue::Record(Vec::new())))
//         )),
//         Err(())
//     );
// }

// #[test]
// fn prefix_addr() {
//     let p = compile::<IrValue, Result<(), ()>>(
//         "
//         filter-map main(x: Prefix) {
//             apply {
//                 if x.address() == 0.0.0.0 {
//                     accept
//                 }
//                 reject
//             }
//         }
//         ",
//     );

//     assert_eq!(
//         p(IrValue::from_any(Box::new(
//             Prefix::from_str("0.0.0.0/8").unwrap()
//         ))),
//         Ok(())
//     );

//     assert_eq!(
//         p(IrValue::from_any(Box::new(
//             Prefix::from_str("127.0.0.0/8").unwrap()
//         ))),
//         Err(())
//     );
// }
