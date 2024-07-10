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

    let pointer_bytes = usize::BITS / 8;

    pipeline::test_file(file!(), s, line!() as usize)
        .parse()
        .unwrap()
        .typecheck(pointer_bytes)
        .unwrap()
        .lower()
}

#[derive(Clone, Debug)]
enum MemVal {
    Struct(Vec<MemVal>),
    // Unfortunately, we can't (with this setup) compute the size and
    // alignment of an enum, so we have to pass that explicitly.
    Enum(u8, usize, usize, Option<Box<MemVal>>),
    Val(IrValue),
}

impl MemVal {
    fn serialize(self, into: &mut Vec<u8>) {
        into.resize(self.padding(into.len()) + into.len(), 0);
        match self {
            MemVal::Struct(fields) => {
                for f in fields {
                    f.serialize(into);
                }
            }
            MemVal::Enum(d, size, _, v) => {
                let old_size = into.len();
                into.push(d);
                if let Some(v) = v {
                    v.serialize(into);
                }
                into.resize(old_size + size, 0)
            }
            MemVal::Val(v) => {
                into.extend_from_slice(&v.as_vec());
            }
        }
    }

    fn _size(&self) -> usize {
        match self {
            MemVal::Struct(fields) => {
                let mut size = 0;
                for f in fields {
                    size += f.padding(size) + f._size()
                }
                size
            }
            MemVal::Enum(_, size, _, _) => *size,
            MemVal::Val(v) => v.get_type().bytes(),
        }
    }

    fn alignment(&self) -> usize {
        match self {
            MemVal::Struct(fields) => {
                let mut align = 1;
                for f in fields {
                    align = align.max(f.alignment());
                }
                align
            }
            MemVal::Enum(_, _, alignment, _) => *alignment,
            MemVal::Val(v) => v.get_type().alignment(),
        }
    }

    fn padding(&self, offset: usize) -> usize {
        let align = self.alignment();
        if offset % align > 0 {
            align - (offset % align)
        } else {
            0
        }
    }
}

#[test]
fn accept() {
    let s = "
        filter-map main(msg: u32) {
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
        filter-map main(x: u32) {
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
        function smaller_than(a: u32, b: u32) -> bool {
            a < b
        }

        function small(x: u32) -> bool {
            smaller_than(10, x) && smaller_than(x, 20)
        }

        filter-map main(msg: u32) {
            apply {
                if small(msg) { accept }
                reject
            }
        }
    ";

    let program = compile(s);

    for x in 0..30 {
        let mut mem = Memory::new();
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
        function in_range(x: u32, low: u32, high: u32) -> bool {
            low < x && x < high
        }

        filter-map main(msg: u32) {
            define {
                a = { low: 10, high: 20 };
            }

            apply {
                if in_range(msg, a.low, a.high) { accept }
                reject
            }
        }
    ";

    let program = compile(s);

    for x in 0..30 {
        let mut mem = Memory::new();
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
            low: u32,
            high: u32,
        }

        function in_range(x: u32, c: Range) -> bool {
            c.low < x && x < c.high
        }

        filter-map main(msg: u32) {
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

    let program = compile(s);

    for x in 0..1 {
        let mut mem = Memory::new();
        let pointer = mem.allocate(1);
        program
            .eval(&mut mem, vec![IrValue::Pointer(pointer), IrValue::U32(x)]);
        let res = mem.read(pointer, 1);
        assert_eq!(&[(10 < x && x < 20) as u8], res);
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

    let program = compile(s);

    for x in 20..21 {
        let mut mem = Memory::new();
        let pointer = mem.allocate(1);
        program
            .eval(&mut mem, vec![IrValue::Pointer(pointer), IrValue::I32(x)]);
        let res = mem.read(pointer, 1);
        assert_eq!(&[(x == 20) as u8], res, "for x = {x}");
    }
}

#[test]
fn enum_values() {
    let s = "
        filter-map main(x: Afi) {
            apply {
                if x == Afi.IpV4 {
                    accept
                } else {
                    reject
                }
            }
        }
    ";

    let program = compile(s);

    for (variant, expected) in [
        // IpV4 -> accepted
        (0, true),
        // // IpV6 -> rejected
        (1, false),
    ] {
        let mut mem = Memory::new();
        let verdict_pointer = mem.allocate(1);
        let afi_pointer = mem.allocate(1);
        mem.write(afi_pointer, &[variant]);
        program.eval(
            &mut mem,
            vec![
                IrValue::Pointer(verdict_pointer),
                IrValue::Pointer(afi_pointer),
            ],
        );
        let res = mem.read(verdict_pointer, 1);
        assert_eq!(&[expected as u8], res);
    }
}

#[test]
fn bmp_message() {
    let s = "
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
    ";

    let program = compile(s);

    for (variant, expected) in [(0, true), (1, false)] {
        let mut mem = Memory::new();
        let verdict_pointer = mem.allocate(1);
        let bmp_pointer = mem.allocate(48);
        mem.write(bmp_pointer, &[variant]);
        program.eval(
            &mut mem,
            vec![
                IrValue::Pointer(verdict_pointer),
                IrValue::Pointer(bmp_pointer),
            ],
        );
        let res = mem.read(verdict_pointer, 1);
        assert_eq!(&[expected as u8], res);
    }
}

fn create_message(variant: u8, port: Option<u16>) -> MemVal {
    let data = port.map(|port| {
        Box::new(MemVal::Struct(vec![
            MemVal::Val(IrValue::ExtPointer(&mut () as *mut _)),
            MemVal::Val(IrValue::U16(port)),
            MemVal::Val(IrValue::U16(0)),
            MemVal::Struct(vec![
                MemVal::Val(IrValue::Bool(false)),
                MemVal::Val(IrValue::Bool(false)),
                MemVal::Val(IrValue::Bool(false)),
                MemVal::Val(IrValue::Bool(false)),
                MemVal::Val(IrValue::Bool(false)),
                MemVal::Val(IrValue::U8(0)),
                MemVal::Val(IrValue::U32(0)),
                MemVal::Val(IrValue::ExtPointer(&mut () as *mut _)),
            ]),
        ]))
    });
    MemVal::Enum(variant, 20, 4, data)
}

#[test]
fn bmp_message_2() {
    let s = "
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
    ";

    let program = compile(s);
    for (var, port, expected) in
        [(2, Some(80), true), (2, Some(10), false), (1, None, false)]
    {
        let val = create_message(var, port);
        let mut vec = Vec::new();
        val.clone().serialize(&mut vec);
        let mut mem = Memory::new();
        let pointer = mem.allocate(1);
        let val_pointer = mem.allocate(vec.len());
        mem.write(val_pointer, &vec);

        program.eval(
            &mut mem,
            vec![IrValue::Pointer(pointer), IrValue::Pointer(val_pointer)],
        );

        let res = mem.read(pointer, 1);
        assert_eq!(&[expected as u8], res, "{val:?}");
    }
}

#[test]
fn bmp_message_3() {
    let s = "
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
    ";

    let program = compile(s);

    for (var, port, expected) in
        [(2, Some(80), true), (2, Some(10), false), (1, None, false)]
    {
        let val = create_message(var, port);
        let mut vec = Vec::new();
        val.clone().serialize(&mut vec);
        let mut mem = Memory::new();
        let pointer = mem.allocate(1);
        let val_pointer = mem.allocate(vec.len());
        mem.write(val_pointer, &vec);

        program.eval(
            &mut mem,
            vec![IrValue::Pointer(pointer), IrValue::Pointer(val_pointer)],
        );

        let res = mem.read(pointer, 1);
        assert_eq!(&[expected as u8], res, "{val:?}");
    }
}

#[test]
fn bmp_message_4() {
    let s = "
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
    ";

    let program = compile(s);

    for (var, port, expected) in [
        (2, Some(80), true),
        (2, Some(12), true),
        (2, Some(70), true),
        (2, Some(10), false),
        (1, None, false),
    ] {
        let val = create_message(var, port);
        let mut vec = Vec::new();
        val.clone().serialize(&mut vec);
        let mut mem = Memory::new();
        let pointer = mem.allocate(1);
        let val_pointer = mem.allocate(vec.len());
        mem.write(val_pointer, &vec);

        program.eval(
            &mut mem,
            vec![IrValue::Pointer(pointer), IrValue::Pointer(val_pointer)],
        );

        let res = mem.read(pointer, 1);
        assert_eq!(&[expected as u8], res, "{val:?}");
    }
}

#[test]
fn bmp_message_5() {
    let s = "
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
    ";

    let program = compile(s);

    for (var, port, expected) in [
        (2, Some(80), true),
        (2, Some(12), false),
        (2, Some(70), false),
        (2, Some(10), false),
        (1, None, false),
    ] {
        let val = create_message(var, port);
        let mut vec = Vec::new();
        val.clone().serialize(&mut vec);
        let mut mem = Memory::new();
        let pointer = mem.allocate(1);
        let val_pointer = mem.allocate(vec.len());
        mem.write(val_pointer, &vec);

        program.eval(
            &mut mem,
            vec![IrValue::Pointer(pointer), IrValue::Pointer(val_pointer)],
        );

        let res = mem.read(pointer, 1);
        assert_eq!(&[expected as u8], res, "{val:?}");
    }
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
    
    let program = compile(s);

    for (value, expected) in [(5, 0), (11, 1)] {
        let mut mem = Memory::new();
        let verdict_pointer = mem.allocate(1);
        program.eval(
            &mut mem,
            vec![
                IrValue::Pointer(verdict_pointer),
                IrValue::U32(value),
            ],
        );
        let res = mem.read(verdict_pointer, 1);
        assert_eq!(&[expected as u8], res);
    }
}

// #[test]
// fn prefix_addr() {
//     let s = "
//         filter-map main(x: Prefix) {
//             apply {
//                 if x.address() == 0.0.0.0 {
//                     accept
//                 }
//                 reject
//             }
//         }
//     ";

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

//     let mut mem = Memory::new();
//     let program = compile(s);
//     let pointer = mem.allocate(1);
//     program.eval(&mut mem, vec![IrValue::Pointer(pointer), IrValue::U32(0)]);
//     let res = mem.read(pointer, 1);
//     assert_eq!(&[1], res);
// }
