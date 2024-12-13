use super::{eval::Memory, value::IrValue};
use crate::{runtime::tests::routecore_runtime, src, Files, Lowered};

#[track_caller]
fn compile(s: Files) -> Lowered {
    // We run this multiple times and only want to init the
    // first time, so ignore failures.
    let _ = env_logger::builder()
        .format_timestamp(None)
        .format_target(false)
        .try_init();

    let runtime = routecore_runtime().unwrap();
    let pointer_bytes = usize::BITS / 8;

    s.parse()
        .unwrap()
        .typecheck(runtime, pointer_bytes)
        .unwrap()
        .lower()
}

/// Helper for constructing complex values for Roto
#[allow(unused)]
#[derive(Clone, Debug)]
enum MemVal {
    Struct(Vec<MemVal>),
    // Unfortunately, we can't (with this setup) compute the size and
    // alignment of an enum, so we have to pass that explicitly.
    Enum(u8, usize, usize, Option<Box<MemVal>>),
    Val(IrValue),
}

#[allow(unused)]
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
    let s = src!(
        "
        filter-map main(msg: u32) {
            apply { accept }
        }
    "
    );

    let mut mem = Memory::new();
    let program = compile(s);
    let pointer = mem.allocate(1);
    let ctx = IrValue::Pointer(mem.allocate(0));
    program.eval(
        &mut mem,
        ctx,
        vec![IrValue::Pointer(pointer), IrValue::U32(0)],
    );
    let res = mem.read_array::<1>(pointer);
    assert_eq!(0, u8::from_ne_bytes(res));
}

#[test]
fn reject() {
    let s = src!(
        "
        filter-map main() {
            apply { reject }
        }
    "
    );

    let mut mem = Memory::new();
    let program = compile(s);
    let pointer = mem.allocate(1);
    let ctx = IrValue::Pointer(mem.allocate(0));
    program.eval(&mut mem, ctx, vec![IrValue::Pointer(pointer)]);
    let res = mem.read_array::<1>(pointer);
    assert_eq!(1, u8::from_ne_bytes(res));
}

#[test]
fn if_else() {
    let s = src!(
        "
        filter-map main() {
            apply { 
                if true && true {
                    accept
                } else {
                    reject
                }
            }
        }      
    "
    );
    let mut mem = Memory::new();
    let program = compile(s);
    let pointer = mem.allocate(1);
    let ctx = IrValue::Pointer(mem.allocate(0));
    program.eval(&mut mem, ctx, vec![IrValue::Pointer(pointer)]);
    let res = mem.read_array::<1>(pointer);
    assert_eq!(0, u8::from_ne_bytes(res));
}

#[test]
fn react_to_rx() {
    let s = src!(
        "
        filter-map main(x: u32) {
            apply {
                if x <= 4 {
                    accept
                } else {
                    reject
                }
            }
        }
    "
    );

    let program = compile(s);

    for i in 0..6 {
        let mut mem = Memory::new();
        let pointer = mem.allocate(1);
        let ctx = IrValue::Pointer(mem.allocate(0));
        program.eval(
            &mut mem,
            ctx,
            vec![IrValue::Pointer(pointer), IrValue::U32(i)],
        );
        let res = mem.read_array::<1>(pointer);
        assert_eq!(u8::from_ne_bytes(res), (i > 4) as u8, "failed at: {i}");
    }
}

#[test]
fn variable() {
    let s = src!(
        "
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
    "
    );

    let mut mem = Memory::new();
    let program = compile(s);
    let pointer = mem.allocate(1);
    let ctx = IrValue::Pointer(mem.allocate(0));
    program.eval(&mut mem, ctx, vec![IrValue::Pointer(pointer)]);
    let res = mem.read_array::<1>(pointer);
    assert_eq!(0, u8::from_ne_bytes(res));
}

#[test]
fn calling_function() {
    let s = src!(
        "
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
    "
    );

    let program = compile(s);

    for x in 0..30 {
        let mut mem = Memory::new();
        let pointer = mem.allocate(1);
        let ctx = IrValue::Pointer(mem.allocate(0));
        program.eval(
            &mut mem,
            ctx,
            vec![IrValue::Pointer(pointer), IrValue::U32(x)],
        );
        let res = mem.read_array::<1>(pointer);
        assert_eq!(!(10 < x && x < 20) as u8, u8::from_ne_bytes(res));
    }
}

#[test]
fn anonymous_record() {
    let s = src!(
        "
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
    "
    );

    let program = compile(s);

    for x in 0..30 {
        let mut mem = Memory::new();
        let pointer = mem.allocate(1);
        let ctx = IrValue::Pointer(mem.allocate(0));
        program.eval(
            &mut mem,
            ctx,
            vec![IrValue::Pointer(pointer), IrValue::U32(x)],
        );
        let res = mem.read_array::<1>(pointer);
        assert_eq!(!(10 < x && x < 20) as u8, u8::from_ne_bytes(res));
    }
}

#[test]
fn typed_record() {
    let s = src!(
        "
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
    "
    );

    let program = compile(s);

    for x in 0..1 {
        let mut mem = Memory::new();
        let pointer = mem.allocate(1);
        let ctx = IrValue::Pointer(mem.allocate(0));
        program.eval(
            &mut mem,
            ctx,
            vec![IrValue::Pointer(pointer), IrValue::U32(x)],
        );
        let res = mem.read_array::<1>(pointer);
        assert_eq!(u8::from_ne_bytes(res), !(10 < x && x < 20) as u8);
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

    let program = compile(s);

    for x in 20..21 {
        let mut mem = Memory::new();
        let pointer = mem.allocate(1);
        let ctx = IrValue::Pointer(mem.allocate(0));
        program.eval(
            &mut mem,
            ctx,
            vec![IrValue::Pointer(pointer), IrValue::I32(x)],
        );
        let res = mem.read_array::<1>(pointer);
        assert_eq!((x != 20) as u8, u8::from_ne_bytes(res), "for x = {x}");
    }
}

#[test]
fn enum_values() {
    let s = src!(
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
    "
    );

    let program = compile(s);

    for (variant, expected) in [
        // IpV4 -> accepted
        (0, 0),
        // // IpV6 -> rejected
        (1, 1),
    ] {
        let mut mem = Memory::new();
        let verdict_pointer = mem.allocate(1);
        let afi_pointer = mem.allocate(1);
        mem.write(afi_pointer, &[variant]);
        let ctx = IrValue::Pointer(mem.allocate(0));
        program.eval(
            &mut mem,
            ctx,
            vec![
                IrValue::Pointer(verdict_pointer),
                IrValue::Pointer(afi_pointer),
            ],
        );
        let res = mem.read_array::<1>(verdict_pointer);
        assert_eq!(expected, u8::from_ne_bytes(res));
    }
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

    let program = compile(s);

    for (value, expected) in [(5, 1), (11, 0)] {
        let mut mem = Memory::new();
        let verdict_pointer = mem.allocate(1);
        let ctx = IrValue::Pointer(mem.allocate(0));
        program.eval(
            &mut mem,
            ctx,
            vec![IrValue::Pointer(verdict_pointer), IrValue::U32(value)],
        );
        let res = mem.read_array::<1>(verdict_pointer);
        assert_eq!(expected as u8, u8::from_ne_bytes(res));
    }
}

#[test]
fn ip_addr_method() {
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

    let program = compile(s);

    for (value, expected) in [(5, 1), (6, 0)] {
        let mut mem = Memory::new();
        let verdict_pointer = mem.allocate(1);
        let ctx = IrValue::Pointer(mem.allocate(0));
        program.eval(
            &mut mem,
            ctx,
            vec![IrValue::Pointer(verdict_pointer), IrValue::U32(value)],
        );
        let res = mem.read_array::<1>(verdict_pointer);
        assert_eq!(expected, u8::from_ne_bytes(res));
    }
}
