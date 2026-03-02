use super::{eval::Memory, value::IrValue};
use crate::{
    FileTree, Runtime, library, pipeline::LoweredToLir, runtime::OptCtx, src,
    value::String,
};

#[track_caller]
fn compile<Ctx: OptCtx>(
    s: FileTree,
    rt: &Runtime<Ctx>,
) -> LoweredToLir<'_, Ctx> {
    // We run this multiple times and only want to init the
    // first time, so ignore failures.
    #[cfg(feature = "logger")]
    let _ = env_logger::builder()
        .format_timestamp(None)
        .format_target(false)
        .try_init();

    s.parse()
        .unwrap()
        .typecheck(rt)
        .unwrap()
        .lower_to_mir()
        .lower_to_lir()
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
        if !offset.is_multiple_of(align) {
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
        filtermap main(msg: u32) {
            accept
        }
    "
    );

    let mut mem = Memory::new();
    let rt = Runtime::new();
    let program = compile(s, &rt);
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
        filtermap main() {
            reject
        }
    "
    );

    let mut mem = Memory::new();
    let rt = Runtime::new();
    let program = compile(s, &rt);
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
        filtermap main() {
            if true && true {
                accept
            } else {
                reject
            }
        }      
    "
    );
    let mut mem = Memory::new();
    let rt = Runtime::new();
    let program = compile(s, &rt);
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
        filtermap main(x: u32) {
            if x <= 4 {
                accept
            } else {
                reject
            }
        }
    "
    );

    let rt = Runtime::new();
    let program = compile(s, &rt);

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
    filtermap main() {
        let a = 5;
        if a == 5 {
            accept
        } else {
            reject
        }
    }
    "
    );

    let mut mem = Memory::new();
    let rt = Runtime::new();
    let program = compile(s, &rt);
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
        fn smaller_than(a: u32, b: u32) -> bool {
            a < b
        }

        fn small(x: u32) -> bool {
            smaller_than(10, x) && smaller_than(x, 20)
        }

        filtermap main(msg: u32) {
            if small(msg) { accept }
            reject
        }
    "
    );

    let rt = Runtime::new();
    let program = compile(s, &rt);

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
        fn in_range(x: u32, low: u32, high: u32) -> bool {
            low < x && x < high
        }

        filtermap main(msg: u32) {
            let a = { low: 10, high: 20 };
            if in_range(msg, a.low, a.high) { accept }
            reject
        }
    "
    );

    let rt = Runtime::new();
    let program = compile(s, &rt);

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
        record Range {
            low: u32,
            high: u32,
        }

        fn in_range(x: u32, c: Range) -> bool {
            c.low < x && x < c.high
        }

        filtermap main(msg: u32) {
            let a = Range { low: 10, high: 20 };
            let b = Range { low: a.low, high: a.high };
            let c = b;
            if in_range(msg, c) { accept }
            reject
        }
    "
    );

    let rt = Runtime::new();
    let program = compile(s, &rt);

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
        record Foo { x: Bar, y: Bar }
        record Bar { a: i32, b: i32 }

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

    let rt = Runtime::new();
    let program = compile(s, &rt);

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

    let rt = Runtime::from_lib(library! {
        fn pow(x: u32, y: u32) -> u32 {
            x.pow(y)
        }
    })
    .unwrap();

    let program = compile(s, &rt);

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
fn u32_method() {
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

    let rt = Runtime::from_lib(library! {
        impl u32 {
            fn is_even(x: u32) -> bool {
                x.is_multiple_of(2)
            }
        }
    })
    .unwrap();
    let program = compile(s, &rt);

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

#[test]
fn string_global() {
    let s = src!(
        r#"fn main() -> bool {
            FOO == "BAR"
        }"#
    );

    let rt = Runtime::from_lib(library! {
        const FOO: String = "BAR".into();
    })
    .unwrap();

    let p = compile(s, &rt);

    let mut mem = Memory::new();
    let ctx = IrValue::Pointer(mem.allocate(0));
    let res = p.eval(&mut mem, ctx, Vec::new()).unwrap();

    assert_eq!(res, IrValue::Bool(true));
}
