use roto::{read_files, Runtime};

struct Bla {
    _x: u16,
    y: u8,
    _z: u32,
}

extern "C" fn get_y(bla: *const Bla) -> u8 {
    unsafe { &*bla }.y
}

fn main() -> Result<(), roto::RotoReport> {
    let mut runtime = Runtime::default();

    runtime.register_type::<Bla>();
    runtime.register_method::<Bla, _, _>("x", get_y as extern "C" fn(_) -> _);

    let compiled = read_files(["examples/simple.roto"])
        .and_then(|x| x.parse())
        .and_then(|x| x.typecheck(runtime, usize::BITS / 8))
        .map(|x| x.lower().codegen())?;

    let func = compiled
        .module
        .get_function::<(*mut Option<u8>, *mut Bla), ()>("main")
        .unwrap();

    for y in 0..20 {
        let mut res: Option<u8> = None;

        // Quick custom assertion that this is the size that Roto expects
        // TODO: This should be automated at some point.
        assert_eq!(std::mem::size_of_val(&res), 2);

        let res_ptr = &mut res as *mut _;
        let mut bla = Bla { _x: 1, y, _z: 1 };
        func.call((res_ptr, &mut bla as *mut _));

        let expected = if y > 10 { Some(y * 2) } else { None };
        println!("main({y}) = {res:?}   (expected: {expected:?})");
    }
    Ok(())
}
