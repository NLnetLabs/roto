use roto::{read_files, Runtime, Verdict};

struct Bla {
    _x: u16,
    y: u8,
    _z: u32,
}

extern "C" fn get_y(bla: *const Bla) -> u8 {
    unsafe { &*bla }.y
}

fn main() -> Result<(), roto::RotoReport> {
    let mut runtime = Runtime::basic().unwrap();

    runtime.register_type::<Bla>().unwrap();
    runtime
        .register_method::<Bla, _, _>("x", get_y as extern "C" fn(_) -> _)
        .unwrap();

    let mut compiled = read_files(["examples/simple.roto"])?
        .compile(runtime, usize::BITS / 8)
        .inspect_err(|e| eprintln!("{e}"))?;

    let func = compiled
        .get_function::<(*mut Bla,), Verdict<u8, ()>>("main")
        .unwrap();

    for y in 0..20 {
        let mut bla = Bla { _x: 1, y, _z: 1 };
        let res = func.call((&mut bla as *mut _,));

        // Quick custom assertion that this is the size that Roto expects
        // TODO: This should be automated at some point.
        assert_eq!(std::mem::size_of_val(&res), 8);

        let expected = if y > 10 {
            Verdict::Accept(y * 2)
        } else {
            Verdict::Reject(())
        };
        println!("main({y}) = {res:?}   (expected: {expected:?})");
    }
    Ok(())
}
