use roto::{read_files, Runtime, Verdict};

struct Bla {
    _x: u16,
    y: u32,
    _z: u32,
}

extern "C" fn get_y(bla: *const Bla) -> u32 {
    unsafe { &*bla }.y
}

fn main() -> Result<(), roto::RotoReport> {
    env_logger::init();

    let mut runtime = Runtime::basic().unwrap();

    runtime.register_type::<Bla>().unwrap();
    runtime
        .register_method::<Bla, _, _>("y", get_y as extern "C" fn(_) -> _)
        .unwrap();

    let mut compiled = read_files(["examples/simple.roto"])?
        .compile(runtime, usize::BITS / 8)
        .inspect_err(|e| eprintln!("{e}"))?;

    let func = compiled
        .get_function::<(*mut Bla,), Verdict<u32, ()>>("main")
        .inspect_err(|e| eprintln!("{e}"))
        .unwrap();

    for y in 0..20 {
        let mut bla = Bla { _x: 1, y, _z: 1 };
        let res = func.call((&mut bla as *mut _,));

        let expected = if y > 10 {
            Verdict::Accept(y * 2)
        } else {
            Verdict::Reject(())
        };
        println!("main({y}) = {res:?}   (expected: {expected:?})");
    }
    Ok(())
}
