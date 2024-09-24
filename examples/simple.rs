use roto::{read_files, Runtime, Verdict};
use roto_macros::roto_function;

struct Bla {
    _x: u16,
    y: u32,
    _z: u32,
}

#[roto_function]
fn get_y(bla: *const Bla) -> u32 {
    unsafe { &*bla }.y
}

fn main() -> Result<(), roto::RotoReport> {
    env_logger::init();

    let mut runtime = Runtime::basic().unwrap();

    runtime.register_type::<Bla>().unwrap();
    runtime.register_method::<Bla, _, _>("y", get_y).unwrap();

    let mut compiled = read_files(["examples/simple.roto"])?
        .compile(runtime, usize::BITS / 8)
        .inspect_err(|e| eprintln!("{e}"))?;

    let func = compiled
        .get_function::<(*mut Bla,), Verdict<u32, ()>>("main")
        .inspect_err(|e| eprintln!("{e}"))
        .unwrap();

    let func2 = compiled
        .get_function::<(u32,), Verdict<(), ()>>("just_reject")
        .inspect_err(|e| eprintln!("{e}"))
        .unwrap();

    // We should now be able to drop this safely, because each func has an Arc
    // to the data it references.
    drop(compiled);

    for y in 0..20 {
        let mut bla = Bla { _x: 1, y, _z: 1 };

        let func = func.clone();
        std::thread::spawn(move || {
            let res = func.call(&mut bla as *mut _);
            let expected = if y > 10 {
                Verdict::Accept(y * 2)
            } else {
                Verdict::Reject(())
            };
            println!("main({y}) = {res:?}   (expected: {expected:?})");
        })
        .join()
        .unwrap();

        let res = func2.call(y);
        println!("{res:?}");
    }
    Ok(())
}
