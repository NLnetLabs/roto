use roto::{read_files, Runtime, Val, Verdict};
use roto_macros::roto_method;

#[derive(Clone, Copy)]
struct Bla {
    x: u32,
}

fn main() -> Result<(), roto::RotoReport> {
    env_logger::init();

    let mut runtime = Runtime::basic().unwrap();

    runtime
        .register_copy_type::<Bla>("Some random type")
        .unwrap();

    #[roto_method(runtime, Bla, x)]
    fn get_x(bla: *const Bla) -> u32 {
        unsafe { &*bla }.x
    }

    let mut compiled = read_files(["examples/runtime.roto"])?
        .compile(runtime, usize::BITS / 8)
        .inspect_err(|e| eprintln!("{e}"))?;

    let func = compiled
        .get_function::<(), (Val<Bla>,), Verdict<u32, ()>>("main")
        .inspect_err(|e| eprintln!("{e}"))
        .unwrap();

    for x in 0..20 {
        let bla = Bla { x };

        let res = func.call(&mut (), Val(bla));
        let expected = if x > 10 {
            Verdict::Accept(x * 2)
        } else {
            Verdict::Reject(())
        };
        println!("main({x}) = {res:?}   (expected: {expected:?})");
    }
    Ok(())
}
