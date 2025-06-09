use roto::{FileTree, Runtime, Val, Verdict};
use roto_macros::roto_method;

#[derive(Clone, Copy)]
struct Bla {
    x: u32,
}

fn main() -> Result<(), roto::RotoReport> {
    env_logger::init();

    let mut runtime = Runtime::new();

    runtime
        .register_copy_type::<Bla>("Some random type")
        .unwrap();

    #[roto_method(runtime, Bla, x)]
    fn get_x(bla: Val<Bla>) -> u32 {
        bla.x
    }

    let mut compiled = FileTree::single_file("examples/runtime.roto")
        .compile(runtime)
        .inspect_err(|e| eprintln!("{e}"))?;

    let func = compiled
        .get_function::<(), fn(Val<Bla>) -> Verdict<u32, ()>>("main")
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
