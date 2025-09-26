use roto::{library, Runtime, Val, Verdict};

#[derive(Clone, Copy)]
struct Bla {
    x: u32,
}

fn main() -> Result<(), roto::RotoReport> {
    #[cfg(feature = "logger")]
    env_logger::init();

    let lib = library! {
        /// Some type I want to register
        copy type Bla = Val<Bla>;

        clone type Bla = Val<u32>;

        impl Val<Bla> {
            fn x(bla: Val<Bla>) -> u32 {
                bla.x
            }
        }
    };

    let runtime = Runtime::from_items(lib).unwrap();

    let mut compiled = runtime
        .compile("examples/runtime.roto")
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
