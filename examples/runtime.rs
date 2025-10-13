use roto::{library, Runtime, Val, Verdict};

#[derive(Clone, Copy)]
struct Vec2 {
    x: i32,
    y: i32,
}

fn main() -> Result<(), roto::RotoReport> {
    #[cfg(feature = "logger")]
    env_logger::init();

    let lib = library! {
        /// Some type I want to register
        #[copy] type Vec2 = Val<Vec2>;

        const ZERO: Val<Vec2> = Val(Vec2 {
            x: 0,
            y: 0,
        });

        impl Val<Vec2> {
            fn x(v: Val<Vec2>) -> i32 {
                v.x
            }

            fn y(v: Val<Vec2>) -> i32 {
                v.y
            }
        }
    };

    let runtime = Runtime::from_lib(lib).unwrap();

    let mut compiled = runtime
        .compile("examples/runtime.roto")
        .inspect_err(|e| eprintln!("{e}"))?;

    let func = compiled
        .get_function::<fn(Val<Vec2>) -> Verdict<i32, ()>>("main")
        .inspect_err(|e| eprintln!("{e}"))
        .unwrap();

    for x in 0..20 {
        let vec = Vec2 { x, y: 0 };

        let res = func.call(Val(vec));
        let expected = if x > 10 {
            Verdict::Accept(x * 2)
        } else {
            Verdict::Reject(())
        };
        println!("main({x}) = {res:?}   (expected: {expected:?})");
    }
    Ok(())
}
