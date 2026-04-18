use roto::{Context, RotoString, Runtime};

#[derive(Clone, Context)]
struct Ctx {
    pub first_name: RotoString,
    pub last_name: RotoString,
}

fn main() {
    let runtime = Runtime::new().with_context_type::<Ctx>().unwrap();

    let mut pkg = runtime.compile("examples/context.roto").unwrap();

    let f = pkg.get_function::<fn() -> RotoString>("greeting").unwrap();

    let mut ctx = Ctx {
        first_name: "John".into(),
        last_name: "Doe".into(),
    };
    println!("{}", f.call(&mut ctx));
}
