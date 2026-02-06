use roto::{Context, Runtime, String};

#[derive(Clone, Context)]
struct Ctx {
    pub first_name: String,
    pub last_name: String,
}

fn main() {
    let runtime = Runtime::new().with_context_type::<Ctx>().unwrap();

    let mut pkg = runtime.compile("examples/context.roto").unwrap();

    let f = pkg.get_function::<fn() -> String>("greeting").unwrap();

    let mut ctx = Ctx {
        first_name: "John".into(),
        last_name: "Doe".into(),
    };
    println!("{}", f.call(&mut ctx));
}
