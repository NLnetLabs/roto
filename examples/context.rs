use std::sync::Arc;

use roto::{Context, Runtime};

#[derive(Clone, Context)]
struct Ctx {
    pub first_name: Arc<str>,
    pub last_name: Arc<str>,
}

fn main() {
    let runtime = Runtime::new().with_context_type::<Ctx>().unwrap();

    let mut pkg = runtime.compile("examples/context.roto").unwrap();

    let f = pkg.get_function::<fn() -> Arc<str>>("greeting").unwrap();

    let mut ctx = Ctx {
        first_name: "John".into(),
        last_name: "Doe".into(),
    };
    println!("{}", f.call(&mut ctx));
}
