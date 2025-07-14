use std::sync::Arc;

use roto::{roto_static_method, FileTree, Runtime, Val};

#[allow(dead_code)]
#[derive(Clone, Debug)]
struct NonEmptyString {
    s: Arc<str>,
}

fn main() -> Result<(), roto::RotoReport> {
    #[cfg(feature = "logger")]
    env_logger::init();

    let mut runtime = Runtime::new();

    runtime
        .register_clone_type_with_name::<NonEmptyString>(
            "NonEmptyString",
            "...",
        )
        .unwrap();

    #[roto_static_method(runtime, NonEmptyString)]
    fn new(s: Arc<str>) -> Option<Val<NonEmptyString>> {
        if s.is_empty() {
            return None;
        }
        Some(Val(NonEmptyString { s }))
    }

    let mut compiled = FileTree::single_file("examples/optional.roto")
        .compile(runtime)
        .inspect_err(|e| eprintln!("{e}"))?;

    let func = compiled
        .get_function::<(), fn(Arc<str>) -> Option<Val<NonEmptyString>>>(
            "main",
        )
        .inspect_err(|e| eprintln!("{e}"))
        .unwrap();

    let res = func.call(&mut (), "".into());
    println!("main(\"\") = {res:?}");

    let res = func.call(&mut (), "foo".into());
    println!("main(\"foo\") = {res:?}");

    Ok(())
}
