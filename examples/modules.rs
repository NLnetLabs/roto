use std::path::Path;

use roto::{FileTree, Runtime};

fn main() -> Result<(), roto::RotoReport> {
    env_logger::init();

    let runtime = Runtime::basic().unwrap();
    let mut compiled = FileTree::directory(Path::new("examples/modules"))
        .compile(runtime, usize::BITS / 8)
        .inspect_err(|e| eprintln!("{e}"))?;

    let f = compiled.get_function::<(), (i32,), i32>("main").unwrap();

    let x = f.call(&mut (), 4i32);
    println!("main(4) = {x}");
    Ok(())
}
