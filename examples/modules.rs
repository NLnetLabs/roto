use roto::Runtime;

fn main() -> Result<(), roto::RotoReport> {
    #[cfg(feature = "logger")]
    env_logger::init();

    let runtime = Runtime::new();
    let mut compiled = runtime
        .compile("examples/modules")
        .inspect_err(|e| eprintln!("{e}"))?;

    let f = compiled.get_function::<(), fn(i32) -> i32>("main").unwrap();

    let x = f.call(&mut (), 4i32);
    println!("main(4) = {x}");
    Ok(())
}
