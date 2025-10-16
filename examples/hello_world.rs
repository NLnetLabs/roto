use roto::Runtime;

fn main() -> Result<(), roto::RotoReport> {
    let mut runtime = Runtime::new();
    runtime.add_io_functions();

    let mut compiled = runtime
        .compile("examples/hello_world.roto")
        .inspect_err(|e| eprintln!("{e}"))?;

    let func = compiled
        .get_function::<fn() -> ()>("main")
        .inspect_err(|e| eprintln!("{e}"))
        .unwrap();

    func.call();

    Ok(())
}
