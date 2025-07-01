use std::{env::args, net::IpAddr};

use roto::{FileTree, Runtime, Verdict};

fn main() -> Result<(), roto::RotoReport> {
    #[cfg(feature = "logger")]
    env_logger::init();

    let runtime = Runtime::new();

    let mut arguments = args();
    let _program_name = arguments.next().unwrap();

    let subcommand = arguments.next();
    if Some("doc") == subcommand.as_deref() {
        runtime.print_documentation();
        return Ok(());
    }

    let mut compiled = FileTree::single_file("examples/simple.roto")
        .compile(runtime)
        .inspect_err(|e| eprintln!("{e}"))?;

    let func = compiled
        .get_function::<(), fn(IpAddr) -> Verdict<(), ()>>("main")
        .inspect_err(|e| eprintln!("{e}"))
        .unwrap();

    let res = func.call(&mut (), "0.0.0.0".parse().unwrap());
    println!("main(0.0.0.0) = {res:?}");

    let res = func.call(&mut (), "1.1.1.1".parse().unwrap());
    println!("main(1.1.1.1) = {res:?}");

    let is_zero = compiled
        .get_function::<(), fn(IpAddr) -> bool>("is_zero")
        .unwrap();

    let res = is_zero.call(&mut (), "0.0.0.0".parse().unwrap());
    println!("is_zero(0.0.0.0) = {res:?}");

    println!();
    let _ = compiled.run_tests(());

    Ok(())
}
