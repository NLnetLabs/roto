use std::{env::args, net::IpAddr};

use roto::{read_files, Runtime, Verdict};

fn main() -> Result<(), roto::RotoReport> {
    env_logger::init();

    let runtime = Runtime::basic().unwrap();

    let mut arguments = args();
    let _program_name = arguments.next().unwrap();

    let subcommand = arguments.next();
    if Some("doc") == subcommand.as_deref() {
        runtime.print_documentation();
        return Ok(());
    }

    let mut compiled = read_files(["examples/simple.roto"])?
        .compile(runtime, usize::BITS / 8)
        .inspect_err(|e| eprintln!("{e}"))?;

    let func = compiled
        .get_function::<(), (IpAddr,), Verdict<(), ()>>("main")
        .inspect_err(|e| eprintln!("{e}"))
        .unwrap();

    let res = func.call(&mut (), "0.0.0.0".parse().unwrap());
    println!("main(0.0.0.0) = {res:?}");

    let res = func.call(&mut (), "1.1.1.1".parse().unwrap());
    println!("main(1.1.1.1) = {res:?}");

    println!();
    let _ = compiled.run_tests(());

    Ok(())
}
