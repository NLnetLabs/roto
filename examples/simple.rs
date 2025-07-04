use std::{net::IpAddr, process::ExitCode};

use roto::{Runtime, Verdict};

fn main() -> ExitCode {
    #[cfg(feature = "logger")]
    env_logger::init();

    let runtime = Runtime::new();

    let result = runtime.compile("example/simple.roto");
    let mut compiled = match result {
        Ok(compiled) => compiled,
        Err(e) => {
            eprint!("{e}");
            return ExitCode::FAILURE;
        }
    };

    let func = compiled
        .get_function::<(), fn(IpAddr) -> Verdict<(), ()>>("main")
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

    ExitCode::SUCCESS
}
