use std::{net::IpAddr, process::ExitCode};

use roto::{Runtime, Verdict};

fn main() -> ExitCode {
    #[cfg(feature = "logger")]
    env_logger::init();

    let runtime = Runtime::new();

    let result = runtime.compile("examples/simple.roto");
    let mut pkg = match result {
        Ok(pkg) => pkg,
        Err(e) => {
            eprint!("{e}");
            return ExitCode::FAILURE;
        }
    };

    let func = pkg
        .get_function::<fn(IpAddr) -> Verdict<(), ()>>("main")
        .unwrap();

    let res = func("0.0.0.0".parse().unwrap());
    println!("main(0.0.0.0) = {res:?}");

    let res = func("1.1.1.1".parse().unwrap());
    println!("main(1.1.1.1) = {res:?}");

    let is_zero = pkg.get_function::<fn(IpAddr) -> bool>("is_zero").unwrap();

    let res = is_zero("0.0.0.0".parse().unwrap());
    println!("is_zero(0.0.0.0) = {res:?}");

    println!();
    let _ = pkg.run_tests();

    ExitCode::SUCCESS
}
