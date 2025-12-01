use std::process::ExitCode;

use roto::{List, Runtime};

fn main() -> ExitCode {
    #[cfg(feature = "logger")]
    env_logger::init();

    let runtime = Runtime::new();

    let result = runtime.compile("examples/list.roto");
    let mut pkg = match result {
        Ok(pkg) => pkg,
        Err(e) => {
            eprint!("{e}");
            return ExitCode::FAILURE;
        }
    };

    let func = pkg
        .get_function::<fn(List<u64>) -> List<u64>>("add_numbers")
        .unwrap();

    let mut list = List::new();
    list.push(0);

    let new_list = func.call(list);
    let v = new_list.to_vec();
    println!("Result: {v:?}");

    ExitCode::SUCCESS
}
