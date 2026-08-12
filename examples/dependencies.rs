use std::process::ExitCode;

use roto::{Runtime, deps::Directory};

fn main() -> ExitCode {
    #[cfg(feature = "logger")]
    env_logger::init();

    let mut rt = Runtime::new();

    rt.add_io_functions();
    rt.set_dependency_source(Directory::new("examples/dependencies/dep"));

    let result =
        rt.compile_with_deps("examples/dependencies.roto", &["math"]);

    let mut pkg = match result {
        Ok(pkg) => pkg,
        Err(e) => {
            eprint!("{e}");
            return ExitCode::FAILURE;
        }
    };

    let func = pkg.get_function::<fn() -> ()>("main").unwrap();
    func.call();

    ExitCode::SUCCESS
}
