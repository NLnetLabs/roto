use std::process::ExitCode;

use roto::Runtime;

fn main() -> ExitCode {
    #[cfg(feature = "logger")]
    env_logger::builder()
        .format_timestamp(None)
        .format_target(false)
        .init();

    let mut rt = Runtime::new();
    rt.add_io_functions();
    rt.cli()
}
