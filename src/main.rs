use clap::Parser;
use roto::{cli, Runtime};

#[derive(Parser)]
struct Cli {
    #[arg(short, long)]
    rx: Option<String>,
    file: String,
}

fn main() {
    #[cfg(feature = "logger")]
    env_logger::builder()
        .format_timestamp(None)
        .format_target(false)
        .init();

    cli(Runtime::new());
}
