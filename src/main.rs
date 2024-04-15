use clap::Parser;
use roto::{lower::value::SafeValue, pipeline};

#[derive(Parser)]
struct Cli {
    #[arg(short, long)]
    rx: Option<String>,
    files: Vec<String>,
}

fn main() {
    env_logger::builder()
        .format_timestamp(None)
        .format_target(false)
        .init();

    let settings = Cli::parse();

    let rx = match settings.rx.as_ref().map(AsRef::as_ref) {
        Some("true") => SafeValue::Bool(true),
        Some("false") => SafeValue::Bool(false),
        Some(x) => SafeValue::U32(x.parse().unwrap()),
        _ => SafeValue::Unit,
    };
    let result = pipeline::run(settings.files, rx);
    match result {
        Ok(r) => println!("{r}"),
        Err(e) => eprintln!("{e}"),
    }
}
