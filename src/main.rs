use clap::Parser;
use roto::{IrValue, Memory, Runtime};

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
        Some("true") => vec![IrValue::Bool(true)],
        Some("false") => vec![IrValue::Bool(false)],
        Some(x) => vec![IrValue::U32(x.parse().unwrap())],
        _ => vec![],
    };

    let mut mem = Memory::new();
    let _result = roto::run(Runtime::default(), settings.files, &mut mem, rx);
    // match result {
    //     Ok(r) => println!("{r}"),
    //     Err(e) => eprintln!("{e}"),
    // }
}
