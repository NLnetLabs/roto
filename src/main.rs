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

    let args = match settings.rx.as_ref().map(AsRef::as_ref) {
        Some("true") => vec![IrValue::Bool(true)],
        Some("false") => vec![IrValue::Bool(false)],
        Some(x) => vec![IrValue::U32(x.parse().unwrap())],
        _ => vec![],
    };

    let mut mem = Memory::new();

    let ptr = mem.allocate(0);
    let result = roto::interpret(
        Runtime::basic().unwrap(),
        settings.files,
        &mut mem,
        IrValue::Pointer(ptr),
        args,
    );
    match result {
        Ok(_) => println!("Ok!"),
        Err(e) => eprintln!("{e}"),
    }
}
