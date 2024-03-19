use std::process::exit;

use roto::compiler::Compiler;

fn main() {
    let source_file = std::env::args_os().nth(1).expect("Expected a file");
    let source_code = std::fs::read_to_string(source_file).expect("Could not read from file");
    let result = Compiler::new().build_from_compiler(&source_code);
    if let Err(err) = result {
        eprintln!("{err}");
        exit(1);
    }
    println!("Compilation succesful!")
}
