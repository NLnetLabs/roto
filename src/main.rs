use roto::pipeline;

fn main() {
    let result = pipeline::run(std::env::args().skip(1));
    match result {
        Ok(_) => eprintln!("Compilation succesful!"),
        Err(e) => eprintln!("{e}")
    }
}
