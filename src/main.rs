use roto::pipeline;

fn main() {
    env_logger::builder()
        .format_timestamp(None)
        .format_target(false)
        .init();
    let result = pipeline::run(std::env::args().skip(1));
    match result {
        Ok(r) => println!("{r}"),
        Err(e) => eprintln!("{e}"),
    }
}
