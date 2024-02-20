fn main() {
    let mut args = std::env::args();
    args.next();

    let cmd = args.next().expect("missing command");

    match cmd.as_ref() {
        "parse" => {
            let file = args.next().expect("need a file to parse");
            parse(&file);
        }
        _ => panic!("unrecognized command: {cmd}"),
    };
}

fn parse(file: &str) {
    let contents = std::fs::read_to_string(file).unwrap();
    match roto::parser::Parser::parse(&contents) {
        Ok(contents) => println!("{contents:?}"),
        Err(err) => println!("{err}"),
    };
}
