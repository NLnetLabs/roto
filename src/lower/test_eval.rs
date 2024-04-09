#[track_caller]
fn typecheck(s: &str) -> Result<(), RotoReport> {
    let files = pipeline::test_file(s);

    // Unwrap on parse because a parse error in this file is never correct.
    // We only want to test for type errors.
    let (trees, spans) = pipeline::parse(&files).unwrap();
    if let Err(e) = pipeline::typecheck(&files, &trees, spans) {
        println!("{e}");
        Err(e)
    } else {
        Ok(())
    }
}

fn eval(s: &str) {

}