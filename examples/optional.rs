use std::sync::Arc;

use roto::{Runtime, Val, library};

#[allow(dead_code)]
#[derive(Clone, Debug, PartialEq)]
struct NonEmptyString {
    s: Arc<str>,
}

fn main() -> Result<(), roto::RotoReport> {
    #[cfg(feature = "logger")]
    env_logger::init();

    let lib = library! {
        #[clone] type NonEmptyString = Val<NonEmptyString>;

        impl Val<NonEmptyString> {
            fn new(s: Arc<str>) -> Option<Val<NonEmptyString>> {
                if s.is_empty() {
                    return None;
                }
                Some(Val(NonEmptyString { s }))
            }
        }
    };

    let rt = Runtime::from_lib(lib).unwrap();

    let mut compiled = rt
        .compile("examples/optional.roto")
        .inspect_err(|e| eprintln!("{e}"))?;

    let func = compiled
        .get_function::<fn(Arc<str>) -> Option<Val<NonEmptyString>>>("main")
        .inspect_err(|e| eprintln!("{e}"))
        .unwrap();

    let res = func.call("".into());
    println!("main(\"\") = {res:?}");

    let res = func.call("foo".into());
    println!("main(\"foo\") = {res:?}");

    Ok(())
}
