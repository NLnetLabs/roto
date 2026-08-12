use insta::{assert_snapshot, glob};
use roto::Runtime;

#[test]
#[cfg(not(miri))]
fn parse_errors() {
    let root = std::env::current_dir().unwrap();
    glob!("scripts/parse_errors/", "*.roto", |path| {
        let runtime = Runtime::new();

        let relative_path = path.strip_prefix(&root).unwrap();
        let res = runtime.compile(relative_path);
        let Err(e) = res else {
            panic!("{path:?} should not succeed");
        };
        let mut string = String::new();
        e.write(&mut string, false).unwrap();
        assert_snapshot!(string);
    });
}

#[test]
#[cfg(not(miri))]
fn type_errors() {
    let root = std::env::current_dir().unwrap();
    glob!("scripts/type_errors/", "*.roto", |path| {
        let runtime = Runtime::new();

        let relative_path = path.strip_prefix(&root).unwrap();
        let res = runtime.compile(relative_path);
        let Err(e) = res else {
            panic!("should not succeed");
        };
        let mut string = String::new();
        e.write(&mut string, false).unwrap();
        assert_snapshot!(string);
    });
}
