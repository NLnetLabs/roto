use insta::{assert_snapshot, glob};
use roto::{FileTree, Runtime};

#[test]
fn parse_errors() {
    let root = std::env::current_dir().unwrap();
    glob!("scripts/parse_errors/", "*.roto", |path| {
        let runtime = Runtime::new();

        let relative_path = path.strip_prefix(&root).unwrap();
        let file_tree = FileTree::read(relative_path);
        let res = file_tree.compile(runtime);
        let Err(e) = res else {
            panic!("should not succeed");
        };
        let mut string = String::new();
        e.write(&mut string, false).unwrap();
        assert_snapshot!(string);
    });
}

#[test]
fn type_errors() {
    let root = std::env::current_dir().unwrap();
    glob!("scripts/type_errors/", "*.roto", |path| {
        let runtime = Runtime::new();

        let relative_path = path.strip_prefix(&root).unwrap();
        let file_tree = FileTree::read(relative_path);
        let res = file_tree.compile(runtime);
        let Err(e) = res else {
            panic!("should not succeed");
        };
        let mut string = String::new();
        e.write(&mut string, false).unwrap();
        assert_snapshot!(string);
    });
}
