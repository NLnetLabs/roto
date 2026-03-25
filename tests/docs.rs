use std::io::Write;

use serde::Deserialize;

#[derive(Deserialize)]
#[allow(unused)]
struct Test {
    src: String,
    lang: String,
    code: String,
    source: String,
    line: usize,
    testoutput: String,
}

#[test]
fn manual_doctests() {
    let test_file = std::fs::File::open("tests/doctests.json").unwrap();
    let test_str = std::io::read_to_string(test_file).unwrap();
    let tests: Vec<Test> = serde_json::from_str(&test_str).unwrap();

    let dir = tempfile::tempdir().unwrap();
    let path = dir.path();

    for test in tests {
        let source_path = path.join(format!(
            "{}:{}",
            test.src.replace("/", "."),
            test.line
        ));

        let mut source_file = std::fs::File::create(&source_path).unwrap();
        source_file.write_all(test.code.as_ref()).unwrap();
        drop(source_file);

        assert_cmd::Command::cargo_bin("roto")
            .unwrap()
            .arg("run")
            .arg(source_path)
            .assert()
            .stdout(test.testoutput);
    }

    drop(dir);
}
