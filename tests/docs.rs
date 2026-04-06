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
    mode: String,
}

#[test]
#[cfg(all(feature = "cli", not(miri)))]
fn manual_doctests() {
    let test_file = std::fs::File::open("tests/doctests.json").unwrap();
    let test_str = std::io::read_to_string(test_file).unwrap();
    let tests: Vec<Test> = serde_json::from_str(&test_str).unwrap();

    let dir = tempfile::tempdir().unwrap();
    let path = dir.path();

    for test in tests {
        if test.mode == "ignore" {
            continue;
        }

        let source_path = path.join(format!(
            "{}:{}",
            test.src.replace("/", "."),
            test.line
        ));

        let has_main = roto::has_main_function(&test.code).unwrap_or(false);

        let mut source_file = std::fs::File::create(&source_path).unwrap();

        if !has_main {
            source_file.write_all(b"fn main() {\n").unwrap();
        }
        source_file.write_all(test.code.as_ref()).unwrap();
        if !has_main {
            source_file.write_all(b"\n}").unwrap();
        }
        drop(source_file);

        // This is inefficient, but really helpful for debuggin the
        // tests.
        dbg!(std::fs::read_to_string(&source_path).unwrap());

        match &*test.mode {
            "run" => {
                assert_cmd::Command::cargo_bin("roto")
                    .unwrap()
                    .arg("run")
                    .arg(source_path)
                    .assert()
                    .stdout(test.testoutput)
                    .stderr("");
            }
            "check" => {
                assert_cmd::Command::cargo_bin("roto")
                    .unwrap()
                    .arg("check")
                    .arg(source_path)
                    .assert()
                    .stdout("All ok!\n")
                    .stderr("")
                    .success();
            }
            "error" => {
                assert_cmd::Command::cargo_bin("roto")
                    .unwrap()
                    .arg("check")
                    .arg(source_path)
                    .assert()
                    .failure();
            }
            _ => {
                panic!("unknown test mode: {}", test.mode);
            }
        }
    }

    drop(dir);
}
