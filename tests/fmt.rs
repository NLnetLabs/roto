use std::path::Path;

use insta::assert_snapshot;
use pretty_assertions::assert_eq;
use roto::fmt::fmt_str;

#[test]
fn fmt_tests() {
    let s = std::fs::read_to_string("tests/fmt_tests.roto").unwrap();
    let mut s = &*s;

    let mut line = 0;
    let mut name;
    let mut content;
    while !s.is_empty() {
        (name, s) = parse_header(s);
        line += 3;
        (content, s) = parse_content(s);

        let formatted =
            fmt_str("tests/fmt_tests.roto", content, line).unwrap();

        assert_snapshot!(name, formatted);
        line += content.lines().count();
    }
}

#[test]
fn examples_idempotent() {
    run_on_all_roto_files(Path::new("examples"), &|path| {
        let s = std::fs::read_to_string(path).unwrap();
        let formatted = fmt_str(&path.to_string_lossy(), &s, 0).unwrap();
        assert_eq!(s, formatted, "path: {}", path.display());
    });
}

fn run_on_all_roto_files(path: &Path, f: &impl Fn(&Path)) {
    for entry in std::fs::read_dir(path).unwrap() {
        let entry = entry.unwrap();
        let ty = entry.file_type().unwrap();
        if ty.is_dir() {
            run_on_all_roto_files(&entry.path(), f);
        } else if entry.path().extension().is_some_and(|s| s == "roto") {
            f(&entry.path())
        }
    }
}

fn parse_header(mut s: &str) -> (&str, &str) {
    s = strip_header_marker(s);
    let header;
    (header, s) = s.split_once('\n').unwrap();
    s = strip_header_marker(s);
    (header.trim().strip_prefix("// ").unwrap(), s)
}

fn parse_content(s: &str) -> (&str, &str) {
    match s.find("// ===") {
        Some(idx) => s.split_at(idx),
        None => (s, ""),
    }
}

fn strip_header_marker(s: &str) -> &str {
    s.strip_prefix("// =")
        .unwrap()
        .trim_start_matches('=')
        .strip_prefix('\n')
        .unwrap()
}
