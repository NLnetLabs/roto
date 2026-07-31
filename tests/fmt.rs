use insta::assert_snapshot;
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
