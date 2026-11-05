#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: (bool, &str)| {
    let mut lexer = roto::fuzz::Lexer::new(0, data.1);

    if data.0 {
        lexer.next();
    } else {
        lexer.f_string_part();
    }
});
