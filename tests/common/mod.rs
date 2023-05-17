use std::io::Write;

pub fn init() {
        let _ = env_logger::builder().format(|buf, record| {
            writeln!(buf, "{}", record.args())
        }).is_test(true).try_init();
    }