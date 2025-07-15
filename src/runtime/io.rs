use std::sync::Arc;

use roto_macros::roto_function;

use crate::Runtime;

impl Runtime {
    pub fn add_io_functions(&mut self) {
        let rt = self;

        #[roto_function(rt)]
        fn print(s: Arc<str>) {
            println!("{s}");
        }
    }
}
