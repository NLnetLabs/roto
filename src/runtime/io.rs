use std::sync::Arc;

use roto_macros::roto_function;

use crate::Runtime;

impl Runtime {
    /// Add functions using I/O to the runtime.
    ///
    /// These functions are disabled by default because Roto might be used in a
    /// context where using I/O is not permitted.
    ///
    /// For now, this just adds the `print` function. More functions will be
    /// added in the future.
    pub fn add_io_functions(&mut self) {
        let rt = self;

        /// Print a string to stdout
        #[roto_function(rt)]
        fn print(s: Arc<str>) {
            println!("{s}");
        }
    }
}
