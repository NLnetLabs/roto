mod dead_code;
pub mod ir;
mod lower;
mod print;

pub use ir::*;
pub use lower::lower_to_mir;
