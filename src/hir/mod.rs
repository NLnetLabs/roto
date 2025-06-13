pub mod ir;
mod lower;
mod print;

pub use lower::lower_to_hir;
