//! Compiler stage to transform the AST into IR
//!
//! For more information on the IR, see the [ir] module.

pub mod eval;
pub mod ir;
pub mod lower;
mod print;
pub mod value;

#[cfg(test)]
mod test_eval;

pub use ir::*;
pub use lower::lower_to_lir;

use ir::{Block, Function, Instruction, Lir, Operand, Var, VarKind};
