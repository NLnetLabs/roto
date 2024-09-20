pub(crate) mod ast;

pub(crate) mod codegen;
pub(crate) mod lower;
pub(crate) mod parser;
pub(crate) mod typechecker;

pub(crate) mod pipeline;
pub(crate) mod runtime;

pub use codegen::TypedFunc;
pub use lower::eval::Memory;
pub use lower::value::IrValue;
pub use pipeline::*;
pub use runtime::{verdict::Verdict, Runtime, RuntimeType};
