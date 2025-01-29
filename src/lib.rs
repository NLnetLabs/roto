// Needed for the roto macros
extern crate self as roto;

mod ast;

mod codegen;
mod file_tree;
mod lower;
mod module;
mod parser;
mod pipeline;
mod runtime;
mod typechecker;

pub use codegen::TypedFunc;
pub use file_tree::FileTree;
pub use lower::eval::Memory;
pub use lower::value::IrValue;
pub use pipeline::*;
pub use roto_macros::{
    roto_function, roto_method, roto_static_method, Context,
};
pub use runtime::{
    context::{Context, ContextField},
    val::Val,
    verdict::Verdict,
    DocumentedFunc, Runtime, RuntimeType,
};
