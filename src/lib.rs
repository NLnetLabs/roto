// Needed for the roto macros
extern crate self as roto;

pub(crate) mod ast;

pub(crate) mod codegen;
pub(crate) mod lower;
pub(crate) mod parser;
pub(crate) mod typechecker;
pub(crate) mod walker;

pub(crate) mod pipeline;
pub(crate) mod runtime;

pub use codegen::TypedFunc;
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
