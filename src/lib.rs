#![doc = include_str!("../README.md")]

// Needed for the roto macros
extern crate self as roto;

mod ast;

mod cli;
mod codegen;
mod file_tree;
mod lower;
mod module;
mod parser;
mod pipeline;
mod runtime;
pub mod tools;
mod typechecker;

pub use cli::cli;
pub use codegen::TypedFunc;
pub use file_tree::{FileTree, SourceFile};
pub use lower::eval::Memory;
pub use lower::value::IrValue;
pub use pipeline::{interpret, Compiled, Lowered, RotoError, RotoReport};
pub use roto_macros::{
    roto_function, roto_method, roto_static_method, Context,
};
pub use runtime::{
    context::{Context, ContextField},
    optional::Optional,
    val::Val,
    verdict::Verdict,
    DocumentedFunc, Runtime, RuntimeType,
};

pub(crate) const FIND_HELP: &str = "\n\
    If you are seeing this error you have found a bug in the Roto compiler.\n\
    Please open an issue at https://github.com/NLnetLabs/roto.";

/// Panic with an internal compiler error
///
/// Calling this macro instead of [`panic!`] signals a bug in the compiler
macro_rules! ice {
    () => {
        panic!("Internal compiler error{}", $crate::FIND_HELP)
    };
    ($s:literal) => {
        panic!("Internal compiler error: {}{}", format!($s), $crate::FIND_HELP)
    };
    ($s:literal, $($t:tt)*) => {
        panic!("Internal compiler error: {}{}", format!($s, $($t)*), $crate::FIND_HELP)
    }
}

pub(crate) use ice;
