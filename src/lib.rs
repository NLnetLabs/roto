#![doc = include_str!("../README.md")]

// Needed for the roto macros
extern crate self as roto;

mod ast;

#[cfg(feature = "cli")]
mod cli;
mod codegen;
mod file_tree;
mod ir_printer;
mod label;
mod lir;
mod mir;
mod module;
pub(crate) mod parser;
mod pipeline;
mod runtime;
pub mod tools;
mod typechecker;

#[cfg(feature = "cli")]
pub use cli::cli;

pub use codegen::TypedFunc;
pub use file_tree::{FileSpec, FileTree, SourceFile};
pub use lir::eval::Memory;
pub use lir::value::IrValue;
pub use pipeline::{
    interpret, Compiled, LoweredToLir, RotoError, RotoReport,
};
pub use roto_macros::{
    roto_function, roto_method, roto_static_method, Context,
};
pub use runtime::{
    context::{Context, ContextField},
    func::Func,
    optional::Optional,
    ty::Reflect,
    val::Val,
    verdict::Verdict,
    Runtime, RuntimeType,
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
