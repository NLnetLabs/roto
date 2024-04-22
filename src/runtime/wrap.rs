use std::{fmt::Debug, rc::Rc};

use crate::lower::value::SafeValue;

/// A function that can be referenced by Roto scripts
///
/// This is usually constructed by the [`wrap`](crate::wrap) macro. A
/// [`WrappedFunction`] contains the function pointer, the number of
/// parameters and a safe wrapper around the function, which operates on
/// [`SafeValue`]. If either the number of arguments or the types of the
/// arguments don't match, the safe wrapper will panic.
///
/// The wrapper can be called easily with [`Self::call`].
#[derive(Clone)]
pub struct WrappedFunction {
    pub pointer: *const u8,
    pub params: usize,
    pub safe: Rc<dyn for<'f> Fn(&'f [SafeValue]) -> SafeValue>,
}

impl WrappedFunction {
    /// Call the safe wrapper of the function
    pub fn call(&self, vals: impl AsRef<[SafeValue]>) -> SafeValue {
        self.safe.as_ref()(vals.as_ref())
    }
}

/// A helper macro that ignores its input and expands to a unit
#[macro_export]
macro_rules! unit {
    ($($tt:tt)*) => {
        ()
    };
}

#[macro_export]
macro_rules! under {
    ($($tt:tt)*) => {
        _
    };
}

/// Wrap a Rust function into a [`WrappedFunction`]
///
/// The macro has to know about the arguments of the function so it has to
/// be used like this:
///
/// ```rust,ignore
/// fn add(x: u32, y: u32) -> u32 {
///     x + y
/// }
///
/// wrap!(add(x, y));
/// ```
///
/// All the parameter types have to implement `TryFrom<SafeValue>` and the
/// return type has to implement `Into<SafeValue>`.
#[macro_export]
macro_rules! wrap {
    ($name:ident ( $( $ref:tt $arg:ident ),* )) => {{
        $name as fn($( $crate::under!($arg) ),* ) -> _;
        $crate::runtime::wrap::WrappedFunction {
            pointer: $name as *const u8,
            // This is a bit of a hack to count the number of arguments that
            // have been passed: construct an array and call `len` on it.
            // Equivalently, it could be generating a `1` for each arg and
            // summing them.
            // A proper way to do this is being worked on:
            // https://github.com/rust-lang/rust/issues/83527
            params: [$( $crate::unit!($arg) ),*].len(),
            safe: ::std::rc::Rc::new(move |params| {
                let [$($arg),*] = params else { panic!() };
                $(
                    let $arg = $arg.to_any();
                    let $arg = $arg.downcast_ref().unwrap();
                )*
                let u = $name($($crate::conversion!($ref $arg)),*);
                $crate::lower::value::SafeValue::from_any(Box::new(u))
            }),
        }
    }};
}

#[macro_export]
macro_rules! conversion {
    (& $arg:ident) => {
        $arg
    };
    (* $arg:ident) => {
        *$arg
    };
}

impl PartialEq for WrappedFunction {
    fn eq(&self, other: &Self) -> bool {
        // We ignore the safe wrapper, because if the pointers match, it
        // should be the same value.
        self.pointer == other.pointer && self.params == other.params
    }
}

impl Eq for WrappedFunction {}

impl Debug for WrappedFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("WrappedFunction")
            .field("pointer", &self.pointer)
            .field("params", &self.params)
            .field("safe", &"<...>")
            .finish()
    }
}
