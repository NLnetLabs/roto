use std::{fmt::Debug, rc::Rc};

use super::value::SafeValue;

#[derive(Clone)]
pub struct WrappedFunction {
    pub pointer: *const u8,
    pub params: usize,
    pub safe: Rc<dyn for<'f> Fn(&'f [SafeValue]) -> SafeValue>,
}

#[macro_export]
macro_rules! unit {
    ($($tt:tt)*) => {
        () 
    };
}

#[macro_export]
macro_rules! wrap {
    ($name:ident ( $( $arg:ident ),* )) => {
        $crate::lower::wrap::WrappedFunction {
            pointer: $name as *const u8,
            params: [$( $crate::unit!($arg) ),*].len(),
            safe: ::std::rc::Rc::new(move |params| {
                let [$($arg),*] = params else { panic!() };
                let u = $name($($arg.try_into().unwrap()),*);
                u.into()
            }),
        }
    };
}

impl PartialEq for WrappedFunction {
    fn eq(&self, other: &Self) -> bool {
        self.pointer == other.pointer
            && self.params == other.params
            && std::ptr::addr_eq(
                Rc::as_ptr(&self.safe),
                Rc::as_ptr(&other.safe),
            )
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

impl WrappedFunction {
    pub fn call(&self, vals: impl AsRef<[SafeValue]>) -> SafeValue {
        self.safe.as_ref()(vals.as_ref())
    }
}
