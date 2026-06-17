use std::net::IpAddr;

use inetnum::{addr::Prefix, asn::Asn};
use sealed::sealed;

use crate::{
    List, RotoString, Val, Value, Verdict,
    value::{ErasedList, StringBytes, StringChars, StringLines},
};

/// The result of executing a script
#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ScriptResult<T> {
    // WARNING: Roto relies on the order of these variants.
    /// The script exited successfully
    Ok(T),

    /// The script panicked with a message.
    Panic(Box<str>),
}

/// Turns a type into a [`ScriptResult`]
///
/// Usually, this just wrap the value in [`ScriptResult::Ok`], but for
/// [`ScriptResult`], this is the identity function.
#[sealed]
pub trait IntoScriptResult {
    /// Type argument of the created [`ScriptResult`].
    type Inner;

    /// Turn the value into a [`ScriptResult`].
    fn into_script_result(self) -> ScriptResult<Self::Inner>;
}

#[sealed]
impl<T> IntoScriptResult for ScriptResult<T> {
    type Inner = T;

    fn into_script_result(self) -> ScriptResult<Self::Inner> {
        self
    }
}

#[sealed]
impl IntoScriptResult for () {
    type Inner = ();

    fn into_script_result(self) -> ScriptResult<Self::Inner> {
        ScriptResult::Ok(self)
    }
}

macro_rules! impl_into_script_result {
    ($t:ident) => {
        #[sealed]
        impl IntoScriptResult for $t {
            type Inner = Self;
            fn into_script_result(self) -> ScriptResult<Self::Inner> {
                ScriptResult::Ok(self)
            }
        }
    };
    ($t:ident<$($a:ident),*>) => {
        #[sealed]
        impl<$($a),*> IntoScriptResult for $t<$($a),*> {
            type Inner = Self;
            fn into_script_result(self) -> ScriptResult<Self::Inner> {
                ScriptResult::Ok(self)
            }
        }
    };
}

impl_into_script_result!(bool);
impl_into_script_result!(u8);
impl_into_script_result!(u16);
impl_into_script_result!(u32);
impl_into_script_result!(u64);
impl_into_script_result!(i8);
impl_into_script_result!(i16);
impl_into_script_result!(i32);
impl_into_script_result!(i64);
impl_into_script_result!(f32);
impl_into_script_result!(f64);
impl_into_script_result!(Asn);
impl_into_script_result!(IpAddr);
impl_into_script_result!(Prefix);
impl_into_script_result!(RotoString);
impl_into_script_result!(StringBytes);
impl_into_script_result!(StringChars);
impl_into_script_result!(StringLines);
impl_into_script_result!(ErasedList);
impl_into_script_result!(Verdict<A, R>);
impl_into_script_result!(Option<T>);
impl_into_script_result!(Result<T, E>);
impl_into_script_result!(Val<T>);

#[sealed]
impl<T: Value> IntoScriptResult for List<T> {
    type Inner = Self;
    fn into_script_result(self) -> ScriptResult<Self::Inner> {
        ScriptResult::Ok(self)
    }
}
