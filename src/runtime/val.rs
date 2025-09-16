use std::ops::{Deref, DerefMut};

/// A wrapper type that makes a registered type safe to pass to and from Roto.
///
/// Only types that implement the sealed [`Reflect`] trait can be passed to
/// Roto. Some primitive types implement this trait directly, but every other
/// type needs to be wrapped in [`Val`]. You should not wrap the primitives in
/// [`Val`].
///
/// This type implements [`Deref`] and [`DerefMut`] to allow for easy access to
/// the inner value.
///
/// ```rust,ignore
/// struct Foo {
///     a: i32,
///     b: i32,
/// }
///
/// runtime.register_type::<Foo>("..").unwrap();
///
/// // compile a script...
///
/// // Pass Foo to Roto wrapped in Val:
/// let f = pkg.get_function::<(), fn(Val<Foo>) -> ()>("main").unwrap();
/// f.call(&mut (), Val(Foo { a: 1, b: 2 }));
///
/// // For Option and Verdict, wrap the inner type in Val
/// pkg.get_function::<(), fn(Option<Val<Foo>>) -> ()>("main").unwrap();
///
/// // Do not wrap types that already implement Reflect.
/// pkg.get_function::<(), fn(i32) -> ()>("main").unwrap();
/// ```
///
/// [`Reflect`]: super::Reflect
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Val<T>(pub T);

impl<T> From<T> for Val<T> {
    fn from(value: T) -> Self {
        Self(value)
    }
}

impl<T> Deref for Val<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Val<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
