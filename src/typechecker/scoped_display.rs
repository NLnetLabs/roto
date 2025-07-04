use core::fmt;

use super::info::TypeInfo;

pub trait TypeDisplay: Sized {
    fn fmt(
        &self,
        type_info: &TypeInfo,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result;

    fn display<'a>(
        &'a self,
        type_info: &'a TypeInfo,
    ) -> impl fmt::Display + 'a {
        TypePrinter {
            type_info,
            inner: self,
        }
    }
}

impl<T: fmt::Display> TypeDisplay for T {
    fn fmt(
        &self,
        _type_info: &TypeInfo,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

struct TypePrinter<'a, T: TypeDisplay> {
    type_info: &'a TypeInfo,
    inner: &'a T,
}

impl<T: TypeDisplay> fmt::Display for TypePrinter<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(self.type_info, f)
    }
}
