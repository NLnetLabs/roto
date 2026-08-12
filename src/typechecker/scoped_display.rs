use core::fmt;

use crate::typechecker::scope::ScopeRef;

use super::info::TypeInfo;

pub trait TypeDisplay: Sized {
    fn fmt(
        &self,
        relative_to: ScopeRef,
        type_info: &TypeInfo,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result;

    fn display<'a>(
        &'a self,
        relative_to: ScopeRef,
        type_info: &'a TypeInfo,
    ) -> impl fmt::Display + 'a {
        TypePrinter {
            type_info,
            relative_to,
            inner: self,
        }
    }
}

impl<T: fmt::Display> TypeDisplay for T {
    fn fmt(
        &self,
        _relative_to: ScopeRef,
        _type_info: &TypeInfo,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

struct TypePrinter<'a, T: TypeDisplay> {
    relative_to: ScopeRef,
    type_info: &'a TypeInfo,
    inner: &'a T,
}

impl<T: TypeDisplay> fmt::Display for TypePrinter<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(self.relative_to, self.type_info, f)
    }
}
