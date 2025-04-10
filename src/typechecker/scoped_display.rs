use core::fmt;

use super::scope::ScopeGraph;

pub trait ScopedDisplay: Sized {
    fn fmt(
        &self,
        graph: &ScopeGraph,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result;

    fn display<'a>(
        &'a self,
        graph: &'a ScopeGraph,
    ) -> impl fmt::Display + 'a {
        ScopedPrinter { graph, inner: self }
    }
}

impl<T: fmt::Display> ScopedDisplay for T {
    fn fmt(
        &self,
        _graph: &ScopeGraph,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

struct ScopedPrinter<'a, T: ScopedDisplay> {
    graph: &'a ScopeGraph,
    inner: &'a T,
}

impl<T: ScopedDisplay> fmt::Display for ScopedPrinter<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(self.graph, f)
    }
}
