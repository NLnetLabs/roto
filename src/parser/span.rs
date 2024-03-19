use std::ops::{Deref, DerefMut, Range};

use miette::SourceSpan;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

impl From<Span> for SourceSpan {
    fn from(value: Span) -> Self {
        (value.start..value.end).into()
    }
}

#[derive(Clone, Debug)]
pub struct Spanned<T> {
    pub inner: T,
    pub span: Span,
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}

impl<T: AsRef<U>, U: ?Sized> AsRef<U> for Spanned<T> {
    fn as_ref(&self) -> &U {
        self.inner.as_ref()
    }
}

pub trait WithSpan: Sized {
    fn with_span(self, span: impl Into<Span>) -> Spanned<Self> {
        Spanned {
            inner: self,
            span: span.into(),
        }
    }
}

impl<T> WithSpan for T {}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}
