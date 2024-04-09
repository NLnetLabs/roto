use std::{
    borrow::Borrow,
    fmt::Display,
    ops::{Deref, DerefMut, Range},
};

#[derive(Debug, Default)]
pub struct Spans(Vec<Span>);

impl Spans {
    pub fn add<T>(&mut self, span: Span, x: T) -> Meta<T> {
        let id = MetaId(self.0.len());
        self.0.push(span);
        Meta { node: x, id }
    }

    pub fn get(&self, x: impl Into<MetaId>) -> Span {
        self.0[x.into().0]
    }

    pub fn merge(&mut self, x: impl Into<MetaId>, y: impl Into<MetaId>) -> Span {
        self.get(x).merge(self.get(y))
    }
}

impl<T> From<&Meta<T>> for MetaId {
    fn from(value: &Meta<T>) -> Self {
        value.id
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub file: usize,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn merge(self, other: Self) -> Self {
        assert_eq!(self.file, other.file);
        Self {
            file: self.file,
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    pub fn new(file: usize, value: Range<usize>) -> Self {
        Self {
            file,
            start: value.start,
            end: value.end,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct MetaId(pub usize);

#[derive(Clone, Debug)]
pub struct Meta<T> {
    pub node: T,
    pub id: MetaId,
}

impl<T: AsRef<U>, U: ?Sized> AsRef<U> for Meta<T> {
    fn as_ref(&self) -> &U {
        self.node.as_ref()
    }
}

impl<T: Borrow<str>> Borrow<str> for Meta<T> {
    fn borrow(&self) -> &str {
        self.node.borrow()
    }
}

impl<T: Display> Display for Meta<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.node.fmt(f)
    }
}

impl<T> Deref for Meta<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

impl<T> DerefMut for Meta<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.node
    }
}
