use std::ops::{Deref, DerefMut};

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
