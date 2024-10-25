#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Val<T>(pub T);

impl<T> From<T> for Val<T> {
    fn from(value: T) -> Self {
        Self(value)
    }
}
