pub struct Val<T>(pub T);

impl<T> From<T> for Val<T> {
    fn from(value: T) -> Self {
        Self(value)
    }
}
