/// Option-like type for with a C-representation
///
/// This type cannot make use of niches because it uses the C-representation.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Optional<T> {
    // WARNING: Roto relies on the order of these variants.
    Some(T),
    None,
}

impl<T> From<Option<T>> for Optional<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(x) => Self::Some(x),
            None => Self::None,
        }
    }
}

impl<T> From<Optional<T>> for Option<T> {
    fn from(value: Optional<T>) -> Self {
        match value {
            Optional::Some(x) => Some(x),
            Optional::None => None,
        }
    }
}
