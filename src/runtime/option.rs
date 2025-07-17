/// Option-like type for with a C-representation
///
/// This type cannot make use of niches because it uses the C-representation.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum RotoOption<T> {
    // WARNING: Roto relies on the order of these variants.
    Some(T),
    None,
}

impl<T> From<Option<T>> for RotoOption<T> {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(x) => Self::Some(x),
            None => Self::None,
        }
    }
}

impl<T> From<RotoOption<T>> for Option<T> {
    fn from(value: RotoOption<T>) -> Self {
        match value {
            RotoOption::Some(x) => Some(x),
            RotoOption::None => None,
        }
    }
}
