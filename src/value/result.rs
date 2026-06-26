/// Result-like type for with a C-representation
///
/// This type cannot make use of niches because it uses the C-representation.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum RotoResult<T, E> {
    // WARNING: Roto relies on the order of these variants.
    Ok(T),
    Err(E),
}

impl<T, E> From<Result<T, E>> for RotoResult<T, E> {
    fn from(value: Result<T, E>) -> Self {
        match value {
            Ok(x) => Self::Ok(x),
            Err(x) => Self::Err(x),
        }
    }
}

impl<T, E> From<RotoResult<T, E>> for Result<T, E> {
    fn from(value: RotoResult<T, E>) -> Self {
        match value {
            RotoResult::Ok(x) => Ok(x),
            RotoResult::Err(x) => Err(x),
        }
    }
}
