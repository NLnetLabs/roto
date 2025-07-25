//! Defines the [`Verdict`] type

/// A `Verdict` is the output of a filtermap
///
/// It is functionally equivalent to a [`Result`], but it has `repr(u8)` to
/// keep the representation synchronized with Roto.
///
/// The [`Verdict::into_result`] and [`Verdict::into_option`] methods are
/// available to map a [`Verdict`] to more conventional types.
#[repr(u8)]
#[derive(Clone, Debug, PartialEq, Eq)]
#[must_use]
pub enum Verdict<A, R> {
    // WARNING: Roto relies on the order of these variants
    Accept(A),
    Reject(R),
}

impl<A, R> Verdict<A, R> {
    pub fn into_result(self) -> Result<A, R> {
        match self {
            Verdict::Accept(x) => Ok(x),
            Verdict::Reject(x) => Err(x),
        }
    }
}

impl<A> Verdict<A, ()> {
    pub fn into_option(self) -> Option<A> {
        match self {
            Self::Accept(x) => Some(x),
            Self::Reject(()) => None,
        }
    }
}
