use std::ptr::NonNull;

type T = ();

/// A Roto clone function
pub type CloneFn = unsafe extern "C" fn(*mut T, *const T);

/// A Roto drop function
pub type DropFn = unsafe extern "C" fn(NonNull<T>);

#[derive(Clone, Debug)]
#[repr(C)]
pub struct VTable {
    size: usize,
    align: usize,

    /// Copies the value behind its second argument to the first
    ///
    /// It should only be `None` for `Copy` types.
    pub clone_fn: Option<CloneFn>,

    /// Drops the value behind its argument in place
    ///
    /// It should only be `None` for types that can be trivially dropped.
    pub drop_fn: Option<DropFn>,
}

impl VTable {
    pub fn new(
        size: usize,
        align: usize,
        clone_fn: Option<CloneFn>,
        drop_fn: Option<DropFn>,
    ) -> Self {
        Self {
            size,
            align,
            clone_fn,
            drop_fn,
        }
    }

    pub fn layout(&self) -> std::alloc::Layout {
        std::alloc::Layout::from_size_align(self.size, self.align).unwrap()
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn align(&self) -> usize {
        self.align
    }
}
