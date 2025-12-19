use std::{alloc::Layout, ptr::NonNull};

type T = ();

/// A Roto clone function
pub type CloneFn = unsafe extern "C" fn(*mut T, *const T);

/// A Roto drop function
pub type DropFn = unsafe extern "C" fn(NonNull<T>);

#[derive(Clone, Debug)]
#[repr(C)]
pub struct VTable {
    pub layout: Layout,
    pub clone_fn: Option<CloneFn>,
    pub drop_fn: Option<DropFn>,
}
