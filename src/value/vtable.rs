use std::{alloc::Layout, ptr::NonNull};

type T = ();
pub type CtxPtr = *mut ();
pub type CloneFn = unsafe extern "C" fn(*mut T, CtxPtr, *const T);
pub type DropFn = unsafe extern "C" fn(CtxPtr, NonNull<T>);

#[derive(Clone, Debug)]
#[repr(C)]
pub struct VTable {
    pub layout: Layout,
    pub clone_fn: Option<CloneFn>,
    pub drop_fn: Option<DropFn>,
}
