//! Implementation of Roto's lists
//!
//! Lists are complicated in Roto because they are generic. That means that the Rust code that implements
//! needs to operate on any element type.
//!
//! We have the following goals with this implementation:
//!
//!  - The list data structure should primarily be implemented in Rust instead of having special codegen for it,
//!    to make it easier to review and audit.
//!  - The list should be as simple as possible to pass from and to Rust.
//!  - We attempt to mirror the implementation of Rust's `Vec`.
//!  - Like any Roto type, it should be cheap to clone.
//!
//! We achieve this by implementing it as a type-erased list in Rust. For safety on the Rust side, we provide a
//! wrapper type that contains the type-erased list along with a proper type parameter. The list is also wrapped
//! in a `Arc<Mutex<_>>`
#![deny(clippy::missing_safety_doc)]
#![deny(clippy::undocumented_unsafe_blocks)]

use std::{
    alloc::{Layout, handle_alloc_error},
    ptr::NonNull,
    sync::{Arc, Mutex},
};

use crate::value::{VTable, vtable::DropFn};

/// The functions for list operations that we call from Roto
pub mod ffi {
    use crate::value::RotoOption;

    use super::ErasedList;

    // We use `*mut ()` and `NonNull<()>` to represent `*mut T` and
    // `NonNull<T>`. To make that clear, we alias `()` here so we can use `T`
    // as a fake type parameter.
    type T = ();

    /// Get a pointer to an element from the list
    ///
    /// # Safety
    ///
    ///  - `out` must be an uninitialized value that is properly aligned.
    ///  - `this` must contain a valid `InnerList`. We take ownership of this
    ///    value.
    ///
    pub unsafe fn list_get(
        out: *mut RotoOption<T>,
        this: ErasedList,
        idx: u64,
    ) {
        let idx = idx.try_into().ok();
        match idx.and_then(|idx| this.get(idx)) {
            Some(src) => {
                // We got a pointer into the list, clone it into out at the correct alignment

                // To leave this value in a valid state even if a panic happens
                // in the clone_fn we first write the discriminant of `None` and
                // only replace that with the discriminant of `Some` if the
                // clone has succeeded.

                // SAFETY: 1 is the discriminant of `None`. We asserted that
                // `out` must be a valid RotoOption<T>.
                unsafe { out.cast::<u8>().write(1) };

                let raw = this.0.lock().unwrap();
                let size = raw.vtable.size();
                let alignment = raw.vtable.align();
                let offset = 1usize.next_multiple_of(alignment);

                // SAFETY: `out` is an uninitialized properly aligned value of
                // RotoOption<T> so we can write the T at the right alignment
                // by writing it to the next multiple of the alignment. The offset
                // is therefore the correct byte offset.
                let dst = unsafe { out.byte_add(offset) };

                // If there is no clone function, we can optimize this by doing a memcpy.
                match raw.vtable.clone_fn {
                    Some(clone_fn) => {
                        // SAFETY: dst is correct per the explanation above. src
                        // is correct because we got it from ErasedList::get.
                        unsafe { (clone_fn)(dst.cast::<()>(), src.as_ptr()) }
                    }
                    None => {
                        // SAFETY: dst is correct per the explanation above.
                        // src is correct because we got it from ErasedList::get. The size
                        // we have is in bytes, hence we have to cast the pointers to u8
                        // for the correct size.
                        unsafe {
                            std::ptr::copy_nonoverlapping(
                                src.cast::<u8>().as_ptr(),
                                dst.cast::<u8>(),
                                size,
                            )
                        };
                    }
                }

                // SAFETY: 0 is the discriminant of `Some`. We asserted that
                // `out` must be a valid RotoOption<T>.
                unsafe { out.cast::<u8>().write(0) };
            }
            None => {
                // write None to out

                // SAFETY: 1 is the discriminant of `None`. We asserted that
                // `out` must be a valid RotoOption<T>.
                unsafe { out.cast::<u8>().write(1) };
            }
        }
    }
}

pub mod boundary {
    use std::{
        alloc::Layout, marker::PhantomData, mem::ManuallyDrop, ptr::NonNull,
        sync::Arc,
    };

    use crate::{
        Value,
        runtime::{extern_clone, extern_drop},
        value::{
            VTable,
            vtable::{CloneFn, DropFn},
        },
    };

    use super::ErasedList;

    /// A Roto list
    ///
    /// This is conceptually similar to a `Arc<Mutex<Vec<T>>>`, that is, a shared
    /// growable array. Note that, in contrast with `Vec`, this type can be modified
    /// with only a shared immutable reference, locking access to the list at each
    /// operation.
    ///
    /// This type is covariant over the type parameter `T`.
    #[repr(transparent)]
    pub struct List<T: Value> {
        inner: ErasedList,

        // This field enforces covariance over the type parameter `T`, since
        // `PhantomData<T>` is covariant over `T`.
        _phantom: PhantomData<T>,
    }

    impl<T: Value> Clone for List<T> {
        fn clone(&self) -> Self {
            Self {
                inner: self.inner.clone(),
                _phantom: self._phantom,
            }
        }
    }

    impl<T: Value> PartialEq for List<T>
    where
        T::Transformed: PartialEq,
    {
        fn eq(&self, other: &Self) -> bool {
            // We could have reused the ErasedList implementation of PartialEq,
            // however, this implementation uses more of Rust's knowledge of
            // the type and probably allows for much more optimization.

            // This is both an optimization and necessary because we cannot lock
            // the same mutex twice.
            if Arc::ptr_eq(&self.inner.0, &other.inner.0) {
                return true;
            }

            let this = self.inner.0.lock().unwrap();

            // SAFETY: The rawlist represents a slice of T::Transformed so
            // we can safely construct a slice from it's parts as long as we
            // hold the lock.
            let this = unsafe {
                std::slice::from_raw_parts::<T::Transformed>(
                    this.ptr.cast().as_ptr(),
                    this.len,
                )
            };

            let other = self.inner.0.lock().unwrap();

            // SAFETY: The rawlist represents a slice of T::Transformed so
            // we can safely construct a slice from it's parts as long as we
            // hold the lock.
            let other = unsafe {
                std::slice::from_raw_parts::<T::Transformed>(
                    other.ptr.cast().as_ptr(),
                    other.len,
                )
            };

            this == other
        }
    }

    impl<T: Value> List<T> {
        /// Create a new empty [`List`]
        pub fn new() -> Self {
            let drop_fn: Option<DropFn> =
                std::mem::needs_drop::<T::Transformed>()
                    .then_some(extern_drop::<T::Transformed> as DropFn);

            let clone_fn: Option<CloneFn> =
                Some(extern_clone::<T::Transformed>);

            let layout = Layout::new::<T::Transformed>();
            Self {
                inner: ErasedList::new(VTable::new(
                    layout.size(),
                    layout.align(),
                    clone_fn,
                    drop_fn,
                )),
                _phantom: PhantomData,
            }
        }

        /// Push a new element to this [`List`]
        pub fn push(&self, elem: T) {
            let elem = T::transform(elem);

            // Don't drop the element because we move it into the list
            let mut elem = ManuallyDrop::new(elem);

            let elem_ptr = NonNull::from_mut(&mut elem).cast::<()>();

            // SAFETY: We have a valid value behind the pointer and forget
            // the value to ensure that we give ownership.
            unsafe { self.inner.push(elem_ptr) };
        }

        /// Get the element at index `idx`
        pub fn get(&self, idx: usize) -> Option<T> {
            let ptr = self.inner.get(idx)?;

            // SAFETY: The list has values of T::Transformed, which means that
            // this cast is valid.
            let transformed =
                unsafe { ptr.cast::<T::Transformed>().as_ref() };

            Some(T::untransform(transformed.clone()))
        }

        /// Concatenate two lists, returning the result.
        ///
        /// The `self` and `other` lists will be not be modified.
        pub fn concat(&self, other: &Self) -> Self {
            // SAFETY: other has the same type as self and therefore the
            // element types of the list match up.
            let inner = unsafe { self.inner.concat(&other.inner) };
            Self {
                inner,
                _phantom: PhantomData,
            }
        }

        /// Swap the elements at index `i` and `j`
        pub fn swap(&self, i: usize, j: usize) {
            self.inner.swap(i, j)
        }

        /// Return the length of this `List`
        pub fn len(&self) -> usize {
            self.inner.len()
        }

        /// Return the capacity of this `List`
        pub fn capacity(&self) -> usize {
            self.inner.capacity()
        }

        /// Return `true` if this `List` has a length of `0` and `false` otherwise
        pub fn is_empty(&self) -> bool {
            self.inner.is_empty()
        }
    }

    impl<T: Value> Default for List<T> {
        fn default() -> Self {
            Self::new()
        }
    }

    impl<T: Clone + Value> List<T> {
        /// Convert this [`List`] into a regular [`Vec`].
        pub fn to_vec(&self) -> Vec<T> {
            let guard = self.inner.0.lock().unwrap();

            // SAFETY: The RawList always contains a valid slice. Even if the
            // RawList has a capacity of 0, the pointer is still non-null and
            // aligned, as required by `from_raw_parts`.
            let slice = unsafe {
                std::slice::from_raw_parts(
                    guard.ptr.cast::<T::Transformed>().as_ptr(),
                    guard.len,
                )
            };

            slice
                .iter()
                .map(|elem| T::untransform(elem.clone()))
                .collect()
        }
    }
}

// We use `*mut ()` and `NonNull<()>` to represent `*mut T` and
// `NonNull<T>`. To make that clear, we alias `u8` here so we can use `T`
// as a fake type parameter.
type T = ();

#[derive(Clone)]
pub(crate) struct ErasedList(Arc<Mutex<RawList>>);

impl PartialEq for ErasedList {
    fn eq(&self, other: &Self) -> bool {
        // This is both an optimization and necessary because we cannot lock
        // the same mutex twice.
        if Arc::ptr_eq(&self.0, &other.0) {
            return true;
        }

        let this = self.0.lock().unwrap();
        let other = other.0.lock().unwrap();

        if this.len != other.len {
            return false;
        }

        for i in 0..self.len() {
            let _elem1 = this.get(i).unwrap();
            let _elem2 = other.get(i).unwrap();

            let is_eq = false; // TODO

            if !is_eq {
                return false;
            }
        }

        true
    }
}

impl ErasedList {
    pub fn new(vtable: VTable) -> Self {
        Self(Arc::new(Mutex::new(RawList::new(vtable))))
    }

    /// Push a value to this list
    ///
    /// # Safety
    ///
    ///  - `elem_ptr` must be a pointer to the element type `T` that the list
    ///    contains. This function takes ownership of this value.
    ///
    pub unsafe fn push(&self, elem_ptr: NonNull<T>) {
        // SAFETY: We require that `elem_ptr` must be a pointer to the element
        // type `T` that the list contains.
        unsafe { self.0.lock().unwrap().push(elem_ptr) };
    }

    /// Concatenate this list with another list returning the result
    ///
    /// # Safety
    ///
    /// Both `self` and `other` must have the same element type.
    ///
    pub unsafe fn concat(&self, other: &Self) -> Self {
        let a = self.0.lock().unwrap();

        let new = Self::new(a.vtable.clone());
        let mut raw = new.0.lock().unwrap();

        // SAFETY: self and other have the same element type
        unsafe { raw.extend(&a) };

        // This drop is important in the case that self == other
        // We need to ensure we don't lock the mutex twice
        drop(a);

        let b = other.0.lock().unwrap();

        // SAFETY: raw and b have the same element type
        unsafe { raw.extend(&b) };

        drop(b);

        drop(raw);

        new
    }

    pub fn get(&self, idx: usize) -> Option<NonNull<T>> {
        self.0.lock().unwrap().get(idx)
    }

    pub fn swap(&self, i: usize, j: usize) {
        self.0.lock().unwrap().swap(i, j)
    }

    pub fn len(&self) -> usize {
        self.0.lock().unwrap().len()
    }

    pub fn capacity(&self) -> usize {
        self.0.lock().unwrap().capacity()
    }

    pub fn is_empty(&self) -> bool {
        self.0.lock().unwrap().is_empty()
    }
}

struct RawList {
    /// Pointer to the current allocation
    ///
    /// If the element type is zero-sized this will be a dangling pointer
    /// and no allocation will happen.
    ptr: NonNull<T>,

    /// Length of the list
    len: usize,

    /// Capacity of this current allocation
    ///
    /// If the element type is zero-sized, this will be usize::MAX.
    capacity: usize,

    /// V-Table of the element type
    vtable: VTable,
}

// SAFETY: The RawList is only not Send because of the pointer, but that simply
// represents a box and is safe to Send.
unsafe impl Send for RawList {}

// SAFETY: RawList does not have interior mutability
unsafe impl Sync for RawList {}

impl Drop for RawList {
    fn drop(&mut self) {
        match self.vtable.drop_fn {
            Some(drop_fn) => {
                // SAFETY: By construction of the RawList, we have a valid drop_fn
                unsafe { self.drop_elements_with(drop_fn) };
            }
            None => {
                self.forget_elements();
            }
        }

        // SAFETY: We are the only one to dealloc
        unsafe { self.dealloc() };
    }
}

impl RawList {
    fn new(vtable: VTable) -> Self {
        Self::with_capacity(0, vtable)
    }

    fn forget_elements(&mut self) {
        self.len = 0;
    }

    /// Drop all the elements in the RawList with a given drop function.
    ///
    /// This should only be called from `RawList::drop`.
    ///
    /// # Safety
    ///
    ///  - The `drop_fn` must contain a function that takes value of the element
    ///    type `T`.
    ///
    unsafe fn drop_elements_with(&mut self, drop_fn: DropFn) {
        let len = self.len;

        // Set self.len to 0 so that if this function panics, we still consider
        // all the elements dropped.
        self.len = 0;

        for i in 0..len {
            let offset = self.offset_of(i);

            // SAFETY: We stay within the allocation because we stay within the
            // length and therefore within the capacity of the list.
            let ptr = unsafe { self.ptr.byte_add(offset).as_ptr() };

            // SAFETY: We give drop_fn the pointer of the value to drop as
            // argument.
            unsafe { (drop_fn)(ptr) }
        }
    }

    fn with_capacity(capacity: usize, vtable: VTable) -> Self {
        // Create a dangling non-null pointer with the right alignment
        // SAFETY: Align cannot be zero, so a pointer constructed with it won't be null.
        let ptr = unsafe {
            NonNull::new_unchecked(std::ptr::without_provenance_mut(
                vtable.align(),
            ))
        };

        // Zero-sized types need some special treatment, because they don't
        // require any allocations. The capacity is therefore always
        // usize::MAX.
        if vtable.size() == 0 {
            return Self {
                ptr,
                len: 0,
                capacity: usize::MAX,
                vtable,
            };
        }

        let mut this = Self {
            ptr,
            len: 0,
            capacity: 0,
            vtable,
        };

        this.reserve(capacity);

        this
    }

    fn current_memory(&self) -> Option<NonNull<T>> {
        if self.vtable.size() == 0 || self.capacity == 0 {
            None
        } else {
            Some(self.ptr)
        }
    }

    /// # Safety
    ///
    /// - This function takes ownership of the element, it should not be used afterwards
    ///
    unsafe fn push(&mut self, elem_ptr: NonNull<T>) {
        if self.vtable.size() > 0 {
            self.reserve(1);
            let offset = self.offset_of(self.len);

            // The pointers are cast to *mut u8, to ensure that
            // copy_nonoverlapping will copy a number of bytes.
            let src = elem_ptr.cast::<u8>().as_ptr();
            let dst = self.ptr.cast::<u8>().as_ptr();

            // SAFETY:
            //  - We stay within the allocation since we reserved the space
            //    for this element.
            let dst = unsafe { dst.byte_add(offset) };

            let size = self.vtable.size();

            // SAFETY:
            //  - The pointers have types of *mut u8 and *const u8 so we are
            //    copying a number of bytes.
            //  - `src` is valid for reads of `size` bytes since it points to a
            //    valid `T`.
            //  - `dst` is valid for writes of `size` bytes since we allocated
            //    the space for it.
            unsafe { std::ptr::copy_nonoverlapping(src, dst, size) };
        }
        self.len += 1;
    }

    /// Extend this RawList with the contents of another
    ///
    /// # Safety
    ///
    /// Both `self` and `other` must have the same element type.
    ///
    unsafe fn extend(&mut self, other: &Self) {
        /// Drops a slice of a list to ensure we don't leak them on a panic
        struct DropGuard {
            ptr: NonNull<()>,
            vtable: VTable,
            offset: usize,
            len: usize,
        }

        impl Drop for DropGuard {
            fn drop(&mut self) {
                let Some(drop_fn) = self.vtable.drop_fn else {
                    return;
                };

                for i in 0..self.len {
                    let offset = self.vtable.size() * (self.offset + i);

                    // SAFETY: We only access addresses we've already written to
                    let src = unsafe { self.ptr.byte_add(offset).as_ptr() };

                    // SAFETY: We drop we've just written to
                    unsafe { (drop_fn)(src) }
                }
            }
        }

        if self.vtable.size() == 0 {
            // Even for zero-sized types, we need to call clone if that is
            // present in the vtable.
            if let Some(clone_fn) = self.vtable.clone_fn {
                // The clone_fn might panic in which case we should clean up the elements
                // that we have cloned. We do this by creating a DropGuard that keeps track
                // of how many elements have been written. At the end of the loop, we forget
                // the guard so that none of the elements get dropped.
                let mut drop_guard = DropGuard {
                    ptr: self.ptr,
                    vtable: self.vtable.clone(),
                    offset: self.len,
                    len: 0,
                };
                for _ in 0..other.len {
                    // SAFETY: The ptr field is always aligned
                    unsafe {
                        (clone_fn)(self.ptr.as_ptr(), other.ptr.as_ptr())
                    };
                    drop_guard.len += 1;
                }
                std::mem::forget(drop_guard);
            }
            self.len += other.len;
            return;
        }

        // Small optimization for when other is empty
        if other.len == 0 {
            return;
        }

        self.reserve(other.len);

        if let Some(clone_fn) = self.vtable.clone_fn {
            // The clone_fn might panic in which case we should clean up the elements
            // that we have cloned. We do this by creating a DropGuard that keeps track
            // of how many elements have been written. At the end of the loop, we forget
            // the guard so that none of the elements get dropped.
            let mut drop_guard = DropGuard {
                ptr: self.ptr,
                vtable: self.vtable.clone(),
                offset: self.len,
                len: 0,
            };
            for i in 0..other.len {
                let src_offset = other.offset_of(i);

                // SAFETY: We stay within the same allocation because we access
                // an element at an index smaller than the capacity.
                let src = unsafe { other.ptr.byte_add(src_offset) };

                let dst_offset = self.offset_of(self.len + i);

                // SAFETY: We stay within the same allocation because we access
                // an element at an index smaller than the new capacity because
                // we reserved space for the number of elements of other.
                let dst = unsafe { self.ptr.byte_add(dst_offset) };

                // SAFETY: We got the offset of dst with offset_of, so it's aligned.
                // The src is valid because we stay within the length of other
                // and get the offset with offset_of.
                unsafe { (clone_fn)(dst.as_ptr(), src.as_ptr()) }

                drop_guard.len += 1;
            }
            std::mem::forget(drop_guard);
        } else {
            let src = other.ptr;

            let dst_offset = self.offset_of(self.len);

            // SAFETY: We stay within the same allocation since we reserved additional
            // space and the end of the list is therefore within the allocation.
            let dst = unsafe { self.ptr.byte_add(dst_offset) };

            let size = other.offset_of(other.len);

            // SAFETY:
            //  - `src` is valid for `size` reads since that's the length of list
            //  - `dst` is valid for `size` writes since we reserved the needed
            //    additional space.
            //  - Both are properly aligned since all elements are aligned.
            //  - They also don't overlap due to Rust's aliasing rules.
            unsafe {
                std::ptr::copy_nonoverlapping(
                    src.cast::<u8>().as_ptr(),
                    dst.cast::<u8>().as_ptr(),
                    size,
                )
            };
        }

        self.len += other.len;
    }

    fn reserve(&mut self, added: usize) {
        if self.vtable.size() == 0 {
            return;
        }

        // The unwrap is intentional here. There's not much we can do except
        // panic when this overflows.
        let new_required = self.len.checked_add(added).unwrap();
        let new_capacity = compute_capacity(self.vtable.size(), new_required);

        if new_capacity > self.capacity {
            if let Some(ptr) = self.current_memory() {
                // SAFETY: At this point, we know that:
                //
                //  - The size of the element layout is non-zero.
                //  - The previous capacity was non-zero since `current_memory`
                //    returned `Some`.
                //  - The new capacity is non-zero since it exceed the old
                //    capacity.
                let new_ptr = unsafe {
                    realloc_array(
                        ptr,
                        self.vtable.layout(),
                        self.capacity,
                        new_capacity,
                    )
                };
                self.ptr = new_ptr;
            } else {
                // SAFETY: At this point, we know that the size of the layout
                // is not zero and we that the `new_capacity` is not
                // zero since it must be greater than `self.capacity` which
                // cannot be negative.
                let new_ptr = unsafe {
                    alloc_array(self.vtable.layout(), new_capacity)
                };
                self.ptr = new_ptr;
            }
            self.capacity = new_capacity;
        }
    }

    fn get(&self, idx: usize) -> Option<NonNull<T>> {
        if idx >= self.len {
            return None;
        }
        let offset = self.offset_of(idx);

        // SAFETY: Since we know that idx < len < capacity and capacity is the
        // size of the allocation, we know that we'll stay within the
        // allocation.
        let ptr = unsafe { self.ptr.byte_add(offset) };

        Some(ptr)
    }

    fn swap(&self, i: usize, j: usize) {
        if i >= self.len || j >= self.len {
            return;
        }

        if i == j {
            return;
        }

        let i = self.offset_of(i);
        let j = self.offset_of(j);

        // SAFETY: Since we know that idx < len < capacity and capacity is the
        // size of the allocation, we know that we'll stay within the
        // allocation.
        let ptr_i = unsafe { self.ptr.byte_add(i) };

        // SAFETY: Since we know that idx < len < capacity and capacity is the
        // size of the allocation, we know that we'll stay within the
        // allocation.
        let ptr_j = unsafe { self.ptr.byte_add(j) };

        // SAFETY: The pointers are elements in the list and therefore aligned
        // and initialized. We also know they don't overlap because we checked
        // for that above.
        unsafe {
            std::ptr::swap_nonoverlapping(
                ptr_i.cast::<u8>().as_ptr(),
                ptr_j.cast::<u8>().as_ptr(),
                self.vtable.size(),
            )
        };
    }

    /// Deallocate this array
    ///
    /// # Safety
    ///
    /// This should only be called once.
    unsafe fn dealloc(&mut self) {
        // If the size of the elements is 0 we never allocated anything
        if self.vtable.size() == 0 {
            return;
        }

        if let Some(ptr) = self.current_memory() {
            // SAFETY: We allocated the ptr with alloc_array or realloc_array.
            unsafe {
                dealloc_array(ptr, self.vtable.layout(), self.capacity)
            };
        }
    }

    fn offset_of(&self, n: usize) -> usize {
        self.vtable.size() * n
    }

    fn is_empty(&self) -> bool {
        self.len == 0
    }

    fn len(&self) -> usize {
        self.len
    }

    fn capacity(&self) -> usize {
        self.capacity
    }
}

fn compute_capacity(size: usize, required: usize) -> usize {
    if required == 0 {
        return 0;
    }

    // This is copied from std's Vec
    //
    // The idea is that doing multiple allocations for small vecs is not worth it
    // compared to just using a bit more space.
    let minimum_capacity = if size == 1 {
        8
    } else if size <= 1024 {
        4
    } else {
        1
    };

    // The unwrap is intentional here. There's not much we can do except
    // panic when this overflows.
    let next_power_of_two = required.checked_next_power_of_two().unwrap();
    Ord::max(next_power_of_two, minimum_capacity)
}

/// # Safety
///
/// See [`GlobalAllocator::Alloc`]
///
/// - `elem_layout` must have a non-zero size.
/// - `n` must be greater than zero.
unsafe fn alloc_array(elem_layout: Layout, n: usize) -> NonNull<T> {
    debug_assert!(elem_layout.size() > 0);
    debug_assert!(n > 0);

    let layout = array_layout(elem_layout, n);

    // SAFETY: We ensure that the layout has a non-zero size.
    let ptr = unsafe { std::alloc::alloc(layout) };
    let ptr = ptr.cast::<T>();

    let Some(ptr) = NonNull::new(ptr) else {
        handle_alloc_error(layout);
    };

    ptr
}

/// # Safety
///
/// See [`GlobalAllocator::Alloc`]
///
/// - `ptr` must have been allocated with `elem_layout` and `old_n`.
/// - `element_layout` must have a non-zero size.
/// - `old_n` must be greater than zero.
/// - `new_n` must be greater than zero.
///
unsafe fn realloc_array(
    ptr: NonNull<T>,
    elem_layout: Layout,
    old_n: usize,
    new_n: usize,
) -> NonNull<T> {
    debug_assert!(elem_layout.size() > 0);
    debug_assert!(old_n > 0);
    debug_assert!(new_n > 0);

    let old_layout = array_layout(elem_layout, old_n);
    let new_layout = array_layout(elem_layout, new_n);

    // SAFETY: We ensure that the layout has a non-zero size and require that
    // the pointer was allocated with alloc.
    let ptr = unsafe {
        std::alloc::realloc(
            ptr.cast::<u8>().as_ptr(),
            old_layout,
            new_layout.size(),
        )
    };

    let ptr = ptr.cast::<T>();

    let Some(ptr) = NonNull::new(ptr) else {
        handle_alloc_error(new_layout);
    };

    ptr
}

/// # Safety
///
/// See [`GlobalAllocator::alloc`]
///
/// - `ptr` must have been allocated by `alloc_array` with the same arguments.
unsafe fn dealloc_array(ptr: NonNull<T>, elem_layout: Layout, n: usize) {
    debug_assert!(elem_layout.size() > 0);
    debug_assert!(n > 0);

    let layout = array_layout(elem_layout, n);

    // SAFETY: We require that `ptr` has been allocated by `alloc_array` with
    // the same arguments.
    unsafe { std::alloc::dealloc(ptr.cast::<u8>().as_ptr(), layout) };
}

/// Compute the array layout for given element layout and array length
fn array_layout(elem_layout: Layout, n: usize) -> Layout {
    debug_assert!(elem_layout.size() > 0);
    debug_assert!(n > 0);

    let element_size = elem_layout.size();
    let align = elem_layout.align();

    let array_size = element_size * n;

    Layout::from_size_align(array_size, align).unwrap()
}

#[cfg(test)]
mod tests {
    use std::sync::{Arc, atomic::AtomicUsize};

    use crate::Val;

    use super::boundary::List;

    #[test]
    fn empty_list() {
        let l = List::<u64>::new();
        assert_eq!(l.to_vec(), Vec::new());
    }

    #[test]
    fn push_and_get() {
        let l = List::<u64>::new();
        l.push(10);
        l.push(20);
        l.push(30);

        assert_eq!(Some(10), l.get(0));
        assert_eq!(Some(20), l.get(1));
        assert_eq!(Some(30), l.get(2));
        assert_eq!(None, l.get(3));
    }

    #[test]
    fn list_of_strings() {
        // Create a list of string to and concat it a few times, which
        // should exercise the clone and drop implementation (especially
        // under valgrind).
        let l = List::<Arc<str>>::new();
        l.push("hello".into());
        l.push("world".into());

        let l = l.concat(&l);
        let l = l.concat(&l);

        assert_eq!(l.len(), 8);

        assert_eq!(
            l.to_vec(),
            vec![
                "hello".into(),
                "world".into(),
                "hello".into(),
                "world".into(),
                "hello".into(),
                "world".into(),
                "hello".into(),
                "world".into(),
            ]
        );
    }

    #[test]
    fn zero_sized_refcount() {
        use std::sync::atomic::Ordering::Relaxed;

        static COUNT: AtomicUsize = AtomicUsize::new(1);

        struct Counter;

        impl Clone for Counter {
            fn clone(&self) -> Self {
                COUNT.fetch_add(1, Relaxed);
                Self
            }
        }

        impl PartialEq for Counter {
            fn eq(&self, _other: &Self) -> bool {
                true
            }
        }

        impl Drop for Counter {
            fn drop(&mut self) {
                COUNT.fetch_sub(1, Relaxed);
            }
        }

        let counter = Counter;

        // This test is made for zero-sized types, so we do a sanity check that
        // we actually are dealing with a zero-sized type.
        assert_eq!(std::mem::size_of::<Val<Counter>>(), 0);

        let list_one = List::<Val<Counter>>::new();
        list_one.push(Val(counter.clone()));

        assert_eq!(COUNT.load(Relaxed), 2);

        let list_two = list_one.concat(&list_one);

        assert_eq!(COUNT.load(Relaxed), 4);

        let list_three = list_two.concat(&list_two);

        assert_eq!(COUNT.load(Relaxed), 8);

        drop(list_one);

        assert_eq!(COUNT.load(Relaxed), 7);

        drop(list_two);

        assert_eq!(COUNT.load(Relaxed), 5);

        drop(list_three);

        assert_eq!(COUNT.load(Relaxed), 1);

        drop(counter);
    }

    #[test]
    fn swap() {
        let list = List::<i32>::new();

        list.push(1);
        list.push(2);
        list.push(3);
        list.push(4);

        list.swap(1, 2);
        list.swap(0, 3);

        assert_eq!(list.to_vec(), vec![4, 3, 2, 1])
    }
}
