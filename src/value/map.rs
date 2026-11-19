//! Implementation of Roto's maps
//!
//! Maps are complicated in Roto because they are generic. That means that the Rust code that implements
//! needs to operate on any element type.
//!
//! We have the following goals with this implementation:
//!
//!  - The map data structure should primarily be implemented in Rust instead of having special codegen for it,
//!    to make it easier to review and audit.
//!  - The map should be as simple as possible to pass from and to Rust.
//!  - We attempt to mirror the implementation of Rust's `HashMap`, but for now we use a simpler implementation
//!    that doesn't do any hashing.
//!  - Like any Roto type, it should be cheap to clone.
//!
//! We achieve this by implementing it as a type-erased map in Rust. For safety on the Rust side, we provide a
//! wrapper type that contains the type-erased map along with a proper type parameters. The map is also wrapped
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
    use std::ptr::NonNull;

    use crate::value::RotoOption;

    use super::ErasedMap;

    // We use `*mut ()` and `NonNull<()>` to represent `*mut T` and
    // `NonNull<T>`. To make that clear, we alias `()` here so we can use `T`
    // as a fake type parameter.
    type K = ();
    type V = ();

    ///
    ///
    /// # Safety
    ///
    ///  - `key_ptr` must be a valid reference to a value of `K`.
    pub unsafe fn map_get(
        out: *mut RotoOption<V>,
        this: ErasedMap,
        key_ptr: NonNull<K>,
    ) {
        let raw = this.0.lock().unwrap();

        // SAFETY: 1 is the discriminant of `None`. We asserted that
        // `out` must be a valid RotoOption<T>.
        unsafe { out.cast::<u8>().write(1) };

        // SAFETY: We require that `key_ptr` is a valid reference to a value
        // of `K`.
        let value_ptr = unsafe { raw.get(key_ptr) };
        match value_ptr {
            None => {
                // Nothing to do
            }
            Some(value_ptr) => {
                let size = raw.value_vtable.size();
                let alignment = raw.value_vtable.align();
                let offset = 1usize.next_multiple_of(alignment);

                // SAFETY: `out` is an uninitialized properly aligned value of
                // RotoOption<T> so we can write the V at the right alignment
                // by writing it to the next multiple of the alignment. The offset
                // is therefore the correct byte offset.
                let dst = unsafe { out.byte_add(offset) };

                // If there is no clone function, we can optimize this by doing a memcpy.
                match raw.value_vtable.clone_fn {
                    Some(clone_fn) => {
                        // SAFETY: dst is correct per the explanation above. src
                        // is correct because we got it from RawMap::get.
                        unsafe {
                            (clone_fn)(dst.cast::<()>(), value_ptr.as_ptr())
                        }
                    }
                    None => {
                        // SAFETY: dst is correct per the explanation above.
                        // value_ptr is correct because we got it from
                        // RawMap::get. The size we have is in bytes, hence
                        // we have to cast the pointers to u8 for the correct
                        // size.
                        unsafe {
                            std::ptr::copy_nonoverlapping(
                                value_ptr.cast::<u8>().as_ptr(),
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
        }

        if let Some(drop_fn) = raw.key_vtable.drop_fn {
            // SAFETY: We are responsible for dropping the key_ptr.
            unsafe { (drop_fn)(key_ptr.as_ptr()) };
        }
    }
}

pub mod boundary {
    use std::{marker::PhantomData, mem::ManuallyDrop, ptr::NonNull};

    use crate::{value::VTable, value::Value};

    use super::ErasedMap;

    /// A Roto map
    ///
    /// This is conceptually similar to a `Arc<Mutex<HashMap<K, V>>>`, that is, a shared
    /// growable map. Note that, in contrast with `HashMap`, this type can be modified
    /// with only a shared immutable reference, locking access to the list at each
    /// operation.
    ///
    /// This type is covariant over the type parameters `K` and `V`.
    #[repr(transparent)]
    pub struct Map<K: Value, V: Value> {
        inner: ErasedMap,

        // This field enforces covariance over the type parameters `K` and `V`, since
        // `PhantomData<(K, V)>` is covariant over `K` and `V`.
        _phantom: PhantomData<(K::Transformed, V::Transformed)>,
    }

    impl<K: Value, V: Value> Clone for Map<K, V> {
        fn clone(&self) -> Self {
            Self {
                inner: self.inner.clone(),
                _phantom: self._phantom,
            }
        }
    }

    impl<K: Value, V: Value> PartialEq for Map<K, V>
    where
        K::Transformed: PartialEq,
        V::Transformed: PartialEq,
    {
        fn eq(&self, other: &Self) -> bool {
            self.inner.eq(&other.inner)
        }
    }

    impl<K: Value, V: Value> Map<K, V>
    where
        K::Transformed: PartialEq,
        V::Transformed: PartialEq,
    {
        /// Create a new empty [`Map`]
        pub fn new() -> Self {
            let key_vtable = VTable::of::<K::Transformed>();
            let value_vtable = VTable::of::<V::Transformed>();

            Self {
                inner: ErasedMap::new(key_vtable, value_vtable),
                _phantom: PhantomData,
            }
        }

        /// Insert a new value into the map.
        ///
        /// If the key is already present in the map, its value is overwritten.
        pub fn insert(&mut self, key: K, value: V) {
            let key = K::transform(key);
            let value = V::transform(value);

            let mut key = ManuallyDrop::new(key);
            let mut value = ManuallyDrop::new(value);

            let key_ptr = NonNull::from_mut(&mut key).cast::<()>();
            let value_ptr = NonNull::from_mut(&mut value).cast::<()>();

            // SAFETY: `key_ptr` and `value_ptr` are valid pointers to K and V
            // respectively. We give ownership over the values because they are
            // wrapped in `ManuallyDrop`.
            unsafe { self.inner.insert(key_ptr, value_ptr) }
        }

        /// Get the value associated with a key.
        pub fn get(&self, key: K) -> Option<V> {
            let mut key = K::transform(key);

            let key_ptr = NonNull::from_mut(&mut key).cast::<()>();

            let value_ptr = self.inner.get(key_ptr)?;

            // SAFETY: The list has values of T::Transformed, which means that
            // this cast is valid.
            let value_transformed =
                unsafe { value_ptr.cast::<V::Transformed>().as_ref() };

            Some(V::untransform(value_transformed.clone()))
        }

        /// Returns whether a key is present in the map.
        pub fn contains(&self, key: K) -> bool {
            let mut key = K::transform(key);

            let key_ptr = NonNull::from_mut(&mut key).cast::<()>();

            // SAFETY: `key_ptr` points to a valid K and we are dropping that
            // value.
            unsafe { self.inner.contains(key_ptr) }
        }

        /// Return the number of elements in this `Map`
        pub fn len(&self) -> usize {
            self.inner.len()
        }

        /// Return the capacity of this `Map`
        pub fn capacity(&self) -> usize {
            self.inner.capacity()
        }

        /// Return `true` if this `Map` has a length of `0` and `false` otherwise
        pub fn is_empty(&self) -> bool {
            self.inner.is_empty()
        }
    }

    impl<K: Value, V: Value> Default for Map<K, V>
    where
        K::Transformed: PartialEq,
        V::Transformed: PartialEq,
    {
        fn default() -> Self {
            Self::new()
        }
    }
}

// We use `*mut ()` and `NonNull<()>` to represent `*mut T` and
// `NonNull<T>`. To make that clear, we alias `u8` here so we can use `T`
// as a fake type parameter.
type T = ();
type K = ();
type V = ();

#[derive(Clone)]
pub(crate) struct ErasedMap(Arc<Mutex<RawMap>>);

impl PartialEq for ErasedMap {
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

        for i in 0..this.len() {
            // SAFETY: `i` is less than `this.len`
            let key1 = unsafe { this.get_key(i) };

            // SAFETY: `i` is less than `this.len`
            let value1 = unsafe { this.get_value(i) };

            // SAFETY: `i` is less than `this.len`
            let Some(value2) = (unsafe { other.get(key1) }) else {
                return false;
            };

            // SAFETY: These values are valid because they are within the length of
            // the list. Note that this is of course not really safe, because the
            // types need to be the same, but we can't enforce that here.
            let is_eq = unsafe {
                (this.value_vtable.eq_fn)(value1.as_ptr(), value2.as_ptr())
            };

            if !is_eq {
                return false;
            }
        }

        true
    }
}

impl ErasedMap {
    pub fn new(key_vtable: VTable, value_vtable: VTable) -> Self {
        Self(Arc::new(Mutex::new(RawMap::new(key_vtable, value_vtable))))
    }

    /// Insert a value into the map
    ///
    /// # Safety
    ///
    ///  - `key_ptr` must be a pointer to the key type `K` that the list
    ///    contains. This function takes ownership of this value.
    ///  - `value_ptr` must be a pointer to the value type `V` that the list
    ///    contains. This function takes ownership of this value.
    ///
    pub unsafe fn insert(&self, key_ptr: NonNull<K>, value_ptr: NonNull<V>) {
        // SAFETY: We require that `elem_ptr` must be a pointer to the element
        // type `T` that the list contains.
        unsafe { self.0.lock().unwrap().insert(key_ptr, value_ptr) };
    }

    /// Get a value from the map based on a key.
    ///
    /// # Safety
    ///
    ///  - `key_ptr` must be a valid pointer to `K`.
    ///  - This function does not take ownership of the value under `key_ptr`.
    pub fn get(&self, key_ptr: NonNull<K>) -> Option<NonNull<V>> {
        // SAFETY: We forward the requirements to the caller.
        unsafe { self.0.lock().unwrap().get(key_ptr) }
    }

    /// Check whether a map contains a key.
    ///
    /// # Safety
    ///
    ///  - The `key_ptr` must point to a value of the key type of the map.
    ///  - There must be no mutables references to that value.
    ///  - This function takes ownership of the value under `key_ptr`.
    pub unsafe fn contains(&self, key_ptr: NonNull<T>) -> bool {
        // SAFETY: We require that the item_ptr points to the same type as in
        // the list.
        unsafe { self.0.lock().unwrap().contains(key_ptr) }
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

struct RawMap {
    /// Pointer to the current allocation
    ///
    /// If the element type is zero-sized this will be a dangling pointer
    /// and no allocation will happen.
    key_ptr: NonNull<K>,

    /// Pointer to the current allocation
    ///
    /// If the element type is zero-sized this will be a dangling pointer
    /// and no allocation will happen.
    value_ptr: NonNull<V>,

    /// Length of the list
    len: usize,

    /// Capacity of this current allocation
    ///
    /// If the element type is zero-sized, this will be usize::MAX.
    capacity: usize,

    /// V-Table of the key type
    key_vtable: VTable,

    /// V-Table of the value type
    value_vtable: VTable,
}

// SAFETY: The RawList is only not Send because of the pointer, but that simply
// represents a box and is safe to Send.
unsafe impl Send for RawMap {}

// SAFETY: RawList does not have interior mutability
unsafe impl Sync for RawMap {}

impl Drop for RawMap {
    fn drop(&mut self) {
        let len = self.len;
        self.len = 0;

        // SAFETY: We are the drop function, so the keys won't be referenced
        // anymore.
        unsafe { self.drop_key_elements_with(len) };

        // SAFETY: We are the drop function, so the values won't be referenced
        // anymore.
        unsafe { self.drop_value_elements_with(len) };

        // SAFETY: We are the only one to dealloc
        unsafe { self.dealloc() };
    }
}

impl RawMap {
    fn new(key_vtable: VTable, value_vtable: VTable) -> Self {
        Self::with_capacity(0, key_vtable, value_vtable)
    }

    /// Drop all keys in the map
    ///
    /// # Safety
    ///
    /// - The keys must be considered dropped after this function.
    unsafe fn drop_key_elements_with(&mut self, len: usize) {
        let Some(drop_fn) = self.key_vtable.drop_fn else {
            return;
        };

        for i in 0..len {
            let offset = self.offset_of_key(i);

            // SAFETY: We stay within the allocation because we stay within the
            // length and therefore within the capacity of the list.
            let ptr = unsafe { self.key_ptr.byte_add(offset).as_ptr() };

            // SAFETY: We give drop_fn the pointer of the value to drop as
            // argument.
            unsafe { (drop_fn)(ptr) }
        }
    }

    /// Drop all values in the map
    ///
    /// # Safety
    ///
    /// - The values must be considered dropped after this function.
    unsafe fn drop_value_elements_with(&mut self, len: usize) {
        let Some(drop_fn) = self.value_vtable.drop_fn else {
            return;
        };

        for i in 0..len {
            // SAFETY: The index is less than self.len by construction and we
            // require that the values are not used after this function.
            unsafe { self.drop_value(i, drop_fn) };
        }
    }

    fn with_capacity(
        capacity: usize,
        key_vtable: VTable,
        value_vtable: VTable,
    ) -> Self {
        // Create a dangling non-null pointer with the right alignment
        // SAFETY: Align cannot be zero, so a pointer constructed with it won't be null.
        let key_ptr = unsafe {
            NonNull::new_unchecked(std::ptr::without_provenance_mut(
                key_vtable.align(),
            ))
        };

        // Create a dangling non-null pointer with the right alignment
        // SAFETY: Align cannot be zero, so a pointer constructed with it won't be null.
        let value_ptr = unsafe {
            NonNull::new_unchecked(std::ptr::without_provenance_mut(
                value_vtable.align(),
            ))
        };

        let mut this = Self {
            key_ptr,
            value_ptr,
            len: 0,
            capacity: 0,
            key_vtable,
            value_vtable,
        };

        this.reserve(capacity);

        this
    }

    fn current_key_memory(&self) -> Option<NonNull<K>> {
        if self.key_vtable.size() == 0 || self.capacity == 0 {
            None
        } else {
            Some(self.key_ptr)
        }
    }

    fn current_value_memory(&self) -> Option<NonNull<V>> {
        if self.value_vtable.size() == 0 || self.capacity == 0 {
            None
        } else {
            Some(self.value_ptr)
        }
    }

    /// # Safety
    ///
    ///  - `key_ptr` must be a valid pointer to `K`.
    ///  - This function takes ownership of `key_ptr` and `value_ptr`.
    unsafe fn insert(&mut self, key_ptr: NonNull<K>, value_ptr: NonNull<V>) {
        // SAFETY: We require that `key_ptr` is valid and we don't transfer
        // ownership.
        let key_idx = unsafe { self.key_index(key_ptr) };
        match key_idx {
            Some(idx) => {
                if let Some(drop_fn) = self.value_vtable.drop_fn {
                    // SAFETY: `idx` is less than `self.len` and we don't
                    // use the value at `idx` later because we immediately
                    // write a new value to this location.
                    unsafe { self.drop_value(idx, drop_fn) };
                }
                // SAFETY: `idx` is less than `self.len` and `value_ptr` is
                // valid. We transfer ownership of `value_ptr` here.
                unsafe { self.write_value(idx, value_ptr) };

                // We didn't store the key value anywhere, so we need to drop
                // it.
                if let Some(drop_fn) = self.key_vtable.drop_fn {
                    // SAFETY: We have ownership over `key_ptr` and haven't
                    // dropped it yet. We also don't use it later.
                    unsafe { (drop_fn)(key_ptr.as_ptr()) }
                }
            }
            None => {
                self.reserve(1);

                // SAFETY: `idx` is less than `self.capacity` because we
                // reserved space for it. The value of `key_ptr` is valid and
                // we transfer ownership of `key_ptr` here.
                unsafe { self.write_key(self.len, key_ptr) };

                // SAFETY: `idx` is less than `self.capacity` because we
                // reserved space for it. The value of `value_ptr` is valid and
                // we transfer ownership of `value_ptr` here.
                unsafe { self.write_value(self.len, value_ptr) };
                self.len += 1;
            }
        }
    }

    /// Get a value from the map based on a key.
    ///
    /// # Safety
    ///
    ///  - `key_ptr` must be a valid pointer to `K`.
    ///  - This function does not take ownership of the value under `key_ptr`.
    unsafe fn get(&self, key_ptr: NonNull<K>) -> Option<NonNull<V>> {
        // SAFETY: We require that `key_ptr` is valid.
        let key_idx = unsafe { self.key_index(key_ptr) };

        // SAFETY: The `idx` is less than `self.len` because we got it
        // from `Self:::key_index`.
        key_idx.map(|idx| unsafe { self.get_value(idx) })
    }

    /// Write a key to an index in the map.
    ///
    /// # Safety
    ///
    ///  - `idx` must be less than `self.capacity`.
    ///  - `key_ptr` must be a valid pointer to `K`.
    ///  - This function takes ownership of the value under `key_ptr`.
    unsafe fn write_key(&mut self, idx: usize, key_ptr: NonNull<K>) {
        let offset = self.offset_of_key(idx);

        // The pointers are cast to *mut u8, to ensure that
        // copy_nonoverlapping will copy a number of bytes.
        let src = key_ptr.cast::<u8>().as_ptr();
        let dst = self.key_ptr.cast::<u8>().as_ptr();

        // SAFETY:
        //  - We stay within the allocation since we reserved the space
        //    for this element.
        let dst = unsafe { dst.byte_add(offset) };

        let size = self.key_vtable.size();

        // SAFETY:
        //  - The pointers have types of *mut u8 and *const u8 so we are
        //    copying a number of bytes.
        //  - `src` is valid for reads of `size` bytes since it points to a
        //    valid `T`.
        //  - `dst` is valid for writes of `size` bytes since we allocated
        //    the space for it.
        unsafe { std::ptr::copy_nonoverlapping(src, dst, size) };
    }

    /// Drop the value at a certain index.
    ///
    /// # Safety
    ///
    ///  - `idx` must be less than `self.capacity`.
    ///  - The value at index `idx` cannot be used anymore.
    unsafe fn drop_value(&mut self, idx: usize, drop_fn: DropFn) {
        let offset = self.offset_of_value(idx);

        // SAFETY: We stay within the allocation because we stay within the
        // length and therefore within the capacity of the list.
        let ptr = unsafe { self.value_ptr.byte_add(offset).as_ptr() };

        // SAFETY: We give drop_fn the pointer of the value to drop as
        // argument.
        unsafe { (drop_fn)(ptr) }
    }

    /// Write a value to an index in the map.
    ///
    /// # Safety
    ///
    ///  - `idx` must be less than `self.capacity`.
    ///  - `value_ptr` must be a valid pointer to `V`.
    ///  - This function takes ownership of the value under `value_ptr`.
    unsafe fn write_value(&mut self, idx: usize, value_ptr: NonNull<V>) {
        let offset = self.offset_of_value(idx);

        // The pointers are cast to *mut u8, to ensure that
        // copy_nonoverlapping will copy a number of bytes.
        let src = value_ptr.cast::<u8>().as_ptr();
        let dst = self.value_ptr.cast::<u8>().as_ptr();

        // SAFETY:
        //  - We stay within the allocation since we reserved the space
        //    for this element.
        let dst = unsafe { dst.byte_add(offset) };

        let size = self.value_vtable.size();

        // SAFETY:
        //  - The pointers have types of *mut u8 and *const u8 so we are
        //    copying a number of bytes.
        //  - `src` is valid for reads of `size` bytes since it points to a
        //    valid `T`.
        //  - `dst` is valid for writes of `size` bytes since we allocated
        //    the space for it.
        unsafe { std::ptr::copy_nonoverlapping(src, dst, size) };
    }

    fn reserve(&mut self, added: usize) {
        // The unwrap is intentional here. There's not much we can do except
        // panic when this overflows.
        let new_required = self.len.checked_add(added).unwrap();

        let size = Ord::max(self.key_vtable.size(), self.value_vtable.size());
        let new_capacity = compute_capacity(size, new_required);

        if new_capacity > self.capacity {
            if let Some(ptr) = self.current_key_memory() {
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
                        self.key_vtable.layout(),
                        self.capacity,
                        new_capacity,
                    )
                };
                self.key_ptr = new_ptr;
            } else if self.key_vtable.size() > 0 {
                // SAFETY: At this point, we know that the size of the layout
                // is not zero and we that the `new_capacity` is not
                // zero since it must be greater than `self.capacity` which
                // cannot be negative.
                let new_ptr = unsafe {
                    alloc_array(self.key_vtable.layout(), new_capacity)
                };
                self.key_ptr = new_ptr;
            }

            if let Some(ptr) = self.current_value_memory() {
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
                        self.value_vtable.layout(),
                        self.capacity,
                        new_capacity,
                    )
                };
                self.value_ptr = new_ptr;
            } else if self.value_vtable.size() > 0 {
                // SAFETY: At this point, we know that the size of the layout
                // is not zero and we that the `new_capacity` is not
                // zero since it must be greater than `self.capacity` which
                // cannot be negative.
                let new_ptr = unsafe {
                    alloc_array(self.key_vtable.layout(), new_capacity)
                };
                self.value_ptr = new_ptr;
            }

            self.capacity = new_capacity;
        }
    }

    /// Get a key from the key list
    ///
    /// # Safety
    ///
    ///  - `idx` needs to be less than `self.len`
    unsafe fn get_key(&self, idx: usize) -> NonNull<K> {
        let offset = self.offset_of_key(idx);

        // SAFETY: Since we know that idx < len < capacity and capacity is the
        // size of the allocation, we know that we'll stay within the
        // allocation.
        unsafe { self.key_ptr.byte_add(offset) }
    }

    /// Get a value from the value list
    ///
    /// # Safety
    ///
    ///  - `idx` needs to be less than `self.len`
    unsafe fn get_value(&self, idx: usize) -> NonNull<V> {
        let offset = self.offset_of_value(idx);

        // SAFETY: Since we know that idx < len < capacity and capacity is the
        // size of the allocation, we know that we'll stay within the
        // allocation.
        unsafe { self.value_ptr.byte_add(offset) }
    }

    /// Check whether the map contains a certain key.
    ///
    /// # Safety
    ///
    ///  - `item` must be valid pointer to `K`.
    ///  - The caller is responsible for dropping this value.
    unsafe fn contains(&self, item: NonNull<K>) -> bool {
        for i in 0..self.len() {
            // SAFETY: `i` is less than `self.len`.
            let elem = unsafe { self.get_key(i) };

            // SAFETY: The element is valid because we got it from the list and
            // the item is valid because we require it to be.
            let is_eq = unsafe {
                (self.key_vtable.eq_fn)(elem.as_ptr(), item.as_ptr())
            };

            if is_eq {
                return true;
            }
        }

        false
    }

    /// # Safety
    ///
    ///  - `key` must be a valid pointer to a `K`
    ///  - We do not take ownership of this value.
    unsafe fn key_index(&self, key: NonNull<K>) -> Option<usize> {
        for i in 0..self.len() {
            // SAFETY: `i` is less than `self.len`.
            let elem = unsafe { self.get_key(i) };

            // SAFETY: The element is valid because we got it from the list and
            // the item is valid because we require it to be.
            let is_eq = unsafe {
                (self.key_vtable.eq_fn)(elem.as_ptr(), key.as_ptr())
            };

            if is_eq {
                return Some(i);
            }
        }

        None
    }

    /// Deallocate this array
    ///
    /// # Safety
    ///
    /// This should only be called once.
    unsafe fn dealloc(&mut self) {
        if let Some(ptr) = self.current_key_memory() {
            // SAFETY: We allocated the ptr with alloc_array or realloc_array.
            unsafe {
                dealloc_array(ptr, self.key_vtable.layout(), self.capacity)
            };
        }

        if let Some(ptr) = self.current_value_memory() {
            // SAFETY: We allocated the ptr with alloc_array or realloc_array.
            unsafe {
                dealloc_array(ptr, self.value_vtable.layout(), self.capacity)
            };
        }
    }

    fn offset_of_key(&self, n: usize) -> usize {
        self.key_vtable.size() * n
    }

    fn offset_of_value(&self, n: usize) -> usize {
        self.value_vtable.size() * n
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
    use super::boundary::Map;

    #[test]
    fn empty_list() {
        let m = Map::<u64, u64>::new();
        assert_eq!(m.len(), 0);
    }

    #[test]
    fn insert_some_stuff() {
        let mut m = Map::<u64, u64>::new();
        m.insert(10, 20);
        m.insert(11, 21);
        m.insert(12, 22);

        assert_eq!(m.get(10), Some(20));
        assert_eq!(m.get(11), Some(21));
        assert_eq!(m.get(12), Some(22));
        assert!(m.contains(10));
        assert!(m.contains(11));
        assert!(m.contains(12));
        assert!(!m.contains(13));
    }
}
