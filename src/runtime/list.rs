use std::{marker::PhantomData, ptr::NonNull};

use super::{layout::Layout, Movability, VTable};

#[repr(transparent)]
struct List<T> {
    inner: RawList,
    _phantom: PhantomData<T>,
}

impl<T> List<T> {
    fn new() -> Self {
        Self {
            inner: RawList::new(VTable::of::<T>()),
            _phantom: PhantomData,
        }
    }
}

struct RawList {
    ptr: Option<NonNull<u8>>,
    vtable: VTable,
    capacity: usize,
    len: usize,
    drop: unsafe extern "C" fn(*mut ()),
}

impl RawList {
    fn new(vtable: VTable) -> Self {
        Self {
            ptr: None,
            vtable,
            capacity: 0,
            len: 0,
        }
    }

    fn realloc(&mut self, required_capacity: usize) {
        self.realloc_exact(required_capacity.next_multiple_of(2));
    }

    fn realloc_exact(&mut self, new_capacity: usize) {
        if new_capacity <= self.capacity {
            return;
        }

        if let Some(ptr) = self.ptr {
            let old_layout = self.vtable.layout.array(self.capacity);
            let new_layout = self.vtable.layout.array(new_capacity);
            let new_size = new_layout.size();

            unsafe {
                std::alloc::realloc(ptr.as_ptr(), old_layout.into(), new_size)
            };
        } else {
            debug_assert_eq!(self.capacity, 0);
            debug_assert_eq!(self.len, 0);

            let new_layout = self.vtable.layout.array(new_capacity);
            let ptr =
                NonNull::new(unsafe { std::alloc::alloc(new_layout.into()) });

            debug_assert!(ptr.is_some());

            self.ptr = ptr;
            self.capacity = new_capacity;
        }
    }

    fn clear(&mut self) {
        let Some(ptr) = self.ptr else {
            return;
        };

        if let Movability::CloneDrop(clone_drop) = self.vtable.movability {
            for x in 0..self.len {
                let offset = x * self.vtable.layout.size();
                let elem_ptr = unsafe { ptr.byte_offset(offset as isize) };
                (clone_drop.drop)(elem_ptr.as_ptr() as *mut ());
            }
        }

        self.len = 0;
    }

    fn dealloc(&mut self) {
        let old_layout = self.vtable.array(self.capacity);
        self.clear();
        let Some(ptr) = self.ptr else {
            return;
        };
        unsafe {
            std::alloc::dealloc(ptr.as_ptr(), old_layout.into());
        }
        self.ptr = None;
    }
}

impl Drop for RawList {
    fn drop(&mut self) {
        self.dealloc();
    }
}
