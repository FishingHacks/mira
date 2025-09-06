use parking_lot::Mutex;
use std::{mem::MaybeUninit, ptr::NonNull};

// leave 96 bytes for the system allocator to put metadata
const PAGE: usize = 4000;

fn default_capacity_for<T>() -> usize {
    (size_of::<T>() / PAGE).max(1)
}

struct ArenaChunk<T> {
    storage: NonNull<[MaybeUninit<T>]>,
    entries: usize,
}

impl<T> ArenaChunk<T> {
    pub(crate) fn new(capacity: usize) -> Self {
        Self {
            storage: NonNull::from(Box::leak(Box::new_uninit_slice(capacity))),
            entries: 0,
        }
    }

    /// SAFETY:
    /// [`Self::can_allocate`] when called with a value n that's greater than 0 must have returned `true` and this must
    /// not be called more than n times.
    unsafe fn allocate_inner(&mut self, value: T) -> NonNull<T> {
        if self.storage.len() <= self.entries {
            unsafe { std::hint::unreachable_unchecked() }
        }
        let p = unsafe { &mut self.storage.as_mut()[self.entries] };
        self.entries += 1;
        *p = MaybeUninit::new(value);
        NonNull::from(unsafe { p.assume_init_mut() })
    }

    pub(crate) fn allocate(&mut self, value: T) -> Option<NonNull<T>> {
        if !self.can_allocate(1) {
            return None;
        }
        Some(unsafe { self.allocate_inner(value) })
    }

    pub(crate) fn allocate_slice(
        &mut self,
        mut values: impl ExactSizeIterator<Item = T>,
    ) -> Option<NonNull<[T]>> {
        let start = self.entries;
        let amount = values.len();
        if !self.can_allocate(amount) {
            return None;
        }
        let Some(first) = values.next() else {
            return Some(NonNull::from(unsafe {
                std::slice::from_raw_parts_mut(NonNull::dangling().as_ptr(), 0)
            }));
        };
        let ptr = unsafe { self.allocate_inner(first) }.as_ptr();
        for value in values {
            unsafe { self.allocate_inner(value) };
        }
        Some(NonNull::from(unsafe {
            std::slice::from_raw_parts_mut(ptr, self.entries - start)
        }))
    }

    pub(crate) const fn capacity(&self) -> usize {
        self.storage.len()
    }

    pub(crate) const fn can_allocate(&self, amount: usize) -> bool {
        self.entries + amount <= self.capacity()
    }
}

impl<T> Drop for ArenaChunk<T> {
    fn drop(&mut self) {
        if std::mem::needs_drop::<T>() {
            unsafe {
                for value in &mut self.storage.as_mut()[..self.entries] {
                    value.assume_init_drop();
                }
            }
        }
        unsafe { drop(Box::from_raw(self.storage.as_ptr())) }
    }
}

#[derive(Default)]
pub struct TypeArena<T> {
    inner: Mutex<Vec<ArenaChunk<T>>>,
}

unsafe impl<T> Send for TypeArena<T> {}
unsafe impl<T> Sync for TypeArena<T> {}

impl<T> TypeArena<T> {
    pub fn new() -> Self {
        Self {
            inner: Mutex::new(Vec::new()),
        }
    }

    fn ensure_size_available(&self, min_cap: usize) {
        let mut lock = self.inner.lock();
        if lock.is_empty() {
            lock.push(ArenaChunk::new(default_capacity_for::<T>().max(min_cap)));
        } else if !lock[lock.len() - 1].can_allocate(min_cap) {
            let size = min_cap.max(lock[lock.len() - 1].capacity() * 2);
            lock.push(ArenaChunk::new(size));
        }
    }

    #[allow(clippy::mut_from_ref)]
    pub fn allocate(&self, value: T) -> &mut T {
        self.ensure_size_available(1);
        let mut guar = self.inner.lock();
        let i = guar.len() - 1;
        let p = guar[i]
            .allocate(value)
            .expect("just checked that this can be allocated")
            .as_ptr();
        unsafe { &mut *p }
    }

    #[allow(clippy::mut_from_ref)]
    pub fn allocate_slice(&self, value: impl ExactSizeIterator<Item = T>) -> &mut [T] {
        self.ensure_size_available(value.len());
        let mut guar = self.inner.lock();
        let i = guar.len() - 1;
        let p = guar[i]
            .allocate_slice(value)
            .expect("just checked that this can be allocated")
            .as_ptr();
        unsafe { &mut *p }
    }
}

impl TypeArena<u8> {
    #[allow(clippy::mut_from_ref)]
    pub fn allocate_str(&self, value: &str) -> &mut str {
        let bytes = self.allocate_slice(value.as_bytes().iter().copied());
        unsafe { str::from_utf8_unchecked_mut(bytes) }
    }
}
