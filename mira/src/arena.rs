use parking_lot::lock_api::RawMutex as _;
use parking_lot::RawMutex;
use std::{alloc::Layout, cell::UnsafeCell, mem::MaybeUninit};

const MAX_ALIGNMENT: usize = std::mem::align_of::<u64>();
const PAGE: usize = 1024;
const HUGE_PAGE: usize = 2 * PAGE * PAGE;

struct Chunk {
    data: Box<[MaybeUninit<u8>]>,
    start: UnsafeCell<usize>,
}

#[allow(dead_code)]
struct UsedChunk(Box<[MaybeUninit<u8>]>);

impl Chunk {
    fn new_with_capacity(capacity: usize) -> Self {
        let data = Box::new_uninit_slice(capacity);
        let start = align_up(data.as_ptr().addr(), MAX_ALIGNMENT) - data.as_ptr().addr();
        Self {
            data,
            start: UnsafeCell::new(start),
        }
    }

    fn capacity(&self) -> usize {
        self.data.len()
    }

    fn into_used(self) -> UsedChunk {
        UsedChunk(self.data)
    }

    /// You have to guarantee that this does not get called by any other thread while the function
    /// is executing on the current thread.
    fn try_alloc(&self, size: usize, align: usize) -> Option<*mut u8> {
        assert!(align <= MAX_ALIGNMENT);
        let size = align_up(size, MAX_ALIGNMENT);
        // SAFETY: Only one thread at the time will execute this.
        let start = unsafe { *self.start.get() };
        if start + size > self.data.len() {
            return None;
        }
        unsafe { *self.start.get() += size };
        Some(&self.data[start..start + size] as *const _ as *mut _)
    }
}

#[inline(always)]
fn align_up(val: usize, align: usize) -> usize {
    debug_assert!(align.is_power_of_two());
    (val + align - 1) & !(align - 1)
}

pub struct Arena {
    chunk: UnsafeCell<Chunk>,
    used_chunks: UnsafeCell<Vec<UsedChunk>>,
    lock: RawMutex,
}

impl Arena {
    pub fn new() -> Self {
        Self {
            chunk: UnsafeCell::new(Chunk::new_with_capacity(PAGE)),
            lock: RawMutex::INIT,
            used_chunks: UnsafeCell::new(Vec::new()),
        }
    }

    fn alloc_raw(&self, layout: Layout) -> *mut u8 {
        if layout.size() == 0 {
            return std::ptr::without_provenance_mut(layout.align().max(1));
        }
        self.lock.lock();
        let chunk = unsafe { &mut *self.chunk.get() };
        if let Some(ptr) = chunk.try_alloc(layout.size(), layout.align()) {
            unsafe { self.lock.unlock() };
            return ptr;
        }
        let new_size = (chunk.capacity() * 2)
            .clamp(PAGE, HUGE_PAGE)
            .min(align_up(layout.size(), layout.align()) + MAX_ALIGNMENT - 1);
        let mut new_chunk = Chunk::new_with_capacity(new_size);
        let ptr = new_chunk
            .try_alloc(layout.size(), layout.align())
            .expect("this can't fail because the new chunk has to have enough space");
        std::mem::swap(chunk, &mut new_chunk);
        unsafe { &mut *self.used_chunks.get() }.push(new_chunk.into_used());
        unsafe { self.lock.unlock() };
        ptr
    }

    // SAFETY: The memory is unitialized and only ever given to one allocation
    #[allow(clippy::mut_from_ref)]
    pub fn alloc<T>(&self, value: T) -> &mut T {
        assert!(!std::mem::needs_drop::<T>());
        let layout = Layout::new::<T>();
        if layout.size() == 0 {
            return unsafe { &mut *std::ptr::without_provenance_mut::<T>(layout.align().max(1)) };
        }
        let mem = self.alloc_raw(Layout::new::<T>()) as *mut T;
        unsafe {
            std::ptr::write(mem, value);
            &mut *mem
        }
    }

    // SAFETY: The memory is unitialized and only ever given to one allocation
    #[allow(clippy::mut_from_ref)]
    #[inline]
    pub fn alloc_slice<T>(&self, slice: &[T]) -> &mut [T]
    where
        T: Copy,
    {
        assert!(!std::mem::needs_drop::<T>());
        let layout = Layout::for_value::<[T]>(slice);
        if layout.size() == 0 {
            return unsafe {
                std::slice::from_raw_parts_mut(
                    std::ptr::without_provenance_mut(layout.align().max(1)),
                    slice.len(),
                )
            };
        }

        let mem = self.alloc_raw(Layout::for_value::<[T]>(slice)) as *mut T;

        unsafe {
            mem.copy_from_nonoverlapping(slice.as_ptr(), slice.len());
            std::slice::from_raw_parts_mut(mem, slice.len())
        }
    }

    #[inline]
    pub fn alloc_str(&self, string: &str) -> &str {
        let slice = self.alloc_slice(string.as_bytes());

        // SAFETY: the result has a copy of the same valid UTF-8 bytes.
        unsafe { std::str::from_utf8_unchecked(slice) }
    }
}

impl Default for Arena {
    fn default() -> Self {
        Self::new()
    }
}

/// Allocations shouldn't be available after the arena is dropped
/// good code:
/// ```
/// use mira::arena::Arena;
/// let mut arena = Arena::new();
/// let twelve = arena.alloc(12u8);
/// let fourteen = arena.alloc(14usize);
/// let meow = arena.alloc_str("meow");
/// drop(arena);
/// ```
/// bad code:
/// ```compile_fail
/// use mira::arena::Arena;
/// let mut arena = Arena::new();
/// let twelve = arena.alloc(12u8);
/// let fourteen = arena.alloc(14usize);
/// let meow = arena.alloc_str("meow");
/// drop(arena);
/// println!("{twelve} {fourteen} {meow}"); // use-after-drop
/// ```
#[allow(unused)]
fn doctests() {}

#[cfg(test)]
mod test {
    use std::ops::Range;

    use super::*;

    fn check_align<T>(arena: &Arena) {
        assert!((arena.alloc_raw(Layout::new::<T>()) as *const T).is_aligned())
    }
    fn addr<T: ?Sized>(v: &T) -> usize {
        (v as *const T).addr()
    }
    fn addr_range<T: ?Sized>(v: &T) -> Range<usize> {
        let addr = (v as *const T).addr();
        addr..addr + std::mem::size_of_val::<T>(v)
    }

    fn all_unique(ranges: &[Range<usize>]) -> bool {
        for (i1, r1) in ranges.iter().enumerate() {
            for (i2, r2) in ranges.iter().enumerate() {
                if i1 == i2 {
                    continue;
                }
                if r1.start < r2.end && r1.end > r2.start {
                    println!("range {r1:?} and {r2:?} aren't unique 1");
                    return false;
                }
                if r2.start < r1.end && r2.end > r1.start {
                    println!("range {r1:?} and {r2:?} aren't unique 2");
                    return false;
                }
            }
        }
        true
    }

    #[test]
    fn all_unique_test() {
        // 2 ranges aren't intersecting
        assert!(all_unique(&[0..5, 5..10]));
        // range 2 starts in range 1 but ends outside
        assert!(!all_unique(&[0..5, 3..10]));
        // range 2 is contained in range 1
        assert!(!all_unique(&[0..10, 3..7]));
        // range 2 starts before range 1 but ends in it
        assert!(!all_unique(&[5..10, 0..6]));
    }

    #[test]
    fn allocs() {
        let arena = Arena::new();
        let allocations = [
            addr_range(arena.alloc(12usize)),
            addr_range(arena.alloc(12usize)),
            addr_range(arena.alloc(12usize)),
            addr_range(arena.alloc_str("meow purrrr")),
            addr_range(arena.alloc_slice(&[1, 2, 3, 4u8])),
        ];
        assert!(all_unique(&allocations));
    }

    #[test]
    fn allocs_zst() {
        let arena = Arena::new();
        let allocations = [
            addr(arena.alloc(())),
            addr(arena.alloc(())),
            addr(arena.alloc(())),
            addr(arena.alloc(())),
            addr(arena.alloc_slice::<u8>(&[])),
            addr(arena.alloc_slice(&[()])),
            addr(arena.alloc_slice::<()>(&[])),
        ];
        // all allocations are zero-sized and have an alignment of 1
        assert!(allocations.iter().all(|v| *v == 1));
        assert_eq!(addr(arena.alloc_slice::<u16>(&[])), 2);
        assert_eq!(addr(arena.alloc_slice::<u32>(&[])), 4);
        assert_eq!(addr(arena.alloc_slice::<u64>(&[])), 8);
        assert_eq!(
            addr(arena.alloc_slice::<usize>(&[])),
            std::mem::align_of::<usize>()
        );
    }

    #[test]
    fn alloc_align() {
        let arena = Arena::new();
        check_align::<usize>(&arena);
        check_align::<u8>(&arena);
        check_align::<u16>(&arena);
        check_align::<u32>(&arena);
        check_align::<u64>(&arena);
    }
}
