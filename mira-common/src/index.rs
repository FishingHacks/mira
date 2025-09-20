use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::{Bound, Index, IndexMut, RangeBounds, RangeInclusive};

pub trait Idx: Copy + Hash + PartialEq + Eq + PartialOrd + Debug {
    const ZERO: Self;

    fn next(self) -> Self;
    fn as_usize(self) -> usize;
    fn from_usize(u: usize) -> Self;
}

#[macro_export]
macro_rules! newty {
    (@inner $vis:vis, $name:ident $($const_name:ident $val:literal)*) => {
        #[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Hash)]
        $vis struct $name {
            __priv: std::num::NonZeroUsize,
        }

        impl $name {
            $vis const ZERO: Self = Self::new(0);
            $vis const MAX: Self = Self::new(usize::MAX - 1);
            $($vis const $const_name: Self = Self::new($val);)*

            $vis const fn new(v: usize) -> Self {
                Self { __priv: std::num::NonZeroUsize::new(v + 1).unwrap() }
            }
            $vis const fn to_usize(self) -> usize {
                self.__priv.get() - 1
            }
        }

        impl $crate::index::Idx for $name {
            const ZERO: Self = Self::new(0);

            fn next(self) -> Self {
                Self::new(self.__priv.get())
            }

            fn as_usize(self) -> usize {
                self.to_usize()
            }

            fn from_usize(u: usize) -> Self {
                Self::new(u)
            }
        }
    };

    (@rem_disp #[display($($t:tt)+)]) => {};
    (@rem_disp $($t:tt)*) => { $($t:tt)* };

    (@disp $name:ident display($($disp:tt)+)) => {
        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_fmt(format_args!($($disp)+, self.to_usize()))
            }
        }
    };
    (@disp $($t:tt)*) => {};

    ($( $(#[$($meta:tt)*])* $vis:vis struct $name:ident { $(const $const_name:ident = $val:literal;)* } )+) => {
        $(
            $crate::newty!(@inner $vis, $name $($const_name $val)* );
            $($crate::newty!(@disp $name $($meta)*);)*
        )+
    }
}

#[derive(Debug)]
pub struct IndexVec<I: Idx, V> {
    next_idx: I,
    values: Vec<V>,
}

impl<I: Idx, V> IndexVec<I, V> {
    pub const fn new() -> Self {
        Self {
            next_idx: I::ZERO,
            values: Vec::new(),
        }
    }

    pub fn add(&mut self, v: V) -> I {
        let i = self.next_idx;
        self.next_idx = self.next_idx.next();
        assert_eq!(i.as_usize(), self.values.len());
        self.values.push(v);
        i
    }

    fn to_bounds<R: RangeBounds<I>>(&self, r: R) -> RangeInclusive<usize> {
        let start = r.start_bound();
        let end = r.end_bound();
        let start = match start {
            Bound::Included(v) => v.as_usize(),
            Bound::Excluded(v) => v.as_usize() + 1,
            Bound::Unbounded => 0,
        };
        let end = match end {
            Bound::Included(v) => v.as_usize(),
            Bound::Excluded(v) => v.as_usize() - 1,
            Bound::Unbounded => self.values.len() - 1,
        };
        start..=end
    }

    pub fn slice<R: RangeBounds<I>>(&self, r: R) -> &[V] {
        &self.values[self.to_bounds(r)]
    }

    pub fn slice_mut<R: RangeBounds<I>>(&mut self, r: R) -> &mut [V] {
        let bounds = self.to_bounds(r);
        &mut self.values[bounds]
    }

    pub fn as_slice(&self) -> &[V] {
        &self.values
    }

    pub fn as_slice_mut(&mut self) -> &mut [V] {
        &mut self.values
    }

    pub fn iter(&self) -> std::slice::Iter<'_, V> {
        self.values.iter()
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, V> {
        self.values.iter_mut()
    }

    pub fn entries(&self) -> impl ExactSizeIterator<Item = (I, &V)> {
        self.iter().enumerate().map(|(i, v)| (I::from_usize(i), v))
    }

    pub fn entries_mut(&mut self) -> impl ExactSizeIterator<Item = (I, &mut V)> {
        self.iter_mut()
            .enumerate()
            .map(|(i, v)| (I::from_usize(i), v))
    }

    pub fn keys(&self) -> impl ExactSizeIterator<Item = I> {
        (0..self.values.len()).map(I::from_usize)
    }

    pub fn get(&self, key: I) -> Option<&V> {
        self.values.get(key.as_usize())
    }

    pub fn get_mut(&mut self, key: I) -> Option<&mut V> {
        self.values.get_mut(key.as_usize())
    }

    pub const fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    pub const fn len(&self) -> usize {
        self.values.len()
    }
}

impl<I: Idx, V> Index<I> for IndexVec<I, V> {
    type Output = V;

    fn index(&self, index: I) -> &Self::Output {
        &self.values[index.as_usize()]
    }
}

impl<I: Idx, V> IndexMut<I> for IndexVec<I, V> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.values[index.as_usize()]
    }
}

impl<I: Idx, V> Default for IndexVec<I, V> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct IndexStore<I: Idx, V> {
    values: HashMap<I, V>,
    last_id: I,
}

impl<I: Idx, V> IndexStore<I, V> {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            last_id: I::ZERO,
        }
    }

    pub fn reserve_id(&mut self) -> I {
        let id = self.last_id;
        self.last_id = self.last_id.next();
        id
    }

    pub fn insert_reserved(&mut self, v: V, id: I) {
        assert!(id < self.last_id);
        assert!(!self.values.contains_key(&id));
        self.values.insert(id, v);
    }

    pub fn add(&mut self, v: V) -> I {
        let id = self.reserve_id();
        self.insert_reserved(v, id);
        id
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = &V> {
        self.values.values()
    }

    pub fn iter_mut(&mut self) -> impl ExactSizeIterator<Item = &mut V> {
        self.values.values_mut()
    }

    pub fn entries(&self) -> impl ExactSizeIterator<Item = (I, &V)> {
        self.values.iter().map(|(i, v)| (*i, v))
    }

    pub fn entries_mut(&mut self) -> impl ExactSizeIterator<Item = (I, &mut V)> {
        self.values.iter_mut().map(|(i, v)| (*i, v))
    }

    pub fn keys(&self) -> impl ExactSizeIterator<Item = I> {
        self.values.keys().copied()
    }

    pub fn get(&self, key: I) -> Option<&V> {
        self.values.get(&key)
    }

    pub fn get_mut(&mut self, key: I) -> Option<&mut V> {
        self.values.get_mut(&key)
    }
}

impl<I: Idx, V> Index<I> for IndexStore<I, V> {
    type Output = V;

    fn index(&self, index: I) -> &Self::Output {
        &self.values[&index]
    }
}

impl<I: Idx, V> IndexMut<I> for IndexStore<I, V> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        self.values.get_mut(&index).expect("no entry found for key")
    }
}

impl<I: Idx, V> Default for IndexStore<I, V> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct IndexMap<I: Idx, V> {
    values: HashMap<I, V>,
}

impl<I: Idx, V> IndexMap<I, V> {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            values: HashMap::with_capacity(capacity),
        }
    }

    pub fn remove(&mut self, key: I) -> Option<V> {
        self.values.remove(&key)
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    pub fn insert(&mut self, i: I, v: V) {
        if self.contains(i) {
            panic!("Tried to insert key {i:?} multiple times");
        }
        self.values.insert(i, v);
    }

    pub fn contains(&self, i: I) -> bool {
        self.values.contains_key(&i)
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item = &V> {
        self.values.values()
    }

    pub fn iter_mut(&mut self) -> impl ExactSizeIterator<Item = &mut V> {
        self.values.values_mut()
    }

    pub fn entries(&self) -> impl ExactSizeIterator<Item = (I, &V)> {
        self.values.iter().map(|(i, v)| (*i, v))
    }

    pub fn entries_mut(&mut self) -> impl ExactSizeIterator<Item = (I, &mut V)> {
        self.values.iter_mut().map(|(i, v)| (*i, v))
    }

    pub fn keys(&self) -> impl ExactSizeIterator<Item = I> {
        self.values.keys().copied()
    }

    pub fn get(&self, key: I) -> Option<&V> {
        self.values.get(&key)
    }

    pub fn get_mut(&mut self, key: I) -> Option<&mut V> {
        self.values.get_mut(&key)
    }
}

impl<I: Idx, V> Index<I> for IndexMap<I, V> {
    type Output = V;

    fn index(&self, index: I) -> &Self::Output {
        &self.values[&index]
    }
}

impl<I: Idx, V> IndexMut<I> for IndexMap<I, V> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        self.values.get_mut(&index).expect("no entry found for key")
    }
}

impl<I: Idx, V> Default for IndexMap<I, V> {
    fn default() -> Self {
        Self::new()
    }
}
