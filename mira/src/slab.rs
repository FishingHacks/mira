use std::{
    fmt::Debug,
    ops::{Index, IndexMut},
};

enum Entry<T> {
    Occupied(T),
    Empty(usize),
}

impl<T: Copy> Copy for Entry<T> {}
impl<T: Clone> Clone for Entry<T> {
    fn clone(&self) -> Self {
        match self {
            Entry::Occupied(v) => Entry::Occupied(v.clone()),
            Entry::Empty(v) => Entry::Empty(*v),
        }
    }
}
impl<T: Debug> Debug for Entry<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Entry::Occupied(v) => Debug::fmt(v, f),
            Entry::Empty(v) => Debug::fmt(v, f),
        }
    }
}

pub struct Slab<T> {
    inner: Vec<Entry<T>>,
    /// if next_idx >= inner.len(), it means the slab is entirely filled. This also means that
    /// the max length is usize::MAX - 1. You should use usize::MAX to ensure that it will always
    /// be >= inner.len().
    next_idx: usize,
}

impl<T: Clone> Clone for Slab<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            next_idx: self.next_idx,
        }
    }
}

impl<T> Slab<T> {
    pub fn clean_slab(&mut self) {
        while let Some(Entry::Empty(_)) = self.inner.last() {
            self.inner.pop();
        }
        self.next_idx = usize::MAX;
        for (idx, v) in self.inner.iter_mut().enumerate() {
            if let Entry::Empty(v) = v {
                *v = self.next_idx;
                self.next_idx = idx;
            }
        }
        self.validate();
    }

    fn validate(&self) {
        if let Some(Entry::Occupied(_)) = self.inner.get(self.next_idx) {
            panic!("invalid slab cleanup D:")
        }
        for v in self.inner.iter() {
            if let Entry::Empty(v) = v {
                if let Some(Entry::Occupied(_)) = self.inner.get(*v) {
                    panic!("invalid slab cleanup D:")
                }
            }
        }
    }

    pub fn push(&mut self, val: T) -> usize {
        match (self.next_idx < self.len()).then_some(self.next_idx) {
            Some(idx) => {
                let entry = &mut self.inner[idx];

                let Entry::Empty(next_idx) = entry else {
                    panic!("attempt to insert into a full slot.");
                };

                self.next_idx = *next_idx;
                *entry = Entry::Occupied(val);
                idx
            }
            None => {
                // self.inner.len() can never reach usize::MAX.
                if self.inner.len() == usize::MAX - 1 {
                    panic!("tried to insert into slab, but its entirely filled.");
                }
                self.inner.push(Entry::Occupied(val));
                self.next_idx = usize::MAX;
                self.inner.len() - 1
            }
        }
    }

    pub fn remove(&mut self, idx: usize) -> Option<T> {
        if matches!(self.inner[idx], Entry::Empty(_)) {
            return None;
        }

        let mut entry = Entry::Empty(self.next_idx);
        self.next_idx = idx;
        std::mem::swap(&mut self.inner[idx], &mut entry);

        match entry {
            Entry::Occupied(v) => Some(v),
            Entry::Empty(_) => None,
        }
    }

    pub fn get_mut(&mut self, idx: usize) -> Option<&mut T> {
        match self.inner.get_mut(idx) {
            Some(Entry::Occupied(v)) => Some(v),
            _ => None,
        }
    }

    pub fn get(&self, idx: usize) -> Option<&T> {
        match self.inner.get(idx) {
            Some(Entry::Occupied(v)) => Some(v),
            _ => None,
        }
    }

    pub const fn new() -> Self {
        Self {
            inner: Vec::new(),
            next_idx: usize::MAX,
        }
    }
    pub fn len(&self) -> usize {
        self.inner.len()
    }
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = (usize, &T)> {
        self.inner.iter().enumerate().filter_map(|(i, v)| match v {
            Entry::Occupied(v) => Some((i, v)),
            _ => None,
        })
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (usize, &mut T)> {
        self.inner
            .iter_mut()
            .enumerate()
            .filter_map(|(i, v)| match v {
                Entry::Occupied(v) => Some((i, v)),
                _ => None,
            })
    }
}

impl<T> Default for Slab<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Debug> Debug for Slab<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.inner, f)
    }
}

impl<T> IndexMut<usize> for Slab<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        match self.get_mut(index) {
            Some(v) => v,
            None => panic!("Cannot get index {index} of slab"),
        }
    }
}

impl<T> Index<usize> for Slab<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        match self.get(index) {
            Some(v) => v,
            None => panic!("Cannot get index {index} of slab"),
        }
    }
}
