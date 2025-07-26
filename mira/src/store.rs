use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    hash::Hash,
    marker::PhantomData,
    ops::{Index, IndexMut},
};

#[derive(PartialOrd, Ord)]
pub struct StoreKey<T: ?Sized>(usize, PhantomData<*const T>);

// SAFETY: the store key does not actually store or associate itself with any data, the generic
// parameter only exists to ensure the storekey does not accidentally get used on a wrong store.
unsafe impl<T: ?Sized> Send for StoreKey<T> {}
unsafe impl<T: ?Sized> Sync for StoreKey<T> {}

impl<T: ?Sized> Copy for StoreKey<T> {}
impl<T: ?Sized> Clone for StoreKey<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T: ?Sized> Hash for StoreKey<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}
impl<T: ?Sized> PartialEq for StoreKey<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<T: ?Sized> Eq for StoreKey<T> {}

impl<T: ?Sized> StoreKey<T> {
    #[inline(always)]
    pub fn cast<O: ?Sized>(self) -> StoreKey<O> {
        StoreKey(self.0, PhantomData)
    }

    /// a key that should never be defined :3
    pub fn undefined() -> Self {
        Self(usize::MAX, PhantomData)
    }
}

impl<T: ?Sized> Debug for StoreKey<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("StoreKey#")?;
        Display::fmt(&self.0, f)?;
        f.write_str(" @ ")?;
        f.write_str(std::any::type_name::<T>())
    }
}
impl<T: ?Sized> Display for StoreKey<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

#[derive(Clone, Debug)]
pub struct Store<T> {
    values: HashMap<StoreKey<T>, T>,
    next_key: usize,
}

impl<T> Default for Store<T> {
    fn default() -> Self {
        Self {
            values: HashMap::new(),
            next_key: 0,
        }
    }
}

impl<T> Store<T> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            values: HashMap::with_capacity(capacity),
            next_key: 0,
        }
    }

    pub fn insert(&mut self, value: T) -> StoreKey<T> {
        let key = StoreKey(self.next_key, PhantomData);
        self.next_key += 1;
        self.values.insert(key, value);
        key
    }

    pub fn reserve_key(&mut self) -> StoreKey<T> {
        let key = StoreKey(self.next_key, PhantomData);
        self.next_key += 1;
        key
    }

    pub fn insert_reserved(&mut self, reserved_key: StoreKey<T>, value: T) {
        if self.next_key <= reserved_key.0 {
            panic!("key was not reserved");
        }
        if self.values.contains_key(&reserved_key) {
            panic!("key was already inserted");
        }
        self.values.insert(reserved_key, value);
    }

    pub fn get(&self, key: &StoreKey<T>) -> Option<&T> {
        self.values.get(key)
    }

    pub fn get_mut(&mut self, key: &StoreKey<T>) -> Option<&mut T> {
        self.values.get_mut(key)
    }

    pub fn remove(&mut self, key: &StoreKey<T>) -> Option<T> {
        self.values.remove(key)
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn index_value_iter(&self) -> impl Iterator<Item = (StoreKey<T>, &T)> {
        self.values.iter().map(|(a, b)| (*a, b))
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.values.values()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.values.values_mut()
    }

    pub fn indices(&self) -> impl Iterator<Item = StoreKey<T>> + use<'_, T> {
        self.values.keys().copied()
    }

    pub fn retain<F>(&mut self, pred: F)
    where
        F: FnMut(&StoreKey<T>, &mut T) -> bool,
    {
        self.values.retain(pred);
    }
}

impl<T> IntoIterator for Store<T> {
    type Item = T;

    type IntoIter = std::collections::hash_map::IntoValues<StoreKey<T>, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.values.into_values()
    }
}

impl<T> Index<StoreKey<T>> for Store<T> {
    type Output = T;

    fn index(&self, index: StoreKey<T>) -> &Self::Output {
        self.get(&index).expect("indexed into empty value")
    }
}
impl<T> Index<&StoreKey<T>> for Store<T> {
    type Output = T;

    fn index(&self, index: &StoreKey<T>) -> &Self::Output {
        self.get(index).expect("indexed into empty value")
    }
}
impl<T> IndexMut<StoreKey<T>> for Store<T> {
    fn index_mut(&mut self, index: StoreKey<T>) -> &mut Self::Output {
        self.get_mut(&index).expect("indexed into empty value")
    }
}
impl<T> IndexMut<&StoreKey<T>> for Store<T> {
    fn index_mut(&mut self, index: &StoreKey<T>) -> &mut Self::Output {
        self.get_mut(index).expect("indexed into empty value")
    }
}

/// A store that associates a key of one type with another type.
#[derive(Clone, Debug)]
pub struct AssociatedStore<T, O: ?Sized> {
    values: HashMap<StoreKey<T>, T>,
    _other: PhantomData<O>,
}

impl<T, O: ?Sized> Default for AssociatedStore<T, O> {
    fn default() -> Self {
        Self {
            values: HashMap::new(),
            _other: PhantomData,
        }
    }
}

impl<T, O: ?Sized> AssociatedStore<T, O> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            values: HashMap::with_capacity(capacity),
            _other: PhantomData,
        }
    }

    pub fn insert(&mut self, key: StoreKey<O>, value: T) -> StoreKey<T> {
        let key = key.cast();
        assert!(!self.values.contains_key(&key));
        self.values.insert(key, value);
        key
    }

    pub fn get_self(&self, key: &StoreKey<T>) -> Option<&T> {
        self.values.get(key)
    }

    pub fn get_mut_self(&mut self, key: &StoreKey<T>) -> Option<&mut T> {
        self.values.get_mut(key)
    }

    pub fn remove_self(&mut self, key: &StoreKey<T>) -> Option<T> {
        self.values.remove(key)
    }

    pub fn get(&self, key: &StoreKey<O>) -> Option<&T> {
        self.values.get(Self::cast_key_ref(key))
    }

    pub fn get_mut(&mut self, key: &StoreKey<O>) -> Option<&mut T> {
        self.values.get_mut(Self::cast_key_ref(key))
    }

    pub fn remove(&mut self, key: &StoreKey<O>) -> Option<T> {
        self.values.remove(Self::cast_key_ref(key))
    }

    pub fn into_store(self) -> Store<T> {
        let next_key = self.values.keys().map(|v| v.0).max().unwrap_or(0);
        Store {
            values: self.values,
            next_key,
        }
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    fn cast_key_ref(k: &StoreKey<O>) -> &StoreKey<T> {
        unsafe { std::mem::transmute(k) }
    }

    pub fn index_value_iter(&self) -> impl Iterator<Item = (StoreKey<T>, &T)> {
        self.values.iter().map(|(a, b)| (*a, b))
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.values.values()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.values.values_mut()
    }

    pub fn indices(&self) -> impl Iterator<Item = StoreKey<T>> + use<'_, T, O> {
        self.values.keys().copied()
    }
}
impl<T, O> Index<StoreKey<O>> for AssociatedStore<T, O> {
    type Output = T;

    fn index(&self, index: StoreKey<O>) -> &Self::Output {
        self.get(&index).expect("indexed into empty value")
    }
}
impl<T, O> Index<&StoreKey<O>> for AssociatedStore<T, O> {
    type Output = T;

    fn index(&self, index: &StoreKey<O>) -> &Self::Output {
        self.get(index).expect("indexed into empty value")
    }
}
impl<T, O> IndexMut<StoreKey<O>> for AssociatedStore<T, O> {
    fn index_mut(&mut self, index: StoreKey<O>) -> &mut Self::Output {
        self.get_mut(&index).expect("indexed into empty value")
    }
}
impl<T, O> IndexMut<&StoreKey<O>> for AssociatedStore<T, O> {
    fn index_mut(&mut self, index: &StoreKey<O>) -> &mut Self::Output {
        self.get_mut(index).expect("indexed into empty value")
    }
}

impl<T, O> From<AssociatedStore<T, O>> for Store<T> {
    fn from(val: AssociatedStore<T, O>) -> Self {
        val.into_store()
    }
}
