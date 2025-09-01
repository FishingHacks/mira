use parking_lot::RwLock;
use std::{collections::HashMap, fmt::Debug, hash::Hash, sync::OnceLock};

pub trait QueryCache: Default {
    type Key: Hash + Eq + Copy + Debug;
    type Value: Copy;

    fn get(&self, k: &Self::Key) -> Option<Self::Value>;
    fn provide(&self, k: Self::Key, v: Self::Value);
}

pub struct DefaultCache<K: Hash + Eq + Copy + Debug, V: Copy> {
    inner: RwLock<HashMap<K, V>>,
}

impl<K: Hash + Eq + Copy + Debug, V: Copy> Default for DefaultCache<K, V> {
    fn default() -> Self {
        Self {
            inner: Default::default(),
        }
    }
}

impl<K: Hash + Eq + Copy + Debug, V: Copy> QueryCache for DefaultCache<K, V> {
    type Key = K;
    type Value = V;

    fn get(&self, k: &K) -> Option<Self::Value> {
        self.inner.read().get(k).copied()
    }

    fn provide(&self, k: K, v: V) {
        if self.inner.write().insert(k, v).is_some() {
            panic!("tried to cache a value for an already cached key");
        }
    }
}

pub struct SingleCache<V: Copy> {
    cache: OnceLock<V>,
}

impl<V: Copy> QueryCache for SingleCache<V> {
    type Key = ();
    type Value = V;

    fn get(&self, _: &()) -> Option<V> {
        self.cache.get().copied()
    }

    fn provide(&self, _: (), v: V) {
        if self.cache.set(v).is_err() {
            panic!("tried to cache a value for an already cached key")
        }
    }
}

impl<V: Copy> Default for SingleCache<V> {
    fn default() -> Self {
        Self {
            cache: OnceLock::new(),
        }
    }
}
