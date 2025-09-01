use mira_common::store::StoreKey;

use crate::{Ty, TypedModule};

use super::cache::{DefaultCache, QueryCache, SingleCache};

pub trait QueryKey {
    type Cache<V: Copy>: QueryCache<Key = Self, Value = V>;
}

impl QueryKey for () {
    type Cache<V: Copy> = SingleCache<V>;
}

impl QueryKey for Ty<'_> {
    type Cache<V: Copy> = DefaultCache<Self, V>;
}

impl QueryKey for StoreKey<TypedModule<'_>> {
    type Cache<V: Copy> = DefaultCache<Self, V>;
}
