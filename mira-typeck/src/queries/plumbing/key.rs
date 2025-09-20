use mira_parser::module::ModuleId;

use crate::Ty;

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

impl QueryKey for ModuleId {
    type Cache<V: Copy> = DefaultCache<Self, V>;
}
