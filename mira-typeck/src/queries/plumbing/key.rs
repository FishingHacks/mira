use mira_parser::module::{ModuleId, StructId};

use crate::Ty;

use super::cache::{DefaultCache, QueryCache, SingleCache};

macro_rules! default_cache {
    ($($ty:ty),* $(,)?) => {
        $(
        impl QueryKey for $ty {
            type Cache<V: Copy> = DefaultCache<Self, V>;
        })*
    };
}

pub trait QueryKey {
    type Cache<V: Copy>: QueryCache<Key = Self, Value = V>;
}

impl QueryKey for () {
    type Cache<V: Copy> = SingleCache<V>;
}

default_cache!(StructId, ModuleId, Ty<'_>);
