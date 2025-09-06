mod plumbing;
use crate::TypedModule;
use crate::define_system;
use mira_common::store::StoreKey;
use mira_macros::queries;
pub use plumbing::{DefaultCache, QueryCache, QueryKey, SingleCache};

queries! {
    #[manually_allocated(u8)]
    mangle_module(StoreKey<TypedModule<'arena>>) -> &'arena str;

    #[manually_allocated(u8)]
    get_module_path(StoreKey<TypedModule<'arena>>) -> &'arena str;

    a(()) -> u8;
    b(()) -> u8;
}

#[allow(clippy::module_inception)]
pub mod queries {
    #[allow(unused_imports)]
    use super::*;
    use crate::define_modules;

    all_queries! { define_modules }
}
all_queries! { names define_system }
