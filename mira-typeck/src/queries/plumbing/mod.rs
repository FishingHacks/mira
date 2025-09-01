mod cache;
mod key;
use std::marker::PhantomData;

pub use cache::QueryCache;
pub use key::QueryKey;

use crate::TypeckCtx;

use super::{Providers, QueryArenas, QueryCaches, QueryStates, QuerySystem};

impl<'arena> QuerySystem<'arena> {
    pub fn new() -> Self {
        Self {
            providers: Providers::NEW,
            arenas: QueryArenas::default(),
            caches: QueryCaches::default(),
            states: QueryStates::default(),
        }
    }

    pub fn get_providers(&mut self) -> &mut Providers<'arena> {
        &mut self.providers
    }
}

impl<'arena> Default for QuerySystem<'arena> {
    fn default() -> Self {
        Self::new()
    }
}

#[macro_export]
macro_rules! macro_if {
    (if ($($t:tt)+) { $($true:tt)* } $(else $($false:tt)*)?) => {
        $($true)*
    };
    (if () { $($true:tt)* } else $($false:tt)*) => { $($false)* };
    (if () { $($true:tt)* }) => {};

    (ifn ($($t:tt)+) { $($true:tt)* } else $($false:tt)*) => {
        $($false)*
    };
    (ifn ($($t:tt)+) { $($true:tt)* }) => {};

    (ifn () { $($true:tt)* } $(else $($false:tt)*)?) => { $($true)* };
}

#[macro_export]
#[allow(unused_macros)]
macro_rules! unref {
    (@start $($t:tt)*) => {
        $crate::unref!(@final $($t)*)
    };
    (@final &'arena $t:ty) => {
        $t
    };
    (@final $t:ty) => {
        compile_error!("Allocated queries have to return &'arena _")
    };
}

#[macro_export]
macro_rules! define_modules {
    ($($allocated_ty:ty)?;$($uncached:ident)?;$($($ctx_name:ident)?:$key_name:ident ($($desc_args:tt)+))?;$($manually_allocated:ident)?;$name:ident;$key:ty;$($value:tt)+) => {
        #[allow(unused, dead_code)]
        pub mod $name {
            use super::*;
            use $crate::queries::{QueryKey, QuerySystem, QueryCache};
            use $crate::TypeckCtx;
            $crate::macro_if!(if($($allocated_ty)?) { use mira_spans::TypeArena; });

            pub type Key<'arena> = $key;
            pub type Value<'arena> = $($value)+;
            pub type Arena<'arena> = $crate::macro_if!(if ($($allocated_ty)?) { TypeArena<$($allocated_ty)?> } else ());
            pub type ReturnTy<'arena> = $crate::macro_if!(if ($($manually_allocated)?)
                { Value<'arena> }
                else $crate::macro_if!(if ($($allocated_ty)?)
                    { $crate::unref!(@start $($value)+) }
                    else Value<'arena>
                )
            );

            pub type Cache<'arena> = $crate::macro_if!(if ($($uncached)?)
                { () }
                else <Key<'arena> as QueryKey>::Cache<Value<'arena>>
            );

            $crate::macro_if!(if($($manually_allocated)?)
                { pub type Fn<'arena> = fn(&TypeckCtx<'arena>, Key<'arena>, &'arena Arena<'arena>) -> ReturnTy<'arena>; }
                else pub type Fn<'arena> = fn(&TypeckCtx<'arena>, Key<'arena>) -> ReturnTy<'arena>;
            );
            $crate::macro_if!(if($($manually_allocated)?)
                { pub fn default_fn<'arena>(_: &TypeckCtx<'arena>, _: Key<'arena>, _: &'arena Arena<'arena>) -> ReturnTy<'arena> {
                        panic!(concat!("No provider was registered for query `", stringify!($name), "`"));
                } }
                else pub fn default_fn<'arena>(_: &TypeckCtx<'arena>, _: Key<'arena>) -> ReturnTy<'arena> {
                    panic!(concat!("No provider was registered for query `", stringify!($name), "`"));
                }
            );

            $crate::macro_if!{ ifn ($($manually_allocated)?) {
                $crate::macro_if!{ if ($($allocated_ty)?)
                    { fn alloc<'arena>(arena: &'arena Arena<'arena>, value: ReturnTy<'arena>) -> &'arena ReturnTy<'arena> {
                        arena.allocate(value)
                    } }
                    else fn alloc<'arena>(_: &'arena (), value: ReturnTy<'arena>) -> ReturnTy<'arena> { value }
                }
            } }
            pub(crate) fn run<'arena>(system: &'arena QuerySystem<'arena>, ctx: &TypeckCtx<'arena>, key: Key<'arena>) -> Value<'arena> {
                $crate::macro_if!(ifn ($($uncached)?) {
                    if let Some(v) = QueryCache::get(&system.caches.$name, &key) {
                        return v;
                    }
                });

                if !system.states.$name.write().insert(key) {
                    panic!("cycle detected while computing {}", description(ctx, key));
                }

                let res = $crate::macro_if!(if ($($manually_allocated)?)
                    { (system.providers.$name)(ctx, key, &system.arenas.$name) }
                    else alloc(&system.arenas.$name, (system.providers.$name)(ctx, key))
                );

                $crate::macro_if!(ifn($($uncached)?) { QueryCache::provide(&system.caches.$name, key, res) });
                system.states.$name.write().remove(&key);
                res
            }

            $crate::macro_if!{if($($($desc_args)*)?) {
                    pub fn description(ctx: &TypeckCtx, $($key_name)?: Key) -> String {
                        $($(let $ctx_name = ctx;)?)?
                        $(format!($($desc_args)+))?
                    }
                }
                else pub fn description(_: &TypeckCtx, _: Key) -> String {
                    format!(stringify!($name))
                }
            }
        }
    };
}

#[macro_export]
macro_rules! define_system {
    ($($name:ident)+) => {
        #[allow(unused, dead_code)]
        #[derive(Clone, Copy)]
        pub struct Providers<'arena> {
            $(pub $name: queries::$name::Fn<'arena>),+
        }
        impl Providers<'_> {
            pub const NEW: Self = Self { $($name: queries::$name::default_fn),+ };
        }

        #[allow(unused, dead_code)]
        #[derive(Default)]
        pub struct QueryStates<'arena> {
            $(pub(crate) $name: parking_lot::RwLock<std::collections::HashSet<queries::$name::Key<'arena>>>,)+
            _marker: std::marker::PhantomData<&'arena ()>,
        }

        #[allow(unused, dead_code)]
        #[derive(Default)]
        pub struct QueryArenas<'arena> {
            $(pub(crate) $name: queries::$name::Arena<'arena>,)+
            _marker: std::marker::PhantomData<&'arena ()>,
        }

        #[allow(unused, dead_code)]
        #[derive(Default)]
        pub struct QueryCaches<'arena> {
            $(pub(crate) $name: queries::$name::Cache<'arena>,)+
            _marker: std::marker::PhantomData<&'arena ()>,
        }
        pub struct QuerySystem<'arena> {
            pub(crate) providers: Providers<'arena>,
            pub(crate) arenas: QueryArenas<'arena>,
            pub(crate) caches: QueryCaches<'arena>,
            pub(crate) states: QueryStates<'arena>,
        }
        impl<'arena> $crate::TypeckCtx<'arena> {
            $(pub fn $name(&self, key: queries::$name::Key<'arena>) -> queries::$name::Value<'arena> { queries::$name::run(self.query_system(), self, key) })*
        }
    };
}

struct NeedsSendSync<T: Send + Sync>(PhantomData<T>);

const _: NeedsSendSync<QuerySystem> = NeedsSendSync(PhantomData);

impl<'arena> TypeckCtx<'arena> {
    pub fn query_system(&self) -> &'arena QuerySystem<'arena> {
        self.ctx.query_system()
    }
}
