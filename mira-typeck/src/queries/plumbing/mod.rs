mod cache;
mod key;
use std::{cell::Cell, marker::PhantomData, sync::Arc, thread::ThreadId};

pub use cache::{DefaultCache, QueryCache, SingleCache};
pub use key::QueryKey;
use mira_errors::{Diagnostic, ErrorData, FatalError, Severity};
use parking_lot::{Condvar, Mutex, lock_api::RwLock};

use crate::{TypeCtx, TypeckCtx};

use super::{Providers, QueryArenas, QueryCaches, QueryStates, QuerySystem};

impl<'arena> QuerySystem<'arena> {
    pub fn new() -> Self {
        Self {
            jobs: RwLock::default(),
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

impl Default for QuerySystem<'_> {
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

                let mut states_writer = system.states.$name.write();
                let res = states_writer.get(&key).copied();
                if let Some(job_id) = res {
                    let reader = system.jobs.read();
                    let res = reader.get(job_id);
                    if let Some(res) = res {
                        let job = &reader[job_id];
                        if job.thread == std::thread::current().id() {
                            drop(reader);
                            drop(states_writer);
                            system.cycle_error(ctx.ctx, job_id);
                        }
                        let waiter = job.finish_waiter();
                        drop(reader);
                        drop(states_writer);
                        waiter.wait();

                        $crate::macro_if!(ifn ($($uncached)?) {
                            return QueryCache::get(&system.caches.$name, &key).expect("waited for a job to finish, but no cache exists");
                        });
                        states_writer = system.states.$name.write();
                    }
                }

                let job = system.enter_job(description(ctx, key));
                states_writer.insert(key, job);
                drop(states_writer);

                let res = $crate::macro_if!(if ($($manually_allocated)?)
                    { (system.providers.$name)(ctx, key, &system.arenas.$name) }
                    else alloc(&system.arenas.$name, (system.providers.$name)(ctx, key))
                );

                $crate::macro_if!(ifn($($uncached)?) { QueryCache::provide(&system.caches.$name, key, res) });
                system.states.$name.write().remove(&key);
                system.exit_job(job);
                res
            }

            $crate::macro_if!{if($($($desc_args)+)?) {
                    pub fn description(ctx: &TypeckCtx, $($key_name)?: Key) -> String {
                        $($(let $ctx_name = ctx;)?)?
                        $(format!($($desc_args)+))?
                    }
                }
                else pub fn description(_: &TypeckCtx<'_>, _: Key<'_>) -> String {
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
            $(pub(crate) $name: parking_lot::RwLock<std::collections::HashMap<queries::$name::Key<'arena>, $crate::queries::plumbing::JobId>>,)+
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
            pub(crate) jobs: parking_lot::RwLock<mira_common::index::IndexStore<$crate::queries::plumbing::JobId, $crate::queries::plumbing::QueryJob<'arena>>>,
        }
        impl<'arena> $crate::TypeckCtx<'arena> {
            $(pub fn $name(&self, key: queries::$name::Key<'arena>) -> queries::$name::Value<'arena> { queries::$name::run(self.query_system(), self, key) })+
        }
    };
}

mira_common::newty! {
    #[display("{}")]
    pub(crate) struct JobId {}
}

struct NeedsSendSync<T: Send + Sync>(PhantomData<T>);

const _: NeedsSendSync<QuerySystem<'_>> = NeedsSendSync(PhantomData);

impl<'arena> TypeckCtx<'arena> {
    pub fn query_system(&self) -> &'arena QuerySystem<'arena> {
        self.ctx.query_system()
    }
}

#[derive(Clone)]
pub(crate) struct FinishWaiter(Arc<(Mutex<bool>, Condvar)>);

impl FinishWaiter {
    pub(crate) fn new() -> Self {
        Self(Arc::new((Mutex::new(false), Condvar::new())))
    }

    pub(crate) fn wait(&self) {
        let mut finished = self.0.0.lock();
        if !*finished {
            self.0.1.wait(&mut finished);
        }
    }
}

pub(crate) struct QueryJob<'arena> {
    desc: Box<str>,
    parent: Option<JobId>,
    pub(crate) thread: ThreadId,
    result: FinishWaiter,
    _marker: PhantomData<&'arena ()>,
}

impl QueryJob<'_> {
    pub(crate) fn new(desc: Box<str>, parent: Option<JobId>) -> Self {
        Self {
            desc,
            parent,
            _marker: PhantomData,
            thread: std::thread::current().id(),
            result: FinishWaiter::new(),
        }
    }

    pub(crate) fn finish(&self) {
        *self.result.0.0.lock() = true;
        self.result.0.1.notify_all();
    }

    pub(crate) fn finish_waiter(&self) -> FinishWaiter {
        assert_ne!(self.thread, std::thread::current().id());
        self.result.clone()
    }
}

thread_local! {
    static CURRENT_JOB: Cell<Option<JobId>> = const { Cell::new(None) };
}

impl<'arena> QuerySystem<'arena> {
    pub(crate) fn enter_job(&self, desc: impl Into<Box<str>>) -> JobId {
        let mut jobs = self.jobs.write();
        let id = jobs.reserve_id();
        jobs.insert_reserved(QueryJob::new(desc.into(), CURRENT_JOB.get()), id);
        CURRENT_JOB.set(Some(id));
        id
    }

    pub(crate) fn exit_job(&self, id: JobId) {
        debug_assert_eq!(CURRENT_JOB.get(), Some(id));
        _ = id;
        let Some(job) = self.jobs.write().remove(id) else {
            panic!("Error: Exited non-existing job {id}")
        };
        job.finish();
        CURRENT_JOB.set(job.parent);
    }

    pub(crate) fn cycle_error(&self, ctx: TypeCtx<'arena>, job: JobId) -> ! {
        let jobs = self.jobs.read();
        let main_query_desc = jobs[job].desc.clone();

        let Some(mut current_job) = CURRENT_JOB.get() else {
            unreachable!("cycle_error: Supposedly no job running currently");
        };
        let mut descs = Vec::new();
        while current_job != job {
            descs.push(jobs[current_job].desc.clone());
            current_job = jobs[current_job]
                .parent
                .expect("cycle_error: job without parent is part of the cycle");
        }

        descs.reverse();

        let err = CyclicQuery {
            descs,
            main_query_desc,
        };
        _ = ctx.emit_diag(Diagnostic::new(err, Severity::Error));
        FatalError.raise()
    }
}

struct CyclicQuery {
    descs: Vec<Box<str>>,
    main_query_desc: Box<str>,
}

impl ErrorData for CyclicQuery {
    fn message<'ctx>(
        &'ctx self,
        _: mira_errors::FormattingCtx<'ctx>,
        cb: &mut dyn FnMut(std::fmt::Arguments<'_>) -> std::fmt::Result,
    ) -> std::fmt::Result {
        cb(format_args!("Cycle detected when {}", self.main_query_desc))
    }

    fn notes<'ctx>(
        &'ctx self,
        _: mira_errors::FormattingCtx<'ctx>,
        cb: &mut dyn FnMut(std::fmt::Arguments<'_>) -> std::fmt::Result,
    ) -> std::fmt::Result {
        for desc in &self.descs {
            cb(format_args!("...which requires {desc}"))?;
        }
        if self.descs.is_empty() {
            cb(format_args!(
                "...which then immediately requires {}, completing the cycle.",
                self.main_query_desc
            ))
        } else {
            cb(format_args!(
                "...which then again requires {}, completing the cycle.",
                self.main_query_desc
            ))
        }
    }
}
