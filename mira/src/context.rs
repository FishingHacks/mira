use std::fmt::Debug;
use std::sync::Arc;

use parking_lot::Mutex;

use crate::{
    arena::Arena,
    string_interner::{InternedStr, StringInterner},
};

pub struct GlobalContext<'arena> {
    string_interner: StringInterner<'arena>,
}

impl Debug for GlobalContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GlobalContext").finish()
    }
}

impl<'arena> GlobalContext<'arena> {
    pub fn new(arena: &'arena Arena) -> Self {
        Self {
            string_interner: StringInterner::new(arena),
        }
    }

    pub fn share(self) -> SharedContext<'arena> {
        SharedContext::new(self)
    }
}

#[derive(Clone, Debug)]
pub struct SharedContext<'arena>(Arc<Mutex<GlobalContext<'arena>>>);

impl<'arena> SharedContext<'arena> {
    pub fn new(ctx: GlobalContext<'arena>) -> Self {
        Self(Arc::new(ctx.into()))
    }

    pub fn intern_str(&self, s: &str) -> InternedStr<'arena> {
        self.0.lock().string_interner.intern(s)
    }
}
