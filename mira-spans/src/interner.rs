use std::{
    borrow::Borrow,
    collections::HashSet,
    fmt::{Debug, Display},
    hash::Hash,
    ops::Deref,
};

use parking_lot::Mutex;

use crate::{arena::Arena, span::SpanData};

macro_rules! interner {
    ($interner: ident, $internee: ident, $ty: ty, |$arena:ident, $value:ident| $blk: expr $(, $default_values:expr)?) => {
        #[derive(Clone, Copy, PartialEq, Eq)]
        pub struct $internee<'arena>(&'arena $ty);

        impl Hash for $internee<'_> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.0.hash(state);
            }
        }

        impl<'arena> Deref for $internee<'arena> {
            type Target = &'arena $ty;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl<'arena> Borrow<&'arena $ty> for $internee<'arena> {
            fn borrow(&self) -> &&'arena $ty {
                &self.0
            }
        }

        impl<T> PartialEq<T> for $internee<'_>
        where
            $ty: PartialEq<T>,
        {
            fn eq(&self, other: &T) -> bool {
                self.0.eq(other)
            }
        }

        pub struct $interner<'arena> {
            arena: &'arena Arena,
            values: HashSet<&'arena $ty>,
        }

        $(const _: () = $interner::check_default($default_values);)?

        impl<'arena> $interner<'arena> {
            #[allow(unused, dead_code)]
            const fn check_default(_: &[$internee<'arena>]) {}

            pub fn new(arena: &'arena Arena) -> Self {
                Self::new_with(arena, &[])
            }

            pub fn new_with(arena: &'arena Arena, custom_values: &[&'arena $ty]) -> Self {
                let mut values = HashSet::new();
                for v in custom_values {
                    values.insert(*v);
                }
                $(for v in $default_values {
                    values.insert(v.0);
                })?
                Self { arena, values }
            }

            pub fn intern(&mut self, $value: impl AsRef<$ty>) -> $internee<'arena> {
                let $value = $value.as_ref();
                if let Some(v) = self.values.get($value) {
                    return $internee(*v);
                }
                let $arena = self.arena;
                let $value = $blk;
                assert!(self.values.insert($value));
                $internee($value)
            }
        }
    };
}
macro_rules! extra_traits {
    (for $internee:ident impl $($trait:ident),* $(,)?) => {
        $(extra_traits!(internal $trait $internee);)*
    };
    (internal debug $internee:ident) => {
        impl Debug for $internee<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                Debug::fmt(self.0, f)
            }
        }
    };
    (internal display $internee:ident) => {
        impl Display for $internee<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                Display::fmt(self.0, f)
            }
        }
    };
}

interner!(
    SymbolInterner,
    Symbol,
    str,
    |arena, s| arena.alloc_str(s),
    symbols::ALL_SYMBOLS
);
extra_traits!(for Symbol impl debug, display);

mod span {
    use crate::arena::Arena;
    use crate::span::SpanData;
    use std::collections::HashSet;
    use std::{borrow::Borrow, fmt::Debug, hash::Hash, ops::Deref};

    interner!(InnerSpanInterner, InternedSpan, SpanData, |arena, s| {
        arena.alloc(*s)
    });
    extra_traits!(for InternedSpan impl debug);
}
use span::InnerSpanInterner;
pub use span::InternedSpan;

pub struct SpanInterner<'arena>(Mutex<InnerSpanInterner<'arena>>);
impl<'arena> SpanInterner<'arena> {
    pub fn new(arena: &'arena Arena) -> Self {
        Self(Mutex::new(InnerSpanInterner::new(arena)))
    }

    pub fn intern(&self, span: SpanData) -> InternedSpan<'arena> {
        self.0.lock().intern(span)
    }
}

impl<'arena> Symbol<'arena> {
    pub const EMPTY: Symbol<'static> = symbols::EMPTY_SYM;

    pub fn to_str(self) -> &'arena str {
        self.0
    }
}

macro_rules! symbols {
    ($( $category:ident { $($sym:ident $(= $value:expr)?),* $(,)? } ),* $(,)?) => {
        pub mod symbols {
        use super::Symbol;
        pub const EMPTY_SYM: Symbol<'static> = Symbol("");

        $(#[allow(non_upper_case_globals, non_snake_case)] pub mod $category {
            use super::Symbol;
            $(pub const $sym: Symbol<'static> = Symbol(symbols!(value $sym $(= $value)?));)*
        })*

        pub(super) const ALL_SYMBOLS: &[Symbol<'static>] = &[EMPTY_SYM, $($($category::$sym),*),*];
        }
    };

    (value $sym:ident) => {
        stringify!($sym)
    };
    (value $sym:ident = $value:expr) => {
        $value
    };
}

symbols!(
    Types { u8, u16, u32, u64, usize, i8, i16, i32, i64, isize, bool, NeverType = "!" },
    Keywords { If, While, For, Pub, As, Else, Asm, Volatile, Impl, Fn, In, Unsized, Struct, Trait },
);
