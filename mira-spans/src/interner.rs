use std::fmt::{Debug, Display};

use parking_lot::Mutex;

use crate::{arena::Arena, span::SpanData};

#[macro_export]
macro_rules! interner {
    ($interner: ident, $internee: ident, $ty: ty, |$arena:ident, $value:ident| $blk: expr $(, $default_values:expr)?) => {
        #[derive(Clone, Copy, Eq)]
        pub struct $internee<'arena>(&'arena $ty);

        #[allow(single_use_lifetimes, trivial_casts)]
        impl<'arena> std::hash::Hash for $internee<'arena> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                (self.0 as *const $ty).hash(state);
            }
        }

        impl<'arena> std::ops::Deref for $internee<'arena> {
            type Target = &'arena $ty;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl<'a> PartialEq<$internee<'a>> for $internee<'a> {
            fn eq(&self, other: &$internee<'a>) -> bool {
                #[cfg(debug_assertions)]
                if self.0 == other.0 && !std::ptr::eq(self.0, other.0) {
                    println!("pointer lies: {:?} {:?} {:p} {:p} (did you maybe forget to add a default value or made the default value or the default values slice `const` instead of `static`?)", self.0, other.0, self.0, other.0)
                }
                std::ptr::eq(self.0, other.0)
            }
        }

        #[allow(single_use_lifetimes, trivial_casts)]
        impl<'arena, T> PartialEq<T> for $internee<'arena>
        where
            $ty: PartialEq<T>,
        {
            fn eq(&self, other: &T) -> bool {
                self.0.eq(other)
            }
        }

        #[allow(unreachable_pub)]
        pub struct $interner<'arena> {
            arena: &'arena Arena,
            values: std::collections::HashSet<&'arena $ty>,
        }

        $(const _: () = $interner::check_default($default_values);)?

        #[allow(unreachable_pub)]
        impl<'arena> $interner<'arena> {
            #[allow(unused, dead_code)]
            const fn check_default(_: &[&'static $internee<'arena>]) {}

            pub fn new(arena: &'arena Arena) -> Self {
                Self::new_with(arena, &[])
            }

            pub fn new_with(arena: &'arena Arena, custom_values: &[&'arena $ty]) -> Self {
                let mut values = std::collections::HashSet::new();
                for v in custom_values {
                    values.insert(*v);
                }
                $(for v in $default_values {
                    let ptr: &'arena $ty = v.0;
                    values.insert(ptr);
                })?
                Self { arena, values }
            }

            #[allow(dead_code)]
            pub fn intern(&mut self, $value: impl AsRef<$ty>) -> $internee<'arena> {
                let $value = $value.as_ref();
                if let Some(v) = self.values.get($value) {
                    return $internee(*v);
                }
                let $arena = self.arena;
                let value = $blk;
                assert!(self.values.insert(value));
                $internee(value)
            }
        }
    };
}
#[macro_export]
macro_rules! owned_intern {
    ($interner:ident, $internee:ident, $ty:ty, |$arena:ident, $value:ident| $blk: expr) => {
        #[allow(unreachable_pub)]
        impl<'arena> $interner<'arena> {
            pub fn intern_owned(&mut self, $value: $ty) -> $internee<'arena> {
                if let Some(v) = self.values.get(&$value) {
                    return $internee(*v);
                }
                let $arena = self.arena;
                let value = $blk;
                assert!(self.values.insert(value));
                $internee(value)
            }
        }
    };
}
#[macro_export]
macro_rules! extra_traits {
    (for $internee:ident impl $($trait:ident),* $(,)?) => {
        $(extra_traits!(internal $trait $internee);)*
    };
    (internal debug $internee:ident) => {
        impl std::fmt::Debug for $internee<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                Debug::fmt(self.0, f)
            }
        }
    };
    (internal display $internee:ident) => {
        impl std::fmt::Display for $internee<'_> {
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
    use std::fmt::Debug;

    interner!(InnerSpanInterner, InternedSpan, SpanData, |arena, s| {
        arena.alloc(*s)
    });
    owned_intern!(InnerSpanInterner, InternedSpan, SpanData, |arena, s| arena
        .alloc(s));
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
        self.0.lock().intern_owned(span)
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
        pub static EMPTY_SYM: Symbol<'static> = Symbol("");

        $(#[allow(non_upper_case_globals, non_snake_case)] pub mod $category {
            use super::Symbol;
            $(pub static $sym: Symbol<'static> = Symbol(symbols!(value $sym $(= $value)?));)*
        })*

        pub(super) static ALL_SYMBOLS: &[&Symbol<'static>] = &[&EMPTY_SYM, $($(&$category::$sym),*),*];
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
    Types { u8, u16, u32, u64, usize, i8, i16, i32, i64, isize, bool, NeverType = "!", self_ty = "Self" },
    Keywords { If, While, For, Pub, As, Else, Asm, Volatile, Impl, Fn, In, Unsized, Struct, Trait },
    DefaultIdents { main, r#macro = "macro", hidden }
);

#[cfg(test)]
mod test {
    use super::*;

    fn addr(v: Symbol<'_>) -> usize {
        <*const str>::addr(v.0)
    }

    #[test]
    fn interner_interns() {
        let arena = Arena::new();
        let mut interner = SymbolInterner::new(&arena);
        println!(
            "u8 is {:p}, but supposed to be {:p}",
            *interner.values.get("u8").unwrap(),
            symbols::Types::u8.0
        );
        for symbol in symbols::ALL_SYMBOLS {
            println!("all symbols {} is {:p}", symbol.0, symbol.0);
        }
        println!("pointer for empty symbol is {:p}", symbols::EMPTY_SYM.0);
        let abcd1 = interner.intern("abcd");
        let abcd2 = interner.intern("abcd");
        let meow1 = interner.intern("meow");
        let purr1 = interner.intern("purr");
        let abcd3 = interner.intern("abcd");
        let meow2 = interner.intern("meow");
        let purr2 = interner.intern("purr");

        let u8 = interner.intern("u8");
        let u16 = interner.intern("u16");
        let u32 = interner.intern("u32");

        assert_eq!(abcd1, abcd2);
        assert_eq!(abcd2, abcd3);
        assert_eq!(meow1, meow2);
        assert_eq!(purr1, purr2);
        assert_eq!(u8, symbols::Types::u8);
        assert_eq!(u16, symbols::Types::u16);
        assert_eq!(u32, symbols::Types::u32);
        assert_eq!(addr(abcd1), addr(abcd2));
        assert_eq!(addr(abcd2), addr(abcd3));
        assert_eq!(addr(meow1), addr(meow2));
        assert_eq!(addr(purr1), addr(purr2));
        assert_eq!(addr(u8), addr(symbols::Types::u8));
        assert_eq!(addr(u16), addr(symbols::Types::u16));
        assert_eq!(addr(u32), addr(symbols::Types::u32));
    }
}
