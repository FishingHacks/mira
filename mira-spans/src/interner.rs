use std::fmt::{Debug, Display, Write};

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

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct Symbol<'arena>(ByteSymbol<'arena>);

impl<'arena> Symbol<'arena> {
    pub const EMPTY: Symbol<'static> = symbols::EMPTY_SYM;

    pub const fn to_str(self) -> &'arena str {
        unsafe { str::from_utf8_unchecked(self.0.0) }
    }

    pub const fn to_byte_sym(self) -> ByteSymbol<'arena> {
        self.0
    }
}

impl<'arena> std::ops::Deref for Symbol<'arena> {
    type Target = &'arena str;

    fn deref(&self) -> &Self::Target {
        unsafe { std::mem::transmute::<&&'arena [u8], &&'arena str>(&self.0.0) }
    }
}

impl Debug for Symbol<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self.to_str(), f)
    }
}

impl Display for Symbol<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.to_str(), f)
    }
}

// interner!(
//     SymbolInterner,
//     Symbol,
//     str,
//     |arena, s| arena.alloc_str(s),
//     symbols::ALL_SYMBOLS
// );
// extra_traits!(for Symbol impl debug, display);

interner!(
    ByteSymbolInterner,
    ByteSymbol,
    [u8],
    |arena, bstr| arena.alloc_slice(bstr),
    symbols::ALL_SYMBOLS
);

impl<'arena> ByteSymbolInterner<'arena> {
    pub fn intern_str(&mut self, s: impl AsRef<str>) -> Symbol<'arena> {
        let v = self.intern(s.as_ref().as_bytes());
        Symbol(v)
    }
}

impl Debug for ByteSymbol<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // b"ASDF\x00" will be printed as c"ASDF", and c"ASDF\0" will be printed as c"ASDF\0".
        let bytes = if self.0.last().copied() == Some(0) {
            f.write_str("c\"")?;
            &self.0[..self.0.len() - 1]
        } else {
            f.write_str("b\"")?;
            self.0
        };
        for c in bytes.utf8_chunks() {
            Display::fmt(&c.valid().escape_default(), f)?;
            for b in c.invalid() {
                if *b == 0 {
                    f.write_str("\\0")?;
                } else {
                    f.write_fmt(format_args!("\\x{b:02x}"))?;
                }
            }
        }
        f.write_char('"')
    }
}

impl Display for ByteSymbol<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // b"ASDF\x00" will be printed as "ASDF, and ASDF\0 will be printed as ASDF\0.
        let bytes = if self.0.last().copied() == Some(0) {
            &self.0[..self.0.len() - 1]
        } else {
            self.0
        };
        for c in bytes.utf8_chunks() {
            Display::fmt(&c.valid().escape_default(), f)?;
            if !c.invalid().is_empty() {
                f.write_char(std::char::REPLACEMENT_CHARACTER)?;
            }
        }
        f.write_char('"')
    }
}

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

macro_rules! symbols {
    ($( $category:ident { $($sym:ident $(= $value:expr)?),* $(,)? } ),* $(,)?) => {
        pub mod symbols {
        use super::{Symbol, ByteSymbol};
        pub static EMPTY_SYM: Symbol<'static> = Symbol(ByteSymbol(b""));

        $(#[allow(non_upper_case_globals, non_snake_case)] pub mod $category {
            use super::{Symbol, ByteSymbol};
            $(pub static $sym: Symbol<'static> = Symbol(ByteSymbol(symbols!(value $sym $(= $value)?).as_bytes()));)*
        })*

        pub(super) static ALL_SYMBOLS: &[&ByteSymbol<'static>] = &[&EMPTY_SYM.0, $($(&$category::$sym.0),*),*];
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
        <*const str>::addr(v.to_str())
    }

    #[test]
    fn interner_interns() {
        let arena = Arena::new();
        let mut interner = ByteSymbolInterner::new(&arena);
        println!(
            "u8 is {:p}, but supposed to be {:p}",
            *interner.values.get("u8".as_bytes()).unwrap(),
            symbols::Types::u8.to_str()
        );
        for symbol in symbols::ALL_SYMBOLS {
            println!("all symbols {} is {:p}", **symbol, symbol.0);
        }
        println!(
            "pointer for empty symbol is {:p}",
            symbols::EMPTY_SYM.to_str()
        );
        let abcd1 = interner.intern_str("abcd");
        let abcd2 = interner.intern_str("abcd");
        let meow1 = interner.intern_str("meow");
        let purr1 = interner.intern_str("purr");
        let abcd3 = interner.intern_str("abcd");
        let meow2 = interner.intern_str("meow");
        let purr2 = interner.intern_str("purr");

        let u8 = interner.intern_str("u8");
        let u16 = interner.intern_str("u16");
        let u32 = interner.intern_str("u32");

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
