use std::{
    borrow::Borrow,
    collections::HashSet,
    fmt::{Debug, Display},
    hash::Hash,
    ops::Deref,
};

use crate::arena::Arena;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct InternedStr<'arena>(&'arena str);

impl Debug for InternedStr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self.0, f)
    }
}
impl Display for InternedStr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

impl Hash for InternedStr<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<'arena> Deref for InternedStr<'arena> {
    type Target = &'arena str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'arena> Borrow<&'arena str> for InternedStr<'arena> {
    fn borrow(&self) -> &&'arena str {
        &self.0
    }
}

impl<T> PartialEq<T> for InternedStr<'_>
where
    str: PartialEq<T>,
{
    fn eq(&self, other: &T) -> bool {
        self.to_str().eq(other)
    }
}

impl<'arena> InternedStr<'arena> {
    pub const EMPTY: InternedStr<'static> = InternedStr(EMPTY_STR);

    pub fn to_str(self) -> &'arena str {
        self.0
    }

    #[cfg(test)]
    pub fn shady_new(s: &'static str) -> Self {
        Self(s)
    }
}

pub struct StringInterner<'arena> {
    arena: &'arena Arena,
    // strings: Mutex<HashSet<&'arena str>>,
    strings: HashSet<&'arena str>,
}

impl<'arena> StringInterner<'arena> {
    pub fn new(arena: &'arena Arena) -> Self {
        Self::new_with(arena, &[])
    }

    pub fn new_with(arena: &'arena Arena, custom_strings: &[&'arena str]) -> Self {
        let mut strings = HashSet::new();
        for s in custom_strings {
            strings.insert(*s);
        }
        for s in DEFAULT_STRINGS {
            strings.insert(s);
        }
        Self { arena, strings }
    }

    pub fn intern(&mut self, s: impl AsRef<str>) -> InternedStr<'arena> {
        let s = s.as_ref();
        if let Some(s) = self.strings.get(s) {
            return InternedStr(s);
        }
        let s = self.arena.alloc_str(s);
        // assert that the string did not already exist
        assert!(self.strings.insert(s));
        InternedStr(s)
    }
}

// static strings that don't have to be allocated on the arena and can instead be taken from the
// programs data section
// TODO: fill this
const DEFAULT_STRINGS: &[&str] = &["u8", EMPTY_STR];
const EMPTY_STR: &str = "";
