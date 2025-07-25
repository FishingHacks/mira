use std::collections::HashSet;

use parking_lot::Mutex;

use crate::arena::Arena;

pub type GlobalStr<'arena> = &'arena str;

pub struct StringInterner<'arena> {
    arena: &'arena Arena,
    strings: Mutex<HashSet<&'arena str>>,
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
        let strings = Mutex::new(strings);
        Self { arena, strings }
    }

    pub fn intern(&self, s: impl AsRef<str>) -> &'arena str {
        let s = s.as_ref();
        let mut strings = self.strings.lock();
        if let Some(s) = strings.get(s) {
            return *s;
        }
        let s = self.arena.alloc_str(s);
        // assert that the string did not already exist
        assert!(strings.insert(s));
        s
    }
}

// static strings that don't have to be allocated on the arena and can instead be taken from the
// programs data section
// TODO: fill this
const DEFAULT_STRINGS: &[&str] = &["u8"];
