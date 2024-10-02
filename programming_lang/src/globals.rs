use std::boxed::Box;
use std::cell::RefCell;
use std::fmt::{Debug, Display, Write};
use std::hash::Hash;

struct GlobalValue<T> {
    refs: usize,
    value: T,
}

struct Slab<T> {
    entries: Vec<Option<T>>,
    last_free: usize,
}

impl<T> Slab<T> {
    pub const fn new() -> Self {
        Self {
            entries: Vec::new(),
            last_free: 0,
        }
    }

    pub fn push(&mut self, value: T) -> usize {
        if self.last_free >= self.entries.len() {
            self.entries.push(Some(value));
            self.last_free = self.entries.len();
            self.entries.len() - 1
        } else {
            self.entries[self.last_free] = Some(value);
            let entry = self.last_free;
            for i in (self.last_free + 1)..self.entries.len() {
                if self.entries[i].is_none() {
                    self.last_free = i;
                    return entry;
                }
            }
            self.last_free = self.entries.len();
            return entry;
        }
    }

    /// trys to remove a value from the slab and returns it if it found one
    pub fn remove(&mut self, index: usize) -> Option<T> {
        if index >= self.entries.len() {
            None
        } else if index < self.last_free {
            self.last_free = index;
            self.entries[index].take()
        } else {
            self.entries[index].take()
        }
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        self.entries.get(index).map(Option::as_ref).flatten()
    }
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.entries.get_mut(index).map(Option::as_mut).flatten()
    }
}

type GlobalStrs = Slab<GlobalValue<Box<str>>>;
//type GlobalPaths = Slab<GlobalValue<Box<Path>>>;

thread_local! {
    static STRINGS: RefCell<GlobalStrs> = RefCell::new(Slab::new());
    //static PATHS: RefCell<GlobalPaths> = RefCell::new(Slab::new());
}

#[derive(Eq)]
pub struct GlobalStr(usize);

impl PartialEq for GlobalStr {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl PartialEq<str> for GlobalStr {
    fn eq(&self, other: &str) -> bool {
        STRINGS.with_borrow(|strings: &GlobalStrs| {
            if let Some(v) = strings.get(self.0) {
                (*v.value).eq(other)
            } else {
                false
            }
        })
    }
}

impl Clone for GlobalStr {
    fn clone(&self) -> Self {
        STRINGS.with_borrow_mut(|strings: &mut GlobalStrs| {
            if let Some(v) = strings.get_mut(self.0) {
                v.refs += 1;
            } else {
                panic!("tried to clone dropped GlobalStr");
            }
        });
        Self(self.0)
    }
}

impl Drop for GlobalStr {
    fn drop(&mut self) {
        STRINGS.with_borrow_mut(|strings: &mut GlobalStrs| {
            if let Some(v) = strings.get_mut(self.0) {
                if v.refs == 0 {
                    strings.remove(self.0);
                } else {
                    v.refs -= 1;
                }
            }
        });
    }
}

impl GlobalStr {
    pub fn new(value: &str) -> Self {
        STRINGS.with_borrow_mut(|strings: &mut GlobalStrs| {
            for (idx, v) in strings.entries.iter_mut().enumerate() {
                if let Some(v) = v {
                    if (*v.value).eq(value) {
                        v.refs += 1;
                        return Self(idx);
                    }
                }
            }
            Self(strings.push(GlobalValue {
                refs: 0,
                value: value.into(),
            }))
        })
    }

    pub fn new_boxed(value: Box<str>) -> Self {
        STRINGS.with_borrow_mut(|strings: &mut GlobalStrs| {
            for (idx, v) in strings.entries.iter_mut().enumerate() {
                if let Some(v) = v {
                    if (*v.value).eq(&*value) {
                        v.refs += 1;
                        return Self(idx);
                    }
                }
            }
            Self(strings.push(GlobalValue { refs: 0, value }))
        })
    }

    pub fn with<T>(&self, func: impl Fn(&str) -> T) -> T {
        STRINGS.with_borrow(|strings: &GlobalStrs| {
            if let Some(v) = strings.get(self.0) {
                func(&v.value)
            } else {
                func("")
            }
        })
    }
}

impl Debug for GlobalStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        STRINGS.with_borrow(|strings: &GlobalStrs| {
            if let Some(v) = strings.get(self.0) {
                Debug::fmt(&*v.value, f)
            } else {
                f.write_str("GlobalStr<missing #")?;
                Display::fmt(&self.0, f)?;
                f.write_char('>')
            }
        })
    }
}

impl Display for GlobalStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        STRINGS.with_borrow(|strings: &GlobalStrs| {
            if let Some(v) = strings.get(self.0) {
                f.write_str(&*v.value)
            } else {
                f.write_str("GlobalStr<missing #")?;
                Display::fmt(&self.0, f)?;
                f.write_char('>')
            }
        })
    }
}

impl Hash for GlobalStr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        STRINGS.with_borrow(|strings: &GlobalStrs| {
            if let Some(v) = strings.get(self.0) {
                v.value.hash(state);
            } else {
                "".hash(state)
            }
        })
    }
}

//#[derive(Eq)]
//pub struct GlobalPath(usize);
//
//impl PartialEq for GlobalPath {
//    fn eq(&self, other: &Self) -> bool {
//        self.0 == other.0
//    }
//}
//
//impl PartialEq<Path> for GlobalPath {
//    fn eq(&self, other: &Path) -> bool {
//        PATHS.with_borrow(|paths: &GlobalPaths| {
//            if let Some(v) = paths.get(self.0) {
//                (*v.value).eq(other)
//            } else {
//                false
//            }
//        })
//    }
//}
//
//impl Clone for GlobalPath {
//    fn clone(&self) -> Self {
//        PATHS.with_borrow_mut(|paths: &mut GlobalPaths| {
//            if let Some(v) = paths.get_mut(self.0) {
//                v.refs += 1;
//            } else {
//                panic!("tried to clone dropped GlobalStr");
//            }
//        });
//        Self(self.0)
//    }
//}
//
//impl Drop for GlobalPath {
//    fn drop(&mut self) {
//        PATHS.with_borrow_mut(|paths: &mut GlobalPaths| {
//            if let Some(v) = paths.get_mut(self.0) {
//                if v.refs == 0 {
//                    paths.remove(self.0);
//                } else {
//                    v.refs -= 1;
//                }
//            }
//        });
//    }
//}
//
//impl GlobalPath {
//    pub fn new(value: &Path) -> Self {
//        PATHS.with_borrow_mut(|paths: &mut GlobalPaths| {
//            for (idx, v) in paths.entries.iter_mut().enumerate() {
//                if let Some(v) = v {
//                    if (*v.value).eq(value) {
//                        v.refs += 1;
//                        return Self(idx);
//                    }
//                }
//            }
//            Self(paths.push(GlobalValue {
//                refs: 0,
//                value: value.into(),
//            }))
//        })
//    }
//
//    pub fn new_boxed(value: Box<Path>) -> Self {
//        PATHS.with_borrow_mut(|paths: &mut GlobalPaths| {
//            for (idx, v) in paths.entries.iter_mut().enumerate() {
//                if let Some(v) = v {
//                    if (*v.value).eq(&*value) {
//                        v.refs += 1;
//                        return Self(idx);
//                    }
//                }
//            }
//            Self(paths.push(GlobalValue { refs: 0, value }))
//        })
//    }
//}
//
//impl Display for GlobalPath {
//    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//        PATHS.with_borrow(|paths: &GlobalPaths| {
//            if let Some(v) = paths.get(self.0) {
//                Display::fmt(&v.value.display(), f)
//            } else {
//                f.write_str("GlobalPath<missing #")?;
//                Display::fmt(&self.0, f)?;
//                f.write_char('>')
//            }
//        })
//    }
//}
//
//impl Debug for GlobalPath {
//    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//        PATHS.with_borrow(|paths: &GlobalPaths| {
//            if let Some(v) = paths.get(self.0) {
//                Debug::fmt(&v.value, f)
//            } else {
//                f.write_str("GlobalPath<missing #")?;
//                Display::fmt(&self.0, f)?;
//                f.write_char('>')
//            }
//        })
//    }
//}
