use parking_lot::RwLock;

use crate::slab::Slab;
use std::boxed::Box;
use std::fmt::{Debug, Display, Write};
use std::hash::Hash;
use std::sync::LazyLock;

#[derive(Debug)]
struct GlobalValue<T: Debug> {
    refs: usize,
    value: T,
}

type GlobalStrs = Slab<GlobalValue<Box<str>>>;

//thread_local! {
static STRINGS: LazyLock<RwLock<GlobalStrs>> = LazyLock::new(|| {
    let mut slab = Slab::new();
    slab.push(GlobalValue {
        refs: 0,
        value: "".into(),
    });
    RwLock::new(slab)
});
//static STRINGS: RefCell<GlobalStrs> = RefCell::new({
//    let mut slab = Slab::new();
//    slab.push(GlobalValue {
//        refs: usize::MAX,
//        value: "".to_string().into_boxed_str(),
//    });
//    slab
//});
//}

#[derive(Eq)]
pub struct GlobalStr(usize);

impl PartialEq for GlobalStr {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl PartialEq<&str> for GlobalStr {
    fn eq(&self, other: &&str) -> bool {
        let strings = STRINGS.read();

        if let Some(v) = strings.get(self.0) {
            (*v.value).eq(*other)
        } else {
            false
        }
    }
}

impl Clone for GlobalStr {
    fn clone(&self) -> Self {
        if self.0 == 0 {
            return Self(self.0);
        }
        let mut strings = STRINGS.write();
        if let Some(v) = strings.get_mut(self.0) {
            v.refs += 1;
        } else {
            panic!("tried to clone dropped GlobalStr {}", self.0);
        }
        Self(self.0)
    }
}

impl Drop for GlobalStr {
    fn drop(&mut self) {
        if self.0 == 0 {
            return;
        }
        let mut strings = STRINGS.write();
        if let Some(v) = strings.get_mut(self.0) {
            if v.refs == 0 {
                strings.remove(self.0);
                #[cfg(debug_assertions)]
                if strings.get(self.0).is_some() {
                    panic!("slab broke {strings:?} {}", self.0);
                }
            } else {
                v.refs -= 1;
            }
        }
    }
}

impl GlobalStr {
    pub const ZERO: GlobalStr = Self(0);

    pub fn new(value: &str) -> Self {
        let mut strings = STRINGS.write();
        for (idx, v) in strings.iter_mut() {
            if (*v.value).eq(value) {
                if idx == 0 {
                    return Self(0);
                }
                v.refs += 1;
                return Self(idx);
            }
        }
        Self(strings.push(GlobalValue {
            refs: 0,
            value: value.into(),
        }))
    }

    pub fn new_boxed(value: Box<str>) -> Self {
        let mut strings = STRINGS.write();
        for (idx, v) in strings.iter_mut() {
            if (*v.value).eq(&*value) {
                if idx == 0 {
                    return Self(0);
                }
                v.refs += 1;
                return Self(idx);
            }
        }
        Self(strings.push(GlobalValue { refs: 0, value }))
    }

    pub fn with<T>(&self, mut func: impl FnMut(&str) -> T) -> T {
        let strings = STRINGS.read();
        func(strings.get(self.0).map(|v| &*v.value).unwrap_or(""))
    }
}

impl From<&str> for GlobalStr {
    fn from(value: &str) -> Self {
        Self::new(value)
    }
}
impl From<Box<str>> for GlobalStr {
    fn from(value: Box<str>) -> Self {
        Self::new_boxed(value)
    }
}
impl From<String> for GlobalStr {
    fn from(value: String) -> Self {
        Self::new_boxed(value.into_boxed_str())
    }
}

impl Debug for GlobalStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let strings = STRINGS.read();

        if let Some(v) = strings.get(self.0) {
            Debug::fmt(&*v.value, f)
        } else {
            f.write_str("GlobalStr<missing #")?;
            Display::fmt(&self.0, f)?;
            f.write_char('>')
        }
    }
}

impl Display for GlobalStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let strings = STRINGS.read();

        if let Some(v) = strings.get(self.0) {
            f.write_str(&v.value)
        } else {
            f.write_str("GlobalStr<missing #")?;
            Display::fmt(&self.0, f)?;
            f.write_char('>')
        }
    }
}

impl Hash for GlobalStr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let strings = STRINGS.read();

        if let Some(v) = strings.get(self.0) {
            v.value.hash(state);
        } else {
            "".hash(state)
        }
    }
}

pub fn clean_slab() {
    STRINGS.write().clean_slab();
}

pub fn print_slab() {
    println!("{:?}", STRINGS.read());
}
