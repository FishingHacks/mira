use std::{cell::Cell, fmt::{Debug, Display, Write}, hash::Hash};

fn with_global_strs<Fn, R>(f: Fn) -> R
where
    Fn: FnOnce(&mut Vec<Box<str>>) -> R,
{
    thread_local!(static GLOBAL_STRS: Cell<Vec<Box<str>>> = Cell::new(Vec::new())); // SAFETY: Only use single-threaded, without interrupts. Also never call .remove(), .clear() or any other operation that changes the vector in a way thats not appending a value/leaving all values that already persist at the same indices and unchanged. Also, dont give mutable references to strings inside this vector or this vector
    let mut strings = GLOBAL_STRS.take();
    let val = f(&mut strings);
    GLOBAL_STRS.set(strings);
    val
}

#[repr(transparent)]
#[derive(Clone, Copy, Eq)]
pub struct GlobalString(usize);

impl Display for GlobalString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        with_global_strs(|strings| match strings.get(self.0) {
            Some(v) => f.write_str(v),
            None => f.write_fmt(format_args!(
                "[internal: Could not get GlobalString #{}]",
                self.0
            )),
        })
    }
}

impl Debug for GlobalString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("GlobalString#")?;
        Debug::fmt(&self.0, f)?;
        f.write_char('(')?;
        with_global_strs(|strings| match strings.get(self.0) {
            Some(v) => Debug::fmt(&**v, f),
            None => f.write_str("not found"),
        })?;
        f.write_char(')')
    }
}

impl From<&str> for GlobalString {
    fn from(value: &str) -> Self {
        with_global_strs(|strings| {
            let index = match strings.iter().position(|v| &**v == value) {
                Some(v) => v,
                _ => {
                    strings.push(value.to_string().into_boxed_str());
                    strings.len() - 1
                }
            };
            Self(index)
        })
    }
}

impl From<String> for GlobalString {
    fn from(value: String) -> Self {
        with_global_strs(|strings| {
            let index = match strings.iter().position(|v| &**v == value) {
                Some(v) => v,
                _ => {
                    strings.push(value.into_boxed_str()); // <- this is fine because its not leaking a mutable pointer and just appends to the vector.
                    strings.len() - 1
                }
            };
            Self(index)
        })
    }
}

impl PartialEq for GlobalString {
    fn eq(&self, other: &Self) -> bool {
        if self.0 == other.0 { return true; }
        with_global_strs(|strings| {
            if let Some(self_str) = strings.get(self.0) {
                if let Some(other_str) = strings.get(other.0) {
                    return self_str == other_str;
                }
            }
            return false;
        })
    }
}

impl PartialOrd for GlobalString {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.0 == other.0 { return Some(std::cmp::Ordering::Equal); }
        with_global_strs(|strings| {
            if let Some(self_str) = strings.get(self.0) {
                if let Some(other_str) = strings.get(other.0) {
                    return self_str.partial_cmp(other_str);
                }
            }
            return None;
        })
    }
}

impl Hash for GlobalString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        with_global_strs(|strings| {
            match strings.get(self.0) {
                None => panic!("GlobalString#{} is not present but expected to be hashable", self.0),
                Some(c) => c.hash(state),
            }
        })
    }
}