use std::{cell::Cell, fmt::Display};

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
#[derive(Debug, Clone, Copy)]
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
