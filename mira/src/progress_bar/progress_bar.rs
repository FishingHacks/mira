use std::fmt::{Debug, Write};

use super::box_drawing_characters as bdc;
use super::sized_line_writer::SizedLineWriter;
use crate::slab::Slab;
use bdc::BoxDrawingChars;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct ProgressItemRef(pub usize);

#[derive(Debug, Clone)]
struct ProgressItem {
    name: Box<str>,
    children: Vec<ProgressItemRef>,
    parent: ProgressItemRef,
}

#[derive(Debug, Clone, Default)]
pub struct ProgressBar {
    chars: BoxDrawingChars,
    items: Slab<ProgressItem>,
    root_children: Vec<ProgressItemRef>,
    last_paint_linenum: usize,
}

#[derive(Debug, Clone, Copy)]
pub enum ProgressBarStyle {
    Normal,
    Bold,
    Double,
    Round,
}

impl ProgressBarStyle {
    pub fn to_chars(self) -> BoxDrawingChars {
        match self {
            ProgressBarStyle::Normal => bdc::NORMAL,
            ProgressBarStyle::Bold => bdc::BOLD,
            ProgressBarStyle::Double => bdc::DOUBLE,
            ProgressBarStyle::Round => bdc::ROUND,
        }
    }
}

fn get_size() -> (u16, u16) {
    termsize::get()
        .map(|v| (v.cols, v.rows))
        .unwrap_or((80, 24))
}

impl ProgressBar {
    pub fn new(style: ProgressBarStyle) -> Self {
        Self {
            chars: style.to_chars(),
            ..Default::default()
        }
    }

    pub fn custom(chars: BoxDrawingChars) -> Self {
        Self {
            chars,
            ..Default::default()
        }
    }

    pub fn add_item(&mut self, name: Box<str>) -> ProgressItemRef {
        let child_ref = ProgressItemRef(self.items.push(ProgressItem {
            name,
            children: Vec::new(),
            parent: ProgressItemRef(usize::MAX),
        }));
        self.root_children.push(child_ref);
        child_ref
    }

    pub fn remove_item(&mut self, item_ref: ProgressItemRef) {
        let Some(item) = self.remove_item_inner(item_ref) else {
            return;
        };
        match self.items.get_mut(item.parent.0) {
            Some(v) => {
                let mut i = 0;
                while i < v.children.len() {
                    if v.children[i] == item_ref {
                        v.children.remove(i);
                    } else {
                        i += 1
                    }
                }
            }
            None => {
                let mut i = 0;
                while i < self.root_children.len() {
                    if self.root_children[i] == item_ref {
                        self.root_children.remove(i);
                    } else {
                        i += 1
                    }
                }
            }
        }
    }

    fn remove_item_inner(&mut self, item_ref: ProgressItemRef) -> Option<ProgressItem> {
        let item = self.items.remove(item_ref.0)?;
        for child in item.children.iter().copied() {
            self.remove_item_inner(child);
        }
        Some(item)
    }

    pub fn add_child_item(&mut self, parent: ProgressItemRef, name: Box<str>) -> ProgressItemRef {
        let child_ref = ProgressItemRef(self.items.push(ProgressItem {
            name,
            children: Vec::new(),
            parent,
        }));

        if let Some(v) = self.items.get_mut(parent.0) {
            v.children.push(child_ref)
        }
        child_ref
    }

    pub fn get_name(&self, item_ref: ProgressItemRef) -> &str {
        &self.items[item_ref.0].name
    }

    fn print_inner(
        &self,
        f: &mut SizedLineWriter,
        children: &[ProgressItemRef],
        last_item: *mut Indent,
        root_node: *const Indent,
    ) -> std::fmt::Result {
        let last_idx = children.len().saturating_sub(1);
        for (idx, value) in children.iter().enumerate() {
            let Some(child) = self.items.get(value.0) else {
                continue;
            };
            for ty in IndentIter(root_node) {
                match ty {
                    IndentType::Space => f.write_char(' ')?,
                    IndentType::Line => f.write_char(self.chars.vertical)?,
                    IndentType::None => continue,
                }
                f.write_char(' ')?;
            }
            let indent_ty = if idx == last_idx {
                f.write_char(self.chars.bottom_left)?;
                f.write_char(' ')?;
                f.write_str(&child.name)?;
                f.write_char('\n')?;
                IndentType::Space
            } else {
                f.write_char(self.chars.vertical_right)?;
                f.write_char(' ')?;
                f.write_str(&child.name)?;
                f.write_char('\n')?;
                IndentType::Line
            };
            let mut new_last_indent = Indent::new(indent_ty);
            assert!(!last_item.is_null());
            unsafe { (*last_item).1 = &new_last_indent }

            let v = self.print_inner(f, &child.children, &mut new_last_indent, root_node);
            unsafe { (*last_item).1 = std::ptr::null() }
            v?
        }
        Ok(())
    }

    pub fn clean(&mut self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("\x1b[G\x1b[K")?;
        for _ in 0..self.last_paint_linenum {
            f.write_str("\x1b[A\x1b[K")?;
        }
        self.last_paint_linenum = 0;
        Ok(())
    }

    pub fn display(&mut self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.clean(f)?;
        let mut f = SizedLineWriter::new(get_size().0 as usize, f);
        f.write_str("root\n")?;
        let mut root_node = Indent::new(IndentType::None);
        match self.print_inner(&mut f, &self.root_children, &mut root_node, &root_node) {
            v @ Ok(_) | v @ Err(_) => {
                self.last_paint_linenum = f.lines_written;
                v
            }
        }
    }
}

#[derive(Clone, Copy)]
struct Indent(IndentType, *const Indent);

impl Indent {
    pub const fn new(ty: IndentType) -> Self {
        Self(ty, std::ptr::null())
    }
}

#[derive(Clone, Copy)]
struct IndentIter(*const Indent);

impl Iterator for IndentIter {
    type Item = IndentType;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0.is_null() {
            None
        } else {
            let Indent(indent_ty, parent) = unsafe { *self.0 };
            self.0 = parent;
            Some(indent_ty)
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum IndentType {
    Space,
    Line,
    None,
}

impl ProgressItemRef {
    pub fn remove(self, bar: &mut ProgressBar) {
        bar.remove_item(self);
    }

    pub fn name(self, bar: &ProgressBar) -> &str {
        &bar.items[self.0].name
    }

    pub fn remove_all_children(self, bar: &mut ProgressBar) {
        let Some(v) = bar.items.get_mut(self.0) else {
            return;
        };
        let children = std::mem::take(&mut v.children);
        for child in children {
            bar.remove_item_inner(child);
        }
    }

    pub fn add_child(self, name: Box<str>, bar: &mut ProgressBar) -> ProgressItemRef {
        bar.add_child_item(self, name)
    }
}
