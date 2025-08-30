use std::collections::HashMap;
use std::fmt::Debug;

use crate::box_drawing_characters::{self as bdc, BoxDrawingChars};
use crate::sized_line_writer::SizedLineWriter;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct ProgressItemRef(pub usize);

#[derive(Debug, Clone)]
pub(crate) struct ProgressItem {
    name: Box<str>,
    pub(crate) children: Vec<ProgressItemRef>,
    parent: ProgressItemRef,
}

#[derive(Debug, Clone, Default)]
pub struct ProgressBar {
    chars: BoxDrawingChars,
    pub(crate) items: HashMap<usize, ProgressItem>,
    pub(crate) root_children: Vec<ProgressItemRef>,
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

    pub fn add_item(&mut self, name: Box<str>, item_ref: ProgressItemRef) {
        self.items.insert(
            item_ref.0,
            ProgressItem {
                name,
                children: Vec::new(),
                parent: ProgressItemRef(usize::MAX),
            },
        );
        self.root_children.push(item_ref);
    }

    pub fn remove_item(&mut self, item_ref: ProgressItemRef) {
        let Some(item) = self.remove_item_inner(item_ref) else {
            return;
        };
        match self.items.get_mut(&item.parent.0) {
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

    pub(crate) fn remove_item_inner(&mut self, item_ref: ProgressItemRef) -> Option<ProgressItem> {
        let item = self.items.remove(&item_ref.0)?;
        for child in item.children.iter().copied() {
            self.remove_item_inner(child);
        }
        Some(item)
    }

    pub fn add_child_item(
        &mut self,
        parent: ProgressItemRef,
        name: Box<str>,
        item_ref: ProgressItemRef,
    ) {
        self.items.insert(
            item_ref.0,
            ProgressItem {
                name,
                children: Vec::new(),
                parent,
            },
        );

        if let Some(v) = self.items.get_mut(&parent.0) {
            v.children.push(item_ref)
        }
    }

    fn print_inner(
        &self,
        f: &mut SizedLineWriter,
        children: &[ProgressItemRef],
        last_item: *mut Indent,
        root_node: *const Indent,
    ) -> std::io::Result<()> {
        let last_idx = children.len().saturating_sub(1);
        for (idx, value) in children.iter().enumerate() {
            let Some(child) = self.items.get(&value.0) else {
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

    pub fn clean(&mut self, f: &mut dyn std::io::Write) -> std::io::Result<()> {
        f.write_all("\x1b[G\x1b[K".as_bytes())?;
        for _ in 0..self.last_paint_linenum {
            f.write_all("\x1b[A\x1b[K".as_bytes())?;
        }
        self.last_paint_linenum = 0;
        Ok(())
    }

    pub fn display(&mut self, f: &mut dyn std::io::Write) -> std::io::Result<()> {
        self.clean(f)?;
        let mut f = SizedLineWriter::new(get_size().0 as usize, f);
        f.write_str("root\n")?;
        let mut root_node = Indent::new(IndentType::None);
        let v = self.print_inner(&mut f, &self.root_children, &mut root_node, &root_node);
        self.last_paint_linenum = f.lines_written;
        v
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
