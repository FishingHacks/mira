use std::fmt::{Arguments, Debug, Display, Write};

use crate::{
    lang_items::LangItems,
    typechecking::{
        expression::TypecheckedExpression, Static, TypecheckedFunctionContract, TypecheckedModule,
        TypedStruct, TypedTrait,
    },
};

pub const INDENT_STR: &str = "    ";

pub struct ReadOnlyTypecheckingContext<'ctx> {
    pub modules: &'ctx [TypecheckedModule],
    pub functions: &'ctx [(TypecheckedFunctionContract, Box<[TypecheckedExpression]>)],
    pub external_functions: &'ctx [(
        TypecheckedFunctionContract,
        Option<Box<[TypecheckedExpression]>>,
    )],
    pub statics: &'ctx [Static],
    pub structs: &'ctx [TypedStruct],
    pub traits: &'ctx [TypedTrait],
    pub lang_items: &'ctx LangItems,
}

pub struct IoWriteWrapper<'a>(pub &'a mut (dyn std::io::Write + 'a));

impl Write for IoWriteWrapper<'_> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.0.write_all(s.as_bytes()).map_err(|_| std::fmt::Error)
    }
}

pub struct Formatter<'a, 'ctx> {
    inner: &'a mut (dyn Write + 'a),
    indent: u8,
    pub ctx: ReadOnlyTypecheckingContext<'ctx>,
}

impl Formatter<'_, '_> {
    pub fn write_indent(&mut self) -> std::fmt::Result {
        for _ in 0..self.indent {
            self.inner.write_str(INDENT_STR)?;
        }
        Ok(())
    }

    pub fn push_indent(&mut self) {
        assert!(self.indent < u8::MAX);
        self.indent += 1;
    }

    pub fn pop_indent(&mut self) {
        assert!(self.indent > 0);
        self.indent -= 1;
    }

    pub fn write_debug(&mut self, v: &dyn Debug) -> std::fmt::Result {
        write!(self, "{v:?}")
    }

    pub fn write_value(&mut self, v: &dyn Display) -> std::fmt::Result {
        write!(self, "{v}")
    }

    pub fn write_str(&mut self, s: &str) -> std::fmt::Result {
        <Self as std::fmt::Write>::write_str(self, s)
    }

    pub fn write_char(&mut self, c: char) -> std::fmt::Result {
        <Self as std::fmt::Write>::write_char(self, c)
    }

    pub fn write_fmt(&mut self, args: Arguments<'_>) -> std::fmt::Result {
        <Self as std::fmt::Write>::write_fmt(self, args)
    }
}

impl std::fmt::Write for Formatter<'_, '_> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        for (idx, split) in s.split('\n').enumerate() {
            if idx != 0 {
                self.inner.write_char('\n')?;
                self.write_indent()?;
            }
            self.inner.write_str(split)?;
        }
        Ok(())
    }

    fn write_char(&mut self, c: char) -> std::fmt::Result {
        self.inner.write_char(c)?;
        if c == '\n' {
            self.write_indent()?;
        }
        Ok(())
    }
}

impl<'a, 'ctx> Formatter<'a, 'ctx> {
    pub fn new(inner: &'a mut (dyn Write + 'a), ctx: ReadOnlyTypecheckingContext<'ctx>) -> Self {
        Self {
            inner,
            indent: 0,
            ctx,
        }
    }
}
