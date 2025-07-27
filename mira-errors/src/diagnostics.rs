use std::{
    borrow::Cow,
    fmt::{Arguments, Debug, Display, Write},
};

use mira::tokenizer::span::{SourceMap, Span, SpanWithFile};
use mira_macros::Display;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Display)]
pub enum Severity {
    #[display("warning")]
    Warn,
    #[display("error")]
    Error,
}

#[derive(Clone, Copy)]
pub struct FormattingCtx<'a> {
    pub source_map: &'a SourceMap,
    pub unicode: bool,
}

impl<'a> FormattingCtx<'a> {
    pub fn display_span(self, span: Span<'_>) -> SpanWithFile {
        span.with_source_file(self.source_map)
    }
}

pub struct Formatter<'a> {
    pub ctx: FormattingCtx<'a>,
    pub(crate) writer: &'a mut dyn Write,
}

impl<'a> Formatter<'a> {
    // displays the span as `file:line:col`
    pub fn display_span_loc(&self, f: &mut Formatter<'_>, span: Span<'_>) -> std::fmt::Result {
        let loc = span.with_source_file(self.ctx.source_map);
        let (line, col) = loc.lookup_pos();
        f.display(loc.file.path.display())?;
        f.write_str(":")?;
        f.display(line)?;
        f.write_str(":")?;
        f.display(col)
    }

    pub fn debug(&mut self, v: impl Debug) -> std::fmt::Result {
        self.write_fmt(format_args!("{v:?}"))
    }

    pub fn display(&mut self, v: impl Display) -> std::fmt::Result {
        self.write_fmt(format_args!("{v}"))
    }

    pub fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.writer.write_str(s)
    }

    pub fn write_fmt(&mut self, args: Arguments<'_>) -> std::fmt::Result {
        self.writer.write_fmt(args)
    }

    pub fn write_char(&mut self, c: char) -> std::fmt::Result {
        self.writer.write_char(c)
    }

    pub fn write_space(&mut self, space: u32) -> std::fmt::Result {
        for _ in 0..space {
            self.writer.write_char(' ')?;
        }
        Ok(())
    }

    pub fn write_match_fmt(
        &mut self,
        ascii: Arguments<'_>,
        unicode: Arguments<'_>,
    ) -> std::fmt::Result {
        if self.ctx.unicode {
            self.writer.write_fmt(unicode)
        } else {
            self.writer.write_fmt(ascii)
        }
    }

    pub(crate) fn new(ctx: FormattingCtx<'a>, writer: &'a mut dyn Write) -> Self {
        Self { ctx, writer }
    }
}

pub trait ErrorData: Send {
    fn message<'ctx>(
        &'ctx self,
        ctx: FormattingCtx<'ctx>,
        cb: &mut dyn FnMut(Arguments<'_>) -> std::fmt::Result,
    ) -> std::fmt::Result;

    #[allow(unused_variables, unused_mut)]
    fn notes<'ctx>(
        &'ctx self,
        ctx: FormattingCtx<'ctx>,
        mut cb: &mut dyn FnMut(Arguments<'_>) -> std::fmt::Result,
    ) -> std::fmt::Result {
        Ok(())
    }

    #[allow(unused_variables)]
    fn labeled_spans(&self, ctx: FormattingCtx<'_>) -> Vec<LabeledSpan> {
        Vec::new()
    }

    fn error_code(&self) -> Option<&'static str> {
        None
    }
}

impl<T: AsRef<str> + Send + ?Sized> ErrorData for T {
    fn message<'ctx>(
        &'ctx self,
        _: FormattingCtx<'ctx>,
        cb: &mut dyn FnMut(Arguments<'_>) -> std::fmt::Result,
    ) -> std::fmt::Result {
        cb(format_args!("{}", self.as_ref()))
    }

    fn notes<'ctx>(
        &'ctx self,
        _: FormattingCtx<'ctx>,
        _: &mut dyn FnMut(Arguments<'_>) -> std::fmt::Result,
    ) -> std::fmt::Result {
        Ok(())
    }
}

pub struct Diagnostic<'arena> {
    diagnostic: Option<Box<DiagnosticInner<'arena, dyn ErrorData + 'arena>>>,
}

impl Drop for Diagnostic<'_> {
    fn drop(&mut self) {
        if self.diagnostic.is_some() {
            println!("Diagnostic was dropped without emitting");
        }
    }
}

impl<'arena> Diagnostic<'arena> {
    pub(crate) fn take_inner(
        &mut self,
    ) -> Option<Box<DiagnosticInner<'arena, dyn ErrorData + 'arena>>> {
        self.diagnostic.take()
    }

    pub fn new<E: ErrorData + 'arena>(err: E, severity: Severity) -> Self {
        Self {
            diagnostic: Some(Box::new(DiagnosticInner {
                severity,
                extra_notes: Vec::new(),
                labels: Vec::new(),
                err,
            })),
        }
    }

    pub fn with_note(mut self, label: impl Into<Cow<'static, str>>) -> Self {
        self.diagnostic
            .as_mut()
            .expect("diagnostic was already emitted")
            .extra_notes
            .push(label.into());
        self
    }
    pub fn with_secondary_label(
        self,
        span: Span<'arena>,
        label: impl Into<Cow<'static, str>>,
    ) -> Self {
        self.with_label(LabeledSpan {
            style: LabeledSpanStyle::Secondary,
            text: label.into(),
            span,
        })
    }
    pub fn with_primary_label(
        self,
        span: Span<'arena>,
        label: impl Into<Cow<'static, str>>,
    ) -> Self {
        self.with_label(LabeledSpan {
            style: LabeledSpanStyle::Primary,
            text: label.into(),
            span,
        })
    }
    pub fn with_label(mut self, label: LabeledSpan<'arena>) -> Self {
        self.diagnostic
            .as_mut()
            .expect("diagnostic was already emitted")
            .labels
            .push(label);
        self
    }

    pub fn add_note(&mut self, label: impl Into<Cow<'static, str>>) -> &mut Self {
        self.diagnostic
            .as_mut()
            .expect("diagnostic was already emitted")
            .extra_notes
            .push(label.into());
        self
    }
    pub fn add_secondary_label(
        &mut self,
        span: Span<'arena>,
        label: impl Into<Cow<'static, str>>,
    ) -> &mut Self {
        self.add_label(LabeledSpan {
            style: LabeledSpanStyle::Secondary,
            text: label.into(),
            span,
        })
    }
    pub fn add_primary_label(
        &mut self,
        span: Span<'arena>,
        label: impl Into<Cow<'static, str>>,
    ) -> &mut Self {
        self.add_label(LabeledSpan {
            style: LabeledSpanStyle::Primary,
            text: label.into(),
            span,
        })
    }
    pub fn add_label(&mut self, label: LabeledSpan<'arena>) -> &mut Self {
        self.diagnostic
            .as_mut()
            .expect("diagnostic was already emitted")
            .labels
            .push(label);
        self
    }
}

pub(crate) struct DiagnosticInner<'arena, E: ErrorData + 'arena + ?Sized> {
    pub(crate) severity: Severity,
    pub(crate) extra_notes: Vec<Cow<'static, str>>,
    pub(crate) labels: Vec<LabeledSpan<'arena>>,
    pub(crate) err: E,
}

#[derive(Clone)]
pub struct LabeledSpan<'arena> {
    pub(crate) style: LabeledSpanStyle,
    pub(crate) text: Cow<'static, str>,
    pub(crate) span: Span<'arena>,
}

impl<'arena> LabeledSpan<'arena> {
    pub fn primary(text: impl Into<Cow<'static, str>>, span: Span<'arena>) -> Self {
        Self {
            style: LabeledSpanStyle::Primary,
            text: text.into(),
            span,
        }
    }
    pub fn secondary(text: impl Into<Cow<'static, str>>, span: Span<'arena>) -> Self {
        Self {
            style: LabeledSpanStyle::Secondary,
            text: text.into(),
            span,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Display)]
pub enum LabeledSpanStyle {
    /// Primary Span, highlights the span using up-arrows
    ///
    /// 0 | ... meow(nya, nya)
    ///   .          ^^^ nya used here
    #[display("primary")]
    Primary,
    /// Secondary Span, highlights the span using dashes
    ///
    /// 0 | ... meow(nya, nya)
    ///   .               --- second nya used here
    #[display("secondary")]
    Secondary,
}
