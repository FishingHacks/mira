use mira_spans::Span;

use crate::{ErrorData, LabeledSpan};

pub(crate) struct ErrorSimple(pub u32);

impl ErrorData for ErrorSimple {
    fn message<'ctx>(
        &'ctx self,
        _: crate::FormattingCtx<'ctx>,
        cb: &mut dyn FnMut(std::fmt::Arguments<'_>) -> std::fmt::Result,
    ) -> std::fmt::Result {
        cb(format_args!("this is a simple error with {}.", self.0))
    }
}

pub(crate) struct ErrorCode;

impl ErrorData for ErrorCode {
    fn message<'ctx>(
        &'ctx self,
        _: crate::FormattingCtx<'ctx>,
        cb: &mut dyn FnMut(std::fmt::Arguments<'_>) -> std::fmt::Result,
    ) -> std::fmt::Result {
        cb(format_args!("this has an error code"))
    }

    fn error_code(&self) -> Option<&'static str> {
        Some("E1834")
    }
}

pub(crate) struct WithSpans<'arena>(pub Span<'arena>, pub Span<'arena>);

impl ErrorData for WithSpans<'_> {
    fn message<'ctx>(
        &'ctx self,
        _: crate::FormattingCtx<'ctx>,
        cb: &mut dyn FnMut(std::fmt::Arguments<'_>) -> std::fmt::Result,
    ) -> std::fmt::Result {
        cb(format_args!("spans error"))
    }

    fn labeled_spans(&'_ self, _: crate::FormattingCtx<'_>) -> Vec<LabeledSpan<'_>> {
        vec![
            LabeledSpan::primary("primary", self.0),
            LabeledSpan::secondary("secondary", self.1),
        ]
    }
}

pub(crate) struct WithNote(pub &'static str);

impl ErrorData for WithNote {
    fn message<'ctx>(
        &'ctx self,
        _: crate::FormattingCtx<'ctx>,
        cb: &mut dyn FnMut(std::fmt::Arguments<'_>) -> std::fmt::Result,
    ) -> std::fmt::Result {
        cb(format_args!("error with a note"))
    }

    fn notes<'ctx>(
        &'ctx self,
        _: crate::FormattingCtx<'ctx>,
        cb: &mut dyn FnMut(std::fmt::Arguments<'_>) -> std::fmt::Result,
    ) -> std::fmt::Result {
        cb(format_args!("{}", self.0))?;
        cb(format_args!("{}", self.0))
    }
}
