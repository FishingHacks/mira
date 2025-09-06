use std::{fmt::Debug, fs::File, io::Write, sync::Arc};

use mira_errors::{Diagnostic, DiagnosticFormatter, Output, StyledPrinter, Styles};
use mira_progress_bar::print_thread::ProgressBarThread;
use mira_spans::SourceMap;

use crate::ErrorTracker;

#[derive(Clone, Copy)]
pub struct ErrorEmitted;

pub enum DiagEmitter {
    Stdout(ProgressBarThread),
    Stderr(ProgressBarThread),
    Discard,
    /// panics when a diagnostic is emitted.
    NoFail,
    File(File),
}

impl Debug for DiagEmitter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DiagEmitter::Stdout(_) => f.write_str("DiagEmitter::Stdout"),
            DiagEmitter::Stderr(_) => f.write_str("DiagEmitter::Stderr"),
            DiagEmitter::Discard => f.write_str("DiagEmitter::Discard"),
            DiagEmitter::NoFail => f.write_str("DiagEmitter::NoFail"),
            DiagEmitter::File(_) => f.write_str("DiagEmitter::File"),
        }
    }
}

pub struct DiagCtx {
    emitter: DiagEmitter,
    printer: DiagnosticFormatter,
    err_count: usize,
}

impl Debug for DiagCtx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DiagCtx")
            .field("err_count", &self.err_count)
            .field("emitter", &self.emitter)
            .finish()
    }
}

impl DiagCtx {
    pub fn new(
        emitter: DiagEmitter,
        source_map: Arc<SourceMap>,
        printer: Box<dyn StyledPrinter>,
        styles: Styles,
    ) -> Self {
        let styles = match emitter {
            DiagEmitter::File(_) => Styles::NO_COLORS,
            _ => styles,
        };
        let printer = DiagnosticFormatter::new(source_map, Output::string(), printer, styles);

        Self {
            emitter,
            printer,
            err_count: 0,
        }
    }

    pub fn emit_diag(&mut self, diag: Diagnostic<'_>) -> ErrorEmitted {
        if diag.is_error() {
            self.err_count += 1;
        }
        if matches!(self.emitter, DiagEmitter::Discard) {
            diag.dismiss();
            return ErrorEmitted;
        }
        self.printer
            .display_diagnostic(diag)
            .expect("strings cannot fail");
        let s = self.printer.get_output_mut().as_string_mut();
        if !s.ends_with("\n\n") {
            s.push('\n');
        }
        match &mut self.emitter {
            DiagEmitter::Stdout(t) => t.print_stdout(std::mem::take(
                self.printer.get_output_mut().as_string_mut(),
            )),
            DiagEmitter::Stderr(t) => t.print_stderr(std::mem::take(
                self.printer.get_output_mut().as_string_mut(),
            )),

            DiagEmitter::File(f) => {
                let res = f.write_all(s.as_bytes());
                s.clear();
                res.expect("failed to write to diagnostic file");
            }
            DiagEmitter::NoFail => {
                panic!(
                    "Error:\n{}",
                    std::mem::take(self.printer.get_output_mut().as_string_mut())
                )
            }
            DiagEmitter::Discard => unreachable!(),
        }
        ErrorEmitted
    }

    pub fn err_count(&self) -> usize {
        self.err_count
    }

    pub fn track_errors(&self) -> ErrorTracker {
        ErrorTracker(self.err_count)
    }

    pub fn errors_happened(&self, tracker: ErrorTracker) -> bool {
        self.err_count != tracker.0
    }
}
