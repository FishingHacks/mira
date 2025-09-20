use std::{
    any::Any,
    collections::HashMap,
    fmt::Write,
    io::{IsTerminal as _, StderrLock, StdoutLock, Write as _},
    ops::Deref,
    sync::Arc,
};

pub use mira_macros::{Display, ErrorData};
use mira_spans::{FileId, SourceFile, SourceMap};
mod diagnostics;
pub mod printers;
pub use diagnostics::*;
use printers::Loc;
pub use printers::{AsciiPrinter, StyledPrinter, Styles, UnicodePrinter};
#[cfg(test)]
pub(crate) mod test_errors;

#[macro_export]
macro_rules! pluralize {
    ($v:expr) => {
        if $v == 1 { "" } else { "s" }
    };
    ($plural:literal, $v:expr) => {
        if $v == 1 { "" } else { $plural }
    };
}

pub trait OutputWriter: Write + Any {}

impl<T: Write + Any> OutputWriter for T {}

use crate as mira_errors;

pub trait DiagEmitter<'ctx>: 'ctx {
    fn emit_diagnostic(&self, diag: Diagnostic<'ctx>);
}

#[derive(ErrorData)]
#[error("couldn't write to stdout: {_0}")]
#[no_arena_lifetime]
pub struct StdoutWriteError(pub std::io::Error);

#[derive(ErrorData)]
#[error("couldn't write `{}`: {_1}", _0.display())]
#[no_arena_lifetime]
pub struct IoWriteError(pub std::path::PathBuf, pub std::io::Error);

#[derive(ErrorData)]
#[error("couldn't create directory `{}`: {_1}", _0.display())]
#[no_arena_lifetime]
pub struct IoCreateDirError(pub std::path::PathBuf, pub std::io::Error);

#[derive(ErrorData)]
#[error("couldn't read `{}`: {_1}", _0.display())]
#[no_arena_lifetime]
pub struct IoReadError(pub std::path::PathBuf, pub std::io::Error);

#[derive(ErrorData)]
#[error("failed to get the working directory: {_0}")]
#[no_arena_lifetime]
pub struct CurrentDirError(pub std::io::Error);

#[derive(ErrorData)]
#[error("failed to open `{}`: {_1}", _0.display())]
#[no_arena_lifetime]
pub struct FileOpenError(pub std::path::PathBuf, pub std::io::Error);

pub enum Output {
    Stdout,
    Stderr,
    String(String),
}

impl Output {
    pub const fn string() -> Self {
        Self::String(String::new())
    }

    pub fn as_string_mut(&mut self) -> &mut String {
        match self {
            Output::Stdout | Output::Stderr => {
                unreachable!("output has to be of type Output::String")
            }
            Output::String(s) => s,
        }
    }

    pub fn as_string_ref(&self) -> &String {
        match self {
            Output::Stdout | Output::Stderr => {
                unreachable!("output has to be of type Output::String")
            }
            Output::String(s) => s,
        }
    }

    pub fn with_writer<R>(&mut self, f: impl FnOnce(&mut dyn Write) -> R) -> R {
        match self {
            Output::Stdout => f(&mut StdoutWriter(std::io::stdout().lock())),
            Output::Stderr => f(&mut StderrWriter(std::io::stderr().lock())),
            Output::String(s) => f(s),
        }
    }
}

struct StderrWriter(StderrLock<'static>);
impl Write for StderrWriter {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        if let Err(e) = self.0.write_all(s.as_bytes()) {
            panic!("Failed to write to stdout while trying to emit diagnostics: {e}");
        }
        Ok(())
    }
}
struct StdoutWriter(StdoutLock<'static>);
impl Write for StdoutWriter {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        if let Err(e) = self.0.write_all(s.as_bytes()) {
            panic!("Failed to write to stdout while trying to emit diagnostics: {e}");
        }
        Ok(())
    }
}

impl<'arena> IntoIterator for Diagnostics<'arena> {
    type Item = Diagnostic<'arena>;

    type IntoIter = <Vec<Diagnostic<'arena>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Default)]
pub struct Diagnostics<'arena>(Vec<Diagnostic<'arena>>);

impl<'arena> Deref for Diagnostics<'arena> {
    type Target = [Diagnostic<'arena>];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'arena> From<Diagnostic<'arena>> for Diagnostics<'arena> {
    fn from(value: Diagnostic<'arena>) -> Self {
        Self(vec![value])
    }
}

impl<'arena> From<Vec<Diagnostic<'arena>>> for Diagnostics<'arena> {
    fn from(value: Vec<Diagnostic<'arena>>) -> Self {
        Self(value)
    }
}

impl<'arena> Diagnostics<'arena> {
    pub const fn new() -> Self {
        Self(Vec::new())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Diagnostic<'arena>> {
        self.0.iter()
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Diagnostic<'arena>> {
        self.0.iter_mut()
    }
    pub fn add(&mut self, diagnostic: Diagnostic<'arena>) -> &mut Diagnostic<'arena> {
        self.0.push(diagnostic);
        self.0.last_mut().unwrap()
    }
    pub fn add_err(&mut self, diagnostic: impl ErrorData + 'arena) -> &mut Diagnostic<'arena> {
        self.add(Diagnostic::new(diagnostic, Severity::Error))
    }
    pub fn add_warn(&mut self, diagnostic: impl ErrorData + 'arena) -> &mut Diagnostic<'arena> {
        self.add(Diagnostic::new(diagnostic, Severity::Warn))
    }
    pub fn extend(&mut self, other: impl IntoIterator<Item = Diagnostic<'arena>>) {
        self.0.extend(other);
    }
}

pub struct DiagnosticFormatter {
    source_map: Arc<SourceMap>,
    unicode: bool,
    output: Output,
    printer: Box<dyn StyledPrinter>,
    styles: Styles,
}

/// determines the style to use by the user's environment variables and terminal capabilities.
pub fn env_style() -> Styles {
    match std::env::var("MIRA_COLOR").ok().as_deref() {
        Some("0" | "none" | "no") => Styles::NO_COLORS,
        Some("1" | "yes") => Styles::DEFAULT,
        _ if std::io::stdout().is_terminal() => Styles::DEFAULT,
        _ => Styles::NO_COLORS,
    }
}

/// determines the printer to use based on the user's environment variables.
pub fn env_printer() -> Box<dyn StyledPrinter> {
    match std::env::var("MIRA_DIAG_STYLE").ok().as_deref() {
        Some("ascii" | "text") => Box::new(AsciiPrinter::new()),
        _ => Box::new(UnicodePrinter::new()),
    }
}

pub fn default_printer() -> Box<dyn StyledPrinter> {
    Box::new(UnicodePrinter::new())
}

pub fn default_styles() -> Styles {
    Styles::NO_COLORS
}

impl DiagnosticFormatter {
    pub fn new(
        source_map: Arc<SourceMap>,
        output: Output,
        printer: Box<dyn StyledPrinter>,
        styles: Styles,
    ) -> Self {
        Self {
            source_map,
            unicode: false,
            output,
            styles,
            printer,
        }
    }

    pub fn unicode(mut self, unicode: bool) -> Self {
        self.unicode = unicode;
        self
    }

    pub fn get_output(&self) -> &Output {
        &self.output
    }

    pub fn get_output_mut(&mut self) -> &mut Output {
        &mut self.output
    }

    pub fn with_output<R>(&mut self, f: impl FnOnce(Formatter<'_>) -> R) -> R {
        let ctx = FormattingCtx {
            source_map: &self.source_map,
            unicode: self.unicode,
        };
        self.output
            .with_writer(move |writer| f(Formatter::new(ctx, writer)))
    }

    pub fn display_diagnostic(&mut self, mut diagnostic: Diagnostic<'_>) -> std::fmt::Result {
        let Some(mut value) = diagnostic.take_inner() else {
            panic!("Diagnostic was already emitted")
        };
        let ctx = FormattingCtx {
            source_map: &self.source_map,
            unicode: self.unicode,
        };
        self.output.with_writer(|writer| {
            Self::print_diagnostic(
                &mut value,
                &mut Formatter::new(ctx, writer),
                &mut *self.printer,
                self.styles,
            )?;
            writer.write_str(self.styles.reset)
        })
    }

    fn print_diagnostic<'arena>(
        diagnostic: &mut DiagnosticInner<'arena, dyn ErrorData + 'arena>,
        f: &mut Formatter<'_>,
        printer: &mut dyn StyledPrinter,
        styles: Styles,
    ) -> std::fmt::Result {
        let ctx = f.ctx;

        let mut files_and_spans: HashMap<FileId, Vec<Loc<'_>>> = HashMap::new();
        let err_labels = diagnostic.err.labeled_spans(ctx);
        for label in diagnostic.labels.iter().chain(err_labels.iter()) {
            let data = label.span.get_span_data();
            let file = ctx.source_map.get_file(data.file).unwrap();
            let (line, start) = file.lookup_file_pos(data.pos);
            let end = start + data.len;
            let style = label.style;
            let text = &label.text;
            files_and_spans.entry(data.file).or_default().push(Loc {
                line,
                start,
                end,
                style,
                text,
            });
        }
        // sort the locations by line and then by start
        files_and_spans.values_mut().for_each(|locs| {
            locs.sort_by(|a, b| {
                if a.line == b.line {
                    a.start.cmp(&b.start)
                } else {
                    a.line.cmp(&b.line)
                }
            });
        });
        let max_line = files_and_spans
            .values()
            .filter_map(|v| v.iter().map(|v| v.line).max())
            .max()
            .unwrap_or(0);
        let max_line_len = Self::line_len(max_line);

        printer.configure(styles, diagnostic.severity, max_line_len);

        diagnostic.err.message(ctx, &mut |message| {
            printer.print_message(f, diagnostic.err.error_code(), message)
        })?;

        for (file, locs) in files_and_spans {
            Self::print_file_spans(ctx.source_map.get_file(file).unwrap(), &locs, f, printer)?
        }
        diagnostic
            .err
            .notes(ctx, &mut |args| printer.print_note(f, args))?;

        for note in &diagnostic.extra_notes {
            printer.print_note(f, format_args!("{note}"))?;
        }

        Ok(())
    }

    /// locs have to be sorted by `loc.line` and then by `loc.start`
    fn print_file_spans(
        file: Arc<SourceFile>,
        locs: &[Loc<'_>],
        f: &mut Formatter<'_>,
        printer: &dyn StyledPrinter,
    ) -> std::fmt::Result {
        // header [[ascii]]:
        //   --> src/main.mr:12:13
        //    |

        // find the first primary location (if there is one), to print it behind the file name
        let line_col = locs
            .iter()
            .find(|v| v.style == LabeledSpanStyle::Primary)
            .map(|loc| (loc.line, loc.start));
        printer.print_header(f, &file.path, line_col)?;

        // locs [[single one, ascii]]:
        //    v- if more than one line seperate the last line from the current, we insert a line split.
        // ...
        // 12 | meow();
        //    .     ^^ `meow` is not a function
        //    ^     ^- primary label and span.
        //    `- space to signify inserted diagnostic messages
        //
        // locs [[multiple, ascii]]:
        // 10 | purr();
        //    .   ^^--^    <- the primary contained the secondary span, so we have to insert more characters after it to faithfully report the entire span.
        //    .   | `- waow fancy secondary info
        //    .   `- primary info :3

        let mut last_line = locs[0].line;
        for lines in LineSplitter(0, locs) {
            let current_line = lines[0].line;
            let diff = current_line - last_line;
            match diff {
                // in case this is the same line (first iteration), or the previous line was the
                // line right before this one, we don't have to do anything.
                ..2 => {}
                // case where between the last and the current line there is 1 line
                // we use last_line for file.get_line, because the current line (1-based) is
                // last_line + 1, but get_line is 0-based, so it's + 1 - 1
                2 => {
                    printer.print_code_line(f, last_line + 1, file.get_line(last_line as usize))?
                }
                // case where there are 2 or more line inbetween the current and last line
                _ => printer.print_code_line_interrupt(f)?,
            }
            last_line = current_line;
            // we have to get line - 1, because get_line works with 0-based lines, while loc.line
            // is 1-based
            let line_str = file.get_line(current_line as usize - 1);
            let line_len = line_str.len();
            // print line contents
            printer.print_code_line(f, current_line, line_str)?;

            // special case: if there is only 1 location, use the following format [ascii printer]:
            // 12 | fn meow()
            //    .    ---- `meow` defined here
            if lines.len() == 1 {
                let loc = lines[0];
                printer.print_loc_single(f, loc, line_len as u32)?;
                continue;
            }

            printer.print_loc_lines(f, lines, line_len as u32)?;

            // print the labels
            for i in 0..lines.len() {
                // because the last loc gets it's label in each iteration to have a cascading
                // effect where the last locations get printed the highest and the earlier ones the
                // lowest, we have to remove the last loc of the previous iteration.
                // the cascading looks something like this in the ascii printer:
                // --- --- -----
                // |   |   `- label of loc 3
                // |   `- label of loc 2
                // `- label of loc 1
                let actual_lines = &lines[..lines.len() - i];
                printer.print_loc_bottom_lines(f, actual_lines)?;
            }
        }

        // footer [[ascii printer]]:
        //   |
        //   ^ the extra line at the bottom helps tell files apart
        printer.print_footer(f)
    }

    fn line_len(value: u32) -> u32 {
        match value {
            0..10 => 1,
            10..100 => 2,
            100..1000 => 3,
            1000..10000 => 4,
            10000..100000 => 5,
            100000..1000000 => 6,
            1000000..10000000 => 7,
            10000000..100000000 => 8,
            100000000..1000000000 => 9,
            1000000000..=u32::MAX => 10,
        }
    }
}

/// Iterates over a list of locations that is sorted by line number, where the item of the iterator
/// are all locations on the same line.
struct LineSplitter<'a, 'b>(pub usize, pub &'a [Loc<'b>]);
impl<'a, 'b> Iterator for LineSplitter<'a, 'b> {
    type Item = &'a [Loc<'b>];

    fn next(&mut self) -> Option<Self::Item> {
        if self.0 >= self.1.len() {
            return None;
        }
        let start = self.0;
        let curline = self.1[start].line;
        loop {
            self.0 += 1;
            if self.0 >= self.1.len() || self.1[self.0].line != curline {
                break;
            }
        }
        Some(&self.1[start..self.0])
    }
}

#[derive(Clone, Copy)]
pub struct FatalError;

impl FatalError {
    /// raises a fatal error
    pub fn raise(self) -> ! {
        std::panic::resume_unwind(Box::new(self))
    }
}
