use std::{
    collections::HashMap,
    fmt::Write,
    io::{StderrLock, StdoutLock, Write as _},
    sync::Arc,
};

use mira::{
    context::SharedContext,
    tokenizer::span::{FileId, SourceFile},
};
pub use mira_macros::{Display, ErrorData};
mod diagnostics;
pub mod printers;
pub use diagnostics::*;
// this is used by the proc macro to not break it when used inside of mira, where mira would be
// `crate::`.
#[doc(hidden)]
pub use mira::tokenizer::span::Span as MiraSpan;
use printers::Loc;
pub use printers::{AsciiPrinter, StyledPrinter, Styles, UnicodePrinter};

pub enum Output {
    Stdout,
    Stderr,
    Custom(Box<dyn Write + Send + Sync + 'static>),
}

impl Output {
    pub fn with_writer<R>(&mut self, f: impl FnOnce(&mut dyn Write) -> R) -> R {
        match self {
            Output::Stdout => f(&mut StdoutWriter(std::io::stdout().lock())),
            Output::Stderr => f(&mut StderrWriter(std::io::stderr().lock())),
            Output::Custom(write) => f(&mut **write),
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

pub struct DiagnosticFormatter<'arena, P: StyledPrinter> {
    ctx: SharedContext<'arena>,
    unicode: bool,
    output: Output,
    printer: P,
    styles: Styles,
}

impl<'arena, P: StyledPrinter> DiagnosticFormatter<'arena, P> {
    pub fn new(ctx: SharedContext<'arena>, output: Output, printer: P, styles: Styles) -> Self {
        Self {
            ctx,
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

    pub fn with_output<R>(&mut self, f: impl FnOnce(Formatter) -> R) -> R {
        let ctx = FormattingCtx {
            source_map: self.ctx.source_map(),
            unicode: self.unicode,
        };
        self.output
            .with_writer(move |writer| f(Formatter::new(ctx, writer)))
    }

    pub fn display_diagnostic(&mut self, mut diagnostic: Diagnostic<'arena>) -> std::fmt::Result {
        let Some(mut value) = diagnostic.take_inner() else {
            panic!("Diagnostic was already emitted")
        };
        let ctx = FormattingCtx {
            source_map: self.ctx.source_map(),
            unicode: self.unicode,
        };
        self.output.with_writer(|writer| {
            Self::print_diagnostic(
                &mut value,
                &mut Formatter::new(ctx, writer),
                &mut self.printer,
                self.styles,
            )
        })
    }

    fn print_diagnostic(
        diagnostic: &mut DiagnosticInner<'arena, dyn ErrorData + 'arena>,
        f: &mut Formatter<'_>,
        printer: &mut P,
        styles: Styles,
    ) -> std::fmt::Result {
        let ctx = f.ctx;

        let mut files_and_spans: HashMap<FileId, Vec<Loc>> = HashMap::new();
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
        locs: &[Loc],
        f: &mut Formatter<'_>,
        printer: &P,
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
                printer.print_loc_single(f, loc)?;
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

    /// Discards the diagnostic without printing it.
    pub fn discard_diagnostic(&mut self, mut diagnostic: Diagnostic<'arena>) {
        _ = diagnostic.take_inner();
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

#[cfg(test)]
mod test {
    use super::*;
    use crate as mira_errors;
    use mira::{
        arena::Arena,
        context::GlobalContext,
        tokenizer::span::{BytePos, SourceMap, Span, SpanData},
    };
    use std::path::Path;

    #[derive(ErrorData)]
    #[error("error: Meow :3")]
    #[note("Emitted by testing")]
    #[note("Fuck u u suck")]
    #[note("cringest error frfr :3")]
    struct TestError<'arena> {
        #[secondary_label("something broookeeeee")]
        meow: Span<'arena>,
    }

    #[test]
    fn test() {
        let arena = Arena::new();
        let ctx = GlobalContext::new(&arena);
        let ctx = ctx.share();
        let source_map = SourceMap::new([].into());
        let file = source_map.new_file(
            Path::new("src/main.rs").into(),
            Path::new("src").into(),
            SOURCE.into(),
        );
        ctx.init_source_map(source_map);
        let mut emitter =
            DiagnosticFormatter::new(ctx, Output::Stdout, UnicodePrinter::new(), Styles::DEFAULT)
                .unicode(true);
        let meow = new_span(ctx, &file, 2, 26, 3);
        let diagnostic = Diagnostic::new(TestError { meow }, Severity::Warn)
            .with_secondary_label(new_span(ctx, &file, 2, 8, 1), "binding `s` declared here")
            .with_secondary_label(new_span(ctx, &file, 5, 16, 5), "borrow of `s` occurs here")
            .with_secondary_label(new_span(ctx, &file, 5, 8, 5), "this is `other`")
            .with_primary_label(
                new_span(ctx, &file, 6, 9, 1),
                "move out of `s` occurs here",
            )
            .with_secondary_label(new_span(ctx, &file, 9, 18, 7), "borrow later used here")
            .with_note("this error originates in the macro `$crate::format_args_nl` which comes from the expansion of the macro `println` (in Nightly builds, run with -Z macro-backtrace for more info)");

        emitter.display_diagnostic(diagnostic).unwrap();
        let diagnostic = Diagnostic::new(TestError { meow }, Severity::Error)
            .with_secondary_label(new_span(ctx, &file, 2, 8, 1), "binding `s` declared here")
            .with_secondary_label(new_span(ctx, &file, 5, 16, 6), "borrow of `s` occurs here")
            .with_secondary_label(new_span(ctx, &file, 5, 8, 5), "this is `other`")
            .with_primary_label(
                new_span(ctx, &file, 6, 9, 1),
                "move out of `s` occurs here",
            )
            .with_secondary_label(new_span(ctx, &file, 8, 18, 7), "borrow later used here")
            .with_note("this error originates in the macro `$crate::format_args_nl` which comes from the expansion of the macro `println` (in Nightly builds, run with -Z macro-backtrace for more info)");

        emitter.display_diagnostic(diagnostic).unwrap();
        panic!()
    }

    fn new_span<'ctx>(
        ctx: SharedContext<'ctx>,
        file: &SourceFile,
        line: u32,
        col: u32,
        len: u32,
    ) -> Span<'ctx> {
        let mut offset = 0;
        for line in 1..line {
            offset += file.get_line(line as usize - 1).len() + 1;
        }
        let pos = BytePos::from_u32(offset as u32 + col);
        ctx.intern_span(SpanData::new(pos, len, file.id))
    }

    const SOURCE: &str = r#"fn main() {
    let s = String::from("nya");


    let other = &mut s;
    drop(s);
    // meow :3
    println!("{s} {other}");
}"#;
}
