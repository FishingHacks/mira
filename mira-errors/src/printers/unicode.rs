use std::{fmt::Arguments, path::Path};

use crate::printers::{color_from_severity, get_loc, print_line_left};
use crate::{Formatter, LabeledSpanStyle, Loc, Severity, StyledPrinter, Styles};

pub struct UnicodePrinter {
    styles: Styles,
    severity_color: &'static str,
    severity: Severity,
    line_col_size: u32,
}

impl UnicodePrinter {
    pub fn new() -> Self {
        Self {
            styles: Styles::NO_COLORS,
            severity_color: "",
            severity: Severity::Warn,
            line_col_size: 0,
        }
    }

    fn color_for_style(&self, style: LabeledSpanStyle) -> &'static str {
        match style {
            LabeledSpanStyle::Primary => self.severity_color,
            LabeledSpanStyle::Secondary => self.styles.blue,
        }
    }

    fn write_loc_style(
        &self,
        f: &mut Formatter<'_>,
        style: LabeledSpanStyle,
        last: LabeledSpanStyle,
    ) -> Result<LabeledSpanStyle, std::fmt::Error> {
        if style != last {
            f.write_str(self.color_for_style(style))?;
        }
        Ok(style)
    }
}

impl Default for UnicodePrinter {
    fn default() -> Self {
        Self::new()
    }
}

impl StyledPrinter for UnicodePrinter {
    fn configure(&mut self, styles: Styles, severity: Severity, line_col_size: u32) {
        self.styles = styles;
        self.severity_color = color_from_severity(&self.styles, severity);
        self.severity = severity;
        self.line_col_size = line_col_size;
    }

    fn print_message(
        &self,
        f: &mut Formatter<'_>,
        error_code: Option<&str>,
        message: Arguments<'_>,
    ) -> std::fmt::Result {
        f.write_str(self.styles.bold)?;
        f.write_str(self.severity_color)?;
        f.display(self.severity)?;
        f.write_str(self.styles.reset_fg)?;
        if let Some(code) = error_code {
            f.write_fmt(format_args!("[{code}]"))?;
        }
        f.write_str(": ")?;
        f.write_fmt(message)?;
        f.write_char('\n')
    }

    fn print_header(
        &self,
        f: &mut Formatter<'_>,
        file: &Path,
        line_col: Option<(u32, u32)>,
    ) -> std::fmt::Result {
        f.write_space(self.line_col_size + 1)?;
        f.write_str(self.styles.bold)?;
        f.write_str(self.styles.blue)?;
        f.write_str("╭──")?;
        f.write_str(self.styles.reset)?;
        f.write_fmt(format_args!("[{}", file.display()))?;
        if let Some((line, col)) = line_col {
            f.write_fmt(format_args!(":{line}:{col}"))?;
        }
        f.write_str("]\n")?;
        print_line_left(f, &self.styles, '│', None, self.line_col_size)?;
        f.write_char('\n')
    }

    fn print_code_line(&self, f: &mut Formatter<'_>, line: u32, code: &str) -> std::fmt::Result {
        print_line_left(f, &self.styles, '│', Some(line), self.line_col_size)?;
        f.write_str(code)?;
        f.write_char('\n')
    }

    fn print_code_line_interrupt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        print_line_left(f, &self.styles, '┆', None, self.line_col_size)?;
        f.write_char('\n')
    }

    fn print_loc_single(&self, f: &mut Formatter<'_>, loc: Loc<'_>) -> std::fmt::Result {
        print_line_left(f, &self.styles, '·', None, self.line_col_size)?;
        f.write_space(loc.start)?;
        f.write_str(self.styles.bold)?;
        f.write_str(self.color_for_style(loc.style))?;
        for _ in 0..loc.end - loc.start {
            match loc.style {
                LabeledSpanStyle::Primary => f.write_char('^')?,
                LabeledSpanStyle::Secondary => f.write_char('─')?,
            }
        }
        f.write_char(' ')?;
        f.write_str(loc.text)?;
        f.write_str(self.styles.reset)?;
        f.write_char('\n')
    }

    fn print_loc_lines(
        &self,
        f: &mut Formatter<'_>,
        locs: &[Loc<'_>],
        line_len: u32,
    ) -> std::fmt::Result {
        print_line_left(f, &self.styles, '·', None, self.line_col_size)?;
        f.write_space(locs[0].start)?;
        f.write_str(self.styles.bold)?;

        let mut last = locs[0].style;
        f.write_str(self.color_for_style(last))?;
        for i in locs[0].start..line_len {
            let loc = get_loc(i, locs);
            if let Some(loc) = loc {
                last = self.write_loc_style(f, loc.style, last)?;
                match loc.style {
                    LabeledSpanStyle::Secondary if loc.start == i => f.write_char('┬')?,
                    LabeledSpanStyle::Primary => f.write_char('^')?,
                    LabeledSpanStyle::Secondary => f.write_char('─')?,
                }
            } else {
                f.write_char(' ')?;
            }
        }
        f.write_str(self.styles.reset)?;
        f.write_char('\n')
    }

    fn print_loc_bottom_lines(&self, f: &mut Formatter<'_>, locs: &[Loc<'_>]) -> std::fmt::Result {
        print_line_left(f, &self.styles, '·', None, self.line_col_size)?;
        f.write_str(self.styles.bold)?;
        let mut last = locs[0].style;
        f.write_str(self.color_for_style(last))?;
        let mut current_char = 0;
        for loc in &locs[..locs.len() - 1] {
            last = self.write_loc_style(f, loc.style, last)?;
            f.write_space(loc.start - current_char)?;
            f.write_char('│')?;
            current_char = loc.start + 1;
        }
        let last_loc = locs[locs.len() - 1];
        self.write_loc_style(f, last_loc.style, last)?;
        f.write_space(last_loc.start - current_char)?;
        f.write_str("╰─ ")?;
        f.write_str(last_loc.text)?;
        f.write_str(self.styles.reset)?;
        f.write_char('\n')
    }

    fn print_footer(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_space(self.line_col_size + 1)?;
        f.write_str(self.styles.bold)?;
        f.write_str(self.styles.blue)?;
        f.write_str("╰──\n")?;
        f.write_str(self.styles.reset)
    }

    fn print_note(&self, f: &mut Formatter<'_>, args: Arguments<'_>) -> std::fmt::Result {
        f.write_space(self.line_col_size + 1)?;
        f.write_str(self.styles.bold)?;
        f.write_str("note")?;
        f.write_str(self.styles.reset)?;
        f.write_str(": ")?;
        f.write_fmt(args)?;
        f.write_char('\n')
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use mira_spans::{Arena, BytePos, FileId, SourceMap, Span, SpanData, interner::SpanInterner};

    use super::*;
    use crate::{Diagnostic, DiagnosticFormatter, Output, test_errors::*};

    // SAFETY: FileId is repr(transparent) around u32.
    const ZERO_FILE_ID: FileId = unsafe { std::mem::transmute(0u32) };

    const SOURCE: &str = r#"
meow :3
meow nya mwrrrrrp purrrr
nya mwrrrp purrr mreaow
"#;

    fn assert_err_eq(diagnostic: Diagnostic<'_>, err: &str) {
        let sourcemap = SourceMap::new();
        let (_, file) = sourcemap.add_package(
            Path::new("/").into(),
            Path::new("/file.mr").into(),
            SOURCE.into(),
            HashMap::new(),
        );
        assert_eq!(file.id, ZERO_FILE_ID);
        let mut formatter = DiagnosticFormatter::new(
            &sourcemap,
            Output::Custom(Box::new(String::new())),
            UnicodePrinter::new(),
            Styles::NO_COLORS,
        );
        formatter.display_diagnostic(diagnostic).unwrap();
        let lhs = formatter
            .get_output()
            .downcast_ref::<String>()
            .unwrap()
            .trim();
        let rhs = err.trim();
        assert_eq!(lhs, rhs);
    }

    #[test]
    fn errors() {
        let arena = Arena::new();
        let span_interner = SpanInterner::new(&arena);
        let span1 = SpanData::new(BytePos::from_u32(12), 3, ZERO_FILE_ID);
        let span2 = SpanData::new(BytePos::from_u32(23), 7, ZERO_FILE_ID);
        let span3 = SpanData::new(BytePos::from_u32(10), 23, ZERO_FILE_ID);
        let span1 = Span::new(span1, &span_interner);
        let span2 = Span::new(span2, &span_interner);
        let span3 = Span::new(span3, &span_interner);

        assert_err_eq(
            Diagnostic::new(ErrorSimple(12), Severity::Error),
            "error: this is a simple error with 12.",
        );
        assert_err_eq(
            Diagnostic::new(ErrorCode, Severity::Warn),
            "warning[E1834]: this has an error code",
        );
        assert_err_eq(
            Diagnostic::new(WithNote("this is an error :o"), Severity::Error),
            r#"error: error with a note
  note: this is an error :o
  note: this is an error :o"#,
        );
        assert_err_eq(
            Diagnostic::new(WithSpans(span1, span2), Severity::Warn),
            r#"warning: spans error
  ╭──[/file.mr:3:3]
  │ 
3 │ meow nya mwrrrrrp purrrr
  ·    ^^^        ┬──────   
  ·    │          ╰─ secondary
  ·    ╰─ primary
  ╰──"#,
        );
        assert_err_eq(
            Diagnostic::new(ErrorSimple(56), Severity::Error)
                .with_note("meow note")
                .with_primary_label(span2, "primary label")
                .with_secondary_label(span3, "secondary label"),
            r#"error: this is a simple error with 56.
  ╭──[/file.mr:3:14]
  │ 
3 │ meow nya mwrrrrrp purrrr
  ·  ┬────────────^^^^^^^───
  ·  │            ╰─ primary label
  ·  ╰─ secondary label
  ╰──
  note: meow note"#,
        );
    }
}
