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
        f.write_str(": ")?;
        if let Some(code) = error_code {
            f.write_fmt(format_args!("[{code}]"))?;
        }
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
