mod ascii;
mod unicode;
use crate::{Formatter, LabeledSpanStyle, Severity};
pub use ascii::AsciiPrinter;
pub use unicode::UnicodePrinter;

use std::fmt::Arguments;
use std::path::Path;

pub trait StyledPrinter {
    fn configure(&mut self, styles: Styles, severity: Severity, line_col_size: u32);
    fn print_message(
        &self,
        f: &mut Formatter<'_>,
        error_code: Option<&str>,
        message: Arguments<'_>,
    ) -> std::fmt::Result;
    fn print_header(
        &self,
        f: &mut Formatter<'_>,
        file: &Path,
        line_col: Option<(u32, u32)>,
    ) -> std::fmt::Result;
    fn print_code_line(&self, f: &mut Formatter<'_>, line: u32, code: &str) -> std::fmt::Result;
    fn print_code_line_interrupt(&self, f: &mut Formatter<'_>) -> std::fmt::Result;
    fn print_loc_single(&self, f: &mut Formatter<'_>, loc: Loc<'_>) -> std::fmt::Result;
    fn print_loc_lines(
        &self,
        f: &mut Formatter<'_>,
        locs: &[Loc<'_>],
        line_len: u32,
    ) -> std::fmt::Result;
    fn print_loc_bottom_lines(&self, f: &mut Formatter<'_>, locs: &[Loc<'_>]) -> std::fmt::Result;
    fn print_footer(&self, f: &mut Formatter<'_>) -> std::fmt::Result;
    fn print_note(&self, f: &mut Formatter<'_>, args: Arguments<'_>) -> std::fmt::Result;
}

impl Styles {
    pub const DEFAULT: Styles = Styles {
        red: "\u{1b}[91m",
        yellow: "\u{1b}[93m",
        reset: "\u{1b}[0m",
        bold: "\u{1b}[1m",
        blue: "\u{1b}[34m",
        reset_fg: "\u{1b}[39m",
    };
    pub const NO_COLORS: Styles = Styles {
        red: "",
        yellow: "",
        reset: "",
        bold: "",
        blue: "",
        reset_fg: "",
    };
}

#[derive(Clone, Copy)]
pub struct Styles {
    pub red: &'static str,
    pub yellow: &'static str,
    pub reset: &'static str,
    pub bold: &'static str,
    pub blue: &'static str,
    pub reset_fg: &'static str,
}

/// Prints the line number (if one is given) padded with the seperator between the code and the line
/// numbers and highlights them.
///
/// $num $seperator
pub fn print_line_left(
    f: &mut Formatter<'_>,
    styles: &Styles,
    seperator: char,
    num: Option<u32>,
    sz: u32,
) -> std::fmt::Result {
    f.write_str(styles.bold)?;
    f.write_str(styles.blue)?;
    match num {
        Some(num) => f.write_fmt(format_args!("{0:<1$}", num, sz as usize + 1))?,
        None => f.write_space(sz + 1)?,
    }
    f.write_char(seperator)?;
    f.write_str(styles.reset)?;
    f.write_char(' ')
}

fn color_from_severity(styles: &Styles, severity: Severity) -> &'static str {
    match severity {
        Severity::Warn => styles.yellow,
        Severity::Error => styles.red,
    }
}

fn get_loc<'a, 'b>(pos: u32, locs: &'a [Loc<'b>]) -> Option<&'a Loc<'b>> {
    let mut best_fitting = None;
    for loc in locs {
        if loc.start <= pos && loc.end > pos {
            best_fitting = Some(loc);
        } else if loc.start > pos {
            break;
        }
    }
    best_fitting
}

#[derive(Clone, Copy)]
/// A Labeled span's location in a file, including it's style, text, line, start and end.
pub struct Loc<'a> {
    pub text: &'a str,
    pub line: u32,
    pub start: u32,
    pub end: u32,
    pub style: LabeledSpanStyle,
}
