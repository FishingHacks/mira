use std::ops::RangeInclusive;

use mira_errors::ErrorData;
use mira_spans::Span;

#[derive(ErrorData, Debug)]
pub enum LexingError<'arena> {
    #[error("unknown start of token: {char}")]
    UnknownTokenError {
        #[primary_label("")]
        span: Span<'arena>,
        char: char,
    },
    #[error("Invalid Number")]
    InvalidNumberError(#[primary_label("this is not a valid number literal")] Span<'arena>),
    #[error("Expected a string closing literal, but found `<eof>`")]
    UnclosedString(#[primary_label("string defined here")] Span<'arena>),
    #[error("Invalid number type")]
    InvalidNumberType(
        #[primary_label("This number is not valid for the number type")] Span<'arena>,
    ),
    #[error("unclosed macro invocation")]
    UnclosedMacro {
        #[primary_label("expected a `{bracket}`")]
        span: Span<'arena>,
        bracket: char,
    },
    #[error("expected one of `(`, `[` or `{{`, but found `{character}`")]
    MacroExpectedBracket {
        #[primary_label("expected one of `(`, `[`, or `{{`")]
        span: Span<'arena>,
        character: char,
    },
    #[error("Module-level doc comment have to be the first token in the file.")]
    InvalidModuleDocComment(#[primary_label("")] Span<'arena>),
    #[error("numeric character escape is too short")]
    UnterminatedNumericEscape(#[primary_label("")] Span<'arena>),
    #[error("invalid character in numeric escape: `{}`", _1.escape_debug())]
    InvalidNumericEscapeChar(#[primary_label("")] Span<'arena>, char),
    #[error("out of range hex escape")]
    OutOfRangeEscape(
        #[primary_label("must be in range  \\x{:x}..=\\x{:x}", _1.start(), _1.end())] Span<'arena>,
        RangeInclusive<u8>,
    ),
    #[error("invalid escape character: `{}`. Valid escapes are: \\n, \\r, \\t, \\e, \\b, \\u{{...}}, and \\xHH", _1.escape_debug())]
    InvalidEscape(#[primary_label("")] Span<'arena>, char),

    // Unicode Escape (\u{...})

    // "\u"
    #[error("incorrect unicode escape sequence")]
    #[note("the format of unicode escape sequences is `\\u{{...}}`")]
    InvalidUnicodeEscape(#[primary_label("")] Span<'arena>),

    // "\u{<invalid char>}"
    #[error("invalid character in unicode escape sequence")]
    #[note("only hex characters are allowed (0-9, a-f, A-F) or a closing curly")]
    InvalidUnicodeEscapeChar(#[primary_label("only hex characters")] Span<'arena>),

    // "\u{..."
    #[error("unterminated unicode escape sequence")]
    UnterminatedUnicodeEscape(#[primary_label("Missing a closing `}}`")] Span<'arena>),

    // "\u{fffffff}" - the inside may only contain up to 6 characters.
    #[error("oversized unicode escape sequence")]
    TooLongUnicodeEscapeSequence(#[primary_label("must have at most 6 hex-digits")] Span<'arena>),

    // "\u{}"
    #[error("empty unicode escape")]
    EmptyUnicodeEscapeSequence(#[primary_label("must have at least 1 hex-digit")] Span<'arena>),

    // "\u{ffffff}" - may only be 0..=10ffff
    #[error("invalid  unicode escape sequence")]
    OutOfRangeUnicodeEscapeSequence(
        #[primary_label("may at most only be \\u{{10ffff}}")] Span<'arena>,
    ),
}
