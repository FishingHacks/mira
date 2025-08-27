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
        loc: Span<'arena>,
        bracket: char,
    },
    #[error("expected one of `(`, `[` or `{{`, but found `{character}`")]
    MacroExpectedBracket {
        #[primary_label("expected one of `(`, `[`, or `{{`")]
        loc: Span<'arena>,
        character: char,
    },
    #[error("Module-level doc comment have to be the first token in the file.")]
    InvalidModuleDocComment(#[primary_label("")] Span<'arena>),
}
