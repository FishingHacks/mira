use std::{
    fmt::{Debug, Display, Write},
    path::PathBuf,
};

pub use mira_errors::{Diagnostic, Diagnostics};
use mira_macros::ErrorData;

use crate::{annotations::AnnotationReceiver, typechecking::Ty};
use mira_lexer::{Token, TokenType};
use mira_spans::{Span, interner::Symbol};

#[derive(ErrorData)]
#[error("couldn't write `{}`: {_1}", _0.display())]
#[no_arena_lifetime]
pub struct IoWriteError(pub PathBuf, pub std::io::Error);

#[derive(ErrorData)]
#[error("couldn't read `{}`: {_1}", _0.display())]
#[no_arena_lifetime]
pub struct IoReadError(pub PathBuf, pub std::io::Error);

#[derive(ErrorData)]
#[error("failed to get the working directory: {_0}")]
#[no_arena_lifetime]
pub struct CurrentDirError(pub std::io::Error);

#[derive(ErrorData)]
#[error("failed to open `{}`: {_1}", _0.display())]
#[no_arena_lifetime]
pub struct FileOpenError(pub PathBuf, pub std::io::Error);

#[derive(ErrorData)]
#[error("failed to write the IR")]
#[no_arena_lifetime]
pub struct FailedToWriteIR;

struct ExpectedOneOfDisplay<'a, T>(&'a [T]);
impl<T: Display> Display for ExpectedOneOfDisplay<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.is_empty() {
            return Ok(());
        }
        f.write_fmt(format_args!("`{}`", self.0[0]))?;
        if self.0.len() < 2 {
            return Ok(());
        }
        for el in &self.0[1..self.0.len() - 1] {
            f.write_fmt(format_args!(", `{el}`"))?;
        }
        f.write_fmt(format_args!(", or `{}`", self.0[self.0.len() - 1]))
    }
}

#[derive(Debug, ErrorData)]
pub enum ParsingError<'arena> {
    #[error("Unmatched parenthese")]
    UnmatchedParen(
        #[secondary_label("unclosed delimiter")] Span<'arena>,
        #[primary_label("")] Span<'arena>,
    ),
    #[error("Invalid meta variable type `{_1}`")]
    #[note("Valid meta types are: `tok`, `token`, and `ident`")]
    InvalidMetaVarType(
        #[primary_label("invalid meta var type")] Span<'arena>,
        Symbol<'arena>,
    ),
    #[error("Expected `{expected}`, but found `{found}`.")]
    Expected {
        #[primary_label("Expected `{expected}`")]
        loc: Span<'arena>,
        expected: TokenType,
        found: Token<'arena>,
    },
    #[error("Expected `{expected}` but found `{found}`")]
    ExpectedToktype {
        #[note("Expected `{expected}`")]
        loc: Span<'arena>,
        expected: TokenType,
        found: TokenType,
    },
    #[error(
        "Expected one of {}, but found `{found}`.",
        ExpectedOneOfDisplay(valid)
    )]
    ExpectedOneOf {
        #[primary_label("Expected one of {}", ExpectedOneOfDisplay(valid))]
        loc: Span<'arena>,
        valid: &'static [TokenType],
        found: Token<'arena>,
    },
    #[error("unresolved import `{name}`")]
    CannotResolveModule {
        #[primary_label("cannot find module `{name}`")]
        loc: Span<'arena>,
        name: Symbol<'arena>,
    },
    #[error(
        "Expected one of `let`, `fn`, `extern`, `struct`, `use`, or `trait`, but found {typ:?}"
    )]
    ExpectedElementForPub {
        #[primary_label("Expected one of `let`, `fn`, `extern`, `struct`, `use`, or `trait`")]
        loc: Span<'arena>,
        typ: TokenType,
    },
    #[error("Output register must start with `=`")]
    #[note("Try using \"={}\"", output.escape_debug())]
    OutputNotStartingWithEqual {
        #[primary_label("Register isn't starting with an `=`")]
        loc: Span<'arena>,
        output: Symbol<'arena>,
    },
    #[error("Input register cannot start with `=` or `~`")]
    #[note("Try using {:?}", &input[1..])]
    InputStartingWithInvalidChar {
        #[primary_label("Register is starting with either `=` or `~`")]
        loc: Span<'arena>,
        input: Symbol<'arena>,
    },
    #[error("A bound register with name `{name}` was already defined")]
    DuplicateAsmReplacer {
        #[primary_label("redefinition here")]
        loc: Span<'arena>,
        name: Symbol<'arena>,
    },
    #[error("Attribute `{name}` cannot be applied to functions")]
    InvalidFunctionAttribute {
        #[primary_label("attribute applied here")]
        loc: Span<'arena>,
        name: Symbol<'arena>,
    },
    #[error("{name} is an invalid intrinsic")]
    InvalidIntrinsic {
        #[primary_label("no such intrinsic")]
        loc: Span<'arena>,
        name: Symbol<'arena>,
    },
    #[error("invalid calling convention: found `{name}`")]
    InvalidCallConv {
        #[primary_label("invalid calling convention")]
        loc: Span<'arena>,
        name: Symbol<'arena>,
    },
    #[error("Expected a type, but found `{found}`")]
    ExpectedType {
        #[primary_label("expected a type")]
        loc: Span<'arena>,
        found: TokenType,
    },
    #[error("Expected a function call")]
    ExpectedFunctionCall {
        #[primary_label("expected a function call")]
        loc: Span<'arena>,
    },
    #[error("Expected one of `,`, or `)`, but found {found}")]
    ExpectedFunctionArgument {
        #[primary_label("Expected one of `,`, or `)`")]
        loc: Span<'arena>,
        found: Token<'arena>,
    },
    #[error("Expected one of `,`, or `]`, but found `{found}`")]
    ExpectedArrayElement {
        #[primary_label("Expected one of `,`, or `]`")]
        loc: Span<'arena>,
        found: TokenType,
    },
    #[error("Expected an expression or `)`, but found `{found}`")]
    ExpectedFunctionArgumentExpression {
        #[primary_label("Expected an expression or `)`")]
        loc: Span<'arena>,
        found: TokenType,
    },
    #[error("Expected one of `=`, or `{{`, but found `{found}`")]
    ExpectedFunctionBody {
        #[primary_label("Expected one of `=`, or `{{`")]
        loc: Span<'arena>,
        found: TokenType,
    },
    #[error("Expected an expression, but found `{found}`")]
    ExpectedExpression {
        #[primary_label("expected an expression")]
        loc: Span<'arena>,
        found: TokenType,
    },
    #[error("Expected an identifier, but found `{found}`")]
    ExpectedIdentifier {
        #[primary_label("Expected an identifier")]
        loc: Span<'arena>,
        found: TokenType,
    },
    #[error("Incorrect Tokenization")]
    #[note("This is a compiler error, report it: https://codeberg.org/fishinghacks/mira")]
    InvalidTokenization(#[primary_label("")] Span<'arena>),
    #[error("Keyword `{keyword}` is not allowed here")]
    InvalidKeyword {
        #[primary_label("Keyword not allowed")]
        loc: Span<'arena>,
        keyword: &'static str,
    },
    #[error("Redefinition of name `{name}`")]
    ItemAlreadyDefined {
        #[primary_label("redefinition here")]
        loc: Span<'arena>,
        name: Symbol<'arena>,
        #[primary_label("`{name}` was originally defined here")]
        first: Span<'arena>,
    },
    #[error("Redefinition of name `{name}`")]
    FunctionAlreadyDefined {
        #[primary_label("redefinition here")]
        loc: Span<'arena>,
        name: Symbol<'arena>,
        #[primary_label("`{name}` was originally defined here")]
        first_func_loc: Span<'arena>,
    },
    #[error("{loc:?}: Expected one of {}fn or `}}`, but found {found:?}", is_trait_impl.then_some("impl, ").unwrap_or(""))]
    StructImplRegionExpect {
        #[primary_label("Expected one of {}fn or `}}`", is_trait_impl.then_some("impl, ").unwrap_or(""))]
        loc: Span<'arena>,
        found: TokenType,
        is_trait_impl: bool,
    },
    #[error("Expected a statement")]
    ExpectedStatement(#[primary_label("Expected a statement")] Span<'arena>),
    #[error("Not a valid statement  for annotations")]
    ExpectedAnnotationStatement(#[primary_label("Invalid statement for annotations")] Span<'arena>),
    #[error("expected a statement")]
    ExpressionAtTopLevel(#[primary_label("Expected a statement here")] Span<'arena>),
    #[error("{thing} is not a valid receiver for annotation `{name}`")]
    AnnotationDoesNotGoOn {
        #[primary_label("annotation `{name}` is invalid here")]
        loc: Span<'arena>,
        name: &'static str,
        thing: AnnotationReceiver,
    },
    #[error("Unknown annotation `{name}`")]
    UnknownAnnotation {
        #[primary_label("Undefined annotation")]
        loc: Span<'arena>,
        name: String,
    },
    #[error("`{_0}` is not a valid module name")]
    #[note("Filenames cannot contain `.`, `\\0`, `<`, `>`, `:`, `\"`, `/`, `\\`, `|`, `?` or `*`.")]
    InvalidFileNameErr(
        Symbol<'arena>,
        #[primary_label("this character is not allowed")] Span<'arena>,
    ),
    #[error("Could not find module `{_0}`")]
    #[note("to create the module `{_0}`, create file `{0}/{_0}.mr` or `{0}/{_0}/mod.mr`", _2.display())]
    #[note(
        "if there is a `mod {_0}` elsewhere in the package, import it with `use crate::...` instead"
    )]
    FileNotFoundErr(Symbol<'arena>, #[primary_label("")] Span<'arena>, PathBuf),
}

#[derive(Debug, ErrorData)]
pub enum ProgramFormingError<'arena> {
    #[error("Expected a Module item, but found a statement instead")]
    NoCodeOutsideOfFunctions(#[primary_label("statement found here")] Span<'arena>),
    #[error("There are no anonymous functions allowed at the module level")]
    AnonymousFunctionAtGlobalLevel(#[primary_label("anonymous function found here")] Span<'arena>),
    #[error("statics and constants only support literal values")]
    GlobalValueNoLiteral(
        #[primary_label("this static or constant does not have a literal")] Span<'arena>,
    ),
    #[error("missing type for static or constant item in the module context")]
    GlobalValueNoType(#[primary_label("this static or constant needs a type")] Span<'arena>),
    #[error("cannot find value `{_1}` in this scope")]
    IdentNotDefined(
        #[primary_label("export defined here")] Span<'arena>,
        Symbol<'arena>,
    ),
    #[error("the name `{_1}` is defined multiple times")]
    IdentAlreadyDefined(
        #[primary_label("`{_1}` redefined here")] Span<'arena>,
        Symbol<'arena>,
    ),
}

pub struct FunctionList<'a>(pub &'a [Ty<'a>]);

impl Display for FunctionList<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("fn(")?;
        for i in 0..self.0.len() {
            if i != 0 {
                f.write_str(", ")?;
            }
            Display::fmt(&self.0[i], f)?;
        }
        f.write_char(')')
    }
}

pub type Result<'arena, T = ()> = std::result::Result<T, Diagnostic<'arena>>;
pub type StdResult<T, E> = std::result::Result<T, E>;
