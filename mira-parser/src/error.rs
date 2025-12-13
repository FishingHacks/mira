use std::{
    fmt::{Debug, Display},
    path::PathBuf,
};

use mira_macros::ErrorData;

use crate::annotations::AnnotationReceiver;
use mira_lexer::{Token, TokenType, token::IdentDisplay};
use mira_spans::{Span, interner::Symbol};
struct ExpectedOneOfDisplay<'a, T>(&'a [T]);

impl<T: Display> Display for ExpectedOneOfDisplay<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.is_empty() {
            return Ok(());
        }
        f.write_fmt(format_args!("{}", self.0[0]))?;
        if self.0.len() < 2 {
            return Ok(());
        }
        for el in &self.0[1..self.0.len() - 1] {
            f.write_fmt(format_args!(", {el}"))?;
        }
        f.write_fmt(format_args!(", or {}", self.0[self.0.len() - 1]))
    }
}

struct Opt<'ctx, 'a>(&'a Option<Token<'ctx>>);
impl Display for Opt<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Some(v) => Display::fmt(v, f),
            None => f.write_str("nothing"),
        }
    }
}

#[derive(Debug, ErrorData)]
pub enum ParsingError<'arena> {
    #[error("Unmatched parenthese")]
    UnmatchedParen(
        #[secondary_label("unclosed delimiter")] Span<'arena>,
        #[primary_label("")] Span<'arena>,
    ),
    #[error("Invalid meta variable type {}", IdentDisplay(*_1))]
    #[note("Valid meta types are: tok, token, and ident")]
    InvalidMetaVarType(
        #[primary_label("invalid meta var type")] Span<'arena>,
        Symbol<'arena>,
    ),
    #[error("Expected {expected}, but found {}.", Opt(found))]
    Expected {
        #[primary_label("Expected {expected}")]
        span: Span<'arena>,
        expected: TokenType,
        found: Option<Token<'arena>>,
    },
    #[error("Expected eof, but found {found}.")]
    ExpectedEnd {
        #[primary_label("Expected eof")]
        span: Span<'arena>,
        found: Token<'arena>,
    },
    #[error("Expected {expected} but found {found}")]
    ExpectedToktype {
        #[primary_label("Expected {expected}")]
        span: Span<'arena>,
        expected: TokenType,
        found: TokenType,
    },
    #[error(
        "Expected one of {}, but found {}.",
        ExpectedOneOfDisplay(valid),
        Opt(found)
    )]
    ExpectedOneOf {
        #[primary_label("Expected one of {}", ExpectedOneOfDisplay(valid))]
        span: Span<'arena>,
        valid: &'static [TokenType],
        found: Option<Token<'arena>>,
    },
    #[error("Expected kleene op (+, * or ?) or a seperator token")]
    ExpectedKleeneOpOrSep {
        #[primary_label("Expected any token but '[', ']', '(', ')', '{{', '}}'")]
        span: Span<'arena>,
    },
    #[error("unresolved import `{}`", IdentDisplay(*name))]
    CannotResolveModule {
        #[primary_label("cannot find module {}", IdentDisplay(*name))]
        span: Span<'arena>,
        name: Symbol<'arena>,
    },
    #[error(
        "Expected one of let, fn, extern, struct, use, or trait, but found {}",
        Opt(found)
    )]
    ExpectedElementForPub {
        #[primary_label("Expected one of let, fn, extern, struct, use, or trait")]
        span: Span<'arena>,
        found: Option<Token<'arena>>,
    },
    #[error("Output register must start with '='")]
    #[note("Try using \"={}\"", output.escape_debug())]
    OutputNotStartingWithEqual {
        #[primary_label("Register isn't starting with an '='")]
        span: Span<'arena>,
        output: Symbol<'arena>,
    },
    #[error("Input register cannot start with '=' or '~'")]
    #[note("Try using {:?}", &input[1..])]
    InputStartingWithInvalidChar {
        #[primary_label("Register is starting with either '=' or '~'")]
        span: Span<'arena>,
        input: Symbol<'arena>,
    },
    #[error("A bound register with name {} was already defined", IdentDisplay(*name))]
    DuplicateAsmReplacer {
        #[primary_label("redefinition here")]
        span: Span<'arena>,
        name: Symbol<'arena>,
    },
    #[error("Attribute `{}` cannot be applied to functions", IdentDisplay(*name))]
    InvalidFunctionAttribute {
        #[primary_label("attribute applied here")]
        span: Span<'arena>,
        name: Symbol<'arena>,
    },
    #[error("{} is an invalid intrinsic", IdentDisplay(*name))]
    InvalidIntrinsic {
        #[primary_label("no such intrinsic")]
        span: Span<'arena>,
        name: Symbol<'arena>,
    },
    #[error("invalid calling convention: found `{}`", IdentDisplay(*name))]
    InvalidCallConv {
        #[primary_label("invalid calling convention")]
        span: Span<'arena>,
        name: Symbol<'arena>,
    },
    #[error("Expected a type, but found {}", Opt(found))]
    ExpectedType {
        #[primary_label("expected a type")]
        span: Span<'arena>,
        found: Option<Token<'arena>>,
    },
    #[error("Expected a function call")]
    ExpectedFunctionCall {
        #[primary_label("expected a function call")]
        span: Span<'arena>,
    },
    #[error("Expected one of ',', or ')', but found {}", Opt(found))]
    ExpectedFunctionArgument {
        #[primary_label("Expected one of ',', or ')'")]
        span: Span<'arena>,
        found: Option<Token<'arena>>,
    },
    #[error("Expected one of ',', or ']', but found {found}")]
    ExpectedArrayElement {
        #[primary_label("Expected one of ',', or ']'")]
        span: Span<'arena>,
        found: TokenType,
    },
    #[error("Expected an expression or ')', but found {found}")]
    ExpectedFunctionArgumentExpression {
        #[primary_label("Expected an expression or ')'")]
        span: Span<'arena>,
        found: TokenType,
    },
    #[error("Expected one of '=', or '{{', but found {}", Opt(found))]
    ExpectedFunctionBody {
        #[primary_label("Expected one of '=', or {{")]
        span: Span<'arena>,
        found: Option<Token<'arena>>,
    },
    #[error("Expected an expression, but found {found}")]
    ExpectedExpression {
        #[primary_label("expected an expression")]
        span: Span<'arena>,
        found: TokenType,
    },
    #[error("Expected an identifier, but found {found}")]
    ExpectedIdentifier {
        #[primary_label("Expected an identifier")]
        span: Span<'arena>,
        found: TokenType,
    },
    #[error("Incorrect Tokenization")]
    #[note("This is a compiler error, report it: https://codeberg.org/fishinghacks/mira")]
    InvalidTokenization(#[primary_label("")] Span<'arena>),
    #[error("Keyword `{keyword}` is not allowed here")]
    InvalidKeyword {
        #[primary_label("Keyword not allowed")]
        span: Span<'arena>,
        keyword: &'static str,
    },
    #[error("Redefinition of name {}", IdentDisplay(*name))]
    ItemAlreadyDefined {
        #[primary_label("redefinition here")]
        span: Span<'arena>,
        name: Symbol<'arena>,
        #[primary_label("`{name}` was originally defined here")]
        first: Span<'arena>,
    },
    #[error("Redefinition of name `{}`", IdentDisplay(*name))]
    FunctionAlreadyDefined {
        #[primary_label("redefinition here")]
        span: Span<'arena>,
        name: Symbol<'arena>,
        #[primary_label("`{name}` was originally defined here")]
        first_func_span: Span<'arena>,
    },
    #[error("Expected one of {}fn or '}}', but found {}", is_trait_impl.then_some("impl, ").unwrap_or(""), Opt(found))]
    StructImplRegionExpect {
        #[primary_label("Expected one of {}fn or '}}'", is_trait_impl.then_some("impl, ").unwrap_or(""))]
        span: Span<'arena>,
        found: Option<Token<'arena>>,
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
        span: Span<'arena>,
        name: &'static str,
        thing: AnnotationReceiver,
    },
    #[error("Unknown annotation `{name}`")]
    UnknownAnnotation {
        #[primary_label("Undefined annotation")]
        span: Span<'arena>,
        name: String,
    },
    #[error("{error}")]
    ArbitraryError {
        error: String,
        label: String,
        #[primary_label("{label}")]
        label_span: Span<'arena>,
    },
    #[error("Cannot chain comparison operators")]
    #[note("use `::<...>` instead of `<...>` to specify type arguments.")]
    ChainedGenericLikeComparison(#[primary_label("")] Span<'arena>),
    #[error("Cannot have a defer or return inside of a defer.")]
    DeferOrReturnInDefer(#[primary_label("")] Span<'arena>),
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
    #[error("cannot find value `{}` in this scope", IdentDisplay(*_1))]
    IdentNotDefined(
        #[primary_label("export defined here")] Span<'arena>,
        Symbol<'arena>,
    ),
    #[error("the name `{}` is defined multiple times", IdentDisplay(*_1))]
    IdentAlreadyDefined(
        #[primary_label("`{}` redefined here", IdentDisplay(*_1))] Span<'arena>,
        Symbol<'arena>,
    ),
    #[error("{} is not a valid module name", IdentDisplay(*_0))]
    #[note("Filenames cannot contain '.', '\\0', '<', '>', ':', '\"', '/', '\\', '|', '?' or '*'.")]
    InvalidFileNameErr(
        Symbol<'arena>,
        #[primary_label("this character is not allowed")] Span<'arena>,
    ),
    #[error("Could not find module {}", IdentDisplay(*_0))]
    #[note("to create the module {1}, create file `{0}/{2}.mr` or `{0}/{3}/mod.mr`", _2.display(), IdentDisplay(*_0), _0.to_str().escape_debug(), _0.to_str().escape_debug())]
    #[note(
        "if there is a `mod {_0}` elsewhere in the package, import it with `use crate::...` instead", _0 = IdentDisplay(*_0)
    )]
    FileNotFoundErr(Symbol<'arena>, #[primary_label("")] Span<'arena>, PathBuf),
}
