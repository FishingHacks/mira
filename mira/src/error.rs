use std::{
    fmt::{Debug, Display, Write},
    path::PathBuf,
};

use thiserror::Error;

use crate::{
    annotations::AnnotationReceiver,
    codegen::CodegenError,
    globals::GlobalStr,
    tokenizer::{Location, TokenType},
    typechecking::{Type, TypecheckingError},
};

#[macro_export]
macro_rules! error_list_wrapper {
    ($(#[$meta:tt])* $vis:vis struct $name:ident($errorty:ident);
    $(
        $add_fn:ident => $error_field:ident($( $field_name:ident : $field_ty:ty ),* $(,)?)
    );* $(;)?
    ) => {
        $(#[$meta])*
        $vis struct $name(pub Vec<$errorty>);

        impl $name {
            $(
                pub fn $add_fn(&mut self$(, $field_name : $field_ty)*) {
                    self.0.push($errorty::$error_field { $($field_name),* });
                }
            )*

            pub fn into_inner(self) -> Vec<$errorty> { self.0 }
            pub fn get_inner(&self) -> &[$errorty] { &self.0 }
            pub fn len(&self) -> usize { self.0.len() }
            pub fn push(&mut self, v: $errorty) { self.0.push(v) }
            pub fn new() -> Self { Self(Vec::new()) }
        }
    };
}
error_list_wrapper!(
    pub struct MiraErrors(MiraError);
    add_parsing => Parsing(inner: ParsingError);
    add_tokenization => Tokenization(inner: TokenizationError);
    add_programforming => ProgramForming(inner: ProgramFormingError);
    add_typechecking => Typechecking(inner: TypecheckingError);
    add_codegen => Codegen(inner: CodegenError);
    add_generic => Generic(inner: &'static str);
);

#[derive(Error, Debug)]
pub enum MiraError {
    #[error("{inner}")]
    Parsing {
        #[from]
        inner: ParsingError,
    },
    #[error("{inner}")]
    Tokenization {
        #[from]
        inner: TokenizationError,
    },
    #[error("{inner}")]
    ProgramForming {
        #[from]
        inner: ProgramFormingError,
    },
    #[error("{inner}")]
    Typechecking {
        #[from]
        inner: TypecheckingError,
    },
    #[error("{inner}")]
    Codegen {
        #[from]
        inner: CodegenError,
    },
    #[error("{inner}")]
    Generic { inner: &'static str },
}

#[derive(Clone, Debug, Error)]
pub enum ProgrammingLangResolveError {
    #[error("{0}: Could not find file at `{1}`")]
    FileNotFound(Location, PathBuf),
    #[error("{0}: Could not find module `{1}`")]
    ModuleNotFound(Location, String),
    #[error("{0}: Absolute paths in use statements are not allowed")]
    AbsolutePath(Location),
}

#[derive(Clone, Debug, Error)]
pub enum ParsingError {
    #[error("{0}: {1} is an invalid intrinsic")]
    InvalidIntrinsic(Location, GlobalStr),
    #[error("{loc}: Expected a type, but found {found:?}")]
    ExpectedType { loc: Location, found: TokenType },
    #[error("{loc}: Expected a function call")]
    ExpectedFunctionCall { loc: Location },
    #[error("{loc}: Expected `,` or `)`, but found {found:?}")]
    ExpectedFunctionArgument { loc: Location, found: TokenType },
    #[error("{loc}: Expected `,` or `]`, but found {found:?}")]
    ExpectedArrayElement { loc: Location, found: TokenType },
    #[error("{loc}: Expected `,` or `}}`, but found {found:?}")]
    ExpectedObjectElement { loc: Location, found: TokenType },
    #[error("{loc}: Expected an expression or `)`, but found {found:?}")]
    ExpectedFunctionArgumentExpression { loc: Location, found: TokenType },
    #[error("{loc}: Expected `=` or `{{`, but found {found:?}")]
    ExpectedFunctionBody { loc: Location, found: TokenType },
    #[error("{loc}: Expected an expression, but found {found:?}")]
    ExpectedExpression { loc: Location, found: TokenType },
    #[error("{loc}: Expected an identifier, but found {found:?}")]
    ExpectedIdentifier { loc: Location, found: TokenType },
    #[error("{loc}: Invalid operand: {operand_type:?}")]
    InvalidOperand {
        loc: Location,
        operand_type: TokenType,
    },
    #[error("{loc}: invalid unary operand: {operand_type:?}")]
    InvalidUnaryOperand {
        loc: Location,
        operand_type: TokenType,
    },
    #[error("{loc}: Incorrect Tokenization (this was an error of the compiler! report it!)")]
    InvalidTokenization { loc: Location },
    #[error("{loc}: Invalid left-side of the assignment")]
    AssignmentInvalidLeftSide { loc: Location },
    #[error("{loc}: Expected {expected:?} but found {found:?}")]
    ExpectedArbitrary {
        loc: Location,
        expected: TokenType,
        found: TokenType,
    },
    #[error("{loc}: `{keyword}` is not allowed here")]
    InvalidKeyword {
        keyword: &'static str,
        loc: Location,
    },
    #[error(
        "{loc}: Function `{name}` is already defined\n{first_func_loc}: `{name}` is defined here"
    )]
    FunctionAlreadyDefined {
        loc: Location,
        name: GlobalStr,
        first_func_loc: Location,
    },
    #[error("{loc}: Expected {}fn or `}}`, but found {found:?}", if *.is_trait_impl { "impl, " } else { "" })]
    StructImplRegionExpect {
        loc: Location,
        found: TokenType,
        is_trait_impl: bool,
    },
    #[error("{loc}: End-of-file")]
    Eof { loc: Location },
    #[error("{loc}: Expected a statement")]
    ExpectedStatement { loc: Location },
    #[error("{loc}: Expected a let, while, if, for, block, function, struct or trait statement")]
    ExpectedAnnotationStatement { loc: Location },
    #[error("{0}")]
    ModuleResolution(#[from] ProgrammingLangResolveError),
    #[error("{loc}: expected a statement, but found an expression")]
    ExpressionAtTopLevel { loc: Location },
    #[error("{loc}: annotation `{name}` cannot go on a {thing}")]
    AnnotationDoesNotGoOn {
        loc: Location,
        name: &'static str,
        thing: AnnotationReceiver,
    },
    #[error("{loc}: Unknown annotation `{name}`")]
    UnknownAnnotation { loc: Location, name: String },
}

impl ParsingError {
    pub fn get_loc(&self) -> &Location {
        match self {
            Self::InvalidIntrinsic(loc, _)
            | Self::ExpectedType { loc, .. }
            | Self::AnnotationDoesNotGoOn { loc, .. }
            | Self::ExpectedExpression { loc, .. }
            | Self::ExpectedIdentifier { loc, .. }
            | Self::InvalidOperand { loc, .. }
            | Self::ExpectedArrayElement { loc, .. }
            | Self::ExpectedObjectElement { loc, .. }
            | Self::ExpectedFunctionArgument { loc, .. }
            | Self::ExpectedFunctionArgumentExpression { loc, .. }
            | Self::ExpectedFunctionBody { loc, .. }
            | Self::ExpectedFunctionCall { loc }
            | Self::InvalidTokenization { loc }
            | Self::AssignmentInvalidLeftSide { loc }
            | Self::Eof { loc }
            | Self::ExpressionAtTopLevel { loc }
            | Self::ExpectedAnnotationStatement { loc }
            | Self::StructImplRegionExpect { loc, .. }
            | Self::ExpectedArbitrary { loc, .. }
            | Self::InvalidUnaryOperand { loc, .. }
            | Self::FunctionAlreadyDefined { loc, .. }
            | Self::UnknownAnnotation { loc, .. }
            | Self::ExpectedStatement { loc, .. }
            | Self::InvalidKeyword { loc, .. } => loc,
            Self::ModuleResolution(err) => match err {
                ProgrammingLangResolveError::FileNotFound(loc, _)
                | ProgrammingLangResolveError::ModuleNotFound(loc, _)
                | ProgrammingLangResolveError::AbsolutePath(loc) => loc,
            },
        }
    }
}

#[derive(Clone, Error, Debug)]
pub enum TokenizationError {
    #[error("{loc}: Unknown token `{character}`")]
    UnknownTokenError { loc: Location, character: char },
    #[error("{loc}: Could not parse the number")]
    InvalidNumberError { loc: Location },
    #[error("{loc}: Expected `\"`, but found nothing")]
    UnclosedString { loc: Location },
    #[error("{0}: Invalid number type")]
    InvalidNumberType(Location),
    #[error("{loc}: unclosed macro invocation (Expected a `{bracket}`))")]
    UnclosedMacro { loc: Location, bracket: char },
    #[error("{loc}: expected a bracket (`(`, `[` or `{{`), but found {character}")]
    MacroExpectedBracket { loc: Location, character: char },
}

impl TokenizationError {
    pub fn invalid_number(loc: Location) -> Self {
        Self::InvalidNumberError { loc }
    }
    pub fn unclosed_string(loc: Location) -> Self {
        Self::UnclosedString { loc }
    }
    pub fn unknown_token(loc: Location, character: char) -> Self {
        Self::UnknownTokenError { loc, character }
    }

    pub fn get_loc(&self) -> &Location {
        match self {
            Self::UnclosedString { loc }
            | Self::InvalidNumberError { loc }
            | Self::InvalidNumberType(loc)
            | Self::UnclosedMacro { loc, .. }
            | Self::MacroExpectedBracket { loc, .. }
            | Self::UnknownTokenError { loc, .. } => &loc,
        }
    }
}

#[derive(Clone, Debug, Error)]
pub enum ProgramFormingError {
    #[error("{0}: Code outside of a function boundary")]
    NoCodeOutsideOfFunctions(Location),
    #[error("{0}: There are no anonymous functions at global level allowed")]
    AnonymousFunctionAtGlobalLevel(Location),
    #[error("{0}: global-level let or const expects you to pass a literal")]
    GlobalValueNoLiteral(Location),
    #[error("{0}: global-level const expects you to pass a type")]
    GlobalValueNoType(Location),
    #[error("{0}: could not find `{1}` in the current module")]
    IdentNotDefined(Location, GlobalStr),
    #[error("{0}: `{1}` is already defined in the current module")]
    IdentAlreadyDefined(Location, GlobalStr),
}

pub struct FunctionList<'a>(pub &'a [Type]);

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
