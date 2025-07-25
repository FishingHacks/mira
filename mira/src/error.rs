use std::fmt::{Debug, Display, Write};

use thiserror::Error;

use crate::{
    annotations::AnnotationReceiver,
    codegen::CodegenError,
    linking::LinkerError,
    string_interner::InternedStr,
    tokenizer::{Location, TokenType},
    typechecking::{Type, TypecheckingError},
};

#[macro_export]
macro_rules! error_list_wrapper {
    ($(#[$meta:tt])* $vis:vis struct $name:ident$(< $($lifetime:tt),* $(,)? >)?($errorty:ident);
    $(
        $add_fn:ident => $error_field:ident($( $field_name:ident : $field_ty:ty ),* $(,)?)
    );* $(;)?
    ) => {
        $(#[$meta])*
        #[derive(Default)]
        $vis struct $name $(<$($lifetime),*>)? (pub Vec<$errorty $(<$($lifetime),*>)?>);

        impl $(<$($lifetime),*>)? $name $(<$($lifetime),*>)? {
            $(
                pub fn $add_fn(&mut self$(, $field_name : $field_ty)*) {
                    self.0.push($errorty::$error_field { $($field_name),* });
                }
            )*

            pub fn into_inner(self) -> Vec<$errorty $(<$($lifetime),*>)?> { self.0 }
            pub fn get_inner(&self) -> &[$errorty] { &self.0 }
            pub fn len(&self) -> usize { self.0.len() }
            pub fn is_empty(&self) -> bool { self.0.is_empty() }
            pub fn push(&mut self, v: $errorty $(<$($lifetime),*>)?) { self.0.push(v) }
        }
    };
}
error_list_wrapper!(
    pub struct MiraErrors<'arena>(MiraError);
    add_parsing => Parsing(inner: ParsingError<'arena>);
    add_tokenization => Tokenization(inner: TokenizationError);
    add_programforming => ProgramForming(inner: ProgramFormingError<'arena>);
    add_typechecking => Typechecking(inner: TypecheckingError<'arena>);
    add_codegen => Codegen(inner: CodegenError);
    add_generic => Generic(inner: &'static str);
    add_io => IO(inner: std::io::Error);
);

macro_rules! from {
    ($name:ident ($ty:ty) $func:ident) => {
        impl<'arena> MiraError<'arena> {
            pub fn $func(value: $ty) -> Self {
                Self::$name { inner: value }
            }
        }
    };
}

from!(Parsing(ParsingError<'arena>) parsing);
from!(ProgramForming(ProgramFormingError<'arena>) program_forming);
from!(Typechecking(TypecheckingError<'arena>) typechecking);
from!(Tokenization(TokenizationError) tokenization);
from!(Codegen(CodegenError) codegen);

#[derive(Error, Debug)]
pub enum MiraError<'arena> {
    #[error("{inner}")]
    Parsing { inner: ParsingError<'arena> },
    #[error("{inner}")]
    Tokenization { inner: TokenizationError },
    #[error("{inner}")]
    ProgramForming { inner: ProgramFormingError<'arena> },
    #[error("{inner}")]
    Typechecking { inner: TypecheckingError<'arena> },
    #[error("{inner}")]
    Codegen { inner: CodegenError },
    #[error("{inner}")]
    Generic { inner: &'static str },
    #[error("{inner}")]
    IO {
        #[from]
        inner: std::io::Error,
    },
    #[error("{inner}")]
    Linking {
        #[from]
        inner: LinkerError,
    },
}

#[derive(Clone, Debug, Error)]
pub enum ParsingError<'arena> {
    #[error("{loc}: Cannot resolved import {name:?}")]
    CannotResolveModule {
        loc: Location,
        name: InternedStr<'arena>,
    },
    #[error("{loc}: Expected let, fn, extern, struct, use or trait, but found {typ:?}")]
    ExpectedElementForPub { loc: Location, typ: TokenType },
    #[error("{loc}: Output constraint must start with `=`")]
    OutputNotStartingWithEqual {
        loc: Location,
        output: InternedStr<'arena>,
    },
    #[error("{loc}: Input constraint cannot start with `=` or `~`")]
    InputStartingWithInvalidChar {
        loc: Location,
        input: InternedStr<'arena>,
    },
    #[error("{loc}: Duplicate Replacer `{replacer}`")]
    DuplicateAsmReplacer {
        loc: Location,
        replacer: InternedStr<'arena>,
    },
    #[error("{loc}: {name} is an invalid function attribute")]
    InvalidFunctionAttribute {
        loc: Location,
        name: InternedStr<'arena>,
    },
    #[error("{loc}: {name} is an invalid intrinsic")]
    InvalidIntrinsic {
        loc: Location,
        name: InternedStr<'arena>,
    },
    #[error("{loc}: {name} is an invalid calling convention")]
    InvalidCallConv {
        loc: Location,
        name: InternedStr<'arena>,
    },
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
    #[error("{loc}: Incorrect Tokenization (this was an error of the compiler! report it!)")]
    InvalidTokenization { loc: Location },
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
        name: InternedStr<'arena>,
        first_func_loc: Location,
    },
    #[error("{loc}: Expected {}fn or `}}`, but found {found:?}", if *.is_trait_impl { "impl, " } else { "" })]
    StructImplRegionExpect {
        loc: Location,
        found: TokenType,
        is_trait_impl: bool,
    },
    #[error("{loc}: Expected a statement")]
    ExpectedStatement { loc: Location },
    #[error("{loc}: Expected a let, while, if, for, block, function, struct or trait statement")]
    ExpectedAnnotationStatement { loc: Location },
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

impl ParsingError<'_> {
    pub fn get_loc(&self) -> &Location {
        match self {
            Self::InvalidIntrinsic { loc, .. }
            | Self::InvalidCallConv { loc, .. }
            | Self::InvalidFunctionAttribute { loc, .. }
            | Self::ExpectedElementForPub { loc, .. }
            | Self::OutputNotStartingWithEqual { loc, .. }
            | Self::InputStartingWithInvalidChar { loc, .. }
            | Self::DuplicateAsmReplacer { loc, .. }
            | Self::ExpectedType { loc, .. }
            | Self::AnnotationDoesNotGoOn { loc, .. }
            | Self::ExpectedExpression { loc, .. }
            | Self::ExpectedIdentifier { loc, .. }
            | Self::ExpectedArrayElement { loc, .. }
            | Self::ExpectedObjectElement { loc, .. }
            | Self::ExpectedFunctionArgument { loc, .. }
            | Self::ExpectedFunctionArgumentExpression { loc, .. }
            | Self::ExpectedFunctionBody { loc, .. }
            | Self::ExpectedFunctionCall { loc }
            | Self::InvalidTokenization { loc }
            | Self::ExpressionAtTopLevel { loc }
            | Self::ExpectedAnnotationStatement { loc }
            | Self::StructImplRegionExpect { loc, .. }
            | Self::ExpectedArbitrary { loc, .. }
            | Self::FunctionAlreadyDefined { loc, .. }
            | Self::UnknownAnnotation { loc, .. }
            | Self::ExpectedStatement { loc, .. }
            | Self::CannotResolveModule { loc, .. }
            | Self::InvalidKeyword { loc, .. } => loc,
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
            | Self::UnknownTokenError { loc, .. } => loc,
        }
    }
}

#[derive(Clone, Debug, Error)]
pub enum ProgramFormingError<'arena> {
    #[error("{0}: Code outside of a function boundary")]
    NoCodeOutsideOfFunctions(Location),
    #[error("{0}: There are no anonymous functions at global level allowed")]
    AnonymousFunctionAtGlobalLevel(Location),
    #[error("{0}: global-level let or const expects you to pass a literal")]
    GlobalValueNoLiteral(Location),
    #[error("{0}: global-level const expects you to pass a type")]
    GlobalValueNoType(Location),
    #[error("{0}: could not find `{1}` in the current module")]
    IdentNotDefined(Location, InternedStr<'arena>),
    #[error("{0}: `{1}` is already defined in the current module")]
    IdentAlreadyDefined(Location, InternedStr<'arena>),
}

pub struct FunctionList<'a>(pub &'a [Type<'a>]);

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
