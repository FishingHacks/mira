use std::{
    collections::HashMap,
    fmt::{Display, Write},
};

use crate::{
    error::ParsingError, module::Function, store::StoreKey, symbols, tokenizer::TokenType,
};
use mira_spans::{interner::InternedStr, Span};

use super::{expression::PathWithoutGenerics, Annotations, Parser, Path};

pub static RESERVED_TYPE_NAMES: &[&str] = &[
    "str", "bool", "char", "void", "i8", "i16", "i32", "i64", "isize", "u8", "u16", "u32", "u64",
    "usize", "f16", "f32", "f64", "!",
];

#[derive(Clone, Eq, Debug)]
pub enum TypeRef<'arena> {
    DynReference {
        num_references: u8,
        span: Span<'arena>,
        traits: Vec<PathWithoutGenerics<'arena>>,
    },
    Reference {
        num_references: u8,
        type_name: Path<'arena>,
        span: Span<'arena>,
    },
    Void(Span<'arena>, u8),
    Never(Span<'arena>),
    UnsizedArray {
        num_references: u8,
        child: Box<TypeRef<'arena>>,
        span: Span<'arena>,
    },
    SizedArray {
        num_references: u8,
        child: Box<TypeRef<'arena>>,
        number_elements: usize,
        span: Span<'arena>,
    },
    Function {
        return_ty: Box<TypeRef<'arena>>,
        args: Vec<TypeRef<'arena>>,
        span: Span<'arena>,
        num_references: u8,
    },
    Tuple {
        num_references: u8,
        span: Span<'arena>,
        elements: Vec<TypeRef<'arena>>,
    },
}

impl Display for TypeRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.get_ref_count() {
            f.write_char('&')?;
        }
        match self {
            Self::DynReference { traits, .. } => {
                f.write_str("dyn")?;
                for trait_name in traits {
                    f.write_char(' ')?;
                    Display::fmt(trait_name, f)?;
                }
                Ok(())
            }
            Self::Reference { type_name, .. } => Display::fmt(type_name, f),
            Self::UnsizedArray { child, .. } => {
                f.write_char('[')?;
                Display::fmt(&**child, f)?;
                f.write_char(']')
            }
            Self::SizedArray {
                child,
                number_elements: amount_of_elements,
                ..
            } => {
                f.write_char('[')?;
                Display::fmt(&**child, f)?;
                f.write_str("; ")?;
                Display::fmt(&amount_of_elements, f)?;
                f.write_char(']')
            }
            Self::Function {
                return_ty, args, ..
            } => {
                f.write_str("fn(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?
                    }
                    Display::fmt(arg, f)?;
                }
                if !matches!(&**return_ty, Self::Void(_, 0) | Self::Never(_)) {
                    f.write_str(" -> ")?;
                    Display::fmt(return_ty, f)?;
                }
                Ok(())
            }
            Self::Tuple { elements, .. } => {
                f.write_char('(')?;
                for (i, elem) in elements.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?
                    }
                    Display::fmt(elem, f)?;
                }
                f.write_char(')')
            }
            Self::Void(..) => f.write_str("void"),
            Self::Never(_) => f.write_str("!"),
        }
    }
}

impl<'arena> TypeRef<'arena> {
    pub fn try_clone_deref(mut self) -> Option<Self> {
        match &mut self {
            TypeRef::DynReference { num_references, .. }
            | TypeRef::Reference { num_references, .. }
            | TypeRef::UnsizedArray { num_references, .. }
            | TypeRef::SizedArray { num_references, .. }
            | TypeRef::Function { num_references, .. }
            | TypeRef::Tuple { num_references, .. }
            | TypeRef::Void(_, num_references) => {
                if *num_references == 0 {
                    None
                } else {
                    *num_references -= 1;
                    Some(self)
                }
            }
            TypeRef::Never(_) => Some(self),
        }
    }

    pub fn can_deref(&self) -> bool {
        self.get_ref_count() > 0
    }

    pub fn get_ref_count(&self) -> u8 {
        match self {
            Self::Void(_, num_references)
            | Self::Reference { num_references, .. }
            | Self::UnsizedArray { num_references, .. }
            | Self::DynReference { num_references, .. }
            | Self::Function { num_references, .. }
            | Self::Tuple { num_references, .. }
            | Self::SizedArray { num_references, .. } => *num_references,
            Self::Never(_) => 0,
        }
    }

    pub fn parse(parser: &mut Parser<'_, 'arena>) -> Result<Self, ParsingError<'arena>> {
        let mut num_references = 0;

        while !parser.is_at_end() {
            // <type> = <subtype> | &<type>
            // <subtype> = [<sized-or-unsized>] | <identifier>
            // <sized-or-unsized> = <type>; <number> | <type>

            if parser.match_tok(TokenType::Ampersand) {
                num_references += 1;
                continue;
            }
            if parser.match_tok(TokenType::LogicalAnd) {
                num_references += 2;
                continue;
            }

            let span = parser.peek().span;
            if parser.match_tok(TokenType::BracketLeft) {
                let child = Box::new(Self::parse(parser)?);
                if parser.match_tok(TokenType::Semicolon) {
                    // case [<type>; <amount>]
                    let (lit, _) = parser.expect_tok(TokenType::UIntLiteral)?.uint_literal()?;
                    parser.expect_tok(TokenType::BracketRight)?;

                    return Ok(Self::SizedArray {
                        num_references,
                        child,
                        number_elements: lit as usize,
                        span: parser.span_from(span),
                    });
                } else {
                    parser.expect_tok(TokenType::BracketRight)?;
                    return Ok(Self::UnsizedArray {
                        num_references,
                        child,
                        span: parser.span_from(span),
                    });
                }
            } else if parser.match_tok(TokenType::LogicalNot) {
                return Ok(Self::Reference {
                    num_references,
                    type_name: Path::new(
                        symbols::Types::NeverType,
                        parser.current().span,
                        Vec::new(),
                    ),
                    span: parser.span_from(span),
                });
            } else if parser.match_tok(TokenType::VoidLiteral) {
                return Ok(Self::Void(parser.span_from(span), num_references));
            } else if parser.peek().typ == TokenType::IdentifierLiteral {
                let name = parser.peek().string_literal()?;
                return if *name == "dyn" {
                    Self::parse_dyn(parser, num_references, span)
                } else {
                    Ok(Self::Reference {
                        num_references,
                        type_name: Path::parse(parser)?,
                        span: parser.span_from(span),
                    })
                };
            } else if parser.match_tok(TokenType::Fn) {
                parser.expect_tok(TokenType::ParenLeft)?;
                let mut args = Vec::new();
                while !parser.match_tok(TokenType::ParenRight) {
                    if !args.is_empty() {
                        if !parser.match_tok(TokenType::Comma) {
                            return Err(ParsingError::ExpectedFunctionArgument {
                                loc: parser.peek().span,
                                found: parser.peek().typ,
                            });
                        }

                        // for trailing comma
                        if parser.match_tok(TokenType::ParenRight) {
                            break;
                        }
                    }

                    args.push(TypeRef::parse(parser)?);
                }

                let return_ty = Box::new(if parser.match_tok(TokenType::ReturnType) {
                    TypeRef::parse(parser)?
                } else {
                    TypeRef::Void(parser.span_from(span), 0)
                });

                return Ok(TypeRef::Function {
                    return_ty,
                    args,
                    span: parser.span_from(span),
                    num_references,
                });
            } else if parser.match_tok(TokenType::ParenLeft) {
                let mut elements = Vec::new();
                while !parser.match_tok(TokenType::ParenRight) {
                    if !elements.is_empty() {
                        parser.expect_tok(TokenType::Comma)?;

                        if parser.match_tok(TokenType::ParenRight) {
                            break;
                        }
                    }

                    elements.push(TypeRef::parse(parser)?);
                }

                return Ok(TypeRef::Tuple {
                    num_references,
                    span: parser.span_from(span),
                    elements,
                });
            } else {
                return Err(ParsingError::ExpectedType {
                    loc: parser.peek().span,
                    found: parser.peek().typ,
                });
            }
        }

        Err(ParsingError::ExpectedType {
            loc: parser.peek().span,
            found: TokenType::Eof,
        })
    }

    fn parse_dyn(
        parser: &mut Parser<'_, 'arena>,
        num_references: u8,
        loc: Span<'arena>,
    ) -> Result<Self, ParsingError<'arena>> {
        parser.advance();
        let mut traits = vec![];

        loop {
            if !traits.is_empty() && !parser.match_tok(TokenType::Plus) {
                break;
            }

            if parser.peek().typ != TokenType::IdentifierLiteral {
                break;
            }
            traits.push(PathWithoutGenerics::parse(parser)?);
        }

        Ok(Self::DynReference {
            num_references,
            span: loc,
            traits,
        })
    }

    pub fn span(&self) -> Span<'arena> {
        match self {
            Self::Never(span)
            | Self::Void(span, _)
            | Self::Reference { span, .. }
            | Self::SizedArray { span, .. }
            | Self::DynReference { span, .. }
            | Self::Function { span, .. }
            | Self::Tuple { span, .. }
            | Self::UnsizedArray { span, .. } => *span,
        }
    }
}

impl PartialEq for TypeRef<'_> {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Reference {
                num_references: self_nor,
                type_name: self_type,
                span: _,
            } => match other {
                Self::Reference {
                    num_references: other_nor,
                    type_name: other_type,
                    span: _,
                } => *other_nor == *self_nor && self_type == other_type,
                _ => false,
            },
            Self::DynReference {
                num_references: self_nor,
                traits: self_traits,
                ..
            } => match other {
                Self::DynReference {
                    num_references: other_nor,
                    traits: other_traits,
                    ..
                } => *other_nor == *self_nor && *self_traits == *other_traits,
                _ => false,
            },
            Self::Function {
                num_references: self_nor,
                return_ty: self_return_ty,
                args: self_args,
                ..
            } => match other {
                Self::Function {
                    num_references: other_nor,
                    return_ty: other_return_ty,
                    args: other_args,
                    ..
                } => {
                    *other_nor == *self_nor
                        && *self_return_ty == *other_return_ty
                        && *self_args == *other_args
                }
                _ => false,
            },
            Self::SizedArray {
                num_references: self_nor,
                child: self_child,
                number_elements: self_aoe,
                span: _,
            } => match other {
                Self::SizedArray {
                    num_references: other_nor,
                    child: other_child,
                    number_elements: other_aoe,
                    span: _,
                } => {
                    *other_nor == *self_nor
                        && *other_aoe == *self_aoe
                        && **other_child == **self_child
                }
                _ => false,
            },
            Self::UnsizedArray {
                num_references: self_nor,
                child: self_child,
                span: _,
            } => match other {
                Self::UnsizedArray {
                    num_references: other_nor,
                    child: other_child,
                    span: _,
                } => *other_nor == *self_nor && **other_child == **self_child,
                _ => false,
            },
            Self::Tuple {
                num_references: self_refs,
                elements: self_elems,
                ..
            } => match other {
                Self::Tuple {
                    num_references: other_refs,
                    elements: other_elems,
                    ..
                } => *self_refs == *other_refs && *self_elems == *other_elems,
                _ => false,
            },
            Self::Never(_) => matches!(other, Self::Never(_)),
            Self::Void(_, refcount) => {
                matches!(other, Self::Void(_, refcount_other) if refcount_other == refcount)
            }
        }
    }
}

pub type Implementation<'arena> = HashMap<InternedStr<'arena>, StoreKey<Function<'arena>>>;

#[derive(Debug)]
pub struct Struct<'arena> {
    pub span: Span<'arena>,
    pub name: InternedStr<'arena>,
    pub fields: Vec<(InternedStr<'arena>, TypeRef<'arena>)>,
    pub generics: Vec<Generic<'arena>>,
    pub global_impl: Implementation<'arena>,
    pub trait_impls: Vec<(InternedStr<'arena>, Implementation<'arena>)>,
    pub annotations: Annotations<'arena>,
}

#[derive(Debug, Clone)]
pub struct Generic<'arena> {
    pub name: InternedStr<'arena>,
    pub bounds: Vec<(PathWithoutGenerics<'arena>, Span<'arena>)>,
    pub sized: bool,
}

impl<'arena> Generic<'arena> {
    pub fn parse(parser: &mut Parser<'_, 'arena>) -> Result<Self, ParsingError<'arena>> {
        let sized = !parser.match_tok(TokenType::Unsized);
        let name = parser.expect_identifier()?;
        let mut bounds = Vec::new();
        if !parser.match_tok(TokenType::Colon) {
            return Ok(Self {
                sized,
                name,
                bounds,
            });
        }
        while parser.peek().typ == TokenType::Plus || bounds.is_empty() {
            if !bounds.is_empty() {
                parser.expect_tok(TokenType::Plus)?;
            }

            let loc = parser.peek().span;
            bounds.push((PathWithoutGenerics::parse(parser)?, loc));
        }
        Ok(Self {
            sized,
            name,
            bounds,
        })
    }
}
