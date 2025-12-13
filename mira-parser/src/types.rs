use std::fmt::{Display, Write};

use crate::error::ParsingError;
use mira_lexer::{Delimiter, TokenTree, TokenType};
use mira_spans::{Ident, Span, interner::symbols};

use super::{Parser, Path, PathWithoutGenerics};

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
        let span = parser.peek_span();

        while !parser.is_at_end() {
            // <type> = <subtype> | &<type>
            // <subtype> = [<sized-or-unsized>] | <identifier>
            // <sized-or-unsized> = <type>; <number> | <type>

            if parser.match_tok_dismiss(TokenType::Ampersand) {
                num_references += 1;
                continue;
            }
            if parser.match_tok_dismiss(TokenType::LogicalAnd) {
                num_references += 2;
                continue;
            }

            // arrays
            if let Some(delim) = parser.match_delim(Delimiter::Brackets) {
                let mut p = parser.subparser(delim.into());
                let child = Box::new(TypeRef::parse(&mut p)?);

                if p.match_tok_dismiss(TokenType::Semicolon) {
                    // [<type>; <amount>]
                    let lit = parser.expect(TokenType::UIntLiteral)?.uint_literal().0;
                    parser.finish()?;

                    return Ok(Self::SizedArray {
                        num_references,
                        child,
                        number_elements: lit as usize,
                        span: delim.full_span(p.ctx().span_interner),
                    });
                }
                parser.finish()?;
                return Ok(Self::UnsizedArray {
                    num_references,
                    child,
                    span: delim.full_span(p.ctx().span_interner),
                });
            }

            // tuples
            if let Some(delim) = parser.match_delim(Delimiter::Parenthesis) {
                let mut p = parser.subparser(delim.into());
                let mut elements = Vec::new();
                while !p.is_at_end() {
                    if !elements.is_empty() {
                        if !p.match_tok_dismiss(TokenType::Comma) {
                            return Err(ParsingError::ExpectedFunctionArgument {
                                span: p.peek_span(),
                                found: p.peek_tok(),
                            });
                        }

                        // for trailing comma
                        if p.is_at_end() {
                            break;
                        }
                    }

                    elements.push(TypeRef::parse(&mut p)?);
                }

                return Ok(Self::Tuple {
                    num_references,
                    span: delim.full_span(p.ctx().span_interner),
                    elements,
                });
            }

            return if let Some(t) = parser.match_tok(TokenType::LogicalNot) {
                let type_name =
                    Path::new(Ident::new(symbols::Types::NeverType, t.span), Vec::new());
                let span = span.combine_with([t.span], parser.ctx().span_interner);
                Ok(Self::Reference {
                    num_references,
                    type_name,
                    span,
                })
            } else if let Some(t) = parser.match_tok(TokenType::VoidLiteral) {
                let span = span.combine_with([t.span], parser.ctx().span_interner);
                Ok(Self::Void(span, num_references))
            } else if let Some(TokenTree::Token(t)) = parser.peek() {
                if *t.string_literal() == "dyn" {
                    Self::parse_dyn(parser, num_references, span)
                } else {
                    let type_name = Path::parse_ty(parser)?;
                    let span =
                        span.combine_with([t.span, type_name.span], parser.ctx().span_interner);
                    Ok(Self::Reference {
                        num_references,
                        type_name,
                        span,
                    })
                }
            } else if let Some(t) = parser.match_tok(TokenType::Fn) {
                let arg_delim = parser.expect_delim(Delimiter::Parenthesis)?;
                let mut argp = parser.subparser(arg_delim.into());
                let mut args = Vec::new();
                while !argp.is_at_end() {
                    if !args.is_empty() {
                        if !argp.match_tok_dismiss(TokenType::Comma) {
                            return Err(ParsingError::ExpectedFunctionArgument {
                                span: argp.peek_span(),
                                found: argp.peek_tok(),
                            });
                        }

                        // for trailing comma
                        if argp.is_at_end() {
                            break;
                        }
                    }

                    args.push(TypeRef::parse(&mut argp)?);
                }

                let return_ty = Box::new(if parser.match_tok_dismiss(TokenType::ReturnType) {
                    TypeRef::parse(parser)?
                } else {
                    TypeRef::Void(t.span, 0)
                });

                let span = span.combine_with(
                    [
                        t.span,
                        return_ty.span(),
                        arg_delim.open_span,
                        arg_delim.close_span,
                    ],
                    parser.ctx().span_interner,
                );
                Ok(TypeRef::Function {
                    return_ty,
                    args,
                    span,
                    num_references,
                })
            } else {
                break;
            };
        }

        Err(ParsingError::ExpectedType {
            span: parser.peek_span(),
            found: parser.peek_tok(),
        })
    }

    fn parse_dyn(
        parser: &mut Parser<'_, 'arena>,
        num_references: u8,
        span: Span<'arena>,
    ) -> Result<Self, ParsingError<'arena>> {
        parser.dismiss();
        let mut traits = vec![];

        loop {
            if !traits.is_empty() && !parser.match_tok_dismiss(TokenType::Plus) {
                break;
            }

            if let Some(TokenTree::Token(t)) = parser.peek()
                && t.ty == TokenType::IdentifierLiteral
            {
                traits.push(PathWithoutGenerics::parse(parser)?);
            } else {
                break;
            }
        }

        Ok(Self::DynReference {
            num_references,
            span,
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

#[derive(Debug, Clone)]
pub struct Generic<'arena> {
    pub name: Ident<'arena>,
    pub bounds: Vec<(PathWithoutGenerics<'arena>, Span<'arena>)>,
    pub sized: bool,
}

impl<'arena> Generic<'arena> {
    pub fn parse(parser: &mut Parser<'_, 'arena>) -> Result<Self, ParsingError<'arena>> {
        let sized = !parser.match_tok_dismiss(TokenType::Unsized);
        let name = parser.expect_identifier()?;
        let mut bounds = Vec::new();
        if !parser.match_tok_dismiss(TokenType::Colon) {
            return Ok(Self {
                sized,
                name,
                bounds,
            });
        }
        while parser.match_tok_dismiss(TokenType::Plus) || bounds.is_empty() {
            let span = parser.peek_span();
            bounds.push((PathWithoutGenerics::parse(parser)?, span));
        }
        Ok(Self {
            sized,
            name,
            bounds,
        })
    }
}
