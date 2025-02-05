use std::{
    collections::HashMap,
    fmt::{Display, Write},
};

use crate::{
    error::ParsingError, globals::GlobalStr, module::FunctionId, parser::Location,
    tokenizer::TokenType,
};

use super::{expression::PathWithoutGenerics, Annotations, Parser, Path};

pub static RESERVED_TYPE_NAMES: &[&'static str] = &[
    "str", "bool", "char", "void", "i8", "i16", "i32", "i64", "isize", "u8", "u16", "u32", "u64",
    "usize", "f16", "f32", "f64", "!",
];

#[derive(Clone, Eq, Debug)]
pub enum TypeRef {
    DynReference {
        num_references: u8,
        loc: Location,
        traits: Vec<PathWithoutGenerics>,
    },
    Reference {
        num_references: u8,
        type_name: Path,
        loc: Location,
    },
    Void(Location, u8),
    Never(Location),
    UnsizedArray {
        num_references: u8,
        child: Box<TypeRef>,
        loc: Location,
    },
    SizedArray {
        num_references: u8,
        child: Box<TypeRef>,
        number_elements: usize,
        loc: Location,
    },
    Function {
        return_ty: Box<TypeRef>,
        args: Vec<TypeRef>,
        loc: Location,
        num_references: u8,
    },
    Tuple {
        num_references: u8,
        loc: Location,
        elements: Vec<TypeRef>,
    },
}

impl Display for TypeRef {
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
                for i in 0..args.len() {
                    if i != 0 {
                        f.write_str(", ")?
                    }
                    Display::fmt(&args[i], f)?;
                }
                if !matches!(&**return_ty, Self::Void(_, 0) | Self::Never(_)) {
                    f.write_str(" -> ")?;
                    Display::fmt(return_ty, f)?;
                }
                Ok(())
            }
            Self::Tuple { elements, .. } => {
                f.write_char('(')?;
                for i in 0..elements.len() {
                    if i != 0 {
                        f.write_str(", ")?
                    }
                    Display::fmt(&elements[i], f)?;
                }
                f.write_char(')')
            }
            Self::Void(..) => f.write_str("void"),
            Self::Never(_) => f.write_str("!"),
        }
    }
}

impl TypeRef {
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

    pub fn parse(parser: &mut Parser) -> Result<Self, ParsingError> {
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

            let loc = parser.peek().location.clone();
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
                        loc,
                    });
                } else {
                    parser.expect_tok(TokenType::BracketRight)?;
                    return Ok(Self::UnsizedArray {
                        num_references,
                        child,
                        loc,
                    });
                }
            } else if parser.match_tok(TokenType::LogicalNot) {
                return Ok(Self::Reference {
                    num_references,
                    type_name: Path::new(GlobalStr::new("!"), Vec::new()),
                    loc,
                });
            } else if parser.match_tok(TokenType::VoidLiteral) {
                return Ok(Self::Void(loc, num_references));
            } else if parser.peek().typ == TokenType::IdentifierLiteral {
                let name = parser.peek().string_literal()?;
                return if *name == "dyn" {
                    Self::parse_dyn(parser, num_references, loc)
                } else {
                    Ok(Self::Reference {
                        num_references,
                        type_name: Path::parse(parser)?,
                        loc,
                    })
                };
            } else if parser.match_tok(TokenType::Fn) {
                parser.expect_tok(TokenType::ParenLeft)?;
                let mut args = Vec::new();
                while !parser.match_tok(TokenType::ParenRight) {
                    if args.len() > 0 {
                        if !parser.match_tok(TokenType::Comma) {
                            return Err(ParsingError::ExpectedFunctionArgument {
                                loc: parser.peek().location.clone(),
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
                    TypeRef::Void(loc.clone(), 0)
                });

                return Ok(TypeRef::Function {
                    return_ty,
                    args,
                    loc,
                    num_references,
                });
            } else if parser.match_tok(TokenType::ParenLeft) {
                let mut elements = Vec::new();
                while !parser.match_tok(TokenType::ParenRight) {
                    if elements.len() > 0 {
                        parser.expect_tok(TokenType::Comma)?;

                        if parser.match_tok(TokenType::ParenRight) {
                            break;
                        }
                    }

                    elements.push(TypeRef::parse(parser)?);
                }

                return Ok(TypeRef::Tuple {
                    num_references,
                    loc,
                    elements,
                });
            } else {
                return Err(ParsingError::ExpectedType {
                    loc: parser.peek().location.clone(),
                    found: parser.peek().typ,
                });
            }
        }

        return Err(ParsingError::ExpectedType {
            loc: parser.peek().location.clone(),
            found: TokenType::Eof,
        });
    }

    fn parse_dyn(
        parser: &mut Parser,
        num_references: u8,
        loc: Location,
    ) -> Result<Self, ParsingError> {
        parser.advance();
        let mut traits = vec![];

        loop {
            if traits.len() > 0 && !parser.match_tok(TokenType::Plus) {
                break;
            }

            if parser.peek().typ != TokenType::IdentifierLiteral {
                break;
            }
            traits.push(PathWithoutGenerics::parse(parser)?);
        }

        return Ok(Self::DynReference {
            num_references,
            loc,
            traits,
        });
    }

    pub fn loc(&self) -> &Location {
        match self {
            Self::Never(loc)
            | Self::Void(loc, _)
            | Self::Reference { loc, .. }
            | Self::SizedArray { loc, .. }
            | Self::DynReference { loc, .. }
            | Self::Function { loc, .. }
            | Self::Tuple { loc, .. }
            | Self::UnsizedArray { loc, .. } => loc,
        }
    }
}

impl PartialEq for TypeRef {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Reference {
                num_references: self_nor,
                type_name: self_type,
                loc: _,
            } => match other {
                Self::Reference {
                    num_references: other_nor,
                    type_name: other_type,
                    loc: _,
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
                loc: _,
            } => match other {
                Self::SizedArray {
                    num_references: other_nor,
                    child: other_child,
                    number_elements: other_aoe,
                    loc: _,
                } => {
                    *other_nor == *self_nor
                        && *other_aoe == *self_aoe
                        && (&**other_child) == (&**self_child)
                }
                _ => false,
            },
            Self::UnsizedArray {
                num_references: self_nor,
                child: self_child,
                loc: _,
            } => match other {
                Self::UnsizedArray {
                    num_references: other_nor,
                    child: other_child,
                    loc: _,
                } => *other_nor == *self_nor && (&**other_child) == (&**self_child),
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

pub type Implementation = HashMap<GlobalStr, FunctionId>;

#[derive(Debug)]
pub struct Struct {
    pub loc: Location,
    pub name: GlobalStr,
    pub fields: Vec<(GlobalStr, TypeRef)>,
    pub generics: Vec<Generic>,
    pub global_impl: Implementation,
    pub trait_impls: Vec<(GlobalStr, Implementation)>,
    pub annotations: Annotations,
}

#[derive(Debug, Clone)]
pub struct Generic {
    pub name: GlobalStr,
    pub bounds: Vec<(PathWithoutGenerics, Location)>,
    pub sized: bool,
}

impl Generic {
    pub fn parse(parser: &mut Parser) -> Result<Self, ParsingError> {
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
        while parser.peek().typ == TokenType::Plus || bounds.len() == 0 {
            if bounds.len() > 0 {
                parser.expect_tok(TokenType::Plus)?;
            }

            let loc = parser.peek().location.clone();
            bounds.push((PathWithoutGenerics::parse(parser)?, loc));
        }
        Ok(Self {
            sized,
            name,
            bounds,
        })
    }
}
