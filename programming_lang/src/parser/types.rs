use std::{
    collections::HashMap,
    fmt::{Display, Write},
};

use crate::{
    error::ParsingError,
    globals::GlobalStr,
    module::FunctionId,
    parser::Location,
    tokenizer::{Literal, TokenType},
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
            Self::Void(..) => f.write_str("void"),
            Self::Never(_) => f.write_str("!"),
        }
    }
}

impl TypeRef {
    pub fn try_clone_deref(self) -> Option<Self> {
        if self.get_ref_count() > 0 {
            return None;
        }
        Some(match self {
            Self::DynReference {
                num_references,
                loc,
                traits,
            } => Self::DynReference {
                num_references: num_references - 1,
                loc,
                traits,
            },
            Self::Reference {
                num_references: number_of_references,
                type_name,
                loc,
            } => Self::Reference {
                num_references: number_of_references - 1,
                type_name,
                loc,
            },
            Self::UnsizedArray {
                num_references: number_of_references,
                child,
                loc,
            } => Self::UnsizedArray {
                num_references: number_of_references - 1,
                child: child.clone(),
                loc,
            },
            Self::SizedArray {
                num_references: number_of_references,
                child,
                number_elements: amount_of_elements,
                loc,
            } => Self::SizedArray {
                num_references: number_of_references - 1,
                child: child.clone(),
                number_elements: amount_of_elements,
                loc,
            },
            Self::Void(loc, refcount) => Self::Void(loc, refcount - 1),
            // Self::Never cannot be dereferenced
            Self::Never(_) => unreachable!(),
        })
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
                    if !parser.match_tok(TokenType::UIntLiteral) {
                        return Err(ParsingError::ExpectedArbitrary {
                            loc: parser.peek().location.clone(),
                            expected: TokenType::UIntLiteral,
                            found: parser.peek().typ,
                        });
                    }
                    let Some(Literal::UInt(lit, _)) = parser.previous().literal else {
                        return Err(ParsingError::InvalidTokenization {
                            loc: parser.previous().location.clone(),
                        });
                    };

                    if !parser.match_tok(TokenType::BracketRight) {
                        return Err(ParsingError::ExpectedArbitrary {
                            loc: parser.peek().location.clone(),
                            expected: TokenType::BracketRight,
                            found: parser.peek().typ,
                        });
                    }

                    return Ok(Self::SizedArray {
                        num_references,
                        child,
                        number_elements: lit as usize,
                        loc,
                    });
                } else if !parser.match_tok(TokenType::BracketRight) {
                    return Err(ParsingError::ExpectedArbitrary {
                        loc: parser.peek().location.clone(),
                        expected: TokenType::BracketRight,
                        found: parser.peek().typ,
                    });
                } else {
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
                return match parser.peek().literal {
                    Some(Literal::String(ref v)) if *v == "dyn" => {
                        Self::parse_dyn(parser, num_references, loc)
                    }
                    Some(Literal::String(_)) => Ok(Self::Reference {
                        num_references,
                        type_name: Path::parse(parser)?,
                        loc,
                    }),
                    _ => return Err(ParsingError::InvalidTokenization { loc }),
                };
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
    pub global_impl: Implementation,
    pub trait_impls: Vec<(GlobalStr, Implementation)>,
    pub annotations: Annotations,
}

#[derive(Debug, Clone)]
pub struct Generic {
    pub name: GlobalStr,
    pub bounds: Vec<(PathWithoutGenerics, Location)>,
}
