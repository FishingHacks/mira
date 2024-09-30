use std::{
    collections::HashMap,
    fmt::{Display, Write},
    rc::Rc,
};

use crate::{
    error::ProgrammingLangParsingError,
    module::FunctionId,
    parser::Location,
    tokenizer::{Literal, TokenType},
};

use super::{Annotations, Parser, Path};

pub static RESERVED_TYPE_NAMES: &[&'static str] = &[
    "str", "bool", "char", "void", "i8", "i16", "i32", "i64", "isize", "u8", "u16", "u32", "u64",
    "usize", "f16", "f32", "f64", "!",
];

#[derive(Clone, Eq, Debug)]
pub enum TypeRef {
    Reference {
        number_of_references: u8,
        type_name: Rc<str>,
        loc: Location,
    },
    Void(Location),
    Never(Location),
    UnsizedArray {
        number_of_references: u8,
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
            Self::Void(_) => f.write_str("void"),
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
            Self::Reference {
                number_of_references,
                type_name,
                loc,
            } => Self::Reference {
                number_of_references: number_of_references - 1,
                type_name: type_name,
                loc,
            },
            Self::UnsizedArray {
                number_of_references,
                child,
                loc,
            } => Self::UnsizedArray {
                number_of_references: number_of_references - 1,
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
            // Self::Void and Self::Never cannot be dereferenced
            Self::Void(_) | Self::Never(_) => unreachable!(),
        })
    }

    pub fn can_deref(&self) -> bool {
        self.get_ref_count() > 0
    }

    pub fn get_ref_count(&self) -> u8 {
        match self {
            Self::Reference {
                number_of_references,
                ..
            }
            | Self::UnsizedArray {
                number_of_references,
                ..
            }
            | Self::SizedArray {
                num_references: number_of_references,
                ..
            } => *number_of_references,
            Self::Void(_) | Self::Never(_) => 0,
        }
    }

    pub fn parse(parser: &mut Parser) -> Result<Self, ProgrammingLangParsingError> {
        let mut number_of_references = 0;

        while !parser.is_at_end() {
            // <type> = <subtype> | &<type>
            // <subtype> = [<sized-or-unsized>] | <identifier>
            // <sized-or-unsized> = <type>; <number> | <type>

            if parser.match_tok(TokenType::Ampersand) {
                number_of_references += 1;
                continue;
            }
            if parser.match_tok(TokenType::LogicalAnd) {
                number_of_references += 2;
                continue;
            }

            let loc = parser.peek().location.clone();
            if parser.match_tok(TokenType::BracketLeft) {
                let child = Box::new(Self::parse(parser)?);
                if parser.match_tok(TokenType::Semicolon) {
                    // case [<type>; <amount>]
                    if !parser.match_tok(TokenType::FloatLiteral) {
                        return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                            loc: parser.peek().location.clone(),
                            expected: TokenType::FloatLiteral,
                            found: parser.peek().typ,
                        });
                    }
                    let Some(Literal::UInt(lit)) = parser.previous().literal else {
                        return Err(ProgrammingLangParsingError::InvalidTokenization {
                            loc: parser.previous().location.clone(),
                        });
                    };

                    if !parser.match_tok(TokenType::BracketRight) {
                        return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                            loc: parser.peek().location.clone(),
                            expected: TokenType::BracketRight,
                            found: parser.peek().typ,
                        });
                    }

                    return Ok(Self::SizedArray {
                        num_references: number_of_references,
                        child,
                        number_elements: lit as usize,
                        loc,
                    });
                } else if !parser.match_tok(TokenType::BracketRight) {
                    return Err(ProgrammingLangParsingError::ExpectedArbitrary {
                        loc: parser.peek().location.clone(),
                        expected: TokenType::BracketRight,
                        found: parser.peek().typ,
                    });
                } else {
                    return Ok(Self::UnsizedArray {
                        number_of_references,
                        child,
                        loc,
                    });
                }
            } else if parser.match_tok(TokenType::LogicalNot) {
                return Ok(Self::Reference {
                    number_of_references,
                    type_name: "!".into(),
                    loc,
                });
            } else if let Some(ident) = parser.expect_identifier().ok() {
                return Ok(Self::Reference {
                    number_of_references,
                    type_name: ident,
                    loc,
                });
            } else {
                return Err(ProgrammingLangParsingError::ExpectedType {
                    loc: parser.peek().location.clone(),
                    found: parser.peek().typ,
                });
            }
        }

        return Err(ProgrammingLangParsingError::ExpectedType {
            loc: parser.peek().location.clone(),
            found: TokenType::Eof,
        });
    }

    pub fn loc(&self) -> &Location {
        match self {
            Self::Never(loc)
            | Self::Void(loc)
            | Self::Reference { loc, .. }
            | Self::SizedArray { loc, .. }
            | Self::UnsizedArray { loc, .. } => loc,
        }
    }
}

impl PartialEq for TypeRef {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Reference {
                number_of_references: self_nor,
                type_name: self_type,
                loc: _,
            } => match other {
                Self::Reference {
                    number_of_references: other_nor,
                    type_name: other_type,
                    loc: _,
                } => *other_nor == *self_nor && self_type == other_type,
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
                number_of_references: self_nor,
                child: self_child,
                loc: _,
            } => match other {
                Self::UnsizedArray {
                    number_of_references: other_nor,
                    child: other_child,
                    loc: _,
                } => *other_nor == *self_nor && (&**other_child) == (&**self_child),
                _ => false,
            },
            Self::Never(_) => matches!(other, Self::Never(_)),
            Self::Void(_) => matches!(other, Self::Void(_)),
        }
    }
}

pub type Implementation = HashMap<Rc<str>, FunctionId>;

#[derive(Debug)]
pub struct Struct {
    pub name: Rc<str>,
    pub fields: Vec<(Rc<str>, TypeRef)>,
    pub global_impl: Implementation,
    pub trait_impls: Vec<(Rc<str>, Implementation)>,
    pub annotations: Annotations,
}

#[derive(Debug, Clone)]
pub struct Generic {
    pub name: Rc<str>,
    pub bounds: Vec<Path>,
}
