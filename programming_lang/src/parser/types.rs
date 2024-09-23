use std::{collections::HashMap, fmt::{Display, Write}, rc::Rc};

use crate::{error::ProgrammingLangParsingError, module::FunctionId, tokenizer::{Literal, TokenType}};

use super::{statement::Annotations, Parser, Path};

pub static RESERVED_TYPE_NAMES: &[&'static str] = &[
    "str", "bool", "char", "void", "i8", "i16", "i32", "i64", "isize", "u8", "u16", "u32", "u64",
    "usize", "f16", "f32", "f64", "!",
];

#[derive(Clone, Eq, Debug)]
pub enum TypeRef {
    Reference {
        number_of_references: u8,
        type_name: Rc<str>,
    },
    UnsizedArray {
        number_of_references: u8,
        child: Box<TypeRef>,
    },
    SizedArray {
        number_of_references: u8,
        child: Box<TypeRef>,
        amount_of_elements: usize,
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
            Self::SizedArray { child, amount_of_elements, .. } => {
                f.write_char('[')?;
                Display::fmt(&**child, f)?;
                f.write_str("; ")?;
                Display::fmt(&amount_of_elements, f)?;
                f.write_char(']')
            }
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
            } => Self::Reference {
                number_of_references: number_of_references - 1,
                type_name: type_name,
            },
            Self::UnsizedArray {
                number_of_references,
                child,
            } => Self::UnsizedArray {
                number_of_references: number_of_references - 1,
                child: child.clone(),
            },
            Self::SizedArray {
                number_of_references,
                child,
                amount_of_elements,
            } => Self::SizedArray {
                number_of_references: number_of_references - 1,
                child: child.clone(),
                amount_of_elements: amount_of_elements,
            },
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
                number_of_references,
                ..
            } => *number_of_references,
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

            if parser.match_tok(TokenType::BracketLeft) {
                let child = Box::new(Self::parse(parser)?);
                if parser.match_tok(TokenType::Semicolon) {
                    // case [<type>; <amount>]
                    if !parser.match_tok(TokenType::FloatLiteral) {
                        return Err(ProgrammingLangParsingError::ExpectedArbitrary { loc: parser.peek().location.clone(), expected: TokenType::FloatLiteral, found: parser.peek().typ });
                    }
                    let Some(Literal::UInt(lit)) = parser.previous().literal else {
                        return Err(ProgrammingLangParsingError::InvalidTokenization { loc: parser.previous().location.clone() });
                    };
                    
                    if !parser.match_tok(TokenType::BracketRight) {
                        return Err(ProgrammingLangParsingError::ExpectedArbitrary { loc: parser.peek().location.clone(), expected: TokenType::BracketRight, found: parser.peek().typ });
                    }

                    return Ok(Self::SizedArray { number_of_references, child, amount_of_elements: lit as usize });
                } else if !parser.match_tok(TokenType::BracketRight) {
                    return Err(ProgrammingLangParsingError::ExpectedArbitrary { loc: parser.peek().location.clone(), expected: TokenType::BracketRight, found: parser.peek().typ });
                } else {
                    return Ok(Self::UnsizedArray { number_of_references, child });
                }
            } else if parser.match_tok(TokenType::LogicalNot) {
                return Ok(Self::Reference { number_of_references, type_name: "!".into() });
            } else if let Some(ident) = parser.expect_identifier().ok() {
                return Ok(Self::Reference { number_of_references, type_name: ident });
            } else {
                return Err(ProgrammingLangParsingError::ExpectedType { loc: parser.peek().location.clone(), found: parser.peek().typ });
            }
        }

        return Err(ProgrammingLangParsingError::ExpectedType { loc: parser.peek().location.clone(), found: TokenType::Eof });
    }
}

impl PartialEq for TypeRef {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Reference {
                number_of_references: self_nor,
                type_name: self_type,
            } => match other {
                Self::Reference {
                    number_of_references: other_nor,
                    type_name: other_type,
                } => *other_nor == *self_nor && self_type == other_type,
                _ => false,
            },
            Self::SizedArray {
                number_of_references: self_nor,
                child: self_child,
                amount_of_elements: self_aoe,
            } => match other {
                Self::SizedArray {
                    number_of_references: other_nor,
                    child: other_child,
                    amount_of_elements: other_aoe,
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
            } => match other {
                Self::UnsizedArray {
                    number_of_references: other_nor,
                    child: other_child,
                } => *other_nor == *self_nor && (&**other_child) == (&**self_child),
                _ => false,
            },
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

#[derive(Debug)]
pub enum Type {
    Reference(TypeRef),
    /// An Array of unknown size
    UnsizedArray(TypeRef),
    /// An Array of known size
    SizedArray(TypeRef, usize),
    /// A reference to some other type
    Struct(Box<Struct>),
    /// Equivalent to: `struct str { length: usize; bytes: &[u8] }`
    ///
    /// Stores the length and the contents of a string
    PrimitiveStr,
    /// A 1-byte boolean
    PrimitiveBool,
    /// A single UTF-8 Character, at most 4 bytes, so sizeof(char) == 4 == sizeof(u32)
    PrimitiveChar,
    /**
    For when a function never returns, 0-sized, can be cast to anything.
    Example:
    ```rs
    extern fn exit(exit_code: i32) -> !;
    fn a() -> u32 {
        exit(12); // This returns the never type (!), so it is valid for u32, or anything else
    }
    ```
    */
    PrimitiveNever,
    /// For when a function returns nothing, 0-sized
    PrimitiveVoid,

    PrimitiveI8,
    PrimitiveI16,
    PrimitiveI32,
    PrimitiveI64,
    PrimitiveISize,

    PrimitiveU8,
    PrimitiveU16,
    PrimitiveU32,
    PrimitiveU64,
    PrimitiveUSize,

    /// 16-bit float (half)
    PrimitiveF16,
    /// 32-bit float (single)
    PrimitiveF32,
    /// 64-bit float (double)
    PrimitiveF64,
}

impl Type {
    pub fn primitive_name_to_type(name: &str) -> Option<Self> {
        match name {
            "str" => Some(Self::PrimitiveStr),
            "bool" => Some(Self::PrimitiveBool),
            "char" => Some(Self::PrimitiveChar),
            "void" => Some(Self::PrimitiveVoid),
            "!" => Some(Self::PrimitiveNever),

            "i8" => Some(Self::PrimitiveI8),
            "i16" => Some(Self::PrimitiveI16),
            "i32" => Some(Self::PrimitiveI32),
            "i64" => Some(Self::PrimitiveI64),
            "isize" => Some(Self::PrimitiveISize),
            "u8" => Some(Self::PrimitiveU8),
            "u16" => Some(Self::PrimitiveU16),
            "u32" => Some(Self::PrimitiveU32),
            "u64" => Some(Self::PrimitiveU64),
            "usize" => Some(Self::PrimitiveUSize),

            "f16" => Some(Self::PrimitiveF16),
            "f32" => Some(Self::PrimitiveF32),
            "f64" => Some(Self::PrimitiveF64),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Generic {
    pub name: Rc<str>,
    pub bounds: Vec<Path>,
}