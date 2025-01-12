use std::fmt::{Display, Write};
use std::str::FromStr;

use crate::annotations::{Annotation, AnnotationReceiver, Annotations};
use crate::error::ParsingError;
use crate::tokenizer::{Literal, Location, Token, TokenType};

use super::{Type, TypecheckingContext};

macro_rules! intrinsics {
    ($($name:ident => $value:ident),* $(,)? ) => {
        #[derive(Clone, Copy, PartialEq, Eq, Debug)]
        pub enum Intrinsic {
            $($name),*
        }

        impl Display for Intrinsic {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(Self::$name => f.write_str(stringify!($value)),)*
                }
            }
        }

        impl FromStr for Intrinsic {
            type Err = ();

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                match s {
                    $(stringify!($value) => Ok(Self::$name),)*
                    _ => Err(()),
                }
            }
        }
    };
}

intrinsics! {
    Drop => drop, // <T>(v: T), equivalent to drop_in_place(&v)
    DropInPlace => drop_in_place, // <unsized T>(v: &T), equivalent to Drop::drop(v)
    Forget => forget, // <T>(v: T), causes the value to not be dropped
    SizeOf => size_of, // <T>() -> usize, returns the size of T in bytes
    SizeOfVal => size_of_val, // <unsized T>(v: &T) -> usize, returns the size of v in bytes
    Transmute => transmute, // <Src, Dst>(v: Src) -> Dst, assumption: sizeof::<Src>() == sizeof::<Dst>()
    Breakpoint => breakpoint,
    Location => location,
    Offset => offset, // <unsized T>(v: &T, off: usize) -> &T, offsets a pointer
    GetMetadata => get_metadata, // <unsized T>(v: &T) -> usize, returns the metadata of a fat
    // pointer or 0 for a thin pointer
    WithMetadata => with_metadata, // <unsized T>(ptr: &void, data: usize) -> &T, attaches the
    // data to the ptr, assuming T is unsized. Errors if T is sized.
    TypeName => type_name, // <unsized T>() -> &str, returns the name of the type T
    Unreachable => unreachable, // marks a location as unreachable
    VtableSize => vtable_size, // (vtable: &void) -> usize, returns the size of the vtable
    VtableDrop => vtable_drop, // (vtable: &void) -> fn(v: &void), returns the drop function of a
    // vtable. May require transmuting the function pointer in case the vtable belongs to an
    // unsized type.
    Read => read, // <T>(v: &T) -> T, reads a memory location even if T is not Copy
    Write => write, // <T>(v: &T, value: T), writes a memory location without dropping the value
    // that was previously there
}

impl Intrinsic {
    pub fn get_type(self, ctx: &TypecheckingContext) -> Type {
        todo!()
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct IntrinsicAnnotation(Intrinsic);

impl Display for IntrinsicAnnotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("IntrinsicAnnotation(")?;
        Display::fmt(&self.0, f)?;
        f.write_char(')')
    }
}

impl Annotation for IntrinsicAnnotation {
    fn get_name(&self) -> &'static str {
        "intrinsic"
    }

    fn is_valid_for(&self, thing: AnnotationReceiver, annotations: &Annotations) -> bool {
        thing == AnnotationReceiver::Function
    }
}

impl IntrinsicAnnotation {
    pub fn parse(mut tokens: Vec<Token>, loc: Location) -> Result<Self, ParsingError> {
        if tokens.len() < 1 {
            return Err(ParsingError::ExpectedArbitrary {
                loc,
                expected: TokenType::StringLiteral,
                found: TokenType::Eof,
            });
        } else if tokens[0].typ != TokenType::StringLiteral {
            return Err(ParsingError::ExpectedArbitrary {
                loc: tokens[0].location.clone(),
                expected: TokenType::StringLiteral,
                found: tokens[0].typ,
            });
        } else if tokens.len() > 1 {
            return Err(ParsingError::ExpectedArbitrary {
                loc: tokens[1].location.clone(),
                expected: TokenType::Eof,
                found: tokens[1].typ,
            });
        } else {
            let token = tokens.remove(0);
            let Some(Literal::String(string)) = token.literal else {
                return Err(ParsingError::InvalidTokenization {
                    loc: tokens[0].location.clone(),
                });
            };
            match string.with(|str| Intrinsic::from_str(str)) {
                Ok(v) => Ok(Self(v)),
                Err(_) => Err(ParsingError::InvalidIntrinsic(token.location, string)),
            }
        }
    }

    pub fn get(&self) -> Intrinsic {
        self.0
    }
    pub fn get_type(&self, ctx: &TypecheckingContext) -> Type {
        self.0.get_type(ctx)
    }
}
