use std::fmt::{Display, Write};
use std::ptr::drop_in_place;
use std::str::FromStr;

use crate::annotations::{Annotation, AnnotationReceiver, Annotations};
use crate::error::ParsingError;
use crate::tokenizer::{Literal, Location, Token, TokenType};
use crate::tokenstream::TokenStream;

use super::{Type, TypecheckingContext, TypecheckingError};

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
    Breakpoint => breakpoint, // () -> void
    Trap => trap, // () -> !
    Location => location, // () -> (u64, u64, &str)
    Offset => offset, // <unsized T>(v: &T, off: usize) -> &T, offsets a pointer
    GetMetadata => get_metadata, // <unsized T>(v: &T) -> usize, returns the metadata of a fat
    // pointer or 0 for a thin pointer
    WithMetadata => with_metadata, // <unsized T>(ptr: &void, data: usize) -> &T, attaches the
    // data to the ptr, assuming T is unsized. Errors if T is sized.
    TypeName => type_name, // <unsized T>() -> &str, returns the name of the type T
    Unreachable => unreachable, // marks a location as unreachable
    Read => read, // <T>(v: &T) -> T, reads a memory location even if T is not Copy
    Write => write, // <T>(v: &T, value: T), writes a memory location without dropping the value
    // that was previously there
    ReturnAddress => return_address, // (level: i32) -> usize, returns the address a "return" would
    Select => select, // <T>(cond: bool, a: T, b: T) -> T, equivalent to cond ? a : b
    VolatileRead => volatile_reade, // <T>(ptr: &T) -> T
    VolatileWrite => volatile_write, // <T>(ptr: &T, val: T);
    // jump to
    // ### INTEGER INTRINSICS ###
    // The following are *only* valid for ints
    ByteSwap => byte_swap, // <T>(v: T) -> T
    BitReverse => reverse_bits, // <T>(v: T) -> T
    CountLeadingZeros => count_leading_zeros, // <T>(v: T) -> u32
    CountTrailingZeros => count_trailing_zeros, // <T>(v: T) -> u32
    CountOnes => count_ones, // <T>(v: T) -> u32
    AddWithOverflow => add_with_overflow, // <T>(a: T, b: T) -> T
    SubWithOverflow => sub_with_overflow, // <T>(a: T, b: T) -> T
    MulWithOverflow => mul_with_overflow, // <T>(a: T, b: T) -> T
    WrappingAdd => wrapping_add, // <T>(a: T, b: T) -> T
    WrappingSub => wrapping_sub, // <T>(a: T, b: T) -> T
    WrappingMul => wrapping_mul, // <T>(a: T, b: T) -> T
    SaturatingAdd => saturating_add, // <T>(a: T, b: T) -> T
    SaturatingSub => saturating_sub, // <T>(a: T, b: T) -> T
    UncheckedAdd => unchecked_add, // <T>(a: T, b: T) -> T
    UncheckedSub => unchecked_sub, // <T>(a: T, b: T) -> T
    UncheckedMul => unchecked_mul, // <T>(a: T, b: T) -> T
    UncheckedDiv => unchecked_div, // <T>(a: T, b: T) -> T
    UncheckedMod => unchecked_mod, // <T>(a: T, b: T) -> T
    UncheckedShl => unchecked_shl, // <T>(a: T, b: T) -> T
    UncheckedShr => unchecked_shr, // <T>(a: T, b: T) -> T
}

impl Intrinsic {
    fn generic_count(&self) -> usize {
        match self {
            Intrinsic::Breakpoint
            | Intrinsic::Trap
            | Intrinsic::Location
            | Intrinsic::Unreachable
            | Intrinsic::ReturnAddress => 0,
            _ => 1,
        }
    }

    pub fn is_valid_for(&self, loc: Location, generics: &[Type]) -> Result<(), TypecheckingError> {
        let required_generics = self.generic_count();
        if generics.len() != required_generics {
            return Err(TypecheckingError::MismatchingGenericCount(
                loc,
                generics.len(),
                required_generics,
            ));
        }

        match self {
            // -------------------------
            // - sized-only intrinsics -
            // -------------------------
            Intrinsic::Drop
            | Intrinsic::SizeOf
            | Intrinsic::Read
            | Intrinsic::Write
            | Intrinsic::Select
            | Intrinsic::VolatileRead
            | Intrinsic::VolatileWrite
            | Intrinsic::Forget => generics[0]
                .is_sized()
                .then_some(())
                .ok_or_else(|| TypecheckingError::NonSizedType(loc, generics[0].clone())),
            // ----------------------
            // - integer intrinsics -
            // ----------------------
            Intrinsic::ByteSwap
            | Intrinsic::BitReverse
            | Intrinsic::CountLeadingZeros
            | Intrinsic::CountTrailingZeros
            | Intrinsic::CountOnes
            | Intrinsic::AddWithOverflow
            | Intrinsic::SubWithOverflow
            | Intrinsic::MulWithOverflow
            | Intrinsic::WrappingAdd
            | Intrinsic::WrappingSub
            | Intrinsic::WrappingMul
            | Intrinsic::SaturatingAdd
            | Intrinsic::SaturatingSub
            | Intrinsic::UncheckedAdd
            | Intrinsic::UncheckedSub
            | Intrinsic::UncheckedMul
            | Intrinsic::UncheckedDiv
            | Intrinsic::UncheckedMod
            | Intrinsic::UncheckedShl
            | Intrinsic::UncheckedShr => generics[0]
                .is_int_like()
                .then_some(())
                .ok_or_else(|| TypecheckingError::IntOnlyIntrinsic(loc, generics[0].clone())),
            // --------------------------
            // - genericless intrinsics -
            // --------------------------
            Intrinsic::Unreachable
            | Intrinsic::Breakpoint
            | Intrinsic::Trap
            | Intrinsic::Location
            | Intrinsic::ReturnAddress => Ok(()),
            // ------------------------
            // - all types intrinsics -
            // ------------------------
            Intrinsic::DropInPlace
            | Intrinsic::SizeOfVal
            | Intrinsic::Offset
            | Intrinsic::GetMetadata
            | Intrinsic::WithMetadata
            | Intrinsic::TypeName => Ok(()),
        }
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
            && annotations
                .get_annotations::<Self>()
                .skip(1)
                .next()
                .is_none()
    }
}

impl IntrinsicAnnotation {
    pub fn parse(mut tokens: TokenStream) -> Result<Self, ParsingError> {
        let (name, loc) = tokens.expect_remove_string()?;
        tokens.finish()?;
        name.with(Intrinsic::from_str)
            .map(Self)
            .map_err(|_| ParsingError::InvalidIntrinsic(loc, name))
    }

    pub fn get(&self) -> Intrinsic {
        self.0
    }
}
