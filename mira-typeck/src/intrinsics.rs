use mira_parser::std_annotations::intrinsic::Intrinsic;
use mira_spans::Span;

use super::{Ty, TypecheckingError};

pub trait IntrinsicExt {
    fn is_valid_for<'arena>(
        &self,
        loc: Span<'arena>,
        generics: &[Ty<'arena>],
    ) -> Result<(), TypecheckingError<'arena>>;
}

fn generic_count(intrinsic: Intrinsic) -> usize {
    match intrinsic {
        Intrinsic::Unreachable | Intrinsic::CallMain => 0,
        _ => 1,
    }
}

impl IntrinsicExt for Intrinsic {
    fn is_valid_for<'arena>(
        &self,
        loc: Span<'arena>,
        generics: &[Ty<'arena>],
    ) -> Result<(), TypecheckingError<'arena>> {
        let required_generics = generic_count(*self);
        if generics.len() != required_generics {
            return Err(TypecheckingError::MismatchingGenericCount(
                loc,
                generics.len(),
                required_generics,
            ));
        }

        match self {
            // ┌───────────────────────┐
            // │ Sized-only Intrinsics │
            // └───────────────────────┘
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
                .ok_or_else(|| TypecheckingError::NonSizedType(loc, generics[0])),
            // ┌────────────────────────┐
            // │ Genericless Intrinsics │
            // └────────────────────────┘
            Intrinsic::Unreachable | Intrinsic::CallMain => Ok(()),
            // ┌──────────────────────┐
            // │ All-types Intrinsics │
            // └──────────────────────┘
            Intrinsic::DropInPlace
            | Intrinsic::SizeOfVal
            | Intrinsic::Offset
            | Intrinsic::GetMetadata
            | Intrinsic::WithMetadata
            | Intrinsic::TypeName => Ok(()),
        }
    }
}
