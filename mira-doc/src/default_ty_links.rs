use mira_typeck::TyKind;

pub(crate) const VOID: &str = "https://fishinghacks.github.io/mira/std/langitems/void_t.html";
pub(crate) const NEVER: &str = "https://fishinghacks.github.io/mira/std/langitems/never_t.html";
pub(crate) const I8: &str = "https://fishinghacks.github.io/mira/std/langitems/i8_t.html";
pub(crate) const I16: &str = "https://fishinghacks.github.io/mira/std/langitems/i16_t.html";
pub(crate) const I32: &str = "https://fishinghacks.github.io/mira/std/langitems/i32_t.html";
pub(crate) const I64: &str = "https://fishinghacks.github.io/mira/std/langitems/i64_t.html";
pub(crate) const ISIZE: &str = "https://fishinghacks.github.io/mira/std/langitems/isize_t.html";
pub(crate) const U8: &str = "https://fishinghacks.github.io/mira/std/langitems/u8_t.html";
pub(crate) const U16: &str = "https://fishinghacks.github.io/mira/std/langitems/u16_t.html";
pub(crate) const U32: &str = "https://fishinghacks.github.io/mira/std/langitems/u32_t.html";
pub(crate) const U64: &str = "https://fishinghacks.github.io/mira/std/langitems/u64_t.html";
pub(crate) const USIZE: &str = "https://fishinghacks.github.io/mira/std/langitems/usize_t.html";
pub(crate) const F32: &str = "https://fishinghacks.github.io/mira/std/langitems/f32_t.html";
pub(crate) const F64: &str = "https://fishinghacks.github.io/mira/std/langitems/f64_t.html";
pub(crate) const BOOL: &str = "https://fishinghacks.github.io/mira/std/langitems/bool_t.html";
pub(crate) const STR: &str = "https://fishinghacks.github.io/mira/std/langitems/str_t.html";

pub fn primitive_ty_to_link(ty: &TyKind) -> &'static str {
    match ty {
        TyKind::PrimitiveVoid => VOID,
        TyKind::PrimitiveNever => NEVER,
        TyKind::PrimitiveI8 => I8,
        TyKind::PrimitiveI16 => I16,
        TyKind::PrimitiveI32 => I32,
        TyKind::PrimitiveI64 => I64,
        TyKind::PrimitiveISize => ISIZE,
        TyKind::PrimitiveU8 => U8,
        TyKind::PrimitiveU16 => U16,
        TyKind::PrimitiveU32 => U32,
        TyKind::PrimitiveU64 => U64,
        TyKind::PrimitiveUSize => USIZE,
        TyKind::PrimitiveF32 => F32,
        TyKind::PrimitiveF64 => F64,
        TyKind::PrimitiveStr => STR,
        TyKind::PrimitiveBool => BOOL,
        _ => unreachable!("Non primitive type: {ty}"),
    }
}

/// maps a string matching a primitive type to it's link. e.g, returns
/// [`Some`]`(`[`crate::default_ty_links::VOID`]`)` for "void", but [`None`] for "CAlloc". Returns
/// [`crate::default_ty_links::NEVER`] for both "never" and "!".
pub fn primitive_link_from_str(s: &str) -> Option<&'static str> {
    match s {
        "never" | "!" => Some(NEVER),
        "void" => Some(VOID),
        "i8" => Some(I8),
        "i16" => Some(I16),
        "i32" => Some(I32),
        "i64" => Some(I64),
        "u8" => Some(U8),
        "u16" => Some(U16),
        "u32" => Some(U32),
        "u64" => Some(U64),
        "f32" => Some(F32),
        "f64" => Some(F64),
        "bool" => Some(BOOL),
        "str" => Some(STR),
        "isize" => Some(ISIZE),
        "usize" => Some(USIZE),
        _ => None,
    }
}
