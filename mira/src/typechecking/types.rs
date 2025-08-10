use mira_spans::{extra_traits, interner, owned_intern, Arena, ArenaList};
use std::fmt::{Debug, Display, Write};
use std::hash::Hash;

use crate::context::SharedContext;
use crate::store::{Store, StoreKey};
use crate::{parser::TypeRef, tokenizer::NumberType};
use mira_spans::Ident;

use super::{TypecheckingContext, TypedStruct, TypedTrait};

pub mod default_types {
    #![allow(non_upper_case_globals)]
    use super::{Ty, TyKind};

    macro_rules! tydef {
        ($($name:ident $kind:ident),* $(,)?) => {
            $(pub static $name: Ty<'static> = Ty(&TyKind::$kind(0));)*
        };
    }

    tydef![
        u8 PrimitiveU8,
        u16 PrimitiveU16,
        u32 PrimitiveU32,
        u64 PrimitiveU64,
        usize PrimitiveUSize,
        i8 PrimitiveI8,
        i16 PrimitiveI16,
        i32 PrimitiveI32,
        i64 PrimitiveI64,
        f32 PrimitiveF32,
        f64 PrimitiveF64,
        isize PrimitiveISize,
        bool PrimitiveBool,
        void PrimitiveVoid,
        str PrimitiveStr,
        self_ PrimitiveSelf,
    ];

    pub static never: Ty<'static> = Ty(&TyKind::PrimitiveNever);

    // types used by lang items
    pub static str_ref: Ty<'static> = Ty(&TyKind::PrimitiveStr(1));
    pub static self_ref: Ty<'static> = Ty(&TyKind::PrimitiveSelf(1));
    pub static void_ref: Ty<'static> = Ty(&TyKind::PrimitiveVoid(1));

    pub static ALL: &[Ty<'static>] = &[
        u8, u16, u32, u64, usize, i8, i16, i32, i64, isize, bool, void, never, str_ref, self_ref,
        void_ref, self_,
    ];
}

interner!(TypeListInterner, TyList, [Ty<'arena>], |arena, types| arena
    .alloc_slice(types));
extra_traits!(for TyList impl debug);
interner!(
    TypeInterner,
    Ty,
    TyKind<'arena>,
    |arena, ty| arena.alloc(ty.clone()),
    default_types::ALL
);
extra_traits!(for Ty impl debug, display);
owned_intern!(TypeInterner, Ty, TyKind<'arena>, |arena, ty| arena
    .alloc(ty));

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionType<'arena> {
    pub arguments: TyList<'arena>,
    pub return_type: Ty<'arena>,
}

impl<'a> AsRef<TyKind<'a>> for &TyKind<'a> {
    fn as_ref(&self) -> &TyKind<'a> {
        self
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TyKind<'arena> {
    DynType {
        trait_refs: ArenaList<'arena, (StoreKey<TypedTrait<'arena>>, Ident<'arena>)>,
        num_references: u8,
    },
    Struct {
        struct_id: StoreKey<TypedStruct<'arena>>,
        name: Ident<'arena>,
        num_references: u8,
    },
    UnsizedArray {
        typ: Ty<'arena>,
        num_references: u8,
    },
    SizedArray {
        typ: Ty<'arena>,
        num_references: u8,
        number_elements: usize,
    },
    Tuple {
        elements: TyList<'arena>,
        num_references: u8,
    },
    Function(FunctionType<'arena>, u8),

    PrimitiveVoid(u8),
    PrimitiveNever,

    PrimitiveI8(u8),
    PrimitiveI16(u8),
    PrimitiveI32(u8),
    PrimitiveI64(u8),
    PrimitiveISize(u8),

    PrimitiveU8(u8),
    PrimitiveU16(u8),
    PrimitiveU32(u8),
    PrimitiveU64(u8),
    PrimitiveUSize(u8),

    PrimitiveF32(u8),
    PrimitiveF64(u8),

    PrimitiveStr(u8),
    PrimitiveBool(u8),
    PrimitiveSelf(u8),

    Generic {
        name: Ident<'arena>,
        num_references: u8,
        generic_id: u8,
        bounds: ArenaList<'arena, StoreKey<TypedTrait<'arena>>>,
        sized: bool,
    },
}

pub fn resolve_primitive_type<'arena>(
    ctx: SharedContext<'arena>,
    typ: &TypeRef<'arena>,
) -> Option<Ty<'arena>> {
    match typ {
        TypeRef::Never(_) => Some(default_types::never),
        TypeRef::Void(_, 0) => Some(default_types::void),
        TypeRef::Void(_, refcount) => Some(ctx.intern_ty(TyKind::PrimitiveVoid(*refcount))),
        TypeRef::Reference {
            num_references: 0,
            type_name,
            span: _,
        } if type_name.entries.len() == 1 && type_name.entries[0].1.is_empty() => {
            match &*type_name.entries[0].0 {
                "!" => Some(default_types::never),
                "void" => Some(default_types::void),
                "i8" => Some(default_types::i8),
                "i16" => Some(default_types::i16),
                "i32" => Some(default_types::i32),
                "i64" => Some(default_types::i64),
                "u8" => Some(default_types::u8),
                "u16" => Some(default_types::u16),
                "u32" => Some(default_types::u32),
                "u64" => Some(default_types::u64),
                "f32" => Some(default_types::f32),
                "f64" => Some(default_types::f64),
                "bool" => Some(default_types::bool),
                "str" => Some(default_types::str),
                "isize" => Some(default_types::isize),
                "usize" => Some(default_types::usize),
                "Self" => Some(default_types::self_),
                _ => None,
            }
        }
        TypeRef::Reference {
            num_references,
            type_name,
            span: _,
        } if type_name.entries.len() == 1 && type_name.entries[0].1.is_empty() => {
            match &*type_name.entries[0].0 {
                "!" => Some(TyKind::PrimitiveNever),
                "void" => Some(TyKind::PrimitiveVoid(*num_references)),
                "i8" => Some(TyKind::PrimitiveI8(*num_references)),
                "i16" => Some(TyKind::PrimitiveI16(*num_references)),
                "i32" => Some(TyKind::PrimitiveI32(*num_references)),
                "i64" => Some(TyKind::PrimitiveI64(*num_references)),
                "u8" => Some(TyKind::PrimitiveU8(*num_references)),
                "u16" => Some(TyKind::PrimitiveU16(*num_references)),
                "u32" => Some(TyKind::PrimitiveU32(*num_references)),
                "u64" => Some(TyKind::PrimitiveU64(*num_references)),
                "f32" => Some(TyKind::PrimitiveF32(*num_references)),
                "f64" => Some(TyKind::PrimitiveF64(*num_references)),
                "bool" => Some(TyKind::PrimitiveBool(*num_references)),
                "str" => Some(TyKind::PrimitiveStr(*num_references)),
                "isize" => Some(TyKind::PrimitiveISize(*num_references)),
                "usize" => Some(TyKind::PrimitiveUSize(*num_references)),
                "Self" => Some(TyKind::PrimitiveSelf(*num_references)),
                _ => None,
            }
            .map(|ty| ctx.intern_ty(ty))
        }
        _ => None,
    }
}

fn align(value: u64, alignment: u32) -> u64 {
    if value % alignment as u64 == 0 {
        value
    } else {
        alignment as u64 - (value % alignment as u64) + value
    }
}

fn values_match<'arena>(
    left: &[StoreKey<TypedTrait<'arena>>],
    right: &[StoreKey<TypedTrait<'arena>>],
) -> bool {
    for v in right {
        if !left.contains(v) {
            return false;
        }
    }
    true
}

impl<'arena> TyKind<'arena> {
    pub fn implements(
        &self,
        traits: &[StoreKey<TypedTrait<'arena>>],
        tc_ctx: &TypecheckingContext<'arena>,
    ) -> bool {
        match self {
            TyKind::DynType {
                trait_refs,
                num_references: 1,
            } => values_match(&trait_refs.iter().map(|v| v.0).collect::<Vec<_>>(), traits),
            TyKind::Struct {
                struct_id,
                num_references: 0,
                ..
            } => values_match(
                &tc_ctx.structs.read()[*struct_id]
                    .trait_impl
                    .keys()
                    .copied()
                    .collect::<Vec<_>>(),
                traits,
            ),
            Self::Generic { bounds, .. } => values_match(bounds, traits),
            _ => false,
        }
    }

    pub fn struct_offset(
        &self,
        ptr_size: u64,
        structs: &Store<TypedStruct<'arena>>,
        element: usize,
    ) -> u64 {
        let struct_id = match self {
            Self::Struct { struct_id, .. } => *struct_id,
            _ => unreachable!(),
        };
        let structure = &structs[struct_id];
        assert!(element < structure.elements.len());
        let mut offset = 0;
        for (_, typ) in &structure.elements[0..element] {
            let (size, alignment) = typ.size_and_alignment(ptr_size, structs);
            offset = align(offset, alignment) + size;
        }
        offset = align(
            offset,
            structure.elements[element].1.alignment(ptr_size, structs),
        );
        offset
    }

    pub fn alignment(&self, ptr_size: u64, structs: &Store<TypedStruct<'arena>>) -> u32 {
        if self.refcount() > 0 {
            return ptr_size as u32;
        }

        match self {
            TyKind::Generic { .. }
            | TyKind::PrimitiveSelf(..)
            | TyKind::PrimitiveStr(..)
            | TyKind::DynType { .. }
            | TyKind::UnsizedArray { .. } => {
                unreachable!("generics, self and unsized types don't have an alignment")
            }
            TyKind::Struct { struct_id, .. } => structs[struct_id]
                .elements
                .iter()
                .map(|v| v.1.alignment(ptr_size, structs))
                .max()
                .unwrap_or(1),
            TyKind::SizedArray { typ, .. } => typ.alignment(ptr_size, structs),
            TyKind::Tuple { elements, .. } => elements
                .iter()
                .map(|v| v.alignment(ptr_size, structs))
                .max()
                .unwrap_or(1),
            TyKind::PrimitiveVoid(_)
            | TyKind::PrimitiveNever
            | TyKind::PrimitiveBool(_)
            | TyKind::PrimitiveU8(_)
            | TyKind::PrimitiveI8(_) => 1,
            TyKind::PrimitiveU16(_) | TyKind::PrimitiveI16(_) => 2,
            TyKind::PrimitiveF32(_) | TyKind::PrimitiveU32(_) | TyKind::PrimitiveI32(_) => 4,
            TyKind::PrimitiveF64(_) | TyKind::PrimitiveU64(_) | TyKind::PrimitiveI64(_) => 8,
            TyKind::Function(..) | TyKind::PrimitiveUSize(_) | TyKind::PrimitiveISize(_) => {
                ptr_size as u32
            }
        }
    }

    pub fn size_and_alignment(
        &self,
        ptr_size: u64,
        structs: &Store<TypedStruct<'arena>>,
    ) -> (u64, u32) {
        if self.refcount() > 0 {
            return if self.is_thin_ptr() {
                (ptr_size, ptr_size as u32)
            } else {
                (ptr_size * 2, ptr_size as u32)
            };
        }

        match self {
            TyKind::Generic { .. }
            | TyKind::PrimitiveSelf(..)
            | TyKind::PrimitiveStr(..)
            | TyKind::DynType { .. }
            | TyKind::UnsizedArray { .. } => {
                unreachable!("generics, self and unsized types don't have an alignment")
            }
            TyKind::Struct { struct_id, .. } => {
                let mut size = 0;
                let mut alignment = 1;
                for (_, element) in structs[struct_id].elements.iter() {
                    let (typ_size, typ_alignment) = element.size_and_alignment(ptr_size, structs);
                    alignment = alignment.max(typ_alignment);
                    size = align(size, typ_alignment) + typ_size;
                }
                (align(size, alignment), alignment)
            }
            TyKind::SizedArray {
                typ,
                number_elements,
                ..
            } => {
                let (size, alignment) = typ.size_and_alignment(ptr_size, structs);
                (size * *number_elements as u64, alignment)
            }
            TyKind::Tuple { elements, .. } => {
                let mut size = 0;
                let mut alignment = 1;
                for element in elements.iter() {
                    let (typ_size, typ_alignment) = element.size_and_alignment(ptr_size, structs);
                    alignment = alignment.max(typ_alignment);
                    size = align(size, typ_alignment) + typ_size;
                }
                (align(size, alignment), alignment)
            }
            TyKind::PrimitiveVoid(_)
            | TyKind::PrimitiveNever
            | TyKind::PrimitiveBool(_)
            | TyKind::PrimitiveU8(_)
            | TyKind::PrimitiveI8(_) => (1, 1),
            TyKind::PrimitiveU16(_) | TyKind::PrimitiveI16(_) => (2, 2),
            TyKind::PrimitiveF32(_) | TyKind::PrimitiveU32(_) | TyKind::PrimitiveI32(_) => (4, 4),
            TyKind::PrimitiveF64(_) | TyKind::PrimitiveU64(_) | TyKind::PrimitiveI64(_) => (8, 8),
            TyKind::Function(..) | TyKind::PrimitiveUSize(_) | TyKind::PrimitiveISize(_) => {
                (ptr_size, ptr_size as u32)
            }
        }
    }

    pub fn refcount(&self) -> u8 {
        match self {
            TyKind::PrimitiveNever => 0,
            TyKind::Struct { num_references, .. }
            | TyKind::UnsizedArray { num_references, .. }
            | TyKind::SizedArray { num_references, .. }
            | TyKind::DynType { num_references, .. }
            | TyKind::Tuple { num_references, .. }
            | TyKind::Generic { num_references, .. }
            | TyKind::Function(_, num_references)
            | TyKind::PrimitiveSelf(num_references)
            | TyKind::PrimitiveVoid(num_references)
            | TyKind::PrimitiveI8(num_references)
            | TyKind::PrimitiveI16(num_references)
            | TyKind::PrimitiveI32(num_references)
            | TyKind::PrimitiveI64(num_references)
            | TyKind::PrimitiveISize(num_references)
            | TyKind::PrimitiveU8(num_references)
            | TyKind::PrimitiveU16(num_references)
            | TyKind::PrimitiveU32(num_references)
            | TyKind::PrimitiveU64(num_references)
            | TyKind::PrimitiveUSize(num_references)
            | TyKind::PrimitiveF32(num_references)
            | TyKind::PrimitiveF64(num_references)
            | TyKind::PrimitiveStr(num_references)
            | TyKind::PrimitiveBool(num_references) => *num_references,
        }
    }

    pub fn is_sized(&self) -> bool {
        if self.refcount() > 0 {
            return true;
        }
        match self {
            TyKind::Generic { sized, .. } => *sized,
            TyKind::PrimitiveSelf(_) => unreachable!("Self should be resolved"),
            TyKind::PrimitiveStr(num_references)
            | TyKind::UnsizedArray { num_references, .. }
            | TyKind::DynType { num_references, .. } => *num_references > 0,
            _ => true,
        }
    }

    pub fn is_thin_ptr(&self) -> bool {
        assert!(self.refcount() != 0);
        // &&str is a thin pointer as it is a reference to a fat pointer, which is a thin pointer
        if self.refcount() > 1 {
            return true;
        }
        match self {
            TyKind::Generic { sized, .. } => *sized,
            TyKind::PrimitiveNever => unreachable!("never can never be referenced"),
            TyKind::PrimitiveSelf(_) => unreachable!("Self should be resolved by now"),
            TyKind::DynType { .. } | TyKind::UnsizedArray { .. } | TyKind::PrimitiveStr(_) => false,
            TyKind::Struct { .. }
            | TyKind::Tuple { .. }
            | TyKind::SizedArray { .. }
            | TyKind::Function(..)
            | TyKind::PrimitiveVoid(_)
            | TyKind::PrimitiveI8(_)
            | TyKind::PrimitiveI16(_)
            | TyKind::PrimitiveI32(_)
            | TyKind::PrimitiveI64(_)
            | TyKind::PrimitiveISize(_)
            | TyKind::PrimitiveU8(_)
            | TyKind::PrimitiveU16(_)
            | TyKind::PrimitiveU32(_)
            | TyKind::PrimitiveU64(_)
            | TyKind::PrimitiveUSize(_)
            | TyKind::PrimitiveF32(_)
            | TyKind::PrimitiveF64(_)
            | TyKind::PrimitiveBool(_) => true,
        }
    }

    pub fn from_numtype(typ: NumberType) -> Option<Self> {
        match typ {
            NumberType::F32 => Some(Self::PrimitiveF32(0)),
            NumberType::F64 => Some(Self::PrimitiveF64(0)),
            NumberType::I8 => Some(Self::PrimitiveI8(0)),
            NumberType::I16 => Some(Self::PrimitiveI16(0)),
            NumberType::I32 => Some(Self::PrimitiveI32(0)),
            NumberType::I64 => Some(Self::PrimitiveI64(0)),
            NumberType::Isize => Some(Self::PrimitiveISize(0)),
            NumberType::U8 => Some(Self::PrimitiveU8(0)),
            NumberType::U16 => Some(Self::PrimitiveU16(0)),
            NumberType::U32 => Some(Self::PrimitiveU32(0)),
            NumberType::U64 => Some(Self::PrimitiveU64(0)),
            NumberType::Usize => Some(Self::PrimitiveUSize(0)),
            NumberType::None => None,
        }
    }

    pub fn take_ref(mut self) -> Self {
        match &mut self {
            TyKind::DynType { num_references, .. }
            | TyKind::Struct { num_references, .. }
            | TyKind::UnsizedArray { num_references, .. }
            | TyKind::SizedArray { num_references, .. }
            | TyKind::Tuple { num_references, .. }
            | TyKind::Function(_, num_references)
            | TyKind::PrimitiveVoid(num_references)
            | TyKind::PrimitiveI8(num_references)
            | TyKind::PrimitiveI16(num_references)
            | TyKind::PrimitiveI32(num_references)
            | TyKind::PrimitiveI64(num_references)
            | TyKind::PrimitiveISize(num_references)
            | TyKind::PrimitiveU8(num_references)
            | TyKind::PrimitiveU16(num_references)
            | TyKind::PrimitiveU32(num_references)
            | TyKind::PrimitiveU64(num_references)
            | TyKind::PrimitiveUSize(num_references)
            | TyKind::PrimitiveF32(num_references)
            | TyKind::PrimitiveF64(num_references)
            | TyKind::PrimitiveStr(num_references)
            | TyKind::PrimitiveBool(num_references)
            | TyKind::PrimitiveSelf(num_references)
            | TyKind::Generic { num_references, .. } => *num_references += 1,
            TyKind::PrimitiveNever => {}
        }
        self
    }

    pub fn deref(&self) -> Option<Self> {
        match self {
            TyKind::DynType { num_references, .. }
            | TyKind::Struct { num_references, .. }
            | TyKind::UnsizedArray { num_references, .. }
            | TyKind::SizedArray { num_references, .. }
            | TyKind::Tuple { num_references, .. }
            | TyKind::Function(_, num_references)
            | TyKind::PrimitiveVoid(num_references)
            | TyKind::PrimitiveI8(num_references)
            | TyKind::PrimitiveI16(num_references)
            | TyKind::PrimitiveI32(num_references)
            | TyKind::PrimitiveI64(num_references)
            | TyKind::PrimitiveISize(num_references)
            | TyKind::PrimitiveU8(num_references)
            | TyKind::PrimitiveU16(num_references)
            | TyKind::PrimitiveU32(num_references)
            | TyKind::PrimitiveU64(num_references)
            | TyKind::PrimitiveUSize(num_references)
            | TyKind::PrimitiveF32(num_references)
            | TyKind::PrimitiveF64(num_references)
            | TyKind::PrimitiveStr(num_references)
            | TyKind::PrimitiveBool(num_references)
            | TyKind::PrimitiveSelf(num_references)
            | TyKind::Generic { num_references, .. } => {
                if *num_references == 0 {
                    None
                } else {
                    Some(self.clone().with_num_refs(*num_references - 1))
                }
            }
            TyKind::PrimitiveNever => Some(TyKind::PrimitiveNever),
        }
    }

    pub fn without_ref(mut self) -> Self {
        match &mut self {
            TyKind::DynType { num_references, .. }
            | TyKind::Struct { num_references, .. }
            | TyKind::UnsizedArray { num_references, .. }
            | TyKind::SizedArray { num_references, .. }
            | TyKind::Tuple { num_references, .. }
            | TyKind::Function(_, num_references)
            | TyKind::PrimitiveVoid(num_references)
            | TyKind::PrimitiveI8(num_references)
            | TyKind::PrimitiveI16(num_references)
            | TyKind::PrimitiveI32(num_references)
            | TyKind::PrimitiveI64(num_references)
            | TyKind::PrimitiveISize(num_references)
            | TyKind::PrimitiveU8(num_references)
            | TyKind::PrimitiveU16(num_references)
            | TyKind::PrimitiveU32(num_references)
            | TyKind::PrimitiveU64(num_references)
            | TyKind::PrimitiveUSize(num_references)
            | TyKind::PrimitiveF32(num_references)
            | TyKind::PrimitiveF64(num_references)
            | TyKind::PrimitiveStr(num_references)
            | TyKind::PrimitiveBool(num_references)
            | TyKind::PrimitiveSelf(num_references)
            | TyKind::Generic { num_references, .. } => {
                *num_references = 0;
                self
            }
            TyKind::PrimitiveNever => self,
        }
    }

    pub fn with_num_refs(mut self, num_refs: u8) -> Self {
        match &mut self {
            TyKind::DynType { num_references, .. }
            | TyKind::Struct { num_references, .. }
            | TyKind::UnsizedArray { num_references, .. }
            | TyKind::SizedArray { num_references, .. }
            | TyKind::Tuple { num_references, .. }
            | TyKind::Function(_, num_references)
            | TyKind::PrimitiveVoid(num_references)
            | TyKind::PrimitiveI8(num_references)
            | TyKind::PrimitiveI16(num_references)
            | TyKind::PrimitiveI32(num_references)
            | TyKind::PrimitiveI64(num_references)
            | TyKind::PrimitiveISize(num_references)
            | TyKind::PrimitiveU8(num_references)
            | TyKind::PrimitiveU16(num_references)
            | TyKind::PrimitiveU32(num_references)
            | TyKind::PrimitiveU64(num_references)
            | TyKind::PrimitiveUSize(num_references)
            | TyKind::PrimitiveF32(num_references)
            | TyKind::PrimitiveF64(num_references)
            | TyKind::PrimitiveStr(num_references)
            | TyKind::PrimitiveBool(num_references)
            | TyKind::PrimitiveSelf(num_references)
            | TyKind::Generic { num_references, .. } => {
                *num_references = num_refs;
                self
            }
            TyKind::PrimitiveNever => self,
        }
    }

    pub fn is_asm_primitive(&self) -> bool {
        matches!(
            self,
            TyKind::PrimitiveI8(0)
                | TyKind::PrimitiveI16(0)
                | TyKind::PrimitiveI32(0)
                | TyKind::PrimitiveI64(0)
                | TyKind::PrimitiveISize(0)
                | TyKind::PrimitiveU8(0)
                | TyKind::PrimitiveU16(0)
                | TyKind::PrimitiveU32(0)
                | TyKind::PrimitiveU64(0)
                | TyKind::PrimitiveUSize(0)
                | TyKind::PrimitiveF32(0)
                | TyKind::PrimitiveF64(0)
                | TyKind::PrimitiveBool(0)
        )
    }

    pub fn is_primitive(&self) -> bool {
        matches!(
            self,
            TyKind::PrimitiveVoid(_)
                | TyKind::PrimitiveNever
                | TyKind::PrimitiveI8(_)
                | TyKind::PrimitiveI16(_)
                | TyKind::PrimitiveI32(_)
                | TyKind::PrimitiveI64(_)
                | TyKind::PrimitiveISize(_)
                | TyKind::PrimitiveU8(_)
                | TyKind::PrimitiveU16(_)
                | TyKind::PrimitiveU32(_)
                | TyKind::PrimitiveU64(_)
                | TyKind::PrimitiveUSize(_)
                | TyKind::PrimitiveF32(_)
                | TyKind::PrimitiveF64(_)
                | TyKind::PrimitiveStr(_)
                | TyKind::PrimitiveBool(_)
        )
    }

    /// Returns whether the type is an integer or unsigned integer
    pub fn is_int_like(&self) -> bool {
        matches!(
            self,
            Self::PrimitiveU8(0)
                | Self::PrimitiveI8(0)
                | Self::PrimitiveU16(0)
                | Self::PrimitiveI16(0)
                | Self::PrimitiveU32(0)
                | Self::PrimitiveI32(0)
                | Self::PrimitiveU64(0)
                | Self::PrimitiveI64(0)
                | Self::PrimitiveUSize(0)
                | Self::PrimitiveISize(0)
        )
    }

    pub fn is_unsigned(&self) -> bool {
        matches!(
            self,
            Self::PrimitiveU8(0)
                | Self::PrimitiveU16(0)
                | Self::PrimitiveU32(0)
                | Self::PrimitiveU64(0)
                | Self::PrimitiveUSize(0)
        )
    }

    pub fn is_signed(&self) -> bool {
        matches!(
            self,
            Self::PrimitiveI8(0)
                | Self::PrimitiveI16(0)
                | Self::PrimitiveI32(0)
                | Self::PrimitiveI64(0)
                | Self::PrimitiveISize(0)
        )
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Self::PrimitiveF32(0) | Self::PrimitiveF64(0))
    }

    pub fn get_bitwidth(&self, isize_bitwidth: u32) -> u32 {
        match self {
            TyKind::DynType { .. }
            | TyKind::Struct { .. }
            | TyKind::UnsizedArray { .. }
            | TyKind::SizedArray { .. }
            | TyKind::Tuple { .. }
            | TyKind::Generic { .. }
            | TyKind::Function(..)
            | TyKind::PrimitiveSelf(_)
            | TyKind::PrimitiveVoid(_)
            | TyKind::PrimitiveBool(_)
            | TyKind::PrimitiveStr(_)
            | TyKind::PrimitiveNever => 0,
            TyKind::PrimitiveU8(_) | TyKind::PrimitiveI8(_) => 8,
            TyKind::PrimitiveU16(_) | TyKind::PrimitiveI16(_) => 16,
            TyKind::PrimitiveF32(_) | TyKind::PrimitiveU32(_) | TyKind::PrimitiveI32(_) => 32,
            TyKind::PrimitiveF64(_) | TyKind::PrimitiveU64(_) | TyKind::PrimitiveI64(_) => 64,
            TyKind::PrimitiveUSize(_) | TyKind::PrimitiveISize(_) => isize_bitwidth,
        }
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, TyKind::PrimitiveBool(0))
    }
}

impl Display for TyKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.refcount() {
            f.write_char('&')?;
        }

        match self {
            TyKind::Struct { name, .. } => Display::fmt(name, f),
            TyKind::DynType { trait_refs, .. } => {
                f.write_str("dyn ")?;
                for (i, (_, trait_ref)) in trait_refs.iter().enumerate() {
                    if i != 0 {
                        f.write_str(" + ")?;
                    }
                    Display::fmt(trait_ref, f)?;
                }
                Ok(())
            }
            TyKind::Tuple { elements, .. } => {
                f.write_char('(')?;
                for (i, elem) in elements.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?
                    }
                    Display::fmt(elem, f)?;
                }
                f.write_char(')')
            }
            TyKind::UnsizedArray { typ, .. } => {
                f.write_char('[')?;
                Display::fmt(typ, f)?;
                f.write_char(']')
            }
            TyKind::SizedArray {
                typ,
                number_elements,
                ..
            } => {
                f.write_char('[')?;
                Display::fmt(typ, f)?;
                f.write_str("; ")?;
                Display::fmt(number_elements, f)?;
                f.write_char(']')
            }
            TyKind::Function(contract, _) => {
                f.write_str("fn")?;
                f.write_char('(')?;
                for (idx, typ) in contract.arguments.iter().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }
                    Display::fmt(typ, f)?;
                }
                f.write_char(')')?;
                if matches!(**contract.return_type, TyKind::PrimitiveVoid(0)) {
                    return Ok(());
                }

                f.write_str(" -> ")?;
                Display::fmt(&contract.return_type, f)
            }
            TyKind::PrimitiveSelf(_) => f.write_str("Self"),
            TyKind::PrimitiveVoid(_) => f.write_str("void"),
            TyKind::PrimitiveI8(_) => f.write_str("i8"),
            TyKind::PrimitiveI16(_) => f.write_str("i16"),
            TyKind::PrimitiveI32(_) => f.write_str("i32"),
            TyKind::PrimitiveI64(_) => f.write_str("i64"),
            TyKind::PrimitiveU8(_) => f.write_str("u8"),
            TyKind::PrimitiveU16(_) => f.write_str("u16"),
            TyKind::PrimitiveU32(_) => f.write_str("u32"),
            TyKind::PrimitiveU64(_) => f.write_str("u64"),
            TyKind::PrimitiveF32(_) => f.write_str("f32"),
            TyKind::PrimitiveF64(_) => f.write_str("f64"),
            TyKind::PrimitiveBool(_) => f.write_str("bool"),
            TyKind::PrimitiveStr(_) => f.write_str("str"),
            TyKind::PrimitiveISize(_) => f.write_str("isize"),
            TyKind::PrimitiveUSize(_) => f.write_str("usize"),
            TyKind::PrimitiveNever => f.write_str("!"),
            TyKind::Generic {
                name,
                sized,
                bounds: traits,
                generic_id,
                ..
            } => {
                if !*sized {
                    f.write_str("unsized ")?;
                }
                Display::fmt(name, f)?;
                if !traits.is_empty() {
                    f.write_str(": ")?;
                    for (i, trait_ref) in traits.iter().enumerate() {
                        if i != 0 {
                            f.write_str(" + ")?;
                        }
                        Display::fmt(trait_ref, f)?;
                    }
                }
                f.write_str(" (#")?;
                Display::fmt(generic_id, f)?;
                f.write_char(')')
            }
        }
    }
}

#[derive(Clone, Debug, Default)]
pub enum TypeSuggestion<'arena> {
    Struct(StoreKey<TypedStruct<'arena>>),
    Array(Box<TypeSuggestion<'arena>>),
    UnsizedArray(Box<TypeSuggestion<'arena>>),
    Number(NumberType),
    Tuple(Vec<TypeSuggestion<'arena>>),
    Bool,
    #[default]
    Unknown,
}

impl Display for TypeSuggestion<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeSuggestion::Struct(id) => f.write_fmt(format_args!("<struct {id}>")),
            TypeSuggestion::Array(v) | TypeSuggestion::UnsizedArray(v) => {
                f.write_fmt(format_args!("[{v}]"))
            }
            TypeSuggestion::Bool => f.write_str("bool"),
            TypeSuggestion::Number(number_type) => Display::fmt(number_type, f),
            TypeSuggestion::Tuple(elements) => {
                f.write_char('(')?;
                for (i, elem) in elements.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?
                    }
                    Display::fmt(elem, f)?
                }
                f.write_char(')')
            }
            TypeSuggestion::Unknown => f.write_str("{unknown}"),
        }
    }
}

impl<'arena> TypeSuggestion<'arena> {
    pub fn to_type(&self, ctx: &TypecheckingContext<'arena>) -> Option<Ty<'arena>> {
        match self {
            TypeSuggestion::Struct(id) => Some(ctx.ctx.intern_ty(TyKind::Struct {
                struct_id: *id,
                name: ctx.structs.read()[*id].name,
                num_references: 0,
            })),
            TypeSuggestion::Array(type_suggestion)
            | TypeSuggestion::UnsizedArray(type_suggestion) => type_suggestion
                .to_type(ctx)
                .map(|typ| TyKind::SizedArray {
                    typ,
                    num_references: 0,
                    number_elements: 0,
                })
                .map(|typ| ctx.ctx.intern_ty(typ)),
            TypeSuggestion::Number(number_type) => Some(match number_type {
                NumberType::F32 => default_types::f32,
                NumberType::F64 => default_types::f64,
                NumberType::I8 => default_types::i8,
                NumberType::I16 => default_types::i16,
                NumberType::None | NumberType::I32 => default_types::i32,
                NumberType::I64 => default_types::i64,
                NumberType::Isize => default_types::isize,
                NumberType::U8 => default_types::u8,
                NumberType::U16 => default_types::u16,
                NumberType::U32 => default_types::u32,
                NumberType::U64 => default_types::u64,
                NumberType::Usize => default_types::usize,
            }),
            TypeSuggestion::Bool => Some(default_types::bool),
            TypeSuggestion::Tuple(elems) => {
                let mut elements = Vec::with_capacity(elems.len());
                for elem in elems {
                    elements.push(elem.to_type(ctx)?);
                }
                let elements = ctx.ctx.intern_tylist(&elements);
                Some(ctx.ctx.intern_ty(TyKind::Tuple {
                    elements,
                    num_references: 0,
                }))
            }
            TypeSuggestion::Unknown => None,
        }
    }

    pub fn from_type(typ: Ty<'arena>) -> Self {
        match &**typ {
            TyKind::PrimitiveStr(_)
            | TyKind::PrimitiveSelf(_)
            | TyKind::Generic { .. }
            | TyKind::Function(..)
            | TyKind::PrimitiveVoid(_)
            | TyKind::PrimitiveNever
            | TyKind::DynType { .. } => Self::Unknown,
            TyKind::Struct { struct_id, .. } => Self::Struct(*struct_id),
            TyKind::SizedArray { typ, .. } => Self::Array(Box::new(Self::from_type(*typ))),
            TyKind::UnsizedArray { typ, .. } => Self::UnsizedArray(Box::new(Self::from_type(*typ))),
            TyKind::PrimitiveI8(_) => Self::Number(NumberType::I8),
            TyKind::PrimitiveI16(_) => Self::Number(NumberType::I16),
            TyKind::PrimitiveI32(_) => Self::Number(NumberType::I32),
            TyKind::PrimitiveI64(_) => Self::Number(NumberType::I64),
            TyKind::PrimitiveISize(_) => Self::Number(NumberType::Isize),
            TyKind::PrimitiveU8(_) => Self::Number(NumberType::U8),
            TyKind::PrimitiveU16(_) => Self::Number(NumberType::U16),
            TyKind::PrimitiveU32(_) => Self::Number(NumberType::U32),
            TyKind::PrimitiveU64(_) => Self::Number(NumberType::U64),
            TyKind::PrimitiveUSize(_) => Self::Number(NumberType::Usize),
            TyKind::PrimitiveF32(_) => Self::Number(NumberType::F32),
            TyKind::PrimitiveF64(_) => Self::Number(NumberType::F64),
            TyKind::PrimitiveBool(_) => Self::Bool,
            TyKind::Tuple { elements, .. } => {
                Self::Tuple(elements.iter().copied().map(Self::from_type).collect())
            }
        }
    }
}
