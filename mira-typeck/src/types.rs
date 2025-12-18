use mira_common::index::IndexMap;
use mira_parser::module::{StructId, TraitId};
use mira_spans::{Arena, ArenaList, extra_traits, interner, owned_intern};
use std::fmt::{Debug, Display, Write};
use std::hash::Hash;

use crate::context::TypeCtx;
use crate::monomorphisation::{Substitute, SubstitutionCtx};
use mira_lexer::NumberType;
use mira_parser::TypeRef;
use mira_spans::Ident;

use super::{TypeckCtx, TypedStruct};

pub mod default_types {
    #![allow(non_upper_case_globals)]
    use super::{Ty, TyKind};

    macro_rules! tydef {
        ($($name:ident $kind:ident),* $(,)?) => {
            $(pub static $name: Ty<'static> = Ty(&TyKind::$kind);)*
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

    // types used by the compiler
    pub static u8_array: Ty<'static> = Ty(&TyKind::UnsizedArray(u8));
    pub static u8_slice: Ty<'static> = Ty(&TyKind::Ref(u8_array));
    pub static str_ref: Ty<'static> = Ty(&TyKind::Ref(str));
    pub static self_ref: Ty<'static> = Ty(&TyKind::Ref(self_));
    pub static void_ref: Ty<'static> = Ty(&TyKind::Ref(void));
    pub static u8_ref: Ty<'static> = Ty(&TyKind::Ref(u8));

    pub static ALL: &[&Ty<'static>] = &[
        &u8, &u16, &u32, &u64, &usize, &i8, &i16, &i32, &i64, &isize, &bool, &void, &never,
        &str_ref, &self_ref, &void_ref, &self_, &u8_ref, &u8_array, &u8_slice,
    ];
}

pub static EMPTY_TYLIST: TyList<'static> = TyList(&[]);
interner!(
    TypeListInterner,
    TyList,
    [Ty<'arena>],
    |arena, types| arena.alloc_slice(types),
    &[&EMPTY_TYLIST]
);
extra_traits!(for TyList impl debug);
impl<'ctx> TyList<'ctx> {
    pub const EMPTY: Self = EMPTY_TYLIST;

    pub fn subst<T: Substitute<'ctx>>(self, ctx: TypeCtx<'ctx>, v: T) -> T {
        v.substitute(&SubstitutionCtx::new(ctx, &self))
    }
}
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
pub enum TyKind<'ctx> {
    DynType(ArenaList<'ctx, (TraitId, Ident<'ctx>)>),
    Struct {
        struct_id: StructId,
        generics: TyList<'ctx>,
        name: Ident<'ctx>,
    },
    UnsizedArray(Ty<'ctx>),
    SizedArray {
        ty: Ty<'ctx>,
        number_elements: usize,
    },
    Tuple(TyList<'ctx>),
    Function(FunctionType<'ctx>),

    PrimitiveVoid,
    PrimitiveNever,

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

    PrimitiveF32,
    PrimitiveF64,

    PrimitiveStr,
    PrimitiveBool,
    PrimitiveSelf,

    Ref(Ty<'ctx>),

    Generic {
        name: Ident<'ctx>,
        generic_id: u8,
        bounds: ArenaList<'ctx, TraitId>,
        sized: bool,
    },
}

/// wraps the type in the amount of references, adding to the already existing references.
/// e.g.(u8, 6) -> &&&&&&u8
/// e.g. (&&u8, 2) -> &&&&u8
pub(crate) fn with_refcount<'arena>(
    ctx: TypeCtx<'arena>,
    mut ty: Ty<'arena>,
    mut refcount: u8,
) -> Ty<'arena> {
    while refcount > 0 {
        refcount -= 1;
        ty = ctx.intern_ty(TyKind::Ref(ty));
    }
    ty
}

pub(crate) fn resolve_primitive_type<'arena>(
    ctx: TypeCtx<'arena>,
    ty: &TypeRef<'arena>,
) -> Option<Ty<'arena>> {
    match ty {
        TypeRef::Never(_) => Some(default_types::never),
        TypeRef::Void(_, 0) => Some(default_types::void),
        TypeRef::Void(_, refcount) => Some(with_refcount(ctx, default_types::void, *refcount)),
        TypeRef::Reference {
            num_references,
            type_name,
            span: _,
        } if type_name.entries.len() == 1 && type_name.entries[0].1.is_empty() => {
            let ty = match &*type_name.entries[0].0 {
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
            };
            Some(with_refcount(ctx, ty?, *num_references))
        }
        _ => None,
    }
}

fn align_up(ptr: u64, alignment: u32) -> u64 {
    if ptr.is_multiple_of(alignment as u64) {
        ptr
    } else {
        (ptr & !(alignment as u64 - 1)) + alignment as u64
    }
}

fn values_match(left: &[TraitId], right: &[TraitId]) -> bool {
    for v in right {
        if !left.contains(v) {
            return false;
        }
    }
    true
}

impl<'arena> Ty<'arena> {
    pub fn deref(self) -> Option<Self> {
        match **self {
            TyKind::Ref(v) => Some(v),
            TyKind::PrimitiveNever => Some(self),
            _ => None,
        }
    }

    pub fn without_ref(mut self) -> Self {
        while let TyKind::Ref(v) = **self {
            self = v;
        }
        self
    }

    pub fn take_ref(self, ctx: TypeCtx<'arena>) -> Self {
        if self == default_types::never {
            self
        } else {
            ctx.intern_ty(TyKind::Ref(self))
        }
    }

    pub fn with_num_refs(mut self, num_refs: u8, ctx: TypeCtx<'arena>) -> Self {
        let refcount = self.refcount();
        if refcount == num_refs {
            return self;
        }
        if refcount < num_refs {
            for _ in 0..num_refs - refcount {
                self = ctx.intern_ty(TyKind::Ref(self));
            }
        } else {
            for _ in 0..refcount - num_refs {
                self = self.deref().unwrap();
            }
        }
        self
    }

    /// returns the amount of references removed and the type without references
    pub fn remove_refs(mut self) -> (u8, Self) {
        let mut count = 0;
        while let TyKind::Ref(ty) = **self {
            count += 1;
            self = ty;
        }
        (count, self)
    }

    pub fn is_bool(self) -> bool {
        self == default_types::bool
    }

    /// checks if self is `void` or `!`
    pub fn is_voidlike(self) -> bool {
        self == default_types::void || self == default_types::never
    }
}
impl<'ctx> TyKind<'ctx> {
    pub fn implements(&self, traits: &[TraitId], tc_ctx: &TypeckCtx<'_>) -> bool {
        let langitem_reader = tc_ctx.lang_items.read();

        let struct_id = match self {
            TyKind::Ref(v) => {
                if let TyKind::DynType(trait_refs) = &***v {
                    return values_match(
                        &trait_refs.iter().map(|v| v.0).collect::<Vec<_>>(),
                        traits,
                    );
                } else {
                    return false;
                }
            }
            &TyKind::Struct { struct_id, .. } => Some(struct_id),
            Self::Generic { bounds, .. } => return values_match(bounds, traits),

            TyKind::PrimitiveI8 => langitem_reader.i8,
            TyKind::PrimitiveI16 => langitem_reader.i16,
            TyKind::PrimitiveI32 => langitem_reader.i32,
            TyKind::PrimitiveI64 => langitem_reader.i64,
            TyKind::PrimitiveISize => langitem_reader.isize,
            TyKind::PrimitiveU8 => langitem_reader.u8,
            TyKind::PrimitiveU16 => langitem_reader.u16,
            TyKind::PrimitiveU32 => langitem_reader.u32,
            TyKind::PrimitiveU64 => langitem_reader.u64,
            TyKind::PrimitiveUSize => langitem_reader.usize,
            TyKind::PrimitiveF32 => langitem_reader.f32,
            TyKind::PrimitiveF64 => langitem_reader.f64,
            TyKind::PrimitiveStr => langitem_reader.str,
            TyKind::PrimitiveBool => langitem_reader.bool,
            _ => return traits.is_empty(),
        };
        drop(langitem_reader);
        let Some(struct_id) = struct_id else {
            return traits.is_empty();
        };

        values_match(
            &tc_ctx.structs.read()[struct_id]
                .trait_impl
                .keys()
                .collect::<Vec<_>>(),
            traits,
        )
    }

    pub fn struct_offset(
        &self,
        ptr_size: u64,
        structs: &IndexMap<StructId, TypedStruct<'ctx>>,
        element: usize,
        ctx: TypeCtx<'ctx>,
    ) -> u64 {
        let &Self::Struct {
            struct_id,
            generics,
            ..
        } = self
        else {
            unreachable!()
        };
        let subst_ctx = SubstitutionCtx::new(ctx, &generics);
        let structure = &structs[struct_id];

        assert!(element < structure.elements.len());
        let mut offset = 0;
        for (_, ty, _) in &structure.elements[0..element] {
            let (size, alignment) = ty
                .substitute(&subst_ctx)
                .size_and_alignment(ptr_size, structs, ctx);
            offset = align_up(offset, alignment) + size;
        }
        offset = align_up(
            offset,
            structure.elements[element]
                .1
                .substitute(&subst_ctx)
                .alignment(ptr_size, structs, ctx),
        );
        offset
    }

    pub fn size(
        &self,
        ptr_size: u64,
        structs: &IndexMap<StructId, TypedStruct<'ctx>>,
        ctx: TypeCtx<'ctx>,
    ) -> u64 {
        match self {
            TyKind::Generic { .. }
            | TyKind::PrimitiveSelf
            | TyKind::PrimitiveStr
            | TyKind::DynType { .. }
            | TyKind::UnsizedArray { .. } => {
                unreachable!("generics, self and unsized types don't have an alignment")
            }
            TyKind::Ref(v) if v.is_sized() => ptr_size,
            TyKind::Ref(_) => ptr_size * 2,
            &TyKind::Struct {
                struct_id,
                generics,
                ..
            } => {
                let mut size = 0;
                let mut alignment = 1;
                for &(_, element, _) in structs[struct_id].elements.iter() {
                    let (typ_size, typ_alignment) = generics
                        .subst(ctx, element)
                        .size_and_alignment(ptr_size, structs, ctx);
                    alignment = alignment.max(typ_alignment);
                    size = align_up(size, typ_alignment) + typ_size;
                }
                align_up(size, alignment)
            }
            &TyKind::SizedArray {
                ty,
                number_elements,
            } => {
                let size = ty.size(ptr_size, structs, ctx);
                size * number_elements as u64
            }
            TyKind::Tuple(elements) => {
                let mut size = 0;
                let mut alignment = 1;
                for element in elements.iter() {
                    let (typ_size, typ_alignment) =
                        element.size_and_alignment(ptr_size, structs, ctx);
                    alignment = alignment.max(typ_alignment);
                    size = align_up(size, typ_alignment) + typ_size;
                }
                align_up(size, alignment)
            }
            TyKind::PrimitiveVoid | TyKind::PrimitiveNever => 0,
            TyKind::PrimitiveBool | TyKind::PrimitiveU8 | TyKind::PrimitiveI8 => 1,
            TyKind::PrimitiveU16 | TyKind::PrimitiveI16 => 2,
            TyKind::PrimitiveF32 | TyKind::PrimitiveU32 | TyKind::PrimitiveI32 => 4,
            TyKind::PrimitiveF64 | TyKind::PrimitiveU64 | TyKind::PrimitiveI64 => 8,
            TyKind::Function(_) | TyKind::PrimitiveUSize | TyKind::PrimitiveISize => ptr_size,
        }
    }

    pub fn alignment(
        &self,
        ptr_size: u64,
        structs: &IndexMap<StructId, TypedStruct<'ctx>>,
        ctx: TypeCtx<'ctx>,
    ) -> u32 {
        match self {
            TyKind::Ref(_) => ptr_size as u32,
            TyKind::Generic { .. }
            | TyKind::PrimitiveSelf
            | TyKind::PrimitiveStr
            | TyKind::DynType { .. }
            | TyKind::UnsizedArray { .. } => {
                unreachable!("generics, self and unsized types don't have an alignment")
            }
            &TyKind::Struct {
                struct_id,
                generics,
                ..
            } => {
                let subst_ctx = SubstitutionCtx::new(ctx, &generics);
                structs[struct_id]
                    .elements
                    .iter()
                    .map(|v| v.1.substitute(&subst_ctx).alignment(ptr_size, structs, ctx))
                    .max()
                    .unwrap_or(1)
            }
            TyKind::SizedArray { ty, .. } => ty.alignment(ptr_size, structs, ctx),
            TyKind::Tuple(elements) => elements
                .iter()
                .map(|v| v.alignment(ptr_size, structs, ctx))
                .max()
                .unwrap_or(1),
            TyKind::PrimitiveVoid
            | TyKind::PrimitiveNever
            | TyKind::PrimitiveBool
            | TyKind::PrimitiveU8
            | TyKind::PrimitiveI8 => 1,
            TyKind::PrimitiveU16 | TyKind::PrimitiveI16 => 2,
            TyKind::PrimitiveF32 | TyKind::PrimitiveU32 | TyKind::PrimitiveI32 => 4,
            TyKind::PrimitiveF64 | TyKind::PrimitiveU64 | TyKind::PrimitiveI64 => 8,
            TyKind::Function(_) | TyKind::PrimitiveUSize | TyKind::PrimitiveISize => {
                ptr_size as u32
            }
        }
    }

    pub fn size_and_alignment(
        &self,
        ptr_size: u64,
        structs: &IndexMap<StructId, TypedStruct<'ctx>>,
        ctx: TypeCtx<'ctx>,
    ) -> (u64, u32) {
        match self {
            TyKind::Generic { .. }
            | TyKind::PrimitiveSelf
            | TyKind::PrimitiveStr
            | TyKind::DynType { .. }
            | TyKind::UnsizedArray { .. } => {
                unreachable!("generics, self and unsized types don't have an alignment")
            }
            TyKind::Ref(v) => {
                if v.is_sized() {
                    (ptr_size, ptr_size as u32)
                } else {
                    (ptr_size * 2, ptr_size as u32)
                }
            }
            &TyKind::Struct {
                struct_id,
                generics,
                ..
            } => {
                let mut size = 0;
                let mut alignment = 1;
                for &(_, element, _) in structs[struct_id].elements.iter() {
                    let (typ_size, typ_alignment) = generics
                        .subst(ctx, element)
                        .size_and_alignment(ptr_size, structs, ctx);
                    alignment = alignment.max(typ_alignment);
                    size = align_up(size, typ_alignment) + typ_size;
                }
                (align_up(size, alignment), alignment)
            }
            TyKind::SizedArray {
                ty,
                number_elements,
            } => {
                let (size, alignment) = ty.size_and_alignment(ptr_size, structs, ctx);
                (size * *number_elements as u64, alignment)
            }
            TyKind::Tuple(elements) => {
                let mut size = 0;
                let mut alignment = 1;
                for element in elements.iter() {
                    let (typ_size, typ_alignment) =
                        element.size_and_alignment(ptr_size, structs, ctx);
                    alignment = alignment.max(typ_alignment);
                    size = align_up(size, typ_alignment) + typ_size;
                }
                (align_up(size, alignment), alignment)
            }
            TyKind::PrimitiveVoid | TyKind::PrimitiveNever => (0, 1),
            TyKind::PrimitiveBool | TyKind::PrimitiveU8 | TyKind::PrimitiveI8 => (1, 1),
            TyKind::PrimitiveU16 | TyKind::PrimitiveI16 => (2, 2),
            TyKind::PrimitiveF32 | TyKind::PrimitiveU32 | TyKind::PrimitiveI32 => (4, 4),
            TyKind::PrimitiveF64 | TyKind::PrimitiveU64 | TyKind::PrimitiveI64 => (8, 8),
            TyKind::Function(_) | TyKind::PrimitiveUSize | TyKind::PrimitiveISize => {
                (ptr_size, ptr_size as u32)
            }
        }
    }

    /// returns if self is [_], [_; _] or (_, _, ...)
    pub fn is_indexable(&self) -> bool {
        matches!(
            self,
            TyKind::UnsizedArray { .. } | TyKind::SizedArray { .. } | TyKind::Tuple { .. }
        )
    }

    /// returns if this type is &_
    pub fn has_refs(&self) -> bool {
        matches!(self, Self::Ref(_))
    }

    /// returns if this type is &&_
    pub fn has_double_refs(&self) -> bool {
        match self {
            Self::Ref(c) => c.has_refs(),
            _ => false,
        }
    }

    pub fn refcount(&self) -> u8 {
        let mut count = 0;
        let mut ty = self;
        while let Self::Ref(t) = ty {
            count += 1;
            ty = &***t;
        }
        count
    }

    pub fn is_zst(&self, structs: &IndexMap<StructId, TypedStruct<'ctx>>) -> bool {
        match self {
            &TyKind::Struct { struct_id, .. } => structs[struct_id]
                .elements
                .iter()
                .all(|(_, ty, _)| ty.is_zst(structs)),
            &TyKind::SizedArray {
                ty,
                number_elements,
            } => number_elements == 0 || ty.is_zst(structs),
            TyKind::Tuple(tys) => tys.iter().all(|v| v.is_zst(structs)),

            TyKind::PrimitiveVoid | TyKind::PrimitiveNever => true,

            TyKind::Function(_)
            | TyKind::PrimitiveI8
            | TyKind::PrimitiveI16
            | TyKind::PrimitiveI32
            | TyKind::PrimitiveI64
            | TyKind::PrimitiveISize
            | TyKind::PrimitiveU8
            | TyKind::PrimitiveU16
            | TyKind::PrimitiveU32
            | TyKind::PrimitiveU64
            | TyKind::PrimitiveUSize
            | TyKind::PrimitiveF32
            | TyKind::PrimitiveF64
            | TyKind::PrimitiveBool
            | TyKind::Ref(_) => false,

            TyKind::UnsizedArray(_) | TyKind::PrimitiveStr | TyKind::DynType(_) => {
                unreachable!("unsized types can't be sized")
            }
            TyKind::PrimitiveSelf { .. } => unreachable!("Self has to be resolved to get a size"),
            TyKind::Generic { .. } => unreachable!("generics have to be resolved to get a size"),
        }
    }

    pub fn is_sized(&self) -> bool {
        match self {
            TyKind::Generic { sized, .. } => *sized,
            TyKind::PrimitiveSelf => unreachable!("Self should be resolved"),
            TyKind::PrimitiveStr | TyKind::UnsizedArray { .. } | TyKind::DynType { .. } => false,
            _ => true,
        }
    }

    pub fn is_thin_ptr(&self) -> bool {
        let ty = match self {
            TyKind::Ref(v) => v,
            _ => unreachable!("only a pointer can be a thin pointer"),
        };
        // &&str is a thin pointer as it is a reference to a fat pointer, which is a thin pointer
        match &**ty {
            TyKind::Ref(_) => true,
            TyKind::Generic { sized, .. } => *sized,
            TyKind::PrimitiveNever => unreachable!("never can never be referenced"),
            TyKind::PrimitiveSelf => unreachable!("Self should be resolved by now"),
            TyKind::DynType { .. } | TyKind::UnsizedArray { .. } | TyKind::PrimitiveStr => false,
            TyKind::Struct { .. }
            | TyKind::Tuple { .. }
            | TyKind::SizedArray { .. }
            | TyKind::Function(..)
            | TyKind::PrimitiveVoid
            | TyKind::PrimitiveI8
            | TyKind::PrimitiveI16
            | TyKind::PrimitiveI32
            | TyKind::PrimitiveI64
            | TyKind::PrimitiveISize
            | TyKind::PrimitiveU8
            | TyKind::PrimitiveU16
            | TyKind::PrimitiveU32
            | TyKind::PrimitiveU64
            | TyKind::PrimitiveUSize
            | TyKind::PrimitiveF32
            | TyKind::PrimitiveF64
            | TyKind::PrimitiveBool => true,
        }
    }

    pub fn is_asm_primitive(&self) -> bool {
        matches!(
            self,
            TyKind::PrimitiveI8
                | TyKind::PrimitiveI16
                | TyKind::PrimitiveI32
                | TyKind::PrimitiveI64
                | TyKind::PrimitiveISize
                | TyKind::PrimitiveU8
                | TyKind::PrimitiveU16
                | TyKind::PrimitiveU32
                | TyKind::PrimitiveU64
                | TyKind::PrimitiveUSize
                | TyKind::PrimitiveF32
                | TyKind::PrimitiveF64
                | TyKind::PrimitiveBool
        )
    }

    pub fn is_primitive(&self) -> bool {
        matches!(
            self,
            TyKind::PrimitiveVoid
                | TyKind::PrimitiveNever
                | TyKind::PrimitiveI8
                | TyKind::PrimitiveI16
                | TyKind::PrimitiveI32
                | TyKind::PrimitiveI64
                | TyKind::PrimitiveISize
                | TyKind::PrimitiveU8
                | TyKind::PrimitiveU16
                | TyKind::PrimitiveU32
                | TyKind::PrimitiveU64
                | TyKind::PrimitiveUSize
                | TyKind::PrimitiveF32
                | TyKind::PrimitiveF64
                | TyKind::PrimitiveStr
                | TyKind::PrimitiveBool
        )
    }

    /// Returns whether the type is an integer or unsigned integer
    pub fn is_int_like(&self) -> bool {
        matches!(
            self,
            Self::PrimitiveU8
                | Self::PrimitiveI8
                | Self::PrimitiveU16
                | Self::PrimitiveI16
                | Self::PrimitiveU32
                | Self::PrimitiveI32
                | Self::PrimitiveU64
                | Self::PrimitiveI64
                | Self::PrimitiveUSize
                | Self::PrimitiveISize
        )
    }

    pub fn is_unsigned(&self) -> bool {
        matches!(
            self,
            Self::PrimitiveU8
                | Self::PrimitiveU16
                | Self::PrimitiveU32
                | Self::PrimitiveU64
                | Self::PrimitiveUSize
        )
    }

    pub fn is_signed(&self) -> bool {
        matches!(
            self,
            Self::PrimitiveI8
                | Self::PrimitiveI16
                | Self::PrimitiveI32
                | Self::PrimitiveI64
                | Self::PrimitiveISize
        )
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Self::PrimitiveF32 | Self::PrimitiveF64)
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
            | TyKind::PrimitiveSelf
            | TyKind::PrimitiveVoid
            | TyKind::PrimitiveBool
            | TyKind::PrimitiveStr
            | TyKind::Ref(_)
            | TyKind::PrimitiveNever => unreachable!(),
            TyKind::PrimitiveU8 | TyKind::PrimitiveI8 => 8,
            TyKind::PrimitiveU16 | TyKind::PrimitiveI16 => 16,
            TyKind::PrimitiveF32 | TyKind::PrimitiveU32 | TyKind::PrimitiveI32 => 32,
            TyKind::PrimitiveF64 | TyKind::PrimitiveU64 | TyKind::PrimitiveI64 => 64,
            TyKind::PrimitiveUSize | TyKind::PrimitiveISize => isize_bitwidth,
        }
    }
}

impl Display for TyKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TyKind::Struct { name, generics, .. } => {
                Display::fmt(name, f)?;
                if !generics.is_empty() {
                    f.write_str("<")?;
                    for (i, ty) in generics.iter().enumerate() {
                        if i != 0 {
                            f.write_str(", ")?;
                        }
                        Display::fmt(ty, f)?;
                    }
                    f.write_str(">")?;
                }
                Ok(())
            }
            TyKind::DynType(trait_refs) => {
                f.write_str("dyn ")?;
                for (i, (_, trait_ref)) in trait_refs.iter().enumerate() {
                    if i != 0 {
                        f.write_str(" + ")?;
                    }
                    Display::fmt(trait_ref, f)?;
                }
                Ok(())
            }
            TyKind::Tuple(elements) => {
                f.write_char('(')?;
                for (i, elem) in elements.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?
                    }
                    Display::fmt(elem, f)?;
                }
                f.write_char(')')
            }
            TyKind::UnsizedArray(ty) => {
                f.write_char('[')?;
                Display::fmt(ty, f)?;
                f.write_char(']')
            }
            TyKind::SizedArray {
                ty,
                number_elements,
                ..
            } => {
                f.write_char('[')?;
                Display::fmt(ty, f)?;
                f.write_str("; ")?;
                Display::fmt(number_elements, f)?;
                f.write_char(']')
            }
            TyKind::Function(contract) => {
                f.write_str("fn")?;
                f.write_char('(')?;
                for (idx, ty) in contract.arguments.iter().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }
                    Display::fmt(ty, f)?;
                }
                f.write_char(')')?;
                if contract.return_type == default_types::void {
                    return Ok(());
                }

                f.write_str(" -> ")?;
                Display::fmt(&contract.return_type, f)
            }
            TyKind::PrimitiveSelf => f.write_str("Self"),
            TyKind::PrimitiveVoid => f.write_str("void"),
            TyKind::PrimitiveI8 => f.write_str("i8"),
            TyKind::PrimitiveI16 => f.write_str("i16"),
            TyKind::PrimitiveI32 => f.write_str("i32"),
            TyKind::PrimitiveI64 => f.write_str("i64"),
            TyKind::PrimitiveU8 => f.write_str("u8"),
            TyKind::PrimitiveU16 => f.write_str("u16"),
            TyKind::PrimitiveU32 => f.write_str("u32"),
            TyKind::PrimitiveU64 => f.write_str("u64"),
            TyKind::PrimitiveF32 => f.write_str("f32"),
            TyKind::PrimitiveF64 => f.write_str("f64"),
            TyKind::PrimitiveBool => f.write_str("bool"),
            TyKind::PrimitiveStr => f.write_str("str"),
            TyKind::PrimitiveISize => f.write_str("isize"),
            TyKind::PrimitiveUSize => f.write_str("usize"),
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
                        Display::fmt(&trait_ref.to_usize(), f)?;
                    }
                }
                f.write_str(" (#")?;
                Display::fmt(generic_id, f)?;
                f.write_char(')')
            }
            TyKind::Ref(c) => {
                f.write_char('&')?;
                Display::fmt(c, f)
            }
        }
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) enum TypeSuggestion<'ctx> {
    Struct(StructId, TyList<'ctx>),
    Array(Box<TypeSuggestion<'ctx>>),
    UnsizedArray(Box<TypeSuggestion<'ctx>>),
    Number(NumberType),
    Tuple(Vec<TypeSuggestion<'ctx>>),
    Bool,
    #[default]
    Unknown,
}

impl Display for TypeSuggestion<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeSuggestion::Struct(id, generics) => {
                f.write_fmt(format_args!("<struct {}", id.to_usize()))?;
                for &generic in **generics {
                    f.write_fmt(format_args!(", {}", generic))?;
                }
                f.write_char('>')
            }
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

impl<'ctx> TypeSuggestion<'ctx> {
    pub(crate) fn to_type(&self, ctx: &TypeckCtx<'ctx>) -> Option<Ty<'ctx>> {
        match self {
            &TypeSuggestion::Struct(struct_id, generics) => {
                Some(ctx.ctx.intern_ty(TyKind::Struct {
                    struct_id,
                    generics,
                    name: ctx.structs.read()[struct_id].name,
                }))
            }
            TypeSuggestion::Array(type_suggestion)
            | TypeSuggestion::UnsizedArray(type_suggestion) => type_suggestion
                .to_type(ctx)
                .map(|ty| TyKind::SizedArray {
                    ty,
                    number_elements: 0,
                })
                .map(|ty| ctx.ctx.intern_ty(ty)),
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
                Some(ctx.ctx.intern_ty(TyKind::Tuple(elements)))
            }
            TypeSuggestion::Unknown => None,
        }
    }

    pub(crate) fn from_type(ty: Ty<'ctx>) -> Self {
        match &**ty {
            TyKind::PrimitiveStr
            | TyKind::PrimitiveSelf
            | TyKind::Generic { .. }
            | TyKind::Function(..)
            | TyKind::PrimitiveVoid
            | TyKind::PrimitiveNever
            | TyKind::DynType { .. } => Self::Unknown,
            &TyKind::Struct {
                struct_id,
                generics,
                ..
            } => Self::Struct(struct_id, generics),
            TyKind::SizedArray { ty, .. } => Self::Array(Box::new(Self::from_type(*ty))),
            TyKind::UnsizedArray(ty) => Self::UnsizedArray(Box::new(Self::from_type(*ty))),
            TyKind::PrimitiveI8 => Self::Number(NumberType::I8),
            TyKind::PrimitiveI16 => Self::Number(NumberType::I16),
            TyKind::PrimitiveI32 => Self::Number(NumberType::I32),
            TyKind::PrimitiveI64 => Self::Number(NumberType::I64),
            TyKind::PrimitiveISize => Self::Number(NumberType::Isize),
            TyKind::PrimitiveU8 => Self::Number(NumberType::U8),
            TyKind::PrimitiveU16 => Self::Number(NumberType::U16),
            TyKind::PrimitiveU32 => Self::Number(NumberType::U32),
            TyKind::PrimitiveU64 => Self::Number(NumberType::U64),
            TyKind::PrimitiveUSize => Self::Number(NumberType::Usize),
            TyKind::PrimitiveF32 => Self::Number(NumberType::F32),
            TyKind::PrimitiveF64 => Self::Number(NumberType::F64),
            TyKind::PrimitiveBool => Self::Bool,
            TyKind::Tuple(elements) => {
                Self::Tuple(elements.iter().copied().map(Self::from_type).collect())
            }
            TyKind::Ref(ty) => Self::from_type(*ty),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn ty_interner_interns() {
        let arena = Arena::new();
        let mut interner = TypeInterner::new(&arena);
        let u8 = interner.intern_owned(TyKind::PrimitiveU8);
        let u16 = interner.intern_owned(TyKind::PrimitiveU16);
        let u8ref = interner.intern_owned(TyKind::Ref(u8));
        assert_eq!(default_types::u8, u8);
        assert_eq!(default_types::u16, u16);
        assert_eq!(default_types::u8_ref, u8ref);
        assert_eq!(default_types::u8.0, u8.0);
        assert_eq!(default_types::u16.0, u16.0);
        assert_eq!(default_types::u8_ref.0, u8ref.0);
    }

    #[test]
    fn ty_list_interner_interns() {
        let arena = Arena::new();
        let mut interner = TypeListInterner::new(&arena);
        let empty = interner.intern([]);
        let one_el1 = interner.intern([default_types::u8]);
        let one_el2 = interner.intern([default_types::u8]);
        assert_eq!(empty, EMPTY_TYLIST);
        assert_eq!(one_el1, one_el2);
    }
}
