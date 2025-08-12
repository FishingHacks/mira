use mira_spans::{Arena, ArenaList, extra_traits, interner, owned_intern};
use std::fmt::{Debug, Display, Write};
use std::hash::Hash;

use crate::context::SharedContext;
use crate::parser::TypeRef;
use crate::store::{Store, StoreKey};
use mira_lexer::NumberType;
use mira_spans::Ident;

use super::{TypecheckingContext, TypedStruct, TypedTrait};

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
    pub static str_ref: Ty<'static> = Ty(&TyKind::Ref(str));
    pub static self_ref: Ty<'static> = Ty(&TyKind::Ref(self_));
    pub static void_ref: Ty<'static> = Ty(&TyKind::Ref(void));
    pub static u8_ref: Ty<'static> = Ty(&TyKind::Ref(u8));

    pub static ALL: &[&Ty<'static>] = &[
        &u8, &u16, &u32, &u64, &usize, &i8, &i16, &i32, &i64, &isize, &bool, &void, &never,
        &str_ref, &self_ref, &void_ref, &self_, &u8_ref,
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
    DynType(ArenaList<'arena, (StoreKey<TypedTrait<'arena>>, Ident<'arena>)>),
    Struct {
        struct_id: StoreKey<TypedStruct<'arena>>,
        name: Ident<'arena>,
    },
    UnsizedArray(Ty<'arena>),
    SizedArray {
        typ: Ty<'arena>,
        number_elements: usize,
    },
    Tuple(TyList<'arena>),
    Function(FunctionType<'arena>),

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

    Ref(Ty<'arena>),

    Generic {
        name: Ident<'arena>,
        generic_id: u8,
        bounds: ArenaList<'arena, StoreKey<TypedTrait<'arena>>>,
        sized: bool,
    },
}

/// wraps the type in the amount of references, adding to the already existing references.
/// e.g.(u8, 6) -> &&&&&&u8
/// e.g. (&&u8, 2) -> &&&&u8
pub fn with_refcount<'arena>(
    ctx: SharedContext<'arena>,
    mut ty: Ty<'arena>,
    mut refcount: u8,
) -> Ty<'arena> {
    while refcount > 0 {
        refcount -= 1;
        ty = ctx.intern_ty(TyKind::Ref(ty));
    }
    ty
}

pub fn resolve_primitive_type<'arena>(
    ctx: SharedContext<'arena>,
    typ: &TypeRef<'arena>,
) -> Option<Ty<'arena>> {
    match typ {
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

    pub fn take_ref(self, ctx: SharedContext<'arena>) -> Self {
        if self == default_types::never {
            self
        } else {
            ctx.intern_ty(TyKind::Ref(self))
        }
    }

    pub fn with_num_refs(mut self, num_refs: u8, ctx: SharedContext<'arena>) -> Self {
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
}
impl<'arena> TyKind<'arena> {
    pub fn implements(
        &self,
        traits: &[StoreKey<TypedTrait<'arena>>],
        tc_ctx: &TypecheckingContext<'arena>,
    ) -> bool {
        match self {
            TyKind::Ref(v) => {
                if let TyKind::DynType(trait_refs) = &***v {
                    values_match(&trait_refs.iter().map(|v| v.0).collect::<Vec<_>>(), traits)
                } else {
                    false
                }
            }
            TyKind::Struct { struct_id, .. } => values_match(
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
        match self {
            TyKind::Ref(_) => ptr_size as u32,
            TyKind::Generic { .. }
            | TyKind::PrimitiveSelf
            | TyKind::PrimitiveStr
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
            TyKind::Tuple(elements) => elements
                .iter()
                .map(|v| v.alignment(ptr_size, structs))
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
        structs: &Store<TypedStruct<'arena>>,
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
            } => {
                let (size, alignment) = typ.size_and_alignment(ptr_size, structs);
                (size * *number_elements as u64, alignment)
            }
            TyKind::Tuple(elements) => {
                let mut size = 0;
                let mut alignment = 1;
                for element in elements.iter() {
                    let (typ_size, typ_alignment) = element.size_and_alignment(ptr_size, structs);
                    alignment = alignment.max(typ_alignment);
                    size = align(size, typ_alignment) + typ_size;
                }
                (align(size, alignment), alignment)
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

    // TODO: deprecate this and replace uses of refcount() > 0 with some method that checks if it
    // has a ref or let TyKind::Ref(ty) = ty.
    pub fn refcount(&self) -> u8 {
        let mut count = 0;
        let mut ty = self;
        while let Self::Ref(t) = ty {
            count += 1;
            ty = &***t;
        }
        count
    }

    pub fn is_sized(&self) -> bool {
        if self.refcount() > 0 {
            return true;
        }
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
            TyKind::Struct { name, .. } => Display::fmt(name, f),
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
            TyKind::UnsizedArray(typ) => {
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
            TyKind::Function(contract) => {
                f.write_str("fn")?;
                f.write_char('(')?;
                for (idx, typ) in contract.arguments.iter().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }
                    Display::fmt(typ, f)?;
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
                        Display::fmt(trait_ref, f)?;
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
            })),
            TypeSuggestion::Array(type_suggestion)
            | TypeSuggestion::UnsizedArray(type_suggestion) => type_suggestion
                .to_type(ctx)
                .map(|typ| TyKind::SizedArray {
                    typ,
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
                Some(ctx.ctx.intern_ty(TyKind::Tuple(elements)))
            }
            TypeSuggestion::Unknown => None,
        }
    }

    pub fn from_type(typ: Ty<'arena>) -> Self {
        match &**typ {
            TyKind::PrimitiveStr
            | TyKind::PrimitiveSelf
            | TyKind::Generic { .. }
            | TyKind::Function(..)
            | TyKind::PrimitiveVoid
            | TyKind::PrimitiveNever
            | TyKind::DynType { .. } => Self::Unknown,
            TyKind::Struct { struct_id, .. } => Self::Struct(*struct_id),
            TyKind::SizedArray { typ, .. } => Self::Array(Box::new(Self::from_type(*typ))),
            TyKind::UnsizedArray(typ) => Self::UnsizedArray(Box::new(Self::from_type(*typ))),
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
}
