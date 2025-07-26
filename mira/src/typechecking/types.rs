use std::hash::Hash;
use std::{
    fmt::{Debug, Display, Write},
    sync::Arc,
};

use crate::store::{Store, StoreKey};
use crate::{interner::InternedStr, parser::TypeRef, tokenizer::NumberType};

use super::{TypecheckingContext, TypedStruct, TypedTrait};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType<'arena> {
    pub arguments: Vec<Type<'arena>>,
    pub return_type: Type<'arena>,
}

#[derive(Clone, Debug, Eq)]
pub enum Type<'arena> {
    DynType {
        trait_refs: Vec<(StoreKey<TypedTrait<'arena>>, InternedStr<'arena>)>,
        num_references: u8,
    },
    Struct {
        struct_id: StoreKey<TypedStruct<'arena>>,
        name: InternedStr<'arena>,
        num_references: u8,
    },
    UnsizedArray {
        typ: Box<Type<'arena>>,
        num_references: u8,
    },
    SizedArray {
        typ: Box<Type<'arena>>,
        num_references: u8,
        number_elements: usize,
    },
    Tuple {
        elements: Vec<Type<'arena>>,
        num_references: u8,
    },
    Function(Arc<FunctionType<'arena>>, u8),

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
        name: InternedStr<'arena>,
        num_references: u8,
        generic_id: u8,
        bounds: Vec<StoreKey<TypedTrait<'arena>>>,
        sized: bool,
    },
}

impl Hash for Type<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.refcount().hash(state);
        match self {
            Type::DynType { trait_refs, .. } => {
                "dyn".hash(state);
                trait_refs.hash(state);
            }
            Type::Struct { struct_id, .. } => struct_id.hash(state),
            Type::UnsizedArray { typ, .. } => {
                "[_]".hash(state);
                typ.hash(state);
            }
            Type::Tuple { elements, .. } => {
                "(".hash(state);
                elements.iter().for_each(|v| v.hash(state));
                ")".hash(state);
            }
            Type::SizedArray {
                typ,
                number_elements,
                ..
            } => {
                "[_;N]".hash(state);
                number_elements.hash(state);
                typ.hash(state);
            }
            Type::Function(arc, _) => {
                arc.arguments.hash(state);
                arc.return_type.hash(state);
            }
            Type::PrimitiveVoid(_) => "void".hash(state),
            Type::PrimitiveNever => "never".hash(state),
            Type::PrimitiveI8(_) => "i8".hash(state),
            Type::PrimitiveI16(_) => "i16".hash(state),
            Type::PrimitiveI32(_) => "i32".hash(state),
            Type::PrimitiveI64(_) => "i64".hash(state),
            Type::PrimitiveISize(_) => "isize".hash(state),
            Type::PrimitiveU8(_) => "u8".hash(state),
            Type::PrimitiveU16(_) => "u16".hash(state),
            Type::PrimitiveU32(_) => "u32".hash(state),
            Type::PrimitiveU64(_) => "u64".hash(state),
            Type::PrimitiveUSize(_) => "usize".hash(state),
            Type::PrimitiveF32(_) => "f32".hash(state),
            Type::PrimitiveF64(_) => "f64".hash(state),
            Type::PrimitiveStr(_) => "str".hash(state),
            Type::PrimitiveBool(_) => "bool".hash(state),
            Type::PrimitiveSelf(_) => "Self".hash(state),
            Type::Generic { name, .. } => name.hash(state),
        }
    }
}

pub fn resolve_primitive_type<'arena>(typ: &TypeRef<'arena>) -> Option<Type<'arena>> {
    match typ {
        TypeRef::Never(_) => Some(Type::PrimitiveNever),
        TypeRef::Void(_, refcount) => Some(Type::PrimitiveVoid(*refcount)),
        TypeRef::Reference {
            num_references,
            type_name,
            span: _,
        } if type_name.entries.len() == 1 && type_name.entries[0].2.is_empty() => {
            match type_name.entries[0].0.to_str() {
                "!" => Some(Type::PrimitiveNever),
                "void" => Some(Type::PrimitiveVoid(*num_references)),
                "i8" => Some(Type::PrimitiveI8(*num_references)),
                "i16" => Some(Type::PrimitiveI16(*num_references)),
                "i32" => Some(Type::PrimitiveI32(*num_references)),
                "i64" => Some(Type::PrimitiveI64(*num_references)),
                "u8" => Some(Type::PrimitiveU8(*num_references)),
                "u16" => Some(Type::PrimitiveU16(*num_references)),
                "u32" => Some(Type::PrimitiveU32(*num_references)),
                "u64" => Some(Type::PrimitiveU64(*num_references)),
                "f32" => Some(Type::PrimitiveF32(*num_references)),
                "f64" => Some(Type::PrimitiveF64(*num_references)),
                "bool" => Some(Type::PrimitiveBool(*num_references)),
                "str" => Some(Type::PrimitiveStr(*num_references)),
                "isize" => Some(Type::PrimitiveISize(*num_references)),
                "usize" => Some(Type::PrimitiveUSize(*num_references)),
                "Self" => Some(Type::PrimitiveSelf(*num_references)),
                _ => None,
            }
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

fn values_match(left: &[StoreKey<TypedTrait>], right: &[StoreKey<TypedTrait>]) -> bool {
    for v in right {
        if !left.contains(v) {
            return false;
        }
    }
    true
}

impl<'arena> Type<'arena> {
    pub fn implements(
        &self,
        traits: &[StoreKey<TypedTrait>],
        tc_ctx: &TypecheckingContext,
    ) -> bool {
        match self {
            Type::DynType {
                trait_refs,
                num_references: 1,
            } => values_match(&trait_refs.iter().map(|v| v.0).collect::<Vec<_>>(), traits),
            Type::Struct {
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
        structs: &Store<TypedStruct>,
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

    pub fn alignment(&self, ptr_size: u64, structs: &Store<TypedStruct>) -> u32 {
        if self.refcount() > 0 {
            return ptr_size as u32;
        }

        match self {
            Type::Generic { .. }
            | Type::PrimitiveSelf(..)
            | Type::PrimitiveStr(..)
            | Type::DynType { .. }
            | Type::UnsizedArray { .. } => {
                unreachable!("generics, self and unsized types don't have an alignment")
            }
            Type::Struct { struct_id, .. } => structs[struct_id]
                .elements
                .iter()
                .map(|v| v.1.alignment(ptr_size, structs))
                .max()
                .unwrap_or(1),
            Type::SizedArray { typ, .. } => typ.alignment(ptr_size, structs),
            Type::Tuple { elements, .. } => elements
                .iter()
                .map(|v| v.alignment(ptr_size, structs))
                .max()
                .unwrap_or(1),
            Type::PrimitiveVoid(_)
            | Type::PrimitiveNever
            | Type::PrimitiveBool(_)
            | Type::PrimitiveU8(_)
            | Type::PrimitiveI8(_) => 1,
            Type::PrimitiveU16(_) | Type::PrimitiveI16(_) => 2,
            Type::PrimitiveF32(_) | Type::PrimitiveU32(_) | Type::PrimitiveI32(_) => 4,
            Type::PrimitiveF64(_) | Type::PrimitiveU64(_) | Type::PrimitiveI64(_) => 8,
            Type::Function(..) | Type::PrimitiveUSize(_) | Type::PrimitiveISize(_) => {
                ptr_size as u32
            }
        }
    }

    pub fn size_and_alignment(&self, ptr_size: u64, structs: &Store<TypedStruct>) -> (u64, u32) {
        if self.refcount() > 0 {
            return if self.is_thin_ptr() {
                (ptr_size, ptr_size as u32)
            } else {
                (ptr_size * 2, ptr_size as u32)
            };
        }

        match self {
            Type::Generic { .. }
            | Type::PrimitiveSelf(..)
            | Type::PrimitiveStr(..)
            | Type::DynType { .. }
            | Type::UnsizedArray { .. } => {
                unreachable!("generics, self and unsized types don't have an alignment")
            }
            Type::Struct { struct_id, .. } => {
                let mut size = 0;
                let mut alignment = 1;
                for (_, element) in structs[struct_id].elements.iter() {
                    let (typ_size, typ_alignment) = element.size_and_alignment(ptr_size, structs);
                    alignment = alignment.max(typ_alignment);
                    size = align(size, typ_alignment) + typ_size;
                }
                (align(size, alignment), alignment)
            }
            Type::SizedArray {
                typ,
                number_elements,
                ..
            } => {
                let (size, alignment) = typ.size_and_alignment(ptr_size, structs);
                (size * *number_elements as u64, alignment)
            }
            Type::Tuple { elements, .. } => {
                let mut size = 0;
                let mut alignment = 1;
                for element in elements {
                    let (typ_size, typ_alignment) = element.size_and_alignment(ptr_size, structs);
                    alignment = alignment.max(typ_alignment);
                    size = align(size, typ_alignment) + typ_size;
                }
                (align(size, alignment), alignment)
            }
            Type::PrimitiveVoid(_)
            | Type::PrimitiveNever
            | Type::PrimitiveBool(_)
            | Type::PrimitiveU8(_)
            | Type::PrimitiveI8(_) => (1, 1),
            Type::PrimitiveU16(_) | Type::PrimitiveI16(_) => (2, 2),
            Type::PrimitiveF32(_) | Type::PrimitiveU32(_) | Type::PrimitiveI32(_) => (4, 4),
            Type::PrimitiveF64(_) | Type::PrimitiveU64(_) | Type::PrimitiveI64(_) => (8, 8),
            Type::Function(..) | Type::PrimitiveUSize(_) | Type::PrimitiveISize(_) => {
                (ptr_size, ptr_size as u32)
            }
        }
    }

    pub fn refcount(&self) -> u8 {
        match self {
            Type::PrimitiveNever => 0,
            Type::Struct { num_references, .. }
            | Type::UnsizedArray { num_references, .. }
            | Type::SizedArray { num_references, .. }
            | Type::DynType { num_references, .. }
            | Type::Tuple { num_references, .. }
            | Type::Generic { num_references, .. }
            | Type::Function(_, num_references)
            | Type::PrimitiveSelf(num_references)
            | Type::PrimitiveVoid(num_references)
            | Type::PrimitiveI8(num_references)
            | Type::PrimitiveI16(num_references)
            | Type::PrimitiveI32(num_references)
            | Type::PrimitiveI64(num_references)
            | Type::PrimitiveISize(num_references)
            | Type::PrimitiveU8(num_references)
            | Type::PrimitiveU16(num_references)
            | Type::PrimitiveU32(num_references)
            | Type::PrimitiveU64(num_references)
            | Type::PrimitiveUSize(num_references)
            | Type::PrimitiveF32(num_references)
            | Type::PrimitiveF64(num_references)
            | Type::PrimitiveStr(num_references)
            | Type::PrimitiveBool(num_references) => *num_references,
        }
    }

    pub fn is_sized(&self) -> bool {
        if self.refcount() > 0 {
            return true;
        }
        match self {
            Type::Generic { sized, .. } => *sized,
            Type::PrimitiveSelf(_) => unreachable!("Self should be resolved"),
            Type::PrimitiveStr(num_references)
            | Type::UnsizedArray { num_references, .. }
            | Type::DynType { num_references, .. } => *num_references > 0,
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
            Type::Generic { sized, .. } => *sized,
            Type::PrimitiveNever => unreachable!("never can never be referenced"),
            Type::PrimitiveSelf(_) => unreachable!("Self should be resolved by now"),
            Type::DynType { .. } | Type::UnsizedArray { .. } | Type::PrimitiveStr(_) => false,
            Type::Struct { .. }
            | Type::Tuple { .. }
            | Type::SizedArray { .. }
            | Type::Function(..)
            | Type::PrimitiveVoid(_)
            | Type::PrimitiveI8(_)
            | Type::PrimitiveI16(_)
            | Type::PrimitiveI32(_)
            | Type::PrimitiveI64(_)
            | Type::PrimitiveISize(_)
            | Type::PrimitiveU8(_)
            | Type::PrimitiveU16(_)
            | Type::PrimitiveU32(_)
            | Type::PrimitiveU64(_)
            | Type::PrimitiveUSize(_)
            | Type::PrimitiveF32(_)
            | Type::PrimitiveF64(_)
            | Type::PrimitiveBool(_) => true,
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
            Type::DynType { num_references, .. }
            | Type::Struct { num_references, .. }
            | Type::UnsizedArray { num_references, .. }
            | Type::SizedArray { num_references, .. }
            | Type::Tuple { num_references, .. }
            | Type::Function(_, num_references)
            | Type::PrimitiveVoid(num_references)
            | Type::PrimitiveI8(num_references)
            | Type::PrimitiveI16(num_references)
            | Type::PrimitiveI32(num_references)
            | Type::PrimitiveI64(num_references)
            | Type::PrimitiveISize(num_references)
            | Type::PrimitiveU8(num_references)
            | Type::PrimitiveU16(num_references)
            | Type::PrimitiveU32(num_references)
            | Type::PrimitiveU64(num_references)
            | Type::PrimitiveUSize(num_references)
            | Type::PrimitiveF32(num_references)
            | Type::PrimitiveF64(num_references)
            | Type::PrimitiveStr(num_references)
            | Type::PrimitiveBool(num_references)
            | Type::PrimitiveSelf(num_references)
            | Type::Generic { num_references, .. } => *num_references += 1,
            Type::PrimitiveNever => {}
        }
        self
    }

    pub fn deref(mut self) -> Result<Self, Self> {
        match &mut self {
            Type::DynType { num_references, .. }
            | Type::Struct { num_references, .. }
            | Type::UnsizedArray { num_references, .. }
            | Type::SizedArray { num_references, .. }
            | Type::Tuple { num_references, .. }
            | Type::Function(_, num_references)
            | Type::PrimitiveVoid(num_references)
            | Type::PrimitiveI8(num_references)
            | Type::PrimitiveI16(num_references)
            | Type::PrimitiveI32(num_references)
            | Type::PrimitiveI64(num_references)
            | Type::PrimitiveISize(num_references)
            | Type::PrimitiveU8(num_references)
            | Type::PrimitiveU16(num_references)
            | Type::PrimitiveU32(num_references)
            | Type::PrimitiveU64(num_references)
            | Type::PrimitiveUSize(num_references)
            | Type::PrimitiveF32(num_references)
            | Type::PrimitiveF64(num_references)
            | Type::PrimitiveStr(num_references)
            | Type::PrimitiveBool(num_references)
            | Type::PrimitiveSelf(num_references)
            | Type::Generic { num_references, .. } => {
                if *num_references == 0 {
                    Err(self)
                } else {
                    *num_references -= 1;
                    Ok(self)
                }
            }
            Type::PrimitiveNever => Ok(self),
        }
    }

    pub fn without_ref(mut self) -> Self {
        match &mut self {
            Type::DynType { num_references, .. }
            | Type::Struct { num_references, .. }
            | Type::UnsizedArray { num_references, .. }
            | Type::SizedArray { num_references, .. }
            | Type::Tuple { num_references, .. }
            | Type::Function(_, num_references)
            | Type::PrimitiveVoid(num_references)
            | Type::PrimitiveI8(num_references)
            | Type::PrimitiveI16(num_references)
            | Type::PrimitiveI32(num_references)
            | Type::PrimitiveI64(num_references)
            | Type::PrimitiveISize(num_references)
            | Type::PrimitiveU8(num_references)
            | Type::PrimitiveU16(num_references)
            | Type::PrimitiveU32(num_references)
            | Type::PrimitiveU64(num_references)
            | Type::PrimitiveUSize(num_references)
            | Type::PrimitiveF32(num_references)
            | Type::PrimitiveF64(num_references)
            | Type::PrimitiveStr(num_references)
            | Type::PrimitiveBool(num_references)
            | Type::PrimitiveSelf(num_references)
            | Type::Generic { num_references, .. } => {
                *num_references = 0;
                self
            }
            Type::PrimitiveNever => self,
        }
    }

    pub fn with_num_refs(mut self, num_refs: u8) -> Self {
        match &mut self {
            Type::DynType { num_references, .. }
            | Type::Struct { num_references, .. }
            | Type::UnsizedArray { num_references, .. }
            | Type::SizedArray { num_references, .. }
            | Type::Tuple { num_references, .. }
            | Type::Function(_, num_references)
            | Type::PrimitiveVoid(num_references)
            | Type::PrimitiveI8(num_references)
            | Type::PrimitiveI16(num_references)
            | Type::PrimitiveI32(num_references)
            | Type::PrimitiveI64(num_references)
            | Type::PrimitiveISize(num_references)
            | Type::PrimitiveU8(num_references)
            | Type::PrimitiveU16(num_references)
            | Type::PrimitiveU32(num_references)
            | Type::PrimitiveU64(num_references)
            | Type::PrimitiveUSize(num_references)
            | Type::PrimitiveF32(num_references)
            | Type::PrimitiveF64(num_references)
            | Type::PrimitiveStr(num_references)
            | Type::PrimitiveBool(num_references)
            | Type::PrimitiveSelf(num_references)
            | Type::Generic { num_references, .. } => {
                *num_references = num_refs;
                self
            }
            Type::PrimitiveNever => self,
        }
    }

    pub fn is_asm_primitive(&self) -> bool {
        matches!(
            self,
            Type::PrimitiveI8(0)
                | Type::PrimitiveI16(0)
                | Type::PrimitiveI32(0)
                | Type::PrimitiveI64(0)
                | Type::PrimitiveISize(0)
                | Type::PrimitiveU8(0)
                | Type::PrimitiveU16(0)
                | Type::PrimitiveU32(0)
                | Type::PrimitiveU64(0)
                | Type::PrimitiveUSize(0)
                | Type::PrimitiveF32(0)
                | Type::PrimitiveF64(0)
                | Type::PrimitiveBool(0)
        )
    }

    pub fn is_primitive(&self) -> bool {
        matches!(
            self,
            Type::PrimitiveVoid(_)
                | Type::PrimitiveNever
                | Type::PrimitiveI8(_)
                | Type::PrimitiveI16(_)
                | Type::PrimitiveI32(_)
                | Type::PrimitiveI64(_)
                | Type::PrimitiveISize(_)
                | Type::PrimitiveU8(_)
                | Type::PrimitiveU16(_)
                | Type::PrimitiveU32(_)
                | Type::PrimitiveU64(_)
                | Type::PrimitiveUSize(_)
                | Type::PrimitiveF32(_)
                | Type::PrimitiveF64(_)
                | Type::PrimitiveStr(_)
                | Type::PrimitiveBool(_)
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
            Type::DynType { .. }
            | Type::Struct { .. }
            | Type::UnsizedArray { .. }
            | Type::SizedArray { .. }
            | Type::Tuple { .. }
            | Type::Generic { .. }
            | Type::Function(..)
            | Type::PrimitiveSelf(_)
            | Type::PrimitiveVoid(_)
            | Type::PrimitiveBool(_)
            | Type::PrimitiveStr(_)
            | Type::PrimitiveNever => 0,
            Type::PrimitiveU8(_) | Type::PrimitiveI8(_) => 8,
            Type::PrimitiveU16(_) | Type::PrimitiveI16(_) => 16,
            Type::PrimitiveF32(_) | Type::PrimitiveU32(_) | Type::PrimitiveI32(_) => 32,
            Type::PrimitiveF64(_) | Type::PrimitiveU64(_) | Type::PrimitiveI64(_) => 64,
            Type::PrimitiveUSize(_) | Type::PrimitiveISize(_) => isize_bitwidth,
        }
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Type::PrimitiveBool(0))
    }
}

impl Display for Type<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.refcount() {
            f.write_char('&')?;
        }

        match self {
            Type::Struct { name, .. } => Display::fmt(name, f),
            Type::DynType { trait_refs, .. } => {
                f.write_str("dyn ")?;
                for (i, (_, trait_ref)) in trait_refs.iter().enumerate() {
                    if i != 0 {
                        f.write_str(" + ")?;
                    }
                    Display::fmt(trait_ref, f)?;
                }
                Ok(())
            }
            Type::Tuple { elements, .. } => {
                f.write_char('(')?;
                for (i, elem) in elements.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?
                    }
                    Display::fmt(elem, f)?;
                }
                f.write_char(')')
            }
            Type::UnsizedArray { typ, .. } => {
                f.write_char('[')?;
                Display::fmt(typ, f)?;
                f.write_char(']')
            }
            Type::SizedArray {
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
            Type::Function(contract, _) => {
                f.write_str("fn")?;
                f.write_char('(')?;
                for (idx, typ) in contract.arguments.iter().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }
                    Display::fmt(typ, f)?;
                }
                f.write_char(')')?;
                if matches!(contract.return_type, Type::PrimitiveVoid(0)) {
                    return Ok(());
                }

                f.write_str(" -> ")?;
                Display::fmt(&contract.return_type, f)
            }
            Type::PrimitiveSelf(_) => f.write_str("Self"),
            Type::PrimitiveVoid(_) => f.write_str("void"),
            Type::PrimitiveI8(_) => f.write_str("i8"),
            Type::PrimitiveI16(_) => f.write_str("i16"),
            Type::PrimitiveI32(_) => f.write_str("i32"),
            Type::PrimitiveI64(_) => f.write_str("i64"),
            Type::PrimitiveU8(_) => f.write_str("u8"),
            Type::PrimitiveU16(_) => f.write_str("u16"),
            Type::PrimitiveU32(_) => f.write_str("u32"),
            Type::PrimitiveU64(_) => f.write_str("u64"),
            Type::PrimitiveF32(_) => f.write_str("f32"),
            Type::PrimitiveF64(_) => f.write_str("f64"),
            Type::PrimitiveBool(_) => f.write_str("bool"),
            Type::PrimitiveStr(_) => f.write_str("str"),
            Type::PrimitiveISize(_) => f.write_str("isize"),
            Type::PrimitiveUSize(_) => f.write_str("usize"),
            Type::PrimitiveNever => f.write_str("!"),
            Type::Generic {
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

impl PartialEq for Type<'_> {
    fn eq(&self, other: &Self) -> bool {
        if self.refcount() != other.refcount() {
            return false;
        }

        match (self, other) {
            (
                Type::DynType {
                    trait_refs: self_traits,
                    ..
                },
                Type::DynType {
                    trait_refs: other_traits,
                    ..
                },
            ) => {
                self_traits.len() == other_traits.len()
                    && self_traits
                        .iter()
                        .map(|v| v.0)
                        .zip(other_traits.iter().map(|v| v.0))
                        .all(|b| b.0 == b.1)
            }
            (
                Type::Struct {
                    struct_id: structure,
                    ..
                },
                Type::Struct {
                    struct_id: other, ..
                },
            ) => *structure == *other,
            (Type::UnsizedArray { typ, .. }, Type::UnsizedArray { typ: other, .. }) => typ == other,
            (
                Type::SizedArray {
                    typ,
                    number_elements,
                    ..
                },
                Type::SizedArray {
                    typ: other_typ,
                    number_elements: other_number_elements,
                    ..
                },
            ) => number_elements == other_number_elements && typ == other_typ,
            (Type::PrimitiveVoid(_), Type::PrimitiveVoid(_)) => true,
            (Type::PrimitiveNever, Type::PrimitiveNever) => true,
            (Type::PrimitiveI8(_), Type::PrimitiveI8(_)) => true,
            (Type::PrimitiveI16(_), Type::PrimitiveI16(_)) => true,
            (Type::PrimitiveI32(_), Type::PrimitiveI32(_)) => true,
            (Type::PrimitiveI64(_), Type::PrimitiveI64(_)) => true,
            (Type::PrimitiveISize(_), Type::PrimitiveISize(_)) => true,
            (Type::PrimitiveU8(_), Type::PrimitiveU8(_)) => true,
            (Type::PrimitiveU16(_), Type::PrimitiveU16(_)) => true,
            (Type::PrimitiveU32(_), Type::PrimitiveU32(_)) => true,
            (Type::PrimitiveU64(_), Type::PrimitiveU64(_)) => true,
            (Type::PrimitiveUSize(_), Type::PrimitiveUSize(_)) => true,
            (Type::PrimitiveF32(_), Type::PrimitiveF32(_)) => true,
            (Type::PrimitiveF64(_), Type::PrimitiveF64(_)) => true,
            (Type::PrimitiveStr(_), Type::PrimitiveStr(_)) => true,
            (Type::PrimitiveBool(_), Type::PrimitiveBool(_)) => true,
            (Type::PrimitiveSelf(_), Type::PrimitiveSelf(_)) => true,
            (
                Type::Generic { generic_id, .. },
                Type::Generic {
                    generic_id: other_id,
                    ..
                },
            ) => generic_id == other_id,
            (Type::Function(id, _), Type::Function(other_id, _)) => id == other_id,
            (
                Type::Tuple { elements, .. },
                Type::Tuple {
                    elements: other_elements,
                    ..
                },
            ) => *elements == *other_elements,
            _ => false,
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
    pub fn to_type(&self, ctx: &TypecheckingContext<'arena>) -> Option<Type<'arena>> {
        match self {
            TypeSuggestion::Struct(id) => Some(Type::Struct {
                struct_id: *id,
                name: ctx.structs.read()[*id].name,
                num_references: 0,
            }),
            TypeSuggestion::Array(type_suggestion)
            | TypeSuggestion::UnsizedArray(type_suggestion) => type_suggestion
                .to_type(ctx)
                .map(Box::new)
                .map(|typ| Type::SizedArray {
                    typ,
                    num_references: 0,
                    number_elements: 0,
                }),
            TypeSuggestion::Number(number_type) => Some(match number_type {
                NumberType::F32 => Type::PrimitiveF32(0),
                NumberType::F64 => Type::PrimitiveF64(0),
                NumberType::I8 => Type::PrimitiveI8(0),
                NumberType::I16 => Type::PrimitiveI16(0),
                NumberType::None | NumberType::I32 => Type::PrimitiveI32(0),
                NumberType::I64 => Type::PrimitiveI64(0),
                NumberType::Isize => Type::PrimitiveISize(0),
                NumberType::U8 => Type::PrimitiveU8(0),
                NumberType::U16 => Type::PrimitiveU16(0),
                NumberType::U32 => Type::PrimitiveU32(0),
                NumberType::U64 => Type::PrimitiveU64(0),
                NumberType::Usize => Type::PrimitiveUSize(0),
            }),
            TypeSuggestion::Bool => Some(Type::PrimitiveBool(0)),
            TypeSuggestion::Tuple(elems) => {
                let mut elements = Vec::with_capacity(elems.len());
                for elem in elems {
                    elements.push(elem.to_type(ctx)?);
                }
                Some(Type::Tuple {
                    elements,
                    num_references: 0,
                })
            }
            TypeSuggestion::Unknown => None,
        }
    }

    pub fn from_type(typ: &Type<'arena>) -> Self {
        match typ {
            Type::PrimitiveStr(_)
            | Type::PrimitiveSelf(_)
            | Type::Generic { .. }
            | Type::Function(..)
            | Type::PrimitiveVoid(_)
            | Type::PrimitiveNever
            | Type::DynType { .. } => Self::Unknown,
            Type::Struct { struct_id, .. } => Self::Struct(*struct_id),
            Type::SizedArray { typ, .. } => Self::Array(Box::new(Self::from_type(typ))),
            Type::UnsizedArray { typ, .. } => Self::UnsizedArray(Box::new(Self::from_type(typ))),
            Type::PrimitiveI8(_) => Self::Number(NumberType::I8),
            Type::PrimitiveI16(_) => Self::Number(NumberType::I16),
            Type::PrimitiveI32(_) => Self::Number(NumberType::I32),
            Type::PrimitiveI64(_) => Self::Number(NumberType::I64),
            Type::PrimitiveISize(_) => Self::Number(NumberType::Isize),
            Type::PrimitiveU8(_) => Self::Number(NumberType::U8),
            Type::PrimitiveU16(_) => Self::Number(NumberType::U16),
            Type::PrimitiveU32(_) => Self::Number(NumberType::U32),
            Type::PrimitiveU64(_) => Self::Number(NumberType::U64),
            Type::PrimitiveUSize(_) => Self::Number(NumberType::Usize),
            Type::PrimitiveF32(_) => Self::Number(NumberType::F32),
            Type::PrimitiveF64(_) => Self::Number(NumberType::F64),
            Type::PrimitiveBool(_) => Self::Bool,
            Type::Tuple { elements, .. } => {
                Self::Tuple(elements.iter().map(Self::from_type).collect())
            }
        }
    }
}
