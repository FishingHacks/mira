use std::{
    fmt::{Debug, Display, Write},
    sync::Arc,
};

use crate::{
    globals::GlobalStr,
    module::{StructId, TraitId},
    parser::TypeRef,
    tokenizer::NumberType,
};

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionType {
    pub arguments: Vec<Type>,
    pub return_type: Type,
}

#[derive(Clone, Debug)]
pub enum Type {
    Trait {
        trait_refs: Vec<TraitId>,
        num_references: u8,
        real_name: GlobalStr,
    },
    DynType {
        trait_refs: Vec<(TraitId, GlobalStr)>,
        num_references: u8,
    },
    Struct {
        struct_id: StructId,
        name: GlobalStr,
        num_references: u8,
    },
    UnsizedArray {
        typ: Box<Type>,
        num_references: u8,
    },
    SizedArray {
        typ: Box<Type>,
        num_references: u8,
        number_elements: usize,
    },
    Function(Arc<FunctionType>, u8),

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

    Generic(GlobalStr, u8),
}

pub fn resolve_primitive_type(typ: &TypeRef) -> Option<Type> {
    match typ {
        TypeRef::Never(_) => Some(Type::PrimitiveNever),
        TypeRef::Void(_, refcount) => Some(Type::PrimitiveVoid(*refcount)),
        TypeRef::Reference {
            num_references,
            type_name,
            loc: _,
        } if type_name.entries.len() == 1 && type_name.entries[0].1.len() == 0 => {
            type_name.entries[0].0.with(|type_name| match type_name {
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
            })
        }
        TypeRef::Reference { .. }
        | TypeRef::DynReference { .. }
        | TypeRef::SizedArray { .. }
        | TypeRef::UnsizedArray { .. } => None,
    }
}

impl Type {
    pub fn refcount(&self) -> u8 {
        match self {
            Type::PrimitiveNever => 0,
            Type::Struct { num_references, .. }
            | Type::UnsizedArray { num_references, .. }
            | Type::SizedArray { num_references, .. }
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
            | Type::PrimitiveBool(num_references)
            | Type::DynType { num_references, .. }
            | Type::Trait { num_references, .. }
            | Type::Generic(_, num_references) => *num_references,
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
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.refcount() {
            f.write_char('&')?;
        }

        match self {
            Type::Struct { name, .. } => Display::fmt(name, f),
            Type::Trait { real_name, .. } => Display::fmt(real_name, f),
            Type::DynType { trait_refs, .. } => {
                f.write_str("dyn ")?;
                for i in 0..trait_refs.len() {
                    if i != 0 {
                        f.write_str(" + ")?;
                    }
                    Display::fmt(&trait_refs[i].1, f)?;
                }
                Ok(())
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
            Type::Generic(global_str, _) => Display::fmt(global_str, f),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        if self.refcount() != other.refcount() {
            return false;
        }

        match (self, other) {
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
            (Type::PrimitiveNever, Type::PrimitiveNever) => todo!(),
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
            (Type::Generic(name, _), Type::Generic(other_name, _)) => name == other_name,
            (Type::Function(id, _), Type::Function(other_id, _)) => id == other_id,
            _ => false,
        }
    }
}
