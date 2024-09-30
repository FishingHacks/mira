use crate::{
    module::Module,
    parser::{Annotations, Implementation, TypeRef},
};
use std::{
    collections::HashMap,
    fmt::{Debug, Display, Write},
    rc::Rc,
};

use super::error::ProgrammingLangTypecheckingError;

#[derive(Debug)]
pub struct TypecheckedModule {
    pub structs: HashMap<Rc<str>, Rc<ResolvedStruct>>,
}

#[derive(Debug)]
pub enum Type {
    Struct {
        structure: Rc<ResolvedStruct>,
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
}

fn put_refcount(refcount: u8, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    for _ in 0..refcount {
        f.write_char('&')?;
    }
    Ok(())
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        put_refcount(self.get_ref_count(), f)?;

        match self {
            Self::PrimitiveBool(_) => f.write_str("bool"),
            Self::PrimitiveI8(_) => f.write_str("i8"),
            Self::PrimitiveI16(_) => f.write_str("i16"),
            Self::PrimitiveI32(_) => f.write_str("i32"),
            Self::PrimitiveI64(_) => f.write_str("i64"),
            Self::PrimitiveU8(_) => f.write_str("u8"),
            Self::PrimitiveU16(_) => f.write_str("u16"),
            Self::PrimitiveU32(_) => f.write_str("u32"),
            Self::PrimitiveU64(_) => f.write_str("u64"),
            Self::PrimitiveISize(_) => f.write_str("isize"),
            Self::PrimitiveUSize(_) => f.write_str("usize"),
            Self::PrimitiveStr(_) => f.write_str("str"),
            Self::PrimitiveNever => f.write_str("!"),
            Self::PrimitiveVoid(_) => f.write_str("void"),
            Self::PrimitiveF32(_) => f.write_str("f32"),
            Self::PrimitiveF64(_) => f.write_str("f64"),
            Self::SizedArray {
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
            Self::UnsizedArray { typ, .. } => {
                f.write_char('[')?;
                Display::fmt(typ, f)?;
                f.write_char(']')
            }
            Self::Struct { structure, .. } => {
                f.write_str("struct ")?;
                f.write_str(&*structure.name)?;
                f.write_str(" {")?;
                for (index, (field_name, field_type)) in structure.fields.iter().enumerate() {
                    f.write_char(' ')?;
                    f.write_str(&**field_name)?;
                    f.write_str(": ")?;
                    Display::fmt(&field_type, f)?;
                    if index < structure.fields.len() - 1 {
                        f.write_char(',')?;
                    } else {
                        f.write_char(' ')?;
                    }
                }
                f.write_char('}')
            }
        }
    }
}

const PTR_SIZE: usize = 8;

impl Type {
    /// Returns the size of the current type in bytes.
    /// `None` means the type is unsized.
    pub fn get_size(&self) -> Option<usize> {
        if self.get_ref_count() > 0 {
            if self.is_fat_pointer() {
                return Some(PTR_SIZE * 2);
            }
            return Some(PTR_SIZE);
        }

        match self {
            Self::UnsizedArray { .. } | Self::PrimitiveStr(_) => None,
            Self::SizedArray {
                typ,
                number_elements,
                ..
            } => Some(typ.get_size()? * number_elements),
            Self::PrimitiveNever | Self::PrimitiveVoid(_) => Some(0),
            Self::PrimitiveU8(_) | Self::PrimitiveI8(_) | Self::PrimitiveBool(_) => Some(1),
            Self::PrimitiveU16(_) | Self::PrimitiveI16(_) => Some(2),
            Self::PrimitiveU32(_) | Self::PrimitiveI32(_) | Self::PrimitiveF32(_) => Some(4),
            Self::PrimitiveU64(_) | Self::PrimitiveI64(_) | Self::PrimitiveF64(_) => Some(8),
            Self::PrimitiveUSize(_) | Self::PrimitiveISize(_) => Some(PTR_SIZE),
            Self::Struct { structure, .. } => get_struct_size(structure),
        }
    }

    /// Returns the amount of references that this current type is deep. (iex `&&&u8` would return 3, while `u8` would return 0)
    pub fn get_ref_count(&self) -> u8 {
        match self {
            Self::UnsizedArray { num_references, .. }
            | Self::SizedArray { num_references, .. }
            | Self::Struct { num_references, .. }
            | Self::PrimitiveVoid(num_references)
            | Self::PrimitiveI8(num_references)
            | Self::PrimitiveI16(num_references)
            | Self::PrimitiveI32(num_references)
            | Self::PrimitiveI64(num_references)
            | Self::PrimitiveISize(num_references)
            | Self::PrimitiveU8(num_references)
            | Self::PrimitiveU16(num_references)
            | Self::PrimitiveU32(num_references)
            | Self::PrimitiveU64(num_references)
            | Self::PrimitiveUSize(num_references)
            | Self::PrimitiveF64(num_references)
            | Self::PrimitiveF32(num_references)
            | Self::PrimitiveStr(num_references)
            | Self::PrimitiveBool(num_references) => *num_references,
            Self::PrimitiveNever => 0,
        }
    }

    /// Returns if the type is a fat pointer (&T + usize). This could be for example a length (in the case of &str or &[T]) or a vtable (in the case of &Trait).
    pub fn is_fat_pointer(&self) -> bool {
        match self.get_ref_count() {
            // &str, &[T] and structs with unsized fields are fat pointers
            // later, also traits will be unsized; however, traits aren't a thing yet
            1 => match self {
                Self::UnsizedArray { .. } | Self::PrimitiveStr(_) => true,
                Self::Struct { structure, .. } => get_struct_size(structure).is_none(),
                _ => false,
            },
            // In the case of a type without a reference, it will never be a fat pointer (because its not a pointer)
            // In the case of 2+ references, it will also never be a fat pointer, because its always gonna point to either a thin or a fat pointer (&&[u8] is a thin pointer to a fat pointer)
            _ => false,
        }
    }

    /// Returns if this type denotes that the function will never return
    pub fn is_never(&self) -> bool {
        matches!(self, Type::PrimitiveNever)
    }
}

fn get_struct_size(structure: &ResolvedStruct) -> Option<usize> {
    let mut size = 0;
    for (_, typ) in structure.fields.iter() {
        size += typ.get_size()?;
    }
    Some(size)
}

#[derive(Debug)]
pub struct ResolvedStruct {
    pub name: Rc<str>,
    pub fields: Vec<(Rc<str>, Type)>,
    pub global_impl: Implementation,
    pub trait_impls: Vec<(Rc<str>, Implementation)>,
    pub annotations: Annotations,
}

pub fn resolve_type(
    typ: &TypeRef,
    module: &mut Module,
    typechecked_module: &mut TypecheckedModule,
) -> Result<Type, ProgrammingLangTypecheckingError> {
    if let Some(typ) = resolve_primitive_type(typ) {
        return Ok(typ);
    }

    match typ {
        TypeRef::Never(_) | TypeRef::Void(_) => unreachable!(), // should've been handled in resolve_type
        TypeRef::SizedArray {
            num_references,
            child,
            number_elements,
            loc: _,
        } => Ok(Type::SizedArray {
            typ: Box::new(resolve_type(&**child, module, typechecked_module)?),
            num_references: *num_references,
            number_elements: *number_elements,
        }),
        TypeRef::UnsizedArray {
            number_of_references,
            child,
            loc: _,
        } => Ok(Type::UnsizedArray {
            typ: Box::new(resolve_type(&**child, module, typechecked_module)?),
            num_references: *number_of_references,
        }),
        TypeRef::Reference {
            number_of_references,
            type_name,
            loc,
        } => {
            if let Some(structure) = typechecked_module.structs.get(type_name).cloned() {
                return Ok(Type::Struct {
                    structure,
                    num_references: *number_of_references,
                });
            }

            let structure = module
                .global_scope
                .structs
                .remove(type_name)
                .ok_or_else(|| ProgrammingLangTypecheckingError::UnboundType {
                    loc: loc.clone(),
                    type_name: type_name.clone(),
                })?;
            let mut resolved_struct = ResolvedStruct {
                name: structure.name.clone(),
                annotations: structure.annotations,
                global_impl: structure.global_impl,
                trait_impls: structure.trait_impls,
                fields: Vec::with_capacity(structure.fields.len()),
            };

            for (index, (field_name, field_type)) in structure.fields.iter().enumerate() {
                let resolved_type = resolve_type(field_type, module, typechecked_module)?;
                if resolved_type.get_size().is_none() && index < structure.fields.len() - 1 {
                    return Err(ProgrammingLangTypecheckingError::UnsizedTypeInsideStruct {
                        loc: field_type.loc().clone(),
                        field_name: field_name.clone(),
                    });
                }
                resolved_struct
                    .fields
                    .push((field_name.clone(), resolved_type));
            }

            let rc_struct = Rc::new(resolved_struct);
            typechecked_module
                .structs
                .insert(structure.name, rc_struct.clone());
            Ok(Type::Struct {
                structure: rc_struct,
                num_references: *number_of_references,
            })
        }
    }
}

fn resolve_primitive_type(typ: &TypeRef) -> Option<Type> {
    match typ {
        TypeRef::Never(_) => Some(Type::PrimitiveNever),
        TypeRef::Void(_) => Some(Type::PrimitiveVoid(0)),
        TypeRef::Reference {
            number_of_references,
            type_name,
            loc: _,
        } => match &**type_name {
            "!" => Some(Type::PrimitiveNever),
            "void" => Some(Type::PrimitiveVoid(*number_of_references)),
            "i8" => Some(Type::PrimitiveI8(*number_of_references)),
            "i16" => Some(Type::PrimitiveI16(*number_of_references)),
            "i32" => Some(Type::PrimitiveI32(*number_of_references)),
            "i64" => Some(Type::PrimitiveI64(*number_of_references)),
            "u8" => Some(Type::PrimitiveU8(*number_of_references)),
            "u16" => Some(Type::PrimitiveU16(*number_of_references)),
            "u32" => Some(Type::PrimitiveU32(*number_of_references)),
            "u64" => Some(Type::PrimitiveU64(*number_of_references)),
            "f32" => Some(Type::PrimitiveF32(*number_of_references)),
            "f64" => Some(Type::PrimitiveF64(*number_of_references)),
            "bool" => Some(Type::PrimitiveBool(*number_of_references)),
            "str" => Some(Type::PrimitiveStr(*number_of_references)),
            "isize" => Some(Type::PrimitiveISize(*number_of_references)),
            "usize" => Some(Type::PrimitiveUSize(*number_of_references)),
            _ => None,
        },
        TypeRef::SizedArray { .. } | TypeRef::UnsizedArray { .. } => None,
    }
}
