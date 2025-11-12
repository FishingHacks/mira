use inkwell::{
    AddressSpace,
    attributes::{Attribute, AttributeLoc},
    context::Context,
    types::{AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::FunctionValue,
};
use mira_common::index::IndexMap;
use mira_parser::module::StructId;
use mira_typeck::{TyKind, TypeCtx, TypedStruct};

use crate::DefaultTypes;

pub(crate) fn has_special_encoding(ty: &TyKind<'_>) -> bool {
    match ty {
        // aggregate values always have special encodings (e.g. [u8; 4] is classified as an i32.)
        TyKind::Struct { .. }
        | TyKind::SizedArray { .. }
        | TyKind::Tuple(_) => true,

        // Unsized types are fat pointers, and as such have a special encoding.
        TyKind::Ref(ty) => !ty.is_sized(),
        // fn(_) -> _ is just a pointer
        TyKind::Function(_)
        // void isn't stored
        | TyKind::PrimitiveVoid
        | TyKind::PrimitiveNever
        // Primitives can't have special encodings.
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
        | TyKind::PrimitiveBool => false,

        TyKind::PrimitiveStr | TyKind::UnsizedArray(_) | TyKind::DynType(_) => {
            unreachable!("unsized types")
        }
        TyKind::PrimitiveSelf => unreachable!("Self should be resolved"),
        TyKind::Generic { .. } => unreachable!("generics should be resolved"),
    }
}

#[derive(Clone, Copy)]
pub(crate) enum ArgumentType<'ctx> {
    Regular(BasicTypeEnum<'ctx>),
    ByVal(BasicTypeEnum<'ctx>),
    SRet(BasicTypeEnum<'ctx>),
    /// This argument type doesn't 'exist'. (it's a zst, like void, !, [_; 0], (), or an empty
    /// struct)
    None,
}

impl<'ctx> ArgumentType<'ctx> {
    pub(crate) fn as_arg_ty(
        &self,
        default_types: &DefaultTypes<'ctx>,
    ) -> Option<BasicTypeEnum<'ctx>> {
        match self {
            &ArgumentType::Regular(v) => Some(v),
            ArgumentType::SRet(_) | ArgumentType::ByVal(_) => Some(default_types.ptr.into()),
            ArgumentType::None => None,
        }
    }

    pub(crate) fn add_ret_attribute_args(
        ret_args: &[Self],
        func: FunctionValue<'ctx>,
        ctx: &'ctx Context,
    ) {
        let mut id = 0;
        if let Self::SRet(ty) = ret_args[0] {
            let kind_id = Attribute::get_named_enum_kind_id("sret");
            let attr = ctx.create_type_attribute(kind_id, ty.as_any_type_enum());
            func.add_attribute(AttributeLoc::Param(id), attr);

            id += 1;
        }

        for arg in &ret_args[1..] {
            match arg {
                ArgumentType::None => {}
                ArgumentType::Regular(_) => id += 1,
                &ArgumentType::ByVal(ty) => {
                    let kind_id = Attribute::get_named_enum_kind_id("byval");
                    let attr = ctx.create_type_attribute(kind_id, ty.as_any_type_enum());
                    func.add_attribute(AttributeLoc::Param(id), attr);
                    id += 1;
                }
                ArgumentType::SRet(_) => unreachable!("sret(_) is not valid for arguments."),
            }
        }
    }

    pub(crate) fn func_ty(
        &self,
        mut args: Vec<BasicMetadataTypeEnum<'ctx>>,
        var_args: bool,
        default_types: &DefaultTypes<'ctx>,
        ctx: &'ctx Context,
    ) -> FunctionType<'ctx> {
        match self {
            ArgumentType::Regular(ty) => ty.fn_type(&args, var_args),
            ArgumentType::ByVal(_) => unreachable!("byval in return type"),
            ArgumentType::SRet(_) => {
                args.insert(0, default_types.ptr.into());
                ctx.void_type().fn_type(&args, var_args)
            }
            ArgumentType::None => ctx.void_type().fn_type(&args, var_args),
        }
    }
}

enum Class {
    Int(u8),
    Float(u8),
}

impl Class {
    pub(self) fn basic_ty<'ctx>(self, ctx: &'ctx Context) -> BasicTypeEnum<'ctx> {
        match self {
            Class::Int(bytes) => ctx
                .custom_width_int_type(bytes as u32 * 8)
                .as_basic_type_enum(),
            Class::Float(4) => ctx.f32_type().as_basic_type_enum(),
            Class::Float(8) => ctx.f64_type().as_basic_type_enum(),
            Class::Float(bytes) => unreachable!("illegal float size: {bytes} bytes"),
        }
    }
}

pub(crate) fn argument<'ctx, 'tycx>(
    ctx: &'ctx Context,
    ty: &TyKind<'tycx>,
    ptrsize: u8,
    structs: &IndexMap<StructId, TypedStruct<'tycx>>,
    make_ty: impl FnOnce(&TyKind<'tycx>) -> BasicTypeEnum<'ctx>,
    ty_cx: TypeCtx<'tycx>,
) -> ArgumentType<'ctx> {
    let (size, align) = ty.size_and_alignment(ptrsize as u64, structs, ty_cx);

    if size == 0 {
        ArgumentType::None
    } else if size <= 8 && matches!(ty, TyKind::Ref(_)) {
        ArgumentType::Regular(ctx.ptr_type(AddressSpace::default()).as_basic_type_enum())
    } else if size <= 8 {
        let int = ctx.custom_width_int_type(size as u32 * 8);
        ArgumentType::Regular(int.as_basic_type_enum())
    } else if size <= 16 {
        let mut classes = Vec::new();
        classify_ty(ty, &mut classes, ptrsize, structs);
        let (a, b) = combine_classes(classes, align as u8);

        let ty = ctx.struct_type(&[a.basic_ty(ctx), b.basic_ty(ctx)], false);
        ArgumentType::Regular(ty.as_basic_type_enum())
    } else {
        ArgumentType::ByVal(make_ty(ty))
    }
}

pub(crate) fn return_ty<'ctx, 'tycx>(
    ctx: &'ctx Context,
    ty: &TyKind<'tycx>,
    ptrsize: u8,
    structs: &IndexMap<StructId, TypedStruct<'tycx>>,
    make_ty: impl FnOnce(&TyKind<'tycx>) -> BasicTypeEnum<'ctx>,
    ty_cx: TypeCtx<'tycx>,
) -> ArgumentType<'ctx> {
    let (size, align) = ty.size_and_alignment(ptrsize as u64, structs, ty_cx);

    if size == 0 {
        ArgumentType::None
    } else if size <= 8 && matches!(ty, TyKind::Ref(_)) {
        ArgumentType::Regular(ctx.ptr_type(AddressSpace::default()).as_basic_type_enum())
    } else if size <= 8 {
        let int = ctx.custom_width_int_type(size as u32 * 8);
        ArgumentType::Regular(int.as_basic_type_enum())
    } else if size <= 16 {
        let mut classes = Vec::new();
        classify_ty(ty, &mut classes, ptrsize, structs);
        let (a, b) = combine_classes(classes, align as u8);

        let ty = ctx.struct_type(&[a.basic_ty(ctx), b.basic_ty(ctx)], false);
        ArgumentType::Regular(ty.as_basic_type_enum())
    } else {
        ArgumentType::SRet(make_ty(ty))
    }
}

fn classify_ty(
    ty: &TyKind<'_>,
    classes: &mut Vec<Class>,
    ptrsize: u8,
    structs: &IndexMap<StructId, TypedStruct<'_>>,
) {
    match ty {
        &TyKind::Struct { struct_id, .. } => structs[struct_id]
            .elements
            .iter()
            .for_each(|(_, ty, _)| classify_ty(ty, classes, ptrsize, structs)),
        TyKind::Tuple(ty_list) => ty_list
            .iter()
            .for_each(|ty| classify_ty(ty, classes, ptrsize, structs)),
        TyKind::PrimitiveVoid => {}
        TyKind::PrimitiveNever => {}
        TyKind::SizedArray {
            ty,
            number_elements,
        } => {
            for _ in 0..*number_elements {
                classify_ty(ty, classes, ptrsize, structs);
            }
        }

        // a bool is like a u8
        TyKind::PrimitiveBool => classes.push(Class::Int(1)),
        // llvm doesn't differentiate between signed and unsigned types.
        TyKind::PrimitiveI8 | TyKind::PrimitiveU8 => classes.push(Class::Int(1)),
        TyKind::PrimitiveI16 | TyKind::PrimitiveU16 => classes.push(Class::Int(2)),
        TyKind::PrimitiveI32 | TyKind::PrimitiveU32 => classes.push(Class::Int(4)),
        TyKind::PrimitiveI64 | TyKind::PrimitiveU64 => classes.push(Class::Int(8)),

        TyKind::PrimitiveF32 => classes.push(Class::Float(4)),
        TyKind::PrimitiveF64 => classes.push(Class::Float(8)),

        // Functions pointers, and usize/isize are pointer-sized integers
        TyKind::Function(_) | TyKind::PrimitiveISize | TyKind::PrimitiveUSize => {
            classes.push(Class::Int(ptrsize))
        }
        // &_ where _ is sized is always just a pointer
        TyKind::Ref(ty) if ty.is_sized() => classes.push(Class::Int(ptrsize)),
        // &_ where _ is not sized is a pointer and a vtable (usize).
        TyKind::Ref(_) => classes.extend([Class::Int(ptrsize), Class::Int(ptrsize)]),

        TyKind::DynType(_) | TyKind::UnsizedArray(_) | TyKind::PrimitiveStr => {
            unreachable!("unsized types cannot be classified")
        }
        TyKind::PrimitiveSelf | TyKind::Generic { .. } => {
            unreachable!("Self and generics should be resolved")
        }
    }
}

fn combine_classes(classes: Vec<Class>, align: u8) -> (Class, Class) {
    let mut size_a = 0;
    let mut float_a = true;
    let mut size_b = 0;
    let mut float_b = true;

    for class in classes {
        match class {
            Class::Int(size) => {
                if size_a + size <= 8 {
                    size_a = align_up(size_a, size);
                    if size_a + size <= 8 {
                        size_a += size;
                        float_a = false;
                        continue;
                    }
                }
                size_b = align_up(size_b, size);
                size_b += size;
                float_b = false;
            }
            Class::Float(size) => {
                if size_a + size <= 8 {
                    size_a = align_up(size_a, size);
                    if size_a + size <= 8 {
                        size_a += size;
                        continue;
                    }
                }
                size_b = align_up(size_b, size);
                size_b += size;
            }
        }
    }
    size_a = align_up(size_a, align);
    size_b = align_up(size_b, align);
    (
        if float_a {
            Class::Float(size_a)
        } else {
            Class::Int(size_a)
        },
        if float_b {
            Class::Float(size_b)
        } else {
            Class::Int(size_b)
        },
    )
}

const fn is_aligned(v: u8, align: u8) -> bool {
    (v & !(align - 1)) == v
}

const fn align_up(v: u8, align: u8) -> u8 {
    if is_aligned(v, align) {
        v
    } else {
        (v & !(align - 1)) + align
    }
}
