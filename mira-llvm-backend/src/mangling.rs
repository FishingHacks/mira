use mira_parser::module::{ExternalFunctionId, FunctionContext, ModuleId, StaticId, StructId};
use mira_parser::std_annotations::alias::ExternAliasAnnotation;
use mira_spans::TypeArena;
use mira_typeck::queries::Providers;
use mira_typeck::{Ty, TyKind, TypeckCtx, default_types};
use std::fmt::Write;
use std::hash::{DefaultHasher, Hash, Hasher};

use crate::FnInstance;

pub const ANON_FN_NAME: &str = "{{anonymous}}";
pub const MANGLED_ANON_FN_NAME: &str = "25$CL$$CL$anonymous$CR$$CR$";

fn mangle_char(c: char, string: &mut String) {
    match c {
        'a'..='z' | 'A'..='Z' | '_' | '.' | '-' | '0'..='9' => string.push(c),
        '<' => string.push_str("$LT$"),
        '>' => string.push_str("$GT$"),
        ',' => string.push_str("$C$"),
        '(' => string.push_str("$PL$"),
        ')' => string.push_str("$PR$"),
        '{' => string.push_str("$CL$"),
        '}' => string.push_str("$CR$"),
        '[' => string.push_str("$BL$"),
        ']' => string.push_str("$BR$"),
        '&' => string.push_str("$RF$"),
        '!' => string.push_str("$EX$"),
        '$' => string.push_str("$D$"),
        '#' => string.push_str("$H$"),
        ';' => string.push_str("$S$"),
        '+' => string.push_str("$P$"),
        _ => string.push('_'),
    }
}

fn path_len(p: &str) -> usize {
    p.chars()
        .map(|v| match v {
            '<' | '>' | '(' | ')' | '{' | '}' | '[' | ']' | '&' | '!' => 4,
            ',' | '$' | '#' | ';' | '+' => 3,
            _ => 1,
        })
        .sum()
}

pub fn mangle_external_function(ctx: &TypeckCtx<'_>, id: ExternalFunctionId) -> String {
    let reader = &ctx.external_functions.read()[id].0;
    if let Some(v) = reader
        .annotations
        .get_first_annotation::<ExternAliasAnnotation>()
    {
        return v.0.to_string();
    }
    reader
        .name
        .as_ref()
        .expect("external functions need a name")
        .to_string()
}

pub fn mangle_static(ctx: &TypeckCtx<'_>, id: StaticId) -> String {
    let static_reader = ctx.statics.read();
    let static_ = &static_reader[id];
    let mut mangled = "_ZN".to_string();
    mangled.push_str(ctx.mangle_module(static_.module_id));
    mangle_path_segment(static_.name.symbol().to_str(), &mut mangled);

    let mut hasher = DefaultHasher::new();
    static_.ty.hash(&mut hasher);
    static_.module_id.hash(&mut hasher);
    write!(mangled, "17h{:x}E", hasher.finish()).expect("writing to a string should never fail");
    mangled
}

pub fn mangle_function_instance<'ctx>(ctx: &TypeckCtx<'ctx>, instance: FnInstance<'ctx>) -> String {
    let fn_reader = ctx.functions.read();
    let mut mangled_name = "_ZN".to_string();

    mangled_name.push_str(match fn_reader[instance.fn_id].0.context {
        FunctionContext::Freestanding => ctx.mangle_module(fn_reader[instance.fn_id].0.module_id),
        FunctionContext::StructFn(struct_id) => ctx.mangle_struct_segment(struct_id),
    });

    match fn_reader[instance.fn_id].0.name {
        None => mangle_generic_segment(ANON_FN_NAME, &instance.generics, &mut mangled_name),
        Some(ref v) => {
            mangle_generic_segment(v.symbol().to_str(), &instance.generics, &mut mangled_name)
        }
    }
    mangled_name.push_str("17h"); // hash
    let mut hasher = DefaultHasher::new();
    fn_reader[instance.fn_id].0.hash(&mut hasher);
    instance.generics.hash(&mut hasher);
    write!(mangled_name, "{:x}", hasher.finish()).expect("writing to a string should never fail");

    mangled_name.push('E');
    mangled_name
}

pub fn mangle_ty(ty: &TyKind<'_>, path: &mut String) {
    match ty {
        TyKind::DynType(traits) => {
            path.push_str("dyn ");
            for (i, (_, name)) in traits.iter().enumerate() {
                if i != 0 {
                    mangle_char('+', path);
                }
                name.chars().for_each(|c| mangle_char(c, path));
            }
        }
        TyKind::Struct { name, .. } => name.chars().for_each(|c| mangle_char(c, path)),
        TyKind::UnsizedArray(ty) => {
            mangle_char('[', path);
            mangle_ty(ty, path);
            mangle_char(']', path);
        }
        TyKind::SizedArray {
            ty,
            number_elements,
        } => {
            mangle_char('[', path);
            mangle_ty(ty, path);
            mangle_char(';', path);
            write!(path, " {number_elements}").unwrap();
            mangle_char(']', path);
        }
        TyKind::Tuple(ty_list) => {
            mangle_char('(', path);
            for (i, ty) in ty_list.iter().enumerate() {
                if i != 0 {
                    mangle_char(',', path);
                }
                mangle_ty(ty, path);
            }
            mangle_char(')', path);
        }
        TyKind::Function(function_type) => {
            path.push_str("fn");
            mangle_char('(', path);
            for (i, ty) in function_type.arguments.iter().enumerate() {
                if i != 0 {
                    mangle_char(',', path);
                }
                mangle_ty(ty, path);
            }
            mangle_char(')', path);
            if function_type.return_type != default_types::void {
                path.push(' ');
                mangle_char('-', path);
                mangle_char('>', path);
                path.push(' ');
                mangle_ty(&function_type.return_type, path);
            }
        }
        TyKind::PrimitiveVoid => path.push_str("void"),
        TyKind::PrimitiveNever => mangle_char('!', path),
        TyKind::PrimitiveI8 => path.push_str("i8"),
        TyKind::PrimitiveI16 => path.push_str("i16"),
        TyKind::PrimitiveI32 => path.push_str("i32"),
        TyKind::PrimitiveI64 => path.push_str("i64"),
        TyKind::PrimitiveISize => path.push_str("isize"),
        TyKind::PrimitiveU8 => path.push_str("u8"),
        TyKind::PrimitiveU16 => path.push_str("u16"),
        TyKind::PrimitiveU32 => path.push_str("u32"),
        TyKind::PrimitiveU64 => path.push_str("u64"),
        TyKind::PrimitiveUSize => path.push_str("usize"),
        TyKind::PrimitiveF32 => path.push_str("f32"),
        TyKind::PrimitiveF64 => path.push_str("f64"),
        TyKind::PrimitiveStr => path.push_str("str"),
        TyKind::PrimitiveBool => path.push_str("bool"),
        TyKind::Ref(ty) => {
            mangle_char('&', path);
            mangle_ty(ty, path);
        }
        TyKind::PrimitiveSelf | TyKind::Generic { .. } => {
            unreachable!("generics and self should be resolved by now.")
        }
    }
}

fn mangle_generic_segment(segment: &str, tys: &[Ty<'_>], path: &mut String) {
    if tys.is_empty() {
        return mangle_path_segment(segment, path);
    }

    let path_needs_semicolon = matches!(segment.chars().next(), None | Some('0'..='9'));
    let mut mangled_segment = String::new();
    if path_needs_semicolon {
        mangled_segment.push('_');
    }
    segment
        .chars()
        .for_each(|c| mangle_char(c, &mut mangled_segment));
    mangle_char('<', &mut mangled_segment);
    for (i, ty) in tys.iter().enumerate() {
        if i != 0 {
            mangle_char(',', &mut mangled_segment);
        }
        mangle_ty(ty, &mut mangled_segment);
    }
    mangle_char('>', &mut mangled_segment);
    write!(path, "{}{mangled_segment}", mangled_segment.len()).unwrap();
}

fn mangle_path_segment(segment: &str, path: &mut String) {
    // if the path is empty or starts with a number, we have to print _<path>. This is because
    // it starts with a number equivalent to it's length, so if the path is 0 sized, that's
    // mean it'd be 0<number>, which is weird (e.g. ``::meow) would be 04meow, with this it'd
    // 1_4meow. Same with number, if the path would be `3`::meow, it'd be 134meow, which is
    // incorrect, so we add a semicolon to make iit 2_34meow. there 34 will not be
    // misinterpreted, because the 2 specified `_3` is the path, so the number for the next
    // path segment cannot be the 3, and only the 4, making this path correct.
    let path_needs_semicolon = matches!(segment.chars().next(), None | Some('0'..='9'));
    let len = path_len(segment) + if path_needs_semicolon { 1 } else { 0 };
    write!(path, "{len}").unwrap();
    if path_needs_semicolon {
        path.push('_');
    }
    for c in segment.chars() {
        mangle_char(c, path);
    }
}

pub fn get_module_path<'ctx>(
    ctx: &TypeckCtx<'ctx>,
    mut cur_mod: ModuleId,
    arena: &'ctx TypeArena<u8>,
) -> &'ctx str {
    let mut paths = Vec::new();
    let reader = ctx.modules.read();
    loop {
        paths.push(reader[cur_mod].name);
        let Some(parent) = reader[cur_mod].parent else {
            break;
        };
        cur_mod = parent;
    }
    let mut path = String::with_capacity(paths.len() * 8);
    for path_sym in paths.into_iter().rev() {
        if !path.is_empty() {
            path.push_str("::");
        }
        path.push_str(path_sym.to_str());
    }
    arena.allocate_str(&path)
}

pub fn mangle_struct(ctx: &TypeckCtx<'_>, id: StructId) -> String {
    let struct_reader = ctx.structs.read();
    let structure = &struct_reader[id];
    let mut name = ctx.get_module_path(structure.module_id).to_string();
    name.push_str("::");
    name.push_str(&structure.name);
    name.push_str("::");
    let mut hasher = DefaultHasher::new();
    structure.hash(&mut hasher);
    write!(name, "{:x}", hasher.finish()).unwrap();
    name
}

pub fn mangle_module<'ctx>(
    ctx: &TypeckCtx<'ctx>,
    mut cur_mod: ModuleId,
    arena: &'ctx TypeArena<u8>,
) -> &'ctx str {
    let mut paths = Vec::new();
    let reader = ctx.modules.read();
    loop {
        paths.push(reader[cur_mod].name);
        let Some(parent) = reader[cur_mod].parent else {
            break;
        };
        cur_mod = parent;
    }
    let mut path = String::with_capacity(paths.len() * 8);
    for path_sym in paths.into_iter().rev() {
        mangle_path_segment(path_sym.to_str(), &mut path);
    }
    arena.allocate_str(&path)
}

pub fn mangle_struct_segment<'ctx>(
    ctx: &TypeckCtx<'ctx>,
    id: StructId,
    arena: &'ctx TypeArena<u8>,
) -> &'ctx str {
    let struct_reader = ctx.structs.read();
    let structure = &struct_reader[id];
    let mut name = String::new();
    name.push_str(ctx.mangle_module(structure.module_id));
    mangle_path_segment(&structure.name, &mut name);
    arena.allocate_str(&name)
}

pub fn provide(providers: &mut Providers<'_>) {
    providers.mangle_module = mangle_module;
    providers.mangle_struct_segment = mangle_struct_segment;
    providers.get_module_path = get_module_path;
}
