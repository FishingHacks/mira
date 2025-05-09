use crate::module::ModuleScopeValue;
use crate::std_annotations::alias::ExternAliasAnnotation;
use crate::store::StoreKey;
use crate::typechecking::{
    TypecheckingContext, TypedExternalFunction, TypedFunction, TypedStatic, TypedStruct,
};
use std::fmt::Write;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::path::Path;

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
        '$' => string.push_str("$D$"),
        '#' => string.push_str("$H$"),
        _ => string.push('_'),
    }
}

fn mangle_path(path: &Path, mangled_string: &mut String) {
    let mut tmp_escaped_name_part = String::new();
    for entry in path.components() {
        let std::path::Component::Normal(name) = entry else {
            continue;
        };
        tmp_escaped_name_part.clear();
        if name.as_encoded_bytes()[0].is_ascii_digit() {
            tmp_escaped_name_part.push('_');
        }
        name.as_encoded_bytes()
            .iter()
            .copied()
            .map(Into::<char>::into)
            .for_each(|c| mangle_char(c, &mut tmp_escaped_name_part));
        write!(
            mangled_string,
            "{}{}",
            tmp_escaped_name_part.len(),
            tmp_escaped_name_part
        )
        .expect("writing to a string should never fail");
    }
}

pub fn mangle_function(ctx: &TypecheckingContext, id: StoreKey<TypedFunction>) -> String {
    let fn_reader = ctx.functions.read();
    let module_reader = ctx.modules.read();
    let module_id = fn_reader[id].0.module_id;
    let v = &module_reader[module_id.cast()];
    let path = v
        .path
        .strip_prefix(v.root.parent().unwrap_or(&v.root))
        .unwrap_or(&v.path);
    let mut mangled_name = "_ZN".to_string();
    mangle_path(path, &mut mangled_name);

    match fn_reader[id].0.name {
        None => mangled_name.push_str(MANGLED_ANON_FN_NAME),
        Some(ref v) => v.with(|v| {
            let mut name = String::new();
            v.chars().for_each(|v| mangle_char(v, &mut name));
            write!(mangled_name, "{}{}", name.len(), name)
                .expect("writing to a string should never fail");
        }),
    }
    mangled_name.push_str("17h"); // hash
    let mut hasher = DefaultHasher::new();
    fn_reader[id].0.hash(&mut hasher);
    write!(mangled_name, "{:x}", hasher.finish()).expect("writing to a string should never fail");

    mangled_name.push('E');
    mangled_name
}

pub fn mangle_external_function(
    ctx: &TypecheckingContext,
    id: StoreKey<TypedExternalFunction>,
) -> String {
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

pub fn mangle_struct(ctx: &TypecheckingContext, id: StoreKey<TypedStruct>) -> String {
    let module_reader = ctx.modules.read();
    let struct_reader = ctx.structs.read();
    let structure = &struct_reader[id];
    let module = &module_reader[structure.module_id.cast()];
    let path = module
        .path
        .strip_prefix(module.root.parent().unwrap_or(&module.root))
        .unwrap_or(&module.path);
    let mut name = path.display().to_string();
    name.push_str("::");
    structure.name.with(|v| name.push_str(v));
    name.push_str("::");
    let mut hasher = DefaultHasher::new();
    structure.hash(&mut hasher);
    write!(name, "{:x}", hasher.finish()).expect("writing to a string should never fail");
    name
}

pub fn mangle_static(ctx: &TypecheckingContext, id: StoreKey<TypedStatic>) -> String {
    let static_reader = ctx.statics.read();
    let structure = &static_reader[id];
    let mut name = String::from("alloc_");
    let mut hasher = DefaultHasher::new();
    structure.0.hash(&mut hasher);
    structure.2.hash(&mut hasher);
    write!(name, "{:x}", hasher.finish()).expect("writing to a string should never fail");
    name
}

pub fn mangle_string(string: &str) -> String {
    let mut name = String::from("str_");
    let mut hasher = DefaultHasher::new();
    string.hash(&mut hasher);
    write!(name, "{:x}", hasher.finish()).expect("writing to a string should never fail");
    name
}

pub fn mangle_name(ctx: &TypecheckingContext, item: ModuleScopeValue) -> String {
    match item {
        ModuleScopeValue::Function(id) => mangle_function(ctx, id.cast()),
        ModuleScopeValue::ExternalFunction(id) => mangle_external_function(ctx, id.cast()),
        ModuleScopeValue::Struct(id) => mangle_struct(ctx, id.cast()),
        ModuleScopeValue::Static(id) => mangle_static(ctx, id.cast()),
        ModuleScopeValue::Module(_) | ModuleScopeValue::Trait(_) => {
            unreachable!("does not have to be mangled")
        }
    }
}
