use mira_common::store::{AssociatedStore, StoreKey};
use mira_parser::module::ModuleScopeValue;
use mira_parser::std_annotations::alias::ExternAliasAnnotation;
use mira_typeck::{
    TypecheckingContext, TypedExternalFunction, TypedFunction, TypedModule, TypedStatic,
    TypedStruct,
};
use parking_lot::RwLock;
use std::fmt::Write;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::sync::LazyLock;

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

fn path_len(p: &str) -> usize {
    p.chars()
        .map(|v| match v {
            '<' | '>' | '(' | ')' | '{' | '}' => 4,
            ',' | '$' | '#' => 3,
            _ => 1,
        })
        .sum()
}

pub fn mangle_function<'arena>(
    ctx: &TypecheckingContext<'arena>,
    id: StoreKey<TypedFunction<'arena>>,
) -> String {
    let fn_reader = ctx.functions.read();
    let mut mangled_name = "_ZN".to_string();
    mangle_module(ctx, fn_reader[id].0.module_id, &mut mangled_name);

    match fn_reader[id].0.name {
        None => mangled_name.push_str(MANGLED_ANON_FN_NAME),
        Some(ref v) => mangle_path_segment(v.symbol().to_str(), &mut mangled_name),
    }
    mangled_name.push_str("17h"); // hash
    let mut hasher = DefaultHasher::new();
    fn_reader[id].0.hash(&mut hasher);
    write!(mangled_name, "{:x}", hasher.finish()).expect("writing to a string should never fail");

    mangled_name.push('E');
    mangled_name
}

pub fn mangle_external_function<'arena>(
    ctx: &TypecheckingContext<'arena>,
    id: StoreKey<TypedExternalFunction<'arena>>,
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

pub fn mangle_struct<'arena>(
    ctx: &TypecheckingContext<'arena>,
    id: StoreKey<TypedStruct<'arena>>,
) -> String {
    let struct_reader = ctx.structs.read();
    let structure = &struct_reader[id];
    let mut name = String::new();
    module_path(ctx, structure.module_id, &mut name);
    name.push_str("::");
    name.push_str(&structure.name);
    name.push_str("::");
    let mut hasher = DefaultHasher::new();
    structure.hash(&mut hasher);
    write!(name, "{:x}", hasher.finish()).expect("writing to a string should never fail");
    name
}

pub fn mangle_static<'arena>(
    ctx: &TypecheckingContext<'arena>,
    id: StoreKey<TypedStatic<'arena>>,
) -> String {
    let static_reader = ctx.statics.read();
    let static_ = &static_reader[id];
    let mut mangled = "_ZN".to_string();
    mangle_module(ctx, static_.module_id, &mut mangled);
    mangle_path_segment(static_.name.symbol().to_str(), &mut mangled);

    let mut hasher = DefaultHasher::new();
    static_.type_.hash(&mut hasher);
    static_.module_id.hash(&mut hasher);
    write!(mangled, "17h{:x}E", hasher.finish()).expect("writing to a string should never fail");
    mangled
}

pub fn mangle_string(string: &str) -> String {
    let mut name = String::from("str_");
    let mut hasher = DefaultHasher::new();
    string.hash(&mut hasher);
    write!(name, "{:x}", hasher.finish()).expect("writing to a string should never fail");
    name
}

pub fn mangle_name<'arena>(
    ctx: &TypecheckingContext<'arena>,
    item: ModuleScopeValue<'arena>,
) -> String {
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

static MODULE_PATHS: LazyLock<RwLock<AssociatedStore<String, TypedModule<'static>>>> =
    LazyLock::new(Default::default);

/// writes the module path, e.g. `std::os` to the string passed in.
pub fn module_path<'ctx>(
    ctx: &TypecheckingContext<'ctx>,
    module: StoreKey<TypedModule<'ctx>>,
    s: &mut String,
) {
    if let Some(v) = MODULE_PATHS.read().get(&module.cast()) {
        s.push_str(v);
        return;
    }
    let mut cur_mod = module.cast();
    let mut paths = Vec::new();
    let reader = ctx.modules.read();
    loop {
        paths.push(reader[cur_mod].name);
        if reader[cur_mod].parent.cast() == cur_mod || reader[cur_mod].root_module.cast() == cur_mod
        {
            break;
        }
        cur_mod = reader[cur_mod].parent.cast();
    }
    let mut path = String::with_capacity(paths.len() * 8);
    for path_sym in paths.into_iter().rev() {
        if !path.is_empty() {
            path.push_str("::");
        }
        path.push_str(path_sym.to_str());
    }
    s.push_str(&path);
    MODULE_PATHS.write().insert(module.cast(), path);
}

static MANGLED_MODULE_NAMES: LazyLock<RwLock<AssociatedStore<String, TypedModule<'static>>>> =
    LazyLock::new(Default::default);

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

/// write the mangled module path, e.g. `3std2os` for `std::os`.
pub fn mangle_module<'ctx>(
    ctx: &TypecheckingContext<'ctx>,
    module: StoreKey<TypedModule<'ctx>>,
    s: &mut String,
) {
    if let Some(v) = MANGLED_MODULE_NAMES.read().get(&module) {
        s.push_str(v);
        return;
    }
    let mut cur_mod = module.cast();
    let mut paths = Vec::new();
    let reader = ctx.modules.read();
    loop {
        paths.push(reader[cur_mod].name);
        if reader[cur_mod].parent.cast() == cur_mod || reader[cur_mod].root_module.cast() == cur_mod
        {
            break;
        }
        cur_mod = reader[cur_mod].parent.cast();
    }
    let mut path = String::with_capacity(paths.len() * 8);
    for path_sym in paths.into_iter().rev() {
        mangle_path_segment(path_sym.to_str(), &mut path);
    }
    s.push_str(&path);
    MANGLED_MODULE_NAMES.write().insert(module.cast(), path);
}
