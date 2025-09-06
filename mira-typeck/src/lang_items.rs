use mira_errors::Diagnostic;
use mira_lexer::token::IdentDisplay;
use std::fmt::{Debug, Display};

use crate::context::TypeCtx;
use crate::{
    FunctionList, Ty, TyKind, TypeckCtx, TypedExternalFunction, TypedFunction,
    TypedFunctionContract, TypedStatic, TypedStruct, TypedTrait, default_types,
};
use mira_common::store::StoreKey;
use mira_macros::ErrorData;
use mira_spans::{Span, interner::Symbol};

struct LangItemTrait<'arena> {
    funcs: Vec<(Symbol<'arena>, LangItemFunction<'arena>)>,
}
struct LangItemStruct<'arena> {
    funcs: Vec<(Symbol<'arena>, LangItemFunction<'arena>)>,
    fields: Vec<(Symbol<'arena>, Ty<'arena>)>,
    traits: Vec<StoreKey<TypedTrait<'arena>>>,
    generics: Vec<(bool, Vec<StoreKey<TypedTrait<'arena>>>)>,
}

struct LangItemFunction<'arena> {
    args: Vec<Ty<'arena>>,
    return_type: Ty<'arena>,
}

impl<'arena> LangItemFunction<'arena> {
    pub fn new(args: Vec<Ty<'arena>>, return_type: Ty<'arena>) -> Self {
        Self { args, return_type }
    }
}

macro_rules! lang_item_def {
    ($($lang_item: ident => $ty: ident),* $(,)?) => {
        #[derive(Debug)]
        pub struct LangItems<'arena> {
            ctx: TypeCtx<'arena>,
            $(pub $lang_item: Option<lang_item_def!(underlying_typ $ty)>,)*
        }

        impl<'arena> LangItems<'arena> {
            pub fn new(ctx: TypeCtx<'arena>) -> Self {
                Self {
                    ctx,
                    $($lang_item: None),*
                }
            }

            /// Pushes a struct as a lang item. returns false if it was not a compiler-internal lang_item
            /// and returns an error if it expected the lang_item to be of a different type.
            #[allow(unreachable_code)]
            pub fn push_struct(&mut self, id: StoreKey<TypedStruct<'arena>>, lang_item: &str, span: Span<'arena>) -> Result<bool, Diagnostic<'arena>> {
                match lang_item {
                    $(stringify!($lang_item) if self.$lang_item.is_some() => return Err(LangItemAssignmentError::Redefinition(span, stringify!($ty)).to_error()),)*
                    $(stringify!($lang_item) => lang_item_def!(expect_struct $ty, self, $lang_item, id, span),)*
                    _ => return Ok(false),
                }
                Ok(true)
            }

            /// Pushes a trait as a lang item. returns false if it was not a compiler-internal lang_item
            /// and returns an error if it expected the lang_item to be of a different type.
            #[allow(unreachable_code)]
            pub fn push_trait(&mut self, id: StoreKey<TypedTrait<'arena>>, lang_item: &str, span: Span<'arena>) -> Result<bool, Diagnostic<'arena>> {
                match lang_item {
                    $(stringify!($lang_item) if self.$lang_item.is_some() => return Err(LangItemAssignmentError::Redefinition(span, stringify!($ty)).to_error()),)*
                    $(stringify!($lang_item) => lang_item_def!(expect_trait $ty, self, $lang_item, id, span),)*
                    _ => return Ok(false),
                }
                Ok(true)
            }

            /// Pushes a static as a lang item. returns false if it was not a compiler-internal lang_item
            /// and returns an error if it expected the lang_item to be of a different type.
            #[allow(unreachable_code)]
            pub fn push_static(&mut self, id: StoreKey<TypedStatic<'arena>>, lang_item: &str, span: Span<'arena>) -> Result<bool, Diagnostic<'arena>> {
                match lang_item {
                    $(stringify!($lang_item) if self.$lang_item.is_some() => return Err(LangItemAssignmentError::Redefinition(span, stringify!($ty)).to_error()),)*
                    $(stringify!($lang_item) => lang_item_def!(expect_static $ty, self, $lang_item, id, span),)*
                    _ => return Ok(false),
                }
                Ok(true)
            }

            #[allow(unreachable_code)]
            fn internal_push_fn(&mut self, _: FunctionLangItem<'arena>, lang_item: &str, span: Span<'arena>) -> Result<bool, Diagnostic<'arena>> {
                match lang_item {
                    $(stringify!($lang_item) if self.$lang_item.is_some() => return Err(LangItemAssignmentError::Redefinition(span, stringify!($ty)).to_error()),)*
                    $(stringify!($lang_item) => lang_item_def!(expect_function $ty, self, $lang_item, id, span),)*
                    _ => return Ok(false),
                }
                Ok(true)
            }

            /// Pushes a static as a lang item. returns false if it was not a compiler-internal lang_item
            /// and returns an error if it expected the lang_item to be of a different type.
            pub fn push_external_function(&mut self, id: StoreKey<TypedExternalFunction<'arena>>, lang_item: &str, span: Span<'arena>) -> Result<bool, Diagnostic<'arena>> {
                self.internal_push_fn(FunctionLangItem::External(id), lang_item, span)
            }

            /// Pushes a static as a lang item. returns false if it was not a compiler-internal lang_item
            /// and returns an error if it expected the lang_item to be of a different type.
            pub fn push_function(&mut self, id: StoreKey<TypedFunction<'arena>>, lang_item: &str, span: Span<'arena>) -> Result<bool, Diagnostic<'arena>> {
                self.internal_push_fn(FunctionLangItem::Internal(id), lang_item, span)
            }

        }
    };

    (underlying_typ Trait) => { StoreKey<TypedTrait<'arena>> };
    (underlying_typ Struct) => { StoreKey<TypedStruct<'arena>> };
    (underlying_typ Function) => { FunctionLangItem<'arena> };
    (underlying_typ Static) => { StoreKey<TypedStatic<'arena>> };

    (expect_struct Struct, $self: ident, $key: ident, $id: ident, $span: ident) => { $self.$key = Some($id) };
    (expect_struct $expected_ty: ident, $self: ident, $key: ident, $id: ident, $span: ident) => { return Err(LangItemAssignmentError::InvalidLangItemError { lang_item: stringify!($key), span: $span, got: LangItemType::Struct, expected: LangItemType::$expected_ty }.to_error()) };

    (expect_trait Trait, $self: ident, $key: ident, $id: ident, $span: ident) => { $self.$key = Some($id) };
    (expect_trait $expected_ty: ident, $self: ident, $key: ident, $id: ident, $span: ident) => { return Err(LangItemAssignmentError::InvalidLangItemError { lang_item: stringify!($key), span: $span, got: LangItemType::Trait, expected: LangItemType::$expected_ty }.to_error()) };

    (expect_static Static, $self: ident, $key: ident, $id: ident, $span: ident) => { $self.$key = Some($id) };
    (expect_static $expected_ty: ident, $self: ident, $key: ident, $id: ident, $span: ident) => { return Err(LangItemAssignmentError::InvalidLangItemError { lang_item: stringify!($key), span: $span, got: LangItemType::Static, expected: LangItemType::$expected_ty }.to_error()) };

    (expect_function Function, $self: ident, $key: ident, $id: ident, $span: ident) => { $self.$key = Some($id) };
    (expect_function $expected_ty: ident, $self: ident, $key: ident, $id: ident, $span: ident) => { return Err(LangItemAssignmentError::InvalidLangItemError { lang_item: stringify!($key), span: $span, got: LangItemType::Function, expected: LangItemType::$expected_ty }.to_error()) };
}

#[derive(Debug, Clone, Copy)]
// TODO: remove after adding a function langitem
#[allow(dead_code)]
pub enum FunctionLangItem<'arena> {
    External(StoreKey<TypedExternalFunction<'arena>>),
    Internal(StoreKey<TypedFunction<'arena>>),
}

#[derive(Clone, Copy, Debug)]
pub enum LangItemType {
    Trait,
    Struct,
    Static,
    Function,
}

impl Display for LangItemType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Trait => f.write_str("trait"),
            Self::Struct => f.write_str("struct"),
            Self::Static => f.write_str("static"),
            Self::Function => f.write_str("function"),
        }
    }
}

#[derive(Clone, Debug, ErrorData)]
pub enum LangItemAssignmentError<'arena> {
    #[error("expected lang item `{lang_item}` to be a {expected}, but got a {got}")]
    InvalidLangItemError {
        #[primary_label("lang item defined here")]
        span: Span<'arena>,
        lang_item: &'static str,
        got: LangItemType,
        expected: LangItemType,
    },
    #[error("tried to redefine lang item `{_1}`")]
    Redefinition(
        #[primary_label("redefinitiion occurs here")] Span<'arena>,
        &'static str,
    ),
}

struct GenericList<'a, 'arena>(&'a [Symbol<'arena>]);
impl Display for GenericList<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in 0..self.0.len() {
            if i != 0 {
                f.write_str(" + ")?;
            }
            Display::fmt(&IdentDisplay(self.0[i]), f)?;
        }
        Ok(())
    }
}

#[derive(Clone, ErrorData, Debug)]
pub enum LangItemError<'arena> {
    #[error("No lang item for `{langitem}` found (type: {ty})")]
    MissingItem {
        langitem: &'static str,
        ty: LangItemType,
    },
    #[error("Static lang-item `{langitem}` is missing trait-implementation {}", IdentDisplay(*name))]
    StaticIsMissingTrait {
        name: Symbol<'arena>,
        langitem: &'static str,
    },
    #[error("Static lang-item `{langitem}` is a primitive, who are not supported")]
    StaticIsPrimitive { langitem: &'static str },
    #[error("Struct lang-item `{langitem}` is missing field {} of type `{ty}`", IdentDisplay(*name))]
    StructMissingElement {
        name: Symbol<'arena>,
        ty: Ty<'arena>,
        langitem: &'static str,
    },
    #[error(
        "Struct lang-item `{langitem}`'s field {} is expected to be {expected} but {found}", IdentDisplay(*name)
    )]
    StructMismatchingField {
        name: Symbol<'arena>,
        expected: Ty<'arena>,
        found: Ty<'arena>,
        langitem: &'static str,
    },
    #[error("Struct lang-item `{langitem}`'s field {} is not expected to be there", IdentDisplay(*name))]
    StructUnexpectedField {
        name: Symbol<'arena>,
        langitem: &'static str,
    },
    #[error("Struct lang-item `{lang_item}` is missing function {}", IdentDisplay(*function))]
    StructMissingFunction {
        lang_item: &'static str,
        function: Symbol<'arena>,
    },
    #[error("Struct lang-item `{lang_item}` is missing the trait {}", IdentDisplay(*trait_name))]
    StructMissingTrait {
        lang_item: &'static str,
        trait_name: Symbol<'arena>,
    },
    #[error("Trait lang-item `{lang_item}` is missing function {}", IdentDisplay(*function))]
    TraitMissingFunction {
        lang_item: &'static str,
        function: Symbol<'arena>,
    },
    #[error(
        "Trait lang-item `{lang_item}`'s function {} has a mismatching signature. Expected: {}, but found {}",
        IdentDisplay(*function),
        FunctionList(expected),
        FunctionList(found)
    )]
    TraitMismatchingArguments {
        expected: Vec<Ty<'arena>>,
        found: Vec<Ty<'arena>>,
        function: Symbol<'arena>,
        lang_item: &'static str,
    },
    #[error(
        "Trait lang-item `{lang_item}`'s function `{}` has a mismatching signature. Expected: fn(...) -> {expected}, but found fn(...) -> {found}", IdentDisplay(*function)
    )]
    TraitMismatchingReturnType {
        expected: Ty<'arena>,
        found: Ty<'arena>,
        function: Symbol<'arena>,
        lang_item: &'static str,
    },
    #[error(
        "Struct lang-item `{lang_item}`'s function {} has a mismatching signature. Expected: {}, but found {}",
        IdentDisplay(*function),
        FunctionList(expected),
        FunctionList(found)
    )]
    StructMismatchingArguments {
        expected: Vec<Ty<'arena>>,
        found: Vec<Ty<'arena>>,
        function: Symbol<'arena>,
        lang_item: &'static str,
    },
    #[error("Struct lang-item `{lang_item}`'s function {} has a mismatching signature. Expected: fn(...) -> {expected}, but found fn(...) -> {found}", IdentDisplay(*function))]
    StructMismatchingReturnType {
        expected: Ty<'arena>,
        found: Ty<'arena>,
        function: Symbol<'arena>,
        lang_item: &'static str,
    },
    #[error("Struct lang-item `{lang_item}`s generic {} requires that it allow unsized types", IdentDisplay(*generic))]
    StructGenericSizingIncompatability {
        lang_item: &'static str,
        generic: Symbol<'arena>,
    },
    #[error(
        "Struct lang-item `{lang_item}`s generic {} doesn't match, expected {}, but found {}",
        IdentDisplay(*generic),
        GenericList(expected),
        GenericList(found)
    )]
    StructGenericMismatch {
        lang_item: &'static str,
        generic: Symbol<'arena>,
        expected: Vec<Symbol<'arena>>,
        found: Vec<Symbol<'arena>>,
    },
    #[error(
        "Struct lang-item `{lang_item}` misses generic with bounds {}",
        GenericList(generic)
    )]
    StructMissesGeneric {
        lang_item: &'static str,
        generic: Vec<Symbol<'arena>>,
    },
    #[error("Struct lang-item `{lang_item}` has an unexpected generic {}", IdentDisplay(*generic))]
    StructUnexpectedGeneric {
        lang_item: &'static str,
        generic: Symbol<'arena>,
    },
    #[error(
        "Function lang-item `{lang_item}` has a mismatching signature. Expected: {}, but found {}",
        FunctionList(expected),
        FunctionList(found)
    )]
    MismatchingArguments {
        expected: Vec<Ty<'arena>>,
        found: Vec<Ty<'arena>>,
        lang_item: &'static str,
    },
    #[error(
        "Function lang-item `{lang_item}` has a mismatching signature. Expected: fn(...) -> {expected}, but found fn(...) -> {found}"
    )]
    MismatchingReturnType {
        expected: Ty<'arena>,
        found: Ty<'arena>,
        lang_item: &'static str,
    },
    #[error("Trait lang-item `{lang_item}` has an unexpected function {}", IdentDisplay(*function))]
    TraitExcessiveFunction {
        lang_item: &'static str,
        function: Symbol<'arena>,
    },
}

macro_rules! check_langitem {
    (required $self:ident.$lang_item:ident: $ty:ident; $reader:ident $context:ident) => {
        if $self.$lang_item.is_none() {
            $context.ctx.emit_missing_item(stringify!($lang_item), LangItemType::$ty);
        }
        check_langitem!($lang_item: $ty; $self $reader $context);
    };
    ($self:ident.$lang_item:ident: $ty:ident; $reader:ident $context:ident) => {
        check_langitem!($lang_item: $ty; $self $reader $context);
    };

    ($lang_item:ident: Trait; $self:ident $reader:ident $context:ident) => {
        if let Some(trait_id) = $self.$lang_item {
            does_trait_match(&$self.$lang_item(), &$reader[trait_id], stringify!($lang_item), $context);
        }
    };
    ($lang_item:ident: Static; $self:ident $reader:ident $context:ident) => {
        if let Some(static_id) = $self.$lang_item {
            $self.$lang_item(|arr| does_static_match(arr, $reader[static_id].ty, stringify!($lang_item), $context));
        }
    };
    ($lang_item:ident: Struct; $self:ident $reader:ident $context:ident) => {
        if let Some(struct_id) = $self.$lang_item {
            does_struct_match(&$self.$lang_item(), &$reader[struct_id], stringify!($lang_item), $context);
        }
    };
    ($lang_item:ident: Function; $self:ident $reader:ident $context:ident) => {
        if let Some(func) = $self.$lang_item {
            let func = match func {
                FunctionLangItem::Internal(id) => &$context.functions.read()[id].0,
                FunctionLangItem::External(id) => &$context.external_functions.read()[id].0,
            };
            does_function_match(&$self.$lang_item(), func, stringify!($lang_item), $context);
        }
    };
    ($lang_item:ident: $ty: ident; $self:ident $reader:ident) => { compile_error!(concat!(stringify!($ty), " is not yet supported")) };
}

lang_item_def! {
    // native type langitems: struct {}; this solely serves the purpose of having methods on those
    // native types. the function should **not** accept `self: Self`
    str => Struct,
    slice => Struct,
    bool => Struct, // done
    f32 => Struct, // done
    f64 => Struct, // done
    i8 => Struct, // done
    u8 => Struct, // done
    i16 => Struct, // done
    u16 => Struct, // done
    i32 => Struct, // done
    u32 => Struct, // done
    i64 => Struct, // done
    u64 => Struct, // done
    isize => Struct, // done
    usize => Struct, // done
    range => Struct,

    // let allocator: dyn lang_item!("allocator_trait");
    allocator => Static, // done

    clone_trait => Trait, // done
    copy_trait => Trait, // done
    drop_trait => Trait, // done
    allocator_trait => Trait, // done
    eq_trait => Trait,
    neq_trait => Trait,
    add_trait => Trait,
    sub_trait => Trait,
    mul_trait => Trait,
    div_trait => Trait,
    mod_trait => Trait,
    index_trait => Trait,
    pos_trait => Trait,
    neg_trait => Trait,
    lnot_trait => Trait,
    bnot_trait => Trait,
    band_trait => Trait,
    bor_trait => Trait,
    bxor_trait => Trait,
    land_trait => Trait,
    lor_trait => Trait,
    gt_trait => Trait,
    lt_trait => Trait,
    gte_trait => Trait,
    lte_trait => Trait,
    lshift_trait => Trait,
    rshift_trait => Trait,
}

impl<'arena> LangItems<'arena> {
    fn empty_struct() -> LangItemStruct<'arena> {
        LangItemStruct {
            traits: Vec::new(),
            funcs: Vec::new(),
            fields: Vec::new(),
            generics: Vec::new(),
        }
    }

    fn bool(&self) -> LangItemStruct<'arena> {
        Self::empty_struct()
    }
    fn f32(&self) -> LangItemStruct<'arena> {
        Self::empty_struct()
    }
    fn f64(&self) -> LangItemStruct<'arena> {
        Self::empty_struct()
    }
    fn u8(&self) -> LangItemStruct<'arena> {
        Self::empty_struct()
    }
    fn i8(&self) -> LangItemStruct<'arena> {
        Self::empty_struct()
    }
    fn u16(&self) -> LangItemStruct<'arena> {
        Self::empty_struct()
    }
    fn i16(&self) -> LangItemStruct<'arena> {
        Self::empty_struct()
    }
    fn u32(&self) -> LangItemStruct<'arena> {
        Self::empty_struct()
    }
    fn i32(&self) -> LangItemStruct<'arena> {
        Self::empty_struct()
    }
    fn u64(&self) -> LangItemStruct<'arena> {
        Self::empty_struct()
    }
    fn i64(&self) -> LangItemStruct<'arena> {
        Self::empty_struct()
    }
    fn usize(&self) -> LangItemStruct<'arena> {
        Self::empty_struct()
    }
    fn isize(&self) -> LangItemStruct<'arena> {
        Self::empty_struct()
    }

    fn allocator_trait(&self) -> LangItemTrait<'arena> {
        // trait Allocator {
        //     fn alloc(self: &Self, size: usize) -> &void;
        //     fn free(self: &Self, ptr: &void);
        //     fn realloc(self: &Self, ptr: &void, new_size: usize) -> &void;
        // }
        LangItemTrait {
            funcs: vec![
                (
                    self.ctx.intern_str("alloc"),
                    LangItemFunction::new(
                        vec![default_types::self_ref, default_types::usize],
                        default_types::void_ref,
                    ),
                ),
                (
                    self.ctx.intern_str("free"),
                    LangItemFunction::new(
                        vec![default_types::self_ref, default_types::void_ref],
                        default_types::void,
                    ),
                ),
                (
                    self.ctx.intern_str("realloc"),
                    LangItemFunction::new(
                        vec![
                            default_types::self_ref,
                            default_types::void_ref,
                            default_types::usize,
                        ],
                        default_types::void_ref,
                    ),
                ),
            ],
        }
    }

    fn allocator<R, F: FnMut(&[StoreKey<TypedTrait<'arena>>]) -> R>(&self, mut func: F) -> R {
        // let allocator: dyn Allocator;
        // even if there's no allocator trait, throw an error if lang item does not exist.
        if let Some(trait_id) = self.allocator_trait {
            func(&[trait_id])
        } else {
            func(&[])
        }
    }

    fn copy_trait(&self) -> LangItemTrait<'arena> {
        // trait Copy {}
        LangItemTrait { funcs: Vec::new() }
    }

    fn clone_trait(&self) -> LangItemTrait<'arena> {
        // trait Clone { fn clone(self: &Self) -> Self; }
        LangItemTrait {
            funcs: vec![(
                self.ctx.intern_str("clone"),
                LangItemFunction::new(vec![default_types::self_ref], default_types::self_),
            )],
        }
    }

    fn drop_trait(&self) -> LangItemTrait<'arena> {
        // trait Clone { fn clone(self: &Self) -> Self; }
        LangItemTrait {
            funcs: vec![(
                self.ctx.intern_str("drop"),
                LangItemFunction::new(vec![default_types::self_ref], default_types::void),
            )],
        }
    }

    pub fn check(&self, context: &TypeckCtx<'arena>) {
        let trait_reader = context.traits.read();
        let struct_reader = context.structs.read();
        let static_reader = context.statics.read();

        check_langitem!(required self.allocator_trait: Trait; trait_reader context);
        check_langitem!(required self.clone_trait: Trait; trait_reader context);
        check_langitem!(required self.drop_trait: Trait; trait_reader context);
        check_langitem!(required self.copy_trait: Trait; trait_reader context);
        check_langitem!(required self.allocator: Static; static_reader context);
        check_langitem!(self.bool: Struct; struct_reader context);
        check_langitem!(self.f32: Struct; struct_reader context);
        check_langitem!(self.f64: Struct; struct_reader context);
        check_langitem!(self.i8: Struct; struct_reader context);
        check_langitem!(self.u8: Struct; struct_reader context);
        check_langitem!(self.i16: Struct; struct_reader context);
        check_langitem!(self.u16: Struct; struct_reader context);
        check_langitem!(self.i32: Struct; struct_reader context);
        check_langitem!(self.u32: Struct; struct_reader context);
        check_langitem!(self.i64: Struct; struct_reader context);
        check_langitem!(self.u64: Struct; struct_reader context);
        check_langitem!(self.isize: Struct; struct_reader context);
        check_langitem!(self.usize: Struct; struct_reader context);
    }
}

#[allow(dead_code)]
fn does_function_match<'arena>(
    func_a: &LangItemFunction<'arena>,
    func_b: &TypedFunctionContract<'arena>,
    lang_item: &'static str,
    context: &TypeckCtx<'arena>,
) -> bool {
    let tracker = context.ctx.track_errors();

    if func_a.return_type != func_b.return_type {
        context
            .ctx
            .emit_mismatching_return_type(func_a.return_type, func_b.return_type, lang_item);
    }

    if !func_a
        .args
        .iter()
        .zip(func_b.arguments.iter())
        .all(|(a, (_, b))| *a == *b)
    {
        let args = func_b.arguments.iter().map(|(_, v)| v).cloned().collect();
        context
            .ctx
            .emit_mismatching_arguments(func_a.args.clone(), args, lang_item);
    }

    !context.ctx.errors_happened(tracker)
}

fn does_struct_match<'arena>(
    structure_a: &LangItemStruct<'arena>,
    structure_b: &TypedStruct<'arena>,
    lang_item: &'static str,
    context: &TypeckCtx<'arena>,
) -> bool {
    let tracker = context.ctx.track_errors();
    let trait_reader = context.traits.read();

    for (i, bounds) in structure_a.generics.iter().enumerate() {
        match structure_b.generics.get(i) {
            Some(generic) => {
                if *bounds.1 != *generic.bounds {
                    let expected = bounds
                        .1
                        .iter()
                        .map(|v| trait_reader[*v].name.symbol())
                        .collect();
                    let found = generic
                        .bounds
                        .iter()
                        .map(|v| trait_reader[*v].name.symbol())
                        .collect();
                    context.ctx.emit_struct_generic_mismatch(
                        lang_item,
                        generic.name.symbol(),
                        expected,
                        found,
                    );
                }
                if !bounds.0 && generic.sized {
                    context.ctx.emit_struct_generic_sizing_incompatability(
                        lang_item,
                        generic.name.symbol(),
                    );
                }
            }
            None => {
                context.ctx.emit_struct_misses_generic(
                    lang_item,
                    bounds
                        .1
                        .iter()
                        .map(|v| trait_reader[*v].name.symbol())
                        .collect(),
                );
            }
        }
    }
    for generic in structure_b
        .generics
        .iter()
        .skip(structure_a.generics.len())
        .map(|v| v.name.symbol())
    {
        context
            .ctx
            .emit_struct_unexpected_generic(lang_item, generic);
    }

    for (element_name, element_type) in structure_a.fields.iter() {
        match structure_b
            .elements
            .iter()
            .find(|(v, _, _)| v.symbol() == *element_name)
        {
            Some(v) if v.1 == *element_type => {}
            Some((_, other_type, _)) => {
                context.ctx.emit_struct_mismatching_field(
                    *element_name,
                    *element_type,
                    *other_type,
                    lang_item,
                );
            }
            None => {
                context
                    .ctx
                    .emit_struct_missing_element(*element_name, *element_type, lang_item);
            }
        }
    }

    for (element_name, _, _) in structure_b.elements.iter().filter(|(v, _, _)| {
        !structure_a
            .fields
            .iter()
            .any(|(name, _)| *name == v.symbol())
    }) {
        context
            .ctx
            .emit_struct_unexpected_field(element_name.symbol(), lang_item);
    }

    for trait_id in structure_a.traits.iter().copied() {
        if !structure_b.trait_impl.contains_key(&trait_id) {
            context
                .ctx
                .emit_struct_missing_trait(lang_item, trait_reader[trait_id].name.symbol());
        }
    }
    drop(trait_reader);
    let func_reader = context.functions.read();

    for (fn_name, func) in structure_a.funcs.iter() {
        let Some(func_impl) = structure_b.global_impl.get(fn_name) else {
            context
                .ctx
                .emit_struct_missing_function(lang_item, *fn_name);
            continue;
        };
        let func_impl = &func_reader[*func_impl].0;

        if func_impl.return_type != func.return_type {
            context.ctx.emit_struct_mismatching_return_type(
                func.return_type,
                func_impl.return_type,
                *fn_name,
                lang_item,
            );
        }

        if !func
            .args
            .iter()
            .zip(func_impl.arguments.iter())
            .all(|(a, (_, b))| *a == *b)
        {
            let found = func_impl
                .arguments
                .iter()
                .map(|(_, v)| v)
                .cloned()
                .collect();
            context.ctx.emit_struct_mismatching_arguments(
                func.args.clone(),
                found,
                *fn_name,
                lang_item,
            );
        }
    }

    !context.ctx.errors_happened(tracker)
}

fn does_static_match<'arena>(
    traits: &[StoreKey<TypedTrait<'arena>>],
    ty: Ty<'arena>,
    lang_item: &'static str,
    context: &TypeckCtx<'arena>,
) -> bool {
    let trait_reader = context.traits.read();
    match &**ty {
        TyKind::DynType(trait_refs) => {
            let tracker = context.ctx.track_errors();
            for trait_id in traits.iter().copied() {
                if !trait_refs.iter().any(|(v, _)| *v == trait_id) {
                    context.ctx.emit_static_is_missing_trait(
                        trait_reader[trait_id].name.symbol(),
                        lang_item,
                    );
                }
            }
            !context.ctx.errors_happened(tracker)
        }
        TyKind::Struct { struct_id, .. } => {
            let struct_traits = &context.structs.read()[*struct_id].trait_impl;
            let tracker = context.ctx.track_errors();
            for trait_id in traits {
                if !struct_traits.contains_key(trait_id) {
                    context.ctx.emit_static_is_missing_trait(
                        trait_reader[*trait_id].name.symbol(),
                        lang_item,
                    );
                }
            }
            !context.ctx.errors_happened(tracker)
        }
        _ => {
            context.ctx.emit_static_is_primitive(lang_item);
            false
        }
    }
}

fn does_trait_match<'arena>(
    trait_a: &LangItemTrait<'arena>,
    trait_b: &TypedTrait<'arena>,
    lang_item: &'static str,
    context: &TypeckCtx<'arena>,
) -> bool {
    let tracker = context.ctx.track_errors();

    for (func_name, func_a) in trait_a.funcs.iter() {
        let Some((_, func_args_b, func_return_b, ..)) = trait_b
            .functions
            .iter()
            .find(|(name, ..)| *func_name == name.symbol())
        else {
            context
                .ctx
                .emit_trait_missing_function(lang_item, *func_name);
            continue;
        };

        if func_a.return_type != *func_return_b {
            context.ctx.emit_trait_mismatching_return_type(
                func_a.return_type,
                *func_return_b,
                *func_name,
                lang_item,
            );
        }

        if func_a.args.len() != func_args_b.len()
            || !func_a
                .args
                .iter()
                .zip(func_args_b.iter())
                .all(|(a, (_, b))| a == b)
        {
            let found = func_args_b.iter().map(|(_, v)| v).cloned().collect();
            context.ctx.emit_trait_mismatching_arguments(
                func_a.args.clone(),
                found,
                *func_name,
                lang_item,
            );
        }
    }

    for (func_name, ..) in trait_b.functions.iter() {
        if !trait_a
            .funcs
            .iter()
            .any(|(name, ..)| *name == func_name.symbol())
        {
            context
                .ctx
                .emit_trait_excessive_function(lang_item, func_name.symbol());
        }
    }

    !context.ctx.errors_happened(tracker)
}
