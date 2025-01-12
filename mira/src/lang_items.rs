use std::fmt::{Debug, Display, Write};

use crate::{
    annotations::{Annotation, AnnotationReceiver, Annotations},
    error::{FunctionList, ParsingError},
    globals::GlobalStr,
    tokenizer::{Literal, Location, Token, TokenType},
    typechecking::{Type, TypecheckedFunctionContract, TypecheckingContext, TypedTrait},
};
use crate::{
    module::{ExternalFunctionId, FunctionId, StaticId, StructId, TraitId},
    typechecking::TypedStruct,
};
use thiserror::Error;

#[derive(Clone, Debug)]
pub struct LangItemAnnotation(String);

impl Annotation for LangItemAnnotation {
    fn get_name(&self) -> &'static str {
        "lang"
    }

    fn is_valid_for(&self, thing: AnnotationReceiver, annotations: &Annotations) -> bool {
        match thing {
            AnnotationReceiver::Function
            | AnnotationReceiver::ExternalFunction
            | AnnotationReceiver::Struct
            | AnnotationReceiver::Trait
            | AnnotationReceiver::Static => true,
            _ => false,
        }
    }
}

impl LangItemAnnotation {
    pub fn get_langitem(&self) -> &str {
        &self.0
    }

    pub fn parse(tokens: Vec<Token>, loc: Location) -> Result<Self, ParsingError> {
        if tokens.len() < 1 {
            return Err(ParsingError::ExpectedArbitrary {
                loc,
                expected: TokenType::StringLiteral,
                found: TokenType::Eof,
            });
        } else if tokens[0].typ != TokenType::StringLiteral {
            return Err(ParsingError::ExpectedArbitrary {
                loc: tokens[0].location.clone(),
                expected: TokenType::StringLiteral,
                found: tokens[0].typ,
            });
        } else if tokens.len() > 1 {
            return Err(ParsingError::ExpectedArbitrary {
                loc: tokens[1].location.clone(),
                expected: TokenType::Eof,
                found: tokens[1].typ,
            });
        } else {
            let Some(Literal::String(ref string)) = tokens[0].literal else {
                return Err(ParsingError::InvalidTokenization {
                    loc: tokens[0].location.clone(),
                });
            };
            Ok(LangItemAnnotation(string.with(|v| v.to_string())))
        }
    }
}

impl Display for LangItemAnnotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("@lang(")?;
        Debug::fmt(&self.0, f)?;
        f.write_char(')')
    }
}

struct LangItemTrait {
    funcs: Vec<(GlobalStr, LangItemFunction)>,
}
struct LangItemStruct {
    funcs: Vec<(GlobalStr, LangItemFunction)>,
    fields: Vec<(GlobalStr, Type)>,
    traits: Vec<TraitId>,
    generics: Vec<Vec<TraitId>>,
}

struct LangItemFunction {
    args: Vec<Type>,
    return_type: Type,
}

impl LangItemFunction {
    pub fn new(args: Vec<Type>, return_type: Type) -> Self {
        Self { args, return_type }
    }
}

macro_rules! lang_item_def {
    ($($lang_item: ident => $ty: ident),* $(,)?) => {
        #[derive(Debug, Default)]
        pub struct LangItems {
            $(pub $lang_item: Option<lang_item_def!(underlying_typ $ty)>,)*
        }

        impl LangItems {
            /// Pushes a struct as a lang item. returns false if it was not a compiler-internal lang_item
            /// and returns an error if it expected the lang_item to be of a different type.
            #[allow(unreachable_code)]
            pub fn push_struct(&mut self, id: StructId, lang_item: &str, loc: Location) -> Result<bool, LangItemAssignmentError> {
                match lang_item {
                    $(stringify!($lang_item) if self.$lang_item.is_some() => return Err(LangItemAssignmentError::Redefinition(stringify!($ty), loc)),)*
                    $(stringify!($lang_item) => lang_item_def!(expect_struct $ty, self, $lang_item, id, loc),)*
                    _ => return Ok(false),
                }
                Ok(true)
            }

            /// Pushes a trait as a lang item. returns false if it was not a compiler-internal lang_item
            /// and returns an error if it expected the lang_item to be of a different type.
            #[allow(unreachable_code)]
            pub fn push_trait(&mut self, id: TraitId, lang_item: &str, loc: Location) -> Result<bool, LangItemAssignmentError> {
                match lang_item {
                    $(stringify!($lang_item) if self.$lang_item.is_some() => return Err(LangItemAssignmentError::Redefinition(stringify!($ty), loc)),)*
                    $(stringify!($lang_item) => lang_item_def!(expect_trait $ty, self, $lang_item, id, loc),)*
                    _ => return Ok(false),
                }
                Ok(true)
            }

            /// Pushes a static as a lang item. returns false if it was not a compiler-internal lang_item
            /// and returns an error if it expected the lang_item to be of a different type.
            #[allow(unreachable_code)]
            pub fn push_static(&mut self, id: StaticId, lang_item: &str, loc: Location) -> Result<bool, LangItemAssignmentError> {
                match lang_item {
                    $(stringify!($lang_item) if self.$lang_item.is_some() => return Err(LangItemAssignmentError::Redefinition(stringify!($ty), loc)),)*
                    $(stringify!($lang_item) => lang_item_def!(expect_static $ty, self, $lang_item, id, loc),)*
                    _ => return Ok(false),
                }
                Ok(true)
            }

            #[allow(unreachable_code)]
            fn internal_push_fn(&mut self, id: FunctionLangItem, lang_item: &str, loc: Location) -> Result<bool, LangItemAssignmentError> {
                match lang_item {
                    $(stringify!($lang_item) if self.$lang_item.is_some() => return Err(LangItemAssignmentError::Redefinition(stringify!($ty), loc)),)*
                    $(stringify!($lang_item) => lang_item_def!(expect_function $ty, self, $lang_item, id, loc),)*
                    _ => return Ok(false),
                }
                Ok(true)
            }

            /// Pushes a static as a lang item. returns false if it was not a compiler-internal lang_item
            /// and returns an error if it expected the lang_item to be of a different type.
            pub fn push_external_function(&mut self, id: ExternalFunctionId, lang_item: &str, loc: Location) -> Result<bool, LangItemAssignmentError> {
                self.internal_push_fn(FunctionLangItem::External(id), lang_item, loc)
            }

            /// Pushes a static as a lang item. returns false if it was not a compiler-internal lang_item
            /// and returns an error if it expected the lang_item to be of a different type.
            pub fn push_function(&mut self, id: FunctionId, lang_item: &str, loc: Location) -> Result<bool, LangItemAssignmentError> {
                self.internal_push_fn(FunctionLangItem::Internal(id), lang_item, loc)
            }

        }
    };

    (underlying_typ Trait) => { TraitId };
    (underlying_typ Struct) => { StructId };
    (underlying_typ Function) => { FunctionLangItem };
    (underlying_typ Static) => { StaticId };

    (expect_struct Struct, $self: ident, $key: ident, $id: ident, $loc: ident) => { $self.$key = Some($id) };
    (expect_struct $expected_ty: ident, $self: ident, $key: ident, $id: ident, $loc: ident) => { return Err(LangItemAssignmentError::InvalidLangItemError { lang_item: stringify!($key), loc: $loc, got: LangItemType::Struct, expected: LangItemType::$expected_ty }) };

    (expect_trait Trait, $self: ident, $key: ident, $id: ident, $loc: ident) => { $self.$key = Some($id) };
    (expect_trait $expected_ty: ident, $self: ident, $key: ident, $id: ident, $loc: ident) => { return Err(LangItemAssignmentError::InvalidLangItemError { lang_item: stringify!($key), loc: $loc, got: LangItemType::Trait, expected: LangItemType::$expected_ty }) };

    (expect_static Static, $self: ident, $key: ident, $id: ident, $loc: ident) => { $self.$key = Some($id) };
    (expect_static $expected_ty: ident, $self: ident, $key: ident, $id: ident, $loc: ident) => { return Err(LangItemAssignmentError::InvalidLangItemError { lang_item: stringify!($key), loc: $loc, got: LangItemType::Static, expected: LangItemType::$expected_ty }) };

    (expect_function Function, $self: ident, $key: ident, $id: ident, $loc: ident) => { $self.$key = Some($id) };
    (expect_function $expected_ty: ident, $self: ident, $key: ident, $id: ident, $loc: ident) => { return Err(LangItemAssignmentError::InvalidLangItemError { lang_item: stringify!($key), loc: $loc, got: LangItemType::Function, expected: LangItemType::$expected_ty }) };
}

#[derive(Debug, Clone, Copy)]
pub enum FunctionLangItem {
    External(ExternalFunctionId),
    Internal(FunctionId),
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

#[derive(Clone, Debug, Error)]
pub enum LangItemAssignmentError {
    #[error("{loc}: expected lang item `{lang_item}` to be a {expected}, but got a {got}")]
    InvalidLangItemError {
        lang_item: &'static str,
        loc: Location,
        got: LangItemType,
        expected: LangItemType,
    },
    #[error("{0}: tried to redefine lang item `{1}`")]
    Redefinition(&'static str, Location),
}

struct GenericList<'a>(&'a [GlobalStr]);
impl Display for GenericList<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in 0..self.0.len() {
            if i != 0 {
                f.write_str(" + ")?;
            }
            Display::fmt(&self.0[i], f)?;
        }
        Ok(())
    }
}

#[derive(Clone, Error, Debug)]
pub enum LangItemError {
    #[error("No lang item for `{0}` found (type: {1})")]
    MissingItem(&'static str, LangItemType),
    #[error("Static lang-item `{1}` is missing trait-implementation `{0}`")]
    StaticIsMissingTrait(GlobalStr, &'static str),
    #[error("Static lang-item `{0}` is a primitive, who are not supported")]
    StaticIsPrimitive(&'static str),
    #[error("Struct lang-item `{2}` is missing field `{1}` of type `{2}`")]
    StructMissingElement(GlobalStr, Type, &'static str),
    #[error("Struct lang-item `{3}`'s field `{1}` is expected to be {1} but {2}")]
    StructMismatchingField(GlobalStr, Type, Type, &'static str),
    #[error("Struct lang-item `{1}`'s field `{0}` is not expected to be there")]
    StructUnexpectedField(GlobalStr, &'static str),
    #[error("Struct lang-item `{lang_item}` is missing function `{function}`")]
    StructMissingFunction {
        lang_item: &'static str,
        function: GlobalStr,
    },
    #[error("Struct lang-item `{lang_item}` is missing the trait `{trait_name}`")]
    StructMissingTrait {
        lang_item: &'static str,
        trait_name: GlobalStr,
    },
    #[error("Trait lang-item `{lang_item}` is missing function `{function}`")]
    TraitMissingFunction {
        lang_item: &'static str,
        function: GlobalStr,
    },
    #[error("Trait lang-item `{lang_item}`'s function `{function}` has a mismatching signature. Expected: {}, but found {}", FunctionList(.expected), FunctionList(.found))]
    TraitMismatchingArguments {
        expected: Vec<Type>,
        found: Vec<Type>,
        function: GlobalStr,
        lang_item: &'static str,
    },
    #[error("Trait lang-item `{lang_item}`'s function `{function}` has a mismatching signature. Expected: fn(...) -> {expected}, but found fn(...) -> {found}")]
    TraitMismatchingReturnType {
        expected: Type,
        found: Type,
        function: GlobalStr,
        lang_item: &'static str,
    },
    #[error("Struct lang-item `{lang_item}`'s function `{function}` has a mismatching signature. Expected: {}, but found {}", FunctionList(.expected), FunctionList(.found))]
    StructMismatchingArguments {
        expected: Vec<Type>,
        found: Vec<Type>,
        function: GlobalStr,
        lang_item: &'static str,
    },
    #[error("Struct lang-item `{lang_item}`'s function `{function}` has a mismatching signature. Expected: fn(...) -> {expected}, but found fn(...) -> {found}")]
    StructMismatchingReturnType {
        expected: Type,
        found: Type,
        function: GlobalStr,
        lang_item: &'static str,
    },
    #[error("Struct lang-item `{lang_item}`s generic `{generic}` doesn't match, expected {}, but found {}", GenericList(.expected), GenericList(.found))]
    StructGenericMismatch {
        lang_item: &'static str,
        generic: GlobalStr,
        expected: Vec<GlobalStr>,
        found: Vec<GlobalStr>,
    },
    #[error("Struct lang-item `{lang_item}` misses generic with bounds {}", GenericList(.generic))]
    StructMissesGeneric {
        lang_item: &'static str,
        generic: Vec<GlobalStr>,
    },
    #[error("Struct lang-item `{lang_item}` has an unexpected generic `{generic}`")]
    StructUnexpectedGeneric {
        lang_item: &'static str,
        generic: GlobalStr,
    },
    #[error("Function lang-item `{lang_item}` has a mismatching signature. Expected: {}, but found {}", FunctionList(.expected), FunctionList(.found))]
    MismatchingArguments {
        expected: Vec<Type>,
        found: Vec<Type>,
        lang_item: &'static str,
    },
    #[error("Function lang-item `{lang_item}` has a mismatching signature. Expected: fn(...) -> {expected}, but found fn(...) -> {found}")]
    MismatchingReturnType {
        expected: Type,
        found: Type,
        lang_item: &'static str,
    },
    #[error("Trait lang-item `{lang_item}` has an unexpected function `{function}`")]
    TraitExcessiveFunction {
        lang_item: &'static str,
        function: GlobalStr,
    },
}

macro_rules! check_langitem {
    (required $self:ident.$lang_item:ident: $ty:ident; $reader:ident $errors:ident $context:ident) => {
        if $self.$lang_item.is_none() {
            $errors.push(LangItemError::MissingItem(
                stringify!($lang_item),
                LangItemType::$ty,
            ));
        }
        check_langitem!($lang_item: $ty; $self $reader $errors $context);
    };
    ($self:ident.$lang_item:ident: $ty:ident; $reader:ident $errors:ident $context:ident) => {
        check_langitem!($lang_item: $ty; $self $reader $errors $context);
    };

    ($lang_item:ident: Trait; $self:ident $reader:ident $errors:ident $context:ident) => {
        if let Some(trait_id) = $self.$lang_item {
            does_trait_match(&$self.$lang_item(), &$reader[trait_id], stringify!($lang_item), $errors);
        }
    };
    ($lang_item:ident: Static; $self:ident $reader:ident $errors:ident $context:ident) => {
        if let Some(static_id) = $self.$lang_item {
            $self.$lang_item(|arr| does_static_match(arr, &$reader[static_id].0, stringify!($lang_item), $errors, $context));
        }
    };
    ($lang_item:ident: Struct; $self:ident $reader:ident $errors:ident $context:ident) => {
        if let Some(struct_id) = $self.$lang_item {
            does_struct_match(&$self.$lang_item(), &$reader[struct_id], stringify!($lang_item), $errors, $context);
        }
    };
    ($lang_item:ident: Function; $self:ident $reader:ident $errors:ident $context:ident) => {
        if let Some(func) = $self.$lang_item {
            let func = match func {
                FunctionLangItem::Internal(id) => &$context.functions.read()[id].0,
                FunctionLangItem::External(id) => &$context.external_functions.read()[id].0,
            };
            does_function_match(&$self.$lang_item(), func, stringify!($lang_item), $errors, $context);
        }
    };
    ($lang_item:ident: $ty: ident; $self:ident $reader:ident $errors:ident) => { compile_error!(concat!(stringify!($ty), " is not yet supported")) };
}

lang_item_def! {
    // native type langitems: struct {}; this solely serves the purpose of having methods on those
    // native types. the function should **not** accept `self: Self`
    str => Struct,
    slice => Struct,
    bool => Struct, // done
    ptr => Struct, // done
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

    printf => Function, // done
}

impl LangItems {
    fn empty_struct() -> LangItemStruct {
        LangItemStruct {
            traits: Vec::new(),
            funcs: Vec::new(),
            fields: Vec::new(),
            generics: Vec::new(),
        }
    }

    fn bool(&self) -> LangItemStruct {
        Self::empty_struct()
    }
    fn f32(&self) -> LangItemStruct {
        Self::empty_struct()
    }
    fn f64(&self) -> LangItemStruct {
        Self::empty_struct()
    }
    fn u8(&self) -> LangItemStruct {
        Self::empty_struct()
    }
    fn i8(&self) -> LangItemStruct {
        Self::empty_struct()
    }
    fn u16(&self) -> LangItemStruct {
        Self::empty_struct()
    }
    fn i16(&self) -> LangItemStruct {
        Self::empty_struct()
    }
    fn u32(&self) -> LangItemStruct {
        Self::empty_struct()
    }
    fn i32(&self) -> LangItemStruct {
        Self::empty_struct()
    }
    fn u64(&self) -> LangItemStruct {
        Self::empty_struct()
    }
    fn i64(&self) -> LangItemStruct {
        Self::empty_struct()
    }
    fn usize(&self) -> LangItemStruct {
        Self::empty_struct()
    }
    fn isize(&self) -> LangItemStruct {
        Self::empty_struct()
    }
    fn ptr(&self) -> LangItemStruct {
        Self::empty_struct()
    }

    fn allocator_trait(&self) -> LangItemTrait {
        // trait Allocator {
        //     fn alloc(self: &Self, size: usize) -> &void;
        //     fn free(self: &Self, ptr: &void);
        //     fn realloc(self: &Self, ptr: &void, new_size: usize) -> &void;
        // }
        LangItemTrait {
            funcs: vec![
                (
                    GlobalStr::new("alloc"),
                    LangItemFunction::new(
                        vec![Type::PrimitiveSelf(1), Type::PrimitiveUSize(0)],
                        Type::PrimitiveVoid(1),
                    ),
                ),
                (
                    GlobalStr::new("free"),
                    LangItemFunction::new(
                        vec![Type::PrimitiveSelf(1), Type::PrimitiveVoid(1)],
                        Type::PrimitiveVoid(0),
                    ),
                ),
                (
                    GlobalStr::new("realloc"),
                    LangItemFunction::new(
                        vec![
                            Type::PrimitiveSelf(1),
                            Type::PrimitiveVoid(1),
                            Type::PrimitiveUSize(0),
                        ],
                        Type::PrimitiveVoid(1),
                    ),
                ),
            ],
        }
    }

    fn allocator<R, F: FnMut(&[TraitId]) -> R>(&self, mut func: F) -> R {
        // let allocator: dyn Allocator;
        // even if there's no allocator trait, throw an error if lang item does not exist.
        if let Some(trait_id) = self.allocator_trait {
            func(&[trait_id])
        } else {
            func(&[])
        }
    }

    fn copy_trait(&self) -> LangItemTrait {
        // trait Copy {}
        LangItemTrait { funcs: Vec::new() }
    }

    fn clone_trait(&self) -> LangItemTrait {
        // trait Clone { fn clone(self: &Self) -> Self; }
        LangItemTrait {
            funcs: vec![(
                GlobalStr::new("clone"),
                LangItemFunction::new(vec![Type::PrimitiveSelf(1)], Type::PrimitiveSelf(0)),
            )],
        }
    }

    fn printf(&self) -> LangItemFunction {
        LangItemFunction {
            return_type: Type::PrimitiveVoid(0),
            // printf(&u8, usize, &u8)
            args: vec![
                Type::PrimitiveU8(1),
                Type::PrimitiveUSize(0),
                Type::PrimitiveU8(1),
            ],
        }
    }

    pub fn check(&self, errors: &mut Vec<LangItemError>, context: &TypecheckingContext) {
        let trait_reader = context.traits.read();
        let struct_reader = context.structs.read();
        let static_reader = context.statics.read();
        let function_reader = context.functions.read();
        let external_function_reader = context.external_functions.read();

        check_langitem!(required self.allocator_trait: Trait; trait_reader errors context);
        check_langitem!(required self.clone_trait: Trait; trait_reader errors context);
        check_langitem!(required self.copy_trait: Trait; trait_reader errors context);
        check_langitem!(required self.allocator: Static; static_reader errors context);
        check_langitem!(required self.printf: Function; static_reader errors context);
        check_langitem!(self.ptr: Struct; struct_reader errors context);
        check_langitem!(self.bool: Struct; struct_reader errors context);
        check_langitem!(self.f32: Struct; struct_reader errors context);
        check_langitem!(self.f64: Struct; struct_reader errors context);
        check_langitem!(self.i8: Struct; struct_reader errors context);
        check_langitem!(self.u8: Struct; struct_reader errors context);
        check_langitem!(self.i16: Struct; struct_reader errors context);
        check_langitem!(self.u16: Struct; struct_reader errors context);
        check_langitem!(self.i32: Struct; struct_reader errors context);
        check_langitem!(self.u32: Struct; struct_reader errors context);
        check_langitem!(self.i64: Struct; struct_reader errors context);
        check_langitem!(self.u64: Struct; struct_reader errors context);
        check_langitem!(self.isize: Struct; struct_reader errors context);
        check_langitem!(self.usize: Struct; struct_reader errors context);
    }
}

fn does_function_match(
    func_a: &LangItemFunction,
    func_b: &TypecheckedFunctionContract,
    lang_item: &'static str,
    errors: &mut Vec<LangItemError>,
    context: &TypecheckingContext,
) -> bool {
    let num_errs = errors.len();

    if func_a.return_type != func_b.return_type {
        errors.push(LangItemError::MismatchingReturnType {
            expected: func_a.return_type.clone(),
            found: func_b.return_type.clone(),
            lang_item,
        });
    }

    if !func_a
        .args
        .iter()
        .zip(func_b.arguments.iter())
        .map(|(a, (_, b))| *a == *b)
        .fold(true, |a, b| a && b)
    {
        errors.push(LangItemError::MismatchingArguments {
            expected: func_a.args.clone(),
            found: func_b.arguments.iter().map(|(_, v)| v.clone()).collect(),
            lang_item,
        });
    }

    errors.len() == num_errs
}

fn does_struct_match(
    structure_a: &LangItemStruct,
    structure_b: &TypedStruct,
    lang_item: &'static str,
    errors: &mut Vec<LangItemError>,
    context: &TypecheckingContext,
) -> bool {
    let num_errs = errors.len();
    let trait_reader = context.traits.read();

    for (i, bounds) in structure_a.generics.iter().enumerate() {
        match structure_b.generics.get(i) {
            Some((name, other_bounds)) => {
                if *bounds != *other_bounds {
                    errors.push(LangItemError::StructGenericMismatch {
                        lang_item,
                        generic: name.clone(),
                        expected: bounds
                            .iter()
                            .map(|v| trait_reader[*v].name.clone())
                            .collect(),
                        found: other_bounds
                            .iter()
                            .map(|v| trait_reader[*v].name.clone())
                            .collect(),
                    })
                }
            }
            None => errors.push(LangItemError::StructMissesGeneric {
                lang_item,
                generic: bounds
                    .iter()
                    .map(|v| trait_reader[*v].name.clone())
                    .collect(),
            }),
        }
    }
    for (generic, ..) in structure_b
        .generics
        .iter()
        .skip(structure_a.generics.len())
        .cloned()
    {
        errors.push(LangItemError::StructUnexpectedGeneric { lang_item, generic })
    }

    for (element_name, element_type) in structure_a.fields.iter() {
        match structure_b
            .elements
            .iter()
            .find(|(v, _)| *v == *element_name)
        {
            Some(v) if v.1 == *element_type => {}
            Some((_, other_type)) => errors.push(LangItemError::StructMismatchingField(
                element_name.clone(),
                element_type.clone(),
                other_type.clone(),
                lang_item,
            )),
            None => errors.push(LangItemError::StructMissingElement(
                element_name.clone(),
                element_type.clone(),
                lang_item,
            )),
        }
    }

    for (element_name, _) in structure_b.elements.iter().filter(|(v, _)| {
        structure_a
            .fields
            .iter()
            .find(|(name, _)| *name == *v)
            .is_none()
    }) {
        errors.push(LangItemError::StructUnexpectedField(
            element_name.clone(),
            lang_item,
        ))
    }

    for trait_id in structure_a.traits.iter().copied() {
        if !structure_b.trait_impl.contains_key(&trait_id) {
            errors.push(LangItemError::StructMissingTrait {
                lang_item,
                trait_name: trait_reader[trait_id].name.clone(),
            })
        }
    }
    drop(trait_reader);
    let func_reader = context.functions.read();

    for (fn_name, func) in structure_a.funcs.iter() {
        let Some(func_impl) = structure_b.global_impl.get(fn_name) else {
            errors.push(LangItemError::StructMissingFunction {
                lang_item,
                function: fn_name.clone(),
            });
            continue;
        };
        let func_impl = &func_reader[*func_impl].0;

        if func_impl.return_type != func.return_type {
            errors.push(LangItemError::StructMismatchingReturnType {
                expected: func.return_type.clone(),
                found: func_impl.return_type.clone(),
                function: fn_name.clone(),
                lang_item,
            });
        }

        if !func
            .args
            .iter()
            .zip(func_impl.arguments.iter())
            .map(|(a, (_, b))| *a == *b)
            .fold(true, |a, b| a && b)
        {
            errors.push(LangItemError::StructMismatchingArguments {
                expected: func.args.clone(),
                found: func_impl.arguments.iter().map(|(_, v)| v.clone()).collect(),
                function: fn_name.clone(),
                lang_item,
            });
        }
    }

    errors.len() == num_errs
}

fn does_static_match(
    traits: &[TraitId],
    typ: &Type,
    lang_item: &'static str,
    errors: &mut Vec<LangItemError>,
    context: &TypecheckingContext,
) -> bool {
    let trait_reader = context.traits.read();
    match typ {
        Type::DynType {
            trait_refs,
            num_references,
        } => {
            let mut matches = true;
            for trait_id in traits.iter().copied() {
                if trait_refs.iter().find(|(v, _)| *v == trait_id).is_none() {
                    matches = false;
                    errors.push(LangItemError::StaticIsMissingTrait(
                        trait_reader[trait_id].name.clone(),
                        lang_item,
                    ));
                }
            }
            matches
        }
        Type::Struct {
            struct_id,
            num_references,
            ..
        } => {
            let struct_traits = &context.structs.read()[*struct_id].trait_impl;
            let mut matches = true;
            for trait_id in traits {
                if !struct_traits.contains_key(trait_id) {
                    matches = false;
                    errors.push(LangItemError::StaticIsMissingTrait(
                        trait_reader[*trait_id].name.clone(),
                        lang_item,
                    ));
                }
            }
            matches
        }
        _ => {
            errors.push(LangItemError::StaticIsPrimitive(lang_item));
            false
        }
    }
}

fn does_trait_match(
    trait_a: &LangItemTrait,
    trait_b: &TypedTrait,
    lang_item: &'static str,
    errors: &mut Vec<LangItemError>,
) -> bool {
    let mut traits_match = false;

    for (func_name, func_a) in trait_a.funcs.iter() {
        let Some((_, func_args_b, func_return_b, ..)) = trait_b
            .functions
            .iter()
            .find(|(name, ..)| func_name == name)
        else {
            traits_match = false;
            errors.push(LangItemError::TraitMissingFunction {
                lang_item,
                function: func_name.clone(),
            });
            continue;
        };

        if func_a.return_type != *func_return_b {
            traits_match = false;
            errors.push(LangItemError::TraitMismatchingReturnType {
                expected: func_a.return_type.clone(),
                found: func_return_b.clone(),
                function: func_name.clone(),
                lang_item,
            });
        }

        if func_a.args.len() != func_args_b.len()
            || !func_a
                .args
                .iter()
                .zip(func_args_b.iter())
                .map(|(a, (_, b))| a == b)
                .fold(true, |a, b| a && b)
        {
            traits_match = false;
            errors.push(LangItemError::TraitMismatchingArguments {
                expected: func_a.args.clone(),
                found: func_args_b.iter().map(|(_, v)| v.clone()).collect(),
                function: func_name.clone(),
                lang_item,
            });
        }
    }

    for (func_name, ..) in trait_b.functions.iter() {
        if trait_a
            .funcs
            .iter()
            .find(|(name, ..)| name == func_name)
            .is_none()
        {
            traits_match = false;
            errors.push(LangItemError::TraitExcessiveFunction {
                lang_item,
                function: func_name.clone(),
            })
        }
    }

    traits_match
}
