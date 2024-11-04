use std::{
    collections::HashMap,
    fmt::{Debug, Display, Write},
    rc::Rc,
    sync::{Arc, RwLock},
};

use type_resolution::{ResolvedStruct, Type};

use crate::{
    globals::GlobalStr,
    parser::{Annotations, LiteralValue, Statement},
    tokenizer::Location,
};

pub use module_resolution::resolve_modules;

mod module_resolution;
mod type_resolution;
mod typecheck;
pub type TypecheckedModules = Arc<RwLock<Vec<TypecheckedModule>>>;

#[derive(Clone)]
pub enum ProgrammingLangTypecheckingError {
    /// An unsized type was found in the struct in a field that is not the last one
    UnsizedTypeInsideStruct {
        loc: Location,
        field_name: GlobalStr,
    },
    UnboundType {
        loc: Location,
        type_name: GlobalStr,
    },
    UnboundExport {
        loc: Location,
        name: GlobalStr,
    },
}

impl Debug for ProgrammingLangTypecheckingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnsizedTypeInsideStruct { loc, field_name } => f.write_fmt(format_args!("{loc}: Unsized types are only valid as the last field of a struct (field `{field_name}` is unsized)")),
            Self::UnboundType { loc, type_name } => f.write_fmt(format_args!("{loc}: Unbound or recursive type `{type_name}`")),
            Self::UnboundExport { loc, name } => f.write_fmt(format_args!("{loc}: No export named `{name}` found.")),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ModuleId(pub usize);

#[derive(Debug, Default)]
pub struct TypecheckingContext {
    functions: RwLock<Vec<(ResolvedFunctionContract, RwLock<Statement>)>>,
    external_functions: RwLock<Vec<ResolvedFunctionContract>>,
    statics: RwLock<Vec<(Type, LiteralValue, ModuleId)>>,
    structs: RwLock<Vec<Rc<ResolvedStruct>>>,
}

#[derive(Debug, Clone, Copy)]
pub enum ScopeItem {
    Function(usize),
    ExternalFunction(usize),
    StaticValue(usize),
    Struct(usize),
}

#[derive(Debug, Clone, Copy)]
pub enum ScopeItemKind {
    Function,
    StaticValue,
    Type,
}

impl Into<ScopeItemKind> for &ScopeItem {
    fn into(self) -> ScopeItemKind {
        match self {
            ScopeItem::Struct(_) => ScopeItemKind::Type,
            ScopeItem::Function(_) | ScopeItem::ExternalFunction(_) => ScopeItemKind::Function,
            ScopeItem::StaticValue(_) => ScopeItemKind::StaticValue,
        }
    }
}

impl Display for ScopeItemKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function => f.write_str("function"),
            Self::StaticValue => f.write_str("static"),
            Self::Type => f.write_str("type"),
        }
    }
}

/// TODO: add traits (and traitbounds)
#[derive(Debug)]
struct ResolvedFunctionContract {
    name: Option<GlobalStr>,
    arguments: Box<[(GlobalStr, Type)]>,
    return_type: Type,
    location: Location,
    annotations: Annotations,
    generics: Box<[GlobalStr]>,
    module_id: ModuleId,
}

pub struct TypecheckedModule {
    /// vec of (name, module id, path)
    pub scope: HashMap<GlobalStr, ScopeItem>,
    pub context: Arc<TypecheckingContext>,
    pub modules: TypecheckedModules,
}

impl Debug for TypecheckedModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TypecheckedModule")
            .field("scope", &self.scope)
            .finish()
    }
}
