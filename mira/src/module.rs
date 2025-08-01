use parking_lot::RwLock;
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    path::Path,
    sync::Arc,
};

use crate::{
    annotations::Annotations,
    context::SharedContext,
    error::ProgramFormingError,
    parser::{
        Expression, FunctionContract, Generic, LiteralValue, PathWithoutGenerics, Statement, Trait,
        TypeRef,
    },
    store::{Store, StoreKey},
};
use mira_spans::{Ident, PackageId, Span};

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum ModuleScopeValue<'arena> {
    Function(StoreKey<Function<'arena>>),
    ExternalFunction(StoreKey<ExternalFunction<'arena>>),
    Struct(StoreKey<BakedStruct<'arena>>),
    Static(StoreKey<Static<'arena>>),
    Module(StoreKey<Module<'arena>>),
    Trait(StoreKey<Trait<'arena>>),
}

#[derive(Debug)]
pub struct BakedStruct<'arena> {
    pub name: Ident<'arena>,
    pub elements: Vec<(Ident<'arena>, TypeRef<'arena>)>,
    pub span: Span<'arena>,
    pub global_impl: HashMap<Ident<'arena>, StoreKey<Function<'arena>>>,
    pub impls: Vec<(
        Ident<'arena>,
        HashMap<Ident<'arena>, StoreKey<Function<'arena>>>,
        Span<'arena>,
    )>,
    pub annotations: Annotations<'arena>,
    pub module_id: StoreKey<Module<'arena>>,
    pub generics: Vec<Generic<'arena>>,
}

pub type Function<'arena> = (
    FunctionContract<'arena>,
    Statement<'arena>,
    StoreKey<Module<'arena>>,
);
pub type ExternalFunction<'arena> = (
    FunctionContract<'arena>,
    Option<Statement<'arena>>,
    StoreKey<Module<'arena>>,
);
pub type Static<'arena> = (
    TypeRef<'arena>,
    LiteralValue<'arena>,
    StoreKey<Module<'arena>>,
    Span<'arena>,
    Annotations<'arena>,
);

pub struct ModuleContext<'arena> {
    pub ctx: SharedContext<'arena>,
    pub modules: RwLock<Store<Module<'arena>>>,
    pub packages: HashMap<PackageId, StoreKey<Module<'arena>>>,
    pub functions: RwLock<Store<Function<'arena>>>,
    pub external_functions: RwLock<Store<ExternalFunction<'arena>>>,
    pub statics: RwLock<Store<Static<'arena>>>, // TODO: const-eval for statics
    pub structs: RwLock<Store<BakedStruct<'arena>>>,
    pub traits: RwLock<Store<Trait<'arena>>>,
}

impl<'arena> ModuleContext<'arena> {
    pub fn new(
        packages: HashMap<PackageId, StoreKey<Module<'arena>>>,
        modules: Store<Module<'arena>>,
        ctx: SharedContext<'arena>,
    ) -> Self {
        Self {
            ctx,
            modules: modules.into(),
            packages,
            functions: Default::default(),
            external_functions: Default::default(),
            statics: Default::default(),
            structs: Default::default(),
            traits: Default::default(),
        }
    }
}

impl Debug for ModuleContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ModuleContext")
            .field("functions", &self.functions.read())
            .field("external_functions", &self.external_functions.read())
            .field("statics", &self.statics.read())
            .field("structs", &self.structs.read())
            .finish()
    }
}

#[derive(Debug)]
pub enum Import<'arena> {
    Unresolved(PathWithoutGenerics<'arena>),
    Resolved(ModuleScopeValue<'arena>),
}

pub struct Module<'arena> {
    pub scope: HashMap<Ident<'arena>, ModuleScopeValue<'arena>>,
    pub imports: HashMap<Ident<'arena>, (Span<'arena>, Import<'arena>)>,
    pub exports: HashSet<Ident<'arena>>,
    pub package: PackageId,
    pub path: Arc<Path>,
    pub root: Arc<Path>,
    pub assembly: Vec<(Span<'arena>, String)>,
}

impl Debug for Module<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Module")
            .field("scope", &self.scope)
            .field("imports", &self.imports)
            .field("exports", &self.exports)
            .finish()
    }
}

impl<'arena> Module<'arena> {
    pub fn new(
        imports: HashMap<Ident<'arena>, (Span<'arena>, Import<'arena>)>,
        exports: HashSet<Ident<'arena>>,
        package: PackageId,
        path: Arc<Path>,
        root: Arc<Path>,
    ) -> Self {
        Self {
            imports,
            exports,
            path,
            root,
            package,
            scope: HashMap::new(),
            assembly: Vec::new(),
        }
    }

    pub fn push_fn(
        &mut self,
        contract: FunctionContract<'arena>,
        mut body: Statement<'arena>,
        module: StoreKey<Module<'arena>>,
        context: &ModuleContext<'arena>,
    ) -> StoreKey<Function<'arena>> {
        let key = context.functions.write().reserve_key();
        body.bake_functions(self, module, context);
        context
            .functions
            .write()
            .insert_reserved(key, (contract, body, module));

        key
    }

    pub fn push_all(
        &mut self,
        statements: Vec<Statement<'arena>>,
        module_id: StoreKey<Module<'arena>>,
        context: &ModuleContext<'arena>,
    ) -> Result<(), Vec<ProgramFormingError<'arena>>> {
        let errors = statements
            .into_iter()
            .map(|statement| self.push_statement(statement, module_id, context))
            .filter_map(|el| el.err())
            .collect::<Vec<_>>();

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn is_defined(&self, name: &Ident<'arena>) -> bool {
        self.scope.contains_key(name) || self.imports.contains_key(name)
    }

    pub fn push_statement(
        &mut self,
        statement: Statement<'arena>,
        module_id: StoreKey<Module<'arena>>,
        context: &ModuleContext<'arena>,
    ) -> Result<(), ProgramFormingError<'arena>> {
        match statement {
            Statement::Function(contract, body, _) => {
                let Some(name) = contract.name else {
                    return Err(ProgramFormingError::AnonymousFunctionAtGlobalLevel(
                        contract.span,
                    ));
                };

                if self.is_defined(&name) {
                    return Err(ProgramFormingError::IdentAlreadyDefined(
                        contract.span,
                        name.symbol(),
                    ));
                }

                let fn_id = self.push_fn(contract, *body, module_id, context);
                self.scope.insert(name, ModuleScopeValue::Function(fn_id));
            }
            Statement::Trait(mut r#trait) => {
                r#trait.module = module_id;
                if self.is_defined(&r#trait.name) {
                    return Err(ProgramFormingError::IdentAlreadyDefined(
                        r#trait.span,
                        r#trait.name.symbol(),
                    ));
                }

                let mut writer = context.traits.write();
                let name = r#trait.name;
                let key = writer.insert(r#trait);
                self.scope.insert(name, ModuleScopeValue::Trait(key));
            }
            Statement::Struct {
                name,
                elements,
                span,
                global_impl,
                impls,
                annotations,
                generics,
            } => {
                if self.is_defined(&name) {
                    return Err(ProgramFormingError::IdentAlreadyDefined(
                        span,
                        name.symbol(),
                    ));
                }

                let mut baked_global_impl = HashMap::new();
                let mut baked_impls = Vec::new();

                for (name, (contract, body)) in global_impl {
                    baked_global_impl
                        .insert(name, self.push_fn(contract, body, module_id, context));
                }

                for (trait_name, implementation, span) in impls {
                    let mut baked_impl = HashMap::new();
                    for (name, (contract, body)) in implementation {
                        baked_impl.insert(name, self.push_fn(contract, body, module_id, context));
                    }
                    baked_impls.push((trait_name, baked_impl, span));
                }

                let baked_struct = BakedStruct {
                    name,
                    elements,
                    annotations,
                    span,
                    global_impl: baked_global_impl,
                    impls: baked_impls,
                    module_id,
                    generics,
                };

                let mut writer = context.structs.write();
                let key = writer.insert(baked_struct);
                self.scope.insert(name, ModuleScopeValue::Struct(key));
            }
            Statement::Var(_, _, None, span, _) => {
                return Err(ProgramFormingError::GlobalValueNoType(span))
            }
            Statement::Var(name, expr, Some(typ), span, annotations) => {
                if self.is_defined(&name) {
                    return Err(ProgramFormingError::IdentAlreadyDefined(
                        span,
                        name.symbol(),
                    ));
                }

                let Expression::Literal(value, _) = expr else {
                    return Err(ProgramFormingError::GlobalValueNoLiteral(expr.span()));
                };
                let mut writer = context.statics.write();
                let key = writer.insert((typ, value, module_id, span, annotations));
                self.scope.insert(name, ModuleScopeValue::Static(key));
            }
            Statement::ExternalFunction(contract, mut body, _) => {
                let Some(name) = contract.name else {
                    return Err(ProgramFormingError::AnonymousFunctionAtGlobalLevel(
                        contract.span,
                    ));
                };

                if self.is_defined(&name) {
                    return Err(ProgramFormingError::IdentAlreadyDefined(
                        contract.span,
                        name.symbol(),
                    ));
                }

                if let Some(ref mut body) = body {
                    body.bake_functions(self, module_id, context);
                }
                let mut writer = context.external_functions.write();
                let key = writer.insert((contract, body.map(|v| *v), module_id));
                self.scope
                    .insert(name, ModuleScopeValue::ExternalFunction(key));
            }
            Statement::ModuleAsm(span, strn) => self.assembly.push((span, strn)),
            // these are handled by the parser itself
            Statement::Use { .. } | Statement::Mod { .. } => {}
            _ => {
                return Err(ProgramFormingError::NoCodeOutsideOfFunctions(
                    statement.span(),
                ))
            }
        }

        Ok(())
    }
}
