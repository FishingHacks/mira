use parking_lot::RwLock;
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    sync::Arc,
};

use crate::{
    Expression, FunctionContract, Generic, LiteralValue, PathWithoutGenerics, Statement, Trait,
    TypeRef, annotations::Annotations, error::ProgramFormingError,
};
use mira_common::store::{AssociatedStore, Store, StoreKey};
use mira_context::{DocComment, SharedCtx};
use mira_spans::{FileId, Ident, SourceFile, Span, Symbol};

mod module_resolution;

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
    pub elements: Vec<(Ident<'arena>, TypeRef<'arena>, DocComment)>,
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
    pub comment: DocComment,
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
#[derive(Debug)]
pub struct Static<'arena> {
    pub ty: TypeRef<'arena>,
    pub value: LiteralValue<'arena>,
    pub module: StoreKey<Module<'arena>>,
    pub span: Span<'arena>,
    pub annotations: Annotations<'arena>,
    pub name: Ident<'arena>,
    pub comment: DocComment,
}

pub type DependencyMap<'arena> = HashMap<Arc<str>, StoreKey<Module<'arena>>>;

pub struct ModuleContext<'arena> {
    pub ctx: SharedCtx<'arena>,
    pub dependencies: AssociatedStore<DependencyMap<'arena>, Module<'arena>>,
    pub modules: RwLock<Store<Module<'arena>>>,
    pub functions: RwLock<Store<Function<'arena>>>,
    pub external_functions: RwLock<Store<ExternalFunction<'arena>>>,
    pub statics: RwLock<Store<Static<'arena>>>, // TODO: const-eval for statics
    pub structs: RwLock<Store<BakedStruct<'arena>>>,
    pub traits: RwLock<Store<Trait<'arena>>>,
}

impl<'arena> ModuleContext<'arena> {
    pub fn new(
        modules: Store<Module<'arena>>,
        ctx: SharedCtx<'arena>,
        dependencies: AssociatedStore<DependencyMap<'arena>, Module<'arena>>,
    ) -> Self {
        Self {
            ctx,
            dependencies,
            modules: modules.into(),
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

pub type Import<'arena> = PathWithoutGenerics<'arena>;
// #[derive(Debug)]
// pub enum Import<'arena> {
//     Unresolved(PathWithoutGenerics<'arena>),
//     Resolved(ModuleScopeValue<'arena>),
// }

pub struct Module<'arena> {
    pub scope: HashMap<Ident<'arena>, ModuleScopeValue<'arena>>,
    pub imports: HashMap<Ident<'arena>, (Span<'arena>, Import<'arena>)>,
    pub exports: HashSet<Ident<'arena>>,
    pub file: Arc<SourceFile>,
    pub package_root: StoreKey<Module<'arena>>,
    pub name: Symbol<'arena>,
    /// if this parent is undefined, that means it's the root package.
    pub parent: StoreKey<Module<'arena>>,
    pub assembly: Vec<(Span<'arena>, String)>,
    pub comment: DocComment,
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

#[derive(Clone, Debug)]
pub struct ParserQueueEntry<'arena> {
    pub file: Arc<std::path::Path>,
    pub file_root: Arc<std::path::Path>,
    pub loaded_file: Option<FileId>,
    pub name: Symbol<'arena>,
    pub package_root: StoreKey<Module<'arena>>,
    /// if this parent is the same as the package_root, it means this module is the package root.
    pub parent: StoreKey<Module<'arena>>,
    pub module_key: StoreKey<Module<'arena>>,
    pub comment: Option<DocComment>,
}

impl<'arena> Module<'arena> {
    pub fn new(
        file: Arc<SourceFile>,
        package_root: StoreKey<Self>,
        parent: StoreKey<Self>,
        name: Symbol<'arena>,
        comment: DocComment,
    ) -> Self {
        Self {
            imports: HashMap::new(),
            exports: HashSet::new(),
            file,
            scope: HashMap::new(),
            assembly: Vec::new(),
            parent,
            package_root,
            name,
            comment,
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
        parser_queue: &RwLock<Vec<ParserQueueEntry<'arena>>>,
        modules: &RwLock<Store<Module<'arena>>>,
    ) -> Result<(), Vec<ProgramFormingError<'arena>>> {
        let errors = statements
            .into_iter()
            .map(|statement| {
                self.push_statement(statement, module_id, context, parser_queue, modules)
            })
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

    fn maybe_add_export(&mut self, public: bool, name: Ident<'arena>) {
        if public {
            debug_assert!(self.exports.insert(name));
        }
    }

    pub fn push_statement(
        &mut self,
        statement: Statement<'arena>,
        module_id: StoreKey<Module<'arena>>,
        context: &ModuleContext<'arena>,
        parser_queue: &RwLock<Vec<ParserQueueEntry<'arena>>>,
        modules: &RwLock<Store<Module<'arena>>>,
    ) -> Result<(), ProgramFormingError<'arena>> {
        match statement {
            Statement::Function {
                contract,
                body,
                public,
                ..
            } => {
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
                self.maybe_add_export(public, name);
            }
            Statement::Trait(mut r#trait) => {
                let public = r#trait.public;
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
                self.maybe_add_export(public, name);
            }
            Statement::Struct {
                name,
                elements,
                span,
                global_impl,
                impls,
                annotations,
                generics,
                public,
                comment,
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
                    comment,
                };

                let mut writer = context.structs.write();
                let key = writer.insert(baked_struct);
                self.scope.insert(name, ModuleScopeValue::Struct(key));
                self.maybe_add_export(public, name);
            }
            Statement::Static {
                var,
                public,
                comment,
            } => {
                if self.is_defined(&var.name) {
                    return Err(ProgramFormingError::IdentAlreadyDefined(
                        var.span,
                        var.name.symbol(),
                    ));
                }

                let Expression::Literal(value, _) = var.value else {
                    return Err(ProgramFormingError::GlobalValueNoLiteral(var.value.span()));
                };
                let mut writer = context.statics.write();
                let key = writer.insert(Static {
                    ty: var.ty.expect("static needs to have a type"),
                    value,
                    module: module_id,
                    span: var.span,
                    annotations: var.annotations,
                    name: var.name,
                    comment,
                });
                self.scope.insert(var.name, ModuleScopeValue::Static(key));
                self.maybe_add_export(public, var.name);
            }
            Statement::ExternalFunction {
                contract,
                mut body,
                public,
                ..
            } => {
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
                self.maybe_add_export(public, name);
            }
            Statement::ModuleAsm(span, strn) => self.assembly.push((span, strn)),
            Statement::Use {
                path,
                alias,
                public,
                span,
            } => {
                let name = alias.unwrap_or(*path.entries.last().unwrap());
                self.imports.insert(name, (span, path));
                self.maybe_add_export(public, name);
            }
            Statement::Mod {
                public,
                name,
                span,
                comment,
            } => {
                if self.is_defined(&name) {
                    return Err(ProgramFormingError::IdentAlreadyDefined(
                        span,
                        name.symbol(),
                    ));
                }
                let file = module_resolution::resolve_module(
                    context.ctx.span_interner,
                    name,
                    self,
                    context.ctx.source_map,
                    span,
                    span.last(context.ctx.span_interner),
                )?;
                let module_key = modules.write().reserve_key();
                parser_queue.write().push(ParserQueueEntry {
                    file,
                    file_root: self.file.package_root.clone(),
                    module_key,
                    name: name.symbol(),
                    loaded_file: None,
                    parent: module_id,
                    package_root: self.package_root,
                    comment,
                });
                self.scope
                    .insert(name, ModuleScopeValue::Module(module_key));
                self.maybe_add_export(public, name);
            }
            _ => {
                return Err(ProgramFormingError::NoCodeOutsideOfFunctions(
                    statement.span(),
                ));
            }
        }

        Ok(())
    }
}
