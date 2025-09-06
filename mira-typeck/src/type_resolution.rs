use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use mira_spans::ArenaList;

use mira_common::store::StoreKey;
use mira_parser::{
    Trait, TypeRef,
    module::{BakedStruct, ExternalFunction, Function, ModuleContext, ModuleScopeValue, Static},
    std_annotations::lang_item::LangItemAnnotation,
};

use crate::error::TypecheckingErrorEmitterExt;

use super::{
    TypeckCtx, TypedFunctionContract, TypedGeneric, TypedStatic, TypedTrait,
    ir::TypedLiteral,
    resolve_import,
    types::{TyKind, default_types, with_refcount},
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum ResolvingState {
    Working,
    Pending,
}

impl<'arena> TypeckCtx<'arena> {
    /// Resolves the types; This should be ran *after* [Self::resolve_imports]
    ///
    /// NOTE: this could potentially deadlock as i write-lock the rwlocks in the context a bunch
    /// and don't free them until fully resolving a value. This module is designed in a way that
    /// such a deadlock should never happen but.. uhh... :3c :3
    /// meow
    pub fn resolve_types(&self, context: Arc<ModuleContext<'arena>>) {
        let mut lang_items_writer = self.lang_items.write();
        let lang_items_writer = &mut *lang_items_writer;

        let mut structs_left = context
            .structs
            .read()
            .indices()
            .zip(std::iter::repeat(ResolvingState::Pending))
            .collect::<HashMap<_, _>>();

        // ┌─────────┐
        // │ Structs │
        // └─────────┘
        while let Some(key) = structs_left.keys().next().copied() {
            let struct_reader = context.structs.read();
            let module_id = struct_reader[key].module_id;
            drop(struct_reader);
            let tracker = self.ctx.track_errors();
            assert!(
                !self.resolve_struct(context.clone(), key, module_id, &mut structs_left),
                "this came from no field, so this shouldn't be recursive"
            );
            if self.ctx.errors_happened(tracker) {
                continue;
            }
            let struct_reader = self.structs.read();
            for annotation in struct_reader[key.cast()]
                .annotations
                .get_annotations::<LangItemAnnotation>()
            {
                if let Err(e) = lang_items_writer.push_struct(
                    key.cast(),
                    annotation.get_langitem(),
                    struct_reader[key.cast()].span,
                ) {
                    self.ctx.emit_diag(e);
                }
            }
        }

        // ┌───────────┐
        // │ Functions │
        // └───────────┘
        let function_keys = context.functions.read().indices().collect::<Vec<_>>();
        for function_key in function_keys {
            let tracker = self.ctx.track_errors();
            self.resolve_function(function_key, &context);
            if self.ctx.errors_happened(tracker) {
                continue;
            }
            let function_reader = self.functions.read();
            for annotation in function_reader[function_key.cast()]
                .0
                .annotations
                .get_annotations::<LangItemAnnotation>()
            {
                if let Err(e) = lang_items_writer.push_function(
                    function_key.cast(),
                    annotation.get_langitem(),
                    function_reader[function_key.cast()].0.span,
                ) {
                    self.ctx.emit_diag(e);
                }
            }
        }

        // ┌────────────────────┐
        // │ External Functions │
        // └────────────────────┘
        let ext_function_keys = context
            .external_functions
            .read()
            .indices()
            .collect::<Vec<_>>();
        for ext_function_key in ext_function_keys {
            let tracker = self.ctx.track_errors();
            self.resolve_ext_function(ext_function_key, &context);
            if self.ctx.errors_happened(tracker) {
                continue;
            }
            let ext_function_reader = self.external_functions.read();
            for annotation in ext_function_reader[ext_function_key.cast()]
                .0
                .annotations
                .get_annotations::<LangItemAnnotation>()
            {
                if let Err(e) = lang_items_writer.push_external_function(
                    ext_function_key.cast(),
                    annotation.get_langitem(),
                    ext_function_reader[ext_function_key.cast()].0.span,
                ) {
                    self.ctx.emit_diag(e);
                }
            }
        }

        // ┌─────────┐
        // │ Statics │
        // └─────────┘
        let statics_keys = context.statics.read().indices().collect::<Vec<_>>();
        for static_key in statics_keys {
            let tracker = self.ctx.track_errors();
            self.resolve_static(static_key, &context);
            if self.ctx.errors_happened(tracker) {
                continue;
            }
            let static_reader = self.statics.read();
            for annotation in static_reader[static_key.cast()]
                .annotations
                .get_annotations::<LangItemAnnotation>()
            {
                if let Err(e) = lang_items_writer.push_static(
                    static_key.cast(),
                    annotation.get_langitem(),
                    static_reader[static_key.cast()].span,
                ) {
                    self.ctx.emit_diag(e);
                }
            }
        }

        // ┌────────┐
        // │ Traits │
        // └────────┘
        let trait_keys = context.traits.read().indices().collect::<Vec<_>>();
        for trait_key in trait_keys {
            let tracker = self.ctx.track_errors();
            self.resolve_trait(trait_key, &context);
            if self.ctx.errors_happened(tracker) {
                continue;
            }
            let trait_reader = self.traits.read();
            for annotation in trait_reader[trait_key.cast()]
                .annotations
                .get_annotations::<LangItemAnnotation>()
            {
                if let Err(e) = lang_items_writer.push_trait(
                    trait_key.cast(),
                    annotation.get_langitem(),
                    trait_reader[trait_key.cast()].span,
                ) {
                    self.ctx.emit_diag(e);
                }
            }
        }

        let struct_keys = context.structs.read().indices().collect::<Vec<_>>();
        for struct_id in struct_keys {
            self.resolve_struct_impls(struct_id, &context);
        }

        lang_items_writer.check(self);
    }

    fn resolve_struct_impls(
        &self,
        struct_key: StoreKey<BakedStruct<'arena>>,
        context: &ModuleContext<'arena>,
    ) {
        let mut writer = context.structs.write();
        let trait_impl = std::mem::take(&mut writer[struct_key].impls);
        drop(writer);

        // it is *okay* to hold the lock here between resolve_type calls as all structs have
        // already been resolved.
        let mut struct_writer = self.structs.write();
        let trait_reader = self.traits.read();
        let function_reader = self.functions.read();
        let module = struct_writer[struct_key.cast()].module_id;

        for (name, implementation, span) in trait_impl {
            let trait_id =
                match resolve_import(context, module.cast(), &[name], span, &mut HashSet::new()) {
                    Err(e) => {
                        self.ctx.emit_diag(e);
                        continue;
                    }
                    Ok(ModuleScopeValue::Trait(trait_id)) => trait_id,
                    Ok(_) => {
                        self.ctx.emit_unbound_ident(span, name.symbol());
                        continue;
                    }
                };

            let typed_trait = &trait_reader[trait_id.cast()];
            if typed_trait.functions.len() != implementation.len() {
                for (name, func_id) in &implementation {
                    if !typed_trait.functions.iter().any(|(v, ..)| v == name) {
                        self.ctx.emit_is_not_trait_member(
                            function_reader[func_id.cast()].0.span,
                            name.symbol(),
                        );
                    }
                }
            }

            let mut trait_impl = Vec::new();
            for (name, args, return_type, ..) in &typed_trait.functions {
                let Some(&func_id) = implementation.get(name) else {
                    self.ctx.emit_missing_trait_item(span, name.symbol());
                    continue;
                };

                let mut with_errs = false;

                let function_contract = &function_reader[func_id.cast()].0;
                if !function_contract
                    .arguments
                    .iter()
                    .zip(args)
                    .all(|((_, typ_a), (_, typ_b))| *typ_a == *typ_b)
                {
                    let expected = args.iter().map(|(_, v)| *v).collect::<Vec<_>>();
                    let found = function_contract
                        .arguments
                        .iter()
                        .map(|(_, v)| *v)
                        .collect::<Vec<_>>();
                    self.ctx
                        .emit_mismatching_arguments(function_contract.span, expected, found);
                    with_errs = true;
                }

                if *return_type != function_contract.return_type {
                    self.ctx.emit_mismatching_return_type(
                        function_contract.span,
                        *return_type,
                        function_contract.return_type,
                    );
                    with_errs = true
                }
                if !with_errs {
                    trait_impl.push(func_id.cast());
                }
            }
            if trait_impl.len() != typed_trait.functions.len() {
                continue;
            }
            struct_writer[struct_key.cast()]
                .trait_impl
                .insert(trait_id.cast(), trait_impl);
        }
        drop(struct_writer);
        drop(function_reader);
        drop(trait_reader);
        let struct_reader = self.structs.read();
        let mut function_writer = self.functions.write();

        for fn_id in struct_reader[struct_key.cast()]
            .global_impl
            .values()
            .copied()
            .chain(
                struct_reader[struct_key.cast()]
                    .trait_impl
                    .values()
                    .flat_map(|v| v.iter())
                    .copied(),
            )
        {
            let self_ty = self.ctx.intern_ty(TyKind::Struct {
                struct_id: struct_key.cast(),
                name: struct_reader[struct_key.cast()].name,
            });

            let contract = &mut function_writer[fn_id].0;
            let (refcount, ty) = contract.return_type.remove_refs();
            if ty == default_types::self_ {
                contract.return_type = with_refcount(self.ctx, self_ty, refcount);
            }
            for t in contract.arguments.iter_mut() {
                let (refcount, ty) = t.1.remove_refs();
                if ty == default_types::self_ {
                    t.1 = with_refcount(self.ctx, self_ty, refcount);
                }
            }
        }
    }

    fn resolve_function(
        &self,
        function_id: StoreKey<Function<'arena>>,
        context: &ModuleContext<'arena>,
    ) {
        let mut writer = context.functions.write();
        let module_id = writer[function_id].2;
        let arguments = std::mem::take(&mut writer[function_id].0.arguments);
        let span = writer[function_id].0.return_type.span();
        let return_type = std::mem::replace(
            &mut writer[function_id].0.return_type,
            TypeRef::Void(span, 0),
        );
        let untyped_generics = std::mem::take(&mut writer[function_id].0.generics);
        let mut generics = Vec::with_capacity(untyped_generics.len());
        for generic in untyped_generics {
            let mut bounds = Vec::with_capacity(generic.bounds.len());
            for mut bound in generic.bounds {
                match resolve_import(
                    context,
                    module_id,
                    bound.0.as_slice(),
                    bound.1,
                    &mut HashSet::new(),
                ) {
                    Ok(ModuleScopeValue::Trait(v)) => bounds.push(v.cast()),
                    Ok(_) => {
                        self.ctx.emit_unbound_ident(
                            bound.1,
                            bound
                                .0
                                .pop()
                                .expect("a path has to have at least one element")
                                .symbol(),
                        );
                    }
                    Err(e) => _ = self.ctx.emit_diag(e),
                }
            }
            generics.push(TypedGeneric {
                name: generic.name,
                sized: generic.sized,
                bounds: ArenaList::new(self.ctx.arena(), &bounds),
            });
        }
        let mut resolved_function_contract = TypedFunctionContract {
            module_id: module_id.cast(),
            name: writer[function_id].0.name,
            span: writer[function_id].0.span,
            annotations: std::mem::take(&mut writer[function_id].0.annotations),
            arguments: Vec::new(),
            return_type: default_types::never,
            generics,
            comment: writer[function_id].0.comment,
        };
        drop(writer);

        let error_tracker = self.ctx.track_errors();
        match self.resolve_type(
            module_id.cast(),
            &return_type,
            &resolved_function_contract.generics,
        ) {
            Ok(v) => resolved_function_contract.return_type = v,
            Err(e) => _ = self.ctx.emit_diag(e),
        }

        for arg in arguments {
            match self.resolve_type(
                module_id.cast(),
                &arg.ty,
                &resolved_function_contract.generics,
            ) {
                Ok(v) => resolved_function_contract.arguments.push((arg.name, v)),
                Err(e) => _ = self.ctx.emit_diag(e),
            }
        }

        if !self.ctx.errors_happened(error_tracker) {
            self.functions.write()[function_id.cast()].0 = resolved_function_contract;
        }
    }

    fn resolve_ext_function(
        &self,
        ext_function_id: StoreKey<ExternalFunction<'arena>>,
        context: &ModuleContext<'arena>,
    ) {
        let mut writer = context.external_functions.write();
        let comment = writer[ext_function_id].0.comment;
        let module_id = writer[ext_function_id].2;
        let arguments = std::mem::take(&mut writer[ext_function_id].0.arguments);
        let span = writer[ext_function_id].0.return_type.span();
        let return_type = std::mem::replace(
            &mut writer[ext_function_id].0.return_type,
            TypeRef::Void(span, 0),
        );
        let mut resolved_function_contract = TypedFunctionContract {
            module_id: module_id.cast(),
            name: writer[ext_function_id].0.name,
            span: writer[ext_function_id].0.span,
            annotations: std::mem::take(&mut writer[ext_function_id].0.annotations),
            arguments: Vec::new(),
            return_type: default_types::never,
            generics: Vec::new(),
            comment,
        };
        drop(writer);

        let tracker = self.ctx.track_errors();
        match self.resolve_type(module_id.cast(), &return_type, &[]) {
            Ok(v) => resolved_function_contract.return_type = v,
            Err(e) => _ = self.ctx.emit_diag(e),
        }

        for arg in arguments {
            match self.resolve_type(module_id.cast(), &arg.ty, &[]) {
                Ok(v) => resolved_function_contract.arguments.push((arg.name, v)),
                Err(e) => _ = self.ctx.emit_diag(e),
            }
        }

        if !self.ctx.errors_happened(tracker) {
            self.external_functions.write()[ext_function_id.cast()] =
                (resolved_function_contract, None);
        }
    }

    fn resolve_static(&self, static_id: StoreKey<Static<'arena>>, context: &ModuleContext<'arena>) {
        let mut writer = context.statics.write();
        let span = writer[static_id].span;
        let name = writer[static_id].name;
        let annotations = std::mem::take(&mut writer[static_id].annotations);
        let dummy_type = TypeRef::Void(writer[static_id].ty.span(), 0);
        let ty = std::mem::replace(&mut writer[static_id].ty, dummy_type);
        let module_id = writer[static_id].module;
        let comment = writer[static_id].comment;
        drop(writer);
        match self.resolve_type(module_id.cast(), &ty, &[]) {
            Ok(v) => {
                self.statics.write()[static_id.cast()] = TypedStatic::new(
                    v,
                    TypedLiteral::Void,
                    module_id.cast(),
                    span,
                    annotations,
                    name,
                    comment,
                );
            }
            Err(e) => _ = self.ctx.emit_diag(e),
        }
    }

    fn resolve_trait(&self, trait_id: StoreKey<Trait<'arena>>, context: &ModuleContext<'arena>) {
        let mut writer = context.traits.write();
        let span = writer[trait_id].span;
        let name = writer[trait_id].name;
        let annotations = std::mem::take(&mut writer[trait_id].annotations);
        let functions = std::mem::take(&mut writer[trait_id].functions);
        let module_id = writer[trait_id].module;
        let comment = writer[trait_id].comment;
        drop(writer);

        let mut typed_functions = Vec::new();
        let tracker = self.ctx.track_errors();

        for func in functions {
            let typed_return_type = match self.resolve_type(module_id.cast(), &func.return_ty, &[])
            {
                Ok(v) => v,
                Err(e) => {
                    self.ctx.emit_diag(e);
                    continue;
                }
            };

            let mut typed_arguments = Vec::new();

            for arg in &func.args {
                match self.resolve_type(module_id.cast(), &arg.ty, &[]) {
                    Ok(v) => typed_arguments.push((arg.name, v)),
                    Err(e) => _ = self.ctx.emit_diag(e),
                }
            }

            typed_functions.push((
                func.name,
                typed_arguments,
                typed_return_type,
                func.annotations,
                func.span,
                func.comment,
            ));
        }

        if !self.ctx.errors_happened(tracker) {
            self.traits.write()[trait_id.cast()] = TypedTrait {
                name,
                span,
                id: trait_id.cast(),
                module_id: module_id.cast(),
                annotations,
                functions: typed_functions,
                comment,
            };
        }
    }
}
