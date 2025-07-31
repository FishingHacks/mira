use std::{collections::HashMap, sync::Arc};

use mira_errors::Diagnostics;

use crate::{
    lang_items::LangItemAnnotation,
    module::{BakedStruct, ExternalFunction, Function, ModuleContext, ModuleScopeValue, Static},
    parser::{Trait, TypeRef},
    store::StoreKey,
};

use super::{
    expression::TypedLiteral, resolve_import, types::Type, TypecheckedFunctionContract,
    TypecheckingContext, TypecheckingErrorDiagnosticsExt, TypedGeneric, TypedStatic, TypedTrait,
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum ResolvingState {
    Working,
    Pending,
}

impl<'arena> TypecheckingContext<'arena> {
    /// Resolves the types; This should be ran *after* [Self::resolve_imports]
    ///
    /// NOTE: this could potentially deadlock as i write-lock the rwlocks in the context a bunch
    /// and don't free them until fully resolving a value. This module is designed in a way that
    /// such a deadlock should never happen but.. uhh... :3c :3
    /// meow
    pub fn resolve_types<'a>(&'a self, context: Arc<ModuleContext<'arena>>) -> Diagnostics<'arena> {
        let mut errors = Diagnostics::new();

        let mut lang_items_writer = self.lang_items.write();
        let lang_items_writer = &mut *lang_items_writer;

        let mut structs_left = context
            .structs
            .read()
            .indices()
            .zip(std::iter::repeat(ResolvingState::Pending))
            .collect::<HashMap<_, _>>();

        // +---------+
        // | Structs |
        // +---------+
        while let Some(key) = structs_left.keys().next().copied() {
            let struct_reader = context.structs.read();
            let module_id = struct_reader[key].module_id;
            drop(struct_reader);
            let err_count = errors.len();
            assert!(
                !self.resolve_struct(
                    context.clone(),
                    key,
                    module_id,
                    &mut errors,
                    &mut structs_left
                ),
                "this came from no field, so this shouldn't be recursive"
            );
            if err_count != errors.len() {
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
                    errors.add(e);
                }
            }
        }

        // +-----------+
        // | Functions |
        // +-----------+
        let function_keys = context.functions.read().indices().collect::<Vec<_>>();
        for function_key in function_keys {
            let error_count = errors.len();
            self.resolve_function(function_key, &context, &mut errors);
            if error_count != errors.len() {
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
                    errors.add(e);
                }
            }
        }

        // +--------------------+
        // | External Functions |
        // +--------------------+
        let ext_function_keys = context
            .external_functions
            .read()
            .indices()
            .collect::<Vec<_>>();
        for ext_function_key in ext_function_keys {
            let error_count = errors.len();
            self.resolve_ext_function(ext_function_key, &context, &mut errors);
            if error_count != errors.len() {
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
                    errors.add(e);
                }
            }
        }

        // +---------+
        // | Statics |
        // +---------+
        let statics_keys = context.statics.read().indices().collect::<Vec<_>>();
        for static_key in statics_keys {
            let error_count = errors.len();
            self.resolve_static(static_key, &context, &mut errors);
            if error_count != errors.len() {
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
                    static_reader[static_key.cast()].loc,
                ) {
                    errors.add(e);
                }
            }
        }

        // +--------+
        // | Traits |
        // +--------+
        let trait_keys = context.traits.read().indices().collect::<Vec<_>>();
        for trait_key in trait_keys {
            let error_count = errors.len();
            self.resolve_trait(trait_key, &context, &mut errors);
            if error_count != errors.len() {
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
                    trait_reader[trait_key.cast()].location,
                ) {
                    errors.add(e);
                }
            }
        }

        let struct_keys = context.structs.read().indices().collect::<Vec<_>>();
        for struct_id in struct_keys {
            self.resolve_struct_impls(struct_id, &context, &mut errors);
        }

        {
            lang_items_writer.check(&mut errors, self);
        }

        errors
    }

    fn resolve_struct_impls(
        &self,
        struct_key: StoreKey<BakedStruct<'arena>>,
        context: &ModuleContext<'arena>,
        errors: &mut Diagnostics<'arena>,
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

        for (name, implementation, loc) in trait_impl {
            let trait_id =
                match resolve_import(context, module.cast(), &[name], &loc, &mut Vec::new()) {
                    Err(e) => {
                        errors.add(e);
                        continue;
                    }
                    Ok(ModuleScopeValue::Trait(trait_id)) => trait_id,
                    Ok(_) => {
                        errors.add_unbound_ident(loc, name.symbol());
                        continue;
                    }
                };

            let typed_trait = &trait_reader[trait_id.cast()];
            if typed_trait.functions.len() != implementation.len() {
                for (name, func_id) in &implementation {
                    if !typed_trait.functions.iter().any(|(v, ..)| v == name) {
                        errors.add_is_not_trait_member(
                            function_reader[func_id.cast()].0.span,
                            name.symbol(),
                        );
                    }
                }
            }

            let mut trait_impl = Vec::new();
            for (name, args, return_type, ..) in &typed_trait.functions {
                let Some(&func_id) = implementation.get(name) else {
                    errors.add_missing_trait_item(loc, name.symbol());
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
                    let expected = args.iter().map(|(_, v)| v.clone()).collect::<Vec<_>>();
                    let found = function_contract
                        .arguments
                        .iter()
                        .map(|(_, v)| v.clone())
                        .collect::<Vec<_>>();
                    errors.add_mismatching_arguments(function_contract.span, expected, found);
                    with_errs = true;
                }

                if *return_type != function_contract.return_type {
                    errors.add_mismatching_return_type(
                        function_contract.span,
                        return_type.clone(),
                        function_contract.return_type.clone(),
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
            let contract = &mut function_writer[fn_id].0;
            if let Type::PrimitiveSelf(num_references) = contract.return_type {
                contract.return_type = Type::Struct {
                    struct_id: struct_key.cast(),
                    name: struct_reader[struct_key.cast()].name,
                    num_references,
                }
            }
            for t in contract.arguments.iter_mut() {
                if let Type::PrimitiveSelf(num_references) = t.1 {
                    t.1 = Type::Struct {
                        struct_id: struct_key.cast(),
                        name: struct_reader[struct_key.cast()].name,
                        num_references,
                    }
                }
            }
        }
    }

    fn resolve_function(
        &self,
        function_id: StoreKey<Function<'arena>>,
        context: &ModuleContext<'arena>,
        errors: &mut Diagnostics<'arena>,
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
                    &bound.1,
                    &mut Vec::new(),
                ) {
                    Ok(ModuleScopeValue::Trait(v)) => bounds.push(v.cast()),
                    Ok(_) => {
                        errors.add_unbound_ident(
                            bound.1,
                            bound
                                .0
                                .pop()
                                .expect("a path has to have at least one element")
                                .symbol(),
                        );
                    }
                    Err(e) => _ = errors.add(e),
                }
            }
            generics.push(TypedGeneric {
                name: generic.name,
                sized: generic.sized,
                bounds,
            });
        }
        let mut resolved_function_contract = TypecheckedFunctionContract {
            module_id: module_id.cast(),
            name: writer[function_id].0.name,
            span: writer[function_id].0.span,
            annotations: std::mem::take(&mut writer[function_id].0.annotations),
            arguments: Vec::new(),
            return_type: Type::PrimitiveNever,
            generics,
        };
        drop(writer);

        let mut has_errors = false;
        match self.resolve_type(
            module_id.cast(),
            &return_type,
            &resolved_function_contract.generics,
        ) {
            Ok(v) => resolved_function_contract.return_type = v,
            Err(e) => {
                has_errors = true;
                errors.add(e);
            }
        }

        for arg in arguments {
            match self.resolve_type(
                module_id.cast(),
                &arg.typ,
                &resolved_function_contract.generics,
            ) {
                Ok(v) => resolved_function_contract.arguments.push((arg.name, v)),
                Err(e) => {
                    has_errors = true;
                    errors.add(e);
                }
            }
        }

        if !has_errors {
            self.functions.write()[function_id.cast()].0 = resolved_function_contract;
        }
    }

    fn resolve_ext_function(
        &self,
        ext_function_id: StoreKey<ExternalFunction<'arena>>,
        context: &ModuleContext<'arena>,
        errors: &mut Diagnostics<'arena>,
    ) {
        let mut writer = context.external_functions.write();
        let module_id = writer[ext_function_id].2;
        let arguments = std::mem::take(&mut writer[ext_function_id].0.arguments);
        let span = writer[ext_function_id].0.return_type.span();
        let return_type = std::mem::replace(
            &mut writer[ext_function_id].0.return_type,
            TypeRef::Void(span, 0),
        );
        let mut resolved_function_contract = TypecheckedFunctionContract {
            module_id: module_id.cast(),
            name: writer[ext_function_id].0.name,
            span: writer[ext_function_id].0.span,
            annotations: std::mem::take(&mut writer[ext_function_id].0.annotations),
            arguments: Vec::new(),
            return_type: Type::PrimitiveNever,
            generics: Vec::new(),
        };
        drop(writer);

        let mut has_errors = false;
        match self.resolve_type(module_id.cast(), &return_type, &[]) {
            Ok(v) => resolved_function_contract.return_type = v,
            Err(e) => {
                has_errors = true;
                errors.add(e);
            }
        }

        for arg in arguments {
            match self.resolve_type(module_id.cast(), &arg.typ, &[]) {
                Ok(v) => resolved_function_contract.arguments.push((arg.name, v)),
                Err(e) => {
                    has_errors = true;
                    errors.add(e);
                }
            }
        }

        if !has_errors {
            self.external_functions.write()[ext_function_id.cast()] =
                (resolved_function_contract, None);
        }
    }

    fn resolve_static(
        &self,
        static_id: StoreKey<Static<'arena>>,
        context: &ModuleContext<'arena>,
        errors: &mut Diagnostics<'arena>,
    ) {
        let mut writer = context.statics.write();
        let span = writer[static_id].3;
        let annotations = std::mem::take(&mut writer[static_id].4);
        let dummy_type = TypeRef::Void(writer[static_id].0.span(), 0);
        let typ = std::mem::replace(&mut writer[static_id].0, dummy_type);
        let module_id = writer[static_id].2;
        drop(writer);
        match self.resolve_type(module_id.cast(), &typ, &[]) {
            Ok(v) => {
                self.statics.write()[static_id.cast()] =
                    TypedStatic::new(v, TypedLiteral::Void, module_id.cast(), span, annotations);
            }
            Err(e) => _ = errors.add(e),
        }
    }

    fn resolve_trait(
        &self,
        trait_id: StoreKey<Trait<'arena>>,
        context: &ModuleContext<'arena>,
        errors: &mut Diagnostics<'arena>,
    ) {
        let mut writer = context.traits.write();
        let span = writer[trait_id].span;
        let name = writer[trait_id].name;
        let annotations = std::mem::take(&mut writer[trait_id].annotations);
        let functions = std::mem::take(&mut writer[trait_id].functions);
        let module_id = writer[trait_id].module;
        drop(writer);

        let mut typed_functions = Vec::new();
        let error_count = errors.len();

        for (name, arguments, return_type, annotations, location) in functions {
            let typed_return_type = match self.resolve_type(module_id.cast(), &return_type, &[]) {
                Ok(v) => v,
                Err(e) => {
                    errors.add(e);
                    continue;
                }
            };

            let mut typed_arguments = Vec::new();

            for arg in arguments {
                match self.resolve_type(module_id.cast(), &arg.typ, &[]) {
                    Ok(v) => typed_arguments.push((arg.name, v)),
                    Err(e) => _ = errors.add(e),
                }
            }

            typed_functions.push((
                name,
                typed_arguments,
                typed_return_type,
                annotations,
                location,
            ));
        }

        if errors.len() == error_count {
            self.traits.write()[trait_id.cast()] = TypedTrait {
                name,
                location: span,
                id: trait_id.cast(),
                module_id: module_id.cast(),
                annotations,
                functions: typed_functions,
            };
        }
    }
}
