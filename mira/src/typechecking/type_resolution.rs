use std::sync::Arc;

use crate::{
    lang_items::{LangItemAnnotation, LangItemErrors},
    module::{
        ExternalFunctionId, FunctionId, ModuleContext, ModuleScopeValue, StaticId, StructId,
        TraitId,
    },
    parser::TypeRef,
};

use super::{
    expression::TypedLiteral, resolve_import, types::Type, TypecheckedFunctionContract,
    TypecheckingContext, TypecheckingError, TypedTrait, DUMMY_LOCATION,
};

impl TypecheckingContext {
    /// Resolves the types; This should be ran *after* [Self::resolve_imports]
    ///
    /// NOTE: this could potentially deadlock as i write-lock the rwlocks in the context a bunch
    /// and don't free them until fully resolving a value. This module is designed in a way that
    /// such a deadlock should never happen but.. uhh... :3c :3
    /// meow
    pub fn resolve_types(&self, context: Arc<ModuleContext>) -> Vec<TypecheckingError> {
        let mut errors = Vec::new();

        let mut lang_items_writer = self.lang_items.write();

        // +---------+
        // | Structs |
        // +---------+
        let num_structs = context.structs.read().len();
        for struct_id in 0..num_structs {
            let struct_reader = context.structs.read();
            let module_id = struct_reader[struct_id].module_id;
            drop(struct_reader);
            let err_count = errors.len();
            assert!(
                !self.resolve_struct(context.clone(), struct_id, module_id, &mut errors),
                "this came from no field, so this shouldn't be recursive"
            );
            if err_count != errors.len() {
                continue;
            }
            let struct_reader = self.structs.read();
            for annotation in struct_reader[struct_id]
                .annotations
                .get_annotations::<LangItemAnnotation>()
            {
                if let Err(e) = lang_items_writer.push_struct(
                    struct_id,
                    annotation.get_langitem(),
                    struct_reader[struct_id].location.clone(),
                ) {
                    errors.push(e.into());
                }
            }
        }

        // +-----------+
        // | Functions |
        // +-----------+
        let num_functions = context.functions.read().len();
        for function_id in 0..num_functions {
            let error_count = errors.len();
            self.resolve_function(function_id, &context, &mut errors);
            if error_count != errors.len() {
                continue;
            }
            let function_reader = self.functions.read();
            for annotation in function_reader[function_id]
                .0
                .annotations
                .get_annotations::<LangItemAnnotation>()
            {
                if let Err(e) = lang_items_writer.push_function(
                    function_id,
                    annotation.get_langitem(),
                    function_reader[function_id].0.location.clone(),
                ) {
                    errors.push(e.into());
                }
            }
        }

        // +--------------------+
        // | External Functions |
        // +--------------------+
        let num_functions = context.external_functions.read().len();
        for ext_function_id in 0..num_functions {
            let error_count = errors.len();
            self.resolve_ext_function(ext_function_id, &context, &mut errors);
            if error_count != errors.len() {
                continue;
            }
            let ext_function_reader = self.external_functions.read();
            for annotation in ext_function_reader[ext_function_id]
                .0
                .annotations
                .get_annotations::<LangItemAnnotation>()
            {
                if let Err(e) = lang_items_writer.push_external_function(
                    ext_function_id,
                    annotation.get_langitem(),
                    ext_function_reader[ext_function_id].0.location.clone(),
                ) {
                    errors.push(e.into());
                }
            }
        }

        // +---------+
        // | Statics |
        // +---------+
        let num_statics = context.statics.read().len();
        for static_id in 0..num_statics {
            let error_count = errors.len();
            self.resolve_static(static_id, &context, &mut errors);
            if error_count != errors.len() {
                continue;
            }
            let static_reader = self.statics.read();
            for annotation in static_reader[static_id]
                .4
                .get_annotations::<LangItemAnnotation>()
            {
                if let Err(e) = lang_items_writer.push_static(
                    static_id,
                    annotation.get_langitem(),
                    static_reader[static_id].3.clone(),
                ) {
                    errors.push(e.into());
                }
            }
        }

        // +--------+
        // | Traits |
        // +--------+
        let num_traits = context.traits.read().len();
        for trait_id in 0..num_traits {
            let error_count = errors.len();
            self.resolve_trait(trait_id, &context, &mut errors);
            if error_count != errors.len() {
                continue;
            }
            let trait_reader = self.traits.read();
            for annotation in trait_reader[trait_id]
                .annotations
                .get_annotations::<LangItemAnnotation>()
            {
                if let Err(e) = lang_items_writer.push_trait(
                    trait_id,
                    annotation.get_langitem(),
                    trait_reader[trait_id].location.clone(),
                ) {
                    errors.push(e.into());
                }
            }
        }

        for struct_id in 0..num_structs {
            self.resolve_struct_impls(struct_id, &context, &mut errors);
        }

        {
            let mut lang_item_check_errors = LangItemErrors::new();
            lang_items_writer.check(&mut lang_item_check_errors, self);
            errors.extend(lang_item_check_errors.0.into_iter().map(Into::into));
        }

        return errors;
    }

    fn resolve_struct_impls(
        &self,
        struct_id: StructId,
        context: &ModuleContext,
        errors: &mut Vec<TypecheckingError>,
    ) {
        let mut writer = context.structs.write();
        let trait_impl = std::mem::take(&mut writer[struct_id].impls);
        drop(writer);

        // it is *okay* to hold the lock here between resolve_type calls as all structs have
        // already been resolved.
        let mut struct_writer = self.structs.write();
        let trait_reader = self.traits.read();
        let function_reader = self.functions.read();
        let module = struct_writer[struct_id].module_id;

        for (name, implementation, loc) in trait_impl {
            let trait_id =
                match resolve_import(context, module, &[name.clone()], &loc, &mut Vec::new()) {
                    Err(e) => {
                        errors.push(e);
                        continue;
                    }
                    Ok(ModuleScopeValue::Trait(trait_id)) => trait_id,
                    Ok(_) => {
                        errors.push(TypecheckingError::UnboundIdent {
                            location: loc,
                            name,
                        });
                        continue;
                    }
                };

            let typed_trait = &trait_reader[trait_id];
            if typed_trait.functions.len() != implementation.len() {
                for (name, func_id) in &implementation {
                    if typed_trait
                        .functions
                        .iter()
                        .find(|(v, ..)| v == name)
                        .is_none()
                    {
                        errors.push(TypecheckingError::IsNotTraitMember {
                            location: function_reader[*func_id].0.location.clone(),
                            name: name.clone(),
                        })
                    }
                }
            }

            let mut trait_impl = Vec::new();
            for (name, args, return_type, ..) in &typed_trait.functions {
                let Some(&func_id) = implementation.get(name) else {
                    errors.push(TypecheckingError::MissingTraitItem {
                        location: loc.clone(),
                        name: name.clone(),
                    });
                    continue;
                };

                let mut with_errs = false;

                let function_contract = &function_reader[func_id].0;
                if !function_contract
                    .arguments
                    .iter()
                    .zip(args)
                    .map(|((_, typ_a), (_, typ_b))| *typ_a == *typ_b)
                    .fold(true, |acc, v| acc && v)
                {
                    let expected = args.iter().map(|(_, v)| v.clone()).collect::<Vec<_>>();
                    let found = function_contract
                        .arguments
                        .iter()
                        .map(|(_, v)| v.clone())
                        .collect::<Vec<_>>();
                    errors.push(TypecheckingError::MismatchingArguments {
                        location: function_contract.location.clone(),
                        expected,
                        found,
                    });
                    with_errs = true;
                }

                if *return_type != function_contract.return_type {
                    errors.push(TypecheckingError::MismatchingReturnType {
                        location: function_contract.location.clone(),
                        expected: return_type.clone(),
                        found: function_contract.return_type.clone(),
                    });
                    with_errs = true
                }
                if !with_errs {
                    trait_impl.push(func_id);
                }
            }
            if trait_impl.len() != typed_trait.functions.len() {
                continue;
            }
            struct_writer[struct_id]
                .trait_impl
                .insert(trait_id, trait_impl);
        }
        drop(struct_writer);
        drop(function_reader);
        drop(trait_reader);
        let struct_reader = self.structs.read();
        let mut function_writer = self.functions.write();

        for fn_id in struct_reader[struct_id]
            .global_impl
            .values()
            .copied()
            .chain(
                struct_reader[struct_id]
                    .trait_impl
                    .values()
                    .map(|v| v.iter())
                    .flatten()
                    .copied(),
            )
        {
            let contract = &mut function_writer[fn_id].0;
            if let Type::PrimitiveSelf(num_references) = contract.return_type {
                contract.return_type = Type::Struct {
                    struct_id,
                    name: struct_reader[struct_id].name.clone(),
                    num_references,
                }
            }
            for t in contract.arguments.iter_mut() {
                if let Type::PrimitiveSelf(num_references) = t.1 {
                    t.1 = Type::Struct {
                        struct_id,
                        name: struct_reader[struct_id].name.clone(),
                        num_references,
                    }
                }
            }
        }
    }

    fn resolve_function(
        &self,
        function_id: FunctionId,
        context: &ModuleContext,
        errors: &mut Vec<TypecheckingError>,
    ) {
        let mut writer = context.functions.write();
        let module_id = writer[function_id].2;
        let arguments = std::mem::take(&mut writer[function_id].0.arguments);
        let return_type = std::mem::replace(
            &mut writer[function_id].0.return_type,
            TypeRef::Void(DUMMY_LOCATION.clone(), 0),
        );
        let generics = std::mem::take(&mut writer[function_id].0.generics)
            .into_iter()
            .map(|generic| generic.name)
            .collect::<Vec<_>>();
        let mut resolved_function_contract = TypecheckedFunctionContract {
            module_id,
            name: writer[function_id].0.name.clone(),
            location: writer[function_id].0.location.clone(),
            annotations: std::mem::take(&mut writer[function_id].0.annotations),
            arguments: Vec::new(),
            return_type: Type::PrimitiveNever,
        };
        drop(writer);

        let mut has_errors = false;
        match self.resolve_type(module_id, &return_type, &generics) {
            Ok(v) => resolved_function_contract.return_type = v,
            Err(e) => {
                has_errors = true;
                errors.push(e);
            }
        }

        for arg in arguments {
            match self.resolve_type(module_id, &arg.typ, &generics) {
                Ok(v) => resolved_function_contract.arguments.push((arg.name, v)),
                Err(e) => {
                    has_errors = true;
                    errors.push(e);
                }
            }
        }

        if !has_errors {
            self.functions.write()[function_id].0 = resolved_function_contract;
        }
    }

    fn resolve_ext_function(
        &self,
        ext_function_id: ExternalFunctionId,
        context: &ModuleContext,
        errors: &mut Vec<TypecheckingError>,
    ) {
        let mut writer = context.external_functions.write();
        let module_id = writer[ext_function_id].2;
        let arguments = std::mem::take(&mut writer[ext_function_id].0.arguments);
        let return_type = std::mem::replace(
            &mut writer[ext_function_id].0.return_type,
            TypeRef::Void(DUMMY_LOCATION.clone(), 0),
        );
        let mut resolved_function_contract = TypecheckedFunctionContract {
            module_id,
            name: writer[ext_function_id].0.name.clone(),
            location: writer[ext_function_id].0.location.clone(),
            annotations: std::mem::take(&mut writer[ext_function_id].0.annotations),
            arguments: Vec::new(),
            return_type: Type::PrimitiveNever,
        };
        drop(writer);

        let mut has_errors = false;
        match self.resolve_type(module_id, &return_type, &[]) {
            Ok(v) => resolved_function_contract.return_type = v,
            Err(e) => {
                has_errors = true;
                errors.push(e);
            }
        }

        for arg in arguments {
            match self.resolve_type(module_id, &arg.typ, &[]) {
                Ok(v) => resolved_function_contract.arguments.push((arg.name, v)),
                Err(e) => {
                    has_errors = true;
                    errors.push(e);
                }
            }
        }

        if !has_errors {
            self.external_functions.write()[ext_function_id] = (resolved_function_contract, None);
        }
    }

    fn resolve_static(
        &self,
        static_id: StaticId,
        context: &ModuleContext,
        errors: &mut Vec<TypecheckingError>,
    ) {
        let mut writer = context.statics.write();
        let location = std::mem::replace(&mut writer[static_id].3, DUMMY_LOCATION.clone());
        let annotations = std::mem::take(&mut writer[static_id].4);
        let dummy_type = TypeRef::Void(writer[static_id].0.loc().clone(), 0);
        let typ = std::mem::replace(&mut writer[static_id].0, dummy_type);
        let module_id = writer[static_id].2;
        drop(writer);
        match self.resolve_type(module_id, &typ, &[]) {
            Ok(v) => {
                self.statics.write()[static_id] =
                    (v, TypedLiteral::Void, module_id, location, annotations);
            }
            Err(e) => errors.push(e),
        }
    }

    fn resolve_trait(
        &self,
        trait_id: TraitId,
        context: &ModuleContext,
        errors: &mut Vec<TypecheckingError>,
    ) {
        let mut writer = context.traits.write();
        let location = std::mem::replace(&mut writer[trait_id].location, DUMMY_LOCATION.clone());
        let name = writer[trait_id].name.clone();
        let annotations = std::mem::take(&mut writer[trait_id].annotations);
        let functions = std::mem::take(&mut writer[trait_id].functions);
        let module_id = writer[trait_id].module_id;
        drop(writer);

        let mut typed_functions = Vec::new();
        let error_count = errors.len();

        for (name, arguments, return_type, annotations, location) in functions {
            let typed_return_type = match self.resolve_type(module_id, &return_type, &[]) {
                Ok(v) => v,
                Err(e) => {
                    errors.push(e);
                    continue;
                }
            };

            let mut typed_arguments = Vec::new();

            for arg in arguments {
                match self.resolve_type(module_id, &arg.typ, &[]) {
                    Ok(v) => typed_arguments.push((arg.name, v)),
                    Err(e) => errors.push(e),
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
            self.traits.write()[trait_id] = TypedTrait {
                name,
                location,
                id: trait_id,
                module_id,
                annotations,
                functions: typed_functions,
            };
        }
    }
}
