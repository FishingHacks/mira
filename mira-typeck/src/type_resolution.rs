use std::{borrow::Cow, sync::Arc};

use mira_common::index::IndexMap;
use mira_errors::ErrorEmitted;
use mira_spans::{ArenaList, Span, interner::symbols};

use mira_parser::{
    TypeRef,
    module::{
        ExternalFunctionId, FunctionContext, FunctionId, ModuleContext, StaticId, StructId, TraitId,
    },
    std_annotations::lang_item::LangItemAnnotation,
};

use crate::{
    ResolvedValue, ScopeKind, Ty, TypeCtx, TypecheckingError, TypedStruct,
    error::TypecheckingErrorEmitterExt, resolve_import_simple,
};

use super::{
    TypeckCtx, TypedFunctionContract, TypedGeneric, TypedStatic, TypedTrait,
    ir::TypedLiteral,
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
    pub fn resolve_types(&self, context: Arc<ModuleContext<'arena>>) -> Result<(), ErrorEmitted> {
        let tracker = self.track_errors();

        let mut structs_left = IndexMap::new();
        context
            .structs
            .read()
            .keys()
            .zip(std::iter::repeat(ResolvingState::Pending))
            .for_each(|(k, v)| structs_left.insert(k, v));

        // ┌─────────┐
        // │ Structs │
        // └─────────┘
        loop {
            let Some(key) = structs_left.keys().next() else {
                break;
            };

            let struct_reader = context.structs.read();
            let module_id = struct_reader[key].module_id;
            drop(struct_reader);
            let tracker = self.track_errors();
            _ = self.resolve_struct(context.clone(), key, module_id, &mut structs_left);
            if self.errors_happened_res(tracker).is_err() {
                continue;
            }
            let struct_reader = self.structs.read();
            for annotation in struct_reader[key]
                .annotations
                .get_annotations::<LangItemAnnotation>()
            {
                if let Err(e) = self.lang_items.write().push_struct(
                    key,
                    annotation.get_langitem(),
                    struct_reader[key].span,
                ) {
                    _ = self.emit_diag(e);
                }
            }
        }

        // ┌───────────┐
        // │ Functions │
        // └───────────┘
        let function_keys = context.functions.read().keys().collect::<Vec<_>>();
        for function_key in function_keys {
            let tracker = self.track_errors();
            self.resolve_function(function_key, &context);
            if self.errors_happened_res(tracker).is_err() {
                continue;
            }
            let function_reader = self.functions.read();
            for annotation in function_reader[function_key]
                .0
                .annotations
                .get_annotations::<LangItemAnnotation>()
            {
                if let Err(e) = self.lang_items.write().push_function(
                    function_key,
                    annotation.get_langitem(),
                    function_reader[function_key].0.span,
                ) {
                    _ = self.emit_diag(e);
                }
            }
        }

        // ┌────────────────────┐
        // │ External Functions │
        // └────────────────────┘
        let ext_function_keys = context.external_functions.read().keys().collect::<Vec<_>>();
        for ext_function_key in ext_function_keys {
            let tracker = self.track_errors();
            self.resolve_ext_function(ext_function_key, &context);
            if self.errors_happened_res(tracker).is_err() {
                continue;
            }
            let ext_function_reader = self.external_functions.read();
            for annotation in ext_function_reader[ext_function_key]
                .0
                .annotations
                .get_annotations::<LangItemAnnotation>()
            {
                if let Err(e) = self.lang_items.write().push_external_function(
                    ext_function_key,
                    annotation.get_langitem(),
                    ext_function_reader[ext_function_key].0.span,
                ) {
                    _ = self.emit_diag(e);
                }
            }
        }

        // ┌─────────┐
        // │ Statics │
        // └─────────┘
        let statics_keys = context.statics.read().keys().collect::<Vec<_>>();
        for static_key in statics_keys {
            let tracker = self.track_errors();
            self.resolve_static(static_key, &context);
            if self.errors_happened_res(tracker).is_err() {
                continue;
            }
            let static_reader = self.statics.read();
            for annotation in static_reader[static_key]
                .annotations
                .get_annotations::<LangItemAnnotation>()
            {
                if let Err(e) = self.lang_items.write().push_static(
                    static_key,
                    annotation.get_langitem(),
                    static_reader[static_key].span,
                ) {
                    _ = self.emit_diag(e);
                }
            }
        }

        // ┌────────┐
        // │ Traits │
        // └────────┘
        let trait_keys = context.traits.read().keys().collect::<Vec<_>>();
        for trait_key in trait_keys {
            let tracker = self.track_errors();
            self.resolve_trait(trait_key, &context);
            if self.errors_happened_res(tracker).is_err() {
                continue;
            }
            let trait_reader = self.traits.read();
            for annotation in trait_reader[trait_key]
                .annotations
                .get_annotations::<LangItemAnnotation>()
            {
                if let Err(e) = self.lang_items.write().push_trait(
                    trait_key,
                    annotation.get_langitem(),
                    trait_reader[trait_key].span,
                ) {
                    _ = self.emit_diag(e);
                }
            }
        }

        let struct_keys = context.structs.read().keys().collect::<Vec<_>>();
        for struct_id in struct_keys {
            self.resolve_struct_impls(struct_id, &context);
        }

        let mut structs_left = IndexMap::new();
        context
            .structs
            .read()
            .keys()
            .zip(std::iter::repeat(ResolvingState::Pending))
            .for_each(|(k, v)| structs_left.insert(k, v));

        // ┌──────────────────────────────────────────┐
        // │ Structs v2 (checking that they're sized) │
        // └──────────────────────────────────────────┘
        let struct_reader = self.structs.read();
        let mut field_struct_list = Vec::new();
        loop {
            let Some(key) = structs_left.keys().next() else {
                break;
            };

            field_struct_list.clear();
            Self::check_struct(
                self.ctx,
                key,
                &mut structs_left,
                &mut field_struct_list,
                &struct_reader,
            );
        }
        drop(struct_reader);

        self.lang_items.write().check(self)?;
        self.errors_happened_res(tracker)
    }

    fn resolve_struct_impls(&self, struct_key: StructId, context: &ModuleContext<'arena>) {
        let mut writer = context.structs.write();
        let trait_impl = std::mem::take(&mut writer[struct_key].impls);
        drop(writer);

        // it is *okay* to hold the lock here between resolve_type calls as all structs have
        // already been resolved.
        let mut struct_writer = self.structs.write();
        let trait_reader = self.traits.read();
        let function_reader = self.functions.read();
        let module = struct_writer[struct_key].module_id;

        for (name, implementation, span) in trait_impl {
            let trait_id = match resolve_import_simple(
                context,
                self.ctx,
                module,
                std::iter::once(&name),
                span,
                // TODO: Add generics here when adding generics to structs.
                &[],
                true,
            ) {
                Err(ErrorEmitted(..)) => {
                    continue;
                }
                Ok(ResolvedValue::Trait(trait_id)) => trait_id,
                Ok(v) => {
                    _ = self.emit_mismatching_scope_type(span, v.into(), ScopeKind::Trait);
                    continue;
                }
            };

            let typed_trait = &trait_reader[trait_id];
            if typed_trait.functions.len() != implementation.len() {
                for (name, &func_id) in &implementation {
                    if !typed_trait.functions.iter().any(|(v, ..)| v == name) {
                        _ = self.emit_is_not_trait_member(
                            function_reader[func_id].0.span,
                            name.symbol(),
                        );
                    }
                }
            }

            let mut trait_impl = Vec::new();
            for (name, args, return_type, ..) in &typed_trait.functions {
                let Some(&func_id) = implementation.get(name) else {
                    _ = self.emit_missing_trait_item(span, name.symbol());
                    continue;
                };

                let mut with_errs = false;

                let function_contract = &function_reader[func_id].0;
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
                    _ = self.emit_mismatching_arguments(function_contract.span, expected, found);
                    with_errs = true;
                }

                if *return_type != function_contract.return_type {
                    _ = self.emit_mismatching_return_type(
                        function_contract.span,
                        *return_type,
                        function_contract.return_type,
                    );
                    with_errs = true
                }
                if !with_errs {
                    trait_impl.push(func_id);
                }
            }
            if trait_impl.len() != typed_trait.functions.len() {
                continue;
            }
            struct_writer[struct_key]
                .trait_impl
                .insert(trait_id, trait_impl);
        }
        drop(struct_writer);
        drop(function_reader);
        drop(trait_reader);
        let struct_reader = self.structs.read();
        let mut function_writer = self.functions.write();

        for fn_id in struct_reader[struct_key]
            .global_impl
            .values()
            .copied()
            .chain(
                struct_reader[struct_key]
                    .trait_impl
                    .iter()
                    .flat_map(|v| v.iter())
                    .copied(),
            )
        {
            let struct_generics = &struct_reader[struct_key].generics;
            let mut generics = Vec::with_capacity(struct_generics.len());
            for (generic_id, generic) in struct_generics.iter().enumerate() {
                generics.push(
                    self.intern_ty(TyKind::Generic {
                        name: generic.name,
                        generic_id: u8::try_from(generic_id)
                            .expect("why tf do u have more than 255 generics :sob:"),
                        bounds: generic.bounds,
                        sized: generic.sized,
                    }),
                );
            }

            let self_ty = self.intern_ty(TyKind::Struct {
                struct_id: struct_key,
                name: struct_reader[struct_key].name,
                generics: self.intern_tylist(&generics),
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

    // Checks that the struct isn't recursive, as well as that
    fn check_struct(
        ctx: TypeCtx<'arena>,
        struct_id: StructId,
        left: &mut IndexMap<StructId, ResolvingState>,
        // a list of field and struct spans
        field_struct_list: &mut Vec<(Span<'arena>, Span<'arena>)>,
        struct_reader: &IndexMap<StructId, TypedStruct<'arena>>,
    ) {
        fn check_ty<'ctx>(
            ctx: TypeCtx<'ctx>,
            ty: Ty<'ctx>,
            left: &mut IndexMap<StructId, ResolvingState>,
            // a list of field and struct spans
            field_struct_list: &mut Vec<(Span<'ctx>, Span<'ctx>)>,
            struct_reader: &IndexMap<StructId, TypedStruct<'ctx>>,
        ) {
            match &**ty {
                // unsized types
                TyKind::DynType(_)
                | TyKind::UnsizedArray(_)
                | TyKind::Generic { sized: false, .. } => {
                    _ = ctx.emit_unsized_struct_field(field_struct_list.last().unwrap().0, ty)
                }
                // check struct
                &TyKind::Struct { struct_id, .. } => match left.get(struct_id) {
                    Some(ResolvingState::Working) => {
                        let last = field_struct_list.last().unwrap();
                        let mut diagnostic = TypecheckingError::CyclicStruct {
                            field: last.0,
                            struct_def: last.1,
                        }
                        .to_error();
                        for &(field_span, struct_span) in field_struct_list.iter().rev().skip(1) {
                            diagnostic.add_primary_label(field_span, "recursive");
                            diagnostic.add_secondary_label(struct_span, "");
                        }
                    }
                    Some(ResolvingState::Pending) => TypeckCtx::check_struct(
                        ctx,
                        struct_id,
                        left,
                        field_struct_list,
                        struct_reader,
                    ),
                    None => {}
                },
                // containers
                &TyKind::SizedArray { ty, .. } => {
                    check_ty(ctx, ty, left, field_struct_list, struct_reader);
                }
                TyKind::Tuple(ty_list) => {
                    for &ty in ty_list.iter() {
                        check_ty(ctx, ty, left, field_struct_list, struct_reader);
                    }
                }

                // self
                TyKind::PrimitiveSelf => {
                    _ = ctx.emit_item_not_found(
                        field_struct_list.last().unwrap().0,
                        symbols::Types::self_ty,
                    );
                }

                // primitives, sized generics, function pointers, and &_
                TyKind::Function(_)
                | TyKind::Ref(_)
                | TyKind::PrimitiveVoid
                | TyKind::PrimitiveNever
                | TyKind::PrimitiveI8
                | TyKind::PrimitiveI16
                | TyKind::PrimitiveI32
                | TyKind::PrimitiveI64
                | TyKind::PrimitiveISize
                | TyKind::PrimitiveU8
                | TyKind::PrimitiveU16
                | TyKind::PrimitiveU32
                | TyKind::PrimitiveU64
                | TyKind::PrimitiveUSize
                | TyKind::PrimitiveF32
                | TyKind::PrimitiveF64
                | TyKind::PrimitiveStr
                | TyKind::PrimitiveBool
                | TyKind::Generic { sized: true, .. } => {}
            }
        }

        match left.get_mut(struct_id) {
            Some(v @ ResolvingState::Pending) => *v = ResolvingState::Working,
            Some(ResolvingState::Working) => unreachable!(),
            None => return,
        }
        let structure = &struct_reader[struct_id];
        let struct_def = structure.name.span();
        for &(name, ty, _) in &structure.elements {
            field_struct_list.push((name.span(), struct_def));
            check_ty(ctx, ty, left, field_struct_list, struct_reader);
            field_struct_list.pop();
        }
        left.remove(struct_id);
    }

    fn resolve_function(&self, function_id: FunctionId, context: &ModuleContext<'arena>) {
        let mut writer = context.functions.write();
        let func = &mut writer[function_id];
        let module_id = func.parent_module;
        let arguments = std::mem::take(&mut func.contract.arguments);
        let span = func.contract.return_type.span();
        let return_type = std::mem::replace(&mut func.contract.return_type, TypeRef::Void(span, 0));
        let untyped_generics = std::mem::take(&mut func.contract.generics);

        let mut generics = Vec::with_capacity(untyped_generics.len());
        for generic in untyped_generics {
            let mut bounds = Vec::with_capacity(generic.bounds.len());
            for bound in generic.bounds {
                match resolve_import_simple(
                    context,
                    self.ctx,
                    module_id,
                    bound.0.as_slice().iter(),
                    bound.1,
                    // no generics during bound resolution, because traits don't have generics.
                    &[],
                    true,
                ) {
                    Ok(ResolvedValue::Trait(v)) => bounds.push(v),
                    Ok(v) => {
                        _ = self.emit_mismatching_scope_type(bound.1, ScopeKind::Trait, v.into());
                    }
                    Err(ErrorEmitted(..)) => {}
                }
            }
            generics.push(TypedGeneric {
                name: generic.name,
                sized: generic.sized,
                bounds: ArenaList::new(self.arena(), &bounds),
            });
        }
        let mut resolved_function_contract = TypedFunctionContract {
            module_id,
            name: func.contract.name,
            span: func.contract.span,
            annotations: std::mem::take(&mut func.contract.annotations),
            arguments: Vec::new(),
            return_type: default_types::never,
            generics,
            comment: func.contract.comment,
            context: func.ctx,
        };
        drop(writer);

        let error_tracker = self.track_errors();

        let generics = match resolved_function_contract.context {
            FunctionContext::Freestanding => Cow::Borrowed(&resolved_function_contract.generics),
            FunctionContext::StructFn(struct_id) => {
                let mut generics = self.structs.read_recursive()[struct_id].generics.clone();
                if generics.is_empty() {
                    Cow::Borrowed(&resolved_function_contract.generics)
                } else {
                    generics.extend_from_slice(&resolved_function_contract.generics);
                    Cow::Owned(generics)
                }
            }
        };

        if let Ok(v) = self.resolve_type(module_id, &return_type, &generics) {
            resolved_function_contract.return_type = v
        }

        for arg in arguments {
            if let Ok(v) = self.resolve_type(module_id, &arg.ty, &generics) {
                resolved_function_contract.arguments.push((arg.name, v))
            }
        }

        if self.errors_happened_res(error_tracker).is_ok() {
            self.functions.write()[function_id].0 = resolved_function_contract;
        }
    }

    fn resolve_ext_function(
        &self,
        ext_function_id: ExternalFunctionId,
        context: &ModuleContext<'arena>,
    ) {
        let mut writer = context.external_functions.write();
        let arguments = std::mem::take(&mut writer[ext_function_id].contract.arguments);
        let void = TypeRef::Void(writer[ext_function_id].contract.return_type.span(), 0);
        let return_type =
            std::mem::replace(&mut writer[ext_function_id].contract.return_type, void);
        let func = &writer[ext_function_id];
        let module_id = func.parent_module;
        let mut resolved_function_contract = TypedFunctionContract {
            module_id,
            name: func.contract.name,
            span: func.contract.span,
            comment: func.contract.comment,
            annotations: std::mem::take(&mut writer[ext_function_id].contract.annotations),
            arguments: Vec::new(),
            return_type: default_types::never,
            generics: Vec::new(),
            context: FunctionContext::Freestanding,
        };
        drop(writer);

        let tracker = self.track_errors();
        if let Ok(v) = self.resolve_type(module_id, &return_type, &[]) {
            resolved_function_contract.return_type = v
        }

        for arg in arguments {
            if let Ok(v) = self.resolve_type(module_id, &arg.ty, &[]) {
                resolved_function_contract.arguments.push((arg.name, v))
            }
        }

        if self.errors_happened_res(tracker).is_ok() {
            self.external_functions.write()[ext_function_id] = (resolved_function_contract, None);
        }
    }

    fn resolve_static(&self, static_id: StaticId, context: &ModuleContext<'arena>) {
        let mut writer = context.statics.write();
        let span = writer[static_id].span;
        let name = writer[static_id].name;
        let annotations = std::mem::take(&mut writer[static_id].annotations);
        let dummy_type = TypeRef::Void(writer[static_id].ty.span(), 0);
        let ty = std::mem::replace(&mut writer[static_id].ty, dummy_type);
        let module_id = writer[static_id].module;
        let comment = writer[static_id].comment;
        drop(writer);
        if let Ok(v) = self.resolve_type(module_id, &ty, &[]) {
            self.statics.write()[static_id] = TypedStatic::new(
                v,
                TypedLiteral::Void,
                module_id,
                span,
                annotations,
                name,
                comment,
            );
        }
    }

    fn resolve_trait(&self, trait_id: TraitId, context: &ModuleContext<'arena>) {
        let mut writer = context.traits.write();
        let span = writer[trait_id].span;
        let name = writer[trait_id].name;
        let annotations = std::mem::take(&mut writer[trait_id].annotations);
        let functions = std::mem::take(&mut writer[trait_id].functions);
        let module_id = writer[trait_id].module;
        let comment = writer[trait_id].comment;
        drop(writer);

        let mut typed_functions = Vec::new();
        let tracker = self.track_errors();

        for func in functions {
            let typed_return_type = match self.resolve_type(module_id, &func.return_ty, &[]) {
                Ok(v) => v,
                Err(ErrorEmitted(..)) => continue,
            };

            let mut typed_arguments = Vec::new();

            for arg in &func.args {
                if let Ok(v) = self.resolve_type(module_id, &arg.ty, &[]) {
                    typed_arguments.push((arg.name, v))
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

        if self.errors_happened_res(tracker).is_ok() {
            self.traits.write()[trait_id] = TypedTrait {
                name,
                span,
                id: trait_id,
                module_id,
                annotations,
                functions: typed_functions,
                comment,
            };
        }
    }
}
