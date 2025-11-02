use mira_parser::module::{ExternalFunctionId, FunctionId, ModuleId, StaticId, StructId, TraitId};

use crate::ir_displayer::ReadOnlyTypecheckingContext;

use super::{
    formatter::Formatter, function::FuncDisplay, module::ModuleDisplay, statics::StaticDisplay,
    structs::StructDisplay, traits::TraitDisplay,
};

#[derive(Clone, Copy)]
pub struct TCContextDisplay;

#[derive(Clone, Copy)]
pub struct ChildrenOfModuleFilter(pub ModuleId);

#[derive(Clone, Copy)]
// filters out nothing
pub struct AllFilter;

impl DisplayFilter for AllFilter {
    fn display_module(&self, _: &ReadOnlyTypecheckingContext<'_, '_>, _: ModuleId) -> bool {
        true
    }
}

impl DisplayFilter for ChildrenOfModuleFilter {
    fn display_module(
        &self,
        ctx: &ReadOnlyTypecheckingContext<'_, '_>,
        mut module: ModuleId,
    ) -> bool {
        loop {
            if module == self.0 {
                return true;
            }
            match ctx.modules[module].parent {
                Some(v) => module = v,
                None => break false,
            }
        }
    }
}

pub trait DisplayFilter {
    fn display_module(&self, ctx: &ReadOnlyTypecheckingContext<'_, '_>, module: ModuleId) -> bool;

    fn display_fn(&self, ctx: &ReadOnlyTypecheckingContext<'_, '_>, fn_id: FunctionId) -> bool {
        self.display_module(ctx, ctx.functions[fn_id].0.module_id)
    }
    fn display_ext_fn(
        &self,
        ctx: &ReadOnlyTypecheckingContext<'_, '_>,
        ext_fn_id: ExternalFunctionId,
    ) -> bool {
        self.display_module(ctx, ctx.external_functions[ext_fn_id].0.module_id)
    }
    fn display_static(
        &self,
        ctx: &ReadOnlyTypecheckingContext<'_, '_>,
        static_id: StaticId,
    ) -> bool {
        self.display_module(ctx, ctx.statics[static_id].module_id)
    }
    fn display_struct(
        &self,
        ctx: &ReadOnlyTypecheckingContext<'_, '_>,
        struct_id: StructId,
    ) -> bool {
        self.display_module(ctx, ctx.structs[struct_id].module_id)
    }
    fn display_trait(&self, ctx: &ReadOnlyTypecheckingContext<'_, '_>, trait_id: TraitId) -> bool {
        self.display_module(ctx, ctx.traits[trait_id].module_id)
    }
}

impl TCContextDisplay {
    pub fn fmt(
        self,
        f: &mut Formatter<'_, '_, '_>,
        filter: &impl DisplayFilter,
    ) -> std::fmt::Result {
        for (id, module) in f.ctx.modules.entries() {
            if !filter.display_module(&f.ctx, id) {
                continue;
            }
            f.write_str("\n\n")?;
            ModuleDisplay(module).fmt(f, id)?;
        }

        for (id, func) in f.ctx.functions.entries() {
            if !filter.display_fn(&f.ctx, id) {
                continue;
            }
            f.write_str("\n\n")?;
            FuncDisplay(&func.0, Some(&func.1)).fmt(f, false, id.to_usize())?;
        }

        for (id, func) in f.ctx.external_functions.entries() {
            if !filter.display_ext_fn(&f.ctx, id) {
                continue;
            }
            f.write_str("\n\n")?;
            FuncDisplay(&func.0, func.1.as_ref()).fmt(f, true, id.to_usize())?;
        }

        for (id, static_value) in f.ctx.statics.entries() {
            if !filter.display_static(&f.ctx, id) {
                continue;
            }
            f.write_str("\n\n")?;
            StaticDisplay(static_value).fmt(f, id)?;
        }

        for (id, structure) in f.ctx.structs.entries() {
            if !filter.display_struct(&f.ctx, id) {
                continue;
            }
            f.write_str("\n\n")?;
            StructDisplay(structure).fmt(f)?;
        }

        for (id, trait_value) in f.ctx.traits.entries() {
            if !filter.display_trait(&f.ctx, id) {
                continue;
            }
            f.write_str("\n\n")?;
            TraitDisplay(trait_value).fmt(f)?;
        }
        f.write_str("\n\n\n\n")?;
        Ok(())
    }
}
