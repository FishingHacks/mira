use super::{
    formatter::Formatter, function::FuncDisplay, module::ModuleDisplay, statics::StaticDisplay,
    structs::StructDisplay, traits::TraitDisplay,
};

#[derive(Clone, Copy)]
pub struct TCContextDisplay;

impl TCContextDisplay {
    pub fn fmt(self, f: &mut Formatter<'_, '_, '_>) -> std::fmt::Result {
        for (id, module) in f.ctx.modules.entries() {
            f.write_str("\n\n")?;
            ModuleDisplay(module).fmt(f, id)?;
        }

        for (id, func) in f.ctx.functions.entries() {
            f.write_str("\n\n")?;
            FuncDisplay(&func.0, Some(&func.1)).fmt(f, false, id.to_usize())?;
        }

        for (id, func) in f.ctx.external_functions.entries() {
            f.write_str("\n\n")?;
            FuncDisplay(&func.0, func.1.as_ref()).fmt(f, true, id.to_usize())?;
        }

        for (id, static_value) in f.ctx.statics.entries() {
            f.write_str("\n\n")?;
            StaticDisplay(static_value).fmt(f, id)?;
        }

        for structure in f.ctx.structs.iter() {
            f.write_str("\n\n")?;
            StructDisplay(structure).fmt(f)?;
        }

        for trait_value in f.ctx.traits.iter() {
            f.write_str("\n\n")?;
            TraitDisplay(trait_value).fmt(f)?;
        }
        Ok(())
    }
}
