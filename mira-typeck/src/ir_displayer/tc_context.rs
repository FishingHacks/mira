use super::{
    formatter::Formatter, function::FuncDisplay, module::ModuleDisplay, statics::StaticDisplay,
    structs::StructDisplay, traits::TraitDisplay,
};

#[derive(Clone, Copy)]
pub struct TCContextDisplay;

impl TCContextDisplay {
    pub fn fmt(self, f: &mut Formatter) -> std::fmt::Result {
        for (id, module) in f.ctx.modules.index_value_iter() {
            f.write_str("\n\n")?;
            ModuleDisplay(module).fmt(f, id)?;
        }

        for (id, func) in f.ctx.functions.index_value_iter() {
            f.write_str("\n\n")?;
            FuncDisplay(&func.0, Some(&func.1)).fmt(f, false, id)?;
        }

        for (id, func) in f.ctx.external_functions.index_value_iter() {
            f.write_str("\n\n")?;
            FuncDisplay(&func.0, func.1.as_ref()).fmt(f, true, id.cast())?;
        }

        for (id, static_value) in f.ctx.statics.index_value_iter() {
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
