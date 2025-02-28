use crate::typechecking::ir_displayer::{function::FuncDisplay, module::ModuleDisplay};

use super::formatter::Formatter;

#[derive(Clone, Copy)]
pub struct TCContextDisplay;

impl TCContextDisplay {
    pub fn fmt(self, f: &mut Formatter) -> std::fmt::Result {
        for (id, module) in f.ctx.modules.iter().enumerate() {
            ModuleDisplay(module).fmt(f, id)?;
            f.write_str("\n\n")?;
        }

        for (id, func) in f.ctx.functions.iter().enumerate() {
            FuncDisplay(&func.0, Some(&func.1)).fmt(f, false, id)?;
            f.write_str("\n\n")?;
        }

        for (id, func) in f.ctx.external_functions.iter().enumerate() {
            FuncDisplay(&func.0, func.1.as_deref()).fmt(f, true, id)?;
            f.write_str("\n\n")?;
        }
        todo!()
    }
}
