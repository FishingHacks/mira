use crate::{TypedFunctionContract, default_types, ir::IR};
use mira_lexer::token::IdentDisplay;

use super::{expressions::write_implicit_block, formatter::Formatter};

pub(super) struct FuncDisplay<'a>(pub &'a TypedFunctionContract<'a>, pub Option<&'a IR<'a>>);

impl FuncDisplay<'_> {
    pub(super) fn fmt(
        &self,
        f: &mut Formatter<'_, '_, '_>,
        is_external: bool,
        id: usize,
    ) -> std::fmt::Result {
        for annotation in self.0.annotations.iter() {
            f.write_value(&annotation)?;
            f.write_char('\n')?;
        }
        if is_external {
            f.write_str("external fn ext_fn_")?;
        } else {
            f.write_str("fn fn_")?;
        }
        f.write_value(&id)?;
        if let Some(name) = &self.0.name {
            f.write_char(' ')?;
            f.write_value(&IdentDisplay(name.symbol()))?;
        }
        f.write_char('(')?;
        for (idx, arg) in self.0.arguments.iter().enumerate() {
            if idx != 0 {
                f.write_str(", ")?;
            }
            f.write_value(&arg.0)?;
            f.write_str(" @ _")?;
            f.write_value(&idx)?;
            f.write_str(": ")?;
            f.write_value(&arg.1)?;
        }
        f.write_char(')')?;
        if self.0.return_type != default_types::void {
            f.write_str(" -> ")?;
            f.write_value(&self.0.return_type)?;
        }
        let Some(ir) = self.1 else {
            return f.write_char(';');
        };
        f.write_char(' ')?;
        write_implicit_block(f, ir.get_entry_block(), ir)
    }
}
