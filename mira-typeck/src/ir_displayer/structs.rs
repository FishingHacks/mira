use mira_lexer::token::IdentDisplay;

use crate::TypedStruct;

use super::Formatter;

pub struct StructDisplay<'a>(pub &'a TypedStruct<'a>);

impl StructDisplay<'_> {
    pub fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.write_value(&self.0.annotations)?;
        f.write_str("struct struct_")?;
        f.write_value(&self.0.id)?;
        f.write_char(' ')?;
        f.write_value(&IdentDisplay(self.0.name.symbol()))?;
        f.write_str(" {")?;
        f.push_indent();

        for (idx, name, ty) in self
            .0
            .elements
            .iter()
            .enumerate()
            .map(|(a, (b, c, _))| (a, b, c))
        {
            if idx != 0 {
                f.write_char(',')?;
            }
            f.write_char('\n')?;
            f.write_value(&IdentDisplay(name.symbol()))?;
            f.write_str(": ")?;
            f.write_value(ty)?;
        }

        if !self.0.global_impl.is_empty() || !self.0.trait_impl.is_empty() {
            f.write_str(";")?;
        }

        for (name, id) in self.0.global_impl.iter() {
            f.write_str("\nfn_")?;
            f.write_value(id)?;
            f.write_char(' ')?;
            f.write_value(&IdentDisplay(name.symbol()))?;
        }

        for (trait_id, funcs) in self.0.trait_impl.iter() {
            f.write_str("\nimpl trait_")?;
            f.write_value(trait_id)?;
            if let Some(name) = f.ctx.traits.get(trait_id).map(|v| &v.name) {
                f.write_char(' ')?;
                f.write_value(&IdentDisplay(name.symbol()))?;
            }
            f.write_str(" {")?;
            f.push_indent();
            for func_id in funcs {
                f.write_str("\nfn_")?;
                f.write_value(func_id)?;
                if let Some(name) = f.ctx.functions.get(func_id).and_then(|v| v.0.name.as_ref()) {
                    f.write_char(' ')?;
                    f.write_value(&IdentDisplay(name.symbol()))?;
                }
            }
            f.pop_indent();
            f.write_str("\n}")?;
        }

        f.pop_indent();
        f.write_str("\n}")
    }
}
