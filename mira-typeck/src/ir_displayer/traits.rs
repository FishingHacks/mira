use mira_lexer::token::IdentDisplay;

use crate::{TypedTrait, default_types};

use super::Formatter;

pub(super) struct TraitDisplay<'a>(pub &'a TypedTrait<'a>);

impl TraitDisplay<'_> {
    pub(super) fn fmt(&self, f: &mut Formatter<'_, '_, '_>) -> std::fmt::Result {
        f.write_value(&self.0.annotations)?;
        f.write_str("trait trait_")?;
        f.write_value(&self.0.id.to_usize())?;
        f.write_char(' ')?;
        f.write_value(&IdentDisplay(self.0.name.symbol()))?;
        f.write_str(" {")?;
        f.push_indent();
        for (name, args, return_type, annotations, _, _) in self.0.functions.iter() {
            f.write_char('\n')?;
            f.write_value(annotations)?;
            f.write_str("fn ")?;
            f.write_value(&IdentDisplay(name.symbol()))?;
            f.write_char('(')?;
            for (idx, name, ty) in args.iter().enumerate().map(|(a, (b, c))| (a, b, c)) {
                if idx != 0 {
                    f.write_str(", ")?;
                }
                f.write_value(&IdentDisplay(name.symbol()))?;
                f.write_str(": ")?;
                f.write_value(ty)?;
            }
            f.write_char(')')?;
            if *return_type != default_types::void {
                f.write_str(" -> ")?;
                f.write_value(return_type)?;
            }
            f.write_char(';')?;
        }
        f.pop_indent();
        f.write_str("\n}")
    }
}
