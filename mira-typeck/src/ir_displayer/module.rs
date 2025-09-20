use crate::TypedModule;
use mira_lexer::token::IdentDisplay;
use mira_parser::module::{ModuleId, ModuleScopeValue};

use super::formatter::Formatter;

#[repr(transparent)]
pub(super) struct ModuleDisplay<'a>(pub &'a TypedModule<'a>);

impl ModuleDisplay<'_> {
    pub(super) fn fmt(&self, f: &mut Formatter<'_, '_, '_>, id: ModuleId) -> std::fmt::Result {
        f.write_str("@root_path(")?;
        f.write_debug(&self.0.file.package_root)?;
        f.write_str(")\nmodule mod_")?;
        f.write_value(&id.to_usize())?;
        f.write_char(' ')?;
        f.write_value(&self.0.file.path.display())?;
        f.write_str(" {")?;
        f.push_indent();

        if !self.0.scope.is_empty() {
            f.write_str("\nscope:")?;
            f.push_indent();
            for entry in self.0.scope.iter() {
                f.write_char('\n')?;
                f.write_value(&IdentDisplay(entry.0.symbol()))?;
                f.write_str(" : ")?;
                fmt_module_scope_value(entry.1, f)?;
            }
            f.pop_indent();
        }

        if !self.0.exports.is_empty() {
            f.write_str("\nexports:")?;
            f.push_indent();
            for name in self.0.exports.iter() {
                f.write_char('\n')?;
                f.write_value(&IdentDisplay(name.symbol()))?;
                if let Some(value) = self.0.scope.get(name) {
                    f.write_str(" (")?;
                    fmt_module_scope_value(value, f)?;
                    f.write_char(')')?;
                }
            }
            f.pop_indent();
        }

        if !self.0.assembly.is_empty() {
            f.write_str("\nasssembly:")?;
            f.push_indent();
            for asm in self.0.assembly.iter().flat_map(|v| v.1.split('\n')) {
                f.write_char('\n')?;
                f.write_debug(&asm)?;
            }
            f.pop_indent();
        }

        f.pop_indent();
        f.write_str("\n}")
    }
}

fn fmt_module_scope_value(
    value: &ModuleScopeValue,
    f: &mut Formatter<'_, '_, '_>,
) -> std::fmt::Result {
    match value {
        ModuleScopeValue::Function(id) => {
            f.write_str("fn_")?;
            f.write_value(&id.to_usize())
        }
        ModuleScopeValue::ExternalFunction(id) => {
            f.write_str("external fn_")?;
            f.write_value(&id.to_usize())
        }
        ModuleScopeValue::Struct(id) => {
            f.write_str("struct_")?;
            f.write_value(&id.to_usize())
        }
        ModuleScopeValue::Static(id) => {
            f.write_str("static_")?;
            f.write_value(&id.to_usize())
        }
        ModuleScopeValue::Module(id) => {
            f.write_str("module_")?;
            f.write_value(&id.to_usize())
        }
        ModuleScopeValue::Trait(id) => {
            f.write_str("trait_")?;
            f.write_value(&id.to_usize())
        }
    }
}
