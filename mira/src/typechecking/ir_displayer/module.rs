use crate::{module::ModuleScopeValue, typechecking::TypecheckedModule};

use super::formatter::Formatter;

#[repr(transparent)]
pub struct ModuleDisplay<'a>(pub &'a TypecheckedModule);

impl ModuleDisplay<'_> {
    pub fn fmt(&self, f: &mut Formatter, id: usize) -> std::fmt::Result {
        f.write_str("@root_path(")?;
        f.write_debug(&self.0.root)?;
        f.write_str(")\nmodule mod_")?;
        f.write_value(&id)?;
        f.write_char(' ')?;
        f.write_debug(&self.0.path)?;
        f.write_str(" {")?;
        f.push_indent();

        if !self.0.scope.is_empty() {
            f.write_str("\nscope:")?;
            f.push_indent();
            for entry in self.0.scope.iter() {
                f.write_char('\n')?;
                f.write_value(&entry.0)?;
                f.write_str(" : ")?;
                fmt_module_scope_value(entry.1, f)?;
            }
            f.pop_indent();
        }

        if !self.0.exports.is_empty() {
            f.write_str("\nexports:")?;
            f.push_indent();
            for (name, exported_name) in self.0.exports.iter() {
                f.write_char('\n')?;
                f.write_value(name)?;
                if name != exported_name {
                    f.write_str(" as ")?;
                    f.write_value(exported_name)?;
                }
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

fn fmt_module_scope_value(value: &ModuleScopeValue, f: &mut Formatter) -> std::fmt::Result {
    match value {
        ModuleScopeValue::Function(id) => {
            f.write_str("fn_")?;
            f.write_value(id)
        }
        ModuleScopeValue::ExternalFunction(id) => {
            f.write_str("external fn_")?;
            f.write_value(id)
        }
        ModuleScopeValue::Struct(id) => {
            f.write_str("struct_")?;
            f.write_value(id)
        }
        ModuleScopeValue::Static(id) => {
            f.write_str("static_")?;
            f.write_value(id)
        }
        ModuleScopeValue::Module(id) => {
            f.write_str("module_")?;
            f.write_value(id)
        }
        ModuleScopeValue::Trait(id) => {
            f.write_str("trait_")?;
            f.write_value(id)
        }
    }
}
