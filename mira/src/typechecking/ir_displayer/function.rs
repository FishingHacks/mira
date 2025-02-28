use crate::typechecking::{expression::TypecheckedExpression, Type, TypecheckedFunctionContract};

use super::{expressions::ExpressionDisplay, formatter::Formatter};

pub struct FuncDisplay<'a>(
    pub &'a TypecheckedFunctionContract,
    pub Option<&'a [TypecheckedExpression]>,
);

impl FuncDisplay<'_> {
    pub fn fmt(&self, f: &mut Formatter, is_external: bool, id: usize) -> std::fmt::Result {
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
            f.write_value(name)?;
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
        if !matches!(self.0.return_type, Type::PrimitiveVoid(0)) {
            f.write_str(" -> ")?;
            f.write_value(&self.0.return_type)?;
        }
        let Some(exprs) = self.1 else { return Ok(()) };
        f.write_str(" {")?;
        f.push_indent();
        for expr in exprs {
            f.write_char('\n')?;
            ExpressionDisplay(expr).fmt(f)?;
        }
        f.pop_indent();
        f.write_str("\n}")
    }
}
