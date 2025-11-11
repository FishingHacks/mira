use crate::{
    TypedFunctionContract, default_types,
    ir::{IR, TypedExpression},
    ir_displayer::expressions::ExpressionDisplay,
};
use mira_lexer::token::IdentDisplay;

use super::formatter::Formatter;

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
            match self.0.context {
                mira_parser::module::FunctionContext::Freestanding => {}
                mira_parser::module::FunctionContext::StructFn(struct_id) => f.write_fmt(
                    format_args!("@child_of(struct({}))\n", struct_id.to_usize()),
                )?,
            }
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
        let exprs = ir.get_entry_block_exprs();
        let block_id = if exprs.len() == 1
            && let TypedExpression::Block(_, id, _) = &exprs[0]
        {
            *id
        } else {
            ir.get_entry_block()
        };

        let exprs = ir.get_block_exprs(block_id);

        f.write_char('{')?;
        f.push_indent();
        if !ir.scope().is_empty() {
            f.write_char('\n')?;
        }

        for (id, entry) in ir.scope().iter().enumerate() {
            f.write_fmt(format_args!("_{id}: {}\n", entry.ty))?;
        }

        for expr in exprs {
            f.write_char('\n')?;
            ExpressionDisplay(expr).fmt(f, ir)?;
        }
        f.pop_indent();
        f.write_str("\n}")
    }
}
