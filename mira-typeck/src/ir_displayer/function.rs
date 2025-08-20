use crate::{
    TypecheckedFunctionContract, TypedFunction, default_types,
    ir::{TypedExpression, TypedLiteral},
};
use mira_common::store::StoreKey;
use mira_lexer::token::IdentDisplay;

use super::{expressions::write_implicit_block, formatter::Formatter};

pub struct FuncDisplay<'a>(
    pub &'a TypecheckedFunctionContract<'a>,
    pub Option<&'a [TypedExpression<'a>]>,
);

impl FuncDisplay<'_> {
    pub fn fmt(
        &self,
        f: &mut Formatter,
        is_external: bool,
        id: StoreKey<TypedFunction>,
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
        let Some(mut exprs) = self.1 else {
            return Ok(());
        };
        if exprs
            .last()
            .map(|v| matches!(v, TypedExpression::Return(_, TypedLiteral::Void)))
            .unwrap_or(false)
        {
            exprs = &exprs[0..exprs.len() - 1];
        }
        write_implicit_block(f, exprs)
    }
}
