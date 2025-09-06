use mira_lexer::token::IdentDisplay;

use crate::ir::TypedLiteral;

use super::formatter::Formatter;

// TypedLiteralDisplay
#[repr(transparent)]
pub(super) struct Tld<'a>(pub &'a TypedLiteral<'a>);

impl Tld<'_> {
    pub(super) fn fmt(&self, f: &mut Formatter<'_, '_, '_>) -> std::fmt::Result {
        match self.0 {
            TypedLiteral::Void => f.write_str("void"),
            TypedLiteral::Dynamic(id) => f.write_value(id),
            TypedLiteral::Function(id, _) => {
                if let Some(name) = &f.ctx.functions[*id].0.name {
                    f.write_str("@name(")?;
                    f.write_value(&IdentDisplay(name.symbol()))?;
                    f.write_str(") ")?;
                }
                f.write_str("fn_")?;
                f.write_value(id)
            }
            TypedLiteral::ExternalFunction(id) => {
                if let Some(name) = &f.ctx.external_functions[*id].0.name {
                    f.write_str("@name(")?;
                    f.write_value(&IdentDisplay(name.symbol()))?;
                    f.write_str(") ")?;
                }
                f.write_str("ext_fn_")?;
                f.write_value(id)
            }
            TypedLiteral::Static(id) => f.write_fmt(format_args!("static_{id}")),
            TypedLiteral::String(sym) => f.write_debug(sym),
            TypedLiteral::Array(_, elements) => {
                f.write_char('[')?;
                for (idx, v) in elements.iter().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }
                    Tld(v).fmt(f)?;
                }
                f.write_char(']')
            }
            TypedLiteral::ArrayInit(_, elem, amount) => {
                f.write_char('[')?;
                Tld(elem).fmt(f)?;
                f.write_str("; ")?;
                f.write_value(amount)?;
                f.write_char(']')
            }
            TypedLiteral::Struct(id, children) => {
                f.write_str("@name(")?;
                f.write_value(&IdentDisplay(f.ctx.structs[*id].name.symbol()))?;
                f.write_str(") struct_")?;
                f.write_value(id)?;
                f.write_str(" {")?;
                f.push_indent();
                for (idx, val) in children.iter().enumerate() {
                    f.write_char('\n')?;
                    if let Some(field_name) = f.ctx.structs.get(id).map(|v| &v.elements[idx].0) {
                        f.write_value(field_name)?;
                        f.write_str(": ")?;
                    }
                    Tld(val).fmt(f)?;
                    f.write_char(',')?;
                }
                f.pop_indent();
                f.write_str("\n}")
            }
            TypedLiteral::Tuple(elements) => {
                f.write_str(".[")?;
                for (idx, v) in elements.iter().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }
                    Tld(v).fmt(f)?;
                }
                f.write_char(']')
            }
            TypedLiteral::F64(v) => f.write_value(v),
            TypedLiteral::F32(v) => f.write_value(v),
            TypedLiteral::U8(v) => f.write_value(v),
            TypedLiteral::U16(v) => f.write_value(v),
            TypedLiteral::U32(v) => f.write_value(v),
            TypedLiteral::U64(v) => f.write_value(v),
            TypedLiteral::USize(v) => f.write_value(v),
            TypedLiteral::I8(v) => f.write_value(v),
            TypedLiteral::I16(v) => f.write_value(v),
            TypedLiteral::I32(v) => f.write_value(v),
            TypedLiteral::I64(v) => f.write_value(v),
            TypedLiteral::ISize(v) => f.write_value(v),
            TypedLiteral::Bool(v) => f.write_value(v),
            TypedLiteral::LLVMIntrinsic(intrinsic) => {
                f.write_fmt(format_args!("llvm::{}", IdentDisplay(*intrinsic)))
            }
            TypedLiteral::Intrinsic(intrinsic, _) => {
                f.write_str("Intrinsic::")?;
                f.write_value(intrinsic)
            }
        }
    }
}
