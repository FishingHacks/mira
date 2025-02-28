use crate::typechecking::expression::TypedLiteral;

use super::formatter::{Formatter, INDENT_STR};

// TypedLiteralDisplay
#[repr(transparent)]
pub struct Tld<'a>(pub &'a TypedLiteral);

impl Tld<'_> {
    pub fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self.0 {
            TypedLiteral::Void => f.write_str("void"),
            TypedLiteral::Dynamic(id) => f.write_fmt(format_args!("_{id}")),
            TypedLiteral::Function(id) => {
                if let Some(name) = &f.ctx.functions[*id].0.name {
                    f.write_str("@name(")?;
                    f.write_debug(name)?;
                    f.write_str(") ")?;
                }
                f.write_str("fn_")?;
                f.write_value(id)
            }
            TypedLiteral::ExternalFunction(id) => {
                if let Some(name) = &f.ctx.external_functions[*id].0.name {
                    f.write_str("@name(")?;
                    f.write_debug(name)?;
                    f.write_str(") ")?;
                }
                f.write_str("ext_fn_")?;
                f.write_value(id)
            }
            TypedLiteral::Static(id) => f.write_fmt(format_args!("static_{id}")),
            TypedLiteral::String(global_str) => f.write_debug(global_str),
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
                f.write_debug(&f.ctx.structs[*id].name)?;
                f.write_str(") struct_")?;
                f.write_value(id)?;
                f.write_str(" {\n")?;
                for val in children.iter() {
                    f.write_str(INDENT_STR)?;
                    Tld(val).fmt(f)?;
                    f.write_str(",\n")?;
                }
                f.write_str("}")
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
            TypedLiteral::Intrinsic(intrinsic) => {
                f.write_str("Intrinsic::")?;
                f.write_value(intrinsic)
            }
        }
    }
}
