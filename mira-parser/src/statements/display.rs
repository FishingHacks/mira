use std::fmt::{Debug, Display, Write as _};

use super::{Argument, For, If, Statement, Trait, Variable, While};
use crate::{FunctionContract, TypeRef};

impl Display for Trait<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.annotations, f)?;

        f.write_str("trait ")?;
        Display::fmt(&self.name, f)?;
        f.write_str("{\n")?;

        for func in self.functions.iter() {
            Display::fmt(&func.annotations, f)?;
            f.write_str("    fn ")?;
            Display::fmt(&func.name, f)?;
            f.write_char('(')?;
            for (i, v) in func.args.iter().enumerate() {
                if i != 0 {
                    f.write_str(", ")?;
                }
                Display::fmt(v, f)?;
            }
            f.write_char(')')?;
            if !matches!(func.return_ty, TypeRef::Void(_, 0)) {
                f.write_str(" -> ")?;
                Display::fmt(&func.return_ty, f)?;
            }
            f.write_str(";\n")?;
        }

        f.write_char('}')
    }
}

impl Display for Statement<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => unreachable!(),
            Self::BakedFunction(id, _) => {
                f.write_fmt(format_args!("(module-fn {})", id.to_usize()))
            }
            Self::BakedExternalFunction(id, _) => {
                f.write_fmt(format_args!("(module-external-fn {})", id.to_usize()))
            }
            Self::BakedStruct(id, _) => {
                f.write_fmt(format_args!("(module-struct {})", id.to_usize()))
            }
            Self::BakedStatic(id, _) => {
                f.write_fmt(format_args!("(module-static {})", id.to_usize()))
            }
            Self::BakedTrait(id, _) => {
                f.write_fmt(format_args!("(module-trait {})", id.to_usize()))
            }

            Self::Trait(r#trait) => Display::fmt(&r#trait, f),
            Self::Var(Variable {
                name,
                value,
                ty: None,
                ..
            }) => f.write_fmt(format_args!("(var-assign {name} {value})")),
            Self::Var(Variable {
                name,
                value,
                ty: Some(ty),
                ..
            }) => f.write_fmt(format_args!("(var-assign {name} {ty} {value})")),
            Self::Static { var, public, .. } => {
                f.write_char('(')?;
                if *public {
                    f.write_str("pub ")?;
                }
                f.write_fmt(format_args!("static {} ", var.name))?;
                match &var.ty {
                    Some(v) => Display::fmt(v, f)?,
                    None => f.write_str("missing-ty")?,
                }
                f.write_fmt(format_args!(" {})", var.value))
            }
            Self::Block(stmts, _, annotations) => {
                Display::fmt(annotations, f)?;

                f.write_char('{')?;
                for stmt in &**stmts {
                    f.write_char('\n')?;
                    Display::fmt(stmt, f)?;
                }
                if !stmts.is_empty() {
                    // {} for empty block, for filled block:
                    // {
                    // statement1
                    // statement2
                    // }
                    f.write_char('\n')?;
                }
                f.write_char('}')
            }
            Self::DeferredBlock(stmts, _, annotations) => {
                Display::fmt(annotations, f)?;

                f.write_str("defer {")?;
                for stmt in &**stmts {
                    f.write_char('\n')?;
                    Display::fmt(stmt, f)?;
                }
                if !stmts.is_empty() {
                    // {} for empty block, for filled block:
                    // {
                    // statement1
                    // statement2
                    // }
                    f.write_char('\n')?;
                }
                f.write_char('}')
            }
            Self::Expr(v) => Display::fmt(v, f),
            Self::DeferredExpr(v) => f.write_fmt(format_args!("(defer {v})")),
            Self::Return(Some(v), _) => f.write_fmt(format_args!("(return {v})")),
            Self::Return(None, _) => f.write_str("(return null)"),
            Self::If(If {
                condition,
                if_stmt,
                else_stmt: Some(else_stmt),
                span: _,
                annotations,
            }) => f.write_fmt(format_args!(
                "{annotations}(if {condition} {if_stmt} {else_stmt})"
            )),
            Self::If(If {
                condition,
                if_stmt,
                else_stmt: None,
                span: _,
                annotations,
            }) => f.write_fmt(format_args!("{annotations}(if {condition} {if_stmt})")),
            Self::DeferredIf(If {
                condition,
                if_stmt,
                else_stmt: Some(else_stmt),
                span: _,
                annotations,
            }) => f.write_fmt(format_args!(
                "{annotations}(deferred_if {condition} {if_stmt} {else_stmt})"
            )),
            Self::DeferredIf(If {
                condition,
                if_stmt,
                else_stmt: None,
                span: _,
                annotations,
            }) => f.write_fmt(format_args!(
                "{annotations}(deferred_if {condition} {if_stmt})"
            )),
            Self::For(For {
                iterator,
                var_name,
                child,
                span: _,
                annotations,
            }) => f.write_fmt(format_args!(
                "{annotations}(for {var_name} {iterator} {child})"
            )),
            Self::DeferredFor(For {
                iterator,
                var_name,
                child,
                span: _,
                annotations,
            }) => f.write_fmt(format_args!(
                "{annotations}(deferred_for {var_name} {iterator} {child})"
            )),
            Self::While(While {
                condition,
                child,
                span: _,
                annotations,
            }) => f.write_fmt(format_args!("{annotations}(while {condition} {child})")),
            Self::DeferredWhile(While {
                condition,
                child,
                span: _,
                annotations,
            }) => f.write_fmt(format_args!(
                "{annotations}(deferred_while {condition} {child})"
            )),
            Self::Struct {
                name,
                elements,
                span: _,
                generics,
                global_impl,
                impls,
                ..
            } => {
                f.write_str("struct ")?;
                Display::fmt(name, f)?;
                if !generics.is_empty() {
                    f.write_char('<')?;
                    for (i, generic) in generics.iter().enumerate() {
                        if i != 0 {
                            f.write_str(", ")?;
                        }
                        Display::fmt(&generic.name, f)?;
                        if !generic.bounds.is_empty() {
                            for i in 0..generic.bounds.len() {
                                if i != 0 {
                                    f.write_str(" + ")?;
                                }
                                Display::fmt(&generic.bounds[i].0, f)?;
                            }
                        }
                    }
                    f.write_char('>')?;
                }
                f.write_str(" {\n")?;

                for arg_name in elements {
                    f.write_str("    ")?;
                    Display::fmt(&arg_name.0, f)?;
                    f.write_str(": ")?;
                    Display::fmt(&arg_name.1, f)?;
                    f.write_str(",\n")?;
                }

                if !global_impl.is_empty() || !impls.is_empty() {
                    f.write_str("\n")?;
                    for (contract, body) in global_impl.values() {
                        f.write_str("    ")?;
                        display_contract(f, contract, false)?;
                        f.write_char(' ')?;
                        Display::fmt(body, f)?;
                        f.write_str(")\n")?;
                    }

                    for (trait_name, impls, _) in impls {
                        f.write_str("    impl ")?;
                        f.write_str(trait_name)?;
                        f.write_str(" {\n")?;
                        for (contract, body) in impls.values() {
                            f.write_str("        ")?;
                            display_contract(f, contract, false)?;
                            f.write_char(' ')?;
                            Display::fmt(body, f)?;
                            f.write_str(")\n")?;
                        }
                        f.write_str("    }\n")?;
                    }
                }

                f.write_str("}")
            }
            Self::Function {
                contract,
                body,
                public,
                ..
            } => {
                f.write_str(public.then_some("pub ").unwrap_or_default())?;
                display_contract(f, contract, false)?;
                f.write_char(' ')?;
                Display::fmt(body, f)?;
                f.write_char(')')
            }
            Self::ExternalFunction {
                contract,
                body: None,
                public,
                ..
            } => {
                f.write_str(public.then_some("pub ").unwrap_or_default())?;
                display_contract(f, contract, true)?;
                f.write_char(')')
            }
            Self::ExternalFunction {
                contract,
                body: Some(body),
                public,
                ..
            } => {
                f.write_str(public.then_some("pub ").unwrap_or_default())?;
                display_contract(f, contract, true)?;
                f.write_char(' ')?;
                Display::fmt(body, f)?;
                f.write_char(')')
            }
            Self::ModuleAsm(_, asm) => {
                f.write_str("asm (")?;
                for asm in asm.split('\n') {
                    Debug::fmt(asm, f)?;
                    f.write_char('\n')?;
                }
                f.write_str(");")
            }
            Self::Use {
                path,
                alias,
                public,
                ..
            } => {
                if *public {
                    f.write_str("pub ")?;
                }
                f.write_str("use ")?;
                Display::fmt(path, f)?;
                if let Some(alias) = alias {
                    f.write_str(" as ")?;
                    f.write_str(alias)?;
                }
                f.write_char(';')
            }
            Self::Mod { name, public, .. } => {
                if *public {
                    f.write_str("pub ")?;
                }
                f.write_str("mod ")?;
                f.write_str(name)?;
                f.write_char(';')
            }
        }
    }
}

pub(crate) fn display_contract(
    f: &mut std::fmt::Formatter<'_>,
    contract: &FunctionContract<'_>,
    is_external: bool,
) -> std::fmt::Result {
    Display::fmt(&contract.annotations, f)?;

    if is_external {
        f.write_str("(external callable ")?;
    } else {
        f.write_str("(callable ")?;
    }

    if !contract.generics.is_empty() {
        f.write_char('<')?;
        for i in 0..contract.generics.len() {
            let generic = &contract.generics[i];
            if i != 0 {
                f.write_str(", ")?;
            }
            Display::fmt(&generic.name, f)?;
            if !generic.bounds.is_empty() {
                f.write_str(": ")?;
                for i in 0..generic.bounds.len() {
                    if i != 0 {
                        f.write_str(" + ")?;
                    }
                    Display::fmt(&generic.bounds[i].0, f)?;
                }
            }
        }
        f.write_str("> ")?;
    }

    if let Some(name) = &contract.name {
        Display::fmt(name, f)?;
        f.write_char(' ')?;
    }
    f.write_char('(')?;
    for i in 0..contract.arguments.len() {
        if i != 0 {
            f.write_char(' ')?;
        }
        Display::fmt(&contract.arguments[i], f)?;
    }
    f.write_char(')')?;
    if let TypeRef::Void(_, 0) = &contract.return_type {
    } else {
        f.write_str(" returns ")?;
        Display::fmt(&contract.return_type, f)?;
    }
    Ok(())
}

impl Display for Argument<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.name, f)?;
        f.write_str(": ")?;
        Display::fmt(&self.ty, f)
    }
}
