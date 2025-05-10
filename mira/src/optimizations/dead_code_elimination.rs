use std::collections::HashSet;

use crate::{
    store::StoreKey,
    typechecking::{
        expression::{TypecheckedExpression, TypedLiteral},
        TypecheckingContext, TypedFunction, TypedStatic,
    },
};

struct DceContext<'tc> {
    used_functions: HashSet<StoreKey<TypedFunction>>,
    used_statics: HashSet<StoreKey<TypedStatic>>,
    funcs_left: HashSet<StoreKey<TypedFunction>>,
    tc_ctx: &'tc TypecheckingContext,
}

pub fn run_dce(
    tc_ctx: &TypecheckingContext,
    used_funcs: &[StoreKey<TypedFunction>],
    used_statics: &[StoreKey<TypedStatic>],
) {
    let mut ctx = DceContext {
        used_functions: HashSet::new(),
        used_statics: HashSet::new(),
        funcs_left: HashSet::new(),
        tc_ctx,
    };

    for key in used_statics.iter().copied() {
        ctx.used_statics.insert(key);
    }

    for func in used_funcs {
        ctx.funcs_left.insert(*func);
    }
    for (_, ext_fn_body) in ctx.tc_ctx.external_functions.read().iter() {
        let Some(body) = ext_fn_body else { continue };
        run_block(body, &mut ctx);
    }
    while let Some(func) = ctx.funcs_left.iter().next().copied() {
        if !ctx.used_functions.contains(&func) {
            run_fn(func, &mut ctx);
        }
    }

    tc_ctx
        .functions
        .write()
        .retain(|k, _| ctx.used_functions.contains(k));
    tc_ctx
        .statics
        .write()
        .retain(|k, _| ctx.used_statics.contains(k));
}

fn run_fn(func: StoreKey<TypedFunction>, ctx: &mut DceContext) {
    ctx.funcs_left.remove(&func);
    ctx.used_functions.insert(func);
    let func_reader = ctx.tc_ctx.functions.read();
    let Some((_, func_body)) = func_reader.get(&func) else {
        return;
    };
    run_block(func_body, ctx);
}

fn run_block(block: &[TypecheckedExpression], ctx: &mut DceContext) {
    for expr in block {
        match expr {
            // === Blocks ===
            TypecheckedExpression::If {
                cond,
                if_block,
                else_block,
                ..
            } => {
                run_literal(cond, ctx);
                run_block(&if_block.0, ctx);
                if let Some((else_block, _)) = else_block {
                    run_block(else_block, ctx);
                }
            }
            TypecheckedExpression::While {
                cond_block,
                cond,
                body,
                ..
            } => {
                run_block(cond_block, ctx);
                run_literal(cond, ctx);
                run_block(&body.0, ctx);
            }
            TypecheckedExpression::Block(_, block, _) => run_block(block, ctx),

            // === Direct call ===
            TypecheckedExpression::DirectCall(_, _, func, lits, _) => {
                if !ctx.used_functions.contains(func) {
                    ctx.funcs_left.insert(*func);
                }
                for lit in lits {
                    run_literal(lit, ctx);
                }
            }
            // === call ===
            TypecheckedExpression::Call(_, _, lit1, lits) => {
                run_literal(lit1, ctx);
                for lit in lits {
                    run_literal(lit, ctx);
                }
            }

            // === single literal ===
            TypecheckedExpression::Reference(_, _, lit)
            | TypecheckedExpression::Dereference(_, _, lit)
            | TypecheckedExpression::Offset(_, _, lit, _)
            | TypecheckedExpression::OffsetNonPointer(_, _, lit, _)
            | TypecheckedExpression::Literal(_, _, lit)
            | TypecheckedExpression::Alias(_, _, lit)
            | TypecheckedExpression::Bitcast(_, _, lit)
            | TypecheckedExpression::IntCast(_, _, lit)
            | TypecheckedExpression::PtrToInt(_, _, lit)
            | TypecheckedExpression::IntToPtr(_, _, lit)
            | TypecheckedExpression::StripMetadata(_, _, lit)
            | TypecheckedExpression::MakeUnsizedSlice(_, _, lit, _)
            | TypecheckedExpression::AttachVtable(_, _, lit, _)
            | TypecheckedExpression::Return(_, lit)
            | TypecheckedExpression::Pos(_, _, lit)
            | TypecheckedExpression::Neg(_, _, lit)
            | TypecheckedExpression::LNot(_, _, lit)
            | TypecheckedExpression::BNot(_, _, lit) => run_literal(lit, ctx),
            // === double literal ===
            TypecheckedExpression::Range {
                lhs: lit1,
                rhs: lit2,
                ..
            }
            | TypecheckedExpression::StoreAssignment(_, lit1, lit2)
            | TypecheckedExpression::Add(_, _, lit1, lit2)
            | TypecheckedExpression::Sub(_, _, lit1, lit2)
            | TypecheckedExpression::Mul(_, _, lit1, lit2)
            | TypecheckedExpression::Div(_, _, lit1, lit2)
            | TypecheckedExpression::Mod(_, _, lit1, lit2)
            | TypecheckedExpression::BAnd(_, _, lit1, lit2)
            | TypecheckedExpression::BOr(_, _, lit1, lit2)
            | TypecheckedExpression::BXor(_, _, lit1, lit2)
            | TypecheckedExpression::GreaterThan(_, _, lit1, lit2)
            | TypecheckedExpression::LessThan(_, _, lit1, lit2)
            | TypecheckedExpression::LAnd(_, _, lit1, lit2)
            | TypecheckedExpression::LOr(_, _, lit1, lit2)
            | TypecheckedExpression::GreaterThanEq(_, _, lit1, lit2)
            | TypecheckedExpression::LessThanEq(_, _, lit1, lit2)
            | TypecheckedExpression::Eq(_, _, lit1, lit2)
            | TypecheckedExpression::Neq(_, _, lit1, lit2)
            | TypecheckedExpression::LShift(_, _, lit1, lit2)
            | TypecheckedExpression::RShift(_, _, lit1, lit2) => {
                run_literal(lit1, ctx);
                run_literal(lit2, ctx);
            }
            // === mutli literal ===
            TypecheckedExpression::DirectExternCall(_, _, _, lits)
            | TypecheckedExpression::DynCall(_, _, lits, _)
            | TypecheckedExpression::IntrinsicCall(_, _, _, lits, _) => {
                for lit in lits {
                    run_literal(lit, ctx);
                }
            }

            // === no literals ===
            TypecheckedExpression::Asm { .. }
            | TypecheckedExpression::DeclareVariable(..)
            | TypecheckedExpression::Empty(..)
            | TypecheckedExpression::Unreachable(..)
            | TypecheckedExpression::None => (),
        }
    }
}

fn run_literal(literal: &TypedLiteral, ctx: &mut DceContext) {
    match literal {
        TypedLiteral::Function(func, _) => {
            if !ctx.used_functions.contains(func) {
                ctx.funcs_left.insert(*func);
            }
        }
        TypedLiteral::Static(static_key) => _ = ctx.used_statics.insert(*static_key),
        TypedLiteral::Array(_, literals)
        | TypedLiteral::Struct(_, literals)
        | TypedLiteral::Tuple(literals) => {
            for lit in literals {
                run_literal(lit, ctx);
            }
        }
        TypedLiteral::ArrayInit(_, literal, _) => run_literal(literal, ctx),

        _ => (),
    }
}
