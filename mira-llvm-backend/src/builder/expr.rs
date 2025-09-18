use inkwell::{
    builder::BuilderError,
    debug_info::DIScope,
    values::{BasicValue, BasicValueEnum},
};
use mira_typeck::ir::{TypedExpression, TypedLiteral};

use super::FunctionCodegenContext;

impl<'ctx, 'arena> FunctionCodegenContext<'ctx, 'arena, '_, '_, '_> {
    pub(crate) fn build_expr(
        &mut self,
        expr: &TypedExpression<'arena>,
        scope: DIScope<'ctx>,
    ) -> Result<(), BuilderError> {
        self.set_current_debug_location(self.ctx.debug_ctx.location(scope, expr.span()));
        match expr {
            // Control flow
            TypedExpression::Return(_, typed_literal) => {
                if matches!(typed_literal, TypedLiteral::Void) {
                    if self.current_block.get_terminator().is_some() {
                        println!(
                            "[WARN]: bb {} Has a return even tho a terminating instruction was already generated",
                            self.current_block.get_name().to_string_lossy(),
                        );
                        return Ok(());
                    }
                    return self.build_return(None).map(|_| ());
                }
                match typed_literal {
                    &TypedLiteral::Dynamic(id) if self.ir.get_ty(id).is_voidlike() => {
                        self.build_return(None)?
                    }
                    &TypedLiteral::Static(id)
                        if self.ctx.tc_ctx.statics.read()[id].ty.is_voidlike() =>
                    {
                        self.build_return(None)?
                    }
                    TypedLiteral::Void => self.build_return(None)?,
                    TypedLiteral::Intrinsic(..) => unreachable!("intrinsic"),
                    lit => self.build_return(Some(&self.basic_value(lit)))?,
                };
                Ok(())
            }
            &TypedExpression::Block(_, block, _) => self.build_block(block, scope, true),
            TypedExpression::If {
                cond,
                if_block,
                else_block,
                ..
            } => self.build_if(cond, *if_block, *else_block, scope),
            TypedExpression::While {
                cond_block,
                cond,
                body,
                span: _,
            } => self.build_while(cond, *cond_block, *body, scope),

            // Ptr
            TypedExpression::StoreAssignment(_, ptr, value) => self.build_store_assign(ptr, value),
            TypedExpression::Reference(_, dst, value) => self.build_reference(*dst, value),
            TypedExpression::Dereference(_, dst, value) => self.build_dereference(*dst, value),

            // Call
            TypedExpression::Call(_, dst, func, args) => self.build_call(*dst, func, args),
            TypedExpression::DirectCall(_, dst, func, args, generics) => {
                // TODO: monomorphisation
                assert!(generics.is_empty());
                self.build_directcall(self.ctx.functions[*func], *dst, args)
            }
            TypedExpression::DirectExternCall(_, dst, func, args) => {
                self.build_directcall(self.ctx.external_functions[*func], *dst, args)
            }
            TypedExpression::LLVMIntrinsicCall(_, dst, sym, args) => {
                self.build_llvm_intrinsic_call(*dst, *sym, args)
            }
            TypedExpression::DynCall(_, dst, args, func_offset) => {
                self.build_dyncall(*dst, args, *func_offset)
            }
            TypedExpression::IntrinsicCall(_, dst, intrinsic, args, generics) => {
                self.build_intrinsic_call(*dst, *intrinsic, args, generics)
            }

            // Reassignment
            TypedExpression::Alias(_, dst, val)
            | TypedExpression::Literal(_, dst, val)
            | TypedExpression::Pos(_, dst, val) => {
                self.push_value(*dst, self.basic_value(val));
                Ok(())
            }

            // Unop
            TypedExpression::Neg(_, dst, val) => {
                let src = self.basic_value(val);
                let value = match src {
                    BasicValueEnum::IntValue(int_value) if int_value.is_constant_int() => {
                        int_value.const_neg().as_basic_value_enum()
                    }
                    BasicValueEnum::IntValue(int_value) => {
                        self.build_int_neg(int_value, "")?.as_basic_value_enum()
                    }
                    BasicValueEnum::FloatValue(float_value) => {
                        self.build_float_neg(float_value, "")?.as_basic_value_enum()
                    }
                    _ => unreachable!(),
                };
                self.push_value(*dst, value);
                Ok(())
            }
            TypedExpression::LNot(_, dst, val) | TypedExpression::BNot(_, dst, val) => {
                let src = self.basic_value(val).into_int_value();
                let v = if src.is_constant_int() {
                    src.const_not().as_basic_value_enum()
                } else {
                    self.build_not(src, "")?.as_basic_value_enum()
                };
                self.push_value(*dst, v);
                Ok(())
            }

            // Binop
            TypedExpression::Add(_, dst, lhs, rhs) => self.build_add(*dst, lhs, rhs),
            TypedExpression::Sub(_, dst, lhs, rhs) => self.build_sub(*dst, lhs, rhs),
            TypedExpression::Mul(_, dst, lhs, rhs) => self.build_mul(*dst, lhs, rhs),
            TypedExpression::Div(_, dst, lhs, rhs) => self.build_div(*dst, lhs, rhs),
            TypedExpression::Mod(_, dst, lhs, rhs) => self.build_rem(*dst, lhs, rhs),
            TypedExpression::BAnd(_, dst, lhs, rhs) => self.build_binary_and(*dst, lhs, rhs),
            TypedExpression::BOr(_, dst, lhs, rhs) => self.build_binary_or(*dst, lhs, rhs),
            TypedExpression::BXor(_, dst, lhs, rhs) => self.build_binary_xor(*dst, lhs, rhs),
            TypedExpression::LShift(_, dst, lhs, rhs) => self.build_lshift(*dst, lhs, rhs),
            TypedExpression::RShift(_, dst, lhs, rhs) => self.build_rshift(*dst, lhs, rhs),

            // Comparison
            TypedExpression::LAnd(_, dst, lhs, rhs, blk) => {
                self.build_logical_and(*dst, lhs, rhs, *blk, scope)
            }
            TypedExpression::LOr(_, dst, lhs, rhs, blk) => {
                self.build_logical_or(*dst, lhs, rhs, *blk, scope)
            }
            TypedExpression::GreaterThanEq(_, dst, lhs, rhs) => self.build_gte(*dst, lhs, rhs),
            TypedExpression::LessThanEq(_, dst, lhs, rhs) => self.build_lte(*dst, lhs, rhs),
            TypedExpression::GreaterThan(_, dst, lhs, rhs) => self.build_gt(*dst, lhs, rhs),
            TypedExpression::LessThan(_, dst, lhs, rhs) => self.build_lt(*dst, lhs, rhs),
            TypedExpression::Eq(_, dst, lhs, rhs) => self.build_eq(*dst, lhs, rhs),
            TypedExpression::Neq(_, dst, lhs, rhs) => self.build_neq(*dst, lhs, rhs),

            // Offset
            TypedExpression::Offset(_, dst, ptr, offset) => self.build_offset(*dst, ptr, *offset),
            TypedExpression::OffsetNonPointer(_, dst, value, offset) => {
                self.build_non_ptr_offset(*dst, value, *offset)
            }

            // Debug info
            TypedExpression::DeclareVariable(span, id, ty, name) => {
                let ty = self.substitute(*ty);
                let ptr = self.get_value_ptr(*id);
                self.ctx.debug_ctx.declare_variable(
                    ptr,
                    scope,
                    *span,
                    ty,
                    *name,
                    self.current_block,
                    self.module,
                    &self.ctx.tc_ctx.structs.read(),
                );
                Ok(())
            }

            // Casting
            TypedExpression::Bitcast(_, dst, val) => self.build_bitcast(*dst, val),
            TypedExpression::PtrToInt(_, dst, val) => self.build_ptrtoint(*dst, val),
            TypedExpression::IntToPtr(_, dst, val) => self.build_inttoptr(*dst, val),
            TypedExpression::IntCast(_, dst, val) => self.build_intcast(*dst, val),
            TypedExpression::StripMetadata(_, dst, val) => self.build_strip_metadata(*dst, val),
            TypedExpression::MakeUnsizedSlice(_, dst, val, len) => {
                self.build_unsize_slice(*dst, val, *len as u64)
            }
            TypedExpression::AttachVtable(_, dst, val, vtable_id) => {
                self.build_attach_vtable(*dst, val, vtable_id)
            }

            // Asm
            TypedExpression::Asm {
                dst,
                inputs,
                registers,
                volatile,
                asm,
                span: _,
            } => self.build_asm(*dst, inputs, registers, *volatile, asm),

            TypedExpression::Unreachable(_) => self.build_unreachable().map(|_| ()),

            TypedExpression::Empty(_) | TypedExpression::None => Ok(()),

            // TODO: raii
            TypedExpression::DropIf(..) | TypedExpression::Drop(..) => todo!(),
            TypedExpression::TraitCall { .. } => todo!(),
            TypedExpression::Range { .. } => todo!(),
        }
    }
}
