use inkwell::{
    builder::BuilderError,
    debug_info::DIScope,
    values::{BasicValue, BasicValueEnum},
};
use mira_typeck::ir::TypedExpression;

use crate::{FnInstance, abi::ArgumentType, builder::call::DirectCallTarget};

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
            TypedExpression::Return(_, None) => self.build_return(None).map(|_| ()),
            &TypedExpression::Return(_, Some(id)) => {
                let ptr = self.get_value_ptr(id);
                match self.return_ty {
                    ArgumentType::Regular(ty) => {
                        let value = self.build_load(ty, ptr, "")?;
                        self.build_return(Some(&value)).map(|_| ())
                    }
                    ArgumentType::SRet(_) => {
                        let ptrsize = self.ctx.default_types.isize.get_bit_width() / 8;
                        let (size, alignment) = self.ir.get_ty(id).size_and_alignment(
                            ptrsize as u64,
                            &self.structs_reader,
                            self.ty_cx(),
                        );
                        let size = self.ctx.default_types.i32.const_int(size, false);
                        self.build_memcpy(self.return_val, alignment, ptr, alignment, size)?;
                        self.build_return(None).map(|_| ())
                    }
                    ArgumentType::ByVal(_) => {
                        unreachable!("byval(_) is illegal for return arguments")
                    }
                    ArgumentType::None => {
                        unreachable!("ArgumenType::None for a non-void return type")
                    }
                }
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
            TypedExpression::DirectCall(_, dst, fn_id, args, generics) => {
                let instance = FnInstance::new_poly(*fn_id, self.substitute(*generics));
                // instantiate the function instance
                self.ctx.get_fn_instance(instance);
                self.build_directcall(DirectCallTarget::Normal(instance), *dst, args)
            }
            TypedExpression::DirectExternCall(_, dst, func, args) => {
                self.build_directcall(DirectCallTarget::Extern(*func), *dst, args)
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
                if self.is_stack_allocated(*dst) {
                    let alloca =
                        self.build_alloca(self.basic_ty(*self.substitute(self.get_ty(*dst))), "")?;
                    self.basic_value_ptr(val, alloca, true)?;
                    self.push_value_raw(*dst, alloca);
                } else {
                    self.push_value(*dst, self.basic_value(val));
                }
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
                    &self.structs_reader,
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

            TypedExpression::TraitCall { .. } => todo!(),
            TypedExpression::Range { .. } => todo!(),
        }
    }
}
