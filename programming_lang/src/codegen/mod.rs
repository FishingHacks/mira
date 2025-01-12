use context::DefaultTypes;
use core::panic;
use std::collections::HashMap;

use crate::{
    globals::GlobalStr,
    typechecking::{
        expression::{TypecheckedExpression, TypedLiteral},
        Type,
    },
};
pub use inkwell::context::Context as InkwellContext;
pub use inkwell::targets::TargetTriple;
pub mod mangling;
pub use context::CodegenContext;
pub use inkwell::support::LLVMString;
use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    module::Module,
    types::{AnyTypeEnum, BasicType, BasicTypeEnum, StructType},
    values::{BasicValueEnum, FunctionValue, GlobalValue},
    FloatPredicate, IntPredicate,
};

mod context;
mod error;

impl<'ctx, 'me> CodegenContext<'ctx> {
    pub fn make_function_codegen_context(
        &'me self,
        tc_scope: Vec<Type>,
        current_fn: FunctionValue<'ctx>,
    ) -> FunctionCodegenContext<'ctx, 'me> {
        FunctionCodegenContext {
            tc_scope,
            scope: Vec::new(),
            builder: &self.builder,
            context: self.context,
            default_types: self.default_types,
            module: &self.module,
            current_fn,
            functions: &self.functions,
            external_functions: &self.external_functions,
            structs: &self.structs,
            statics: &self.statics,
            string_map: &self.string_map,
        }
    }
}

pub struct FunctionCodegenContext<'ctx, 'codegen> {
    tc_scope: Vec<Type>,
    scope: Vec<BasicValueEnum<'ctx>>,
    builder: &'codegen Builder<'ctx>,
    context: &'codegen Context,
    default_types: DefaultTypes<'ctx>,
    module: &'codegen Module<'ctx>,
    current_fn: FunctionValue<'ctx>,
    functions: &'codegen Vec<FunctionValue<'ctx>>,
    external_functions: &'codegen Vec<FunctionValue<'ctx>>,
    structs: &'codegen Vec<StructType<'ctx>>,
    statics: &'codegen Vec<GlobalValue<'ctx>>,
    string_map: &'codegen HashMap<GlobalStr, GlobalValue<'ctx>>,
}

impl<'ctx> FunctionCodegenContext<'ctx, '_> {
    pub fn push_value(&mut self, id: usize, value: BasicValueEnum<'ctx>) {
        if self.scope.len() - 1 != id {
            panic!(
                "inserting values out-of-order, expected value _{}, but got value _{id}",
                self.scope.len() - 1
            );
        }
        self.scope.push(value);
    }
}

impl<'ctx> Type {
    fn to_llvm_basic_type(
        &self,
        default_types: &DefaultTypes<'ctx>,
        structs: &Vec<StructType<'ctx>>,
    ) -> BasicTypeEnum<'ctx> {
        if self.refcount() > 0 {
            if self.is_thin_ptr() {
                return default_types.ptr.into();
            } else {
                return default_types.fat_ptr.into();
            }
        }

        match self {
            Type::Generic(..) | Type::Trait { .. } => {
                unreachable!("generics should be resolved by now")
            }
            Type::UnsizedArray { .. } => panic!("llvm types must be sized, `[_]` is not"),
            Type::PrimitiveStr(_) => panic!("llvm types must be sized, `str` is not"),
            Type::PrimitiveSelf(_) => unreachable!("Self must be resolved at this point"),
            Type::DynType { .. } => panic!("llvm types must be sized, `dyn _` is not"),
            Type::Struct { struct_id, .. } => structs[*struct_id].into(),
            Type::SizedArray {
                typ,
                number_elements,
                ..
            } => typ
                .to_llvm_basic_type(default_types, structs)
                .array_type(*number_elements as u32)
                .into(),
            // our function types are always pointers because all function types are pointers in llvm
            Type::Function(..) => default_types.ptr.into(),
            Type::PrimitiveNever | Type::PrimitiveVoid(_) => panic!(
                "void and never should be ignored as llvm types outside of function return values"
            ),
            Type::PrimitiveU8(_) | Type::PrimitiveI8(_) => default_types.i8.into(),
            Type::PrimitiveU16(_) | Type::PrimitiveI16(_) => default_types.i16.into(),
            Type::PrimitiveU32(_) | Type::PrimitiveI32(_) => default_types.i32.into(),
            Type::PrimitiveU64(_) | Type::PrimitiveI64(_) => default_types.i64.into(),
            Type::PrimitiveUSize(_) | Type::PrimitiveISize(_) => default_types.isize.into(),
            Type::PrimitiveF32(_) => default_types.f32.into(),
            Type::PrimitiveF64(_) => default_types.f64.into(),
            Type::PrimitiveBool(_) => default_types.bool.into(),
        }
    }
}

fn is_value_const(v: &BasicValueEnum<'_>) -> bool {
    match v {
        BasicValueEnum::ArrayValue(v) => v.is_const(),
        BasicValueEnum::IntValue(v) => v.is_const(),
        BasicValueEnum::FloatValue(v) => v.is_const(),
        BasicValueEnum::PointerValue(v) => v.is_const(),
        BasicValueEnum::StructValue(v) => v.is_const(),
        BasicValueEnum::VectorValue(v) => v.is_const(),
    }
}

fn poison_val<'ctx>(v: BasicTypeEnum<'ctx>) -> BasicValueEnum<'ctx> {
    match v {
        BasicTypeEnum::ArrayType(v) => v.get_poison().into(),
        BasicTypeEnum::FloatType(v) => v.get_poison().into(),
        BasicTypeEnum::IntType(v) => v.get_poison().into(),
        BasicTypeEnum::PointerType(v) => v.get_poison().into(),
        BasicTypeEnum::StructType(v) => v.get_poison().into(),
        BasicTypeEnum::VectorType(v) => v.get_poison().into(),
    }
}

fn static_to_basic_type<'ctx>(static_value: GlobalValue<'ctx>) -> BasicTypeEnum<'ctx> {
    match static_value.get_value_type() {
        AnyTypeEnum::ArrayType(ty) => ty.into(),
        AnyTypeEnum::FloatType(ty) => ty.into(),
        AnyTypeEnum::FunctionType(_) => panic!("A static should never be a function"),
        AnyTypeEnum::IntType(ty) => ty.into(),
        AnyTypeEnum::PointerType(ty) => ty.into(),
        AnyTypeEnum::StructType(ty) => ty.into(),
        AnyTypeEnum::VectorType(ty) => ty.into(),
        AnyTypeEnum::VoidType(_) => panic!("A static should never be void"),
    }
}

impl<'ctx> TypedLiteral {
    fn fn_ctx_to_basic_value(
        &self,
        ctx: &FunctionCodegenContext<'ctx, '_>,
    ) -> BasicValueEnum<'ctx> {
        self.to_basic_value(
            &ctx.scope,
            &ctx.default_types,
            ctx.structs,
            ctx.builder,
            ctx.statics,
            ctx.functions,
            ctx.external_functions,
            ctx.string_map,
        )
    }
    fn to_basic_value(
        &self,
        scope: &Vec<BasicValueEnum<'ctx>>,
        default_types: &DefaultTypes<'ctx>,
        structs: &Vec<StructType<'ctx>>,
        builder: &Builder<'ctx>,
        statics: &Vec<GlobalValue<'ctx>>,
        functions: &Vec<FunctionValue<'ctx>>,
        ext_functions: &Vec<FunctionValue<'ctx>>,
        string_map: &HashMap<GlobalStr, GlobalValue<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        match self {
            TypedLiteral::Void => panic!("void should be ignored in llvm"),
            TypedLiteral::Dynamic(id) => scope[*id],
            TypedLiteral::Function(id) => {
                functions[*id].as_global_value().as_pointer_value().into()
            }
            TypedLiteral::ExternalFunction(id) => ext_functions[*id]
                .as_global_value()
                .as_pointer_value()
                .into(),
            TypedLiteral::Static(id) => {
                let static_value = statics[*id];
                builder
                    .build_load(
                        static_to_basic_type(static_value),
                        static_value.as_pointer_value().into(),
                        "",
                    )
                    .expect("the type should always match")
                    .into()
            }
            TypedLiteral::String(global_str) => {
                let ptr = string_map[global_str].as_pointer_value().into();
                let size = global_str.with(|v| v.len());
                let len_const = default_types.isize.const_int(size as u64, false).into();
                default_types
                    .fat_ptr
                    .const_named_struct(&[ptr, len_const])
                    .into()
            }
            TypedLiteral::Array(ty, elements) => {
                if elements.len() == 0 {
                    return ty
                        .to_llvm_basic_type(default_types, structs)
                        .array_type(0)
                        .const_zero()
                        .into();
                }
                let mut insert_value_vec: Vec<(usize, BasicValueEnum<'ctx>)> = Vec::new();

                macro_rules! array_const_value {
                    ($ty:expr,$into_val_fn:ident) => {{
                        let mut const_elements = Vec::new();
                        for (i, v) in elements.iter().enumerate() {
                            let val = v
                                .to_basic_value(
                                    scope,
                                    default_types,
                                    structs,
                                    builder,
                                    statics,
                                    functions,
                                    ext_functions,
                                    string_map,
                                )
                                .$into_val_fn();
                            if val.is_const() {
                                const_elements.push(val);
                            } else {
                                insert_value_vec.push((i, val.into()));
                                const_elements.push($ty.get_poison());
                            }
                        }
                        $ty.const_array(&const_elements)
                    }};
                }
                let mut const_value = match ty.to_llvm_basic_type(default_types, structs) {
                    BasicTypeEnum::ArrayType(array_type) => {
                        array_const_value!(array_type, into_array_value)
                    }
                    BasicTypeEnum::FloatType(float_type) => {
                        array_const_value!(float_type, into_float_value)
                    }
                    BasicTypeEnum::IntType(int_type) => {
                        array_const_value!(int_type, into_int_value)
                    }
                    BasicTypeEnum::PointerType(pointer_type) => {
                        array_const_value!(pointer_type, into_pointer_value)
                    }
                    BasicTypeEnum::StructType(struct_type) => {
                        array_const_value!(struct_type, into_struct_value)
                    }
                    BasicTypeEnum::VectorType(vector_type) => {
                        array_const_value!(vector_type, into_vector_value)
                    }
                };

                for v in insert_value_vec.drain(..) {
                    const_value = builder
                        .build_insert_value(const_value, v.1, v.0 as u32, "")
                        .expect("integer should never be out of range")
                        .into_array_value();
                }
                const_value.into()
            }
            TypedLiteral::Struct(struct_id, vec) => {
                if vec.len() < 1 {
                    return structs[*struct_id].const_named_struct(&[]).into();
                }
                let mut non_const_value = Vec::new();
                let mut const_value = structs[*struct_id].const_named_struct(
                    &vec.iter()
                        .enumerate()
                        .map(|(i, v)| {
                            let val = v.to_basic_value(
                                scope,
                                default_types,
                                structs,
                                builder,
                                statics,
                                functions,
                                ext_functions,
                                string_map,
                            );
                            if is_value_const(&val) {
                                val
                            } else {
                                let poison_val = poison_val(val.get_type());
                                non_const_value.push((i, val));
                                poison_val
                            }
                        })
                        .collect::<Vec<_>>(),
                );
                for v in non_const_value.drain(..) {
                    const_value = builder
                        .build_insert_value(const_value, v.1, v.0 as u32, "")
                        .expect("integer should never be out of range")
                        .into_struct_value();
                }
                const_value.into()
            }
            TypedLiteral::F64(v) => default_types.f64.const_float(*v).into(),
            TypedLiteral::F32(v) => default_types.f32.const_float(*v as f64).into(),
            TypedLiteral::U8(v) => default_types.i8.const_int(*v as u64, false).into(),
            TypedLiteral::U16(v) => default_types.i16.const_int(*v as u64, false).into(),
            TypedLiteral::U32(v) => default_types.i32.const_int(*v as u64, false).into(),
            TypedLiteral::U64(v) => default_types.i64.const_int(*v, false).into(),
            TypedLiteral::USize(v) => default_types.isize.const_int(*v as u64, false).into(),
            TypedLiteral::I8(v) => default_types.i8.const_int(*v as u64, false).into(),
            TypedLiteral::I16(v) => default_types.i16.const_int(*v as u64, false).into(),
            TypedLiteral::I32(v) => default_types.i32.const_int(*v as u64, false).into(),
            TypedLiteral::I64(v) => default_types.i64.const_int(*v as u64, false).into(),
            TypedLiteral::ISize(v) => default_types.isize.const_int(*v as u64, false).into(),
            TypedLiteral::Bool(v) => default_types.bool.const_int(*v as u64, false).into(),
            TypedLiteral::Intrinsic(intrinsic) => {
                unreachable!("intrinsics should've been resolved by now")
            }
        }
    }
}

macro_rules! f_s_u {
    ($ctx:expr, $typ: expr, $lhs: expr, $rhs: expr, $sint: ident($($sint_val:expr),*), $uint: ident($($uint_val:expr),*), $float: ident($($float_val:expr),*), $err:literal) => {
        match $typ {
            Type::PrimitiveI8(0)
            | Type::PrimitiveI16(0)
            | Type::PrimitiveI32(0)
            | Type::PrimitiveI64(0)
            | Type::PrimitiveISize(0) => $ctx
                .builder
                .$sint($($sint_val,)* $lhs.into_int_value(), $rhs.into_int_value(), "")?
                .into(),
            Type::PrimitiveU8(0)
            | Type::PrimitiveU16(0)
            | Type::PrimitiveU32(0)
            | Type::PrimitiveU64(0)
            | Type::PrimitiveUSize(0) => $ctx
                .builder
                .$uint($($uint_val,)* $lhs.into_int_value(), $rhs.into_int_value(), "")?
                .into(),
            Type::PrimitiveF32(0) | Type::PrimitiveF64(0) => $ctx
                .builder
                .$float($($float_val,)* $lhs.into_float_value(), $rhs.into_float_value(), "")?
                .into(),
            _ => unreachable!($err),
        }
    };
    (with_bool $ctx:expr, $typ: expr, $lhs: expr, $rhs: expr, $sint: ident, $uint: ident, $float: ident, $bool: ident, $err:literal) => {
        match $typ {
            Type::PrimitiveI8(0)
            | Type::PrimitiveI16(0)
            | Type::PrimitiveI32(0)
            | Type::PrimitiveI64(0)
            | Type::PrimitiveISize(0) => $ctx
                .builder
                .$sint($lhs.into_int_value(), $rhs.into_int_value(), "")?
                .into(),
            Type::PrimitiveU8(0)
            | Type::PrimitiveU16(0)
            | Type::PrimitiveU32(0)
            | Type::PrimitiveU64(0)
            | Type::PrimitiveUSize(0) => $ctx
                .builder
                .$uint($lhs.into_int_value(), $rhs.into_int_value(), "")?
                .into(),
            Type::PrimitiveF32(0) | Type::PrimitiveF64(0) => $ctx
                .builder
                .$float($lhs.into_float_value(), $rhs.into_float_value(), "")?
                .into(),
            Type::PrimitiveBool(0) => $ctx
                .builder
                .$bool($lhs.into_int_value(), $rhs.into_int_value(), "")?
                .into(),
            _ => unreachable!($err),
        }
    };
}

impl TypecheckedExpression {
    fn build(&self, ctx: &mut FunctionCodegenContext) -> Result<(), BuilderError> {
        match self {
            TypecheckedExpression::Return(location, typed_literal) => {
                match typed_literal {
                    TypedLiteral::Void => ctx.builder.build_return(None)?,
                    TypedLiteral::Intrinsic(_) => unreachable!("intrinsic"),
                    lit => ctx
                        .builder
                        .build_return(Some(&lit.fn_ctx_to_basic_value(ctx)))?,
                };
                Ok(())
            }
            TypecheckedExpression::Block(location, child, annotations) => {
                for c in child {
                    c.build(ctx)?;
                }
                Ok(())
            }
            TypecheckedExpression::If {
                loc,
                cond,
                if_block,
                else_block: None,
                annotations,
            } => {
                let if_basic_block = ctx.context.append_basic_block(ctx.current_fn, "then");
                let end_basic_block = ctx.context.append_basic_block(ctx.current_fn, "endif");
                ctx.builder.build_conditional_branch(
                    cond.fn_ctx_to_basic_value(ctx).into_int_value(),
                    if_basic_block,
                    end_basic_block,
                )?;
                ctx.builder.position_at_end(if_basic_block);
                for expr in if_block {
                    expr.build(ctx)?;
                }
                ctx.builder.build_unconditional_branch(end_basic_block)?;
                Ok(())
            }

            TypecheckedExpression::If {
                loc,
                cond,
                if_block,
                else_block: Some(else_block),
                annotations,
            } => {
                let if_basic_block = ctx.context.append_basic_block(ctx.current_fn, "then");
                let else_basic_block = ctx.context.append_basic_block(ctx.current_fn, "else");
                let end_basic_block = ctx.context.append_basic_block(ctx.current_fn, "endif");
                ctx.builder.build_conditional_branch(
                    cond.fn_ctx_to_basic_value(ctx).into_int_value(),
                    if_basic_block,
                    else_basic_block,
                )?;
                ctx.builder.position_at_end(if_basic_block);
                for expr in if_block {
                    expr.build(ctx)?;
                }
                ctx.builder.build_unconditional_branch(end_basic_block)?;
                ctx.builder.position_at_end(else_basic_block);
                for expr in else_block {
                    expr.build(ctx)?;
                }
                ctx.builder.build_unconditional_branch(end_basic_block)?;
                Ok(())
            }
            TypecheckedExpression::While {
                loc,
                cond_block,
                cond,
                body,
            } => todo!(),
            TypecheckedExpression::Range {
                location,
                typ,
                lhs,
                rhs,
                inclusive,
                dst,
            } => todo!(),
            TypecheckedExpression::StoreAssignment(location, typed_literal, typed_literal1) => {
                todo!()
            }
            TypecheckedExpression::Call(location, typed_literal, typed_literal1, vec) => todo!(),
            TypecheckedExpression::DirectCall(location, typed_literal, _, vec) => todo!(),
            TypecheckedExpression::IntrinsicCall(location, typed_literal, intrinsic, vec) => {
                todo!()
            }
            TypecheckedExpression::Pos(location, dst, src) => {
                Ok(ctx.push_value(*dst, src.fn_ctx_to_basic_value(ctx)))
            }
            TypecheckedExpression::Neg(location, typed_literal, typed_literal1) => todo!(),
            TypecheckedExpression::LNot(location, typed_literal, typed_literal1) => todo!(),
            TypecheckedExpression::BNot(location, typed_literal, typed_literal1) => todo!(),
            TypecheckedExpression::Add(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst];
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_nsw_add(),
                    build_int_nuw_add(),
                    build_float_add(),
                    "tc should have errored if you try to add 2 non-int/float values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::Sub(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst];
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_nsw_sub(),
                    build_int_nuw_sub(),
                    build_float_sub(),
                    "tc should have errored if you try to subtract 2 non-int/float values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::Mul(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst];
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_nsw_mul(),
                    build_int_nuw_mul(),
                    build_float_mul(),
                    "tc should have errored if you try to multiply 2 non-int/float values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::Div(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst];
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_signed_div(),
                    build_int_unsigned_div(),
                    build_float_div(),
                    "tc should have errored if you try to divide 2 non-int/float values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::Mod(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst];
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_signed_rem(),
                    build_int_unsigned_rem(),
                    build_float_rem(),
                    "tc should have errored if you try to mod 2 non-int/float values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::BAnd(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst];
                if typ.is_int_like() || *typ == Type::PrimitiveBool(0) {
                    ctx.push_value(
                        *dst,
                        ctx.builder
                            .build_and(lhs.into_int_value(), rhs.into_int_value(), "")?
                            .into(),
                    );
                } else {
                    unreachable!(
                        "tc should have errored if you try to binary and 2 non-int/bool values"
                    );
                }
                Ok(())
            }
            TypecheckedExpression::BOr(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst];
                if typ.is_int_like() || *typ == Type::PrimitiveBool(0) {
                    ctx.push_value(
                        *dst,
                        ctx.builder
                            .build_or(lhs.into_int_value(), rhs.into_int_value(), "")?
                            .into(),
                    );
                } else {
                    unreachable!(
                        "tc should have errored if you try to binary or 2 non-int/bool values"
                    );
                }
                Ok(())
            }
            TypecheckedExpression::BXor(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst];
                if typ.is_int_like() || *typ == Type::PrimitiveBool(0) {
                    ctx.push_value(
                        *dst,
                        ctx.builder
                            .build_xor(lhs.into_int_value(), rhs.into_int_value(), "")?
                            .into(),
                    );
                } else {
                    unreachable!("tc should have errored if you try to xor 2 non-int/bool values");
                }
                Ok(())
            }
            TypecheckedExpression::GreaterThan(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst];
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_compare(IntPredicate::SGT),
                    build_int_compare(IntPredicate::UGT),
                    build_float_compare(FloatPredicate::UGT),
                    "tc should have errored if you try to compare 2 non-int/float values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::LessThan(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst];
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_compare(IntPredicate::SLT),
                    build_int_compare(IntPredicate::ULT),
                    build_float_compare(FloatPredicate::ULT),
                    "tc should have errored if you try to compare 2 non-int/float values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::LAnd(location, dst, lhs, rhs) => todo!(),
            TypecheckedExpression::LOr(location, dst, lhs, rhs) => todo!(),
            TypecheckedExpression::GreaterThanEq(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst];
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_compare(IntPredicate::SGE),
                    build_int_compare(IntPredicate::UGE),
                    build_float_compare(FloatPredicate::UGE),
                    "tc should have errored if you try to compare 2 non-int/float values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::LessThanEq(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst];
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_compare(IntPredicate::SLE),
                    build_int_compare(IntPredicate::ULE),
                    build_float_compare(FloatPredicate::ULE),
                    "tc should have errored if you try to compare 2 non-int/float values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::Eq(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst];
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_compare(IntPredicate::EQ),
                    build_int_compare(IntPredicate::EQ),
                    build_float_compare(FloatPredicate::UEQ),
                    "tc should have errored if you try to compare 2 non-int/float values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::Neq(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst];
                let v = f_s_u!(
                    ctx,
                    typ,
                    lhs,
                    rhs,
                    build_int_compare(IntPredicate::NE),
                    build_int_compare(IntPredicate::NE),
                    build_float_compare(FloatPredicate::UNE),
                    "tc should have errored if you try to compare 2 non-int/float values"
                );
                ctx.push_value(*dst, v);
                Ok(())
            }
            TypecheckedExpression::LShift(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst];
                if typ.is_int_like() {
                    ctx.push_value(
                        *dst,
                        ctx.builder
                            .build_left_shift(lhs.into_int_value(), rhs.into_int_value(), "")?
                            .into(),
                    );
                    Ok(())
                } else {
                    unreachable!("tc should have errored if you try to left shift a non-int value");
                }
            }
            TypecheckedExpression::RShift(location, dst, lhs, rhs) => {
                let lhs = lhs.fn_ctx_to_basic_value(ctx);
                let rhs = rhs.fn_ctx_to_basic_value(ctx);
                let typ = &ctx.tc_scope[*dst];
                if typ.is_int_like() {
                    ctx.push_value(
                        *dst,
                        ctx.builder
                            .build_right_shift(
                                lhs.into_int_value(),
                                rhs.into_int_value(),
                                false,
                                "",
                            )?
                            .into(),
                    );
                    Ok(())
                } else {
                    unreachable!(
                        "tc should have errored if you try to right shift a non-int value"
                    );
                }
            }
            // &void is a nullptr
            TypecheckedExpression::Reference(_, dst, TypedLiteral::Void) => {
                Ok(ctx.push_value(*dst, ctx.default_types.ptr.const_zero().into()))
            }
            TypecheckedExpression::Reference(location, typed_literal, typed_literal1) => todo!(),
            TypecheckedExpression::MakeUnsizedSlice(_, dst, src, sz) => {
                let fat_ptr_no_ptr = ctx.default_types.fat_ptr.const_named_struct(&[
                    ctx.default_types.ptr.get_poison().into(),
                    ctx.default_types.isize.const_int(*sz as u64, false).into(),
                ]);
                let fat_ptr = ctx
                    .builder
                    .build_insert_value(fat_ptr_no_ptr, src.fn_ctx_to_basic_value(ctx), 0, "")?
                    .into_struct_value();
                ctx.push_value(*dst, fat_ptr.into());

                Ok(())
            }
            TypecheckedExpression::Dereference(location, typed_literal, typed_literal1) => todo!(),
            // &struct, <- i64 0, i32 <idx>
            // &[a] <- extractvalue 0 and then i64 n of that
            // &[a; n] <- i64 0 n (for getelementptr [a; n]) or i64 n (for getelementptr a)
            TypecheckedExpression::Offset(location, dst, src, offset) => {
                todo!()
            }
            TypecheckedExpression::OffsetNonPointer(location, dst, src, offset_value) => {
                let src = src.fn_ctx_to_basic_value(ctx);
                if src.is_array_value() {
                    ctx.push_value(
                        *dst,
                        ctx.builder.build_extract_value(
                            src.into_array_value(),
                            *offset_value as u32,
                            "",
                        )?,
                    );
                } else if src.is_struct_value() {
                    ctx.push_value(
                        *dst,
                        ctx.builder.build_extract_value(
                            src.into_struct_value(),
                            *offset_value as u32,
                            "",
                        )?,
                    );
                } else {
                    panic!("offsetnonptr should never be used with a src element that is not an aggregate value")
                }
                Ok(())
            }
            TypecheckedExpression::TraitCall(
                location,
                typed_literal,
                typed_literal1,
                _,
                global_str,
            ) => todo!(),
            TypecheckedExpression::Alias(location, lhs, rhs) => {
                Ok(ctx.push_value(*lhs, ctx.scope[*rhs]))
            }
            TypecheckedExpression::Literal(location, dst, src) => {
                Ok(ctx.push_value(*dst, src.fn_ctx_to_basic_value(ctx)))
            }
            TypecheckedExpression::Empty(location) => Ok(()),
            TypecheckedExpression::None => {
                unreachable!("None-expressions are not valid and indicate an error")
            }
        }
    }
}
