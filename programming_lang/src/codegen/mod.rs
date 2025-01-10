use std::{
    fmt::Write,
    hash::{DefaultHasher, Hash, Hasher},
};

use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    module::Module,
    types::{BasicType, BasicTypeEnum, FloatType, IntType, PointerType, StructType},
    values::{BasicValueEnum, FunctionValue},
    FloatPredicate, IntPredicate,
};

use crate::{
    module::ModuleScopeValue,
    std_annotations::alias_annotation::ExternAliasAnnotation,
    typechecking::{
        expression::{TypecheckedExpression, TypedLiteral},
        Type, TypecheckingContext,
    },
};

pub fn mangle_name(ctx: &TypecheckingContext, item: ModuleScopeValue) -> String {
    match item {
        ModuleScopeValue::Function(id) => {
            let fn_reader = ctx.functions.read();
            let module_reader = ctx.modules.read();
            let module_id = fn_reader[id].0.module_id;
            let v = &module_reader[module_id];
            let path = v.path.strip_prefix(&v.root).unwrap_or(&v.path);
            let mut mangled_name = "_ZN".to_string();
            let mut tmp_escaped_name_part = String::new();
            for entry in path.components() {
                let std::path::Component::Normal(name) = entry else {
                    continue;
                };
                tmp_escaped_name_part.clear();
                for byte in name.as_encoded_bytes() {
                    match (*byte) as char {
                        'a'..='z' | 'A'..='Z' | '_' | '.' | '-' | '0'..='9' => {
                            tmp_escaped_name_part.push(*byte as char)
                        }
                        '<' => tmp_escaped_name_part.push_str("$LT$"),
                        '>' => tmp_escaped_name_part.push_str("$GT$"),
                        ',' => tmp_escaped_name_part.push_str("$C$"),
                        '(' => tmp_escaped_name_part.push_str("$PL$"),
                        ')' => tmp_escaped_name_part.push_str("$PR$"),
                        '{' => tmp_escaped_name_part.push_str("$CL$"),
                        '}' => tmp_escaped_name_part.push_str("$CR$"),
                        '$' => tmp_escaped_name_part.push_str("$D$"),
                        _ => tmp_escaped_name_part.push('_'),
                    }
                }
                write!(
                    mangled_name,
                    "{}{}",
                    tmp_escaped_name_part.len(),
                    tmp_escaped_name_part
                )
                .expect("writing to a string should never fail");
            }

            match fn_reader[id].0.name {
                None => mangled_name.push_str("23$CL$$CL$anon_fn$CR$$CR$"), // {{anon_fn}}
                Some(ref v) => v.with(|v| {
                    tmp_escaped_name_part.clear();
                    for c in v.chars() {
                        match c {
                            'a'..='z' | 'A'..='Z' | '_' | '.' | '-' | '0'..='9' => {
                                tmp_escaped_name_part.push(c)
                            }
                            '<' => tmp_escaped_name_part.push_str("$LT$"),
                            '>' => tmp_escaped_name_part.push_str("$GT$"),
                            ',' => tmp_escaped_name_part.push_str("$C$"),
                            '(' => tmp_escaped_name_part.push_str("$PL$"),
                            ')' => tmp_escaped_name_part.push_str("$PR$"),
                            '{' => tmp_escaped_name_part.push_str("$CL$"),
                            '}' => tmp_escaped_name_part.push_str("$CR$"),
                            '$' => tmp_escaped_name_part.push_str("$D$"),
                            _ => tmp_escaped_name_part.push('_'),
                        }
                    }
                    write!(
                        mangled_name,
                        "{}{}",
                        tmp_escaped_name_part.len(),
                        tmp_escaped_name_part
                    )
                    .expect("writing to a string should never fail");
                }),
            }
            mangled_name.push_str("17h"); // hash
            let mut hasher = DefaultHasher::new();
            fn_reader[id].0.hash(&mut hasher);
            write!(mangled_name, "{:x}", hasher.finish())
                .expect("writing to a string should never fail");

            mangled_name
        }
        ModuleScopeValue::ExternalFunction(id) => {
            let reader = &ctx.external_functions.read()[id].0;
            if let Some(v) = reader
                .annotations
                .get_first_annotation::<ExternAliasAnnotation>()
            {
                return v.0.to_string();
            }
            reader
                .name
                .as_ref()
                .expect("external functions need a name")
                .to_string()
        }
        ModuleScopeValue::Static(_)
        | ModuleScopeValue::Struct(_)
        | ModuleScopeValue::Module(_)
        | ModuleScopeValue::Trait(_) => unreachable!("does not have to be mangled"),
    }
}

struct DefaultTypes<'ctx> {
    isize: IntType<'ctx>,
    i8: IntType<'ctx>,
    i16: IntType<'ctx>,
    i32: IntType<'ctx>,
    i64: IntType<'ctx>,
    bool: IntType<'ctx>,
    ptr: PointerType<'ctx>,
    fat_ptr: StructType<'ctx>,
    f32: FloatType<'ctx>,
    f64: FloatType<'ctx>,
}

struct CodegenContext<'ctx> {
    tc_scope: Vec<Type>,
    scope: Vec<BasicValueEnum<'ctx>>,
    builder: Builder<'ctx>,
    context: &'ctx Context,
    default_types: DefaultTypes<'ctx>,
    module: Module<'ctx>,
    current_fn: FunctionValue<'ctx>,
}

impl<'ctx> CodegenContext<'ctx> {
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
    fn to_llvm_basic_type(&self, ctx: &CodegenContext<'ctx>) -> BasicTypeEnum<'ctx> {
        if self.refcount() > 0 {
            if self.is_thin_ptr() {
                return ctx.default_types.ptr.into();
            } else {
                return ctx.default_types.fat_ptr.into();
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
            Type::Struct {
                struct_id,
                num_references,
                ..
            } => todo!(),
            Type::SizedArray {
                typ,
                number_elements,
                ..
            } => typ
                .to_llvm_basic_type(ctx)
                .array_type(*number_elements as u32)
                .into(),
            // our function types are always pointers because all function types are pointers in llvm
            Type::Function(..) => ctx.default_types.ptr.into(),
            Type::PrimitiveNever | Type::PrimitiveVoid(_) => panic!(
                "void and never should be ignored as llvm types outside of function return values"
            ),
            Type::PrimitiveU8(_) | Type::PrimitiveI8(_) => ctx.default_types.i8.into(),
            Type::PrimitiveU16(_) | Type::PrimitiveI16(_) => ctx.default_types.i16.into(),
            Type::PrimitiveU32(_) | Type::PrimitiveI32(_) => ctx.default_types.i32.into(),
            Type::PrimitiveU64(_) | Type::PrimitiveI64(_) => ctx.default_types.i64.into(),
            Type::PrimitiveUSize(_) | Type::PrimitiveISize(_) => ctx.default_types.isize.into(),
            Type::PrimitiveF32(_) => ctx.default_types.f32.into(),
            Type::PrimitiveF64(_) => ctx.default_types.f64.into(),
            Type::PrimitiveBool(_) => ctx.default_types.bool.into(),
        }
    }
}

impl<'ctx> TypedLiteral {
    fn to_basic_value(&self, ctx: &CodegenContext<'ctx>) -> BasicValueEnum<'ctx> {
        match self {
            TypedLiteral::Void => panic!("void should be ignored in llvm"),
            TypedLiteral::Dynamic(id) => ctx.scope[*id],
            TypedLiteral::Function(_) => todo!(),
            TypedLiteral::ExternalFunction(_) => todo!(),
            TypedLiteral::Static(_) => todo!(),
            TypedLiteral::String(global_str) => todo!(),
            TypedLiteral::Array(ty, vec) => {
                if vec.len() == 0 {
                    return ty.to_llvm_basic_type(ctx).array_type(0).const_zero().into();
                } else {
                    let mut val = ctx
                        .builder
                        .build_insert_value(
                            ty.to_llvm_basic_type(ctx)
                                .array_type(vec.len() as u32)
                                .get_poison(),
                            vec[0].to_basic_value(ctx),
                            0,
                            "",
                        )
                        .expect("the index should never be out of range")
                        .into_array_value();
                    for i in 1..vec.len() {
                        val = ctx
                            .builder
                            .build_insert_value(val, vec[0].to_basic_value(ctx), 0, "")
                            .expect("the index should never be out of range")
                            .into_array_value();
                    }
                    val.into()
                }
            }
            TypedLiteral::Struct(_, vec) => todo!(),
            TypedLiteral::F64(v) => ctx.default_types.f64.const_float(*v).into(),
            TypedLiteral::F32(v) => ctx.default_types.f32.const_float(*v as f64).into(),
            TypedLiteral::U8(v) => ctx.default_types.i8.const_int(*v as u64, false).into(),
            TypedLiteral::U16(v) => ctx.default_types.i16.const_int(*v as u64, false).into(),
            TypedLiteral::U32(v) => ctx.default_types.i32.const_int(*v as u64, false).into(),
            TypedLiteral::U64(v) => ctx.default_types.i64.const_int(*v, false).into(),
            TypedLiteral::USize(v) => ctx.default_types.isize.const_int(*v as u64, false).into(),
            TypedLiteral::I8(v) => ctx.default_types.i8.const_int(*v as u64, false).into(),
            TypedLiteral::I16(v) => ctx.default_types.i16.const_int(*v as u64, false).into(),
            TypedLiteral::I32(v) => ctx.default_types.i32.const_int(*v as u64, false).into(),
            TypedLiteral::I64(v) => ctx.default_types.i64.const_int(*v as u64, false).into(),
            TypedLiteral::ISize(v) => ctx.default_types.isize.const_int(*v as u64, false).into(),
            TypedLiteral::Bool(v) => ctx.default_types.bool.const_int(*v as u64, false).into(),
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
    fn build(&self, ctx: &mut CodegenContext) -> Result<(), BuilderError> {
        match self {
            TypecheckedExpression::Return(location, typed_literal) => {
                match typed_literal {
                    TypedLiteral::Void => ctx.builder.build_return(None)?,
                    TypedLiteral::Intrinsic(_) => unreachable!("intrinsic"),
                    lit => ctx.builder.build_return(Some(&lit.to_basic_value(ctx)))?,
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
                    cond.to_basic_value(ctx).into_int_value(),
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
                    cond.to_basic_value(ctx).into_int_value(),
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
            TypecheckedExpression::Pos(location, dst, src) => todo!(),
            TypecheckedExpression::Neg(location, typed_literal, typed_literal1) => todo!(),
            TypecheckedExpression::LNot(location, typed_literal, typed_literal1) => todo!(),
            TypecheckedExpression::BNot(location, typed_literal, typed_literal1) => todo!(),
            TypecheckedExpression::Add(location, dst, lhs, rhs) => {
                let lhs = lhs.to_basic_value(ctx);
                let rhs = rhs.to_basic_value(ctx);
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
                let lhs = lhs.to_basic_value(ctx);
                let rhs = rhs.to_basic_value(ctx);
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
                let lhs = lhs.to_basic_value(ctx);
                let rhs = rhs.to_basic_value(ctx);
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
                let lhs = lhs.to_basic_value(ctx);
                let rhs = rhs.to_basic_value(ctx);
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
                let lhs = lhs.to_basic_value(ctx);
                let rhs = rhs.to_basic_value(ctx);
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
                let lhs = lhs.to_basic_value(ctx);
                let rhs = rhs.to_basic_value(ctx);
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
                let lhs = lhs.to_basic_value(ctx);
                let rhs = rhs.to_basic_value(ctx);
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
                let lhs = lhs.to_basic_value(ctx);
                let rhs = rhs.to_basic_value(ctx);
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
                let lhs = lhs.to_basic_value(ctx);
                let rhs = rhs.to_basic_value(ctx);
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
                let lhs = lhs.to_basic_value(ctx);
                let rhs = rhs.to_basic_value(ctx);
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
                let lhs = lhs.to_basic_value(ctx);
                let rhs = rhs.to_basic_value(ctx);
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
                let lhs = lhs.to_basic_value(ctx);
                let rhs = rhs.to_basic_value(ctx);
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
                let lhs = lhs.to_basic_value(ctx);
                let rhs = rhs.to_basic_value(ctx);
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
                let lhs = lhs.to_basic_value(ctx);
                let rhs = rhs.to_basic_value(ctx);
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
                let lhs = lhs.to_basic_value(ctx);
                let rhs = rhs.to_basic_value(ctx);
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
                let lhs = lhs.to_basic_value(ctx);
                let rhs = rhs.to_basic_value(ctx);
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
            TypecheckedExpression::Reference(location, typed_literal, typed_literal1) => todo!(),
            TypecheckedExpression::SizedArrayToUnsizedArrayRef(
                location,
                typed_literal,
                typed_literal1,
                _,
            ) => todo!(),
            TypecheckedExpression::Dereference(location, typed_literal, typed_literal1) => todo!(),
            TypecheckedExpression::Offset(location, typed_literal, typed_literal1, vec) => todo!(),
            TypecheckedExpression::OffsetNonPointer(
                location,
                typed_literal,
                typed_literal1,
                offset_value,
            ) => todo!(),
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
            TypecheckedExpression::Literal(location, typed_literal, typed_literal1) => todo!(),
            TypecheckedExpression::Empty(location) => todo!(),
            TypecheckedExpression::None => todo!(),
        }
    }
}
