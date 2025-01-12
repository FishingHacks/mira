use core::panic;
use std::{
    collections::{HashMap, HashSet},
    path::Path,
    sync::Arc,
};

use super::{
    error::CodegenError,
    mangling::{mangle_function, mangle_static, mangle_string, mangle_struct},
};
use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    module::{Linkage, Module},
    support::LLVMString,
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
    },
    types::{BasicType, FloatType, IntType, PointerType, StructType},
    values::{FunctionValue, GlobalValue},
    AddressSpace, OptimizationLevel,
};

use crate::{
    globals::GlobalStr,
    module::FunctionId,
    std_annotations::{alias_annotation::ExternAliasAnnotation, ext_var_arg::ExternVarArg},
    typechecking::{
        expression::{TypecheckedExpression, TypedLiteral},
        typechecking::ScopeTypeMetadata,
        Type, TypecheckingContext,
    },
};

#[derive(Clone, Copy)]
pub struct DefaultTypes<'ctx> {
    pub isize: IntType<'ctx>,
    pub i8: IntType<'ctx>,
    pub i16: IntType<'ctx>,
    pub i32: IntType<'ctx>,
    pub i64: IntType<'ctx>,
    pub bool: IntType<'ctx>,
    pub ptr: PointerType<'ctx>,
    // { ptr, isize }
    pub fat_ptr: StructType<'ctx>,
    pub f32: FloatType<'ctx>,
    pub f64: FloatType<'ctx>,
    // {}
    pub empty_struct: StructType<'ctx>,
}

pub struct CodegenContext<'ctx> {
    pub(super) tc_ctx: Arc<TypecheckingContext>,
    pub(super) builder: Builder<'ctx>,
    pub(super) context: &'ctx Context,
    pub(super) default_types: DefaultTypes<'ctx>,
    pub module: Module<'ctx>,
    pub machine: TargetMachine,
    pub triple: TargetTriple,
    pub(super) functions: Vec<FunctionValue<'ctx>>,
    pub(super) external_functions: Vec<FunctionValue<'ctx>>,
    pub(super) structs: Vec<StructType<'ctx>>,
    pub(super) statics: Vec<GlobalValue<'ctx>>,
    pub(super) string_map: HashMap<GlobalStr, GlobalValue<'ctx>>,
}

impl<'a> CodegenContext<'a> {
    pub fn make_context(
        context: &'a Context,
        triple: TargetTriple,
        ctx: Arc<TypecheckingContext>,
        module: &str,
    ) -> Result<CodegenContext<'a>, CodegenError> {
        Target::initialize_all(&InitializationConfig::default());
        let target = Target::from_triple(&triple)?;
        let Some(machine) = target.create_target_machine(
            &triple,
            "",
            "",
            OptimizationLevel::Aggressive,
            RelocMode::PIC,
            CodeModel::Default,
        ) else {
            return Err(CodegenError::UnknownTriple(triple));
        };
        let module = context.create_module(module);
        module.set_triple(&triple);
        let target_data = machine.get_target_data();
        let data_layout = target_data.get_data_layout();
        module.set_data_layout(&data_layout);
        let isize_type = context.ptr_sized_int_type(&target_data, None);
        let ptr_type = context.ptr_type(AddressSpace::default());
        let default_types = DefaultTypes {
            i8: context.i8_type(),
            i16: context.i16_type(),
            i32: context.i32_type(),
            i64: context.i64_type(),
            f32: context.f32_type(),
            f64: context.f64_type(),
            bool: context.bool_type(),
            isize: isize_type,
            ptr: ptr_type,
            fat_ptr: context.struct_type(&[ptr_type.into(), isize_type.into()], false),
            empty_struct: context.struct_type(&[], false),
        };
        let builder = context.create_builder();
        let mut strings = collect_strings(&ctx);
        let mut string_map = HashMap::new();
        for strn in strings.drain() {
            let (constant, length, mangled_name) = strn.with(|v| {
                (
                    context.const_string(v.as_bytes(), false),
                    v.as_bytes().len(),
                    mangle_string(v),
                )
            });
            let global = module.add_global(
                default_types.i8.array_type(length as u32),
                None,
                &mangled_name,
            );
            global.set_linkage(Linkage::Internal);
            global.set_initializer(&constant);
            string_map.insert(strn, global);
        }
        drop(strings);

        let struct_reader = ctx.structs.read();
        let structs = struct_reader
            .iter()
            .enumerate()
            .map(|(i, structure)| context.opaque_struct_type(&mangle_struct(&ctx, i)))
            .collect::<Vec<_>>();
        for i in 0..struct_reader.len() {
            let fields = struct_reader[i]
                .elements
                .iter()
                .map(|(_, t)| t)
                .map(|t| match t {
                    Type::PrimitiveVoid(0) | Type::PrimitiveNever => {
                        default_types.i8.array_type(0).into()
                    }
                    t => t.to_llvm_basic_type(&default_types, &structs),
                })
                .collect::<Vec<_>>();
            assert!(
                structs[i].set_body(&fields, false),
                "struct should not yet be initialized"
            );
        }
        drop(struct_reader);

        // functions
        let function_reader = ctx.functions.read();
        let functions = function_reader
            .iter()
            .enumerate()
            .map(|(i, (c, b))| (i, c, b))
            .map(|(i, contract, _)| {
                let param_types = contract
                    .arguments
                    .iter()
                    .filter_map(|(_, t)| match t {
                        Type::PrimitiveVoid(0) | Type::PrimitiveNever => None,
                        t => Some(t.to_llvm_basic_type(&default_types, &structs).into()),
                    })
                    .collect::<Vec<_>>();

                let fn_typ =
                    if let Type::PrimitiveNever | Type::PrimitiveVoid(0) = contract.return_type {
                        context.void_type().fn_type(&param_types, false)
                    } else {
                        contract
                            .return_type
                            .to_llvm_basic_type(&default_types, &structs)
                            .fn_type(&param_types, false)
                    };
                let name = mangle_function(&ctx, i);
                module.add_function(name.as_str(), fn_typ, Some(Linkage::Internal))
            })
            .collect::<Vec<_>>();
        drop(function_reader);

        // external functions
        let ext_function_reader = ctx.external_functions.read();
        let external_functions = ext_function_reader
            .iter()
            .enumerate()
            .map(|(i, (c, b))| (i, c, b))
            .map(|(i, contract, _)| {
                let param_types = contract
                    .arguments
                    .iter()
                    .filter_map(|(_, t)| match t {
                        Type::PrimitiveVoid(0) | Type::PrimitiveNever => None,
                        t => Some(t.to_llvm_basic_type(&default_types, &structs).into()),
                    })
                    .collect::<Vec<_>>();

                let fn_typ =
                    if let Type::PrimitiveNever | Type::PrimitiveVoid(0) = contract.return_type {
                        context.void_type().fn_type(&param_types, false)
                    } else {
                        contract
                            .return_type
                            .to_llvm_basic_type(&default_types, &structs)
                            .fn_type(
                                &param_types,
                                contract
                                    .annotations
                                    .get_first_annotation::<ExternVarArg>()
                                    .is_some(),
                            )
                    };
                let name = if let Some(name) = contract
                    .annotations
                    .get_first_annotation::<ExternAliasAnnotation>()
                    .map(|v| &v.0)
                {
                    name
                } else {
                    contract
                        .name
                        .as_ref()
                        .expect("external functions should always have a name")
                };

                name.with(|name| module.add_function(name, fn_typ, None))
            })
            .collect::<Vec<_>>();
        drop(ext_function_reader);

        let static_reader = ctx.statics.read();
        let statics = static_reader
            .iter()
            .enumerate()
            .map(|(i, v)| {
                module.add_global(
                    v.0.to_llvm_basic_type(&default_types, &structs),
                    None,
                    &mangle_static(&ctx, i),
                )
            })
            .collect::<Vec<_>>();
        for (i, v) in static_reader.iter().enumerate() {
            statics[i].set_linkage(Linkage::Internal);
            statics[i].set_initializer(&v.1.to_basic_value(
                &|i| panic!("index out of bounds: the length is 0, but the index is {i}"),
                &default_types,
                &structs,
                &builder,
                &statics,
                &functions,
                &external_functions,
                &string_map,
            ));
        }
        drop(static_reader);

        return Ok(CodegenContext {
            context,
            module,
            builder,
            default_types,
            functions,
            external_functions,
            structs,
            statics,
            string_map,
            machine,
            triple,
            tc_ctx: ctx,
        });
    }

    pub fn compile_fn(
        &self,
        fn_id: FunctionId,
        scope: Vec<(Type, ScopeTypeMetadata)>,
        is_external: bool,
    ) -> Result<(), BuilderError> {
        let func = if is_external {
            self.external_functions[fn_id]
        } else {
            self.functions[fn_id]
        };
        let function_reader = self.tc_ctx.functions.read();
        let ext_function_reader = self.tc_ctx.external_functions.read();
        let body = if is_external {
            let Some(v) = &ext_function_reader[fn_id].1 else {
                return Ok(());
            };
            &**v
        } else {
            &*function_reader[fn_id].1
        };
        let contract = if is_external {
            &ext_function_reader[fn_id].0
        } else {
            &function_reader[fn_id].0
        };
        let mut function_ctx = self.make_function_codegen_context(scope, func);
        let void_arg = self.default_types.empty_struct.const_zero().into();

        let body_basic_block = self
            .context
            .append_basic_block(func, &format!("entry_{}", fn_id));
        self.builder.position_at_end(body_basic_block);

        let mut param_idx = 0;
        for (idx, arg) in contract
            .arguments
            .iter()
            .enumerate()
            .map(|v| (v.0, &v.1 .1))
        {
            if matches!(arg, Type::PrimitiveVoid(0) | Type::PrimitiveNever) {
                function_ctx.push_value(idx, void_arg);
            } else {
                function_ctx.push_value(idx, func.get_nth_param(param_idx).unwrap());
                param_idx += 1;
            }
        }

        for expr in body {
            expr.codegen(&mut function_ctx)?;
        }
        Ok(())
    }

    pub fn write_object_file(&self, path: impl AsRef<Path>) -> Result<(), LLVMString> {
        self.machine
            .write_to_file(&self.module, FileType::Object, path.as_ref())
    }

    pub fn run(&self) -> Result<(), LLVMString> {
        let execution_engine = self
            .module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)?;
        let func = unsafe {
            execution_engine
                .get_function::<unsafe extern "C" fn(usize, *const *const u8)>("main")
                .expect("failed to get function main")
        };
        unsafe {
            func.call(0, std::ptr::null());
        }
        Ok(())
    }
}

fn collect_strings(ctx: &TypecheckingContext) -> HashSet<GlobalStr> {
    let function_reader = ctx.functions.read();
    let ext_function_reader = ctx.external_functions.read();
    let statics_reader = ctx.statics.read();
    let mut strings = HashSet::new();

    for (_, body) in function_reader.iter() {
        collect_strings_for_expressions(body, &mut strings);
    }
    for (_, body) in ext_function_reader.iter() {
        if let Some(body) = body {
            collect_strings_for_expressions(body, &mut strings);
        }
    }
    for static_value in statics_reader.iter() {
        collect_strings_for_typed_literal(&static_value.1, &mut strings);
    }

    strings
}

fn collect_strings_for_expressions(
    exprs: &[TypecheckedExpression],
    strings: &mut HashSet<GlobalStr>,
) {
    for expr in exprs {
        match expr {
            TypecheckedExpression::Block(_, exprs, _) => {
                collect_strings_for_expressions(exprs, strings)
            }
            TypecheckedExpression::If {
                cond,
                if_block,
                else_block,
                ..
            } => {
                collect_strings_for_typed_literal(cond, strings);
                collect_strings_for_expressions(if_block, strings);
                _ = else_block
                    .as_ref()
                    .map(|v| collect_strings_for_expressions(v, strings));
            }
            TypecheckedExpression::While {
                loc,
                cond_block,
                cond,
                body,
            } => {
                collect_strings_for_expressions(cond_block, strings);
                collect_strings_for_expressions(body, strings);
                collect_strings_for_typed_literal(&cond, strings);
            }
            TypecheckedExpression::Call(_, _, lhs, args) => {
                collect_strings_for_typed_literal(lhs, strings);
                for v in args {
                    collect_strings_for_typed_literal(v, strings);
                }
            }
            TypecheckedExpression::DirectCall(.., args)
            | TypecheckedExpression::DirectExternCall(.., args)
            | TypecheckedExpression::IntrinsicCall(.., args) => {
                for v in args {
                    collect_strings_for_typed_literal(v, strings);
                }
            }
            TypecheckedExpression::Range { lhs, rhs, .. }
            | TypecheckedExpression::StoreAssignment(_, lhs, rhs)
            | TypecheckedExpression::Add(.., lhs, rhs)
            | TypecheckedExpression::Sub(.., lhs, rhs)
            | TypecheckedExpression::Mul(.., lhs, rhs)
            | TypecheckedExpression::Div(.., lhs, rhs)
            | TypecheckedExpression::Mod(.., lhs, rhs)
            | TypecheckedExpression::BAnd(.., lhs, rhs)
            | TypecheckedExpression::BOr(.., lhs, rhs)
            | TypecheckedExpression::BXor(.., lhs, rhs)
            | TypecheckedExpression::GreaterThan(.., lhs, rhs)
            | TypecheckedExpression::LessThan(.., lhs, rhs)
            | TypecheckedExpression::LAnd(.., lhs, rhs)
            | TypecheckedExpression::LOr(.., lhs, rhs)
            | TypecheckedExpression::GreaterThanEq(.., lhs, rhs)
            | TypecheckedExpression::LessThanEq(.., lhs, rhs)
            | TypecheckedExpression::Eq(.., lhs, rhs)
            | TypecheckedExpression::Neq(.., lhs, rhs)
            | TypecheckedExpression::LShift(.., lhs, rhs)
            | TypecheckedExpression::RShift(.., lhs, rhs) => {
                collect_strings_for_typed_literal(lhs, strings);
                collect_strings_for_typed_literal(rhs, strings);
            }
            TypecheckedExpression::Return(_, lit)
            | TypecheckedExpression::Reference(.., lit)
            | TypecheckedExpression::Dereference(.., lit)
            | TypecheckedExpression::Offset(.., lit, _)
            | TypecheckedExpression::OffsetNonPointer(.., lit, _)
            | TypecheckedExpression::TraitCall(.., lit, _, _)
            | TypecheckedExpression::Literal(.., lit)
            | TypecheckedExpression::MakeUnsizedSlice(.., lit, _)
            | TypecheckedExpression::Pos(.., lit)
            | TypecheckedExpression::Neg(.., lit)
            | TypecheckedExpression::LNot(.., lit)
            | TypecheckedExpression::Alias(.., lit)
            | TypecheckedExpression::BNot(.., lit) => {
                collect_strings_for_typed_literal(lit, strings)
            }
            TypecheckedExpression::Empty(_) | TypecheckedExpression::None => (),
        }
    }
}
fn collect_strings_for_typed_literal(lit: &TypedLiteral, strings: &mut HashSet<GlobalStr>) {
    match lit {
        TypedLiteral::String(global_str) => _ = strings.insert(global_str.clone()),
        TypedLiteral::Array(_, vec) => vec
            .iter()
            .for_each(|v| collect_strings_for_typed_literal(v, strings)),
        TypedLiteral::Struct(_, vec) => vec
            .iter()
            .for_each(|v| collect_strings_for_typed_literal(v, strings)),
        _ => (),
    }
}
