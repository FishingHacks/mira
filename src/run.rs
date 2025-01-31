use std::{io::Write, path::Path, process::Command, sync::Arc};

use mira::{
    codegen::{CodegenContext, CodegenError, InkwellContext, TargetTriple},
    error::MiraError,
    module::{Module, ModuleContext},
    parser::ParserQueueEntry,
    tokenizer::Tokenizer,
    typechecking::{
        ir_displayer::TypecheckingContextDisplay,
        typechecking::{typecheck_function, typecheck_static},
        TypecheckingContext,
    },
};
use parking_lot::RwLock;

#[derive(Default)]
pub struct RunOptions {
    pub llvm_ir: Option<Box<dyn Write>>,
    pub llvm_bc: Option<Box<dyn Write>>,
    pub asm: Option<Box<dyn Write>>,
    pub ir: Option<Box<dyn Write>>,
    pub obj: Option<Box<dyn Write>>,
}

pub fn link(obj_file: &Path, exec_file: &Path, args: &[String]) -> bool {
    let mut c_compiler_path = None;
    if let Some(paths) = std::env::var_os("PATH") {
        for mut path in std::env::split_paths(&paths) {
            for compiler in ["gcc", "clang", "cc"] {
                path.push(compiler);
                if path.exists() {
                    c_compiler_path = Some(path);
                    break;
                }
                path.pop();
            }
        }
    }
    let Some(program) = c_compiler_path else {
        println!("Could not find a c compiler");
        return false;
    };

    match Command::new(program)
        .arg(obj_file)
        .arg("-o")
        .arg(exec_file)
        .args(args)
        .spawn()
        .and_then(|mut v| v.wait())
    {
        Err(e) => {
            println!("failed to launchj c compiler: {e:?}");
            false
        }
        Ok(v) if !v.success() => {
            println!("compiler exited with failure: {:?}", v);
            false
        }
        Ok(_) => true,
    }
}

pub fn compile(
    file: impl Into<Arc<Path>>,
    root_directory: impl Into<Arc<Path>>,
    source: impl AsRef<str>,
    mut config: RunOptions,
) -> Result<(), Vec<MiraError>> {
    let file: Arc<Path> = file.into();
    let filename = file
        .file_name()
        .expect("file needs a filename")
        .to_string_lossy()
        .into_owned();
    let context = parse_all(file.clone(), root_directory.into(), source.as_ref())?;

    let typechecking_context = TypecheckingContext::new(context.clone());
    let errs = typechecking_context.resolve_imports(context.clone());
    if errs.len() > 0 {
        return Err(errs.into_iter().map(Into::into).collect());
    }
    let errs = typechecking_context.resolve_types(context.clone());
    if errs.len() > 0 {
        return Err(errs.into_iter().map(Into::into).collect());
    }

    let num_functions = { typechecking_context.functions.read().len() };
    let num_ext_functions = { typechecking_context.external_functions.read().len() };
    let num_statics = { typechecking_context.statics.read().len() };

    let mut errs = Vec::new();
    let mut scopes_fns = Vec::with_capacity(num_functions);
    let mut scopes_ext_fns = Vec::with_capacity(num_ext_functions);

    for i in 0..num_functions {
        match typecheck_function(&typechecking_context, &context, i, false) {
            Ok(v) => scopes_fns.push(v),
            Err(e) => errs.extend(e),
        }
    }

    for i in 0..num_ext_functions {
        match typecheck_function(&typechecking_context, &context, i, true) {
            Ok(v) => scopes_ext_fns.push(v),
            Err(e) => errs.extend(e),
        }
    }
    for i in 0..num_statics {
        typecheck_static(&typechecking_context, &context, i, &mut errs);
    }

    for err in &errs {
        println!("{err:?}");
    }

    if errs.len() > 0 {
        return Err(errs.into_iter().map(Into::into).collect());
    }

    let mut errs = Vec::new();

    let num_fns = { typechecking_context.functions.read().len() };
    let num_ext_fns = { typechecking_context.external_functions.read().len() };
    let context = InkwellContext::create();
    let codegen_context = CodegenContext::make_context(
        &context,
        TargetTriple::create("x86_64-unknown-linux-gnu"),
        typechecking_context.clone(),
        &filename,
        file,
        false,
    )
    .expect("failed to create the llvm context");
    for fn_id in 0..num_fns {
        if let Err(e) = codegen_context.compile_fn(fn_id, scopes_fns.remove(0), false) {
            errs.push(MiraError::Codegen { inner: e.into() });
        }
    }
    for fn_id in 0..num_ext_fns {
        if let Err(e) = codegen_context.compile_fn(fn_id, scopes_ext_fns.remove(0), true) {
            errs.push(MiraError::Codegen { inner: e.into() });
        }
    }
    codegen_context.finalize_debug_info();
    if let Err(e) = codegen_context.check() {
        errs.push(CodegenError::LLVMNative(e).into());
    }
    if errs.len() > 0 {
        return Err(errs);
    }

    codegen_context
        .optimize_o3()
        .map_err(CodegenError::LLVMNative)
        .map_err(Into::into)
        .map_err(|v| vec![v])?;

    if let Some(ir_writer) = &mut config.ir {
        if let Err(e) = write!(
            ir_writer,
            "{}",
            TypecheckingContextDisplay(&typechecking_context)
        ) {
            errs.push(CodegenError::IO(e).into());
        }
    }

    if let Some(ir_writer) = &mut config.llvm_ir {
        if let Err(e) = codegen_context.emit_llvm_ir(ir_writer) {
            errs.push(CodegenError::IO(e).into());
        }
    }

    if let Some(bc_writer) = &mut config.llvm_bc {
        if let Err(e) = codegen_context.emit_llvm_bitcode(bc_writer) {
            errs.push(CodegenError::IO(e).into());
        }
    }

    if let Some(asm_writer) = &mut config.asm {
        if let Err(e) = codegen_context.emit_assembly(asm_writer) {
            errs.push(e.into());
        }
    }

    if let Some(obj_writer) = &mut config.obj {
        if let Err(e) = codegen_context.emit_object(obj_writer) {
            errs.push(e.into());
        }
    }

    Ok(())
}

fn parse_all(
    file: Arc<Path>,
    root_directory: Arc<Path>,
    source: &str,
) -> Result<Arc<ModuleContext>, Vec<MiraError>> {
    let mut errors = vec![];

    let mut tokenizer = Tokenizer::new(source.as_ref(), file.clone());
    if let Err(errs) = tokenizer.scan_tokens() {
        errors.extend(
            errs.into_iter()
                .map(|inner| MiraError::Tokenization { inner }),
        );
    }

    let modules = Arc::new(RwLock::new(vec![ParserQueueEntry {
        file,
        root: root_directory.clone(),
    }]));
    let mut current_parser = tokenizer.to_parser(modules.clone(), root_directory);

    let module_context = Arc::new(ModuleContext::default());

    loop {
        let (statements, parsing_errors) = current_parser.parse_all();
        errors.extend(
            parsing_errors
                .into_iter()
                .map(|inner| MiraError::Parsing { inner }),
        );
        let (path, root) = {
            let module = &modules.read()[module_context.modules.read().len()];
            (module.file.clone(), module.root.clone())
        };
        let mut module = Module::new(module_context.clone(), current_parser.imports, path, root);
        if let Err(errs) = module.push_all(statements, module_context.modules.read().len()) {
            errors.extend(
                errs.into_iter()
                    .map(|inner| MiraError::ProgramForming { inner }),
            );
        }
        let mut writer = module_context.modules.write();
        writer.push(module);

        let read_modules = modules.read();
        if read_modules.len() > writer.len() {
            let entry = read_modules[writer.len()].clone();
            drop(read_modules);
            drop(writer);
            let mut tokenizer = Tokenizer::new(
                &std::fs::read_to_string(&entry.file).expect("failed to read module file"),
                entry.file,
            );
            if let Err(errs) = tokenizer.scan_tokens() {
                errors.extend(
                    errs.into_iter()
                        .map(|inner| MiraError::Tokenization { inner }),
                );
            }
            current_parser = tokenizer.to_parser(modules.clone(), entry.root);
        } else {
            break;
        }
    }

    if errors.len() > 0 {
        Err(errors)
    } else {
        Ok(module_context)
    }
}
