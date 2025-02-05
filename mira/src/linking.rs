use std::{
    fs::File,
    io::Write,
    path::{Path, PathBuf},
    process::{Child, Command, ExitStatus},
    sync::Arc,
    time::Instant,
};

use inkwell::{context::Context, object_file};
use parking_lot::RwLock;
use thiserror::Error;

use crate::{
    codegen::{CodegenConfig, CodegenContext, CodegenError},
    error::MiraError,
    module::{Module, ModuleContext},
    parser::ParserQueueEntry,
    tokenizer::Tokenizer,
    typechecking::{
        ir_displayer::TCContextDisplay,
        typechecking::{typecheck_function, typecheck_static},
        TypecheckingContext,
    },
};

#[derive(Debug)]
pub enum LinkerInput {
    LinkLibrary(String),
    LinkPath(PathBuf),
    LinkPathLookup(PathBuf),
}

pub struct LinkOptions<'a> {
    pub linker_path: PathBuf,
    pub object_files: &'a [PathBuf],
    pub inputs: &'a [LinkerInput],
    pub output: &'a Path,
    pub args: &'a [String],
    pub link_crt: bool,
    pub debug_info: bool,
    pub verbose: bool,
    pub linker_script: Option<&'a Path>,
    pub create_dynamic_library: bool,
}

pub trait Linker: Send + Sync {
    fn can_link_crt(&self) -> bool;
    fn exists_in(&self, path: &Path) -> Option<PathBuf>;
    fn link(&self, opts: LinkOptions) -> Result<(), LinkerError>;
}

macro_rules! arg_if {
    ($command:ident $arg:expr, if $cond: expr) => {
        if $cond {
            $command.arg($arg);
        }
    };
}

#[derive(Debug, Error)]
pub enum LinkerError {
    #[error("Unable to find a valid linker")]
    UnableToLocateLinker,
    #[error("failed to run command {0:?}: {1}")]
    SpawnFailed(Command, std::io::Error),
    #[error("command {0:?} exited with a failure (exit code {1:?})")]
    UnsuccessfulExit(Command, ExitStatus, Child),
}

struct LdLikeLinker(&'static str);

impl Linker for LdLikeLinker {
    fn can_link_crt(&self) -> bool {
        false
    }
    fn exists_in(&self, path: &Path) -> Option<PathBuf> {
        let bin_path = path.join(self.0);
        bin_path.exists().then_some(bin_path)
    }

    fn link(&self, opts: LinkOptions) -> Result<(), LinkerError> {
        assert!(!opts.link_crt);
        let mut command = Command::new(opts.linker_path);
        arg_if!(command "-g", if opts.debug_info);
        arg_if!(command "-shared", if opts.create_dynamic_library);
        if let Some(script) = opts.linker_script.as_ref() {
            command.arg("--script").arg(script);
        }
        opts.object_files.iter().for_each(|v| _ = command.arg(v));
        command.arg("-o").arg(opts.output);
        for input in opts.inputs {
            match input {
                LinkerInput::LinkLibrary(lib) => _ = command.arg("-l").arg(lib),
                LinkerInput::LinkPath(path) => _ = command.arg(path),
                LinkerInput::LinkPathLookup(path) => _ = command.arg("-L").arg(path),
            }
        }
        command.args(opts.args);
        if opts.verbose {
            println!("[INFO] Running {command:?}");
        }
        let mut child = match command.spawn() {
            Ok(child) => child,
            Err(e) => return Err(LinkerError::SpawnFailed(command, e)),
        };
        match child.wait().expect("command wasnt ran") {
            c if !c.success() => Err(LinkerError::UnsuccessfulExit(command, c, child)),
            _ => Ok(()),
        }
    }
}

struct GccLikeLinker(&'static str);

impl Linker for GccLikeLinker {
    fn can_link_crt(&self) -> bool {
        true
    }
    fn exists_in(&self, path: &Path) -> Option<PathBuf> {
        let bin_path = path.join(self.0);
        bin_path.exists().then_some(bin_path)
    }

    fn link(&self, opts: LinkOptions) -> Result<(), LinkerError> {
        let mut command = Command::new(opts.linker_path);
        arg_if!(command "-g", if opts.debug_info);
        arg_if!(command "-nostdlib", if !opts.link_crt);
        arg_if!(command "-shared", if opts.create_dynamic_library);
        if let Some(script) = opts.linker_script.as_ref() {
            command.arg("--script").arg(script);
        }
        opts.object_files.iter().for_each(|v| _ = command.arg(v));
        command.arg("-o").arg(opts.output);
        for input in opts.inputs {
            match input {
                LinkerInput::LinkLibrary(lib) => _ = command.arg("-l").arg(lib),
                LinkerInput::LinkPath(path) => _ = command.arg(path),
                LinkerInput::LinkPathLookup(path) => _ = command.arg("-L").arg(path),
            }
        }
        command.args(opts.args);
        if opts.verbose {
            println!("[INFO] Running {command:?}");
        }
        let mut child = match command.spawn() {
            Ok(child) => child,
            Err(e) => return Err(LinkerError::SpawnFailed(command, e)),
        };
        match child.wait().expect("command wasnt ran") {
            c if !c.success() => Err(LinkerError::UnsuccessfulExit(command, c, child)),
            _ => Ok(()),
        }
    }
}

static LINKERS: &[&dyn Linker] = &[
    &LdLikeLinker("ld"),
    &LdLikeLinker("ld.lld"),
    &GccLikeLinker("cc"),
    &GccLikeLinker("c++"),
    &GccLikeLinker("gcc"),
    &GccLikeLinker("g++"),
    &GccLikeLinker("clang"),
    &GccLikeLinker("clang++"),
];

pub fn search_for_linker(
    with_c_runtime: bool,
    additional_dirs: &[PathBuf],
) -> Option<(&'static dyn Linker, PathBuf)> {
    for dir in additional_dirs {
        for linker in LINKERS {
            if !linker.can_link_crt() && with_c_runtime {
                continue;
            }
            if let Some(path) = linker.exists_in(dir) {
                return Some((*linker, path));
            }
        }
    }
    if let Some(paths) = std::env::var_os("PATH") {
        for dir in std::env::split_paths(&paths) {
            for linker in LINKERS {
                if !linker.can_link_crt() && with_c_runtime {
                    continue;
                }
                if let Some(path) = linker.exists_in(&dir) {
                    return Some((*linker, path));
                }
            }
        }
    }
    None
}

pub struct FullCompilationOptions<'a> {
    /// The file the source came from. Used to evaluate relative imports
    pub file: Arc<Path>,
    /// The path the import `@root/` points to
    pub root_directory: Arc<Path>,
    /// The file that will appear in locations and debug info
    pub debug_file: Arc<Path>,
    /// The source that will be parsed, reads `self.file` if None.
    /// Has to be specified if `self.file` points at a file that does not actually exists
    /// (e.g. because this is running in a repl)
    pub source: Option<&'a str>,
    /// Set to true if you need a shared object (.so / .dll)
    pub shared_object: bool,
    /// An Optional path to a linker script
    pub linker_script: Option<&'a Path>,
    /// The path to the obj (`.o`) file
    /// if not specified, this won't generate the object file nor link it.
    pub obj_path: Option<PathBuf>,
    /// Set to true if you want the target-specific extension of the shared object / executable to
    /// the exec_path.
    pub add_extension_to_exe: bool,
    /// The path to the resulting executable
    /// if not specified, this won't link the object file
    pub exec_path: Option<PathBuf>,
    /// The codegen options such as target, optimization level, etc
    pub codegen_opts: CodegenConfig<'a>,
    /// Set to true if you want to link with the c runtime. This will automatically invoke the
    /// `main` symbol and fail to link if it can't find a main symbol.
    ///
    /// Definition of main:
    /// extern fn main(argc: usize, argv: &&u8) -> int
    pub link_with_crt: bool,
    /// Additional arguments that are being passed to the linker
    pub additional_linker_args: &'a [String],
    /// Additional directories to search for the linker binary
    pub additional_linker_directories: &'a [PathBuf],
    /// Set to true if you want to print what is being currently done to stdout.
    pub verbose: bool,
    /// Set to true if you want the binary to have debug info
    pub with_debug_info: bool,
    /// Writer to write the intermediate representation to (this DOES NOT look good, use for
    /// debugging purposes only. Expects user to have knowledge of compiler internals)
    pub ir_writer: Option<Box<dyn Write>>,
    /// Writer to write the llvm intermediate representation to
    pub llvm_ir_writer: Option<Box<dyn Write>>,
    /// Writer to write the llvm bitcode to
    pub llvm_bc_writer: Option<Box<dyn Write>>,
    /// Writer to write the assembly to
    pub asm_writer: Option<Box<dyn Write>>,
}

/// Runs the pipeline to turn a source file into an executable or shared object.
pub fn run_full_compilation_pipeline(
    mut opts: FullCompilationOptions,
) -> Result<(), Vec<MiraError>> {
    macro_rules! vprintln {
        (nn $($t:tt)*) => {
            if opts.verbose { print!($($t)*); _ = std::io::stdout().flush(); }
        };
        ($($t:tt)*) => {
            if opts.verbose { println!($($t)*); }
        };
    }

    if opts.add_extension_to_exe && opts.exec_path.is_some() {
        let mut path = opts.exec_path.unwrap().into_os_string();
        if opts.shared_object {
            path.push(opts.codegen_opts.target.os.dynamic_lib_ext());
        } else {
            path.push(opts.codegen_opts.target.os.exe_file_ext());
        }
        opts.exec_path = Some(PathBuf::from(path));
    }

    if opts.verbose {
        println!("Using target {}", opts.codegen_opts.target);
        if opts.codegen_opts.runtime_safety {
            println!("Runtime Safety: Enabled");
        } else {
            println!("Runtime Safety: Disabled");
        }
        println!("Optimizations: {:?}", opts.codegen_opts.optimizations);
        println!("Relocation Mode: {:?}", opts.codegen_opts.reloc_mode);
        println!("Assuming cpu features: {}", opts.codegen_opts.cpu_features);
    }
    let now = Instant::now();
    vprintln!("Parsing...");
    let mut read = String::new();
    let source = opts
        .source
        .map(Ok)
        .unwrap_or_else(|| {
            read = std::fs::read_to_string(&opts.file)?;
            Ok(&read)
        })
        .map_err(|inner| vec![inner])?;
    let module_context = parse_all(
        opts.file,
        opts.root_directory,
        opts.debug_file.clone(),
        source,
        opts.verbose,
    );
    vprintln!("Parsing took {:?}", Instant::now().duration_since(now));
    let module_context = module_context?;
    let now = Instant::now();
    vprintln!("Type Resolution...");
    let typechecking_context = TypecheckingContext::new(module_context.clone());
    let errs = typechecking_context.resolve_imports(module_context.clone());
    if errs.len() > 0 {
        return Err(errs.into_iter().map(Into::into).collect());
    }
    let errs = typechecking_context.resolve_types(module_context.clone());
    if errs.len() > 0 {
        return Err(errs.into_iter().map(Into::into).collect());
    }

    vprintln!("Typechecking...");

    let num_functions = { typechecking_context.functions.read().len() };
    let num_ext_functions = { typechecking_context.external_functions.read().len() };
    let num_statics = { typechecking_context.statics.read().len() };

    let mut errs = Vec::new();
    let mut scopes_fns = Vec::with_capacity(num_functions);
    let mut scopes_ext_fns = Vec::with_capacity(num_ext_functions);

    for i in 0..num_functions {
        vprintln!(nn "\rTypechecking function #{i}");
        match typecheck_function(&typechecking_context, &module_context, i, false) {
            Ok(v) => scopes_fns.push(v),
            Err(e) => errs.extend(e),
        }
    }
    vprintln!();

    for i in 0..num_ext_functions {
        vprintln!(nn "\rTypechecking external function #{i}");
        match typecheck_function(&typechecking_context, &module_context, i, true) {
            Ok(v) => scopes_ext_fns.push(v),
            Err(e) => errs.extend(e),
        }
    }
    vprintln!();

    for i in 0..num_statics {
        vprintln!(nn "\rTypechecking static #{i}");
        typecheck_static(&typechecking_context, &module_context, i, &mut errs);
    }

    vprintln!(
        "\nType Resolution and Typechecking took {:?}",
        Instant::now().duration_since(now)
    );

    let now = Instant::now();

    if errs.len() > 0 {
        return Err(errs.into_iter().map(Into::into).collect());
    }

    let mut errs = Vec::new();

    match opts.ir_writer.as_mut().map(|v| {
        v.write_fmt(format_args!(
            "{:#}",
            TCContextDisplay(&typechecking_context)
        ))
    }) {
        Some(Err(e)) => errs.push(e.into()),
        _ => (),
    }

    vprintln!("Codegen...");
    let num_fns = { typechecking_context.functions.read().len() };
    let num_ext_fns = { typechecking_context.external_functions.read().len() };
    let context = Context::create();

    let debug_filename = opts
        .debug_file
        .file_name()
        .expect("debug file needs a filename")
        .to_string_lossy();
    let mut codegen_context = CodegenContext::new(
        &context,
        typechecking_context.clone(),
        &debug_filename,
        opts.debug_file.clone(),
        opts.codegen_opts,
    )
    .expect("failed to create the llvm context");
    for fn_id in 0..num_fns {
        vprintln!(nn "\rCompiling function #{fn_id}");
        if let Err(e) = codegen_context.compile_fn(fn_id, scopes_fns.remove(0), false) {
            errs.push(MiraError::Codegen { inner: e.into() });
        }
    }
    vprintln!();

    for fn_id in 0..num_ext_fns {
        vprintln!(nn "\rCompiling external function #{fn_id}");
        if let Err(e) = codegen_context.compile_fn(fn_id, scopes_ext_fns.remove(0), true) {
            errs.push(MiraError::Codegen { inner: e.into() });
        }
    }
    vprintln!();

    if let Err(e) = codegen_context.finish() {
        errs.push(CodegenError::LLVMNative(e).into());
    }

    vprintln!("Codegen took {:?}", Instant::now().duration_since(now));

    // emit llvm ir even in the case there are errors. LLVM errors are often more-or-less cryptic, and
    // seeing what junk the compiler generated will probably help in diagnosing them.
    // this hopefully should not error.
    if let Some(ir_writer) = opts.llvm_ir_writer.as_mut() {
        if let Err(e) = codegen_context.write_ir(ir_writer) {
            errs.push(e.into());
        }
    }

    if errs.len() > 0 {
        return Err(errs);
    }

    if let Some(bc_writer) = opts.llvm_bc_writer.as_mut() {
        if let Err(e) = codegen_context.write_bitcode(bc_writer) {
            errs.push(e.into());
        }
    }

    if let Some(asm_writer) = opts.asm_writer.as_mut() {
        if let Err(e) = codegen_context.write_assembly(asm_writer) {
            errs.push(e.into());
        }
    }

    let Some(obj_path) = opts.obj_path else {
        return (errs.len() == 0).then_some(()).ok_or(errs);
    };
    let now = Instant::now();

    let mut obj_file = match File::options()
        .write(true)
        .create(true)
        .open(&obj_path)
        .map_err(|v| errs.push(v.into()))
    {
        Ok(v) => v,
        Err(()) => return Err(errs),
    };

    vprintln!("Writing object");

    if let Err(e) = codegen_context.write_object(&mut obj_file) {
        errs.push(e.into());
    }
    drop(obj_file);

    if errs.len() > 0 {
        return Err(errs);
    }

    let Some(exec_path) = opts.exec_path else {
        return Ok(());
    };

    vprintln!("Locating Linker");

    let Some((linker, linker_path)) =
        search_for_linker(opts.link_with_crt, opts.additional_linker_directories)
    else {
        return Err(vec![LinkerError::UnableToLocateLinker.into()]);
    };

    vprintln!("Linking");
    linker
        .link(LinkOptions {
            linker_path,
            object_files: &[obj_path],
            inputs: &[],
            output: &exec_path,
            args: opts.additional_linker_args,
            link_crt: opts.link_with_crt,
            debug_info: opts.with_debug_info,
            verbose: opts.verbose,
            linker_script: opts.linker_script,
            create_dynamic_library: opts.shared_object,
        })
        .map_err(|v| vec![v.into()])?;
    vprintln!("Linking took {:?}", Instant::now().duration_since(now));
    Ok(())
}

/// Parses a string of text into a module
///
/// `file` - The file the source came from. Used to evaluate relative imports
/// `root_directory` - The path the import `@root/` points to
/// `debug_file` - The file that will appear in locations and debug info
/// `source` - The source that will be parsed
pub fn parse_all(
    file: Arc<Path>,
    root_directory: Arc<Path>,
    debug_file: Arc<Path>,
    source: &str,
    verbose: bool,
) -> Result<Arc<ModuleContext>, Vec<MiraError>> {
    let mut errors = vec![];
    macro_rules! vprintln {
        ($($t:tt)*) => {
            if verbose { println!($($t)*); }
        };
    }

    if debug_file != file {
        vprintln!("Tokenizing {file:?} as {debug_file:?}");
    } else {
        vprintln!("Tokenizing {file:?}");
    }
    let mut tokenizer = Tokenizer::new(source.as_ref(), debug_file.clone());
    if let Err(errs) = tokenizer.scan_tokens() {
        errors.extend(
            errs.into_iter()
                .map(|inner| MiraError::Tokenization { inner }),
        );
    }

    let modules = Arc::new(RwLock::new(vec![ParserQueueEntry {
        file: debug_file,
        root: root_directory.clone(),
    }]));
    let mut current_parser = tokenizer.to_parser(modules.clone(), root_directory);
    current_parser.file = file;

    let module_context = Arc::new(ModuleContext::default());

    loop {
        vprintln!("Parsing {:?}", current_parser.file);
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
            vprintln!("Tokenizing {:?}", tokenizer.file);
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
