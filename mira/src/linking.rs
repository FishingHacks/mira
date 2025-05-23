use std::{
    fs::File,
    io::{ErrorKind, Write},
    path::{Path, PathBuf},
    process::{Child, Command, ExitStatus},
    sync::Arc,
};

use inkwell::context::Context;
use parking_lot::RwLock;
use thiserror::Error;

mod parsing;
use parsing::parse_all;

use crate::{
    codegen::{CodegenConfig, CodegenContext, CodegenError, Optimizations},
    error::MiraError,
    module_resolution::ModuleResolver,
    optimizations,
    progress_bar::{ProgressBar, ProgressBarStyle},
    store::AssociatedStore,
    target::{Abi, Arch, Os, Target},
    typechecking::{
        intrinsics::IntrinsicAnnotation,
        ir_displayer::{Formatter, IoWriteWrapper, ReadOnlyTypecheckingContext, TCContextDisplay},
        typechecking::{typecheck_external_function, typecheck_function, typecheck_static},
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
    pub target: Target,
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
    #[error("Unable to find a valid program loader for the operating sytem")]
    UnableToLocateLoader,
    #[error("failed to run command {0:?}: {1}")]
    SpawnFailed(Command, std::io::Error),
    #[error("command {0:?} exited with a failure (exit code {1:?})")]
    UnsuccessfulExit(Command, ExitStatus, Child),
}

struct LdLikeLinker(&'static str);
fn linux_find_loader(is_32_bit: bool) -> Option<PathBuf> {
    for f in std::fs::read_dir("/lib").ok()?.filter_map(Result::ok) {
        let filename = f.file_name();
        let Some(str) = filename.to_str() else {
            continue;
        };
        if is_32_bit && str.starts_with("lib/ld-linux.so.") {
            return Some(f.path());
        }
        if !is_32_bit && str.starts_with("ld-linux-x86-64.so.") {
            return Some(f.path());
        }
    }
    None
}

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
        #[allow(clippy::single_match)]
        match opts.target.arch {
            Arch::X86 => _ = command.arg("-m32"),
            _ => (),
        }
        #[allow(clippy::single_match)]
        match opts.target.abi {
            Abi::Gnu => _ = command.arg("-lc"),
            _ => (),
        }
        #[allow(clippy::single_match)]
        match opts.target.os {
            Os::Linux => {
                let loader = linux_find_loader(opts.target.arch.is_32_bit())
                    .ok_or(LinkerError::UnableToLocateLoader)?;
                command.arg("-dynamic-linker").arg(loader);
            }
            _ => (),
        }

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

        #[allow(clippy::single_match)]
        match opts.target.arch {
            Arch::X86 => _ = command.arg("-m32"),
            _ => (),
        }
        #[allow(clippy::single_match)]
        match opts.target.abi {
            Abi::Gnu => _ = command.arg("-lc"),
            _ => (),
        }

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
    /// The root file will automatically have `use "<always_include>"::*;` at the top, causing this
    /// file to be built into each compilation. Use this for the file where you defined your procedure
    /// that represents the entry point of the executable and calls the main function.
    pub always_include: Option<String>,
    /// A list of resolvers to use to resolve imports.
    pub resolvers: Arc<[Box<dyn ModuleResolver>]>,
    /// The resolvers will use this function to check if a path exists
    pub path_exists: Arc<dyn Fn(&std::path::Path) -> bool + Send + Sync>,
    /// The resolvers will use this function to check if a path is an directory
    pub path_is_dir: Arc<dyn Fn(&std::path::Path) -> bool + Send + Sync>,
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
    /// The path to the resulting executable or dynamic library.
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

macro_rules! setters {
    ($($name:ident => $field:ident : $ty:ty),* $(,)?) => {
        $(pub fn $name(&mut self, value: $ty) -> &mut Self {
            self.$field = value;
            self
        })*
    };
}

#[allow(dead_code)]
impl<'a> FullCompilationOptions<'a> {
    pub fn new(
        file: Arc<Path>,
        root_directory: Arc<Path>,
        resolvers: Arc<[Box<dyn ModuleResolver>]>,
        path_exists: Arc<dyn Fn(&std::path::Path) -> bool + Send + Sync>,
        path_is_dir: Arc<dyn Fn(&std::path::Path) -> bool + Send + Sync>,
    ) -> Self {
        let debug_file = file.clone();
        Self {
            file,
            root_directory,
            debug_file,
            source: None,
            always_include: None,
            resolvers,
            path_exists,
            path_is_dir,
            shared_object: false,
            linker_script: None,
            obj_path: None,
            add_extension_to_exe: false,
            exec_path: None,
            codegen_opts: CodegenConfig::new_debug(),
            link_with_crt: false,
            additional_linker_args: &[],
            additional_linker_directories: &[],
            verbose: false,
            with_debug_info: true,
            ir_writer: None,
            llvm_ir_writer: None,
            llvm_bc_writer: None,
            asm_writer: None,
        }
    }
    setters![
        with_source => source: Option<&'a str>,
        set_file => file: Arc<Path>,
        set_root_dir => root_directory: Arc<Path>,
        set_debug_file => debug_file: Arc<Path>,
        always_include_file => always_include: Option<String>,
        set_module_resolver => resolvers: Arc<[Box<dyn ModuleResolver>]>,
        shared_object => shared_object: bool,
        set_linker_script => linker_script: Option<&'a Path>,
        set_object_path => obj_path: Option<PathBuf>,
        link_with_c_runtime => link_with_crt: bool,
        set_additional_linker_args => additional_linker_args: &'a [String],
        set_additional_linker_searchdirs => additional_linker_directories: &'a [PathBuf],
        set_verbose_printing => verbose: bool,
        generate_debug_info => with_debug_info: bool,
        ir_writer => ir_writer: Option<Box<dyn Write>>,
        llvm_ir_writer => llvm_ir_writer: Option<Box<dyn Write>>,
        llvm_bc_writer => llvm_bc_writer: Option<Box<dyn Write>>,
        asm_writer => asm_writer: Option<Box<dyn Write>>,
        set_codegen_opts => codegen_opts: CodegenConfig<'a>,
    ];
    /// Set the path of the binary and if to add the target-dependent extension to it
    pub fn set_binary_path(
        &mut self,
        executable_path: Option<PathBuf>,
        add_target_specific_extension: bool,
    ) -> &mut Self {
        self.exec_path = executable_path;
        self.add_extension_to_exe = add_target_specific_extension;
        self
    }
    pub fn set_target(&mut self, target: Target) -> &mut Self {
        self.codegen_opts.target = target;
        self
    }
    pub fn set_optimizations(&mut self, optimizations: Optimizations) -> &mut Self {
        self.codegen_opts.optimizations = optimizations;
        self
    }
    pub fn set_runtime_safety(&mut self, runtime_safety: bool) -> &mut Self {
        self.codegen_opts.runtime_safety = runtime_safety;
        self
    }
    pub fn target(&self) -> Target {
        self.codegen_opts.target
    }
}

/// Runs the pipeline to turn a source file into an executable or shared object.
/// Returns the path to the executable
pub fn run_full_compilation_pipeline(
    mut opts: FullCompilationOptions,
) -> Result<Option<PathBuf>, Vec<MiraError>> {
    if opts.add_extension_to_exe && opts.exec_path.is_some() {
        let mut path = opts.exec_path.unwrap().into_os_string();
        if opts.shared_object {
            path.push(opts.codegen_opts.target.os.dynamic_lib_ext());
        } else {
            path.push(opts.codegen_opts.target.os.exe_file_ext());
        }
        let mut path = PathBuf::from(path);
        if path.is_relative() {
            path = match std::env::current_dir() {
                Ok(v) => v,
                Err(e) => {
                    println!("failed to get the current directory");
                    return Err(vec![MiraError::IO { inner: e }]);
                }
            }
            .join(path);
        }
        opts.exec_path = Some(path);
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
    let mut read = String::new();
    let source = opts
        .source
        .map(Ok)
        .unwrap_or_else(|| {
            read = std::fs::read_to_string(&opts.file)?;
            Ok(&read)
        })
        .map_err(|inner| vec![inner])?;

    let progress_bar = Arc::new(RwLock::new(ProgressBar::new(ProgressBarStyle::Normal)));
    let parser_item = progress_bar.write().add_item("Parsing".into());

    let module_context = parse_all(
        opts.file,
        opts.root_directory,
        opts.debug_file.clone(),
        source,
        opts.always_include,
        opts.resolvers,
        opts.path_exists,
        opts.path_is_dir,
        progress_bar.clone(),
        parser_item,
    );
    progress_bar.write().remove_item(parser_item);
    let typechecking_item = progress_bar.write().add_item("Typechecking".into());
    let type_resolution_item = progress_bar.write().add_item("Type Resolution".into());
    print_progress_bar(&progress_bar);

    let module_context = module_context?;
    let typechecking_context = TypecheckingContext::new(module_context.clone());
    let errs = typechecking_context.resolve_imports(module_context.clone());
    if !errs.is_empty() {
        return Err(errs.into_iter().map(Into::into).collect());
    }
    let errs = typechecking_context.resolve_types(module_context.clone());
    if !errs.is_empty() {
        return Err(errs.into_iter().map(Into::into).collect());
    }

    progress_bar.write().remove_item(type_resolution_item);

    let function_keys = typechecking_context
        .functions
        .read()
        .indices()
        .collect::<Vec<_>>();
    let ext_function_keys = typechecking_context
        .external_functions
        .read()
        .indices()
        .collect::<Vec<_>>();
    let static_keys = typechecking_context
        .statics
        .read()
        .indices()
        .collect::<Vec<_>>();

    let mut errs = Vec::new();
    let mut scopes_fns = AssociatedStore::new();
    let mut scopes_ext_fns = AssociatedStore::new();

    for key in function_keys {
        let item = progress_bar.write().add_child_item(
            typechecking_item,
            format!("Typechecking function {key}").into_boxed_str(),
        );
        print_progress_bar(&progress_bar);

        match typecheck_function(&typechecking_context, &module_context, key) {
            Ok(v) => _ = scopes_fns.insert(key, v),
            Err(e) => errs.extend(e),
        }

        progress_bar.write().remove_item(item);
    }

    for key in ext_function_keys {
        let item = progress_bar.write().add_child_item(
            typechecking_item,
            format!("Typechecking external function {key}").into_boxed_str(),
        );
        print_progress_bar(&progress_bar);

        match typecheck_external_function(&typechecking_context, &module_context, key) {
            Ok(v) => _ = scopes_ext_fns.insert(key, v),
            Err(e) => errs.extend(e),
        }

        progress_bar.write().remove_item(item);
    }

    for key in static_keys {
        let item = progress_bar.write().add_child_item(
            typechecking_item,
            format!("Typechecking static {key}").into_boxed_str(),
        );
        print_progress_bar(&progress_bar);

        typecheck_static(&typechecking_context, &module_context, key, &mut errs);

        progress_bar.write().remove_item(item);
    }

    progress_bar.write().remove_item(typechecking_item);
    let dce_item = progress_bar
        .write()
        .add_item("Dead code elimination".into());
    print_progress_bar(&progress_bar);

    optimizations::dead_code_elimination::run_dce(&typechecking_context, &[], &[]);

    progress_bar.write().remove_item(dce_item);
    print_progress_bar(&progress_bar);

    if !errs.is_empty() {
        return Err(errs.into_iter().map(Into::into).collect());
    }

    let mut errs = Vec::new();

    if let Some(writer) = opts.ir_writer.as_mut() {
        let readonly_ctx = ReadOnlyTypecheckingContext {
            modules: &typechecking_context.modules.read(),
            functions: &typechecking_context.functions.read(),
            external_functions: &typechecking_context.external_functions.read(),
            statics: &typechecking_context.statics.read(),
            structs: &typechecking_context.structs.read(),
            traits: &typechecking_context.traits.read(),
            lang_items: &typechecking_context.lang_items.read(),
        };
        let mut wrapper = IoWriteWrapper(writer);
        let mut formatter = Formatter::new(&mut wrapper, readonly_ctx);
        if TCContextDisplay.fmt(&mut formatter).is_err() {
            errs.push(MiraError::IO {
                inner: ErrorKind::Other.into(),
            });
        }
    }

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

    drop(module_context);
    crate::globals::clean_slab();

    let codegen_item = progress_bar.write().add_item("Codegen".into());
    print_progress_bar(&progress_bar);

    for (fn_id, (contract, _)) in typechecking_context.functions.read().index_value_iter() {
        if contract.annotations.has_annotation::<IntrinsicAnnotation>() {
            continue;
        }
        let item = progress_bar.write().add_child_item(
            codegen_item,
            format!("Codegening function #{fn_id}").into_boxed_str(),
        );
        print_progress_bar(&progress_bar);

        if let Err(e) = codegen_context.compile_fn(
            fn_id,
            scopes_fns.remove(&fn_id).expect("scope should be there"),
        ) {
            errs.push(MiraError::Codegen { inner: e.into() });
        }
        progress_bar.write().remove_item(item);
    }

    for fn_id in typechecking_context.external_functions.read().indices() {
        let item = progress_bar.write().add_child_item(
            codegen_item,
            format!("Codegening external function #{fn_id}").into_boxed_str(),
        );
        print_progress_bar(&progress_bar);

        if let Err(e) = codegen_context.compile_external_fn(
            fn_id,
            scopes_ext_fns
                .remove(&fn_id)
                .expect("scope should be there"),
        ) {
            errs.push(MiraError::Codegen { inner: e.into() });
        }
        progress_bar.write().remove_item(item);
    }

    drop(scopes_fns);
    drop(scopes_ext_fns);

    progress_bar
        .write()
        .add_child_item(codegen_item, "Optimizing".into());
    print_progress_bar(&progress_bar);

    if let Err(e) = codegen_context.finish() {
        errs.push(CodegenError::LLVMNative(e).into());
    }

    progress_bar.write().remove_item(codegen_item);
    print_progress_bar(&progress_bar);

    // emit llvm ir even in the case there are errors. LLVM errors are often more-or-less cryptic, and
    // seeing what junk the compiler generated will probably help in diagnosing them.
    // this hopefully should not error.
    if let Some(ir_writer) = opts.llvm_ir_writer.as_mut() {
        if let Err(e) = codegen_context.write_ir(ir_writer) {
            errs.push(e.into());
        }
    }

    if !errs.is_empty() {
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
        drop(codegen_context);
        crate::globals::clean_slab();

        return (errs.is_empty()).then_some(opts.exec_path).ok_or(errs);
    };

    let mut obj_file = match File::options()
        .write(true)
        .create(true)
        .truncate(true)
        .open(&obj_path)
        .map_err(|v| errs.push(v.into()))
    {
        Ok(v) => v,
        Err(()) => return Err(errs),
    };

    if let Err(e) = codegen_context.write_object(&mut obj_file) {
        errs.push(e.into());
    }
    drop(obj_file);

    if !errs.is_empty() {
        return Err(errs);
    }

    let Some(exec_path) = opts.exec_path else {
        return Ok(opts.exec_path);
    };

    let Some((linker, linker_path)) =
        search_for_linker(opts.link_with_crt, opts.additional_linker_directories)
    else {
        return Err(vec![LinkerError::UnableToLocateLinker.into()]);
    };

    linker
        .link(LinkOptions {
            linker_path,
            object_files: &[obj_path],
            inputs: &[
                LinkerInput::LinkLibrary("unwind".into()),
                LinkerInput::LinkLibrary("unwind-x86_64".into()),
            ],
            output: &exec_path,
            args: opts.additional_linker_args,
            link_crt: opts.link_with_crt,
            debug_info: opts.with_debug_info,
            verbose: opts.verbose,
            linker_script: opts.linker_script,
            create_dynamic_library: opts.shared_object,
            target: opts.codegen_opts.target,
        })
        .map_err(|v| vec![v.into()])?;

    drop(codegen_context);
    crate::globals::clean_slab();

    Ok(Some(exec_path))
}

/*
/// Parses a string of text into a module
///
/// `file` - The file the source came from. Used to evaluate relative imports
/// `root_directory` - The path the import `@root/` points to
/// `debug_file` - The file that will appear in locations and debug info
/// `source` - The source that will be parsed
#[allow(clippy::too_many_arguments)]
pub fn parse_all(
    file: Arc<Path>,
    root_directory: Arc<Path>,
    debug_file: Arc<Path>,
    source: &str,
    always_include: Option<String>,
    resolvers: Arc<[Box<dyn ModuleResolver>]>,
    path_exists: Arc<dyn Fn(&std::path::Path) -> bool>,
    path_is_dir: Arc<dyn Fn(&std::path::Path) -> bool>,
    progress_bar: Arc<RwLock<ProgressBar>>,
    parsing_item: ProgressItemRef,
) -> Result<Arc<ModuleContext>, Vec<MiraError>> {
    let mut errors = vec![];

    let mut item = progress_bar.write().add_child_item(
        parsing_item,
        format!("Tokenizing {}", file.display()).into_boxed_str(),
    );
    print_progress_bar(&progress_bar);
    let mut tokenizer = Tokenizer::new(source, debug_file.clone());
    if let Some(path) = always_include {
        let loc = Location::new(debug_file.clone(), 0, 0);
        tokenizer.push_token(Token {
            typ: TokenType::Use,
            literal: None,
            location: loc.clone(),
        });
        tokenizer.push_token(Token {
            typ: TokenType::StringLiteral,
            literal: Some(Literal::String(path.into())),
            location: loc.clone(),
        });
        tokenizer.push_token(Token {
            typ: TokenType::Semicolon,
            literal: None,
            location: loc.clone(),
        });
    }
    if let Err(errs) = tokenizer.scan_tokens() {
        errors.extend(
            errs.into_iter()
                .map(|inner| MiraError::Tokenization { inner }),
        );
    }

    let modules = Arc::new(RwLock::new(vec![ParserQueueEntry {
        file: debug_file,
        root_dir: root_directory.clone(),
    }]));
    let mut current_parser = tokenizer.to_parser(
        modules.clone(),
        root_directory,
        resolvers.clone(),
        path_exists.clone(),
        path_is_dir.clone(),
    );
    current_parser.file = file;

    let module_context = Arc::new(ModuleContext::default());

    loop {
        {
            let mut writer = progress_bar.write();
            writer.remove_item(item);
            item = writer.add_child_item(
                parsing_item,
                format!("Parsing {}", current_parser.file.display()).into_boxed_str(),
            );
        }
        print_progress_bar(&progress_bar);

        let (statements, parsing_errors) = current_parser.parse_all();
        errors.extend(
            parsing_errors
                .into_iter()
                .map(|inner| MiraError::Parsing { inner }),
        );
        let (path, root) = {
            let module = &modules.read()[module_context.modules.read().len()];
            (module.file.clone(), module.root_dir.clone())
        };
        let mut module = Module::new(current_parser.imports, path, root);
        if let Err(errs) = module.push_all(
            statements,
            module_context.modules.read().len(),
            &module_context,
        ) {
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
            let source = match std::fs::read_to_string(&entry.file) {
                Ok(v) => v,
                Err(e) => {
                    errors.push(e.into());
                    return Err(errors);
                }
            };
            let mut tokenizer = Tokenizer::new(&source, entry.file);
            {
                let mut writer = progress_bar.write();
                writer.remove_item(item);
                item = writer.add_child_item(
                    parsing_item,
                    format!("Tokenizing {}", tokenizer.file.display()).into_boxed_str(),
                );
            }
            print_progress_bar(&progress_bar);
            if let Err(errs) = tokenizer.scan_tokens() {
                errors.extend(
                    errs.into_iter()
                        .map(|inner| MiraError::Tokenization { inner }),
                );
            }
            current_parser = tokenizer.to_parser(
                modules.clone(),
                entry.root_dir,
                resolvers.clone(),
                path_exists.clone(),
                path_is_dir.clone(),
            );
        } else {
            progress_bar.write().remove_item(parsing_item);
            print_progress_bar(&progress_bar);
            break;
        }
    }

    if errors.is_empty() {
        Ok(module_context)
    } else {
        Err(errors)
    }
}
*/

struct ClosureDisplay<F: Fn(&mut std::fmt::Formatter) -> std::fmt::Result>(pub F);
impl<F: Fn(&mut std::fmt::Formatter) -> std::fmt::Result> std::fmt::Display for ClosureDisplay<F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (self.0)(f)
    }
}
pub fn print_progress_bar(bar: &RwLock<ProgressBar>) {
    let mut stdout = std::io::stdout().lock();
    stdout
        .write_fmt(format_args!(
            "{}\n",
            ClosureDisplay(|f| bar.write().display(f))
        ))
        .expect("failed to write to stdout");
    _ = stdout.flush();
}
