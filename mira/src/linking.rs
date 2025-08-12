use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::Write,
    mem::MaybeUninit,
    path::{Path, PathBuf},
    process::{Child, Command, ExitStatus},
    sync::Arc,
};

use inkwell::context::Context;
use mira_errors::Diagnostics;
use mira_macros::ErrorData;
use mira_progress_bar::ProgressBarStyle;

mod parsing;
use parsing::parse_all;

use crate::{
    codegen::{CodegenConfig, CodegenContext, CodegenError, Optimizations},
    context::SharedContext,
    error::{CurrentDirError, FailedToWriteIR, FileOpenError, IoReadError},
    optimizations,
    store::AssociatedStore,
    target::{Abi, Arch, Os, Target},
    typechecking::{
        TypecheckingContext,
        intrinsics::IntrinsicAnnotation,
        ir_displayer::{Formatter, IoWriteWrapper, ReadOnlyTypecheckingContext, TCContextDisplay},
        typechecking::{typecheck_external_function, typecheck_function, typecheck_static},
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
    #[allow(clippy::result_large_err)]
    fn link(&self, opts: LinkOptions) -> Result<(), LinkerError>;
}

macro_rules! arg_if {
    ($command:ident $arg:expr, if $cond: expr) => {
        if $cond {
            $command.arg($arg);
        }
    };
}

#[derive(Debug, ErrorData)]
#[no_arena_lifetime]
pub enum LinkerError {
    #[error("Unable to find a valid linker")]
    UnableToLocateLinker,
    #[error("Unable to find a valid program loader for the operating sytem")]
    UnableToLocateLoader,
    #[error("failed to run command {_0:?}: {_1}")]
    SpawnFailed(Command, std::io::Error),
    #[error("command {_0:?} exited with a failure (exit code {_1:?})")]
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

#[derive(Clone, Debug)]
pub enum LibraryInput {
    Path,
    String(Arc<str>),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct LibraryId(usize);

#[derive(Default)]
pub struct LibraryTree {
    main: Option<LibraryId>,
    libraries: Vec<Library>,
    names: HashSet<Arc<str>>,
}

pub struct LibraryBuilder<'tree> {
    tree: &'tree mut LibraryTree,
    root: Arc<Path>,
    root_file_path: Arc<Path>,
    input: LibraryInput,
    dependencies: HashMap<Arc<str>, LibraryId>,
}

impl<'tree> LibraryBuilder<'tree> {
    #[must_use]
    pub fn with_source(mut self, source: Arc<str>) -> Self {
        self.input = LibraryInput::String(source);
        self
    }

    #[must_use]
    pub fn with_dependency(mut self, name: &str, id: LibraryId) -> Self {
        self.dependencies.insert(self.tree.intern(name), id);
        self
    }

    pub fn add_dependency(&mut self, name: &str, id: LibraryId) -> &mut Self {
        self.dependencies.insert(self.tree.intern(name), id);
        self
    }

    pub fn build(self) -> LibraryId {
        let id = LibraryId(self.tree.libraries.len());
        self.tree.libraries.push(Library {
            root: self.root,
            root_file_path: self.root_file_path,
            input: self.input,
            dependencies: self.dependencies,
        });
        id
    }
}

impl LibraryTree {
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn build_library(
        &mut self,
        module_root: Arc<Path>,
        file_path: Arc<Path>,
    ) -> LibraryBuilder {
        LibraryBuilder {
            tree: self,
            root: module_root,
            root_file_path: file_path,
            input: LibraryInput::Path,
            dependencies: HashMap::new(),
        }
    }

    /// the "main" library, aka the executable/library that's currently being compiled
    pub fn main_library(&mut self, library: LibraryId) {
        assert!(
            self.main.is_none(),
            "Only one main library can exist at a time"
        );
        self.main = Some(library);
    }

    fn intern(&mut self, s: &str) -> Arc<str> {
        if let Some(s) = self.names.get(s) {
            return s.clone();
        }
        let s: Arc<str> = s.into();
        self.names.insert(s.clone());
        s
    }
}

pub struct Library {
    pub root: Arc<Path>,
    pub root_file_path: Arc<Path>,
    pub input: LibraryInput,
    pub dependencies: HashMap<Arc<str>, LibraryId>,
}

pub struct FullCompilationOptions<'a> {
    /// the packages in this compilation
    pub library_tree: LibraryTree,
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
    pub fn new(library_tree: LibraryTree) -> Self {
        Self {
            library_tree,
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
    pub fn library_tree(&mut self) -> &mut LibraryTree {
        &mut self.library_tree
    }
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
pub fn run_full_compilation_pipeline<'arena>(
    ctx: SharedContext<'arena>,
    mut opts: FullCompilationOptions,
) -> Result<Option<PathBuf>, Diagnostics<'arena>> {
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
                Err(e) => return Err(CurrentDirError(e).to_error().into()),
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

    let source_map = ctx.source_map();
    assert!(
        opts.library_tree.main.is_some(),
        "No library was selected as the main one"
    );
    let main_lib_id = opts.library_tree.main.unwrap();
    let mut packages = Vec::new();

    for library in opts.library_tree.libraries.into_iter() {
        let deps = library
            .dependencies
            .into_iter()
            .map(|(k, v)| (k, packages[v.0]))
            .collect();
        let (package, _) = match library.input {
            LibraryInput::Path => source_map
                .add_package_load(library.root, library.root_file_path.clone(), deps)
                .map_err(|e| IoReadError(library.root_file_path.to_path_buf(), e).to_error())?,
            LibraryInput::String(s) => {
                source_map.add_package(library.root, library.root_file_path, s, deps)
            }
        };
        packages.push(package.id);
    }
    let main_lib = packages[main_lib_id.0];
    drop(packages);
    drop(opts.library_tree.names);
    ctx.source_map().set_main_package(main_lib);

    let (thread, mut progress_bar) =
        mira_progress_bar::print_thread::start_thread(ProgressBarStyle::Normal);
    let _deferred = DeferFn::new(move || _ = thread.join());
    let parser_item = progress_bar.add_item("Parsing".into());

    let module_context = parse_all(ctx, progress_bar.clone(), parser_item);
    progress_bar.remove(parser_item);
    let typechecking_item = progress_bar.add_item("Typechecking".into());
    let type_resolution_item = progress_bar.add_item("Type Resolution".into());

    let module_context = module_context?;
    let typechecking_context = TypecheckingContext::new(ctx, module_context.clone());
    let errs = typechecking_context.resolve_imports(module_context.clone());
    if !errs.is_empty() {
        return Err(errs);
    }
    let errs = typechecking_context.resolve_types(module_context.clone());
    if !errs.is_empty() {
        return Err(errs);
    }

    progress_bar.remove(type_resolution_item);

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

    let mut errs = Diagnostics::new();
    let mut scopes_fns = AssociatedStore::new();
    let mut scopes_ext_fns = AssociatedStore::new();

    for key in function_keys {
        let item = progress_bar.add_child(
            typechecking_item,
            format!("Typechecking function {key}").into_boxed_str(),
        );

        if let Ok(v) = typecheck_function(&typechecking_context, &module_context, key, &mut errs) {
            scopes_fns.insert(key, v);
        }

        progress_bar.remove(item);
    }

    for key in ext_function_keys {
        let item = progress_bar.add_child(
            typechecking_item,
            format!("Typechecking external function {key}").into_boxed_str(),
        );

        if let Ok(v) =
            typecheck_external_function(&typechecking_context, &module_context, key, &mut errs)
        {
            scopes_ext_fns.insert(key, v);
        }

        progress_bar.remove(item);
    }

    for key in static_keys {
        let item = progress_bar.add_child(
            typechecking_item,
            format!("Typechecking static {key}").into_boxed_str(),
        );

        typecheck_static(&typechecking_context, &module_context, key, &mut errs);

        progress_bar.remove(item);
    }

    progress_bar.remove(typechecking_item);

    if let Err(err) = typechecking_context.validate_main_function(ctx, &module_context) {
        errs.add_err(err);
    }

    let dce_item = progress_bar.add_item("Dead code elimination".into());

    optimizations::dead_code_elimination::run_dce(&typechecking_context, &[], &[]);

    progress_bar.remove(dce_item);

    if !errs.is_empty() {
        return Err(errs);
    }

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
            errs.add_err(FailedToWriteIR);
        }
    }

    let context = Context::create();

    let root_file = ctx.source_map().packages()[0].root_file;
    let file = ctx.source_map().get_file(root_file).unwrap().path.clone();
    let filename = file
        .file_name()
        .expect("file should have a filename")
        .to_string_lossy();
    let mut codegen_context = CodegenContext::new(
        &context,
        typechecking_context.clone(),
        &filename,
        file.clone(),
        opts.codegen_opts,
        ctx,
    )
    .expect("failed to create the llvm context");

    drop(module_context);

    let codegen_item = progress_bar.add_item("Codegen".into());

    for (fn_id, (contract, _)) in typechecking_context.functions.read().index_value_iter() {
        if contract.annotations.has_annotation::<IntrinsicAnnotation>() {
            continue;
        }
        let item = progress_bar.add_child(
            codegen_item,
            format!("Codegening function #{fn_id}").into_boxed_str(),
        );

        if let Err(e) = codegen_context.compile_fn(
            fn_id,
            scopes_fns.remove(&fn_id).expect("scope should be there"),
        ) {
            errs.add_err(CodegenError::from(e));
        }
        progress_bar.remove(item);
    }

    for fn_id in typechecking_context.external_functions.read().indices() {
        let item = progress_bar.add_child(
            codegen_item,
            format!("Codegening external function #{fn_id}").into_boxed_str(),
        );

        if let Err(e) = codegen_context.compile_external_fn(
            fn_id,
            scopes_ext_fns
                .remove(&fn_id)
                .expect("scope should be there"),
        ) {
            errs.add_err(CodegenError::from(e));
        }
        progress_bar.remove(item);
    }

    drop(scopes_fns);
    drop(scopes_ext_fns);

    progress_bar.add_child(codegen_item, "Optimizing".into());

    if let Err(e) = codegen_context.finish() {
        errs.add_err(CodegenError::from(e));
    }

    progress_bar.remove(codegen_item);

    // emit llvm ir even in the case there are errors. LLVM errors are often more-or-less cryptic, and
    // seeing what junk the compiler generated will probably help in diagnosing them.
    // this hopefully should not error.
    if let Some(ir_writer) = opts.llvm_ir_writer.as_mut() {
        if let Err(e) = codegen_context.write_ir(ir_writer) {
            errs.add_err(CodegenError::WriteLLVMIRError(e));
        }
    }

    if !errs.is_empty() {
        return Err(errs);
    }

    if let Some(bc_writer) = opts.llvm_bc_writer.as_mut() {
        if let Err(e) = codegen_context.write_bitcode(bc_writer) {
            errs.add_err(CodegenError::WriteBitcodeError(e));
        }
    }

    if let Some(asm_writer) = opts.asm_writer.as_mut() {
        if let Err(e) = codegen_context.write_assembly(asm_writer) {
            errs.add_err(e);
        }
    }

    let Some(obj_path) = opts.obj_path else {
        drop(codegen_context);

        return (errs.is_empty()).then_some(opts.exec_path).ok_or(errs);
    };

    let mut obj_file = match File::options()
        .write(true)
        .create(true)
        .truncate(true)
        .open(&obj_path)
        .map_err(|e| _ = errs.add_err(FileOpenError(obj_path.clone(), e)))
    {
        Ok(v) => v,
        Err(()) => return Err(errs),
    };

    if let Err(e) = codegen_context.write_object(&mut obj_file) {
        errs.add_err(e);
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
        errs.add_unable_to_locate_linker();
        return Err(errs);
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
        .map_err(move |v| {
            errs.add_err(v);
            errs
        })?;

    drop(codegen_context);

    Ok(Some(exec_path))
}

struct DeferFn<F: FnOnce()>(MaybeUninit<F>);
impl<F: FnOnce()> Drop for DeferFn<F> {
    fn drop(&mut self) {
        // SAFETY: drop should only be called once, and this is the only place where we remove the
        // function
        let func = unsafe { self.0.assume_init_read() };
        func()
    }
}
impl<F: FnOnce()> DeferFn<F> {
    pub fn new(f: F) -> Self {
        Self(MaybeUninit::new(f))
    }
}
