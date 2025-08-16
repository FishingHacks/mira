use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::Write,
    mem::MaybeUninit,
    path::{Path, PathBuf},
    sync::Arc,
};

use mira_errors::{
    CurrentDirError, Diagnostic, Diagnostics, FileOpenError, IoReadError, IoWriteError,
    StdoutWriteError,
};
use mira_progress_bar::ProgressBarStyle;

mod parsing;
pub use parsing::expand_macros;
use parsing::parse_all;

use mira_common::store::AssociatedStore;
use mira_linking::{self, LinkOptions, LinkerErrorDiagnosticsExt as _, LinkerInput};
use mira_parser::std_annotations::intrinsic::IntrinsicAnnotation;
use mira_target::Target;
use mira_typeck::{
    TypeCtx, TypecheckingContext,
    ir_displayer::{Formatter, ReadOnlyTypecheckingContext, TCContextDisplay},
    optimizations,
    typechecking::{typecheck_external_function, typecheck_function, typecheck_static},
};
// TODO: This should be abstracted away so we don't have to handle different configs.
use mira_llvm_backend::{CodegenConfig, CodegenContextBuilder, CodegenError, Optimizations};

pub use mira_errors::{AsciiPrinter, DiagnosticFormatter, Output, UnicodePrinter};
pub use mira_spans::Arena;

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

struct EmitMethodWithDiags<'a, 'b, 'arena>(&'a mut EmitMethod, &'b mut Diagnostics<'arena>);

impl<'a, 'b, 'arena> EmitMethodWithDiags<'a, 'b, 'arena> {
    pub fn new(method: &'a mut EmitMethod, diags: &'b mut Diagnostics<'arena>) -> Self {
        Self(method, diags)
    }
}

impl std::fmt::Write for EmitMethodWithDiags<'_, '_, '_> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.0
            .emit_diags(s.as_bytes(), self.1)
            .then_some(())
            .ok_or(std::fmt::Error)
    }
}

#[derive(Debug)]
pub enum EmitMethod {
    None,
    Stdout,
    File(Box<Path>, Option<File>),
}

impl EmitMethod {
    pub const fn none() -> Self {
        Self::None
    }
    pub const fn stdout() -> Self {
        Self::Stdout
    }
    pub const fn file(path: Box<Path>) -> Self {
        Self::File(path, None)
    }

    pub fn is_void(&self) -> bool {
        matches!(self, EmitMethod::None)
    }

    pub fn emit(&mut self, data: &[u8]) -> Result<(), Diagnostic<'static>> {
        match self {
            EmitMethod::None => Ok(()),
            EmitMethod::Stdout => std::io::stdout()
                .write_all(data)
                .map_err(|v| StdoutWriteError(v).to_error()),
            EmitMethod::File(path, Some(f)) => f
                .write_all(data)
                .map_err(|v| IoWriteError(path.to_path_buf(), v).to_error()),
            EmitMethod::File(path, f @ None) => {
                let mut file = File::create(&path)
                    .map_err(|v| FileOpenError(path.to_path_buf(), v).to_error())?;
                let res = file
                    .write_all(data)
                    .map_err(|v| IoWriteError(path.to_path_buf(), v).to_error());
                *f = Some(file);
                res
            }
        }
    }

    /// emits the error into the diagnostics struct, returning if the operating was successful.
    pub fn emit_diags(&mut self, data: &[u8], diagnostics: &mut Diagnostics) -> bool {
        self.emit(data).map_err(|v| _ = diagnostics.add(v)).is_ok()
    }
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
    pub ir_writer: EmitMethod,
    /// Writer to write the llvm intermediate representation to
    pub llvm_ir_writer: EmitMethod,
    /// Writer to write the llvm bitcode to
    pub llvm_bc_writer: EmitMethod,
    /// Writer to write the assembly to
    pub asm_writer: EmitMethod,
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
            ir_writer: EmitMethod::None,
            llvm_ir_writer: EmitMethod::None,
            llvm_bc_writer: EmitMethod::None,
            asm_writer: EmitMethod::None,
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
        ir_writer => ir_writer: EmitMethod,
        llvm_ir_writer => llvm_ir_writer: EmitMethod,
        llvm_bc_writer => llvm_bc_writer: EmitMethod,
        asm_writer => asm_writer: EmitMethod,
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
    ctx: TypeCtx<'arena>,
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

    let module_context = parse_all(ctx.into(), progress_bar.clone(), parser_item);
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

    if !opts.ir_writer.is_void() {
        let readonly_ctx = ReadOnlyTypecheckingContext {
            modules: &typechecking_context.modules.read(),
            functions: &typechecking_context.functions.read(),
            external_functions: &typechecking_context.external_functions.read(),
            statics: &typechecking_context.statics.read(),
            structs: &typechecking_context.structs.read(),
            traits: &typechecking_context.traits.read(),
            lang_items: &typechecking_context.lang_items.read(),
        };
        let mut wrapper = EmitMethodWithDiags::new(&mut opts.ir_writer, &mut errs);
        let mut formatter = Formatter::new(&mut wrapper, readonly_ctx);
        // it is fine to dismiss errors because they got added to `errs` anyway.
        _ = TCContextDisplay.fmt(&mut formatter);
    }

    let root_file = ctx.source_map().packages()[0].root_file;
    let file = ctx.source_map().get_file(root_file).unwrap().path.clone();
    let filename = file
        .file_name()
        .expect("file should have a filename")
        .to_string_lossy();
    let ctx_builder = CodegenContextBuilder::new();
    let mut codegen_context = ctx_builder
        .build(
            typechecking_context.clone(),
            &filename,
            file.clone(),
            opts.codegen_opts,
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
    if !opts.llvm_ir_writer.is_void() {
        let ir = codegen_context.gen_ir();
        _ = opts.llvm_ir_writer.emit_diags(ir.to_bytes(), &mut errs);
    }

    if !errs.is_empty() {
        return Err(errs);
    }

    if !opts.llvm_bc_writer.is_void() {
        let bitcode = codegen_context.gen_bitcode();
        _ = opts
            .llvm_bc_writer
            .emit_diags(bitcode.as_slice(), &mut errs);
    }

    if !opts.asm_writer.is_void() {
        match codegen_context.gen_assembly() {
            Ok(asm) => _ = opts.asm_writer.emit_diags(asm.as_slice(), &mut errs),
            Err(e) => _ = errs.add_err(e),
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
        mira_linking::search_for_linker(opts.link_with_crt, opts.additional_linker_directories)
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

#[cfg(not(feature = "mira-llvm-backend"))]
compile_error!("you have to enable llvm20-1, llvm19-1 or llvm18-0");
#[cfg(any(
    all(feature = "llvm20-1", feature = "llvm19-1"),
    all(feature = "llvm20-1", feature = "llvm18-0"),
    all(feature = "llvm19-1", feature = "llvm18-0"),
))]
compile_error!("only one of llvm20-1, llvm19-1 or llvm18-0 are allowed to be enabled");
