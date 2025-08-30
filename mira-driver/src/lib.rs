use std::{fmt::Arguments, fs::File, io::Write, path::Path, sync::Arc};

use mira_common::store::StoreKey;
use mira_context::DiagEmitter as DiagEmitMethod;
use mira_context::SharedCtx;
use mira_errors::{Diagnostic, Diagnostics, FileOpenError, IoWriteError, StdoutWriteError};
use mira_progress_bar::{ProgressItemRef, print_thread::ProgressBarThread};

pub use libfinder::find_library;
pub use libtree::*;
pub use mira_progress_bar::ProgressBarStyle;

mod libfinder;
mod libtree;
mod parsing;

#[cfg(not(feature = "typeck"))]
use mira_context::GlobalCtx;

#[cfg(feature = "typeck")]
use mira_spans::Arena;
#[cfg(feature = "typeck")]
use mira_typeck::GlobalContext;

#[cfg(feature = "macros")]
use mira_lexer::Token;

#[cfg(feature = "parsing")]
use mira_parser::module::{Module, ModuleContext};

#[cfg(feature = "typeck")]
use mira_typeck::{TypeCtx, TypecheckingContext, TypedFunction, ir_displayer::Formatter};

#[cfg(feature = "codegen")]
use mira_llvm_backend::{CodegenConfig, CodegenContext, CodegenContextBuilder, CodegenError};

#[cfg(feature = "linking")]
use mira_linking::LinkerInput;
#[cfg(feature = "linking")]
use mira_target::Target;
#[cfg(feature = "linking")]
use std::path::PathBuf;

pub struct ErrorEmitted;

pub type EmitResult<T> = Result<T, ErrorEmitted>;

struct EmitMethodWithDiags<'a, 'b, 'arena>(&'a mut EmitMethod, &'b mut Diagnostics<'arena>);

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
    Stdout,
    File(Box<Path>, Option<File>),
}

impl EmitMethod {
    pub const fn stdout() -> Self {
        Self::Stdout
    }
    pub const fn file(path: Box<Path>) -> Self {
        Self::File(path, None)
    }

    pub fn emit_fmt(&mut self, args: Arguments<'_>) -> Result<(), Diagnostic<'static>> {
        match self {
            EmitMethod::Stdout => std::io::stdout()
                .write_fmt(args)
                .map_err(|v| StdoutWriteError(v).to_error()),
            EmitMethod::File(path, Some(f)) => f
                .write_fmt(args)
                .map_err(|v| IoWriteError(path.to_path_buf(), v).to_error()),
            EmitMethod::File(path, f @ None) => {
                let mut file = File::create(&path)
                    .map_err(|v| FileOpenError(path.to_path_buf(), v).to_error())?;
                let res = file
                    .write_fmt(args)
                    .map_err(|v| IoWriteError(path.to_path_buf(), v).to_error());
                *f = Some(file);
                res
            }
        }
    }

    pub fn emit(&mut self, data: &[u8]) -> Result<(), Diagnostic<'static>> {
        match self {
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

pub struct ContextData {
    // #[cfg(not(feature = "typeck"))]
    // ctx: GlobalCtx<'ctx>,
    // #[cfg(feature = "typeck")]
    // ctx: GlobalContext<'ctx>,
    progress_bar: ProgressBarThread,
}

pub enum DiagEmitter {
    Stdout,
    Stderr,
    File(File),
    NoFail,
    Discard,
}

impl ContextData {
    #[cfg(feature = "typeck")]
    /// if no style is specified, no bar will be displayed.
    pub fn new<'ctx>(
        arena: &'ctx Arena,
        progress_bar_style: Option<ProgressBarStyle>,
        emitter: DiagEmitter,
    ) -> (GlobalContext<'ctx>, Self) {
        let progress_bar = mira_progress_bar::print_thread::start_thread(progress_bar_style);
        let emitter = match emitter {
            DiagEmitter::Stdout => DiagEmitMethod::Stdout(progress_bar.clone()),
            DiagEmitter::Stderr => DiagEmitMethod::Stderr(progress_bar.clone()),
            DiagEmitter::File(file) => DiagEmitMethod::File(file),
            DiagEmitter::NoFail => DiagEmitMethod::NoFail,
            DiagEmitter::Discard => DiagEmitMethod::Discard,
        };
        let ctx = GlobalContext::new(
            arena,
            emitter,
            mira_errors::env_printer(),
            mira_errors::env_style(),
        );
        let me = Self { progress_bar };
        (ctx, me)
    }

    #[cfg(not(feature = "typeck"))]
    /// if no style is specified, no bar will be displayed.
    pub fn new<'ctx>(
        arena: &'ctx Arena,
        progress_bar_style: Option<ProgressBarStyle>,
        emitter: DiagEmitter,
    ) -> (GlobalCtx<'ctx>, Self) {
        let (thread, progress_bar) =
            mira_progress_bar::print_thread::start_thread(progress_bar_style);
        let emitter = match emitter {
            DiagEmitter::Stdout => DiagEmitMethod::Stdout(progress_bar.clone()),
            DiagEmitter::Stderr => DiagEmitMethod::Stderr(progress_bar.clone()),
            DiagEmitter::File(file) => DiagEmitMethod::File(file),
            DiagEmitter::NoFail => DiagEmitMethod::NoFail,
            DiagEmitter::Discard => DiagEmitMethod::Discard,
        };
        let ctx = GlobalContext::new(
            arena,
            emitter,
            mira_errors::env_printer(),
            mira_errors::env_style(),
        );
        let me = Self {
            progress_bar,
            _pbthread: DropThreadJoin(Some(thread)),
        };
        (ctx, me)
    }

    #[cfg(feature = "typeck")]
    pub fn to_context<'ctx>(self, ctx: TypeCtx<'ctx>) -> Context<'ctx> {
        Context { ctx, data: self }
    }

    #[cfg(not(feature = "typeck"))]
    pub fn to_context<'ctx>(self, ctx: SharedCtx<'ctx>) -> Context<'ctx> {
        Context { ctx, data: self }
    }
}

pub struct Context<'ctx> {
    #[cfg(not(feature = "typeck"))]
    ctx: SharedCtx,
    #[cfg(feature = "typeck")]
    ctx: TypeCtx<'ctx>,
    data: ContextData,
}

impl<'ctx> Context<'ctx> {
    fn shared_ctx(&self) -> SharedCtx<'ctx> {
        #[cfg(feature = "typeck")]
        return self.ctx.to_shared();
        #[cfg(not(feature = "typeck"))]
        self.ctx
    }

    #[cfg(feature = "macros")]
    pub fn expand_macros(&mut self, file: Arc<Path>) -> EmitResult<Vec<Token<'ctx>>> {
        use mira_errors::IoReadError;
        use mira_lexer::{Lexer, LexingError};

        let f = match self
            .shared_ctx()
            .source_map
            .load_file(file.clone(), file.parent().unwrap().into())
        {
            Ok(v) => v,
            Err(e) => {
                self.ctx
                    .emit_diag(IoReadError(file.to_path_buf(), e).to_error());
                return Err(ErrorEmitted);
            }
        };
        let mut lexer = Lexer::new(self.shared_ctx(), f);
        _ = lexer.scan_tokens().map_err(|e| {
            self.ctx
                .emit_diags(e.into_iter().map(LexingError::to_error))
        });

        let file = lexer.file.clone();
        let tokens = lexer.into_tokens();
        let mut diags = Diagnostics::new();
        match mira_parser::expand_tokens(self.shared_ctx(), file.clone(), tokens, &mut diags) {
            Some(v) => Ok(v),
            None => {
                self.ctx.emit_diags(diags);
                Err(ErrorEmitted)
            }
        }
    }

    fn emit(&self, method: &mut EmitMethod, args: Arguments<'_>) -> EmitResult<()> {
        match method.emit_fmt(args) {
            Ok(()) => Ok(()),
            Err(diag) => {
                self.ctx.emit_diag(diag);
                Err(ErrorEmitted)
            }
        }
    }

    #[cfg(feature = "macros")]
    pub fn emit_tokens(&self, tokens: &[Token<'ctx>], mut method: EmitMethod) -> EmitResult<()> {
        for tok in tokens {
            self.emit(&mut method, format_args!("{tok} "))?;
        }
        self.emit(&mut method, format_args!("\n"))
    }

    #[cfg(feature = "parsing")]
    /// Parses all files, returning a shared reference to the module context as well as the key to
    /// the primary module of the library tree.
    pub fn parse_all_files(
        &mut self,
        libtree: &LibraryTree,
    ) -> EmitResult<(Arc<ModuleContext<'ctx>>, StoreKey<Module<'ctx>>)> {
        use crate::parsing::parse_all;

        let parser_item = self.data.progress_bar.add_item("Parsing".into());

        let res = parse_all(
            self.shared_ctx(),
            self.data.progress_bar.clone(),
            parser_item,
            libtree,
        );
        self.data.progress_bar.remove(parser_item);
        let v = res
            .map_err(|v| self.ctx.emit_diags(v))
            .map_err(|()| ErrorEmitted)?;
        Ok(v)
    }

    pub fn add_progress_item(&mut self, item: impl Into<Box<str>>) -> ProgressItemRef {
        self.data.progress_bar.add_item(item.into())
    }

    pub fn add_progress_item_child(
        &mut self,
        parent: ProgressItemRef,
        item: impl Into<Box<str>>,
    ) -> ProgressItemRef {
        self.data.progress_bar.add_child(parent, item.into())
    }

    pub fn add_typechecking_item(&mut self) -> ProgressItemRef {
        self.add_progress_item("Typechecking")
    }

    pub fn add_codegen_item(&mut self) -> ProgressItemRef {
        self.add_progress_item("Codegen")
    }

    pub fn remove_progress_item(&mut self, item: ProgressItemRef) {
        self.data.progress_bar.remove(item);
    }

    #[cfg(feature = "typeck")]
    pub fn resolve_types(
        &mut self,
        module_ctx: &Arc<ModuleContext<'ctx>>,
        typechecking_item: ProgressItemRef,
    ) -> EmitResult<Arc<TypecheckingContext<'ctx>>> {
        use mira_typeck::TypecheckingContext;

        let type_resolution_item =
            self.add_progress_item_child(typechecking_item, "Type Resolution");

        let typeck_ctx = TypecheckingContext::new(self.ctx, module_ctx.clone());
        let tracker = self.ctx.track_errors();
        typeck_ctx.resolve_imports(module_ctx.clone());
        if self.ctx.errors_happened(tracker) {
            return Err(ErrorEmitted);
        }
        typeck_ctx.resolve_types(module_ctx.clone());
        if self.ctx.errors_happened(tracker) {
            return Err(ErrorEmitted);
        }

        self.remove_progress_item(type_resolution_item);
        Ok(typeck_ctx)
    }

    #[cfg(feature = "typeck")]
    pub fn typecheck_items(
        &mut self,
        module_ctx: &Arc<ModuleContext<'ctx>>,
        typeck_ctx: &Arc<TypecheckingContext<'ctx>>,
        typechecking_item: ProgressItemRef,
    ) -> EmitResult<()> {
        let function_keys = typeck_ctx.functions.read().indices().collect::<Vec<_>>();
        let ext_function_keys = typeck_ctx
            .external_functions
            .read()
            .indices()
            .collect::<Vec<_>>();
        let static_keys = typeck_ctx.statics.read().indices().collect::<Vec<_>>();

        let mut errs = Diagnostics::new();

        for key in function_keys {
            use mira_typeck::typechecking::typecheck_function;

            let item = self
                .add_progress_item_child(typechecking_item, format!("Typechecking function {key}"));

            _ = typecheck_function(typeck_ctx, module_ctx, key, &mut errs);
            mira_typeck::ir::passes::run_passes(
                &mut typeck_ctx.functions.write()[key.cast()].1,
                self.ctx,
            );

            self.remove_progress_item(item);
        }

        for key in ext_function_keys {
            use mira_typeck::typechecking::typecheck_external_function;

            let item = self.add_progress_item_child(
                typechecking_item,
                format!("Typechecking external function {key}"),
            );

            _ = typecheck_external_function(typeck_ctx, module_ctx, key, &mut errs);
            if let Some(body) = typeck_ctx.external_functions.write()[key].1.as_mut() {
                mira_typeck::ir::passes::run_passes(body, self.ctx);
            }

            self.remove_progress_item(item);
        }

        for key in static_keys {
            use mira_typeck::typechecking::typecheck_static;

            let item = self
                .add_progress_item_child(typechecking_item, format!("Typechecking static {key}"));

            typecheck_static(typeck_ctx, module_ctx, key, &mut errs);

            self.remove_progress_item(item);
        }

        if !errs.is_empty() {
            self.ctx.emit_diags(errs);
            return Err(ErrorEmitted);
        }

        Ok(())
    }

    #[cfg(feature = "typeck")]
    /// Checks that the main function exists in the main module, and verifies it has a valid
    /// signature, then returns it.
    pub fn validate_main_fn(
        &mut self,
        typeck_ctx: &Arc<TypecheckingContext<'ctx>>,
        main_module: StoreKey<Module<'ctx>>,
    ) -> EmitResult<StoreKey<TypedFunction<'ctx>>> {
        typeck_ctx
            .validate_main_function(main_module)
            .map_err(|v| self.ctx.emit_diag(v.to_error()))
            .map_err(|()| ErrorEmitted)
    }

    #[cfg(feature = "typeck")]
    pub fn run_dead_code_elimination(&mut self, typeck_ctx: &Arc<TypecheckingContext<'ctx>>) {
        use mira_typeck::optimizations;

        let item = self.add_progress_item("Dead code elimination");

        optimizations::dead_code_elimination::run_dce(typeck_ctx, &[], &[]);

        self.remove_progress_item(item);
    }

    #[cfg(feature = "typeck")]
    pub fn emit_ir(
        &self,
        typeck_ctx: &TypecheckingContext<'ctx>,
        mut method: EmitMethod,
    ) -> EmitResult<()> {
        use mira_typeck::ir_displayer::{ReadOnlyTypecheckingContext, TCContextDisplay};

        use crate::EmitMethodWithDiags;

        let readonly_ctx = ReadOnlyTypecheckingContext {
            modules: &typeck_ctx.modules.read(),
            functions: &typeck_ctx.functions.read(),
            external_functions: &typeck_ctx.external_functions.read(),
            statics: &typeck_ctx.statics.read(),
            structs: &typeck_ctx.structs.read(),
            traits: &typeck_ctx.traits.read(),
            lang_items: &typeck_ctx.lang_items.read(),
        };
        let mut errs = Diagnostics::new();
        let mut writer = EmitMethodWithDiags(&mut method, &mut errs);
        let mut formatter = Formatter::new(&mut writer, readonly_ctx);
        match TCContextDisplay.fmt(&mut formatter) {
            Ok(()) => Ok(()),
            Err(_) => {
                self.ctx.emit_diags(errs);
                Err(ErrorEmitted)
            }
        }
    }

    #[cfg(feature = "codegen")]
    pub fn codegen<'cg_ctx>(
        &mut self,
        typeck_ctx: &Arc<TypecheckingContext<'ctx>>,
        main_module: StoreKey<Module<'ctx>>,
        codegen_opts: CodegenConfig<'ctx>,
        builder: &'cg_ctx CodegenContextBuilder,
        codegen_item: ProgressItemRef,
    ) -> (CodegenContext<'cg_ctx, 'ctx>, EmitResult<()>)
    where
        'ctx: 'cg_ctx,
    {
        use mira_llvm_backend::CodegenError;

        let file = typeck_ctx.modules.read()[main_module].file.path.clone();
        let filename = file
            .file_name()
            .expect("file should have a filename")
            .to_string_lossy();

        let mut codegen_context = builder
            .build(typeck_ctx.clone(), &filename, file.clone(), codegen_opts)
            .expect("failed to create the llvm context");
        let mut errs = Diagnostics::new();

        for (fn_id, (contract, _)) in typeck_ctx.functions.read().index_value_iter() {
            use mira_parser::std_annotations::{
                intrinsic::IntrinsicAnnotation, llvm_intrinsic::LLVMIntrinsicAnnotation,
            };

            if contract.annotations.has_annotation::<IntrinsicAnnotation>()
                || contract
                    .annotations
                    .has_annotation::<LLVMIntrinsicAnnotation>()
            {
                continue;
            }
            let item =
                self.add_progress_item_child(codegen_item, format!("Codegening function #{fn_id}"));

            if let Err(e) = codegen_context.compile_fn(fn_id) {
                errs.add_err(CodegenError::from(e));
            }
            self.remove_progress_item(item);
        }

        for fn_id in typeck_ctx.external_functions.read().indices() {
            let item = self.add_progress_item_child(
                codegen_item,
                format!("Codegening external function #{fn_id}"),
            );

            if let Err(e) = codegen_context.compile_external_fn(fn_id) {
                errs.add_err(CodegenError::from(e));
            }
            self.remove_progress_item(item);
        }

        (
            codegen_context,
            (errs.is_empty())
                .then_some(())
                .ok_or(ErrorEmitted)
                .inspect_err(|_| self.ctx.emit_diags(errs)),
        )
    }

    #[cfg(feature = "codegen")]
    pub fn optimize(
        &mut self,
        codegen_ctx: &CodegenContext<'_, 'ctx>,
        codegen_item: ProgressItemRef,
    ) -> EmitResult<()> {
        self.add_progress_item_child(codegen_item, "Optimizing");

        if let Err(e) = codegen_ctx.finish() {
            self.remove_progress_item(codegen_item);
            self.ctx.emit_diag(CodegenError::from(e).to_error());
            return Err(ErrorEmitted);
        }

        self.remove_progress_item(codegen_item);
        Ok(())
    }

    #[cfg(feature = "codegen")]
    pub fn emit_llvm_ir(
        &self,
        codegen_ctx: &CodegenContext<'_, 'ctx>,
        mut method: EmitMethod,
    ) -> EmitResult<()> {
        let ir = codegen_ctx.gen_ir();
        match method.emit(ir.to_bytes()) {
            Ok(()) => Ok(()),
            Err(e) => {
                self.ctx.emit_diag(e);
                Err(ErrorEmitted)
            }
        }
    }

    #[cfg(feature = "codegen")]
    pub fn emit_llvm_bc(
        &self,
        codegen_ctx: &CodegenContext<'_, 'ctx>,
        mut method: EmitMethod,
    ) -> EmitResult<()> {
        let bitcode = codegen_ctx.gen_bitcode();
        match method.emit(bitcode.as_slice()) {
            Ok(()) => Ok(()),
            Err(e) => {
                self.ctx.emit_diag(e);
                Err(ErrorEmitted)
            }
        }
    }

    #[cfg(feature = "codegen")]
    pub fn emit_asm(
        &self,
        codegen_ctx: &CodegenContext<'_, 'ctx>,
        mut method: EmitMethod,
    ) -> EmitResult<()> {
        let asm = codegen_ctx
            .gen_assembly()
            .map_err(|v| self.ctx.emit_diag(v.to_error()))
            .map_err(|()| ErrorEmitted)?;
        match method.emit(asm.as_slice()) {
            Ok(()) => Ok(()),
            Err(e) => {
                self.ctx.emit_diag(e);
                Err(ErrorEmitted)
            }
        }
    }

    #[cfg(feature = "codegen")]
    pub fn emit_object(
        &self,
        codegen_ctx: &CodegenContext<'_, 'ctx>,
        path: &Path,
    ) -> EmitResult<()> {
        use mira_errors::FileOpenError;
        use std::fs::File;

        let mut obj_file = File::options()
            .write(true)
            .create(true)
            .truncate(true)
            .open(path)
            .map_err(|e| FileOpenError(path.to_path_buf(), e).to_error())
            .map_err(|e| self.ctx.emit_diag(e))
            .map_err(|()| ErrorEmitted)?;

        codegen_ctx
            .write_object(&mut obj_file)
            .map_err(|e| self.ctx.emit_diag(e.to_error()))
            .map_err(|()| ErrorEmitted)
    }

    #[cfg(feature = "linking")]
    pub fn link(&self, mut opts: LinkOpts) -> EmitResult<()> {
        use mira_linking::{LinkOptions, LinkerError};

        let Some((linker, linker_path)) = mira_linking::search_for_linker(
            opts.target.needs_crt(),
            opts.additional_linker_searchdir,
        ) else {
            self.ctx
                .emit_diag(LinkerError::UnableToLocateLinker.to_error());
            return Err(ErrorEmitted);
        };

        opts.input.push(LinkerInput::LinkLibrary("unwind".into()));
        opts.input
            .push(LinkerInput::LinkLibrary("unwind-x86_64".into()));
        linker
            .link(LinkOptions {
                linker_path,
                object_files: opts.obj_path,
                inputs: &opts.input,
                output: opts.output_path,
                args: opts.additional_args,
                link_crt: opts.target.needs_crt(),
                debug_info: opts.debug_info,
                verbose: opts.verbose,
                linker_script: opts.link_script,
                create_dynamic_library: opts.shared_obj,
                target: opts.target,
            })
            .map_err(|v| self.ctx.emit_diag(v.to_error()))
            .map_err(|()| ErrorEmitted)
    }
}

#[cfg(feature = "linking")]
pub struct LinkOpts<'a> {
    /// additional directories to searcher linkers in
    pub additional_linker_searchdir: &'a [PathBuf],
    /// additional arguments to pass to the linker
    pub additional_args: &'a [String],
    /// emit debug info
    pub debug_info: bool,
    /// make the linker verbose
    pub verbose: bool,
    /// link script
    pub link_script: Option<&'a Path>,
    /// emit a shared objectg
    pub shared_obj: bool,
    /// the target
    pub target: Target,
    /// the output path
    pub output_path: &'a Path,
    /// the object paths
    pub obj_path: &'a [PathBuf],
    /// additional linker input
    pub input: Vec<LinkerInput>,
}

#[cfg(all(not(feature = "mira-llvm-backend"), feature = "codegen"))]
compile_error!("you have to enable llvm20-1, llvm19-1 or llvm18-0 when codegen is enabled");
#[cfg(any(
    all(feature = "llvm20-1", feature = "llvm19-1", feature = "codegen"),
    all(feature = "llvm20-1", feature = "llvm18-0", feature = "codegen"),
    all(feature = "llvm19-1", feature = "llvm18-0", feature = "codegen"),
))]
compile_error!("only one of llvm20-1, llvm19-1 or llvm18-0 are allowed to be enabled");
