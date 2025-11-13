use super::compile::compile;
use crate::editor::{get_path, run_editor};
use crate::repl::Repl;
use crate::VERSION;

use std::str::FromStr;
use std::time::Instant;
use std::{
    error::Error,
    io::ErrorKind,
    path::{Path, PathBuf},
    process::Command,
    sync::Arc,
};

use mira_argparse::{EditorMode, ReplArgs};
use mira_driver::{
    find_library, ContextData, DiagEmitter, EmitMethod, LibraryTree, ProgressBarStyle,
};
use mira_errors::Diagnostics;
use mira_lexer::Lexer;
use mira_llvm_backend::CodegenConfig;
use mira_parser::module::ModuleId;
use mira_parser::Parser;
use mira_spans::{Arena, FileId, SourceFile};
use mira_target::{Target, NATIVE_TARGET};

use super::about::print_about;

pub(crate) fn repl_main(args: ReplArgs) -> Result<(), Box<dyn Error>> {
    let Some(libmirastd) = find_library("mirastd") else {
        println!("Failed to find mirastd");
        return Ok(());
    };
    let current_dir: Arc<Path> = std::env::current_dir()?.into();
    let file: Arc<Path> = current_dir.join("stdin_buffer").into();
    let editor_path = get_path();
    let editor_mode = match args.mode {
        Some(EditorMode::Cli) => false,
        Some(EditorMode::Editor) => true,
        None => editor_path.is_some(),
    };
    let buf = args
        .file
        .map(std::fs::read_to_string)
        .unwrap_or_else(|| Ok("pub fn main() {\n    \"Hello, World\".println();\n}".into()))?;

    let mut repl = Repl::<Data>::new(
        vec![
            ("mode", |args, r| match args.trim() {
                v @ ("cli" | "editor") => {
                    r.data.editor_mode = v == "editor";
                    println!("now in {v} mode");
                }
                v => println!("unknown mode {v}"),
            }),
            ("load", |rest, r| {
                let rest = rest.trim();
                let path = Path::new(rest);
                let read = match std::fs::read_to_string(path) {
                    Ok(v) => {
                        r.buf.push_str(&v);
                        v.len()
                    }
                    Err(e) if e.kind() == ErrorKind::NotFound => {
                        println!("Could not find file `{rest}`");
                        return;
                    }
                    Err(e) => return println!("Failed to read file: {e:?}"),
                };
                println!("read {read} b");
            }),
            ("write", |rest, r| {
                let rest = rest.trim();
                let path = Path::new(rest);
                match std::fs::write(path, r.buf.as_bytes()) {
                    Ok(_) => println!("Wrote {} bytes", r.buf.len()),
                    Err(e) => println!("Failed to write to {}: {e:?}", path.display()),
                }
            }),
            ("edit", |_, r| {
                if let Some(path) = &r.data.editor_path {
                    run_editor(&mut r.buf, path);
                } else {
                    println!("No editor found")
                }
            }),
            ("ast", |_, repl| display_ast(repl)),
            ("check", |_, repl| compile_run("", repl, false)),
            ("run", |args, repl| compile_run(args, repl, true)),
            ("build", |args, repl| compile_run(args, repl, false)),
            ("help", |_, repl| print_help(repl.data.editor_mode)),
            ("about", |_, _| print_about()),
            ("targets", |_, _| {
                println!("Targets:");
                for target in Target::targets() {
                    println!("{target}");
                }
                println!("------------------------------------------");
                println!("Native Target: {NATIVE_TARGET}");
            }),
        ],
        |_, r| {
            if r.data.editor_mode {
                if let Some(path) = &r.data.editor_path {
                    run_editor(&mut r.buf, path);
                } else {
                    println!("No editor found");
                }
            }
        },
        Data {
            current_dir,
            file,
            editor_path,
            editor_mode,
            std_main_file: libmirastd.join("lib.mr").into(),
            libmirastd: libmirastd.into(),
        },
        buf,
    );
    repl.run()
}

fn print_help(editor_mode: bool) {
    //┌─┐└┘│├┬┴┼┴┤
    if editor_mode {
        println!("┌─[ repl commands ]──┬────────────────────────────[ mode: editor ]─┐");
    } else {
        println!("┌─[ repl commands ]──┬───────────────────────────────[ mode: cli ]─┐");
    }
    // prints line as shown below:
    let _ = ("┌─[ repl commands ]──┬────────────────────────────[ mode: <mode> ]─┐",);
    println!("│ .^ <line> <code>   │ inserts code after the specified line       │");
    println!("│ .v <line> <code>   │ inserts code before the specified line      │");
    println!("│ .% <line> <code>   │ replaces the specified line with the code   │");
    println!("│ .[ <line> <code>   │ prepends the code to the specified line     │");
    println!("│ .] <line> <code>   │ appends the code to the specified line      │");
    println!("│ .esc <code>        │ appends the code to the buffer              │");
    println!("│ .dd <line>         │ deletes the specified line                  │");
    println!("│ .del <line>        │ deletes the specified line                  │");
    println!("│ .delete <line>     │ deletes the specified line                  │");
    println!("│ .gc <line>         │ toggles a comment on the specified line     │");
    println!("│ .comment <line>    │ toggles a comment on the specified line     │");
    println!("│ .mode cli          │ makes pressing enter not launch an editor   │");
    println!("│ .mode editor       │ makes pressing enter launch the editor      │");
    println!("│ .list [-l]         │ lists the code; -l: with line numbers       │");
    println!("│ .load <path>       │ loads the path into the buffer              │");
    println!("│ .write <path>      │ writes the buffer to the path               │");
    println!("│ .edit              │ launches an editor                          │");
    println!("│ .clear             │ clears the current buffer                   │");
    println!("│ .help              │ prints the help menu                        │");
    println!("│ .targets           │ print targets                               │");
    println!("│ .about             │ prints the about message                    │");
    println!("│ .exit              │ exits the repl                              │");
    println!("│ .check             │ typechecks the code                         │");
    println!("├────────────────────┼─────────────────────────────────────────────┤");
    println!("│ .run / .build      │ runs/builds the code                        │");
    println!("│ Run Options:       │ Other arguments are passed to the linker    │");
    println!("│ --llvm-ir [file]   │ emits llvm ir                               │");
    println!("│ --llvm-bc [file]   │ emits llvm bitcode                          │");
    println!("│ --asm [file]       │ emits the assembly                          │");
    println!("│ --ir [file]        │ [dev] emits the intermediate representation │");
    println!("│ --obj <file>       │ emits the object code                       │");
    println!("│ --exec <file>      │ emits the executable                        │");
    println!("│ --target <target>  │ set the target (<arch>-<os>[-<abi>])        │");
    println!("│ --verbose          │ Output what the compiler is doing           │");
    let _ = ("└─ [ mira vN.N.N ]───┴─────────────────────────────────────────────┘",);
    // prints line as shown above
    assert!(VERSION.len() <= 7);
    print!("└─ [ mira v{VERSION} ]");
    for _ in 0..(7 - VERSION.len()) {
        print!("─");
    }
    println!("─┴─────────────────────────────────────────────┘");
}

struct Data {
    current_dir: Arc<Path>,
    file: Arc<Path>,
    editor_path: Option<PathBuf>,
    editor_mode: bool,
    libmirastd: Arc<Path>,
    std_main_file: Arc<Path>,
}

fn parse_opts(args: &str) -> Vec<String> {
    let mut opts = Vec::new();
    let mut buf = String::new();
    let mut in_str = false;
    let mut escape = false;
    for c in args.chars() {
        if c.is_ascii_whitespace() && !in_str {
            if !buf.is_empty() {
                opts.push(buf);
            }
            buf = String::new();
        } else if escape {
            escape = false;
            buf.push(c);
        } else if in_str && c == '"' {
            opts.push(buf);
            buf = String::new();
            in_str = false;
        } else if c == '"' {
            in_str = true;
        } else if c == '\\' {
            escape = true;
        } else {
            buf.push(c);
        }
    }
    if !buf.is_empty() {
        opts.push(buf);
    }
    opts
}

fn display_ast(repl: &Repl<Data>) {
    let arena = Arena::new();
    let (ctx, data) = ContextData::new(&arena, Some(ProgressBarStyle::Normal), DiagEmitter::Stdout);
    let s_ctx = ctx.ctx();
    let ctx = data.to_context(ctx.ty_ctx());
    let source_file = Arc::new(SourceFile::new(
        FileId::ZERO,
        Path::new("stdin_buffer").into(),
        Path::new("/").into(),
        repl.buf.clone().into(),
    ));
    let mut lexer = Lexer::new(s_ctx, source_file.clone());
    if let Err(e) = lexer.scan_tokens() {
        for e in e {
            let _ = s_ctx.emit_diag(e.to_error());
        }
        return;
    }

    let tokens = {
        let mut diagnostics = Diagnostics::new();
        match mira_parser::expand_tokens(
            s_ctx,
            source_file.clone(),
            lexer.into_tokens(),
            &mut diagnostics,
        ) {
            Some(v) => v,
            None => {
                for e in diagnostics {
                    let _ = s_ctx.emit_diag(e);
                }
                return;
            }
        }
    };

    let mut parser = Parser::from_tokens(s_ctx, &tokens, source_file, ModuleId::ZERO);
    let (stmts, errs) = parser.parse_all();
    for err in errs {
        let _ = s_ctx.emit_diag(err.to_error());
    }
    for stmt in stmts {
        use std::fmt::Write;

        let mut s = String::new();
        writeln!(s, "{stmt}").unwrap();

        ctx.print_stdout(s);
    }
}

fn compile_run(rest: &str, repl: &mut Repl<Data>, run: bool) {
    drop(std::fs::remove_file("/tmp/mira_executable"));
    drop(std::fs::remove_file("/tmp/mira_object.o"));
    let now = Instant::now();
    _compile_run(rest, repl, run);
    println!("Compilation took {:?}", now.elapsed());
    drop(std::fs::remove_file("/tmp/mira_executable"));
    drop(std::fs::remove_file("/tmp/mira_object.o"));
}

fn _compile_run(rest: &str, repl: &mut Repl<Data>, run: bool) {
    if !repl.data.libmirastd.exists() || !repl.data.libmirastd.is_dir() {
        println!("Error: Previously found library `mirastd` no longer exists");
        return;
    }

    let mut opts = parse_opts(rest);
    let mut llvm_ir_writer = None;
    let mut llvm_bc_writer = None;
    let mut asm_writer = None;
    let mut ir_writer = None;
    let mut obj_file = None;
    let mut exec_file = None;
    let mut target = Target::from_name("x86_64-linux");
    let mut verbose = false;
    let mut i = 0;
    while i < opts.len() {
        match opts[i].as_str() {
            "--verbose" => {
                opts.remove(i);
                verbose = true;
            }
            "--llvm-ir" => {
                opts.remove(i);
                if opts.get(i).filter(|v| !v.starts_with('-')).is_some() {
                    let file = opts.remove(i);
                    llvm_ir_writer = Some(EmitMethod::file(PathBuf::from(file).into_boxed_path()));
                } else {
                    llvm_ir_writer = Some(EmitMethod::stdout());
                }
            }
            "--llvm-bc" => {
                opts.remove(i);
                if opts.get(i).filter(|v| !v.starts_with('-')).is_some() {
                    let file = opts.remove(i);
                    llvm_bc_writer = Some(EmitMethod::file(PathBuf::from(file).into_boxed_path()));
                } else {
                    llvm_bc_writer = Some(EmitMethod::stdout());
                }
            }
            "--asm" => {
                opts.remove(i);
                if opts.get(i).filter(|v| !v.starts_with('-')).is_some() {
                    let file = opts.remove(i);
                    asm_writer = Some(EmitMethod::file(PathBuf::from(file).into_boxed_path()));
                } else {
                    asm_writer = Some(EmitMethod::stdout());
                }
            }
            "--ir" => {
                opts.remove(i);
                if opts.get(i).filter(|v| !v.starts_with('-')).is_some() {
                    let file = opts.remove(i);
                    ir_writer = Some(EmitMethod::file(PathBuf::from(file).into_boxed_path()));
                } else {
                    ir_writer = Some(EmitMethod::stdout());
                }
            }
            "--obj" => {
                opts.remove(i);
                if opts.get(i).is_none() {
                    println!("`obj` option needs a file");
                    return;
                }
                let file = opts.remove(i);
                let path = PathBuf::from(file);
                obj_file = Some(path);
            }
            "--exec" => {
                opts.remove(i);
                if opts.get(i).is_none() {
                    println!("`obj` option needs a file");
                    return;
                }
                let file = opts.remove(i);
                let path = PathBuf::from(file);
                exec_file = Some(path);
            }
            "--target" => {
                opts.remove(i);
                if opts.get(i).is_none() {
                    println!("`target` needs a target");
                    return;
                }
                match Target::from_str(&opts.remove(i)) {
                    Ok(v) => target = v,
                    Err(e) => {
                        println!("Failed to parse target: {e}.");
                        println!("Reverting to previous target {target}");
                    }
                }
            }
            _ => i += 1,
        }
    }

    if run && exec_file.is_none() {
        exec_file = Some(Path::new("/tmp/mira_executable").to_path_buf());
    }
    if exec_file.is_some() && obj_file.is_none() {
        let path = Path::new("/tmp/mira_object.o");
        obj_file = Some(path.to_path_buf());
    }

    let mut libtree = LibraryTree::new();
    let libstd = libtree
        .build_library(
            repl.data.libmirastd.clone(),
            repl.data.std_main_file.clone(),
            "std",
        )
        .build();
    let mainlib = libtree
        .build_library(
            repl.data.current_dir.clone(),
            repl.data.file.clone(),
            "repl_buffer",
        )
        .with_source(repl.buf.clone().into())
        .with_dependency("std", libstd)
        .build();
    libtree.main_library(mainlib);

    let Ok(()) = compile(
        &libtree,
        CodegenConfig::new_debug(),
        llvm_ir_writer,
        llvm_bc_writer,
        asm_writer,
        ir_writer,
        obj_file,
        exec_file.as_deref(),
        &opts,
        verbose,
    ) else {
        return;
    };

    if !run {
        return;
    }

    let Some(exec_file) = exec_file else {
        unreachable!("run is specified, but exec_file is None")
    };
    let mut cmd = Command::new(&exec_file);
    if verbose {
        println!("[INFO] Running {cmd:?}");
        println!("----[ run output ]----");
    }
    match cmd.spawn().and_then(|mut v| v.wait()) {
        Err(e) => println!("-> failed to run the program: {e:?}"),
        Ok(v) if !v.success() => println!("\n\n  process exited with error: {v:?}"),
        Ok(_) => println!("\n"),
    }
}
