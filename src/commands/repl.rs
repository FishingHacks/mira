use crate::editor::{get_path, run_editor};
use crate::repl::Repl;
use std::{
    error::Error,
    io::{ErrorKind, Write},
    path::{Path, PathBuf},
    process::Command,
    str::FromStr,
    sync::Arc,
};

use crate::{libfinder, VER};
use clap::{Args, ValueEnum};
use mira::arena::Arena;
use mira::context::GlobalContext;
use mira::module_resolution::SingleModuleResolver;
use mira::{
    codegen::CodegenConfig,
    linking::{run_full_compilation_pipeline, FullCompilationOptions},
    module_resolution::{
        AbsoluteResolver, BasicModuleResolver, RelativeResolver, SingleFileModuleResolver,
    },
    target::{Target, NATIVE_TARGET},
};

use super::about::print_about;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum EditorMode {
    Cli,
    Editor,
}

#[derive(Debug, Args)]
pub struct ReplArgs {
    #[arg(value_enum, short = 'm', long = "mode")]
    /// The mode to start the repl in, editor causes your favorite cli-based editor to open.
    mode: Option<EditorMode>,
    /// Specify the file to load, if any
    file: Option<PathBuf>,
}

pub fn repl_main(args: ReplArgs) -> Result<(), Box<dyn Error>> {
    let Some(libmirastd) = libfinder::find_library("mirastd") else {
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
    let buf = args.file.map(std::fs::read_to_string).unwrap_or_else(|| {
        // pub fn main(argc: usize, argv: &&u8) {
        //     "Hello, World".println();
        // }
        Ok("pub fn main(argc: usize, argv: &&u8) {\n    \"Hello, World\".println();\n}".into())
    })?;

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
    assert!(VER.len() <= 7);
    print!("└─ [ mira v{VER} ]");
    for _ in 0..(7 - VER.len()) {
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

fn compile_run(rest: &str, repl: &mut Repl<Data>, run: bool) {
    let _ = std::fs::remove_file("/tmp/mira_executable");
    let _ = std::fs::remove_file("/tmp/mira_object.o");
    _compile_run(rest, repl, run);
    let _ = std::fs::remove_file("/tmp/mira_executable");
    let _ = std::fs::remove_file("/tmp/mira_object.o");
}

fn _compile_run(rest: &str, repl: &mut Repl<Data>, run: bool) {
    if !repl.data.libmirastd.exists() || !repl.data.libmirastd.is_dir() {
        println!("Error: Previously found library `mirastd` no longer exists");
        return;
    }

    let mut opts = parse_opts(rest);
    let mut llvm_ir_writer: Option<Box<dyn Write>> = None;
    let mut llvm_bc_writer: Option<Box<dyn Write>> = None;
    let mut asm_writer: Option<Box<dyn Write>> = None;
    let mut ir_writer: Option<Box<dyn Write>> = None;
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
                    let path = PathBuf::from(file);
                    match std::fs::File::options()
                        .write(true)
                        .read(false)
                        .create(true)
                        .truncate(true)
                        .open(&path)
                    {
                        Err(e) => println!("Failed to open {}: {e:?}", path.display()),
                        Ok(v) => llvm_ir_writer = Some(Box::new(v)),
                    }
                } else {
                    llvm_ir_writer = Some(Box::new(std::io::stdout()));
                }
            }
            "--llvm-bc" => {
                opts.remove(i);
                if opts.get(i).filter(|v| !v.starts_with('-')).is_some() {
                    let file = opts.remove(i);
                    let path = PathBuf::from(file);
                    match std::fs::File::options()
                        .write(true)
                        .read(false)
                        .create(true)
                        .truncate(true)
                        .open(&path)
                    {
                        Err(e) => println!("Failed to open {}: {e:?}", path.display()),
                        Ok(v) => llvm_bc_writer = Some(Box::new(v)),
                    }
                } else {
                    llvm_bc_writer = Some(Box::new(std::io::stdout()));
                }
            }
            "--asm" => {
                opts.remove(i);
                if opts.get(i).filter(|v| !v.starts_with('-')).is_some() {
                    let file = opts.remove(i);
                    let path = PathBuf::from(file);
                    match std::fs::File::options()
                        .write(true)
                        .read(false)
                        .create(true)
                        .truncate(true)
                        .open(&path)
                    {
                        Err(e) => println!("Failed to open {}: {e:?}", path.display()),
                        Ok(v) => asm_writer = Some(Box::new(v)),
                    }
                } else {
                    asm_writer = Some(Box::new(std::io::stdout()));
                }
            }
            "--ir" => {
                opts.remove(i);
                if opts.get(i).filter(|v| !v.starts_with('-')).is_some() {
                    let file = opts.remove(i);
                    let path = PathBuf::from(file);
                    match std::fs::File::options()
                        .write(true)
                        .read(false)
                        .create(true)
                        .truncate(true)
                        .open(&path)
                    {
                        Err(e) => println!("Failed to open {}: {e:?}", path.display()),
                        Ok(v) => ir_writer = Some(Box::new(v)),
                    }
                } else {
                    ir_writer = Some(Box::new(std::io::stdout()));
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

    let mut compilation_opts = FullCompilationOptions::new(
        repl.data.file.clone(),
        repl.data.current_dir.clone(),
        Box::new([
            Box::new(RelativeResolver),
            Box::new(AbsoluteResolver),
            Box::new(SingleModuleResolver(
                "std".into(),
                repl.data.libmirastd.clone(),
            )),
            Box::new(SingleFileModuleResolver::new(
                "\0__root",
                repl.data.file.clone(),
                None,
            )),
            Box::new(BasicModuleResolver(vec![
                ".mira/modules/$name",
                ".mira/modules/$name/src",
            ])),
        ]),
    );
    compilation_opts
        .with_source(Some(&repl.buf))
        .set_binary_path(exec_file.clone(), false)
        .set_object_path(obj_file)
        .set_additional_linker_args(&opts)
        .set_verbose_printing(verbose)
        .ir_writer(ir_writer)
        .llvm_ir_writer(llvm_ir_writer)
        .llvm_bc_writer(llvm_bc_writer)
        .asm_writer(asm_writer)
        .always_include_file(Some("std/prelude.mr".into()))
        .codegen_opts
        .optimizations_of(CodegenConfig::new_release_safe());

    if let Err(e) =
        run_full_compilation_pipeline(GlobalContext::new(&Arena::new()).share(), compilation_opts)
    {
        println!("Failed to compile:");
        for e in e.iter() {
            println!("{e}");
        }
        drop(e);
        return;
    }

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
