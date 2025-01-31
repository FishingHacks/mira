use std::{
    collections::HashSet,
    error::Error,
    io::ErrorKind,
    path::{Path, PathBuf},
    process::Command,
    sync::{Arc, LazyLock},
};
mod editor;
mod repl;
mod run;
use editor::{get_path, run_editor};
use repl::Repl;
use run::{compile, link, RunOptions};

use mira::{AUTHORS as MIRA_AUTHORS, VERSION as VER};

const MIRAC_VERSION: &str = env!("CARGO_PKG_VERSION");
const MIRAC_AUTHORS: &str = env!("CARGO_PKG_AUTHORS");
pub const AUTHORS: LazyLock<Vec<&'static str>> = LazyLock::new(|| {
    let hashset = MIRA_AUTHORS
        .split(':')
        .chain(MIRAC_AUTHORS.split(':'))
        .collect::<HashSet<_>>();
    hashset.into_iter().collect()
});

fn print_help() {
    //┌─┐└┘│├┬┴┼┴┤
    println!("┌─[ repl commands ]──┬─────────────────────────────────────────────┐");
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
    _ = "     └─ [ mira vN.N.N ]───┴─────────────────────────────────────────────┘";
    // prints line as shown above
    assert!(VER.len() <= 7);
    print!("└─ [ mira v{VER} ]");
    for _ in 0..(7 - VER.len()) {
        print!("─");
    }
    println!("─┴─────────────────────────────────────────────┘");
}

fn print_about() {
    println!("--- Mira Compiler ---");
    println!(" -> mirac version {MIRAC_VERSION}");
    println!(" -> mira version {VER}");
    println!(" -> Contributors:");
    for author in AUTHORS.iter() {
        println!("    -> {author}");
    }
}

struct Data {
    current_dir: Arc<Path>,
    file: Arc<Path>,
    editor_path: Option<PathBuf>,
    editor_mode: bool,
}

fn parse_opts<'a>(args: &'a str) -> Vec<String> {
    let mut opts = Vec::new();
    let mut buf = String::new();
    let mut in_str = false;
    let mut escape = false;
    for c in args.chars() {
        if c.is_ascii_whitespace() && !in_str {
            if buf.len() > 0 {
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
    if buf.len() > 0 {
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
    let mut opts = parse_opts(rest);
    let mut run_opts = RunOptions::default();
    let mut obj_file = None;
    let mut exec_file = None;
    let mut i = 0;
    while i < opts.len() {
        match opts[i].as_str() {
            "--llvm-ir" => {
                opts.remove(i);
                if let Some(_) = opts.get(i).filter(|v| !v.starts_with('-')) {
                    let file = opts.remove(i);
                    let path = PathBuf::from(file);
                    match std::fs::File::options()
                        .write(true)
                        .read(false)
                        .create(true)
                        .open(&path)
                    {
                        Err(e) => println!("Failed to open {}: {e:?}", path.display()),
                        Ok(v) => run_opts.llvm_ir = Some(Box::new(v)),
                    }
                } else {
                    run_opts.llvm_ir = Some(Box::new(std::io::stdout()));
                }
            }
            "--llvm-bc" => {
                opts.remove(i);
                if let Some(_) = opts.get(i).filter(|v| !v.starts_with('-')) {
                    let file = opts.remove(i);
                    let path = PathBuf::from(file);
                    match std::fs::File::options()
                        .write(true)
                        .read(false)
                        .create(true)
                        .open(&path)
                    {
                        Err(e) => println!("Failed to open {}: {e:?}", path.display()),
                        Ok(v) => run_opts.llvm_bc = Some(Box::new(v)),
                    }
                } else {
                    run_opts.llvm_bc = Some(Box::new(std::io::stdout()));
                }
            }
            "--asm" => {
                opts.remove(i);
                if let Some(_) = opts.get(i).filter(|v| !v.starts_with('-')) {
                    let file = opts.remove(i);
                    let path = PathBuf::from(file);
                    match std::fs::File::options()
                        .write(true)
                        .read(false)
                        .create(true)
                        .open(&path)
                    {
                        Err(e) => println!("Failed to open {}: {e:?}", path.display()),
                        Ok(v) => run_opts.asm = Some(Box::new(v)),
                    }
                } else {
                    run_opts.asm = Some(Box::new(std::io::stdout()));
                }
            }
            "--ir" => {
                opts.remove(i);
                if let Some(_) = opts.get(i).filter(|v| !v.starts_with('-')) {
                    let file = opts.remove(i);
                    let path = PathBuf::from(file);
                    match std::fs::File::options()
                        .write(true)
                        .read(false)
                        .create(true)
                        .open(&path)
                    {
                        Err(e) => println!("Failed to open {}: {e:?}", path.display()),
                        Ok(v) => run_opts.ir = Some(Box::new(v)),
                    }
                } else {
                    run_opts.ir = Some(Box::new(std::io::stdout()));
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
                match std::fs::File::options()
                    .write(true)
                    .read(false)
                    .create(true)
                    .open(&path)
                {
                    Err(e) => println!("Failed to open {}: {e:?}", path.display()),
                    Ok(v) => run_opts.obj = Some(Box::new(v)),
                }
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
            _ => i += 1,
        }
    }

    if run && exec_file.is_none() {
        exec_file = Some(Path::new("/tmp/mira_executable").to_path_buf());
    }
    if exec_file.is_some() && !obj_file.is_some() {
        let path = Path::new("/tmp/mira_object.o");
        match std::fs::File::options()
            .write(true)
            .read(false)
            .create(true)
            .open(&path)
        {
            Err(e) => return println!("Failed to open {}: {e:?}", path.display()),
            Ok(v) => run_opts.obj = Some(Box::new(v)),
        }
        obj_file = Some(path.to_path_buf());
    }
    if let Err(e) = compile(
        repl.data.file.clone(),
        repl.data.current_dir.clone(),
        &repl.buf,
        run_opts,
    ) {
        for e in e {
            println!("{e}");
        }
        return;
    }
    if !exec_file.is_some() {
        return;
    }

    if !link(
        obj_file.as_ref().unwrap(),
        exec_file.as_ref().unwrap(),
        &opts,
    ) {
        return;
    }
    if !run {
        return;
    }

    match Command::new(exec_file.as_ref().unwrap())
        .spawn()
        .and_then(|mut v| v.wait())
    {
        Err(e) => println!("-> failed to run the program: {e:?}"),
        Ok(v) if !v.success() => println!("\n\n  process exited with error: {:?}", v),
        Ok(_) => println!("\n"),
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let current_dir: Arc<Path> = std::env::current_dir()?.into();
    let file: Arc<Path> = current_dir.join("stdin_buffer").into();
    let mut buffer = String::new();
    let editor_path = get_path(None);
    let editor_mode = editor_path.is_some();
    if let Some(editor_path) = &editor_path {
        run_editor(&mut buffer, editor_path);
    }

    let mut repl = Repl::<Data>::new(
        vec![
            ("mode", |args, r| match args.trim() {
                v @ ("cli" | "editor") => {
                    r.data.editor_mode = v == "editor";
                    println!("now in {v} mode");
                }
                v @ _ => println!("unknown mode {v}"),
            }),
            ("load", |rest, r| {
                let rest = rest.trim();
                let path = Path::new(rest);
                match std::fs::read_to_string(path) {
                    Ok(v) => r.buf = v,
                    Err(e) if e.kind() == ErrorKind::NotFound => {
                        println!("Could not find file `{}`", rest);
                        return;
                    }
                    Err(e) => return println!("Failed to read file: {e:?}"),
                };
                println!("read {} b", r.buf.len());
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
            ("help", |_, _| print_help()),
            ("about", |_, _| print_about()),
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
        },
    );
    repl.run()
    /*
    loop {
        write!(stdout, "> ")?;
        stdout.flush()?;
        let mut input = String::new();
        stdin.read_line(&mut input)?;
        let input = input.trim_end();
        if input.len() < 1 {
            if editor_mode {
                if let Some(editor_path) = &editor_path {
                    run_editor(&mut buffer, editor_path);
                }
            }
            continue;
        }

        if input.starts_with(".") {
            let input = &input[1..];
            let (cmd, rest) = {
                let mut chars = input.chars();
                let first_char = chars.next();
                let second_char = chars.next();
                if matches!(first_char, Some('^' | 'v' | '=' | '%' | ']' | '['))
                    && matches!(second_char, Some('0'..='9'))
                {
                    input.split_at(1)
                } else {
                    input.split_once(' ').unwrap_or((input, ""))
                }
            };

            match cmd {
                "]" => {
                    let rest = rest.trim();
                    let (line, rest) = rest.split_once(' ').unwrap_or((rest, ""));
                    let line = line.trim();
                    let line: usize = match line.trim().parse() {
                        Ok(v) => v,
                        Err(e) => {
                            writeln!(stdout, "Could not parse {line:?} as number: {e:?}")?;
                            continue;
                        }
                    };

                    match get_line_start(line + 1, &buffer) {
                        Ok(idx) => buffer.insert_str(idx - 1, rest),
                        Err(lines) if lines == line + 1 => {
                            buffer.insert_str(buffer.len().saturating_sub(1), rest);
                        }
                        Err(lines) => writeln!(
                            stdout,
                            "line {line} doesn't exist (buffer has {lines} lines)"
                        )?,
                    }
                }
                "[" => {
                    let rest = rest.trim();
                    let (line, rest) = rest.split_once(' ').unwrap_or((rest, ""));
                    let line = line.trim();
                    let line: usize = match line.trim().parse() {
                        Ok(v) => v,
                        Err(e) => {
                            writeln!(stdout, "Could not parse {line:?} as number: {e:?}")?;
                            continue;
                        }
                    };

                    if line == 0 {
                        buffer.insert_str(0, rest);
                    } else {
                        match get_line_start(line, &buffer) {
                            Ok(idx) => buffer.insert_str(idx, rest),
                            Err(lines) => writeln!(
                                stdout,
                                "line {line} doesn't exist (buffer has {lines} lines)"
                            )?,
                        }
                    }
                }
                "%" => {
                    let rest = rest.trim();
                    let (line, rest) = rest.split_once(' ').unwrap_or((rest, ""));
                    let line = line.trim();
                    let line: usize = match line.trim().parse() {
                        Ok(v) => v,
                        Err(e) => {
                            writeln!(stdout, "Could not parse {line:?} as number: {e:?}")?;
                            continue;
                        }
                    };
                    let (start, end) = {
                        let mut start = usize::MAX;
                        let mut end = usize::MAX;
                        let mut lines = 0;
                        let num = line;
                        for (idx, c) in buffer.char_indices() {
                            if c == '\n' {
                                lines += 1;
                            }
                            if num == 0 && start == usize::MAX {
                                start = idx;
                            }
                            if c == '\n' && num == 0 {
                                end = idx;
                                break;
                            }
                        }
                        if start == usize::MAX {
                            writeln!(
                                stdout,
                                "line {line} doesn't exist (buffer has {lines} lines)"
                            )?;
                            continue;
                        } else {
                            if end == usize::MAX {
                                end = buffer.len() - 1;
                            }
                            (start, end)
                        }
                    };
                    buffer.replace_range(start..end, rest);
                }
                "v" => {
                    let rest = rest.trim();
                    let (line, rest) = rest.split_once(' ').unwrap_or((rest, ""));
                    let line = line.trim();
                    let line: usize = match line.trim().parse() {
                        Ok(v) => v,
                        Err(e) => {
                            writeln!(stdout, "Could not parse {line:?} as number: {e:?}")?;
                            continue;
                        }
                    };

                    if line == 0 {
                        buffer.insert_str(0, rest);
                        buffer.insert(rest.len(), '\n');
                    } else {
                        match get_line_start(line, &buffer) {
                            Ok(idx) => {
                                buffer.insert_str(idx, rest);
                                buffer.insert(idx + rest.len(), '\n');
                            }
                            Err(lines) => writeln!(
                                stdout,
                                "line {line} doesn't exist (buffer has {lines} lines)"
                            )?,
                        }
                    }
                }
                "^" => {
                    let rest = rest.trim();
                    let (line, rest) = rest.split_once(' ').unwrap_or((rest, ""));
                    let line = line.trim();
                    let line: usize = match line.trim().parse() {
                        Ok(v) => v,
                        Err(e) => {
                            writeln!(stdout, "Could not parse {line:?} as number: {e:?}")?;
                            continue;
                        }
                    };
                    match get_line_start(line + 1, &buffer) {
                        Ok(idx) => {
                            buffer.insert_str(idx, rest);
                            buffer.insert(idx + rest.len(), '\n');
                        }
                        Err(lines) if lines == line + 1 => {
                            buffer.push_str(rest);
                            buffer.push('\n');
                        }
                        Err(lines) => writeln!(
                            stdout,
                            "line {line} doesn't exist (buffer has {lines} lines)"
                        )?,
                    }
                }
                "dd" | "del" | "delete" => {
                    let num: usize = match rest.trim().parse() {
                        Ok(v) => v,
                        Err(e) => {
                            writeln!(stdout, "Could not parse {rest:?} as number: {e:?}")?;
                            continue;
                        }
                    };
                    let (start, end) = {
                        let mut start = usize::MAX;
                        let mut end = usize::MAX;
                        let mut lines = 0;
                        let mut line = num;
                        for (idx, c) in buffer.char_indices() {
                            if c == '\n' {
                                lines += 1;
                            }
                            if line == 0 && start == usize::MAX {
                                start = idx;
                            }
                            if c == '\n' && line == 0 {
                                end = idx;
                                break;
                            } else if c == '\n' {
                                line -= 1;
                            }
                        }
                        if start == usize::MAX {
                            writeln!(
                                stdout,
                                "line {num} doesn't exist (buffer has {lines} lines)"
                            )?;
                            continue;
                        } else {
                            if end == usize::MAX {
                                end = buffer.len() - 1;
                            }
                            (start, end)
                        }
                    };
                    buffer.replace_range(start..=end, "");
                }
                "gc" | "comment" => {
                    let num: usize = match rest.trim().parse() {
                        Ok(v) => v,
                        Err(e) => {
                            writeln!(stdout, "Could not parse {rest:?} as number: {e:?}")?;
                            continue;
                        }
                    };
                    let start = {
                        let mut start = usize::MAX;
                        let mut lines = 0;
                        let mut line = num;
                        for (idx, c) in buffer.char_indices() {
                            if c == '\n' {
                                lines += 1;
                            }
                            if line == 0 && start == usize::MAX {
                                start = idx;
                            }
                            if c == '\n' && line == 0 {
                                break;
                            } else if c == '\n' {
                                line -= 1;
                            }
                        }
                        if start == usize::MAX {
                            writeln!(
                                stdout,
                                "line {num} doesn't exist (buffer has {lines} lines)"
                            )?;
                            continue;
                        } else {
                            start
                        }
                    };
                    if buffer[start..].starts_with("//") {
                        buffer.remove(start);
                        buffer.remove(start);
                    } else {
                        buffer.insert(start, '/');
                        buffer.insert(start, '/');
                    }
                }
                "mode" => match rest {
                    "cli" | "edit" => {
                        editor_mode = rest == "edit";
                        println!("Now in {rest} mode");
                    }
                    _ => println!("Unknown mode {rest}"),
                },
                "edit" => match &editor_path {
                    Some(editor_path) => run_editor(&mut buffer, editor_path),
                    None => println!("No editor found"),
                },
                "esc" => {
                    buffer.push_str(rest);
                    buffer.push('\n');
                }
                "clear" => {
                    buffer.clear();
                    writeln!(stdout, "cleared buffer")?;
                }
                "list" => {
                    let rest = rest.trim();
                    let with_line_numbers = rest == "-l" || rest.starts_with("-l ");
                    for (idx, line) in buffer.lines().enumerate() {
                        if with_line_numbers {
                            write!(stdout, "{idx: <4} ")?;
                        }
                        writeln!(stdout, "{}", line)?;
                    }
                }
                "write" => {
                    let rest = rest.trim();
                    let path = Path::new(rest);
                    match std::fs::write(path, buffer.as_bytes()) {
                        Ok(_) => println!("Wrote {} bytes", buffer.len()),
                        Err(e) => println!("Failed to write to {}: {e:?}", path.display()),
                    }
                }
                "load" => {
                    buffer.clear();
                    let rest = rest.trim();
                    let path = Path::new(rest);
                    let mut file = match OpenOptions::new().read(true).open(path) {
                        Ok(v) => v,
                        Err(e) if e.kind() == ErrorKind::NotFound => {
                            writeln!(stdout, "Could not find file `{}`", rest)?;
                            continue;
                        }
                        Err(e) => return Err(e),
                    };
                    let bytes_read = file.read_to_string(&mut buffer)?;
                    assert_eq!(bytes_read, buffer.len());
                    drop(file);
                    writeln!(stdout, "read {} b", bytes_read)?;
                }
                "check" => {
                    if let Err(errs) = compile(
                        file.clone(),
                        current_dir.clone(),
                        &buffer,
                        RunOptions::default(),
                    ) {
                        for e in errs.into_iter() {
                            writeln!(stdout, "{e}")?;
                        }
                    }
                }
                "run" | "compile" => {
                    todo!();
                }
                "exit" => break Ok(()),
                "help" => print_repl_help(),
                "about" => print_about(),
                _ => {
                    writeln!(stdout, "Unknown command `{cmd}`.")?;
                    print_repl_help();
                }
            };

            continue;
        }

        buffer.push_str(input);
        buffer.push('\n');
    }*/
}
