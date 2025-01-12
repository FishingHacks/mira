use parking_lot::RwLock;
use std::{
    fs::OpenOptions,
    io::{stdin, stdout, ErrorKind, Read, Write},
    path::{Path, PathBuf},
    sync::Arc,
};

use programming_lang::{
    codegen::{
        mangling::{mangle_external_function, mangle_function},
        CodegenContext, InkwellContext, TargetTriple,
    },
    error::ProgrammingLangError,
    module::{Module, ModuleContext},
    parser::ParserQueueEntry,
    tokenizer::Tokenizer,
    typechecking::{
        typechecking::{typecheck_function, typecheck_static},
        TypecheckingContext,
    },
};

fn print_help() {
    //┌─┐└┘│├┬┴┼┴┤
    println!("┌─[ commands ]─────┬───────────────────────────────────────────┐");
    println!("│ .^ <line> <code> │ inserts code after the specified line     │");
    println!("│ .v <line> <code> │ inserts code before the specified line    │");
    println!("│ .% <line> <code> │ replaces the specified line with the code │");
    println!("│ .[ <line> <code> │ prepends the code to the specified line   │");
    println!("│ .] <line> <code> │ appends the code to the specified line    │");
    println!("│ .esc <code>      │ appends the code to the buffer            │");
    println!("│ .dd <line>       │ deletes the specified line                │");
    println!("│ .del <line>      │ deletes the specified line                │");
    println!("│ .delete <line>   │ deletes the specified line                │");
    println!("│ .gc <line>       │ toggles a comment on the specified line   │");
    println!("│ .comment <line>  │ toggles a comment on the specified line   │");
    println!("│ .clear           │ clears the current buffer                 │");
    println!("│ .list [-l]       │ lists the code; -l: with line numbers     │");
    println!("│ .load <path>     │ loads the path into the buffer            │");
    println!("│ .run             │ runs the code                             │");
    println!("│ .help            │ prints the help menu                      │");
    println!("│ .exit            │ exits the repl                            │");
    println!("└──────────────────┴───────────────────────────────────────────┘");
}

/// Returns the start index of the line or the number of lines
fn get_line_start(mut line: usize, buffer: &String) -> Result<usize, usize> {
    let mut lines = 0;
    for (idx, c) in buffer.char_indices() {
        if line == 0 {
            return Ok(idx);
        }
        if c == '\n' {
            line -= 1;
            lines += 1;
        }
    }
    Err(lines)
}

fn main() -> std::io::Result<()> {
    let current_dir: Arc<Path> = std::env::current_dir()?.into();
    let file: Arc<Path> = current_dir.join("stdin_buffer").into();
    let mut buffer = String::new();
    let mut stdout = stdout();
    let stdin = stdin();

    loop {
        write!(stdout, "> ")?;
        stdout.flush()?;
        let mut input = String::with_capacity(50);
        stdin.read_line(&mut input)?;
        let input = input.trim_end();
        if input.len() < 1 {
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
                        let mut num = line;
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
                            } else if c == '\n' {
                                num -= 1;
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
                "load" => {
                    buffer.clear();
                    let rest = rest.trim();
                    let path = PathBuf::from(rest);
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
                "run" => {
                    if let Err(errs) = run(file.clone(), current_dir.clone(), &buffer) {
                        writeln!(stdout, "-------------------------------------")?;
                        for e in errs.into_iter() {
                            writeln!(stdout, "{e}")?;
                        }
                    }
                }
                "exit" => break Ok(()),
                "help" => print_help(),
                _ => {
                    writeln!(stdout, "Unknown command `{cmd}`.")?;
                    print_help();
                }
            };

            continue;
        }

        buffer.push_str(input);
        buffer.push('\n');
    }
}

fn run(
    file: impl Into<Arc<Path>>,
    root_directory: impl Into<Arc<Path>>,
    source: impl AsRef<str>,
) -> Result<(), Vec<ProgrammingLangError>> {
    let file: Arc<Path> = file.into();
    let filename = file
        .file_name()
        .expect("file needs a filename")
        .to_string_lossy()
        .into_owned();
    let context = parse_all(file, root_directory.into(), source.as_ref())?;

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

    for i in 0..num_functions {
        errs.extend(typecheck_function(
            &typechecking_context,
            &context,
            i,
            false,
        ));
    }

    for i in 0..num_ext_functions {
        errs.extend(typecheck_function(&typechecking_context, &context, i, true));
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

    println!(
        "{:#}",
        programming_lang::typechecking::ir_displayer::TypecheckingContextDisplay(
            &*typechecking_context
        )
    );

    let num_fns = { typechecking_context.functions.read().len() };
    let num_ext_fns = { typechecking_context.external_functions.read().len() };
    for i in 0..num_fns {
        println!(
            "mangle_name(fn({i})) = {:?}",
            mangle_function(&typechecking_context, i)
        );
    }
    for i in 0..num_ext_fns {
        println!(
            "mangle_name(ext_fn({i})) = {:?}",
            mangle_external_function(&typechecking_context, i)
        );
    }
    println!("----------------------------");
    println!("Compiling...");
    let context = InkwellContext::create();
    let codegen_context = CodegenContext::make_context(
        &context,
        TargetTriple::create("x86_64-unknown-linux-gnu"),
        &typechecking_context,
        &filename,
    )
    .expect("failed to create the llvm context");
    println!("{}", codegen_context.module.to_string());
    codegen_context
        .write_object_file(Path::new("out.o"))
        .unwrap();

    Ok(())
}

fn parse_all(
    file: Arc<Path>,
    root_directory: Arc<Path>,
    source: &str,
) -> Result<Arc<ModuleContext>, Vec<ProgrammingLangError>> {
    let mut errors = vec![];

    let mut tokenizer = Tokenizer::new(source.as_ref(), file.clone());
    if let Err(errs) = tokenizer.scan_tokens() {
        errors.extend(errs.into_iter().map(ProgrammingLangError::Tokenization));
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
                .map(ProgrammingLangError::Parsing),
        );
        let (path, root) = {
            let module = &modules.read()[module_context.modules.read().len()];
            (module.file.clone(), module.root.clone())
        };
        let mut module = Module::new(module_context.clone(), current_parser.imports, path, root);
        if let Err(errs) = module.push_all(statements, module_context.modules.read().len()) {
            errors.extend(errs.into_iter().map(ProgrammingLangError::ProgramForming));
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
                errors.extend(errs.into_iter().map(ProgrammingLangError::Tokenization));
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
