use std::{
    fmt::Debug, fs::read_to_string, io::{stdin, stdout, Write}, path::{Path, PathBuf}, rc::Rc, time::Instant
};

use programming_lang::{
    error::ProgrammingLangError, module::Module, tokenizer::Tokenizer,
};

fn main() -> std::io::Result<()> {
    // if let Err(e) = run_file("./main.lang") {
    //     println!("Could not run file: {e:?}")
    // }
    // return Ok(());
    let file: Rc<Path> = PathBuf::from("<stdin>").into();
    let mut module = Module::new();

    loop {
        print!("> ");
        let _ = stdout().flush();
        let mut str = String::with_capacity(50);
        let Ok(_) = stdin().read_line(&mut str) else {
            continue;
        };

        if str == "\n" || str == "\r\n" {
            continue;
        }
        if str.trim() == ".exit" {
            break;
        }
        if str.trim() == ".help" {
            //┌─┐└┘│├┬┴┼┴┤
            // TODO: Better help menu
            println!("┌───────┬──────────────────────┐");
            println!("│ .help │ Prints the help menu │");
            println!("│ .exit │ Exits the repl       │");
            println!("└───────┴──────────────────────┘");

            continue;
        }

        let start = Instant::now();
        let mut tokenizer = Tokenizer::new(&str, file.clone());

        println!(
            "Creating tokenizer: {}μs",
            Instant::now().duration_since(start).as_micros()
        );
        let start = Instant::now();

        if let Err(errors) = tokenizer.scan_tokens() {
            println!("Errors occurred during tokenization:");
            for error in errors {
                println!("{error:?}");
            }
            continue;
        }

        println!(
            "Tokenization: {}μs",
            Instant::now().duration_since(start).as_micros()
        );

        print!("Tokens: [");
        for i in tokenizer.get_tokens().iter() {
            print!("{i}, ");
        }
        println!("]");

        let start = Instant::now();

        let mut parser = tokenizer.to_parser();
        let parsed = match parser.parse_all() {
            Ok(statements) => {
                for stmt in &statements {
                    println!("parsed: {stmt}");
                }
                statements
            }
            Err(errors) => {
                for error in &errors {
                    println!("{error:?}");
                }
                continue;
            }
        };

        println!(
            "Parsing: {}μs",
            Instant::now().duration_since(start).as_micros()
        );

        let start = Instant::now();

        match module.push_all(parsed) {
            Ok(_) => (),
            Err(errors) => {
                for err in &errors {
                    println!("{err:?}");
                }
                continue;
            }
        }

        println!(
            "Module Creation: {}μs",
            Instant::now().duration_since(start).as_micros()
        );
    }

    Ok(())
}

enum ProgrammingLangIoError {
    ProgrammingLangError(Vec<ProgrammingLangError>),
    Io(std::io::Error),
}

impl From<std::io::Error> for ProgrammingLangIoError {
    fn from(value: std::io::Error) -> Self {
        Self::Io(value)
    }
}

impl From<Vec<ProgrammingLangError>> for ProgrammingLangIoError {
    fn from(value: Vec<ProgrammingLangError>) -> Self {
        Self::ProgrammingLangError(value)
    }
}

impl Debug for ProgrammingLangIoError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Io(v) => Debug::fmt(v, f),
            Self::ProgrammingLangError(v) => Debug::fmt(v, f),
        }
    }
}

#[allow(dead_code)]
fn run_file<P: AsRef<Path>>(path: P) -> Result<(), ProgrammingLangIoError> {
    let path = path.as_ref();
    let source_code = read_to_string(path)?;

    let start = Instant::now();
    let mut tokenizer = Tokenizer::new(&source_code, path.into());

    println!(
        "Creating tokenizer: {}μs",
        Instant::now().duration_since(start).as_micros()
    );
    let start = Instant::now();

    if let Err(errors) = tokenizer.scan_tokens() {
        return Err(errors
            .into_iter()
            .map(|el| el.into())
            .collect::<Vec<_>>()
            .into());
    }

    println!(
        "Tokenization: {}μs",
        Instant::now().duration_since(start).as_micros()
    );
    let start = Instant::now();

    let mut parser = tokenizer.to_parser();
    let mut errors: Vec<ProgrammingLangError> = vec![];
    while parser.current < parser.tokens.len() - 1 {
        match parser.parse_statement(true) {
            Ok(v) => {
                println!("Parsed: {v}");
            }
            Err(e) => {
                errors.push(e.into());
                parser.bail();
            }
        }
    }
    println!(
        "Parsing: {}μs",
        Instant::now().duration_since(start).as_micros()
    );
    if errors.len() > 0 {
        Err(errors.into())
    } else {
        Ok(())
    }
}
