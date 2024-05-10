use std::{fs::read_to_string, io::Result};

use programming_lang::tokenizer::Tokenizer;

fn main() -> Result<()> {
    let source_code = read_to_string("./main.lang")?;

    let mut scanner = Tokenizer::new(&source_code, "main.lang");
    let tokens = scanner.scan_tokens().unwrap();

    for token in &tokens {
        println!("{token:?}");
    }

    Ok(())
}

