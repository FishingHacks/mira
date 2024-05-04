use std::{fs::read_to_string, io::Result};

use programming_lang::Lexer;

fn main() -> Result<()> {
    let source_code = read_to_string("./main.lang")?;

    let mut scanner = Lexer::new(&source_code, "main.lang");
    
    for token in scanner {
        println!("{token:?}")
    }

    Ok(())
}
