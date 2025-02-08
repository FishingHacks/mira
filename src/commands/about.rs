use crate::{AUTHORS, MIRAC_VERSION, VER};

pub fn print_about() {
    println!("--- Mira Compiler ---");
    println!(" -> mirac version {MIRAC_VERSION}",);
    println!(" -> mira version {VER}");
    println!(" -> Contributors:");
    for author in AUTHORS.iter() {
        println!("    -> {author}");
    }
}
