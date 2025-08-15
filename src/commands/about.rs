use mira::target::NATIVE_TARGET;

use crate::{MIRAC_VERSION, VER};

pub fn print_about() {
    println!("--- Mirac {MIRAC_VERSION} ---");
    println!(" -> mira version: {VER}");
    let (major, minor, patch) = mira_llvm_backend::llvm_version();
    println!(" -> llvm version: {major}.{minor}.{patch}");
    let (expected_major, expected_minor) = mira_llvm_backend::expected_llvm_version();
    if major != expected_major || minor != expected_minor {
        println!("  llvm version was expected to be {expected_major}.{expected_minor}, but it is not. this could lead to bugs during code generation.");
    }
    println!(" -> host: {NATIVE_TARGET}");
}
