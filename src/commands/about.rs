use mira_target::NATIVE_TARGET;

use crate::{COMMIT, COMMIT_DATE, COMMIT_SHORT, VERSION};

pub(crate) fn print_about() {
    print!("--- Mirac {VERSION} ");
    match (COMMIT_SHORT, COMMIT_DATE) {
        (None, None) => println!("---"),
        (None, Some(v)) | (Some(v), None) => println!("({v}) ---"),
        (Some(l), Some(r)) => println!("({l} {r}) ---"),
    }
    println!(" -> version: {VERSION}");
    let (major, minor, patch) = mira_llvm_backend::llvm_version();
    println!(" -> llvm version: {major}.{minor}.{patch}");
    let (expected_major, expected_minor) = mira_llvm_backend::expected_llvm_version();
    if major != expected_major || minor != expected_minor {
        println!("  llvm version was expected to be {expected_major}.{expected_minor}, but is not. this could lead to bugs during code generation.");
    }
    println!(" -> host: {NATIVE_TARGET}");
    if let Some(hash) = COMMIT {
        println!(" -> commit-hash: {hash}");
    }
}
