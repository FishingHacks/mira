use mira::target::NATIVE_TARGET;

use crate::{MIRAC_VERSION, VER};

pub fn print_about() {
    println!("--- Mirac {MIRAC_VERSION} ---");
    println!(" -> mira version: {VER}");
    let (major, minor, patch) = get_llvm_ver();
    println!(" -> llvm version: {major}.{minor}.{patch}");
    // TODO: change this to work with features when moving to a more independent LLVM backend.
    if major != 20 || minor != 1 {
        println!("  llvm version was expected to be 20.1, but it is not. this could lead to bugs during code generation.");
    }
    println!(" -> host: {NATIVE_TARGET}");
}

fn get_llvm_ver() -> (u32, u32, u32) {
    let mut major = 0u32;
    let mut minor = 0u32;
    let mut patch = 0u32;

    unsafe {
        llvm_sys::core::LLVMGetVersion(&mut major, &mut minor, &mut patch);
    }

    (major, minor, patch)
}
