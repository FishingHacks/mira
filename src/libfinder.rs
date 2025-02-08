//! Module for finding the directory that the standard library and others are in.
//! It searches for `lib/<library>` and `lib<library>` in the directory the exe is in and every
//! parent directory of it.
//! E.g., if the executable is in `/home/user/.mira/bin/mirac` and `mirastd` was requested, it searches:
//! - /home/user/.mira/bin/lib/mirastd
//! - /home/user/.mira/bin/libmirastd
//! - /home/user/.mira/lib/mirastd
//! - /home/user/.mira/libmirastd
//! - /home/user/lib/mirastd
//! - /home/user/libmirastd
//! - /home/lib/mirastd
//! - /home/libmirastd
//! - /lib/mirastd
//! - /libmirastd
use std::path::PathBuf;

pub fn find_library(lib: &str) -> Option<PathBuf> {
    let lib_lib = format!("lib{lib}");

    let Ok(mut current_exe) = std::env::current_exe() else {
        return None;
    };
    // pop the executable off
    current_exe.pop();

    // only look in the 10 immedate directories, this should be plenty
    let mut iterations = 10;
    while current_exe.as_os_str().len() > 1 {
        if iterations == 0 {
            eprintln!("[WARN]: Searching for library {lib} exceeded allowed cost");
            return None;
        }
        iterations -= 1;
        current_exe.push("lib");
        current_exe.push(lib);
        if current_exe.exists() && current_exe.is_dir() {
            return Some(current_exe);
        }
        current_exe.pop();
        current_exe.pop();
        current_exe.push(&lib_lib);
        if current_exe.exists() && current_exe.is_dir() {
            return Some(current_exe);
        }
        // pop 2 to get to the parent directory of the directory we're currently searching
        current_exe.pop();
        current_exe.pop();
    }
    None
}
