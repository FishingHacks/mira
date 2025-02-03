use std::{
    io::Write,
    path::{Path, PathBuf},
};

pub fn get_path(path: Option<&str>) -> Option<PathBuf> {
    let mut editor_path = path
        .map(PathBuf::from)
        .filter(|v| v.exists())
        .or_else(|| std::env::var_os("EDITOR").map(PathBuf::from))
        .filter(|v| v.exists());

    let paths = [
        // TODO: Add paths for macos and windows
        Path::new("/bin/nvim"),
        Path::new("/bin/vim"),
        Path::new("/bin/nano"),
        Path::new("/bin/hx"),
        Path::new("/usr/bin/nvim"),
        Path::new("/usr/bin/vim"),
        Path::new("/usr/bin/nano"),
        Path::new("/usr/bin/hx"),
    ];
    for path in paths {
        if editor_path.is_some() {
            break;
        }
        if path.exists() {
            editor_path = Some(path.into())
        }
    }
    editor_path
}

pub fn run_editor(buffer: &mut String, editor: &Path) {
    let tmp_file = format!(
        "/tmp/mira_repl_content_{}.mr",
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|v| v.as_secs())
            .unwrap_or_default(),
    );
    let tmp_file = PathBuf::from(tmp_file);
    println!("Using temporary file {tmp_file:?}");
    if let Err(e) = std::fs::write(&tmp_file, buffer.as_bytes()) {
        println!("Failed to write the temporary file: {e:?}");
        return;
    }
    let child = match std::process::Command::new(editor).arg(&tmp_file).spawn() {
        Ok(v) => v,
        Err(e) => {
            println!("Failed to spawn editor: {e:?}");
            let _ = std::fs::remove_file(&tmp_file);
            return;
        }
    };
    match child.wait_with_output() {
        Err(e) => {
            println!("Failed to spawn editor: {e:?}");
            let _ = std::fs::remove_file(&tmp_file);
            return;
        }
        Ok(v) => {
            if !v.status.success() {
                println!("Editor exited unsuccessfully: {v:?}");
                print!("Do you want to see the temporary file? [y/n] [default: y] ");
                std::io::stdout()
                    .flush()
                    .expect("flushing stdout shouldn't fail");
                let mut input = String::new();
                std::io::stdin()
                    .read_line(&mut input)
                    .expect("reading stdin shouldn't fail");
                let trimmed = input.trim();
                if trimmed != "n" && trimmed != "no" {
                    match std::fs::read(&tmp_file) {
                        Err(e) => println!("Failed to read tempfile: {e:?}"),
                        Ok(v) => std::io::stdout()
                            .write_all(&v)
                            .expect("writing to stdout shouldn't fail"),
                    }
                }
                input.clear();
                print!("Do you want to read the temporary file to the buffer and delete it? [y/n] [default: n] ");
                std::io::stdout()
                    .flush()
                    .expect("flushing stdout shouldn't fail");
                std::io::stdin()
                    .read_line(&mut input)
                    .expect("reading stdin shouldn't fail");
                let trimmed = input.trim();
                if trimmed != "y" && trimmed != "yes" {
                    println!("did not read the temporary file");
                    return;
                }
            }
            match std::fs::read_to_string(&tmp_file) {
                Err(e) => {
                    println!("Failed to read the temporary file. Not deleting.\nError: {e:?}");
                    return;
                }
                Ok(v) => {
                    *buffer = v;
                    let _ = std::fs::remove_file(&tmp_file);
                }
            }
        }
    }
}
