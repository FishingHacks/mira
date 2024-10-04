use std::path::{Path, PathBuf};

use crate::{error::ProgrammingLangResolveError, tokenizer::Location};

pub fn resolve_module(
    name: &str,
    current_directory: &Path,
    root_directory: &Path,
    loc: &Location,
) -> Result<(PathBuf, Option<PathBuf>), ProgrammingLangResolveError> {
    let mut name_override = None;
    // file: the string of the rest of the import statement, excluding `./` or `<module_name>/`
    // if the file is "", it will be resolved to the `directory/<directory name>.lang`.
    // "." in `~/test/meow.lang` will be resolved to `~/test/test.lang`
    // "std" in `~/test/test.lang` will be resolved to `~/test/.lang/modules/std/std.lang`
    // the `@root` package will always resolve to the root directory.
    let (mut directory, file, root) = if name.starts_with("./") {
        (current_directory.to_path_buf(), &name[2..], None)
    } else if name == "." {
        (current_directory.to_path_buf(), "", None)
    } else if name.starts_with("@root/") {
        (root_directory.to_path_buf(), &name[6..], None)
    } else if name == "@root" {
        (root_directory.to_path_buf(), "", None)
    } else if name.starts_with('/') {
        return Err(ProgrammingLangResolveError::AbsolutePath(loc.clone()));
    } else {
        // module lookup
        let (module_name, file_path) = name.split_once('/').unwrap_or((name, ""));

        let mut file = root_directory.to_path_buf();

        file.push(".lang");
        file.push("modules");
        file.push(module_name);

        if !file.exists() {
            return Err(ProgrammingLangResolveError::ModuleNotFound(
                loc.clone(),
                module_name.to_string(),
            ));
        }

        name_override = Some(
            file.file_name()
                .expect("directory does not have a name")
                .to_os_string(),
        );

        (file.join("src"), file_path, Some(file))
    };

    if !directory.exists() {
        return Err(ProgrammingLangResolveError::FileNotFound(
            loc.clone(),
            directory,
        ));
    }

    directory.push(file);

    if directory.is_dir() {
        let name = if let Some(name) = name_override {
            name
        } else if let Some(name) = directory.file_name() {
            name.to_os_string()
        } else {
            return Err(ProgrammingLangResolveError::FileNotFound(
                loc.clone(),
                directory,
            ));
        };
        let mut name = name.to_os_string();
        name.push(".lang");
        directory.push(name);
        if directory.exists() {
            Ok((directory, root))
        } else {
            Err(ProgrammingLangResolveError::FileNotFound(
                loc.clone(),
                directory,
            ))
        }
    } else {
        if directory.exists() {
            return Ok((directory, root));
        }
        let Some(name) = directory.file_name() else {
            return Err(ProgrammingLangResolveError::FileNotFound(
                loc.clone(),
                directory,
            ));
        };
        let mut name = name.to_os_string();
        name.push(".lang");
        directory.set_file_name(name);
        if directory.exists() {
            Ok((directory, root))
        } else {
            Err(ProgrammingLangResolveError::FileNotFound(
                loc.clone(),
                directory,
            ))
        }
    }
}
