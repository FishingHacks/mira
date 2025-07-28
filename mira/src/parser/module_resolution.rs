use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use mira_spans::{ImportData, ModuleResolver, ResolvedPath, SourceMap, Span};

// ./_ -> relative path
// /_  -> absolute path
// _/_  -> module/package import
// (potentially) @name -> builtin import

pub fn resolve_module(
    name: &str,
    current_dir: &Path,
    root_dir: Arc<Path>,
    span: Span,
    source_map: &SourceMap,
) -> Option<ResolvedPath> {
    if name.is_empty() {
        return None;
    }

    if name.as_bytes()[0] == b'.' && name[1..].starts_with(std::path::MAIN_SEPARATOR_STR) {
        let skip = std::path::MAIN_SEPARATOR_STR.len() + 1;
        let name = &name[skip..];
        let data = ImportData {
            root_dir,
            current_dir,
            import: name,
            span,
            source_map,
        };
        for resolver in source_map.module_resolvers() {
            if let v @ Some(_) = resolver.resolve_relative(data.clone()) {
                return v;
            }
        }
    } else if name.starts_with(std::path::MAIN_SEPARATOR_STR) {
        let data = ImportData {
            root_dir,
            current_dir,
            import: name,
            span,
            source_map,
        };
        for resolver in source_map.module_resolvers() {
            if let v @ Some(_) = resolver.resolve_absolute(data.clone()) {
                return v;
            }
        }
    } else {
        let (module, import) = name
            .split_once(std::path::MAIN_SEPARATOR_STR)
            .unwrap_or((name, ""));

        let data = ImportData {
            root_dir,
            current_dir,
            import,
            span,
            source_map,
        };
        for resolver in source_map.module_resolvers() {
            if let v @ Some(_) = resolver.resolve_module(data.clone(), module) {
                return v;
            }
        }
    }
    None
}

/// Does the following lookups ($name refers to the last part of the path):
/// - if $name ends with .mr, returns Some if the path exists and is not a directory, otherwise
///   returns None
/// - if $name does not exist or is not a directory and $name.mr exists and is not a directory,
///   return $name.mr (e.g. `hello.mr` for `hello`)
/// - if $name does not exist or is not a directory, return None
/// - if $name/$name.mr exists and is not a directory, return it
/// - if $name/lib.mr exists and is not a directory, return it
/// - otherwise, return none
///
///
/// $path = dir.join(import)
/// $name = $path.file_name()
///
/// $name ends_with ".mr"
/// ├─ true:
/// │  $path exists and is a file
/// │  ├─ true: return Some($path)
/// │  └─ false: return None
/// └─ false:
/// .  $path exists and $path is a directory
/// .  ├─ true:
/// .  │  $path/$name.mr exists and is a file
/// .  │  └─ true: return Some($path/$name.mr)
/// .  │  $path/lib.mr exists and is a file
/// .  │  ├─ true: return Some($path/lib.mr)
/// .  │  └─ false: return None
/// .  └─ false:
/// .  .  $path.mr exists and is a file
/// .  .  ├─ true: return Some($path.mr)
/// .  .  └─ false: return None
pub fn default_lookup_import_in_directory(
    import: &str,
    dir: &Path,
    source_map: &SourceMap,
) -> Option<PathBuf> {
    let mut path = dir.join(import);
    if import.ends_with(".mr") {
        return (source_map.exists(&path) && !source_map.is_dir(&path)).then_some(path);
    }
    if source_map.exists(&path) && source_map.is_dir(&path) {
        // $name/$name.mr and $name/lib.mr
        let mut name = path.file_name()?.to_os_string();
        name.push(".mr");
        path.push(&name);
        if source_map.exists(&path) && !source_map.is_dir(&path) {
            return Some(path);
        }
        path.pop();
        path.push("lib.mr");
        (source_map.exists(&path) && !source_map.is_dir(&path)).then_some(path)
    // $name will turn into $name.mr if $name does not end with .mr or $name does not exist
    } else {
        let mut name = path.file_name()?.to_os_string();
        name.push(".mr");
        path.pop();
        path.push(&name);
        (source_map.exists(&path) && !source_map.is_dir(&path)).then_some(path)
    }
}

pub struct RelativeResolver;
impl ModuleResolver for RelativeResolver {
    fn resolve_relative(&self, data: ImportData) -> Option<ResolvedPath> {
        let file =
            default_lookup_import_in_directory(data.import, data.current_dir, data.source_map)?;
        let file = file.into();
        let root_dir = data.root_dir;
        Some(ResolvedPath { file, root_dir })
    }
}

pub struct AbsoluteResolver;
impl ModuleResolver for AbsoluteResolver {
    fn resolve_absolute(&self, data: ImportData) -> Option<ResolvedPath> {
        let file = default_lookup_import_in_directory(
            data.import.split(std::path::MAIN_SEPARATOR_STR).last()?,
            Path::new(data.import).parent()?,
            data.source_map,
        )?;
        let root_dir = file.parent()?.to_path_buf().into();
        let file = file.into();
        Some(ResolvedPath { file, root_dir })
    }
}

pub struct SingleModuleResolver(pub String, pub Arc<Path>);
impl ModuleResolver for SingleModuleResolver {
    fn resolve_module(&self, data: ImportData, module_name: &str) -> Option<ResolvedPath> {
        if module_name != self.0 {
            return None;
        }
        let file =
            default_lookup_import_in_directory(data.import, &self.1, data.source_map)?.into();
        let root_dir = self.1.clone();
        Some(ResolvedPath { root_dir, file })
    }
}

/// NOTE: The root modules is *always* the path the first index into the vector resolves to.
/// E.g.: For search paths of ["modules/$name", "modules/$name/src", "modules/$name/lib"], the root
/// directory is always "modules/$name".
pub struct BasicModuleResolver(pub Vec<&'static str>);
impl ModuleResolver for BasicModuleResolver {
    fn resolve_module(&self, data: ImportData, module_name: &str) -> Option<ResolvedPath> {
        if self.0.is_empty() {
            return None;
        }
        let root_dir = PathBuf::from(self.0[0].replace("$name", module_name)).into();
        for module in self.0.iter() {
            let path = PathBuf::from(module.replace("$name", module_name));
            if !data.source_map.exists(&path) || !data.source_map.is_dir(&path) {
                continue;
            }
            let Some(file) =
                default_lookup_import_in_directory(data.import, &path, data.source_map)
            else {
                continue;
            };
            return Some(ResolvedPath {
                root_dir,
                file: file.into(),
            });
        }
        None
    }
}

pub struct SingleFileModuleResolver {
    pub name: String,
    pub file: Arc<Path>,
    pub root_dir: Arc<Path>,
}
impl ModuleResolver for SingleFileModuleResolver {
    fn resolve_module(&self, data: ImportData, module_name: &str) -> Option<ResolvedPath> {
        if !data.import.is_empty() || module_name != self.name {
            return None;
        }
        Some(ResolvedPath {
            root_dir: self.root_dir.clone(),
            file: self.file.clone(),
        })
    }
}

impl SingleFileModuleResolver {
    pub fn new(name: &str, file: Arc<Path>, root_dir: Option<Arc<Path>>) -> Self {
        let root_dir = root_dir.unwrap_or_else(|| {
            file.parent()
                .map(Into::into)
                .unwrap_or(Path::new("").into())
        });
        let name = name.to_string();
        Self {
            name,
            file,
            root_dir,
        }
    }
}
