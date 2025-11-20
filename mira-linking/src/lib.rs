use std::{
    path::{Path, PathBuf},
    process::{Child, Command, ExitStatus},
};

use mira_macros::ErrorData;

use mira_target::{Abi, Os, Target};

#[derive(Debug)]
pub enum LinkerInput {
    LinkLibrary(String),
    LinkPath(PathBuf),
    LinkPathLookup(PathBuf),
}

pub struct LinkOptions<'a> {
    pub linker_path: PathBuf,
    pub object_files: &'a [PathBuf],
    pub inputs: &'a [LinkerInput],
    pub output: &'a Path,
    pub args: &'a [String],
    pub link_crt: bool,
    pub debug_info: bool,
    pub verbose: bool,
    pub linker_script: Option<&'a Path>,
    pub create_dynamic_library: bool,
    pub target: Target,
}

pub trait Linker: Send + Sync {
    fn can_link_crt(&self) -> bool;
    fn exists_in(&self, path: &Path) -> Option<PathBuf>;
    #[allow(clippy::result_large_err)]
    fn link(&self, opts: LinkOptions<'_>) -> Result<(), LinkerError>;
}

macro_rules! arg_if {
    ($command:ident $arg:expr, if $cond: expr) => {
        if $cond {
            $command.arg($arg);
        }
    };
}

#[derive(Debug, ErrorData)]
#[no_arena_lifetime]
pub enum LinkerError {
    #[error("Unable to find a valid linker")]
    UnableToLocateLinker,
    #[error("Unable to find a valid program loader for the operating sytem")]
    UnableToLocateLoader,
    #[error("failed to run command {_0:?}: {_1}")]
    SpawnFailed(Command, std::io::Error),
    #[error("command {_0:?} exited with a failure (exit code {_1:?})")]
    UnsuccessfulExit(Command, ExitStatus, Child),
}

struct LdLikeLinker(&'static str);
fn linux_find_loader(is_32_bit: bool) -> Option<PathBuf> {
    for f in std::fs::read_dir("/lib").ok()?.filter_map(Result::ok) {
        let filename = f.file_name();
        let Some(str) = filename.to_str() else {
            continue;
        };
        if is_32_bit && str.starts_with("lib/ld-linux.so.") {
            return Some(f.path());
        }
        if !is_32_bit && str.starts_with("ld-linux-x86-64.so.") {
            return Some(f.path());
        }
    }
    None
}

impl Linker for LdLikeLinker {
    fn can_link_crt(&self) -> bool {
        false
    }
    fn exists_in(&self, path: &Path) -> Option<PathBuf> {
        let bin_path = path.join(self.0);
        bin_path.exists().then_some(bin_path)
    }

    fn link(&self, opts: LinkOptions<'_>) -> Result<(), LinkerError> {
        assert!(!opts.link_crt);

        let mut command = Command::new(opts.linker_path);
        #[allow(clippy::single_match)]
        match opts.target.abi {
            Abi::Gnu => _ = command.args(["-lc", "-lm"]),
            _ => (),
        }
        #[allow(clippy::single_match)]
        match opts.target.os {
            Os::Linux => {
                let loader = linux_find_loader(opts.target.arch.is_32_bit())
                    .ok_or(LinkerError::UnableToLocateLoader)?;
                command.arg("-dynamic-linker").arg(loader);
            }
            _ => (),
        }

        arg_if!(command "-g", if opts.debug_info);
        arg_if!(command "-shared", if opts.create_dynamic_library);
        if let Some(script) = opts.linker_script.as_ref() {
            command.arg("--script").arg(script);
        }
        opts.object_files.iter().for_each(|v| _ = command.arg(v));
        command.arg("-o").arg(opts.output);
        for input in opts.inputs {
            match input {
                LinkerInput::LinkLibrary(lib) => _ = command.arg("-l").arg(lib),
                LinkerInput::LinkPath(path) => _ = command.arg(path),
                LinkerInput::LinkPathLookup(path) => _ = command.arg("-L").arg(path),
            }
        }
        command.args(opts.args);
        if opts.verbose {
            println!("[INFO] Running {command:?}");
        }
        let mut child = match command.spawn() {
            Ok(child) => child,
            Err(e) => return Err(LinkerError::SpawnFailed(command, e)),
        };
        match child.wait().expect("command wasnt ran") {
            c if !c.success() => Err(LinkerError::UnsuccessfulExit(command, c, child)),
            _ => Ok(()),
        }
    }
}

struct CcLikeLinker(&'static str);

impl Linker for CcLikeLinker {
    fn can_link_crt(&self) -> bool {
        true
    }
    fn exists_in(&self, path: &Path) -> Option<PathBuf> {
        let bin_path = path.join(self.0);
        bin_path.exists().then_some(bin_path)
    }

    fn link(&self, opts: LinkOptions<'_>) -> Result<(), LinkerError> {
        let mut command = Command::new(opts.linker_path);

        #[allow(clippy::single_match)]
        match opts.target.abi {
            Abi::Gnu => _ = command.args(["-lc", "-lm"]),
            _ => (),
        }

        arg_if!(command "-g", if opts.debug_info);
        arg_if!(command "-nostdlib", if !opts.link_crt);
        arg_if!(command "-shared", if opts.create_dynamic_library);
        if let Some(script) = opts.linker_script.as_ref() {
            command.arg("--script").arg(script);
        }
        opts.object_files.iter().for_each(|v| _ = command.arg(v));
        command.arg("-o").arg(opts.output);
        for input in opts.inputs {
            match input {
                LinkerInput::LinkLibrary(lib) => _ = command.arg("-l").arg(lib),
                LinkerInput::LinkPath(path) => _ = command.arg(path),
                LinkerInput::LinkPathLookup(path) => _ = command.arg("-L").arg(path),
            }
        }
        command.args(opts.args);
        if opts.verbose {
            println!("[INFO] Running {command:?}");
        }
        let mut child = match command.spawn() {
            Ok(child) => child,
            Err(e) => return Err(LinkerError::SpawnFailed(command, e)),
        };
        match child.wait().expect("command wasnt ran") {
            c if !c.success() => Err(LinkerError::UnsuccessfulExit(command, c, child)),
            _ => Ok(()),
        }
    }
}

static LINKERS: &[&dyn Linker] = &[
    &LdLikeLinker("ld"),
    &LdLikeLinker("ld.lld"),
    &CcLikeLinker("cc"),
    &CcLikeLinker("c++"),
    &CcLikeLinker("gcc"),
    &CcLikeLinker("g++"),
    &CcLikeLinker("clang"),
    &CcLikeLinker("clang++"),
];

pub fn search_for_linker(
    with_c_runtime: bool,
    additional_dirs: &[PathBuf],
) -> Option<(&'static dyn Linker, PathBuf)> {
    for dir in additional_dirs {
        for linker in LINKERS {
            if !linker.can_link_crt() && with_c_runtime {
                continue;
            }
            if let Some(path) = linker.exists_in(dir) {
                return Some((*linker, path));
            }
        }
    }
    if let Some(paths) = std::env::var_os("PATH") {
        for dir in std::env::split_paths(&paths) {
            for linker in LINKERS {
                if !linker.can_link_crt() && with_c_runtime {
                    continue;
                }
                if let Some(path) = linker.exists_in(&dir) {
                    return Some((*linker, path));
                }
            }
        }
    }
    None
}
