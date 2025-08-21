use std::{
    collections::{HashMap, HashSet},
    path::Path,
    sync::Arc,
};

#[derive(Clone, Debug)]
pub enum LibraryInput {
    Path,
    String(Arc<str>),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct LibraryId(pub(crate) usize);

#[derive(Default)]
pub struct LibraryTree {
    pub(crate) main: Option<LibraryId>,
    pub(crate) libraries: Vec<Library>,
    pub(crate) names: HashSet<Arc<str>>,
}

pub struct LibraryBuilder<'tree> {
    tree: &'tree mut LibraryTree,
    root: Arc<Path>,
    root_file_path: Arc<Path>,
    input: LibraryInput,
    dependencies: HashMap<Arc<str>, LibraryId>,
}

impl<'tree> LibraryBuilder<'tree> {
    #[must_use]
    pub fn with_source(mut self, source: Arc<str>) -> Self {
        self.input = LibraryInput::String(source);
        self
    }

    #[must_use]
    pub fn with_dependency(mut self, name: &str, id: LibraryId) -> Self {
        self.dependencies.insert(self.tree.intern(name), id);
        self
    }

    pub fn add_dependency(&mut self, name: &str, id: LibraryId) -> &mut Self {
        self.dependencies.insert(self.tree.intern(name), id);
        self
    }

    pub fn build(self) -> LibraryId {
        let id = LibraryId(self.tree.libraries.len());
        self.tree.libraries.push(Library {
            root: self.root,
            root_file_path: self.root_file_path,
            input: self.input,
            dependencies: self.dependencies,
        });
        id
    }
}

impl LibraryTree {
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn build_library(
        &mut self,
        module_root: Arc<Path>,
        file_path: Arc<Path>,
    ) -> LibraryBuilder {
        LibraryBuilder {
            tree: self,
            root: module_root,
            root_file_path: file_path,
            input: LibraryInput::Path,
            dependencies: HashMap::new(),
        }
    }

    /// the "main" library, aka the executable/library that's currently being compiled
    pub fn main_library(&mut self, library: LibraryId) {
        assert!(
            self.main.is_none(),
            "Only one main library can exist at a time"
        );
        self.main = Some(library);
    }

    fn intern(&mut self, s: &str) -> Arc<str> {
        if let Some(s) = self.names.get(s) {
            return s.clone();
        }
        let s: Arc<str> = s.into();
        self.names.insert(s.clone());
        s
    }
}

pub struct Library {
    pub root: Arc<Path>,
    pub root_file_path: Arc<Path>,
    pub input: LibraryInput,
    pub dependencies: HashMap<Arc<str>, LibraryId>,
}
