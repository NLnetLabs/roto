use std::{collections::HashMap, path::Path};

use crate::{
    RotoError, RotoReport, Runtime,
    deps::{self, DepGraph},
    parser::meta::Spans,
    runtime::OptCtx,
};

#[derive(Debug)]
pub struct ReadError {
    pub path: String,
    pub err: std::io::Error,
}

impl ReadError {
    fn new(path: &Path, err: std::io::Error) -> Self {
        Self {
            path: path.to_string_lossy().into(),
            err,
        }
    }
}

/// Something that can be loaded into a [`Package`].
///
/// Usually, this will be something like a filepath from which the package is
/// read.
pub trait Load {
    /// Load the [`Package`].
    fn load(self) -> Result<Package, ReadError>;
}

impl Load for &str {
    fn load(self) -> Result<Package, ReadError> {
        FileTree::read(self)?.load()
    }
}

impl Load for &Path {
    fn load(self) -> Result<Package, ReadError> {
        FileTree::read(self)?.load()
    }
}

impl Load for FileTree {
    fn load(self) -> Result<Package, ReadError> {
        Ok(Package::new(&self.files[0].module_name.clone(), self))
    }
}

#[derive(Clone)]
pub struct Package {
    pub(crate) name: String,
    pub(crate) files: FileTree,
    pub(crate) deps: Vec<String>,
}

impl Package {
    /// Create a new [`Package`].
    pub fn new(name: &str, files: FileTree) -> Self {
        Self {
            name: name.into(),
            files,
            deps: Vec::new(),
        }
    }

    /// Declare that this package requires a dependency
    pub fn add_dependency(&mut self, dep: &str) -> &mut Self {
        self.deps.push(dep.into());
        self
    }

    pub(crate) fn resolve<Ctx: OptCtx>(
        self,
        rt: &Runtime<Ctx>,
    ) -> Result<DepGraph, RotoReport> {
        if let Some(provider) = &rt.rt.dependency_provider {
            deps::resolve(provider, self)
        } else {
            if !self.deps.is_empty() {
                return Err(crate::RotoReport {
                    files: Vec::new(),
                    errors: vec![RotoError::DepsNotFound(self.deps)],
                    spans: Spans::default(),
                });
            }

            let main = self.name.into();
            let mut pkgs = HashMap::new();
            pkgs.insert(main, self.files);

            Ok(DepGraph {
                main,
                pkgs,
                deps: HashMap::new(),
            })
        }
    }
}

/// A filename with its contents
#[derive(Clone, Debug)]
pub struct SourceFile {
    /// The filename of the file.
    ///
    /// This should include the full path to the file, since this is used in diagnostics.
    pub name: String,

    /// Name of the module that this file represents.
    ///
    /// This usually matches the file name.
    pub module_name: String,

    /// Contents of the file.
    pub contents: String,

    /// The line offset that should be added to the location in error
    /// messages.
    ///
    /// This is used to add the offset of a string of source text in a test,
    /// so that Roto errors can refer to locations in Rust files accurately.
    pub location_offset: usize,

    /// Subfiles (only for `mod.roto` files)
    pub children: Vec<usize>,
}

impl SourceFile {
    /// Return the name of the file for diagnostics.
    pub fn name(&self) -> String {
        if self.location_offset > 0 {
            format!("{}@{}", self.name, self.location_offset)
        } else {
            self.name.clone()
        }
    }

    /// Read a [`Path`] into a [`SourceFile`].
    pub fn read(path: &Path) -> Result<Self, ReadError> {
        Self::read_internal(path).map_err(|e| ReadError::new(path, e))
    }

    fn read_internal(path: &Path) -> Result<Self, std::io::Error> {
        let file_name = path
            .file_name()
            .ok_or(std::io::Error::other("invalid path"))?;
        let module_name = if file_name == "mod.roto" {
            path.parent()
                .ok_or(std::io::Error::other("invalid path"))?
                .file_name()
                .ok_or(std::io::Error::other("invalid path"))?
        } else {
            path.file_stem()
                .ok_or(std::io::Error::other("invalid path"))?
        }
        .to_string_lossy()
        .to_string();

        let name = path.to_string_lossy().to_string();
        let contents = std::fs::read_to_string(path)?;
        Ok(Self {
            name,
            module_name,
            contents,
            location_offset: 0,
            children: Vec::new(),
        })
    }
}

/// A set of files loaded and ready to be parsed
#[derive(Clone, Debug)]
pub struct FileTree {
    /// All files
    ///
    /// The root of the tree is the files at index 0
    pub(crate) files: Vec<SourceFile>,
}

/// Directory structure that makes up a Roto script
///
/// This allows for a lot of control about the files loaded and how they
/// are structured. However, one would typically use [`FileTree::read`] which
/// uses Roto's standard file discovery procedure. A [`FileSpec`] can also
/// be used to create complex scripts programmatically from Rust, without
/// writing scripts to disk.
pub enum FileSpec {
    /// A single file; the leaf of a file tree.
    File(SourceFile),

    /// A directory with a `mod.roto` file and some child modules.
    Directory(SourceFile, Vec<FileSpec>),
}

impl FileTree {
    /// Read a [`FileTree`] based on a path.
    ///
    /// If the path refers to a file, only that file will be read. If the path
    /// instead refers to a directory, that directory will be read recursively.
    pub fn read(path: impl AsRef<Path>) -> Result<Self, ReadError> {
        let path = path.as_ref();
        if path.metadata().is_ok_and(|t| t.file_type().is_dir()) {
            Self::directory(path)
        } else {
            Self::single_file(path)
        }
    }

    /// Read a single file script
    pub fn single_file(path: impl AsRef<Path>) -> Result<Self, ReadError> {
        let mut path = path.as_ref().to_path_buf();
        if path.extension().is_none_or(|e| e != "roto") {
            path.add_extension("roto");
        }
        let file = SourceFile::read(&path)?;
        Ok(FileTree { files: vec![file] })
    }

    /// Crea a fake file for testing purposes.
    ///
    /// The location offset should refer to the file offset of the string that
    /// contains the contents. This ensures that proper diagnostics can be
    /// created for this test file.
    pub fn test_file(
        file: &str,
        source: &str,
        location_offset: usize,
    ) -> Self {
        FileTree {
            files: vec![SourceFile {
                module_name: "pkg".into(),
                location_offset,
                name: file.into(),
                contents: source.into(),
                children: Vec::new(),
            }],
        }
    }

    /// Read the files specified in a [`FileSpec`].
    ///
    /// No automatic discovery of files with be done.
    pub fn file_spec(file_spec: FileSpec) -> FileTree {
        fn inner(
            parent: usize,
            files: &mut Vec<SourceFile>,
            file_spec: FileSpec,
        ) {
            match file_spec {
                FileSpec::File(file) => {
                    let idx = files.len();
                    files.push(file);
                    files[parent].children.push(idx);
                }
                FileSpec::Directory(file, specs) => {
                    let idx = files.len();
                    files.push(file);
                    files[parent].children.push(idx);
                    for spec in specs {
                        inner(idx, files, spec)
                    }
                }
            }
        }

        let mut files = Vec::new();
        match file_spec {
            FileSpec::File(file) => files.push(file),
            FileSpec::Directory(file, specs) => {
                let idx = files.len();
                files.push(file);
                for spec in specs {
                    inner(idx, &mut files, spec)
                }
            }
        }
        Self { files }
    }

    /// A Roto script defined by a directory
    pub fn directory(root: &Path) -> Result<FileTree, ReadError> {
        let pkg_file = SourceFile::read(&root.join("pkg.roto"))?;
        assert_eq!(pkg_file.module_name, "pkg");
        let mut tree = Self {
            files: vec![pkg_file],
        };
        tree.find_files(0, root)?;
        Ok(tree)
    }

    fn find_files(
        &mut self,
        parent_id: usize,
        path: &Path,
    ) -> Result<(), ReadError> {
        for entry in
            std::fs::read_dir(path).map_err(|e| ReadError::new(path, e))?
        {
            let entry = entry.map_err(|e| ReadError::new(path, e))?;
            let path = entry.path();
            let file_type =
                entry.file_type().map_err(|e| ReadError::new(&path, e))?;

            if file_type.is_dir() {
                self.process_subdir(parent_id, &path)?;
                continue;
            }

            if path.extension().is_none_or(|ext| ext != "roto") {
                continue;
            }

            let ident = path
                .file_stem()
                .ok_or_else(|| {
                    ReadError::new(
                        &path,
                        std::io::Error::other("invalid path"),
                    )
                })?
                .to_str()
                .ok_or_else(|| {
                    ReadError::new(
                        &path,
                        std::io::Error::other(
                            "file name is not a valid Roto identifier",
                        ),
                    )
                })?;

            if ident == "pkg" || ident == "mod" {
                continue;
            }

            let file = SourceFile::read(&path)?;

            let idx = self.files.len();
            self.files.push(file);
            self.files[parent_id].children.push(idx);
        }

        Ok(())
    }

    fn process_subdir(
        &mut self,
        parent_id: usize,
        path: &Path,
    ) -> Result<(), ReadError> {
        let file_path = path.join("mod.roto");

        if !file_path.exists() {
            return Ok(());
        }

        let file = SourceFile::read(&file_path)?;

        let idx = self.files.len();
        self.files.push(file);
        self.files[parent_id].children.push(idx);

        self.find_files(idx, path)
    }
}
