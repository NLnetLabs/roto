//! Loading Roto files
//!
//! How Roto files are loaded is implemented by types that give you varying
//! levels of control:
//!
//! - [`&Path`](`Path`) or [`&str`](`str`) is interpreted as the path to either
//!   a file or a directory representing a module.
//! - [`Source`] can be restricted to contain the path to a file, the path to a
//!   directory or just any path. It can also contain a script represented by
//!   a string in memory.
//! - [`SourceSpec`] is a collection of [`Source`]s.
//! - [`FileTree`] is a pre-loaded set of files. Everything in a [`FileTree`]
//!   is kept in memory and Roto will not do any filesystem operations while
//!   compiling.
//!
//! Any of these can be passed to [`Runtime::compile`].
//!
//! In many cases, you should just use a [`Path`]. However, you could, for
//! example, reach for a [`FileTree`] if you have a server that runs Roto code
//! that should run Roto code that is stored elsewhere. A client could then
//! load the files, serialize that and send it to the server, which then
//! deserializes it into a [`FileTree`] and compiles it directly without having
//! to store the files.
//!
//! A [`SourceSpec`] can be used to add modules to the script programmatically,
//! which will get compiled alongside the script.

use std::path::{Path, PathBuf};

use crate::{Package, RotoError, RotoReport, Runtime, runtime::OptCtx};

fn read_error(p: &Path, e: std::io::Error) -> RotoReport {
    RotoReport {
        errors: vec![RotoError::Read(p.to_string_lossy().into(), e)],
        ..Default::default()
    }
}

/// A trait that allows Roto files to be loaded.
pub trait Load {
    /// Load the source into a [`FileTree`].
    fn load(self) -> Result<FileTree, RotoReport>;
}

impl Load for &Path {
    fn load(self) -> Result<FileTree, RotoReport> {
        Source::Path {
            path: self.to_path_buf(),
        }
        .load()
    }
}

impl Load for &str {
    fn load(self) -> Result<FileTree, RotoReport> {
        Source::Path {
            path: PathBuf::from(self),
        }
        .load()
    }
}

impl Load for Source {
    fn load(self) -> Result<FileTree, RotoReport> {
        SourceSpec::from(self).load()
    }
}

impl Load for SourceSpec {
    fn load(self) -> Result<FileTree, RotoReport> {
        FileTree::from_spec(self)
    }
}

impl Load for FileTree {
    fn load(self) -> Result<FileTree, RotoReport> {
        Ok(self)
    }
}

/// A specification of which file to load for a Roto script.
pub struct SourceSpec {
    /// Source for this file.
    pub src: Source,

    /// Files representing submodules for this file.
    pub children: Vec<SourceSpec>,
}

/// A single source for a Roto script.
pub enum Source {
    /// A path that can either a file or a directory.
    Path {
        /// The path to load
        path: PathBuf,
    },

    /// A single Roto file.
    File {
        /// The path to load.
        path: PathBuf,
    },

    /// A directory containing a module structure or Roto files.
    Dir {
        /// The path to load.
        path: PathBuf,
    },

    /// An in-memory Roto file.
    Content {
        /// The (fake) path of the file that this source represents.
        ///
        /// This is used for diagnostics.
        path: PathBuf,

        /// The module name.
        module_name: String,

        /// The string containing Roto code.
        contents: String,

        /// Line offset that should be used when printing diagnostics.
        location_offset: usize,
    },
}

impl From<Source> for SourceSpec {
    fn from(value: Source) -> Self {
        SourceSpec {
            src: value,
            children: Vec::new(),
        }
    }
}

impl From<&str> for SourceSpec {
    fn from(value: &str) -> Self {
        Self::from(Source::Path { path: value.into() })
    }
}

impl From<&Path> for SourceSpec {
    fn from(value: &Path) -> Self {
        Self::from(Source::Path {
            path: value.to_path_buf(),
        })
    }
}

impl SourceSpec {
    /// Create a new [`SourceSpec`] from [`Source`] and some other child sources.
    ///
    /// If you don't have child sources, uses `SourceSpec::from` instead.
    pub fn new<S: Into<SourceSpec>>(
        src: Source,
        children: impl IntoIterator<Item = S>,
    ) -> Self {
        SourceSpec {
            src,
            children: children.into_iter().map(Into::into).collect(),
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
    pub fn read(path: &Path) -> Result<Self, RotoReport> {
        Self::read_internal(path).map_err(|e| read_error(path, e))
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
#[derive(Debug)]
pub struct FileTree {
    /// All files
    ///
    /// The root of the tree is the files at index 0
    pub files: Vec<SourceFile>,
}

impl FileTree {
    /// Read a [`FileTree`] based on a path.
    ///
    /// If the path refers to a file, only that file will be read. If the path
    /// instead refers to a directory, that directory will be read recursively.
    pub fn read(path: impl AsRef<Path>) -> Result<Self, RotoReport> {
        let path = path.as_ref();
        if path
            .metadata()
            .map_err(|e| read_error(path, e))?
            .file_type()
            .is_dir()
        {
            Self::directory(path)
        } else {
            Self::single_file(path)
        }
    }

    /// Read a single file script
    pub fn single_file(path: impl AsRef<Path>) -> Result<Self, RotoReport> {
        let mut file = SourceFile::read(path.as_ref())?;
        file.module_name = "pkg".into();
        Ok(FileTree { files: vec![file] })
    }

    /// Create a fake file for testing purposes.
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

    /// Create a [`FileTree`] from a [`SourceSpec`]
    fn from_spec(spec: SourceSpec) -> Result<FileTree, RotoReport> {
        fn add_source(
            tree: &mut FileTree,
            src: &Source,
            root: bool,
        ) -> Result<(), RotoReport> {
            match src {
                Source::Path { path } => {
                    if path
                        .metadata()
                        .map_err(|e| read_error(path, e))?
                        .file_type()
                        .is_dir()
                    {
                        let file_path = if root {
                            Path::new(path).join("pkg.roto")
                        } else {
                            Path::new(path).join("mod.roto")
                        };
                        let file = SourceFile::read(&file_path)?;
                        tree.files.push(file);
                        tree.find_files(tree.files.len() - 1, path)?;
                    } else {
                        let file = SourceFile::read(Path::new(path))?;
                        tree.files.push(file);
                    }
                }
                Source::File { path } => {
                    let file = SourceFile::read(path)?;
                    tree.files.push(file);
                }
                Source::Dir { path } => {
                    let file_path = if root {
                        Path::new(path).join("pkg.roto")
                    } else {
                        Path::new(path).join("mod.roto")
                    };
                    let file = SourceFile::read(&file_path)?;
                    tree.files.push(file);
                    tree.find_files(tree.files.len() - 1, path)?;
                }
                Source::Content {
                    path,
                    module_name,
                    contents: content,
                    location_offset,
                } => {
                    let file = SourceFile {
                        module_name: module_name.clone(),
                        location_offset: *location_offset,
                        name: path.display().to_string(),
                        contents: content.into(),
                        children: Vec::new(),
                    };
                    tree.files.push(file);
                }
            }
            Ok(())
        }

        fn recurse(
            tree: &mut FileTree,
            parent_id: usize,
            children: &[SourceSpec],
        ) -> Result<(), RotoReport> {
            for child in children {
                let id = tree.files.len();
                tree.files[parent_id].children.push(id);
                add_source(tree, &child.src, false)?;
                recurse(tree, id, &child.children)?;
            }

            Ok(())
        }

        let mut tree = FileTree { files: Vec::new() };
        add_source(&mut tree, &spec.src, true)?;
        recurse(&mut tree, 0, &spec.children)?;
        tree.files[0].module_name = "pkg".into();
        Ok(tree)
    }

    /// A Roto script defined by a directory
    pub fn directory(root: &Path) -> Result<FileTree, RotoReport> {
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
    ) -> Result<(), RotoReport> {
        for entry in
            std::fs::read_dir(path).map_err(|e| read_error(path, e))?
        {
            let entry = entry.map_err(|e| read_error(path, e))?;
            let path = entry.path();
            let file_type =
                entry.file_type().map_err(|e| read_error(&path, e))?;

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
                    read_error(&path, std::io::Error::other("invalid path"))
                })?
                .to_str()
                .ok_or_else(|| {
                    read_error(
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
    ) -> Result<(), RotoReport> {
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
