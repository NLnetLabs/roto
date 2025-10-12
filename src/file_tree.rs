use std::path::Path;

use crate::{runtime::OptionCtx, Package, RotoError, RotoReport, Runtime};

fn read_error(p: &Path, e: std::io::Error) -> RotoReport {
    RotoReport {
        errors: vec![RotoError::Read(p.to_string_lossy().into(), e)],
        ..Default::default()
    }
}

/// A filename with its contents
#[derive(Clone, Debug)]
pub struct SourceFile {
    pub name: String,
    pub module_name: String,
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
    pub fn compile<Ctx: OptionCtx>(
        self,
        rt: &Runtime<Ctx>,
    ) -> Result<Package, RotoReport> {
        let checked = self.parse()?.typecheck(rt)?;
        let pkg = checked.lower_to_mir().lower_to_lir().codegen();
        Ok(pkg)
    }
}

/// Directory structure that makes up a Roto script
///
/// This allows for a lot of control about the files loaded and how they
/// are structured. However, one would typically use [`FileTree::read`] which
/// uses Roto's standard file discovery procedure. A [`FileSpec`] can also
/// be used to create complex scripts programmatically from Rust, without
/// writing scripts to disk.
pub enum FileSpec {
    File(SourceFile),
    Directory(SourceFile, Vec<FileSpec>),
}

impl FileTree {
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

    pub fn single_file(path: impl AsRef<Path>) -> Result<Self, RotoReport> {
        let mut file = SourceFile::read(path.as_ref())?;
        file.module_name = "pkg".into();
        Ok(FileTree { files: vec![file] })
    }

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
