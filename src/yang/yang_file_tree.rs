use std::path::Path;

use crate::SourceFile;
use crate::{pipeline::RotoError, pipeline::RotoReport};

fn read_error(p: &Path, e: std::io::Error) -> RotoReport {
    RotoReport {
        errors: vec![RotoError::Read(p.to_string_lossy().into(), e)],
        ..Default::default()
    }
}

/// A set of files loaded and ready to be parsed
#[derive(Debug)]
pub struct YangFileTree {
    /// All files
    ///
    /// The root of the tree is the files at index 0
    pub files: Vec<SourceFile>,
}

// impl FileTree {
//     /// Compile the files in a [`FileTree`] and return the compiled [`Package`].
//     pub fn compile<Ctx: OptCtx>(
//         self,
//         rt: &Runtime<Ctx>,
//     ) -> Result<Package<Ctx>, RotoReport> {
//         let checked = self.parse()?.typecheck(rt)?;
//         let pkg = checked.lower_to_mir().lower_to_lir().codegen();
//         Ok(pkg)
//     }
// }

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

impl YangFileTree {
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
        Ok(YangFileTree { files: vec![file] })
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
        YangFileTree {
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
    pub fn file_spec(file_spec: FileSpec) -> YangFileTree {
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
    pub fn directory(root: &Path) -> Result<YangFileTree, RotoReport> {
        let pkg_file = SourceFile::read(&root.join("rotonda-main.yang"))?;
        // assert_eq!(pkg_file.module_name, "rotonda");
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

            if path.extension().is_none_or(|ext| ext != "y") {
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
