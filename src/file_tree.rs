use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use indexmap::IndexMap;
use libc::CN_DST_IDX;
use unicode_ident::is_xid_start;

use crate::{
    Package, RotoError, RotoReport, Runtime,
    ast::Identifier,
    ice,
    module::Module,
    parser::{ParseError, meta::Span},
    runtime::OptCtx,
    typechecker::scope::YangModuleDefinition,
    yang::types::YangNameSpace,
};

pub(crate) fn read_error(p: PathBuf, e: std::io::Error) -> RotoReport {
    RotoReport {
        errors: vec![RotoError::Read(p.to_string_lossy().into(), e)],
        ..Default::default()
    }
}

pub(crate) fn custom_error(span: Span, e: std::io::Error) -> RotoReport {
    RotoReport {
        errors: vec![RotoError::Parse(ParseError::custom(e, "label", span))],
        ..Default::default()
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
        Self::read_internal(path)
            .map_err(|e| read_error(path.to_path_buf(), e))
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
    /// Compile the files in a [`FileTree`] and return the compiled [`Package`].
    pub fn compile<Ctx: OptCtx>(
        self,
        rt: &Runtime<Ctx>,
    ) -> Result<Package<Ctx>, RotoReport> {
        let checked = self.parse()?.typecheck(rt)?;
        let pkg = checked.lower_to_mir().lower_to_lir().codegen();
        Ok(pkg)
    }
}

#[derive(Debug, Clone)]
pub enum YangModuleSpec {
    Parsed(SourceFile),
    // pathbuf, revision
    Candidate((PathBuf, Option<String>)),
}

impl YangModuleSpec {
    fn source_file(&self) -> Option<&SourceFile> {
        if let YangModuleSpec::Parsed(f) = self {
            return Some(f);
        }
        None
    }

    fn path_buf(&self) -> Option<&PathBuf> {
        if let YangModuleSpec::Candidate(pb) = self {
            return Some(&pb.0);
        }
        None
    }
}

impl std::fmt::Display for YangModuleSpec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            YangModuleSpec::Parsed(source_file) => {
                write!(f, "{} (parsed)", source_file.module_name)
            }
            YangModuleSpec::Candidate(c) => {
                write!(
                    f,
                    "{}@{} (indexed)",
                    c.0.display(),
                    c.1.clone().unwrap_or("<NO_REVISION>".to_string())
                )
            }
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct YangFiles {
    // pub(crate) entry_point: SourceFile,
    pub(crate) lib: IndexMap<Identifier, YangModuleSpec>,
}

impl YangFiles {
    /// Read all yang files in a directory, while recursing into subdirs.
    pub fn create_yang_lib(
        lib_path: &Path,
        entry_file_name: &str,
        default_ext: &str,
    ) -> Result<Self, RotoReport> {
        println!("[yang_module_files] in {}", lib_path.display());
        println!("[yang_module_files] {}", entry_file_name);

        let mut lib = IndexMap::<Identifier, YangModuleSpec>::new();
        let root_dir = std::fs::read_dir(lib_path)
            .map_err(|e| read_error(lib_path.to_path_buf(), e))?;

        for (i, f) in root_dir.enumerate() {
            let f = f.map_err(|e| read_error(lib_path.to_path_buf(), e))?;
            if f.path()
                .extension()
                .map(|ext| ext == default_ext)
                .unwrap_or(false)
            {
                let Some((name, rev)) = f
                    .file_name()
                    .to_str()
                    .map(|n| n.split('@'))
                    .and_then(|mut n| {
                        n.next().and_then(|name| {
                            Self::as_ident(name).map(|name| {
                                n.next()
                                    .map(|rev| (name, Self::extract_rev(rev)))
                                    .or(Some((name, Ok(None))))
                            })
                        })
                    })
                    .flatten()
                else {
                    return Err(read_error(
                        lib_path.to_path_buf(),
                        std::io::Error::other(format!(
                            "File `{}` cannot be turned into a valid \
                                 module name",
                            f.path().display()
                        )),
                    ));
                };

                let Ok(rev) = rev else {
                    return Err(read_error(
                        lib_path.to_path_buf(),
                        std::io::Error::other(rev.unwrap_err()),
                    ));
                };

                lib.insert(name, YangModuleSpec::Candidate((f.path(), rev)));
            }
        }

        Ok(YangFiles { lib })
    }

    pub(crate) fn as_ident(name: &str) -> Option<Identifier> {
        if name
            .find(|c: char| {
                !(c.is_alphanumeric() || c == '_' || c == '-' || c == '.')
            })
            .is_none()
            && name.starts_with(|c: char| {
                c.is_ascii_uppercase() || c.is_ascii_lowercase() || c == '_'
            })
        {
            return Some(Identifier::from(name));
        }

        None
    }

    // We're expecting the remainder from stripping the module name and the
    // '@' sign here, e.g. '2018-03-10.yang'
    fn extract_rev(file_name: &str) -> Result<Option<String>, String> {
        let Some(rev) = file_name.split(".yang").next() else {
            return Err(format!(
                "file name part `{}` cannot be split into name and revision",
                file_name
            ));
        };

        if rev.split("-").fold(0, |acc, x| {
            if x.chars().all(|c| c.is_alphanumeric()) {
                acc + 1
            } else {
                acc
            }
        }) != 3
        {
            return Err(format!(
                "file name part `{}` is not a valid revision date",
                file_name
            ));
        };

        Ok(Some(rev.to_string()))
    }

    /// try stuff
    pub fn try_get_or_load(
        &mut self,
        name: &Identifier,
    ) -> Result<(&SourceFile, usize), std::io::Error> {
        let err = std::io::Error::other(format!(
            "cannot find module with name `{name}`"
        ));

        match self.lib.get_full_mut(name) {
            Some((idx, _, YangModuleSpec::Parsed(parsed))) => {
                Ok((&*parsed, idx))
            }
            Some((idx, _, candidate)) => {
                let module_file =
                    SourceFile::read(candidate.path_buf().unwrap_or_else(
                        || ice!("cannot create path buf for file"),
                    ))
                    .map_err(|e| {
                        std::io::Error::other(format!(
                            "Cannot read file for module {:?}",
                            candidate
                        ))
                    })?;
                *candidate = YangModuleSpec::Parsed(module_file);
                candidate.source_file().ok_or(err).map(|c| (c, idx))
            }
            None => Err(err),
        }
    }

    fn find_file_names(
        // &mut self,
        // parent_id: usize,
        path: PathBuf,
        default_ext: &str,
        exclude_entry: Option<&str>,
    ) -> Result<Vec<PathBuf>, RotoReport> {
        let mut file_names = vec![];

        let dir_entries = std::fs::read_dir(&path)
            .map_err(|e| read_error(path.clone(), e))?;

        for entry in dir_entries {
            let entry = entry.map_err(|e| read_error(path.clone(), e))?;
            let file_type =
                entry.file_type().map_err(|e| read_error(entry.path(), e))?;

            if file_type.is_dir() {
                file_names.extend(Self::find_file_names(
                    path.clone(),
                    default_ext,
                    exclude_entry,
                )?);
                continue;
            }

            if entry
                .path()
                .extension()
                .is_none_or(|ext| ext != default_ext)
            {
                continue;
            }

            if entry.path().file_name().and_then(|n| n.to_str())
                == exclude_entry
            {
                continue;
            }

            let _ident = entry.path().file_stem().ok_or_else(|| {
                read_error(
                    entry.path(),
                    std::io::Error::other("invalid path"),
                )
            })?;
            file_names.push(entry.path());
        }

        Ok(file_names)
    }

    fn process_subdir_names(
        &mut self,
        path: PathBuf,
        default_ext: &str,
    ) -> Result<Vec<PathBuf>, RotoReport> {
        Self::find_file_names(path, default_ext, None)
    }

    /// Iterator over all files that are loaded
    pub fn iter_parsed(&self) -> impl Iterator<Item = &SourceFile> {
        self.lib.values().filter_map(|m| {
            let YangModuleSpec::Parsed(f) = m else {
                return None;
            };
            Some(f)
        })
    }
}

impl From<YangFiles> for HashMap<usize, SourceFile> {
    fn from(value: YangFiles) -> Self {
        value
            .lib
            .iter()
            .enumerate()
            .filter_map(|(i, (_, sf))| {
                let YangModuleSpec::Parsed(f) = sf else {
                    return None;
                };
                Some((i, f.clone()))
            })
            .collect::<HashMap<usize, SourceFile>>()
    }
}

// pub struct YangFileIter<'a> {
//     files: &'a YangFiles,
//     count: usize,
// }

// impl<'a> Iterator for YangFileIter<'a> {
//     type Item = &'a SourceFile;

//     fn next(&mut self) -> Option<Self::Item> {
//         if self.count == 0 {
//             self.count += 1;
//             return Some(&self.files.entry_point);
//         }

//         while self.count < self.files.lib.len() {
//             if let Some(YangModuleSpec::Parsed(source_file)) =
//                 self.files.lib.get(self.count - 1)
//             {
//                 self.count += 1;
//                 return Some(source_file);
//             }
//         }

//         None
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

impl FileTree {
    /// Read a [`FileTree`] based on a path.
    ///
    /// If the path refers to a file, only that file will be read. If the path
    /// instead refers to a directory, that directory will be read recursively.
    pub fn read(path: impl AsRef<Path>) -> Result<Self, RotoReport> {
        let path = path.as_ref();
        if path
            .metadata()
            .map_err(|e| read_error(path.to_path_buf(), e))?
            .file_type()
            .is_dir()
        {
            Self::directory(path, "pkg.roto")
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
    pub fn directory(
        root: &Path,
        f_name: &str,
    ) -> Result<FileTree, RotoReport> {
        let pkg_file = SourceFile::read(&root.join(f_name))?;
        // assert_eq!(pkg_file.module_name, "pkg");
        let mut tree = Self {
            files: vec![pkg_file],
        };
        tree.find_files(0, root, "yang", Some(f_name))?;
        Ok(tree)
    }

    fn find_files(
        &mut self,
        parent_id: usize,
        path: &Path,
        default_ext: &str,
        exclude_entry: Option<&str>,
    ) -> Result<(), RotoReport> {
        for entry in std::fs::read_dir(path)
            .map_err(|e| read_error(path.to_path_buf(), e))?
        {
            let entry =
                entry.map_err(|e| read_error(path.to_path_buf(), e))?;
            let path = entry.path();
            let file_type = entry
                .file_type()
                .map_err(|e| read_error(path.to_path_buf(), e))?;

            if file_type.is_dir() {
                self.process_subdir(parent_id, &path, default_ext)?;
                continue;
            }

            if path.extension().is_none_or(|ext| ext != default_ext) {
                continue;
            }

            if path.file_name().and_then(|n| n.to_str()) == exclude_entry {
                continue;
            }

            let ident = path
                .file_stem()
                .ok_or_else(|| {
                    read_error(
                        path.to_path_buf(),
                        std::io::Error::other("invalid path"),
                    )
                })?
                .to_str()
                .ok_or_else(|| {
                    read_error(
                        path.to_path_buf(),
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
        default_ext: &str,
    ) -> Result<(), RotoReport> {
        let file_path = path.join("mod.roto");

        if !file_path.exists() {
            return Ok(());
        }

        let file = SourceFile::read(&file_path)?;

        let idx = self.files.len();
        self.files.push(file);
        self.files[parent_id].children.push(idx);

        self.find_files(idx, path, default_ext, None)
    }
}
