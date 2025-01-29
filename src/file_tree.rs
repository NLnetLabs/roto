use std::path::Path;

use crate::{Compiled, RotoReport, Runtime};

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
    pub fn read(path: &Path) -> Self {
        let file_name = path.file_name().unwrap();
        let module_name = if file_name == "mod.roto" {
            path.parent().unwrap().file_name().unwrap()
        } else {
            file_name
        }
        .to_string_lossy()
        .to_string();
        let name = path.to_string_lossy().to_string();
        let contents = std::fs::read_to_string(path).unwrap();
        Self {
            name,
            module_name,
            contents,
            location_offset: 0,
            children: Vec::new(),
        }
    }
}

/// Compiler stage: Files loaded and ready to be parsed
pub struct FileTree {
    /// All files
    ///
    /// The root of the tree is the files at index 0
    pub files: Vec<SourceFile>,
}

impl FileTree {
    pub fn compile(
        self,
        rt: Runtime,
        pointer_bytes: u32,
    ) -> Result<Compiled, RotoReport> {
        let compiled = self
            .parse()?
            .typecheck(rt, pointer_bytes)?
            .lower()
            .codegen();
        Ok(compiled)
    }
}

impl FileTree {
    pub fn read(path: &Path) -> Self {
        if path.metadata().unwrap().file_type().is_dir() {
            Self::directory(path)
        } else {
            Self::single_file(path)
        }
    }

    /// A Roto script consisting of a single file
    pub fn single_file(path: impl AsRef<Path>) -> Self {
        let mut file = SourceFile::read(path.as_ref());
        file.module_name = "lib".into();
        FileTree { files: vec![file] }
    }

    pub fn test_file(
        file: &str,
        source: &str,
        location_offset: usize,
    ) -> Self {
        FileTree {
            files: vec![SourceFile {
                module_name: "lib".into(),
                location_offset,
                name: file.into(),
                contents: source.into(),
                children: Vec::new(),
            }],
        }
    }

    /// A Roto script defined by a directory
    pub fn directory(root: &Path) -> FileTree {
        let lib_file = SourceFile::read(&root.join("lib.roto"));
        assert_eq!(lib_file.module_name, "lib");
        let mut tree = Self {
            files: vec![lib_file],
        };
        tree.find_files(0, root);
        tree
    }

    fn find_files(&mut self, parent_id: usize, path: &Path) {
        for entry in std::fs::read_dir(path).unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            if entry.file_type().unwrap().is_dir() {
                self.process_subdir(parent_id, &path);
                continue;
            }

            if path.extension().map_or(false, |ext| ext == "roto") {
                continue;
            }

            let ident = path.file_stem().unwrap().to_str().unwrap();

            if ident == "lib" || ident == "mod" {
                continue;
            }

            let file = SourceFile::read(&path);

            let idx = self.files.len();
            self.files.push(file);
            self.files[parent_id].children.push(idx);
        }
    }

    fn process_subdir(&mut self, parent_id: usize, path: &Path) {
        let file_path = path.join("mod.roto");

        if !file_path.exists() {
            return;
        }

        let file = SourceFile::read(&file_path);

        let idx = self.files.len();
        self.files.push(file);
        self.files[parent_id].children.push(idx);

        self.find_files(idx, path);
    }
}
