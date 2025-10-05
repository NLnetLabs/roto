use std::path::Path;

use crate::file_tree::{FileTree, SourceFile};

#[test]
fn source_file_try_read_empty_file() {
    let source = SourceFile::read(Path::new(""));
    let _err = source.unwrap_err();
}

#[test]
fn source_file_try_read_nonexistent_file() {
    let source = SourceFile::read(Path::new("/nonexistent/file/pkg.roto"));
    let _err = source.unwrap_err();
}

#[test]
fn file_tree_try_single_file() {
    let tree = FileTree::single_file("");
    let _err = tree.unwrap_err();
}
