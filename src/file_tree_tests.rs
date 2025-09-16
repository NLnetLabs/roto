use std::path::Path;

use crate::file_tree::{FileTree, FileTreeError, SourceFile};

#[test]
fn source_file_try_read_empty_file() {
    let source = SourceFile::try_read(Path::new(""));
    let err = source.unwrap_err();

    assert!(matches!(err, FileTreeError::PathNotFound));
}

#[test]
fn source_file_try_read_nonexistent_file() {
    let source =
        SourceFile::try_read(Path::new("/non-existent/file/pkg.roto"));
    let err = source.unwrap_err();

    assert!(matches!(err, FileTreeError::IOError(_)));
}

#[test]
fn file_tree_try_single_file() {
    let tree = FileTree::try_single_file("");
    let err = tree.unwrap_err();

    assert!(matches!(err, FileTreeError::PathNotFound));
}
