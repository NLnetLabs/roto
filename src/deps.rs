//! Dependency management for Roto scripts
//!
//! We're currently not taking versions of dependencies into account.

use std::{
    collections::{HashMap, VecDeque, hash_map::Entry},
    path::{Path, PathBuf},
};

use crate::{
    FileTree, RotoError, RotoReport,
    ast::Identifier,
    file_tree::{Load, Package, ReadError},
    module::Parsed,
    parser::meta::Spans,
};

#[derive(Debug)]
pub(crate) struct DepGraph {
    pub(crate) main: Identifier,
    pub(crate) pkgs: HashMap<Identifier, FileTree>,
    pub(crate) deps: HashMap<Identifier, Vec<Identifier>>,
}

impl DepGraph {
    pub(crate) fn parse(self) -> Result<Parsed, RotoReport> {
        Parsed::from_files(self)
    }
}

pub(crate) fn resolve(
    provider: &BoxDepSource,
    pkg: Package,
) -> Result<DepGraph, RotoReport> {
    let main = pkg.name.into();

    let mut pkgs = HashMap::new();
    let mut deps = HashMap::new();

    pkgs.insert(main, pkg.files);

    let main_deps: Vec<Identifier> =
        pkg.deps.iter().map(Into::into).collect();
    deps.insert(main, main_deps.clone());

    let mut unresolved = VecDeque::new();
    unresolved.extend(main_deps);

    let mut deps_not_found = Vec::<String>::new();
    let mut errors = Vec::new();

    while let Some(dep) = unresolved.pop_front() {
        if pkgs.contains_key(&dep) {
            continue;
        }

        let Some(res) = provider.0.provide(dep.as_str()) else {
            deps_not_found.push(dep.as_str().into());
            continue;
        };

        match res {
            Ok(pkg) => {
                let name = pkg.name.into();
                let pkg_deps: Vec<Identifier> =
                    pkg.deps.iter().map(Into::into).collect();
                pkgs.insert(name, pkg.files);
                deps.insert(name, pkg_deps.clone());
                unresolved.extend(pkg_deps)
            }
            Err(e) => {
                errors.push(RotoError::Read(e));
            }
        }
    }

    if !deps_not_found.is_empty() {
        errors.push(RotoError::DepsNotFound(deps_not_found));
    }

    if !errors.is_empty() {
        return Err(RotoReport {
            files: Vec::new(),
            errors,
            spans: Spans::default(),
        });
    }

    Ok(DepGraph { main, pkgs, deps })
}

pub(crate) struct BoxDepSource(Box<dyn DepSource>);

impl BoxDepSource {
    pub fn new(provider: impl DepSource + 'static) -> Self {
        Self(Box::new(provider))
    }
}

impl Clone for BoxDepSource {
    fn clone(&self) -> Self {
        Self(self.0.clone_to_box())
    }
}

/// A source of Roto dependencies.
///
/// This can be added to a runtime with [`Runtime::set_dependency_source`].
///
/// You can implement this yourself or use one of the provided implementors
/// such as [`Directory`] and [`Map`].
pub trait DepSource {
    /// Provide a dependency to a script.
    ///
    /// The dependency is requested by name. If this source cannot provide
    /// the dependency, it should return `None`.
    fn provide(&self, dep: &str) -> Option<Result<Package, ReadError>>;

    /// Clone it self to a boxed version of itself.
    ///
    /// Usually this can be implemented as `Box::new(self.clone())`.
    ///
    /// This method is required because `DepSource` needs to be dyn-compatible
    /// and `Clone` is not.
    fn clone_to_box(&self) -> Box<dyn DepSource>;
}

/// A dependency source that searches for dependencies in a directory.
#[derive(Clone)]
pub struct Directory {
    path: PathBuf,
}

impl Directory {
    /// Create a new [`Directory`], which will be looking for dependencies in
    /// `path`.
    pub fn new(path: impl AsRef<Path>) -> Self {
        Self {
            path: path.as_ref().into(),
        }
    }
}

impl DepSource for Directory {
    fn clone_to_box(&self) -> Box<dyn DepSource> {
        Box::new(self.clone())
    }

    fn provide(&self, dep: &str) -> Option<Result<Package, ReadError>> {
        let path = self.path.join(dep);
        if !path.exists() && !path.with_extension("roto").exists() {
            return None;
        }
        Some((&path).load())
    }
}

/// A map of dependencies.
///
/// Each dependency is a pre-loaded [`Package`]. To create a [`Package`] to
/// insert into this map, see either [`Package::new`] or [`Load`].
#[derive(Clone, Default)]
pub struct Map {
    map: HashMap<String, Package>,
}

/// A dependency with this name already exists in this map.
#[derive(Debug)]
#[allow(dead_code)]
pub struct AlreadyExistsError(String);

impl Map {
    /// Create a new [`Map`].
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    /// Add a new dependency to this dependency source.
    pub fn insert(
        &mut self,
        name: &str,
        pkg: Package,
    ) -> Result<(), AlreadyExistsError> {
        match self.map.entry(name.into()) {
            Entry::Occupied(_) => Err(AlreadyExistsError(name.into())),
            Entry::Vacant(e) => {
                e.insert(pkg);
                Ok(())
            }
        }
    }
}

impl DepSource for Map {
    fn provide(&self, dep: &str) -> Option<Result<Package, ReadError>> {
        self.map.get(dep).map(|p| Ok(p.clone()))
    }

    fn clone_to_box(&self) -> Box<dyn DepSource> {
        Box::new(self.clone())
    }
}

impl<F: Clone + Fn(&str) -> Option<Result<Package, ReadError>> + 'static>
    DepSource for F
{
    fn provide(&self, dep: &str) -> Option<Result<Package, ReadError>> {
        self(dep)
    }

    fn clone_to_box(&self) -> Box<dyn DepSource> {
        Box::new(self.clone())
    }
}
