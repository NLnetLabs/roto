use std::{borrow::Borrow, collections::{hash_map::Entry, HashMap}};

use super::{Type, TypeResult};

/// A type checking scope
#[derive(Default)]
pub struct Scope<'a> {
    /// Map from identifier to type
    variables: HashMap<String, Type>,
    /// Parent scope
    parent: Option<&'a Scope<'a>>,
}

impl<'a> Scope<'a> {
    /// Create a new scope over self
    ///
    /// The wrapped scope cannot be mutated while the new scope exist.
    pub fn wrap(&'a self) -> Self {
        Self {
            variables: HashMap::default(),
            parent: Some(self),
        }
    }

    pub fn get_var(&self, k: impl AsRef<str>) -> TypeResult<&Type> {
        self.variables
            .get(k.as_ref())
            .ok_or_else(|| format!("The variable {} is undefined", k.as_ref()))
            .or_else(|e| self.parent.ok_or(e).and_then(|s| s.get_var(k)))
    }

    pub fn insert_var(
        &mut self,
        v: impl AsRef<str>,
        t: impl Borrow<Type>,
    ) -> TypeResult<&mut Type> {
        let v = v.as_ref().to_string();
        let t = t.borrow().clone();
        match self.variables.entry(v) {
            Entry::Occupied(entry) => Err(format!(
                "variable {} defined multiple times in the same scope",
                entry.key()
            )),
            Entry::Vacant(entry) => Ok(entry.insert(t)),
        }
    }
}
