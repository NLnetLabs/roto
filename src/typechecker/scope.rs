use std::{
    borrow::Borrow,
    collections::{hash_map::Entry, HashMap},
};

use crate::{ast::Identifier, parser::meta::Meta};

use super::{error, Type, TypeResult};

/// A type checking scope
#[derive(Default)]
pub struct Scope<'a> {
    /// Map from identifier to fully qualified type
    variables: HashMap<String, Type>,
    /// Parent scope
    parent: Option<&'a Scope<'a>>,
    /// Prefix for fully qualified names
    prefix: String,
}

impl<'a> Scope<'a> {
    /// Create a new scope over self
    ///
    /// The wrapped scope cannot be mutated while the new scope exist.
    pub fn wrap(&'a self, ident: &str) -> Self {
        Self {
            variables: HashMap::default(),
            parent: Some(self),
            prefix: format!("{}{ident}::", self.prefix),
        }
    }

    pub fn get_var(&self, k: &Meta<Identifier>) -> TypeResult<(String, &Type)> {
        self.variables
            .get(k.as_ref())
            .ok_or_else(|| error::simple(
                format!(
                    "cannot find variable `{}` in this scope",
                    k.as_ref()
                ),
                "not found in this scope",
                k.id,
            ))
            .map(|t| {
                (format!("{}{k}", self.prefix), t)
            })
            .or_else(|e| self.parent.ok_or(e).and_then(|s| s.get_var(k)))
    }

    pub fn insert_var(
        &mut self,
        v: &Meta<Identifier>,
        t: impl Borrow<Type>,
    ) -> TypeResult<(String, &mut Type)> {
        let t = t.borrow().clone();
        let v_string = v.as_ref().to_string();
        match self.variables.entry(v_string) {
            Entry::Occupied(entry) => Err(error::simple(
                format!(
                    "variable {} defined multiple times in the same scope",
                    entry.key()
                ),
                "variable already declared",
                v.id
            )),
            Entry::Vacant(entry) => Ok((format!("{}{v}", self.prefix), entry.insert(t))),
        }
    }
}
