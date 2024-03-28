use std::{
    borrow::Borrow,
    collections::{hash_map::Entry, HashMap},
};

use crate::{ast::Identifier, parser::span::Spanned};

use super::{error, Type, TypeResult};

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

    pub fn get_var(&self, k: &Spanned<Identifier>) -> TypeResult<&Type> {
        self.variables
            .get(k.as_ref())
            .ok_or_else(|| error::simple(
                &format!(
                    "cannot find variable `{}` in this scope",
                    k.as_ref()
                ),
                "not found in this scope",
                k.span,
            ))
            .or_else(|e| self.parent.ok_or(e).and_then(|s| s.get_var(k)))
    }

    pub fn insert_var(
        &mut self,
        v: &Spanned<Identifier>,
        t: impl Borrow<Type>,
    ) -> TypeResult<&mut Type> {
        let t = t.borrow().clone();
        let v_string = v.as_ref().to_string();
        match self.variables.entry(v_string) {
            Entry::Occupied(entry) => Err(error::simple(
                &format!(
                    "variable {} defined multiple times in the same scope",
                    entry.key()
                ),
                "variable already declared",
                v.span
            )),
            Entry::Vacant(entry) => Ok(entry.insert(t)),
        }
    }
}
