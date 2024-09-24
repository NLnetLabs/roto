use std::{
    borrow::Borrow,
    collections::{btree_map::Entry, BTreeMap},
};

use crate::{
    ast::Identifier,
    parser::meta::{Meta, MetaId},
};

use super::Type;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ScopeRef(Option<usize>);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct DefinitionRef(pub ScopeRef, pub Identifier);

#[derive(Default)]
pub struct ScopeGraph {
    globals: BTreeMap<Identifier, Definition>,
    scopes: Vec<Scope>,
}

/// A type checking scope
struct Scope {
    /// Map from identifier to fully qualified type
    variables: BTreeMap<Identifier, Definition>,
    scope_type: ScopeType,
    parent: ScopeRef,
}

struct Definition {
    ty: Type,
    id: MetaId,
}

pub enum ScopeType {
    Function(Identifier),
    MatchArm(usize, Option<usize>),
}

impl ScopeGraph {
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new root scope
    pub fn root(&mut self) -> ScopeRef {
        ScopeRef(None)
    }

    /// Create a new scope over `scope`
    pub fn wrap(
        &mut self,
        parent: ScopeRef,
        scope_type: ScopeType,
    ) -> ScopeRef {
        self.scopes.push(Scope {
            scope_type,
            variables: BTreeMap::new(),
            parent,
        });
        ScopeRef(Some(self.scopes.len() - 1))
    }

    pub fn parent(&self, ScopeRef(scope): ScopeRef) -> Option<ScopeRef> {
        scope.map(|idx| self.scopes[idx].parent)
    }

    pub fn get_var<'a>(
        &'a self,
        mut scope: ScopeRef,
        identifier: &Meta<Identifier>,
    ) -> Option<(DefinitionRef, &'a Type)> {
        loop {
            let variables = match scope.0 {
                Some(idx) => &self.scopes[idx].variables,
                None => &self.globals,
            };
            if let Some(def) = variables.get(identifier) {
                return Some((
                    DefinitionRef(scope, identifier.node),
                    &def.ty,
                ));
            }

            scope = self.parent(scope)?;
        }
    }

    pub fn insert_var<'a>(
        &'a mut self,
        scope: ScopeRef,
        v: &Meta<Identifier>,
        t: impl Borrow<Type>,
    ) -> Result<(DefinitionRef, &'a Type), MetaId> {
        let t = t.borrow().clone();
        let variables = match scope.0 {
            Some(idx) => &mut self.scopes[idx].variables,
            None => &mut self.globals,
        };
        match variables.entry(v.node) {
            Entry::Occupied(entry) => Err(entry.get().id),
            Entry::Vacant(entry) => Ok((
                DefinitionRef(scope, v.node),
                &entry.insert(Definition { ty: t, id: v.id }).ty,
            )),
        }
    }
}

impl ScopeGraph {
    pub fn print_scope(&self, mut scope: ScopeRef) -> String {
        let mut idents = Vec::new();
        while let Some(idx) = scope.0 {
            let s = &self.scopes[idx];
            let ident = match &s.scope_type {
                ScopeType::Function(name) => name.as_str().to_string(),
                ScopeType::MatchArm(idx, Some(arm)) => {
                    format!("$match_{idx}_arm_{arm}")
                }
                ScopeType::MatchArm(idx, None) => {
                    format!("$match_{idx}_arm_default")
                }
            };
            idents.push(ident);
            scope = s.parent;
        }
        idents.reverse();
        idents.join("::")
    }
}
