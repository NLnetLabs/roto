use std::collections::{BTreeMap, BTreeSet};

use crate::{
    ast::Ident,
    parser::meta::{Meta, MetaId},
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct ScopedIdent {
    scope: ScopeRef,
    ident: Ident,
}

/// Resolves references to names to their definitions
#[derive(Default)]
pub struct NameResolver {
    scoped_idents: BTreeMap<Meta<Ident>, ScopedIdent>,
    globals: BTreeSet<Ident>,
    scopes: Vec<Scope>,
}

/// A type checking scope
struct Scope {
    items: BTreeMap<Ident, MetaId>,
    scope_type: ScopeType,
    parent: ScopeRef,
    imports: Vec<Import>,
}

enum Import {
    Wildcard(ScopeRef),
    Ident(ScopedIdent),
}

pub enum ScopeType {
    Then(usize),
    Else(usize),
    Module(Ident),
    Function(Ident),
    MatchArm(usize, Option<usize>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ScopeRef {
    Global,
    Local(LocalScopeRef),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LocalScopeRef(usize);

impl NameResolver {
    fn new() -> Self {
        Self::default()
    }

    fn root() -> ScopeRef {
        ScopeRef::Global
    }

    fn scope_ident(&mut self, scope: ScopeRef, ident: Meta<Ident>) {
        self.scoped_idents
            .insert(
                ident.clone(),
                ScopedIdent {
                    scope,
                    ident: *ident,
                },
            )
            .unwrap();
    }

    fn define_ident(
        &mut self,
        scope: ScopeRef,
        ident: Meta<Ident>,
    ) -> Result<(), MetaId> {
        use std::collections::btree_map::Entry;
        self.scope_ident(scope, ident.clone());
        match scope {
            ScopeRef::Global => {
                if !self.globals.insert(*ident) {
                    return Err(MetaId(0));
                }
            }
            ScopeRef::Local(LocalScopeRef(id)) => {
                match self.scopes[id].items.entry(*ident) {
                    Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(ident.id);
                    }
                    Entry::Occupied(occupied_entry) => {
                        return Err(*occupied_entry.get())
                    }
                }
            }
        };
        Ok(())
    }

    /// Create a new scope over `scope`
    pub fn wrap(
        &mut self,
        parent: impl Into<ScopeRef>,
        scope_type: ScopeType,
    ) -> LocalScopeRef {
        self.scopes.push(Scope {
            scope_type,
            items: BTreeMap::new(),
            parent: parent.into(),
            imports: Vec::new(),
        });
        LocalScopeRef(self.scopes.len() - 1)
    }

    pub fn parent(&self, scope: ScopeRef) -> Option<ScopeRef> {
        match scope {
            ScopeRef::Global => None,
            ScopeRef::Local(LocalScopeRef(idx)) => {
                Some(self.scopes[idx].parent)
            }
        }
    }
}
