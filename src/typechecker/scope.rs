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
pub enum ScopeRef {
    Global,
    Local(LocalScopeRef),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct LocalScopeRef(usize);

impl From<LocalScopeRef> for ScopeRef {
    fn from(value: LocalScopeRef) -> Self {
        ScopeRef::Local(value)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum DefinitionRef {
    Context(Identifier, usize),
    Constant(Identifier),
    Local(LocalScopeRef, Identifier),
}

impl DefinitionRef {
    pub fn to_scope_and_name(self) -> (ScopeRef, Identifier) {
        match self {
            Self::Constant(ident) | Self::Context(ident, _) => {
                (ScopeRef::Global, ident)
            }
            Self::Local(scope, ident) => (ScopeRef::Local(scope), ident),
        }
    }
}

#[derive(Default)]
pub struct ScopeGraph {
    globals: BTreeMap<Identifier, GlobalDefinition>,
    scopes: Vec<Scope>,
}

struct GlobalDefinition {
    ty: Type,
    kind: GlobalDefinitionKind,
}

enum GlobalDefinitionKind {
    Context(usize),
    Constant,
}

/// A type checking scope
struct Scope {
    /// Map from identifier to fully qualified type
    variables: BTreeMap<Identifier, LocalDefinition>,
    scope_type: ScopeType,
    parent: ScopeRef,
}

struct LocalDefinition {
    ty: Type,
    id: MetaId,
}

pub enum ScopeType {
    Module(Identifier),
    Function(Identifier),
    MatchArm(usize, Option<usize>),
}

impl ScopeGraph {
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new root scope
    pub fn root(&mut self) -> ScopeRef {
        ScopeRef::Global
    }

    /// Create a new scope over `scope`
    pub fn wrap(
        &mut self,
        parent: impl Into<ScopeRef>,
        scope_type: ScopeType,
    ) -> LocalScopeRef {
        self.scopes.push(Scope {
            scope_type,
            variables: BTreeMap::new(),
            parent: parent.into(),
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

    pub fn get_var<'a>(
        &'a self,
        scope: LocalScopeRef,
        identifier: &Meta<Identifier>,
    ) -> Option<(DefinitionRef, &'a Type)> {
        let mut scope = ScopeRef::Local(scope);

        loop {
            match scope {
                ScopeRef::Local(LocalScopeRef(idx)) => {
                    if let Some(def) =
                        &self.scopes[idx].variables.get(identifier)
                    {
                        return Some((
                            DefinitionRef::Local(
                                LocalScopeRef(idx),
                                identifier.node,
                            ),
                            &def.ty,
                        ));
                    }
                }
                ScopeRef::Global => {
                    if let Some(def) = self.globals.get(identifier) {
                        let def_ref = match def.kind {
                            GlobalDefinitionKind::Context(offset) => {
                                DefinitionRef::Context(**identifier, offset)
                            }
                            GlobalDefinitionKind::Constant => {
                                DefinitionRef::Constant(**identifier)
                            }
                        };
                        return Some((def_ref, &def.ty));
                    }
                }
            };
            scope = self.parent(scope)?;
        }
    }

    pub fn insert_context<'a>(
        &'a mut self,
        v: &Meta<Identifier>,
        ty: impl Borrow<Type>,
        offset: usize,
    ) -> Result<(DefinitionRef, &'a Type), ()> {
        let ty = ty.borrow().clone();
        match self.globals.entry(v.node) {
            Entry::Occupied(_) => Err(()),
            Entry::Vacant(entry) => Ok((
                DefinitionRef::Constant(**v),
                &entry
                    .insert(GlobalDefinition {
                        ty,
                        kind: GlobalDefinitionKind::Context(offset),
                    })
                    .ty,
            )),
        }
    }

    pub fn insert_global_constant<'a>(
        &'a mut self,
        v: &Meta<Identifier>,
        ty: impl Borrow<Type>,
    ) -> Result<(DefinitionRef, &'a Type), ()> {
        let ty = ty.borrow().clone();
        match self.globals.entry(v.node) {
            Entry::Occupied(_) => Err(()),
            Entry::Vacant(entry) => Ok((
                DefinitionRef::Constant(**v),
                &entry
                    .insert(GlobalDefinition {
                        ty,
                        kind: GlobalDefinitionKind::Constant,
                    })
                    .ty,
            )),
        }
    }

    pub fn insert_var<'a>(
        &'a mut self,
        scope: LocalScopeRef,
        v: &Meta<Identifier>,
        ty: impl Borrow<Type>,
    ) -> Result<(DefinitionRef, &'a Type), MetaId> {
        let ty = ty.borrow().clone();

        match self.scopes[scope.0].variables.entry(v.node) {
            Entry::Occupied(entry) => Err(entry.get().id),
            Entry::Vacant(entry) => Ok((
                DefinitionRef::Local(scope, v.node),
                &entry.insert(LocalDefinition { ty, id: v.id }).ty,
            )),
        }
    }
}

impl ScopeGraph {
    pub fn print_scope(&self, mut scope: ScopeRef) -> String {
        let mut idents = Vec::new();
        while let ScopeRef::Local(s) = scope {
            let s = &self.scopes[s.0];
            let ident = match &s.scope_type {
                ScopeType::Module(name) => name.as_str().to_string(),
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
