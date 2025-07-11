//! Type checking scopes

use core::fmt;
use std::collections::btree_map::{BTreeMap, Entry};

use crate::{
    ast::Identifier,
    ice,
    parser::meta::{Meta, MetaId},
};

use super::{
    info::TypeInfo,
    scoped_display::TypeDisplay,
    types::{EnumVariant, FunctionDefinition, TypeDefinition},
    Type,
};

/// A reference to a [`Scope`] in a [`ScopeGraph`]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScopeRef(usize);

impl ScopeRef {
    /// The scope at the root of a [`ScopeGraph`]
    pub const GLOBAL: Self = Self(0);
}

/// Combination of a scope and an identifier
///
/// This forms a unique name for an item.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ResolvedName {
    pub scope: ScopeRef,
    pub ident: Identifier,
}

impl TypeDisplay for ResolvedName {
    fn fmt(
        &self,
        type_info: &TypeInfo,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        let scope = type_info.scope_graph.print_scope(self.scope);
        if scope.is_empty() {
            write!(f, "{}", self.ident)
        } else {
            write!(f, "{scope}.{}", self.ident)
        }
    }
}

/// A declaration in a [`ScopeGraph`]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Declaration {
    pub name: ResolvedName,
    pub kind: DeclarationKind,
    pub id: MetaId,
    pub scope: Option<ScopeRef>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DeclarationKind {
    Value(ValueKind, Type),
    Type(TypeDefinition),
    Function(FunctionDefinition, Type),
    Module,
    Method(FunctionDefinition, Type),
    Variant(TypeDefinition, EnumVariant),
    Stub(StubDeclarationKind),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ValueKind {
    Local,
    Constant,
    Context(usize),
}

/// An incomplete declaration in a [`ScopeGraph`]
///
/// This is used to declare types before they can be fully defined.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct StubDeclaration {
    pub name: ResolvedName,
    pub kind: StubDeclarationKind,
    pub scope: Option<ScopeRef>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum StubDeclarationKind {
    Context,
    Constant,
    Variable,
    /// The declaration is a type
    ///
    /// The usize parameter specifies the number of type parameters
    Type(usize),
    Function,
    Module,
    Method,
    Variant,
}

#[derive(Clone)]
pub struct ScopeGraph {
    pub declarations: BTreeMap<ResolvedName, Declaration>,
    scopes: Vec<Scope>,
}

/// A type checking scope
#[derive(Clone)]
struct Scope {
    scope_type: ScopeType,
    parent: Option<ScopeRef>,
    imports: BTreeMap<Identifier, (MetaId, ResolvedName)>,
}

/// The syntactic structure that a scope represents
///
/// This is used primarily for printing a roughly human-readable name.
#[derive(Clone)]
pub enum ScopeType {
    Root,
    Then(usize),
    Else(usize),
    WhileBody(usize),
    Module(ModuleScope),
    Function(Identifier),
    MatchArm(usize, Option<usize>),
    Type(Identifier),
}

#[derive(Clone)]
pub struct ModuleScope {
    pub name: ResolvedName,
    pub parent_module: Option<ScopeRef>,
}

impl Declaration {
    pub fn to_stub(&self) -> StubDeclaration {
        StubDeclaration {
            name: self.name,
            kind: self.kind.to_stub(),
            scope: self.scope,
        }
    }
}

impl DeclarationKind {
    fn to_stub(&self) -> StubDeclarationKind {
        match self {
            Self::Value(ValueKind::Local, _) => StubDeclarationKind::Variable,
            Self::Value(ValueKind::Constant, _) => {
                StubDeclarationKind::Constant
            }
            Self::Value(ValueKind::Context(_), _) => {
                StubDeclarationKind::Context
            }
            Self::Type(def) => {
                StubDeclarationKind::Type(def.type_parameters())
            }
            Self::Function(_, _) => StubDeclarationKind::Function,
            Self::Method(_, _) => StubDeclarationKind::Method,
            Self::Variant(_, _) => StubDeclarationKind::Variant,
            Self::Module => StubDeclarationKind::Module,
            Self::Stub(s) => *s,
        }
    }
}

impl ScopeGraph {
    pub fn new() -> Self {
        Self {
            declarations: BTreeMap::new(),
            scopes: vec![Scope {
                scope_type: ScopeType::Root,
                parent: None,
                imports: BTreeMap::new(),
            }],
        }
    }

    /// Create a new root scope
    pub fn root(&mut self) -> ScopeRef {
        ScopeRef::GLOBAL
    }

    /// Create a new scope over `scope`
    pub fn wrap(
        &mut self,
        parent: impl Into<ScopeRef>,
        scope_type: ScopeType,
    ) -> ScopeRef {
        let idx = self.scopes.len();
        self.scopes.push(Scope {
            scope_type,
            parent: Some(parent.into()),
            imports: BTreeMap::new(),
        });
        ScopeRef(idx)
    }

    pub fn parent(&self, scope: ScopeRef) -> Option<ScopeRef> {
        self.scopes[scope.0].parent
    }

    pub fn resolve_name(
        &self,
        mut scope: ScopeRef,
        ident: &Meta<Identifier>,
        recurse: bool,
    ) -> Option<StubDeclaration> {
        loop {
            let name = ResolvedName {
                scope,
                ident: **ident,
            };
            if let Some(d) = self.declarations.get(&name) {
                return Some(d.to_stub());
            }

            if !recurse {
                return None;
            }

            if let Some(x) = self.scopes[scope.0].imports.get(ident) {
                return Some(self.declarations.get(&x.1).unwrap().to_stub());
            }

            scope = self.parent(scope)?;
        }
    }

    pub fn get_declaration(&self, name: ResolvedName) -> Declaration {
        let Some(dec) = self.declarations.get(&name) else {
            ice!("Could not get declaration: {}", name.ident);
        };
        dec.clone()
    }

    pub fn insert_import(
        &mut self,
        scope: ScopeRef,
        id: MetaId,
        name: ResolvedName,
    ) -> Result<(), MetaId> {
        let map = &mut self.scopes[scope.0].imports;
        match map.entry(name.ident) {
            Entry::Occupied(entry) => Err(entry.get().0),
            Entry::Vacant(entry) => {
                entry.insert((id, name));
                Ok(())
            }
        }
    }

    pub fn insert_context(
        &mut self,
        v: &Meta<Identifier>,
        ty: &Type,
        offset: usize,
    ) -> Result<ResolvedName, ()> {
        let name = ResolvedName {
            scope: ScopeRef::GLOBAL,
            ident: **v,
        };
        let kind =
            DeclarationKind::Value(ValueKind::Context(offset), ty.clone());
        let id = v.id;

        match self.declarations.entry(name) {
            Entry::Occupied(_) => Err(()),
            Entry::Vacant(entry) => {
                entry.insert(Declaration {
                    name,
                    kind,
                    id,
                    scope: None,
                });
                Ok(name)
            }
        }
    }

    pub fn insert_global_constant(
        &mut self,
        v: &Meta<Identifier>,
        ty: &Type,
    ) -> Result<ResolvedName, ()> {
        let name = ResolvedName {
            scope: ScopeRef::GLOBAL,
            ident: **v,
        };
        let kind = DeclarationKind::Value(ValueKind::Constant, ty.clone());
        let id = v.id;

        match self.declarations.entry(name) {
            Entry::Occupied(_) => Err(()),
            Entry::Vacant(entry) => {
                entry.insert(Declaration {
                    name,
                    kind,
                    id,
                    scope: None,
                });
                Ok(name)
            }
        }
    }

    pub fn insert_var(
        &mut self,
        scope: ScopeRef,
        ident: &Meta<Identifier>,
        ty: &Type,
    ) -> Result<ResolvedName, MetaId> {
        let kind = DeclarationKind::Value(ValueKind::Local, ty.clone());
        let dec = self.insert_declaration(scope, ident, kind)?;
        Ok(dec.name)
    }

    pub fn insert_type(
        &mut self,
        scope: ScopeRef,
        ident: &Meta<Identifier>,
        ty: TypeDefinition,
    ) -> Result<(), MetaId> {
        let kind = DeclarationKind::Type(ty.clone());
        let new_scope = self.wrap(scope, ScopeType::Type(**ident));
        let dec = self.insert_declaration(scope, ident, kind)?;

        dec.scope = Some(new_scope);
        Ok(())
    }

    pub fn insert_module(
        &mut self,
        scope: ScopeRef,
        ident: &Meta<Identifier>,
        mod_scope: ScopeRef,
    ) -> Result<(), MetaId> {
        let kind = DeclarationKind::Module;
        let dec = self.insert_declaration(scope, ident, kind)?;
        dec.scope = Some(mod_scope);
        Ok(())
    }

    pub fn insert_function(
        &mut self,
        scope: ScopeRef,
        ident: &Meta<Identifier>,
        definition: FunctionDefinition,
        ty: &Type,
    ) -> Result<ResolvedName, MetaId> {
        let kind = DeclarationKind::Function(definition, ty.clone());
        let dec = self.insert_declaration(scope, ident, kind)?;
        Ok(dec.name)
    }

    pub fn insert_method(
        &mut self,
        scope: ScopeRef,
        ident: &Meta<Identifier>,
        definition: FunctionDefinition,
        ty: &Type,
    ) -> Result<ResolvedName, MetaId> {
        let kind = DeclarationKind::Method(definition, ty.clone());
        let dec = self.insert_declaration(scope, ident, kind)?;
        Ok(dec.name)
    }

    pub fn insert_stub(
        &mut self,
        scope: ScopeRef,
        ident: &Meta<Identifier>,
        stub: StubDeclarationKind,
    ) {
        let name = ResolvedName {
            scope,
            ident: **ident,
        };
        self.declarations.insert(
            name,
            Declaration {
                kind: DeclarationKind::Stub(stub),
                name,
                id: MetaId(0),
                scope: None,
            },
        );
    }

    pub fn insert_declaration(
        &mut self,
        scope: ScopeRef,
        ident: &Meta<Identifier>,
        kind: DeclarationKind,
    ) -> Result<&mut Declaration, MetaId> {
        let name = ResolvedName {
            scope,
            ident: **ident,
        };
        let new = Declaration {
            name,
            kind,
            id: ident.id,
            scope: None,
        };
        match self.declarations.entry(name) {
            Entry::Vacant(entry) => Ok(entry.insert(new)),
            Entry::Occupied(entry) => {
                let old = entry.into_mut();

                // We can only overwrite the existing enty if it is a
                // stub declaration of the same kind as the new
                // declaration and if the new declaration is not a stub
                // declaration.
                let DeclarationKind::Stub(stub_kind) = old.kind else {
                    return Err(old.id);
                };

                if matches!(new.kind, DeclarationKind::Stub(_)) {
                    return Err(old.id);
                }

                if new.kind.to_stub() != stub_kind {
                    return Err(old.id);
                }

                *old = new;
                Ok(old)
            }
        }
    }

    pub fn parent_module(
        &self,
        mut scope: ScopeRef,
    ) -> Option<StubDeclaration> {
        loop {
            let s = &self.scopes[scope.0];

            if let ScopeType::Module(m) = &s.scope_type {
                let ScopeType::Module(parent) =
                    &self.scopes[m.parent_module?.0].scope_type
                else {
                    unreachable!();
                };
                return Some(StubDeclaration {
                    name: parent.name,
                    kind: StubDeclarationKind::Module,
                    scope: m.parent_module,
                });
            }

            scope = self.parent(scope)?;
        }
    }
}

impl Default for ScopeGraph {
    fn default() -> Self {
        Self::new()
    }
}

impl ScopeGraph {
    pub fn num_of_scopes(&self) -> usize {
        self.scopes.len()
    }

    pub fn module_name(&self, m: &ModuleScope) -> String {
        let mut idents = Vec::new();

        let mut m = Some(m);
        while let Some(current) = m {
            idents.push(current.name.ident.to_string());
            m = current.parent_module.map(|idx| {
                let s = &self.scopes[idx.0];
                let ScopeType::Module(m) = &s.scope_type else {
                    panic!();
                };
                m
            });
        }

        idents.reverse();
        idents.join(".")
    }
    pub fn print_scope(&self, scope: ScopeRef) -> String {
        let mut idents = Vec::new();
        let mut scope = Some(scope);
        while let Some(s) = scope {
            let s = &self.scopes[s.0];
            let ident = match &s.scope_type {
                ScopeType::Root => break,
                ScopeType::Module(m) => self.module_name(m),
                ScopeType::Function(name) => name.as_str().to_string(),
                ScopeType::Then(idx) => {
                    format!("$if_{idx}_then")
                }
                ScopeType::Else(idx) => {
                    format!("$if_{idx}_else")
                }
                ScopeType::MatchArm(idx, Some(arm)) => {
                    format!("$match_{idx}_arm_{arm}")
                }
                ScopeType::MatchArm(idx, None) => {
                    format!("$match_{idx}_arm_default")
                }
                ScopeType::WhileBody(idx) => {
                    format!("$while_body_{idx}")
                }
                ScopeType::Type(name) => name.as_str().to_string(),
            };
            idents.push(ident);
            scope = s.parent;
        }
        idents.reverse();
        idents.join(".")
    }
}
