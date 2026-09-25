//! Type checking scopes

use core::fmt;
use std::collections::btree_map::{BTreeMap, Entry};

use crate::{
    ast::Identifier,
    ice,
    parser::meta::{Meta, MetaId},
    runtime::extern_eq,
    typechecker::types::Signature,
};

use super::{
    Type,
    info::TypeInfo,
    scoped_display::TypeDisplay,
    types::{EnumVariant, FunctionDefinition, TypeDefinition},
};

/// A reference to a [`Scope`] in a [`ScopeGraph`]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScopeRef(pub usize);

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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Declaration {
    pub name: ResolvedName,
    pub kind: DeclarationKind,
    pub id: MetaId,
    pub scope: Option<ScopeRef>,
    pub doc: String,
}

impl Declaration {
    pub(crate) fn recent_revision(&self, other: &str) -> Option<bool> {
        let Some(rev) = (match &self.kind {
            DeclarationKind::YangModule(decl) => &decl.definition.revision,
            DeclarationKind::YangSubModule(decl) => &decl.definition.revision,
            _ => {
                ice!("no module found");
            }
        }) else {
            return None;
        };

        Some(rev.as_str() > other)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DeclarationKind {
    Value(ValueKind, Option<Type>),
    Type(TypeOrStub),
    Function(Option<FunctionDeclaration>),
    Module,
    YangModule(YangModuleDeclaration),
    YangSubModule(YangSubModuleDeclaration),
    Method(Option<FunctionDeclaration>),
    Enum(Option<(TypeDefinition, EnumVariant)>),
    TypeParam(Identifier),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionDeclaration {
    pub definition: FunctionDefinition,
    pub parameter_names: Vec<Identifier>,
    pub signature: Signature,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct YangModuleDeclaration {
    pub definition: YangModuleDefinition,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct YangSubModuleDeclaration {
    pub definition: YangSubModuleDefinition,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct YangModuleDefinition {
    pub name: Identifier,
    pub prefix: Identifier,
    pub namespace: String,
    pub revision: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct YangSubModuleDefinition {
    pub name: Identifier,
    pub revision: Option<String>,
    pub belongs_to: Identifier,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeOrStub {
    Type(TypeDefinition),
    Stub { num_params: usize },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ValueKind {
    Local,
    Constant,
    Context(usize),
}

#[derive(Clone, Debug)]
pub struct ScopeGraph {
    pub declarations: BTreeMap<ResolvedName, Declaration>,
    scopes: Vec<Scope>,
}

/// A type checking scope
#[derive(Clone, Debug)]
struct Scope {
    scope_type: ScopeType,
    parent: Option<ScopeRef>,
    imports: BTreeMap<Identifier, (MetaId, ResolvedName)>,
}

/// The syntactic structure that a scope represents
///
/// This is used primarily for printing a roughly human-readable name.
#[derive(Clone, Debug)]
pub enum ScopeType {
    Root,
    Then(usize),
    Else(usize),
    WhileBody(usize),
    ForBody(usize),
    Module(ModuleScope),
    Function(Identifier),
    MatchArm(usize, Option<usize>),
    Type(Identifier),
    TypeParams,
    Block(usize),
}

#[derive(Clone, Debug)]
pub struct ModuleScope {
    pub name: ResolvedName,
    pub parent_module: Option<ScopeRef>,
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

    pub fn declarations_in(
        &self,
        scope: ScopeRef,
    ) -> impl Iterator<Item = &Declaration> {
        self.declarations
            .iter()
            .filter(move |(n, _)| n.scope == scope)
            .map(|(_, d)| d)
    }

    pub fn parent(&self, scope: ScopeRef) -> Option<ScopeRef> {
        self.scopes[scope.0].parent
    }

    pub fn resolve_name(
        &self,
        mut scope: ScopeRef,
        ident: &Meta<Identifier>,
        recurse: bool,
    ) -> Option<Declaration> {
        println!("ident resolve {ident} scope {:?}", scope);
        loop {
            let name = ResolvedName {
                scope,
                ident: **ident,
            };
            if let Some(d) = self.declarations.get(&name) {
                return Some(d.clone());
            }

            if !recurse {
                return None;
            }

            if let Some(x) = self.scopes[scope.0].imports.get(ident) {
                println!("{ident}");
                println!("resolve x {:?}", x.1);
                println!("scope {:?}", scope);
                println!("imports {:?}", self.scopes[scope.0].imports);
                println!(
                    "decla {:?}",
                    self.declarations
                        .iter()
                        .find(|(a, b)| a.ident == x.1.ident)
                        .into_iter()
                        .collect::<Vec<_>>()
                );
                return Some(self.declarations.get(&x.1).unwrap().clone());
            }

            println!("[resolve_name] {scope:?}");

            scope = self.parent(scope)?;
        }
    }

    pub fn get_declaration(&self, name: ResolvedName) -> Declaration {
        let Some(dec) = self.declarations.get(&name) else {
            ice!("Could not get declaration: {}", name.ident);
        };
        dec.clone()
    }

    pub fn get_declaration_mut(
        &mut self,
        name: ResolvedName,
    ) -> &mut Declaration {
        let Some(dec) = self.declarations.get_mut(&name) else {
            ice!("Could not get declaration: {}", name.ident);
        };
        dec
    }

    pub fn insert_import(
        &mut self,
        scope: ScopeRef,
        id: MetaId,
        key: Identifier,
        name: ResolvedName,
    ) -> Result<(), MetaId> {
        let map = &mut self.scopes[scope.0].imports;
        match map.entry(key) {
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
        let kind = DeclarationKind::Value(
            ValueKind::Context(offset),
            Some(ty.clone()),
        );
        let id = v.id;

        match self.declarations.entry(name) {
            Entry::Occupied(_) => Err(()),
            Entry::Vacant(entry) => {
                entry.insert(Declaration {
                    name,
                    kind,
                    id,
                    doc: String::new(),
                    scope: None,
                });
                Ok(name)
            }
        }
    }

    pub fn insert_runtime_constant(
        &mut self,
        scope: ScopeRef,
        v: &Meta<Identifier>,
        ty: &Type,
        doc: String,
    ) -> Result<ResolvedName, ()> {
        let name = ResolvedName { scope, ident: **v };
        let kind =
            DeclarationKind::Value(ValueKind::Constant, Some(ty.clone()));
        let id = v.id;

        match self.declarations.entry(name) {
            Entry::Occupied(_) => Err(()),
            Entry::Vacant(entry) => {
                entry.insert(Declaration {
                    name,
                    kind,
                    id,
                    doc,
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
        let kind = DeclarationKind::Value(ValueKind::Local, Some(ty.clone()));
        let dec = self.insert_declaration(
            scope,
            ident,
            kind,
            String::new(),
            |_| false,
        )?;
        Ok(dec.name)
    }

    pub fn insert_const(
        &mut self,
        scope: ScopeRef,
        ident: &Meta<Identifier>,
        ty: &Type,
    ) -> Result<ResolvedName, MetaId> {
        let kind =
            DeclarationKind::Value(ValueKind::Constant, Some(ty.clone()));
        let dec = self.insert_declaration(
            scope,
            ident,
            kind,
            String::new(),
            |kind| {
                matches!(
                    kind,
                    DeclarationKind::Value(ValueKind::Constant, None)
                )
            },
        )?;
        Ok(dec.name)
    }

    pub fn insert_type(
        &mut self,
        scope: ScopeRef,
        ident: &Meta<Identifier>,
        doc: String,
        ty: TypeDefinition,
    ) -> Result<(), MetaId> {
        let kind = DeclarationKind::Type(TypeOrStub::Type(ty.clone()));
        let new_scope = self.wrap(scope, ScopeType::Type(**ident));
        let dec =
            self.insert_declaration(scope, ident, kind, doc, |kind| {
                if let DeclarationKind::Type(TypeOrStub::Stub {
                    num_params,
                }) = kind
                {
                    *num_params == ty.type_name().arguments.len()
                } else {
                    false
                }
            })?;

        if dec.scope.is_none() {
            dec.scope = Some(new_scope);
        }
        Ok(())
    }

    pub fn insert_module(
        &mut self,
        scope: ScopeRef,
        ident: &Meta<Identifier>,
        doc: String,
        mod_scope: ScopeRef,
    ) -> Result<(), MetaId> {
        let kind = DeclarationKind::Module;
        let dec =
            self.insert_declaration(scope, ident, kind, doc, |_| false)?;
        dec.scope = Some(mod_scope);
        Ok(())
    }

    pub fn insert_module2(
        &mut self,
        scope: ScopeRef,
        kind: DeclarationKind,
        ident: &Meta<Identifier>,
        doc: String,
        // mod_scope: ScopeRef,
    ) -> Result<&mut Declaration, MetaId> {
        // let kind = DeclarationKind::YangModule(decl);
        // let ident = declaration.ident();

        let name = ResolvedName {
            scope: ScopeRef::GLOBAL,
            ident: **ident,
        };

        let decl = match self.declarations.entry(name) {
            Entry::Vacant(entry) => {
                let new = Declaration {
                    name,
                    kind: kind.clone(),
                    id: ident.id,
                    scope: None,
                    doc: doc.clone(),
                };
                Ok(entry.insert(new))
            }
            Entry::Occupied(entry) => Err(entry.into_mut()),
        };

        match decl {
            Ok(decl) => Ok(decl),
            Err(exist_decl) => {
                if let DeclarationKind::YangModule(YangModuleDeclaration {
                    definition: md,
                }) = &exist_decl.kind
                {
                    let Some(new_rev) = &md.revision else {
                        return Err(exist_decl.id);
                    };
                    let out_string = new_rev.clone();

                    let Some(more_recent) =
                        exist_decl.recent_revision(new_rev)
                    else {
                        return Err(exist_decl.id);
                    };

                    if more_recent {
                        *exist_decl = Declaration {
                            name,
                            kind,
                            id: ident.id,
                            scope: None,
                            doc,
                        };
                    }
                    println!(
                        "duplicate module {}: picking revision {}",
                        name.ident, out_string
                    );
                    return Ok(exist_decl);
                };

                Err(exist_decl.id)
            }
        }
    }

    pub fn insert_function(
        &mut self,
        scope: ScopeRef,
        ident: &Meta<Identifier>,
        definition: FunctionDefinition,
        parameter_names: Vec<Identifier>,
        doc: String,
        signature: Signature,
    ) -> Result<ResolvedName, MetaId> {
        let kind = DeclarationKind::Function(Some(FunctionDeclaration {
            definition,
            parameter_names,
            signature,
        }));

        let dec =
            self.insert_declaration(scope, ident, kind, doc, |kind| {
                matches!(kind, DeclarationKind::Function(None))
            })?;

        Ok(dec.name)
    }

    pub fn insert_method(
        &mut self,
        scope: ScopeRef,
        ident: &Meta<Identifier>,
        definition: FunctionDefinition,
        parameter_names: Vec<Identifier>,
        doc: String,
        signature: Signature,
    ) -> Result<ResolvedName, MetaId> {
        let kind = DeclarationKind::Method(Some(FunctionDeclaration {
            definition,
            parameter_names,
            signature,
        }));
        let dec =
            self.insert_declaration(scope, ident, kind, doc, |kind| {
                matches!(kind, DeclarationKind::Method(None))
            })?;
        Ok(dec.name)
    }

    pub fn insert_declaration(
        &mut self,
        scope: ScopeRef,
        ident: &Meta<Identifier>,
        kind: DeclarationKind,
        doc: String,
        update_if: impl Fn(&DeclarationKind) -> bool,
    ) -> Result<&mut Declaration, MetaId> {
        let name = ResolvedName {
            scope,
            ident: **ident,
        };
        match self.declarations.entry(name) {
            Entry::Vacant(entry) => {
                let new = Declaration {
                    name,
                    kind,
                    id: ident.id,
                    scope: None,
                    doc,
                };
                Ok(entry.insert(new))
            }
            Entry::Occupied(entry) => {
                let old = entry.into_mut();
                if update_if(&old.kind) {
                    old.kind = kind;
                    Ok(old)
                } else {
                    Err(old.id)
                }
            }
        }
    }

    pub fn parent_module(&self, mut scope: ScopeRef) -> Option<Declaration> {
        loop {
            let s = &self.scopes[scope.0];

            if let ScopeType::Module(m) = &s.scope_type {
                let ScopeType::Module(parent) =
                    &self.scopes[m.parent_module?.0].scope_type
                else {
                    unreachable!();
                };
                return Some(self.get_declaration(parent.name));
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
                ScopeType::Block(idx) => {
                    format!("$block_{idx}")
                }
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
                ScopeType::ForBody(idx) => {
                    format!("$for_body_{idx}")
                }
                ScopeType::Type(name) => name.as_str().to_string(),
                ScopeType::TypeParams => "$type_params".into(),
            };
            idents.push(ident);
            scope = s.parent;
        }
        idents.reverse();
        idents.join(".")
    }
}
