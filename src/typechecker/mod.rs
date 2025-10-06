//! Type checker for Roto scripts
//!
//! This type checker performs simplified Hindley-Milner type inference.
//! The simplification is that we only have one situation in which general
//! type variables are generated: method instantiation. Otherwise, we do
//! not have to deal with polymorphism at all. However, we might still
//! extend the type system later to accommodate for that.
//!
//! The current implementation is based on Algorithm M, described in
//! <https://dl.acm.org/doi/pdf/10.1145/291891.291892>. The advantage of
//! Algorithm M over the classic Algorithm W is that it yields errors
//! earlier in the type inference, so that errors appear more often where
//! they are caused.
//!
//! See also <https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system>.
//!
//! # Type checking steps
//!
//! There are several type checking steps that happen at each compilation. The
//! order of these steps is fixed for a variety of reasons.
//!
//! ## Declaring built-in types
//!
//! We start by declaring all the built-in types, since these need to be
//! available for things in the runtime and in the script. This includes
//! primitives (`bool`, `u32`, etc.), `Option[T]`, `Verdict[A, R]` and
//! things like that.
//!
//! The methods of these types are also declared.
//!
//! See [`TypeChecker::declare_builtin_types`].
//!
//! ## Declaring runtime types and functions
//!
//! After the built-in types, we move on to the runtime types, methods
//! and functions. These can reference built-in types. We now have everything
//! we need up front, so we can move on to type checking the script itself.
//!
//! See [`TypeChecker::declare_runtime_items`].
//!
//! ## Declaring modules
//!
//! We first determine the general structure of the script. Meaning that we
//! build the scope tree for the modules and add a [`Declaration`] for
//! the items in it. At this stage, we do not have all the information to
//! resolve the contents of each declaration, so each [`Declaration`] only
//! contains the minimal information we need for name resolution.
//!
//! See [`TypeChecker::declare_modules`].
//!
//! ## Resolving imports
//!
//! With the modules in place, it's possible to resolve the module-level imports
//! in script. An important detail is that the order of imports does not impact
//! the semantics of the script. Therefore, we keep trying to resolve each of them
//! until we either have none left or we can't resolve further.
//!
//! See [`TypeChecker::declare_imports`].
//!
//! ## Declaring types
//!
//! The full structure for name resolution is now in place, which means we can
//! start filling in the each [`Declaration`] we found before and add its
//! with its actual definition. We start with the types declared in the script.
//!
//! See [`TypeChecker::declare_types`].
//!
//! ## Detecting type cycles
//!
//! It's important to ensure that types are not recursive, because a recursive
//! type has an infinite size, so we have an additional check for this.
//!
//! See [`detect_type_cycles`].
//!
//! ## Declaring functions
//!
//! Since functions can call each other, we first declare each function and
//! its type without type checking its body.
//!
//! See [`TypeChecker::declare_functions`].
//!
//! ## Type checking function bodies
//!
//! We can now type check the actual expressions in the script.
//!
//! See [`TypeChecker::tree`]
//!
//! ## Force filtermap types to unit
//!
//! Some filtermaps only `accept` or `reject`, leaving the other type
//! undetermined. We force those to the unit type.
//!
//! See [`TypeChecker::force_filtermap_types`]
//!
//! [`Declaration`]: scope::Declaration

use crate::{
    ast::{self, Identifier},
    ice,
    module::{Module, ModuleTree},
    parser::meta::{Meta, MetaId},
    runtime::{
        ty::{TypeDescription, TypeRegistry},
        Runtime, RuntimeFunctionRef,
    },
};
use cycle::detect_type_cycles;
use scope::{
    DeclarationKind, ModuleScope, ResolvedName, ScopeRef, ScopeType,
    TypeOrStub,
};
use scoped_display::TypeDisplay;
use std::{any::TypeId, borrow::Borrow};
use types::{
    FunctionDefinition, MustBeSigned, Type, TypeDefinition, TypeName,
};

use self::{
    error::TypeError,
    types::{default_types, Function, Signature},
};

mod cycle;
pub(crate) mod error;
mod expr;
mod function;
pub mod info;
pub mod scope;
pub mod scoped_display;
#[cfg(test)]
mod tests;
pub mod types;
mod unionfind;

pub use expr::{PathValue, ResolvedPath};
use info::TypeInfo;

/// Holds the state for type checking
///
/// Most type checking steps are methods on this type.
#[derive(Clone)]
pub struct TypeChecker {
    /// The list of built-in functions, methods and static methods.
    functions: Vec<Function>,
    pub(crate) type_info: TypeInfo,
    match_counter: usize,
    if_else_counter: usize,
    while_counter: usize,
}

/// Result of type checking
pub type TypeResult<T> = Result<T, TypeError>;

pub fn typecheck(
    runtime: &Runtime,
    module_tree: &ModuleTree,
) -> TypeResult<TypeInfo> {
    let mut type_checker = runtime.type_checker.clone();
    type_checker.declare_context(runtime)?;
    type_checker.check_module_tree(module_tree)
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut checker = TypeChecker {
            functions: Vec::new(),
            type_info: TypeInfo::new(),
            match_counter: 0,
            if_else_counter: 0,
            while_counter: 0,
        };
        checker.declare_builtin_types().unwrap();
        checker
    }

    pub fn get_scope_graph(&self) -> &scope::ScopeGraph {
        &self.type_info.scope_graph
    }

    /// Perform type checking for a module tree (i.e. the entire program)
    pub fn check_module_tree(
        mut self,
        tree: &ModuleTree,
    ) -> Result<TypeInfo, TypeError> {
        let modules = self.declare_modules(tree)?;
        self.declare_imports(&modules)?;
        self.declare_types(&modules)?;

        detect_type_cycles(&self.type_info.types).map_err(|description| {
            self.error_simple(
                description,
                "type cycle detected",
                MetaId(0), // TODO: make a more useful error here with the recursive chain
            )
        })?;

        self.declare_functions(&modules)?;
        self.tree(&modules)?;
        self.force_filtermap_types(&modules);
        Ok(self.type_info)
    }

    pub(crate) fn get_scope_of(
        &self,
        scope: ScopeRef,
        ident: Identifier,
    ) -> Option<ScopeRef> {
        self.type_info
            .scope_graph
            .resolve_name(
                scope,
                &Meta {
                    node: ident,
                    id: MetaId(0),
                },
                false,
            )?
            .scope
    }

    fn declare_builtin_types(&mut self) -> TypeResult<()> {
        for (ident, doc, ty) in default_types() {
            let ident = Meta {
                node: ident,
                id: MetaId(0),
            };
            let name = ResolvedName {
                scope: ScopeRef::GLOBAL,
                ident: *ident,
            };
            self.type_info
                .scope_graph
                .insert_type(ScopeRef::GLOBAL, &ident, doc, ty.clone())
                .map_err(|id| self.error_declared_twice(&ident, id))?;

            if let TypeDefinition::Enum(_, variants) = &ty {
                let dec = self.type_info.scope_graph.get_declaration(name);
                let DeclarationKind::Type(TypeOrStub::Type(type_def)) =
                    dec.kind
                else {
                    ice!();
                };
                let scope = dec.scope.unwrap();
                for variant in variants {
                    self.type_info
                        .scope_graph
                        .insert_declaration(
                            scope,
                            &Meta {
                                node: variant.name,
                                id: MetaId(0),
                            },
                            DeclarationKind::Variant(
                                type_def.clone(),
                                variant.clone(),
                            ),
                            String::new(),
                            |_| false,
                        )
                        .unwrap();
                }
            }

            self.type_info.types.insert(name, ty);
        }
        Ok(())
    }

    pub(crate) fn declare_runtime_module(
        &mut self,
        parent: Option<ScopeRef>,
        ident: Identifier,
        doc: String,
    ) -> Result<ScopeRef, String> {
        let mod_scope = ModuleScope {
            name: ResolvedName {
                ident,
                scope: parent.unwrap_or(ScopeRef::GLOBAL),
            },
            parent_module: parent,
        };
        let mod_scope = self
            .type_info
            .scope_graph
            .wrap(ScopeRef::GLOBAL, ScopeType::Module(mod_scope));

        let scope = parent.unwrap_or(ScopeRef::GLOBAL);

        // TODO: Remove unwrap
        let ident = Meta {
            node: ident,
            id: MetaId(0),
        };

        self.type_info
            .scope_graph
            .insert_module(scope, &ident, doc, mod_scope)
            .map_err(|_| {
                format!("An item with the name `{ident}` already exists")
            })?;

        Ok(mod_scope)
    }

    pub(crate) fn declare_runtime_type(
        &mut self,
        scope: ScopeRef,
        ident: Identifier,
        type_id: TypeId,
        doc: String,
    ) -> Result<(), String> {
        let name = ResolvedName { scope, ident };
        let ident = Meta {
            node: ident,
            id: MetaId(0),
        };

        // Small edge case: the primitives are already in the typechecker, so we
        // skip them, but we should override the documentation.
        if let Some(other) =
            self.type_info.scope_graph.resolve_name(scope, &ident, true)
        {
            if let DeclarationKind::Type(TypeOrStub::Type(
                TypeDefinition::Primitive(_),
            )) = other.kind
            {
                let dec = self
                    .type_info
                    .scope_graph
                    .get_declaration_mut(other.name);
                dec.doc = doc;
                return Ok(());
            }
        }

        let ty = TypeDefinition::Runtime(name, type_id);
        self.type_info
            .scope_graph
            .insert_type(ScopeRef::GLOBAL, &ident, doc, ty.clone())
            .map_err(|_id| {
                format!("Item `{ident}` already exists in this scope")
            })?;

        self.type_info.types.insert(name, ty);
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn declare_runtime_function(
        &mut self,
        scope: ScopeRef,
        ident: Identifier,
        id: RuntimeFunctionRef,
        parameter_names: Vec<Identifier>,
        parameter_types: Vec<Type>,
        return_type: Type,
        doc: String,
        method: bool,
    ) -> Result<(), String> {
        let ty = Type::Function(
            parameter_types.clone(),
            Box::new(return_type.clone()),
        );

        // TODO: Figure out some what to make nice spans for built-in types
        let ident = Meta {
            node: ident,
            id: MetaId(0),
        };
        let def = FunctionDefinition::Runtime(id);

        // TODO: Remove unwraps here
        let name = if method {
            self.type_info
                .scope_graph
                .insert_method(scope, &ident, def, parameter_names, doc, &ty)
                .map_err(|_| "name is declared twice")?
        } else {
            self.type_info
                .scope_graph
                .insert_function(
                    scope,
                    &ident,
                    def,
                    parameter_names,
                    doc,
                    &ty,
                )
                .map_err(|_| "name is declared twice")?
        };

        let signature = Signature {
            parameter_types,
            return_type,
        };

        self.functions.push(Function::new(
            name,
            &[],
            signature.clone(),
            FunctionDefinition::Runtime(id),
        ));

        self.type_info
            .runtime_function_signatures
            .insert(id, signature);

        Ok(())
    }

    pub(crate) fn rust_type_to_roto_type(
        runtime: &Runtime,
        t: TypeId,
    ) -> Result<Type, String> {
        let ty = TypeRegistry::get(t).unwrap();

        if ty.type_id == TypeId::of::<()>() {
            return Ok(Type::Unit);
        }

        match ty.description {
            TypeDescription::Leaf => {
                let name = runtime
                    .get_runtime_type(ty.type_id)
                    .ok_or_else(|| {
                        format!("unregistered type: {}", ty.rust_name)
                    })?
                    .name();
                Ok(Type::Name(TypeName {
                    name,
                    arguments: Vec::new(),
                }))
            }
            TypeDescription::Option(t) => {
                Ok(Type::option(Self::rust_type_to_roto_type(runtime, t)?))
            }
            TypeDescription::Verdict(a, r) => Ok(Type::verdict(
                Self::rust_type_to_roto_type(runtime, a)?,
                Self::rust_type_to_roto_type(runtime, r)?,
            )),
            TypeDescription::Val(_) => {
                let name = runtime
                    .get_runtime_type(ty.type_id)
                    .ok_or_else(|| {
                        format!("unregistered type: {}", ty.rust_name)
                    })?
                    .name();
                Ok(Type::Name(TypeName {
                    name,
                    arguments: Vec::new(),
                }))
            }
        }
    }

    pub(crate) fn declare_runtime_constant(
        &mut self,
        scope: ScopeRef,
        ident: Identifier,
        ty: Type,
        doc: String,
    ) -> Result<(), String> {
        if self
            .type_info
            .scope_graph
            .insert_constant(
                scope,
                &Meta {
                    id: MetaId(0),
                    node: ident,
                },
                &ty,
                doc,
            )
            .is_err()
        {
            // TODO: Improve error message
            return Err("Name declared twice!".into());
        }
        Ok(())
    }

    pub(crate) fn declare_runtime_import(
        &mut self,
        scope: ScopeRef,
        name: ResolvedName,
    ) -> Result<(), String> {
        if self
            .type_info
            .scope_graph
            .insert_import(scope, MetaId(0), name)
            .is_err()
        {
            // TODO: Improve error message
            return Err("Name declared twice!".into());
        }
        Ok(())
    }

    fn declare_context(&mut self, runtime: &Runtime) -> TypeResult<()> {
        if let Some(ctx) = runtime.context() {
            for field in &ctx.fields {
                let name =
                    runtime.get_runtime_type(field.type_id).unwrap().name();
                self.insert_context(
                    Meta {
                        id: MetaId(0),
                        node: Identifier::from(field.name),
                    },
                    Type::Name(TypeName {
                        name,
                        arguments: Vec::new(),
                    }),
                    field.offset,
                )?;
            }
        }

        Ok(())
    }

    fn declare_modules<'a>(
        &mut self,
        tree: &'a ModuleTree,
    ) -> TypeResult<Vec<(ScopeRef, &'a Module)>> {
        let mut modules = Vec::<(ScopeRef, &'a Module)>::new();
        for m in &tree.modules {
            let Module {
                ident,
                ast,
                children: _,
                parent,
            } = m;
            let parent_module = parent.map(|p| modules[p.0].0);
            let mod_scope = ModuleScope {
                name: ResolvedName {
                    ident: **ident,
                    scope: parent_module.unwrap_or(ScopeRef::GLOBAL),
                },
                parent_module,
            };
            let scope = self
                .type_info
                .scope_graph
                .wrap(ScopeRef::GLOBAL, ScopeType::Module(mod_scope));

            if let Some(p) = parent_module {
                self.insert_module(p, ident, String::new(), scope)?;
            } else {
                self.insert_module(
                    ScopeRef::GLOBAL,
                    ident,
                    String::new(),
                    scope,
                )?;
            }

            for d in &ast.declarations {
                let (kind, ident) = match d {
                    ast::Declaration::Record(x) => (
                        DeclarationKind::Type(TypeOrStub::Stub {
                            num_params: 0,
                        }),
                        x.ident.clone(),
                    ),
                    ast::Declaration::Function(x) => {
                        (DeclarationKind::Function(None), x.ident.clone())
                    }
                    ast::Declaration::FilterMap(x) => {
                        (DeclarationKind::Function(None), x.ident.clone())
                    }
                    ast::Declaration::Import(_) => continue,
                    ast::Declaration::Test(_) => continue,
                };

                let res = self.type_info.scope_graph.insert_declaration(
                    scope,
                    &ident,
                    kind,
                    String::new(),
                    |_| false,
                );
                if let Err(e) = res {
                    return Err(self.error_declared_twice(&ident, e));
                }
            }
            modules.push((scope, m))
        }
        Ok(modules)
    }

    fn declare_imports(
        &mut self,
        modules: &[(ScopeRef, &Module)],
    ) -> TypeResult<()> {
        for &(scope, module) in modules {
            let mut paths = Vec::new();
            for expr in &module.ast.declarations {
                let ast::Declaration::Import(new_paths) = expr else {
                    continue;
                };
                for path in new_paths {
                    paths.push(path);
                }
            }
            self.imports(scope, &paths)?;
        }
        Ok(())
    }

    fn declare_types(
        &mut self,
        modules: &[(ScopeRef, &Module)],
    ) -> TypeResult<()> {
        for &(scope, module) in modules {
            for expr in &module.ast.declarations {
                match expr {
                    ast::Declaration::Function(_)
                    | ast::Declaration::FilterMap(_)
                    | ast::Declaration::Test(_)
                    | ast::Declaration::Import(_) => continue,
                    ast::Declaration::Record(
                        ast::RecordTypeDeclaration { ident, record_type },
                    ) => {
                        let name = ResolvedName {
                            scope,
                            ident: **ident,
                        };
                        let ty = TypeDefinition::Record(
                            TypeName {
                                name,
                                arguments: Vec::new(),
                            },
                            self.evaluate_record_type(scope, record_type)?,
                        );
                        self.type_info
                            .scope_graph
                            .insert_type(
                                scope,
                                ident,
                                String::new(),
                                ty.clone(),
                            )
                            .map_err(|e| {
                                self.error_declared_twice(ident, e)
                            })?;
                        let opt = self.type_info.types.insert(name, ty);
                        assert!(opt.is_none());
                    }
                }
            }
        }

        Ok(())
    }

    fn declare_functions(
        &mut self,
        modules: &[(ScopeRef, &Module)],
    ) -> TypeResult<()> {
        for &(scope, module) in modules {
            for expr in &module.ast.declarations {
                match expr {
                    ast::Declaration::Function(x) => {
                        let ty = self.function_type(scope, x)?;
                        self.insert_function(
                            scope,
                            x.ident.clone(),
                            FunctionDefinition::Roto,
                            x.params.0.iter().map(|(i, _)| i.node).collect(),
                            String::new(),
                            &ty,
                        )?;
                    }
                    ast::Declaration::FilterMap(x) => {
                        let ty = self.filter_map_type(scope, x)?;
                        self.insert_function(
                            scope,
                            x.ident.clone(),
                            FunctionDefinition::Roto,
                            x.params.0.iter().map(|(i, _)| i.node).collect(),
                            String::new(),
                            &ty,
                        )?;
                    }
                    ast::Declaration::Test(_) => continue,
                    ast::Declaration::Record(_) => continue,
                    ast::Declaration::Import(_) => continue,
                }
            }
        }
        Ok(())
    }

    fn tree(&mut self, modules: &[(ScopeRef, &Module)]) -> TypeResult<()> {
        for &(scope, module) in modules {
            for expr in &module.ast.declarations {
                match &expr {
                    ast::Declaration::FilterMap(f) => {
                        self.filter_map(scope, f)?;
                    }
                    ast::Declaration::Function(x) => {
                        self.function(scope, x)?;
                    }
                    ast::Declaration::Test(x) => {
                        self.test(scope, x)?;
                    }
                    _ => {}
                }
            }
        }

        Ok(())
    }

    fn force_filtermap_types(&mut self, modules: &[(ScopeRef, &Module)]) {
        for &(_, module) in modules {
            for expr in &module.ast.declarations {
                if let ast::Declaration::FilterMap(f) = &expr {
                    let ty = self.type_info.type_of(&f.ident);
                    let Type::Function(_, return_type) = &ty else {
                        ice!("filtermap should always have a function type")
                    };

                    let Type::Name(TypeName { name: _, arguments }) =
                        &**return_type
                    else {
                        ice!("return type of a filtermap should always be a verdict")
                    };
                    let [a, r] = &arguments[..] else {
                        ice!("return type of a filtermap should always be a verdict")
                    };

                    if let Type::Var(x) = self.resolve_type(a) {
                        self.unify(
                            &Type::Var(x),
                            &Type::unit(),
                            f.ident.id,
                            None,
                        )
                        .unwrap();
                    }
                    if let Type::Var(x) = self.resolve_type(r) {
                        self.unify(
                            &Type::Var(x),
                            &Type::unit(),
                            f.ident.id,
                            None,
                        )
                        .unwrap();
                    }
                }
            }
        }
    }

    fn imports(
        &mut self,
        scope: ScopeRef,
        paths: &[&Meta<ast::Path>],
    ) -> TypeResult<()> {
        // We want imports to work in any order and sometimes there are
        // dependencies between them. This means that we process them in a loop
        // where we exit either if we have no unresolved imports anymore or when
        // we can no longer make progress, in which case we error.
        let mut paths = paths.to_vec();
        loop {
            let last_len = paths.len();
            paths.retain(|p| self.import(scope, p).is_err());
            let new_len = paths.len();
            if new_len == 0 {
                return Ok(());
            }
            if new_len == last_len {
                for p in &paths {
                    self.import(scope, p)?;
                }
            }
        }
    }

    fn import(
        &mut self,
        scope: ScopeRef,
        path: &ast::Path,
    ) -> TypeResult<()> {
        let mut idents = path.idents.iter();
        let (ident, declaration) =
            self.resolve_module_part_of_path(scope, &mut idents)?;

        // This is a bit of an oversimplification. The
        // resolve_module_part_of_path should just give us the thing
        // to import, but we might expand import functionality to enum
        // constructors.
        if let Some(_ident) = idents.next() {
            return Err(self.error_expected_module(ident, declaration));
        }

        self.type_info
            .scope_graph
            .insert_import(scope, ident.id, declaration.name)
            .map_err(|old| self.error_declared_twice(ident, old))
    }

    /// Create a fresh variable in the unionfind structure
    fn fresh_var(&mut self) -> Type {
        self.type_info.unionfind.fresh(Type::Var)
    }

    /// Create a fresh integer variable in the unionfind structure
    fn fresh_int(&mut self) -> Type {
        self.type_info
            .unionfind
            .fresh(|n| Type::IntVar(n, types::MustBeSigned::No))
    }

    /// Create a fresh integer variable in the unionfind structure
    fn fresh_float(&mut self) -> Type {
        self.type_info.unionfind.fresh(Type::FloatVar)
    }

    /// Create a fresh record variable in the unionfind structure
    fn fresh_record(
        &mut self,
        fields: Vec<(Meta<Identifier>, Type)>,
    ) -> Type {
        let fields =
            fields.into_iter().map(|(s, t)| (s.clone(), t)).collect();
        self.type_info
            .unionfind
            .fresh(move |x| Type::RecordVar(x, fields))
    }

    /// Insert a context variable into the global scope
    fn insert_context(
        &mut self,
        k: Meta<Identifier>,
        ty: impl Borrow<Type>,
        offset: usize,
    ) -> TypeResult<()> {
        let ty = ty.borrow();
        match self.type_info.scope_graph.insert_context(&k, ty, offset) {
            Ok(name) => {
                self.type_info.resolved_names.insert(k.id, name);
                self.type_info.expr_types.insert(k.id, ty.clone());
                Ok(())
            }
            Err(()) => Err(self.error_declared_twice(&k, MetaId(0))),
        }
    }

    /// Insert a function name into the given scope
    fn insert_function(
        &mut self,
        scope: ScopeRef,
        k: Meta<Identifier>,
        definition: FunctionDefinition,
        parameter_names: Vec<Identifier>,
        doc: String,
        ty: impl Borrow<Type>,
    ) -> TypeResult<()> {
        let ty = ty.borrow();
        match self.type_info.scope_graph.insert_function(
            scope,
            &k,
            definition,
            parameter_names,
            doc,
            ty,
        ) {
            Ok(name) => {
                self.type_info.resolved_names.insert(k.id, name);
                self.type_info.expr_types.insert(k.id, ty.clone());
                Ok(())
            }
            Err(old) => Err(self.error_declared_twice(&k, old)),
        }
    }

    /// Insert a variable into the given scope
    fn insert_var(
        &mut self,
        scope: ScopeRef,
        k: Meta<Identifier>,
        ty: impl Borrow<Type>,
    ) -> TypeResult<()> {
        let ty = ty.borrow();
        match self.type_info.scope_graph.insert_var(scope, &k, ty) {
            Ok(name) => {
                self.type_info.resolved_names.insert(k.id, name);
                self.type_info.expr_types.insert(k.id, ty.clone());
                Ok(())
            }
            Err(old) => Err(self.error_declared_twice(&k, old)),
        }
    }

    /// Insert a variable into the given scope
    fn insert_module(
        &mut self,
        scope: ScopeRef,
        k: &Meta<Identifier>,
        doc: String,
        mod_scope: ScopeRef,
    ) -> TypeResult<()> {
        match self
            .type_info
            .scope_graph
            .insert_module(scope, k, doc, mod_scope)
        {
            Ok(()) => Ok(()),
            Err(old) => Err(self.error_declared_twice(k, old)),
        }
    }

    /// Unify two types
    ///
    /// This function tries to find the most general unification of two
    /// types. If they cannot be unified an error is generated.
    ///
    /// The types do not need to be resolved before this function.
    ///
    /// Note that this function modifies the union find structure. The changes
    /// it makes cannot be undone. This is a possible improvement for the
    /// future, so that we can attempt multiple unifications to find a correct
    /// one.
    fn unify(
        &mut self,
        a: &Type,
        b: &Type,
        span: MetaId,
        cause: Option<MetaId>,
    ) -> TypeResult<Type> {
        if let Some(ty) = self.unify_inner(a, b) {
            Ok(ty)
        } else {
            let a = self.resolve_type(a);
            let b = self.resolve_type(b);
            Err(self.error_mismatched_types(&a, &b, span, cause))
        }
    }

    fn unify_inner(&mut self, a: &Type, b: &Type) -> Option<Type> {
        use Type::*;
        let a = self.resolve_type(a);
        let b = self.resolve_type(b);
        Some(match (a, b) {
            // Evidently, if two types are identical, they trivially unify
            (a, b) if a == b => a,
            // Explicit type variables need to be replaced with fresh type
            // variable. If they appear here, something has gone wrong.
            (a @ ExplicitVar(_), b) => {
                ice!(
                    "Cannot unify explicit var: {}, {}",
                    a.display(&self.type_info),
                    b.display(&self.type_info),
                )
            }
            (a, b @ ExplicitVar(_)) => {
                ice!(
                    "Cannot unify explicit var: {}, {}",
                    a.display(&self.type_info),
                    b.display(&self.type_info),
                )
            }
            // The never type is special and unifies with anything
            (Never, x) | (x, Never) => x,
            (IntVar(a, a_signed), IntVar(b, b_signed)) => {
                self.unify_intvars(a, a_signed, b, b_signed)
            }
            (IntVar(b, s), Name(name)) | (Name(name), IntVar(b, s)) => {
                if !name.arguments.is_empty() {
                    return None;
                }
                let type_def = self.type_info.resolve_type_name(&name);

                let correct = if s == MustBeSigned::Yes {
                    type_def.is_signed_int()
                } else {
                    type_def.is_int()
                };

                if !correct {
                    return None;
                }
                self.type_info.unionfind.set(b, Name(name.clone()));
                Name(name)
            }
            (FloatVar(a), b @ FloatVar(_)) => {
                self.type_info.unionfind.set(a, b.clone());
                b.clone()
            }
            (FloatVar(b), Name(name)) | (Name(name), FloatVar(b)) => {
                if !name.arguments.is_empty() {
                    return None;
                }
                let type_def = self.type_info.resolve_type_name(&name);
                if !type_def.is_float() {
                    return None;
                }
                self.type_info.unionfind.set(b, Name(name.clone()));
                Name(name)
            }
            (Var(a), b) => {
                self.type_info.unionfind.set(a, b.clone());
                b.clone()
            }
            (a, Var(b)) => {
                self.type_info.unionfind.set(b, a.clone());
                a.clone()
            }
            (
                RecordVar(a_var, a_fields),
                ref b @ RecordVar(_, ref b_fields),
            ) => {
                self.unify_fields(&a_fields, b_fields)?;
                self.type_info.unionfind.set(a_var, b.clone());
                b.clone()
            }
            (RecordVar(a_var, a_fields), ref b @ Record(ref b_fields)) => {
                self.unify_fields(&a_fields, b_fields)?;
                self.type_info.unionfind.set(a_var, b.clone());
                b.clone()
            }
            (ref a @ Record(ref a_fields), RecordVar(b_var, b_fields)) => {
                self.unify_fields(a_fields, &b_fields)?;
                self.type_info.unionfind.set(b_var, a.clone());
                a.clone()
            }
            (RecordVar(var, fields), Name(name))
            | (Name(name), RecordVar(var, fields)) => {
                // TODO: Allow type parameters on records
                if !name.arguments.is_empty() {
                    return None;
                }
                let type_def = self.type_info.resolve_type_name(&name);
                let TypeDefinition::Record(name, named_fields) = type_def
                else {
                    return None;
                };
                self.unify_fields(&fields, &named_fields)?;
                self.type_info.unionfind.set(var, Name(name.clone()));
                Name(name)
            }
            // Type names unify if they have the same name and their arguments
            // can be unified.
            (Name(a), Name(b)) => {
                if a.name != b.name {
                    return None;
                }
                for (a_param, b_param) in a.arguments.iter().zip(&b.arguments)
                {
                    self.unify_inner(a_param, b_param)?;
                }
                Name(b)
            }
            // Anything else cannot be unified.
            (_a, _b) => {
                return None;
            }
        })
    }

    fn unify_intvars(
        &mut self,
        a: usize,
        a_signed: MustBeSigned,
        b: usize,
        b_signed: MustBeSigned,
    ) -> Type {
        // We have to ensure that `Yes` has priority over `No`. We map `a` to
        // `b` by default, so if `a` has `Yes` and `b` has `No` we need to map
        // `b` to `a` instead.
        if a_signed == MustBeSigned::Yes && b_signed == MustBeSigned::No {
            self.type_info.unionfind.set(b, Type::IntVar(a, a_signed));
            Type::IntVar(a, a_signed)
        } else {
            self.type_info.unionfind.set(a, Type::IntVar(b, b_signed));
            Type::IntVar(b, b_signed)
        }
    }

    fn unify_fields(
        &mut self,
        a_fields: &[(Meta<Identifier>, Type)],
        b_fields: &[(Meta<Identifier>, Type)],
    ) -> Option<Vec<(Meta<Identifier>, Type)>> {
        if a_fields.len() != b_fields.len() {
            return None;
        }

        let mut b_fields = b_fields.to_vec();
        let mut new_fields = Vec::new();
        for (name, a_ty) in a_fields {
            let idx =
                b_fields.iter().position(|(n, _)| n.node == name.node)?;
            let (_, b_ty) = b_fields.remove(idx);
            new_fields.push((name.clone(), self.unify_inner(a_ty, &b_ty)?))
        }

        Some(new_fields)
    }

    /// Resolve a type variable to a type.
    fn resolve_type(&mut self, t: &Type) -> Type {
        if let Type::Var(x)
        | Type::IntVar(x, _)
        | Type::FloatVar(x)
        | Type::RecordVar(x, _) = t
        {
            self.type_info.unionfind.find(*x).clone()
        } else {
            t.clone()
        }
    }

    /// Evaluate a type expression into a [`Type`]
    fn evaluate_type_expr(
        &self,
        scope: ScopeRef,
        ty: &ast::TypeExpr,
    ) -> TypeResult<Type> {
        Ok(match ty {
            ast::TypeExpr::Path(path, params) => {
                let params = match &params {
                    Some(params) => &params.node[..],
                    None => &[],
                };
                self.resolve_type_path(scope, path, params)?
            }
            ast::TypeExpr::Record(record_ty) => {
                Type::Record(self.evaluate_record_type(scope, record_ty)?)
            }
            ast::TypeExpr::Option(inner) => {
                let inner = self.evaluate_type_expr(scope, inner)?;
                Type::option(inner)
            }
            ast::TypeExpr::Never => Type::Never,
            ast::TypeExpr::Unit => Type::unit(),
        })
    }

    fn evaluate_record_type(
        &self,
        scope: ScopeRef,
        expr: &ast::RecordType,
    ) -> TypeResult<Vec<(Meta<Identifier>, Type)>> {
        let mut type_fields = Vec::new();

        for (ident, ty) in &expr.fields.node {
            let field_type = self.evaluate_type_expr(scope, ty)?;
            type_fields.push((ident, field_type))
        }

        let mut unspanned_type_fields = Vec::new();
        for field in &type_fields {
            let same_fields: Vec<_> = type_fields
                .iter()
                .filter_map(|(ident, _typ)| {
                    if ident.node == field.0.node {
                        Some(ident.id)
                    } else {
                        None
                    }
                })
                .collect();
            if same_fields.len() > 1 {
                let ident = field.0.as_str();
                return Err(self.error_duplicate_fields(ident, &same_fields));
            }
            unspanned_type_fields.push((field.0.clone(), field.1.clone()));
        }

        Ok(unspanned_type_fields)
    }
}
