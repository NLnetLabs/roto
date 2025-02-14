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

use crate::{
    ast::{self, Identifier},
    module::{Module, ModuleTree},
    parser::meta::{Meta, MetaId},
    runtime::{FunctionKind, Runtime, RuntimeFunction},
};
use cycle::detect_type_cycles;
use scope::{
    ModuleScope, ResolvedName, ScopeRef, ScopeType, StubDeclarationKind,
};
use std::{borrow::Borrow, collections::HashMap};
use types::{FunctionDefinition, Type};

use self::{
    error::TypeError,
    types::{default_types, Function, Signature},
};

mod cycle;
pub(crate) mod error;
mod expr;
mod filter_map;
pub mod info;
pub mod scope;
#[cfg(test)]
mod tests;
pub mod types;
mod unionfind;

pub use expr::{PathValue, ResolvedPath};
use info::TypeInfo;

pub struct TypeChecker {
    /// The list of built-in functions, methods and static methods.
    functions: Vec<Function>,
    type_info: TypeInfo,
    match_counter: usize,
    if_else_counter: usize,
}

pub type TypeResult<T> = Result<T, TypeError>;

pub fn typecheck(
    runtime: &Runtime,
    module_tree: &ModuleTree,
    pointer_bytes: u32,
) -> TypeResult<TypeInfo> {
    TypeChecker::check_module_tree(runtime, module_tree, pointer_bytes)
}

impl TypeChecker {
    /// Perform type checking for a module tree (i.e. the entire program)
    pub fn check_module_tree(
        runtime: &Runtime,
        tree: &ModuleTree,
        pointer_bytes: u32,
    ) -> Result<TypeInfo, TypeError> {
        let mut checker = TypeChecker {
            functions: Vec::new(),
            type_info: TypeInfo::new(pointer_bytes),
            match_counter: 0,
            if_else_counter: 0,
        };

        // Add all the stuff that's gonna be included in any script
        // We unwrap these because they really shouldn't fail. If
        // the built-in types would fail then it would be an internal
        // compiler error and if the runtime fails then the runtime is
        // malformed.
        checker.declare_builtin_types().unwrap();
        checker.declare_runtime_items(runtime).unwrap();

        let modules = checker.declare_modules(tree)?;
        checker.declare_imports(&modules)?;
        checker.declare_types(&modules)?;

        detect_type_cycles(&checker.type_info.types).map_err(
            |description| {
                checker.error_simple(
                    description,
                    "type cycle detected",
                    MetaId(0), // TODO: make a more useful error here with the recursive chain
                )
            },
        )?;

        checker.declare_functions(&modules)?;
        checker.tree(&modules)?;

        Ok(checker.type_info)
    }

    fn declare_builtin_types(&mut self) -> TypeResult<()> {
        for (ident, ty) in default_types() {
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
                .insert_type(ScopeRef::GLOBAL, &ident, ty.clone())
                .map_err(|id| self.error_declared_twice(&ident, id))?;
            self.type_info.types.insert(name, ty);
        }
        Ok(())
    }

    fn declare_runtime_items(&mut self, runtime: &Runtime) -> TypeResult<()> {
        self.declare_runtime_types(runtime)?;
        self.declare_runtime_functions(runtime)?;
        self.declare_constants(runtime)?;
        self.declare_context(runtime)?;
        Ok(())
    }

    fn declare_runtime_types(&mut self, runtime: &Runtime) -> TypeResult<()> {
        for ty in &runtime.runtime_types {
            let ident = Identifier::from(ty.name());
            let name = ResolvedName {
                scope: ScopeRef::GLOBAL,
                ident,
            };
            let ident = Meta {
                node: ident,
                id: MetaId(0),
            };

            // If the type is already in the scope graph, then it's a primitive type, we don't need
            // to insert it again
            if self
                .type_info
                .scope_graph
                .resolve_name(ScopeRef::GLOBAL, &ident, true)
                .is_none()
            {
                let ty = Type::BuiltIn(name, ty.type_id());
                self.type_info
                    .scope_graph
                    .insert_type(ScopeRef::GLOBAL, &ident, ty.clone())
                    .map_err(|id| self.error_declared_twice(&ident, id))?;
                self.type_info.types.insert(name, ty);
            }
        }
        Ok(())
    }

    fn declare_runtime_functions(
        &mut self,
        runtime: &Runtime,
    ) -> TypeResult<()> {
        // We need to know about all runtime methods, static methods and
        // functions when we start type checking. Therefore, we have to map
        // the runtime methods with TypeIds to function types.
        for func in &runtime.functions {
            let RuntimeFunction {
                name,
                description,
                kind,
                id: _,
                docstring: _,
                argument_names: _,
            } = func;

            let name = ResolvedName {
                scope: ScopeRef::GLOBAL,
                ident: name.into(),
            };

            let mut rust_parameters =
                description.parameter_types().iter().map(|ty| {
                    let ident = runtime.get_runtime_type(*ty).unwrap().name();
                    let name = ResolvedName {
                        scope: ScopeRef::GLOBAL,
                        ident: ident.into(),
                    };
                    Type::Name(name)
                });

            let return_type = rust_parameters.next().unwrap();

            let parameter_types: Vec<_> = rust_parameters.collect();

            let kind = match kind {
                FunctionKind::Free => types::FunctionKind::Free,
                FunctionKind::Method(id) => {
                    let ident = runtime.get_runtime_type(*id).unwrap().name();
                    let name = ResolvedName {
                        scope: ScopeRef::GLOBAL,
                        ident: ident.into(),
                    };
                    types::FunctionKind::Method(Type::Name(name))
                }
                FunctionKind::StaticMethod(id) => {
                    let ident = runtime.get_runtime_type(*id).unwrap().name();
                    let name = ResolvedName {
                        scope: ScopeRef::GLOBAL,
                        ident: ident.into(),
                    };
                    types::FunctionKind::StaticMethod(Type::Name(name))
                }
            };

            let ident = Meta {
                node: name.ident,
                id: MetaId(0),
            };

            // Free functions are put in the global namespace
            if let types::FunctionKind::Free = kind {
                self.type_info
                    .scope_graph
                    .insert_function(
                        ScopeRef::GLOBAL,
                        &ident,
                        FunctionDefinition::Runtime(func.get_ref()),
                        &Type::Function(
                            parameter_types.clone(),
                            Box::new(return_type.clone()),
                        ),
                    )
                    .unwrap();
            }

            self.functions.push(Function::new(
                kind,
                name,
                &[],
                parameter_types,
                return_type,
                FunctionDefinition::Runtime(func.get_ref()),
            ));
        }

        Ok(())
    }

    fn declare_constants(&mut self, runtime: &Runtime) -> TypeResult<()> {
        for (v, t) in runtime.iter_constants() {
            self.insert_global_constant(
                Meta {
                    id: MetaId(0),
                    node: v,
                },
                Type::Name(ResolvedName {
                    scope: ScopeRef::GLOBAL,
                    ident: runtime.get_runtime_type(t).unwrap().name().into(),
                }),
            )?
        }

        Ok(())
    }

    fn declare_context(&mut self, runtime: &Runtime) -> TypeResult<()> {
        if let Some(ctx) = &runtime.context {
            for field in &ctx.fields {
                let ty = runtime
                    .get_runtime_type(field.type_id)
                    .unwrap()
                    .name()
                    .into();
                let name = ResolvedName {
                    scope: ScopeRef::GLOBAL,
                    ident: ty,
                };
                self.insert_context(
                    Meta {
                        id: MetaId(0),
                        node: Identifier::from(field.name),
                    },
                    Type::Name(name),
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
                self.insert_module(p, ident, scope)?;
            } else {
                self.insert_module(ScopeRef::GLOBAL, ident, scope)?;
            }

            for d in &ast.declarations {
                let (kind, ident) = match d {
                    ast::Declaration::Record(x) => {
                        (StubDeclarationKind::Type, x.ident.clone())
                    }
                    ast::Declaration::Function(x) => {
                        (StubDeclarationKind::Function, x.ident.clone())
                    }
                    ast::Declaration::FilterMap(x) => {
                        (StubDeclarationKind::Function, x.ident.clone())
                    }
                    ast::Declaration::Import(_) => continue,
                    ast::Declaration::Test(_) => continue,
                };
                self.type_info.scope_graph.insert_stub(scope, &ident, kind);
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
                let ast::Declaration::Import(path) = expr else {
                    continue;
                };
                paths.push(path);
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
                        let ty = Type::NamedRecord(
                            name,
                            self.evaluate_record_type(
                                scope,
                                &record_type.key_values,
                            )?,
                        );
                        self.type_info
                            .scope_graph
                            .insert_type(scope, ident, ty.clone())
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
                            &ty,
                        )?;
                    }
                    ast::Declaration::FilterMap(x) => {
                        let ty = self.filter_map_type(scope, x)?;
                        self.insert_function(
                            scope,
                            x.ident.clone(),
                            FunctionDefinition::Roto,
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
    
    fn imports(&mut self, scope: ScopeRef, paths: &[&Meta<ast::Path>]) -> TypeResult<()> {
        // We want imports to work in any order and sometimes there are
        // dependencies between them. This means that we process them in a loop
        // where we exit either if we have no unresolved imports anymore or when
        // we can no longer make progress, in which case we error.
        let mut paths = paths.to_vec();
        loop {
            let last_len = paths.len();
            paths.retain(|p| {
                self.import(scope, p).is_err()
            });
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

    fn import(&mut self, scope: ScopeRef, path: &ast::Path) -> TypeResult<()> {
        let mut idents = path.idents.iter();
        let (ident, stub) = self.resolve_module_part_of_path(scope, &mut idents)?;

        // This is a bit of an oversimplification. The
        // resolve_module_part_of_path should just give us the thing
        // to import, but we might expand import functionality to enum
        // constructors.
        if let Some(_ident) = idents.next() {
            return Err(self.error_expected_module(ident, stub));
        }

        self.type_info.scope_graph.insert_import(scope, ident.id, stub.name).map_err(|old| self.error_declared_twice(ident, old))
    }

    /// Create a fresh variable in the unionfind structure
    fn fresh_var(&mut self) -> Type {
        self.type_info.unionfind.fresh(Type::Var)
    }

    /// Create a fresh integer variable in the unionfind structure
    fn fresh_int(&mut self) -> Type {
        self.type_info.unionfind.fresh(Type::IntVar)
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

    /// Check whether `a` is a subtype of `b`
    ///
    /// Currently, the only possible subtype relation is generated by
    /// type variables. For example, `[String]` is a subtype of `[T]` if
    /// `T` is a type variable.
    fn subtype_of(&mut self, a: &Type, b: &Type) -> bool {
        self.subtype_inner(a, b, &mut HashMap::new())
    }

    fn subtype_inner(
        &mut self,
        a: &Type,
        b: &Type,
        subs: &mut HashMap<usize, Type>,
    ) -> bool {
        let mut a = self.resolve_type(a);
        let b = self.resolve_type(b);

        loop {
            let Type::Var(v) = a else {
                break;
            };
            let Some(t) = subs.get(&v) else {
                break;
            };
            a = t.clone();
        }

        match (a, b) {
            (a, b) if a == b => true,
            (Type::Var(x), t) => {
                subs.insert(x, t);
                true
            }
            (Type::List(a), Type::List(b)) => {
                self.subtype_inner(&a, &b, subs)
            }
            (Type::NamedRecord(a_name, _), Type::NamedRecord(b_name, _)) => {
                a_name == b_name
            }
            (Type::Record(a_fields), Type::Record(b_fields)) => {
                self.subtype_fields(&a_fields, &b_fields, subs)
            }
            (
                Type::RecordVar(_, a_fields),
                Type::Record(b_fields)
                | Type::NamedRecord(_, b_fields)
                | Type::RecordVar(_, b_fields),
            ) => self.subtype_fields(&a_fields, &b_fields, subs),
            // TODO: Named record and other stuff
            _ => false,
        }
    }

    fn subtype_fields(
        &mut self,
        a_fields: &[(Meta<Identifier>, Type)],
        b_fields: &[(Meta<Identifier>, Type)],
        subs: &mut HashMap<usize, Type>,
    ) -> bool {
        if a_fields.len() != b_fields.len() {
            return false;
        }

        for (name, ty_a) in a_fields {
            let Some((_, ty_b)) = b_fields.iter().find(|(n, _)| n == name)
            else {
                return false;
            };
            if !self.subtype_inner(ty_a, ty_b, subs) {
                return false;
            }
        }
        true
    }

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

    fn insert_global_constant(
        &mut self,
        k: Meta<Identifier>,
        ty: impl Borrow<Type>,
    ) -> TypeResult<()> {
        let ty = ty.borrow();
        match self.type_info.scope_graph.insert_global_constant(&k, ty) {
            Ok(name) => {
                self.type_info.resolved_names.insert(k.id, name);
                self.type_info.expr_types.insert(k.id, ty.clone());
                Ok(())
            }
            Err(()) => Err(self.error_declared_twice(&k, MetaId(0))),
        }
    }

    fn insert_function(
        &mut self,
        scope: ScopeRef,
        k: Meta<Identifier>,
        definition: FunctionDefinition,
        ty: impl Borrow<Type>,
    ) -> TypeResult<()> {
        let ty = ty.borrow();
        match self
            .type_info
            .scope_graph
            .insert_function(scope, &k, definition, ty)
        {
            Ok(name) => {
                self.type_info.resolved_names.insert(k.id, name);
                self.type_info.expr_types.insert(k.id, ty.clone());
                Ok(())
            }
            Err(old) => Err(self.error_declared_twice(&k, old)),
        }
    }

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

    fn insert_module(
        &mut self,
        scope: ScopeRef,
        k: &Meta<Identifier>,
        mod_scope: ScopeRef,
    ) -> TypeResult<()> {
        match self
            .type_info
            .scope_graph
            .insert_module(scope, k, mod_scope)
        {
            Ok(()) => Ok(()),
            Err(old) => Err(self.error_declared_twice(k, old)),
        }
    }

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
            Err(self.error_mismatched_types(a, b, span, cause))
        }
    }

    fn unify_inner(&mut self, a: &Type, b: &Type) -> Option<Type> {
        use types::Primitive::*;
        use Type::*;
        let a = self.resolve_type(a);
        let b = self.resolve_type(b);

        Some(match (a, b) {
            // We never recurse into NamedRecords, so they are included here.
            (a, b) if a == b => a,
            (Never, x) | (x, Never) => x,
            (
                IntVar(a),
                b @ (Primitive(U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64)
                | IntVar(_)),
            ) => {
                self.type_info.unionfind.set(a, b.clone());
                b.clone()
            }
            (
                a @ Primitive(U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64),
                IntVar(b),
            ) => {
                self.type_info.unionfind.set(b, a.clone());
                a.clone()
            }
            (Var(a), b) => {
                self.type_info.unionfind.set(a, b.clone());
                b.clone()
            }
            (a, Var(b)) => {
                self.type_info.unionfind.set(b, a.clone());
                a.clone()
            }
            (List(a), List(b)) => List(Box::new(self.unify_inner(&a, &b)?)),
            (Verdict(a1, r1), Verdict(a2, r2)) => Verdict(
                Box::new(self.unify_inner(&a1, &a2)?),
                Box::new(self.unify_inner(&r1, &r2)?),
            ),
            (
                RecordVar(a_var, a_fields),
                ref b @ (RecordVar(_, ref b_fields)
                | Record(ref b_fields)
                | NamedRecord(_, ref b_fields)),
            ) => {
                self.unify_fields(&a_fields, b_fields)?;
                self.type_info.unionfind.set(a_var, b.clone());
                b.clone()
            }
            (
                ref a @ (Record(ref a_fields) | NamedRecord(_, ref a_fields)),
                RecordVar(b_var, b_fields),
            ) => {
                self.unify_fields(a_fields, &b_fields)?;
                self.type_info.unionfind.set(b_var, a.clone());
                a.clone()
            }
            (_a, _b) => {
                return None;
            }
        })
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

    /// Resolve type vars to a type.
    ///
    /// This procedure does not recurse into records and enums
    fn resolve_type(&mut self, t: &Type) -> Type {
        let mut t = t.clone();

        if let Type::Var(x) | Type::IntVar(x) | Type::RecordVar(x, _) = t {
            t = self.type_info.unionfind.find(x).clone();
        }

        if let Type::Name(x) = t {
            t = self.type_info.types[&x].clone();
        }

        t
    }

    /// Instantiate all type variables in a method
    ///
    /// Instantiation in this context means replacing the explicit type variables with
    /// fresh variables.
    fn instantiate_method(&mut self, method: &Function) -> Signature {
        // This is probably all quite slow, but we can figure out a more
        // efficient way later.
        let Function {
            name: _,
            vars,
            signature:
                Signature {
                    kind,
                    parameter_types,
                    return_type,
                },
            definition: _,
        } = method;

        let mut kind = kind.clone();
        let mut parameter_types = parameter_types.clone();
        let mut return_type = return_type.clone();

        for method_var in vars {
            let var = self.fresh_var();
            let f = |x: &Type| {
                x.substitute(&Type::ExplicitVar(*method_var), &var)
            };

            kind = match kind {
                types::FunctionKind::Free => types::FunctionKind::Free,
                types::FunctionKind::Method(ty) => {
                    types::FunctionKind::Method(f(&ty))
                }
                types::FunctionKind::StaticMethod(ty) => {
                    types::FunctionKind::StaticMethod(f(&ty))
                }
            };

            for ty in &mut parameter_types {
                *ty = f(ty);
            }

            return_type = f(&return_type);
        }

        Signature {
            kind,
            parameter_types,
            return_type,
        }
    }

    fn evaluate_record_type(
        &self,
        scope: ScopeRef,
        fields: &[(Meta<Identifier>, ast::RecordFieldType)],
    ) -> TypeResult<Vec<(Meta<Identifier>, Type)>> {
        let mut type_fields = Vec::new();

        for (ident, ty) in fields {
            let field_type = self.evaluate_field_type(scope, ty)?;
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

    fn evaluate_field_type(
        &self,
        scope: ScopeRef,
        ty: &ast::RecordFieldType,
    ) -> TypeResult<Type> {
        Ok(match ty {
            ast::RecordFieldType::Identifier(ident) => {
                // If the type for this is unknown, we insert None,
                // which signals that the type is mentioned but not
                // yet declared. The declaration will override it
                // with Some(...) if we encounter it later.
                let stub = self
                    .type_info
                    .scope_graph
                    .resolve_name(scope, ident, true)
                    .ok_or_else(|| self.error_not_defined(ident))?;
                if let StubDeclarationKind::Type = stub.kind {
                    Type::Name(stub.name)
                } else {
                    return Err(self.error_expected_type(ident, stub));
                }
            }
            ast::RecordFieldType::Record(fields) => Type::Record(
                self.evaluate_record_type(scope, &fields.key_values)?,
            ),
            ast::RecordFieldType::List(inner) => {
                let inner = self.evaluate_field_type(scope, &inner.node)?;
                Type::List(Box::new(inner))
            }
        })
    }
}
