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
    parser::meta::{Meta, MetaId},
    runtime::{FunctionKind, Runtime, RuntimeFunction},
};
use cycle::detect_type_cycles;
use scope::{ScopeGraph, ScopeRef};
use std::{
    borrow::Borrow,
    collections::{hash_map::Entry, HashMap},
};
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

use info::TypeInfo;

pub struct TypeChecker<'s> {
    /// The list of built-in functions, methods and static methods.
    functions: Vec<Function>,
    type_info: TypeInfo,
    scope_graph: &'s mut ScopeGraph,
    match_counter: usize,
}

pub type TypeResult<T> = Result<T, TypeError>;

pub fn typecheck(
    runtime: &Runtime,
    scope_graph: &mut ScopeGraph,
    tree: &ast::SyntaxTree,
    pointer_bytes: u32,
) -> TypeResult<TypeInfo> {
    TypeChecker::check_syntax_tree(runtime, scope_graph, tree, pointer_bytes)
}

enum MaybeDeclared {
    /// A declared type, where the span of the type declaration is
    /// available for user-defined types, but not for built-in types.
    Declared(Type, Option<MetaId>),
    /// An (as of yet) undeclared type with the spans of where it is
    /// referenced.
    Undeclared(MetaId),
}

impl TypeChecker<'_> {
    /// Perform type checking for a syntax tree
    pub fn check_syntax_tree(
        runtime: &Runtime,
        scope_graph: &mut ScopeGraph,
        tree: &ast::SyntaxTree,
        pointer_bytes: u32,
    ) -> TypeResult<TypeInfo> {
        // This map contains MaybeDeclared, where Undeclared represents a type that
        // is referenced, but not (yet) declared. At the end of all the type
        // declarations, we check whether any nones are left to determine
        // whether any types are unresolved.
        // The builtin types are added right away.
        let mut types: HashMap<Identifier, MaybeDeclared> =
            default_types(runtime)
                .into_iter()
                .map(|(s, t)| (s, MaybeDeclared::Declared(t.clone(), None)))
                .rev()
                .collect();

        let mut checker = TypeChecker {
            functions: Vec::new(),
            type_info: TypeInfo::new(pointer_bytes),
            scope_graph,
            match_counter: 0,
        };

        let root_scope = checker.scope_graph.root();

        for (v, t) in types::globals() {
            checker.insert_var(
                root_scope,
                Meta {
                    id: MetaId(0),
                    node: v,
                },
                t,
            )?;
        }

        // We go over the expressions three times:
        //  1. Collect all type definitions
        //  2. Collect all function definitions
        //  3. Type check all function bodies
        for expr in &tree.declarations {
            match expr {
                ast::Declaration::Rib(ast::Rib {
                    ident,
                    contain_ty,
                    body,
                }) => {
                    let ty = checker
                        .create_contains_type(&mut types, contain_ty, body)?;
                    checker.insert_var(
                        root_scope,
                        ident.clone(),
                        Type::Rib(Box::new(ty)),
                    )?;
                }
                ast::Declaration::Table(ast::Table {
                    ident,
                    contain_ty,
                    body,
                }) => {
                    let ty = checker
                        .create_contains_type(&mut types, contain_ty, body)?;
                    checker.insert_var(
                        root_scope,
                        ident.clone(),
                        Type::Table(Box::new(ty)),
                    )?;
                }
                ast::Declaration::OutputStream(ast::OutputStream {
                    ident,
                    contain_ty,
                    body,
                }) => {
                    let ty = checker
                        .create_contains_type(&mut types, contain_ty, body)?;
                    checker.insert_var(
                        root_scope,
                        ident.clone(),
                        Type::OutputStream(Box::new(ty)),
                    )?;
                }
                ast::Declaration::Record(ast::RecordTypeDeclaration {
                    ident,
                    record_type,
                }) => {
                    let ty = Type::NamedRecord(
                        ident.node,
                        checker.evaluate_record_type(
                            &mut types,
                            &record_type.key_values,
                        )?,
                    );
                    checker.store_type(&mut types, ident, ty)?;
                }
                _ => {}
            }
        }

        for (name, ty) in types {
            match ty {
                MaybeDeclared::Declared(ty, _) => {
                    checker.type_info.add_type(name, ty)
                }
                MaybeDeclared::Undeclared(reference_span) => {
                    return Err(checker.error_undeclared_type(&Meta {
                        id: reference_span,
                        node: name,
                    }));
                }
            }
        }

        detect_type_cycles(&checker.type_info.types).map_err(
            |description| {
                checker.error_simple(
                    description,
                    "type cycle detected",
                    MetaId(0), // TODO: make a more useful error here with the recursive chain
                )
            },
        )?;

        // We need to know about all runtime methods, static methods and
        // functions when we start type checking. Therefore, we have to map
        // the runtime methods with TypeIds to function types.
        for func in &runtime.functions {
            let RuntimeFunction {
                name,
                description,
                kind,
            } = func;

            let mut rust_parameters =
                description.parameter_types().iter().map(|ty| {
                    let name = &runtime.get_runtime_type(*ty).unwrap().name;
                    let name = Identifier::from(name);
                    Type::Name(name)
                });

            let return_type = rust_parameters.next().unwrap();
            let parameter_types: Vec<_> = rust_parameters.collect();

            let kind = match kind {
                FunctionKind::Free => types::FunctionKind::Free,
                FunctionKind::Method(id) => {
                    let name = &runtime.get_runtime_type(*id).unwrap().name;
                    let name = Identifier::from(name);
                    types::FunctionKind::Method(Type::Name(name))
                }
                FunctionKind::StaticMethod(id) => {
                    let name = &runtime.get_runtime_type(*id).unwrap().name;
                    let name = Identifier::from(name);
                    types::FunctionKind::StaticMethod(Type::Name(name))
                }
            };

            let name = Identifier::from(name);

            checker.functions.push(Function::new(
                kind,
                name,
                &[],
                parameter_types,
                return_type,
                FunctionDefinition::Runtime(func.clone()),
            ));
        }

        // Filter-maps are pretty generic: they do not have a fixed
        // output type at the moment. It's a mess. So we don't declare
        // them, which means that they cannot be called by anything else,
        // which honestly makes sense. But that means that we start by
        // only declaring functions in the root scope.
        for expr in &tree.declarations {
            if let ast::Declaration::Function(x) = expr {
                let ty = checker.function_type(x)?;
                checker.insert_var(root_scope, x.ident.clone(), &ty)?;
                checker.type_info.expr_types.insert(x.ident.id, ty.clone());
            }
        }

        for expr in &tree.declarations {
            match expr {
                ast::Declaration::FilterMap(f) => {
                    let ty = checker.fresh_var();
                    checker.insert_var(
                        root_scope,
                        f.ident.clone(),
                        ty.clone(),
                    )?;
                    let ty2 = checker.filter_map(root_scope, f)?;
                    checker.unify(&ty, &ty2, f.ident.id, None)?;
                }
                ast::Declaration::Function(x) => {
                    checker.function(root_scope, x)?;
                }
                _ => {}
            }
        }

        Ok(checker.type_info)
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

    fn get_type(&self, type_name: Identifier) -> Option<&Type> {
        self.type_info.types.get(&type_name)
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
            (Type::Table(a), Type::Table(b))
            | (Type::OutputStream(a), Type::OutputStream(b))
            | (Type::List(a), Type::List(b))
            | (Type::Rib(a), Type::Rib(b)) => {
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

    fn insert_var(
        &mut self,
        scope: ScopeRef,
        k: Meta<Identifier>,
        t: impl Borrow<Type>,
    ) -> TypeResult<()> {
        match self.scope_graph.insert_var(scope, &k, t) {
            Ok((name, t)) => {
                self.type_info.resolved_names.insert(k.id, name);
                self.type_info.expr_types.insert(k.id, t.clone());
                Ok(())
            }
            Err(old) => Err(self.error_declared_twice(&k, old)),
        }
    }

    fn get_var<'a>(
        &'a mut self,
        scope: ScopeRef,
        k: &Meta<Identifier>,
    ) -> TypeResult<&'a Type> {
        let Some((name, t)) = self.scope_graph.get_var(scope, k) else {
            return Err(self.error_not_defined(k));
        };
        self.type_info.resolved_names.insert(k.id, name);
        self.type_info.expr_types.insert(k.id, t.clone());
        Ok(t)
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
                b @ (Primitive(U8 | U16 | U32 | I8 | I16 | I32) | IntVar(_)),
            ) => {
                self.type_info.unionfind.set(a, b.clone());
                b.clone()
            }
            (a @ Primitive(U8 | U16 | U32 | I8 | I16 | I32), IntVar(b)) => {
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
            (Table(a), Table(b)) => {
                Table(Box::new(self.unify_inner(&a, &b)?))
            }
            (OutputStream(a), OutputStream(b)) => {
                OutputStream(Box::new(self.unify_inner(&a, &b)?))
            }
            (Rib(a), Rib(b)) => Rib(Box::new(self.unify_inner(&a, &b)?)),
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

    fn store_type(
        &self,
        types: &mut HashMap<Identifier, MaybeDeclared>,
        k: &Meta<ast::Identifier>,
        v: Type,
    ) -> TypeResult<()> {
        match types.entry(k.node) {
            Entry::Occupied(mut entry) => {
                if let MaybeDeclared::Declared(_, existing_span) = entry.get()
                {
                    return Err(match existing_span {
                        Some(existing_span) => {
                            self.error_declared_twice(k, *existing_span)
                        }
                        None => self.error_tried_to_overwrite_builtin(k),
                    });
                }
                entry.insert(MaybeDeclared::Declared(v, Some(k.id)));
            }
            Entry::Vacant(entry) => {
                entry.insert(MaybeDeclared::Declared(v, Some(k.id)));
            }
        };
        Ok(())
    }

    fn create_contains_type(
        &self,
        types: &mut HashMap<Identifier, MaybeDeclared>,
        contain_ty: &Meta<ast::Identifier>,
        body: &ast::RibBody,
    ) -> TypeResult<Type> {
        let ty = Type::NamedRecord(
            contain_ty.node,
            self.evaluate_record_type(types, &body.key_values)?,
        );
        self.store_type(types, contain_ty, ty.clone())?;
        Ok(ty)
    }

    fn evaluate_record_type(
        &self,
        types: &mut HashMap<Identifier, MaybeDeclared>,
        fields: &[(Meta<Identifier>, ast::RibFieldType)],
    ) -> TypeResult<Vec<(Meta<Identifier>, Type)>> {
        // We store the spans temporarily to be able to create nice a
        let mut type_fields = Vec::new();

        for (ident, ty) in fields {
            let field_type = self.evaluate_field_type(types, ty)?;
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
        types: &mut HashMap<Identifier, MaybeDeclared>,
        ty: &ast::RibFieldType,
    ) -> TypeResult<Type> {
        Ok(match ty {
            ast::RibFieldType::Identifier(ty) => {
                // If the type for this is unknown, we insert None,
                // which signals that the type is mentioned but not
                // yet declared. The declaration will override it
                // with Some(...) if we encounter it later.
                types
                    .entry(ty.node)
                    .or_insert(MaybeDeclared::Undeclared(ty.id));
                Type::Name(ty.node)
            }
            ast::RibFieldType::Record(fields) => Type::Record(
                self.evaluate_record_type(types, &fields.key_values)?,
            ),
            ast::RibFieldType::List(inner) => {
                let inner = self.evaluate_field_type(types, &inner.node)?;
                Type::List(Box::new(inner))
            }
        })
    }
}
