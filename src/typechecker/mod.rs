//! Type checker for Roto scripts
//!
//! This type checker performs simplified Hindley-Milner type inference.
//! The simplification is that we only have one situation in which general
//! type variables are generated: method instantiation. Otherwise, we do
//! not have to deal with polymorphism at all. However, we might still
//! extend the type system later to accodomate for that.
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
};
use scope::Scope;
use std::collections::{hash_map::Entry, HashMap};
use types::Type;

use self::{
    error::TypeError,
    types::{default_types, Arrow, Method},
    unionfind::UnionFind,
};

pub(crate) mod error;
mod expr;
mod filter_map;
mod scope;
#[cfg(test)]
mod tests;
pub mod types;
mod unionfind;

pub struct TypeChecker<'methods> {
    /// The unionfind structure that maps type variables to types
    unionfind: UnionFind,
    /// Map from type names to types
    types: HashMap<String, Type>,
    /// The list of built-in methods.
    methods: &'methods [Method],
    /// The list of built-in static methods.
    static_methods: &'methods [Method],
}

pub type TypeResult<T> = Result<T, TypeError>;

pub fn typecheck(tree: &ast::SyntaxTree) -> TypeResult<()> {
    let methods = types::methods();
    let static_methods = types::static_methods();

    let mut type_checker = TypeChecker {
        unionfind: UnionFind::new(),
        types: HashMap::new(),
        methods: &methods,
        static_methods: &static_methods,
    };

    type_checker.check_syntax_tree(tree)
}

enum MaybeDeclared {
    /// A declared type, where the span of the type declaration is
    /// available for user-defined types, but not for built-in types.
    Declared(Type, Option<MetaId>),
    /// An (as of yet) undeclared type with the spans of where it is
    /// referenced.
    Undeclared(MetaId),
}

impl<'methods> TypeChecker<'methods> {
    /// Perform type checking for a syntax tree
    pub fn check_syntax_tree(
        &mut self,
        tree: &ast::SyntaxTree,
    ) -> TypeResult<()> {
        // This map contains Option<Type>, where None represnts a type that
        // is referenced, but not (yet) declared. At the end of all the type
        // declarations, we check whether any nones are left to determine
        // whether any types are unresolved.
        // The builtin types are added right away.
        let mut types: HashMap<String, MaybeDeclared> = default_types()
            .into_iter()
            .map(|(s, t)| {
                (s.to_string(), MaybeDeclared::Declared(t.clone(), None))
            })
            .collect();

        let mut root_scope = Scope::default();

        for (v, t) in types::globals() {
            root_scope.insert_var(
                &Meta {
                    id: MetaId(0),
                    node: Identifier(v.into()),
                },
                t,
            )?;
        }

        let mut filter_maps = Vec::new();
        for expr in &tree.expressions {
            match expr {
                // We'll do all filter-maps after all type declarations.
                // This guarantees that all types have been declared once
                // we get to the filter-maps.
                ast::Declaration::FilterMap(x) => filter_maps.push(x),
                ast::Declaration::Rib(ast::Rib {
                    ident,
                    contain_ty,
                    body,
                }) => {
                    let ty =
                        create_contains_type(&mut types, contain_ty, body)?;
                    root_scope.insert_var(ident, Type::Rib(Box::new(ty)))?;
                }
                ast::Declaration::Table(ast::Table {
                    ident,
                    contain_ty,
                    body,
                }) => {
                    let ty =
                        create_contains_type(&mut types, contain_ty, body)?;
                    root_scope
                        .insert_var(ident, Type::Table(Box::new(ty)))?;
                }
                ast::Declaration::OutputStream(ast::OutputStream {
                    ident,
                    contain_ty,
                    body,
                }) => {
                    let ty =
                        create_contains_type(&mut types, contain_ty, body)?;
                    root_scope.insert_var(
                        ident,
                        Type::OutputStream(Box::new(ty)),
                    )?;
                }
                ast::Declaration::Record(ast::RecordTypeDeclaration {
                    ident,
                    record_type,
                }) => {
                    let ty = Type::NamedRecord(
                        ident.0.clone(),
                        evaluate_record_type(
                            &mut types,
                            &record_type.key_values,
                        )?,
                    );
                    store_type(&mut types, ident, ty)?;
                }
            }
        }

        // Check for any undeclared types in the type declarations
        // self.types will then only contain data types with valid
        // type names.
        self.types = types
            .into_iter()
            .map(|(s, t)| match t {
                MaybeDeclared::Declared(t, _) => Ok((s, t)),
                MaybeDeclared::Undeclared(reference_span) => {
                    Err(error::undeclared_type(&Meta {
                        id: reference_span,
                        node: Identifier(s.into()),
                    }))
                }
            })
            .collect::<Result<_, _>>()?;

        self.detect_type_cycles().map_err(|description| {
            error::simple(
                &description,
                "type cycle detected",
                MetaId(0), // TODO: make a more useful error here with the recursive chain
            )
        })?;

        for f in filter_maps {
            self.filter_map(&root_scope, f)?;
        }

        Ok(())
    }

    /// Create a fresh variable in the unionfind structure
    fn fresh_var(&mut self) -> Type {
        self.unionfind.fresh(Type::Var)
    }

    /// Create a fresh integer variable in the unionfind structure
    fn fresh_int(&mut self) -> Type {
        self.unionfind.fresh(Type::IntVar)
    }

    /// Create a fresh record variable in the unionfind structure
    fn fresh_record(
        &mut self,
        fields: Vec<(&Meta<Identifier>, Type)>,
    ) -> Type {
        let fields = fields
            .into_iter()
            .map(|(s, t)| (s.to_string(), t))
            .collect();
        self.unionfind.fresh(move |x| Type::RecordVar(x, fields))
    }

    fn get_type(&self, type_name: impl AsRef<str>) -> Option<&Type> {
        self.types.get(&type_name.as_ref().to_string())
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
        a_fields: &[(String, Type)],
        b_fields: &[(String, Type)],
        subs: &mut HashMap<usize, Type>,
    ) -> bool {
        if a_fields.len() != b_fields.len() {
            false;
        }

        for (name, ty_a) in a_fields {
            let Some((_, ty_b)) = b_fields.iter().find(|(n, _)| n == name)
            else {
                return false;
            };
            if !self.subtype_inner(&ty_a, ty_b, subs) {
                return false;
            }
        }
        return true;
    }

    fn unify(
        &mut self,
        a: &Type,
        b: &Type,
        span: MetaId,
        cause: Option<MetaId>,
    ) -> TypeResult<Type> {
        let a = self.resolve_type(a);
        let b = self.resolve_type(b);

        if let Some(ty) = self.unify_inner(&a, &b) {
            Ok(ty)
        } else {
            Err(error::mismatched_types(a, b, span, cause))
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
            (IntVar(a), b @ (Primitive(U8 | U16 | U32) | IntVar(_))) => {
                self.unionfind.set(a, b.clone());
                b.clone()
            }
            (a @ Primitive(U8 | U16 | U32), IntVar(b)) => {
                self.unionfind.set(b, a.clone());
                a.clone()
            }
            (Var(a), b) => {
                self.unionfind.set(a, b.clone());
                b.clone()
            }
            (a, Var(b)) => {
                self.unionfind.set(b, a.clone());
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
            (
                RecordVar(a_var, a_fields),
                ref b @ (RecordVar(_, ref b_fields)
                | Record(ref b_fields)
                | NamedRecord(_, ref b_fields)),
            ) => {
                self.unify_fields(&a_fields, b_fields)?;
                self.unionfind.set(a_var, b.clone());
                b.clone()
            }
            (
                ref a @ (Record(ref a_fields) | NamedRecord(_, ref a_fields)),
                RecordVar(b_var, b_fields),
            ) => {
                self.unify_fields(a_fields, &b_fields)?;
                self.unionfind.set(b_var, a.clone());
                a.clone()
            }
            (_a, _b) => {
                return None;
            }
        })
    }

    fn unify_fields(
        &mut self,
        a_fields: &[(String, Type)],
        b_fields: &[(String, Type)],
    ) -> Option<Vec<(String, Type)>> {
        if a_fields.len() != b_fields.len() {
            return None;
        }

        let mut b_fields = b_fields.to_vec();
        let mut new_fields = Vec::new();
        for (name, a_ty) in a_fields {
            let Some(idx) = b_fields.iter().position(|(n, _)| n == name)
            else {
                return None;
            };
            let (_, b_ty) = b_fields.remove(idx);
            new_fields.push((name.clone(), self.unify_inner(&a_ty, &b_ty)?))
        }

        Some(new_fields)
    }

    /// Resolve type vars to a type.
    ///
    /// This procedure does not recurse into records and enums
    fn resolve_type(&mut self, t: &Type) -> Type {
        let mut t = t.clone();

        if let Type::Var(x) | Type::IntVar(x) | Type::RecordVar(x, _) = t {
            t = self.unionfind.find(x).clone();
        }

        if let Type::Name(x) = t {
            t = self.types[&x].clone();
        }

        t
    }

    /// Instantiate all type variables in a method
    ///
    /// Instantiation in this context means replacing the explicit type variables with
    /// fresh variables.
    fn instantiate_method(&mut self, method: &Method) -> Arrow {
        // This is probably all quite slow, but we can figure out a more
        // efficient way later.
        let Method {
            receiver_type,
            name: _,
            vars,
            argument_types,
            return_type,
        } = method;

        let mut rec = receiver_type.clone();
        let mut args = argument_types.clone();
        let mut ret = return_type.clone();

        for method_var in vars {
            let var = self.fresh_var();
            let f =
                |x: &Type| x.substitute(&Type::ExplicitVar(method_var), &var);

            rec = f(&rec);
            for a in &mut args {
                *a = f(a);
            }
            ret = f(&ret);
        }

        Arrow { rec, args, ret }
    }

    /// Return an error if there is a cycles in the type declarations
    ///
    /// The simplest case of a cycle os a recursive type:
    ///
    /// ```roto
    /// type A { a: A }
    /// ```
    ///
    /// Another simple example of a cycle are mutually recursive types:
    ///
    /// ```roto
    /// type A { b: B }
    /// type B { a: A }
    /// ```
    ///
    /// To detect cycles, we do a DFS topological sort. The strange part
    /// of this  procedure that we only care about named types and their
    /// relation. So we only record the names of the types that we have
    /// visited.
    ///
    /// This algorithm is the Depth-first search algorithm described at
    /// <https://en.wikipedia.org/wiki/Topological_sorting>, where `false`
    /// is a temporary mark and `true` is a permanent mark.
    fn detect_type_cycles(&self) -> Result<(), String> {
        let mut visited = HashMap::new();

        for ident in self.types.keys() {
            self.visit_name(&mut visited, ident)?;
        }

        Ok(())
    }

    fn visit_name<'a>(
        &'a self,
        visited: &mut HashMap<&'a str, bool>,
        s: &'a str,
    ) -> Result<(), String> {
        match visited.get(s) {
            Some(false) => return Err(format!("cycle detected on {s}!")),
            Some(true) => return Ok(()),
            None => {}
        };

        visited.insert(s, false);
        self.visit(visited, &self.types[s])?;
        visited.insert(s, true);

        Ok(())
    }

    fn visit<'a>(
        &'a self,
        visited: &mut HashMap<&'a str, bool>,
        ty: &'a Type,
    ) -> Result<(), String> {
        match ty {
            Type::Var(_)
            | Type::IntVar(_)
            | Type::ExplicitVar(_)
            | Type::RecordVar(_, _) => {
                Err("there should be no unresolved type variables left"
                    .into())
            }
            Type::Never => {
                Err("never should not appear in a type declaration".into())
            }
            Type::Primitive(_)
            | Type::Term(_)
            | Type::Action(_)
            | Type::Filter(_)
            | Type::FilterMap(_) => {
                // do nothing on primitive types
                // no need to recurse into them.
                Ok(())
            }
            Type::Table(t)
            | Type::OutputStream(t)
            | Type::Rib(t)
            | Type::List(t) => self.visit(visited, t),
            Type::NamedRecord(_, fields) | Type::Record(fields) => {
                for (_, ty) in fields {
                    self.visit(visited, ty)?;
                }
                Ok(())
            }
            Type::Enum(_, variants) => {
                for (_, ty) in variants {
                    if let Some(ty) = ty {
                        self.visit(visited, ty)?;
                    }
                }
                Ok(())
            }
            Type::Name(ident) => self.visit_name(visited, &ident),
        }
    }
}

fn store_type(
    types: &mut HashMap<String, MaybeDeclared>,
    k: &Meta<ast::Identifier>,
    v: Type,
) -> TypeResult<()> {
    let k_string = k.node.to_string();
    match types.entry(k_string) {
        Entry::Occupied(mut entry) => {
            if let MaybeDeclared::Declared(_, existing_span) = entry.get() {
                return Err(match existing_span {
                    Some(existing_span) => {
                        error::declared_twice(k, *existing_span)
                    }
                    None => error::tried_to_overwrite_builtin(k),
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
    types: &mut HashMap<String, MaybeDeclared>,
    contain_ty: &Meta<ast::Identifier>,
    body: &ast::RibBody,
) -> TypeResult<Type> {
    let ty = Type::NamedRecord(
        contain_ty.0.to_string(),
        evaluate_record_type(types, &body.key_values)?,
    );
    store_type(types, contain_ty, ty.clone())?;
    Ok(ty)
}

fn evaluate_record_type(
    types: &mut HashMap<String, MaybeDeclared>,
    fields: &[(Meta<Identifier>, ast::RibFieldType)],
) -> TypeResult<Vec<(String, Type)>> {
    // We store the spans temporarily to be able to create nice a
    let mut type_fields = Vec::new();

    for (ident, ty) in fields {
        let field_type = evaluate_field_type(types, ty)?;
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
            return Err(error::duplicate_fields(
                field.0.as_ref(),
                &same_fields,
            ));
        }
        unspanned_type_fields
            .push((field.0.node.to_string(), field.1.clone()));
    }

    Ok(unspanned_type_fields)
}

fn evaluate_field_type(
    types: &mut HashMap<String, MaybeDeclared>,
    ty: &ast::RibFieldType,
) -> TypeResult<Type> {
    Ok(match ty {
        ast::RibFieldType::Identifier(ty) => {
            // If the type for this is unknown, we insert None,
            // which signals that the type is mentioned but not
            // yet declared. The declaration will override it
            // with Some(...) if we encounter it later.
            types
                .entry(ty.as_ref().to_string())
                .or_insert(MaybeDeclared::Undeclared(ty.id));
            Type::Name(ty.0.clone())
        }
        ast::RibFieldType::Record(fields) => {
            Type::Record(evaluate_record_type(types, &fields.key_values)?)
        }
        ast::RibFieldType::List(inner) => {
            let inner = evaluate_field_type(types, &*inner.node)?;
            Type::List(Box::new(inner))
        }
    })
}
