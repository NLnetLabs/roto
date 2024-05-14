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
    runtime::{wrap::WrappedFunction, Runtime},
};
use scope::Scope;
use std::{
    borrow::Borrow,
    collections::{hash_map::Entry, HashMap},
};
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

/// The output of the type checker that is used for lowering
#[derive(Clone, Default)]
pub struct TypeInfo {
    /// The unionfind structure that maps type variables to types
    unionfind: UnionFind,
    /// Map from type names to types
    types: HashMap<String, Type>,
    /// The types we inferred for each Expr
    ///
    /// This might not be fully resolved yet.
    expr_types: HashMap<MetaId, Type>,
    /// The fully qualified (and hence unique) name for each identifier.
    fully_qualified_names: HashMap<MetaId, String>,
    /// The ids of all the `Expr::Access` nodes that should be interpreted
    /// as enum variant constructors.
    enum_variant_constructors: HashMap<MetaId, Type>,
    /// Builtin methods
    methods: HashMap<MetaId, WrappedFunction>,
    diverges: HashMap<MetaId, bool>,
}

impl TypeInfo {
    pub fn full_name(&self, x: impl Into<MetaId>) -> String {
        self.fully_qualified_names[&x.into()].clone()
    }

    pub fn type_of(&mut self, x: impl Into<MetaId>) -> Type {
        let ty = self.expr_types[&x.into()].clone();
        self.resolve(&ty)
    }

    pub fn diverges(&mut self, x: impl Into<MetaId>) -> bool {
        self.diverges[&x.into()]
    }

    pub fn offset_of(
        &mut self,
        record: &Type,
        field: &str,
        pointer_bytes: u32,
    ) -> (Type, u32) {
        let record = self.resolve(record);
        let (Type::Record(fields)
        | Type::RecordVar(_, fields)
        | Type::NamedRecord(_, fields)) = record
        else {
            panic!()
        };

        let mut offset = 0;
        for f in fields {
            if f.0 == field {
                return (f.1, offset);
            }
            offset += self.size_of(&f.1, pointer_bytes);
        }
        panic!("Field not found")
    }

    pub fn method(
        &mut self,
        x: impl Into<MetaId>,
    ) -> Option<&WrappedFunction> {
        self.methods.get(&x.into())
    }

    pub fn enum_variant_constructor(
        &self,
        x: impl Into<MetaId>,
    ) -> Option<&Type> {
        self.enum_variant_constructors.get(&x.into())
    }

    pub fn resolve(&mut self, t: &Type) -> Type {
        let mut t = t.clone();

        if let Type::Var(x) | Type::IntVar(x) | Type::RecordVar(x, _) = t {
            t = self.unionfind.find(x).clone();
        }

        if let Type::Name(x) = t {
            t = self.types[&x].clone();
        }

        t
    }

    pub fn size_of(&mut self, t: &Type, pointer_bytes: u32) -> u32 {
        let t = self.resolve(t);
        match t {
            // Never is zero-sized
            Type::Never => 0,
            // Int variables are inferred to u32
            Type::IntVar(_) => 4,
            // Records have the size of their fields
            Type::Record(fields)
            | Type::NamedRecord(_, fields)
            | Type::RecordVar(_, fields) => fields
                .iter()
                .map(|(_, t)| self.size_of(t, pointer_bytes))
                .sum(),
            Type::Primitive(p) => p.size(),
            Type::List(_)
            | Type::Table(_) => pointer_bytes,
            | Type::OutputStream(_) => pointer_bytes,
            | Type::Rib(_) => pointer_bytes,
            _ => 0,
        }
    }
}

pub struct TypeChecker<'r, 'methods> {
    runtime: &'r Runtime,
    /// The list of built-in methods.
    methods: &'methods [Method],
    /// The list of built-in static methods.
    static_methods: &'methods [Method],
    type_info: TypeInfo,
}

pub type TypeResult<T> = Result<T, TypeError>;

pub fn typecheck(
    runtime: &Runtime,
    tree: &ast::SyntaxTree,
) -> TypeResult<TypeInfo> {
    let methods = types::methods(runtime);
    let static_methods = types::static_methods();

    let mut type_checker = TypeChecker {
        methods: &methods,
        runtime,
        static_methods: &static_methods,
        type_info: TypeInfo::default(),
    };

    type_checker.check_syntax_tree(tree)?;

    // Compute the sizes of all types in the syntax tree
    Ok(type_checker.type_info)
}

enum MaybeDeclared {
    /// A declared type, where the span of the type declaration is
    /// available for user-defined types, but not for built-in types.
    Declared(Type, Option<MetaId>),
    /// An (as of yet) undeclared type with the spans of where it is
    /// referenced.
    Undeclared(MetaId),
}

impl<'r, 'methods> TypeChecker<'r, 'methods> {
    /// Perform type checking for a syntax tree
    pub fn check_syntax_tree(
        &mut self,
        tree: &ast::SyntaxTree,
    ) -> TypeResult<()> {
        // This map contains MaybeDeclared, where Undeclared represents a type that
        // is referenced, but not (yet) declared. At the end of all the type
        // declarations, we check whether any nones are left to determine
        // whether any types are unresolved.
        // The builtin types are added right away.
        let mut types: HashMap<String, MaybeDeclared> =
            default_types(self.runtime)
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
                    node: Identifier(v),
                },
                t,
            )?;
        }

        // We go over the expressions three times:
        //  1. Collect all type definitions
        //  2. Collect all function definitions
        //  3. Type check all function bodies
        for expr in &tree.expressions {
            match expr {
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
                _ => {}
            }
        }

        // Check for any undeclared types in the type declarations
        // self.types will then only contain data types with valid
        // type names.
        self.type_info.types = types
            .into_iter()
            .map(|(s, t)| match t {
                MaybeDeclared::Declared(t, _) => Ok((s, t)),
                MaybeDeclared::Undeclared(reference_span) => {
                    Err(error::undeclared_type(&Meta {
                        id: reference_span,
                        node: Identifier(s),
                    }))
                }
            })
            .collect::<Result<_, _>>()?;

        self.detect_type_cycles().map_err(|description| {
            error::simple(
                description,
                "type cycle detected",
                MetaId(0), // TODO: make a more useful error here with the recursive chain
            )
        })?;

        // Filter-maps are pretty generic: they do not have a fixed
        // output type at the moment. It's a mess. So we don't declare
        // them, which means that they cannot be called by anything else,
        // which honestly makes sense.
        for expr in &tree.expressions {
            match expr {
                ast::Declaration::Term(x) => {
                    let ty = self.term_type(x)?;
                    self.insert_var(&mut root_scope, &x.ident, &ty)?;
                    self.type_info.expr_types.insert(x.ident.id, ty.clone());
                }
                ast::Declaration::Action(x) => {
                    let ty = self.action_type(x)?;
                    self.insert_var(&mut root_scope, &x.ident, &ty)?;
                    self.type_info.expr_types.insert(x.ident.id, ty.clone());
                    self.type_info.full_name(&x.ident);
                }
                _ => {}
            }
        }

        for expr in &tree.expressions {
            match expr {
                ast::Declaration::FilterMap(f) => {
                    let ty = self.fresh_var();
                    self.insert_var(&mut root_scope, &f.ident, ty.clone())?;
                    let ty2 = self.filter_map(&root_scope, f)?;
                    self.unify(&ty, &ty2, f.ident.id, None)?;
                }
                ast::Declaration::Term(x) => {
                    self.term(&root_scope, x)?;
                }
                ast::Declaration::Action(x) => {
                    self.action(&root_scope, x)?;
                }
                _ => {}
            }
        }

        Ok(())
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
        fields: Vec<(&Meta<Identifier>, Type)>,
    ) -> Type {
        let fields = fields
            .into_iter()
            .map(|(s, t)| (s.to_string(), t))
            .collect();
        self.type_info
            .unionfind
            .fresh(move |x| Type::RecordVar(x, fields))
    }

    fn get_type(&self, type_name: impl AsRef<str>) -> Option<&Type> {
        self.type_info.types.get(type_name.as_ref())
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

    fn insert_var<'a>(
        &mut self,
        scope: &'a mut Scope,
        k: &Meta<Identifier>,
        t: impl Borrow<Type>,
    ) -> TypeResult<&'a mut Type> {
        let (name, t) = scope.insert_var(k, t)?;
        self.type_info.fully_qualified_names.insert(k.id, name);
        self.type_info.expr_types.insert(k.id, t.clone());
        Ok(t)
    }

    fn get_var<'a>(
        &mut self,
        scope: &'a Scope,
        k: &Meta<Identifier>,
    ) -> TypeResult<&'a Type> {
        let (name, t) = scope.get_var(k)?;
        self.type_info.fully_qualified_names.insert(k.id, name);
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
        a_fields: &[(String, Type)],
        b_fields: &[(String, Type)],
    ) -> Option<Vec<(String, Type)>> {
        if a_fields.len() != b_fields.len() {
            return None;
        }

        let mut b_fields = b_fields.to_vec();
        let mut new_fields = Vec::new();
        for (name, a_ty) in a_fields {
            let idx = b_fields.iter().position(|(n, _)| n == name)?;
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
    fn instantiate_method(&mut self, method: &Method) -> Arrow {
        // This is probably all quite slow, but we can figure out a more
        // efficient way later.
        let Method {
            receiver_type,
            name: _,
            vars,
            argument_types,
            return_type,
            function,
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

        Arrow {
            rec,
            args,
            ret,
            function: function.clone(),
        }
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

        for ident in self.type_info.types.keys() {
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
        self.visit(visited, &self.type_info.types[s])?;
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
            | Type::BuiltIn(_, _)
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
            Type::Verdict(t1, t2) => {
                self.visit(visited, t1)?;
                self.visit(visited, t2)
            }
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
            Type::Name(ident) => self.visit_name(visited, ident),
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
            let inner = evaluate_field_type(types, &inner.node)?;
            Type::List(Box::new(inner))
        }
    })
}
