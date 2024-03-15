//! Type checker for Roto scripts

use crate::ast::{self, TypeIdentField};
use scope::Scope;
use std::collections::{hash_map::Entry, HashMap};
use types::Type;

use self::types::{default_types, Arrow, Method, Primitive};

mod filter_map;
mod expr;
mod scope;
#[cfg(test)]
mod tests;
mod types;

pub struct TypeChecker {
    counter: usize,
    /// Map from type var index to a type (or another type var)
    ///
    /// Since type vars are indices, we can get the type easily by indexing.
    /// This map is not retroactively updated; resolving a type might need
    /// multiple hops.
    type_var_map: HashMap<usize, Type>,
    /// Map from integer literals to types
    int_var_map: HashMap<usize, Type>,
    /// Map from anonymous records to named records
    record_var_map: HashMap<usize, Type>,
    /// Map from type names to types
    types: HashMap<String, Type>,
    methods: Vec<Method>,
    static_methods: Vec<Method>,
}

pub type TypeResult<T> = Result<T, String>;

impl TypeChecker {
    pub fn new() -> TypeChecker {
        Self {
            counter: 0,
            type_var_map: HashMap::new(),
            int_var_map: HashMap::new(),
            record_var_map: HashMap::new(),
            types: HashMap::new(),
            methods: types::methods(),
            static_methods: types::static_methods(),
        }
    }

    pub fn check(&mut self, tree: &ast::SyntaxTree) -> TypeResult<()> {
        let mut filter_maps = Vec::new();

        // This map contains Option<Type>, where None represnts a type that
        // is referenced, but not (yet) declared. At the end of all the type
        // declarations, we check whether any nones are left to determine
        // whether any types are unresolved.
        // The builtin types are added right away.
        let mut types: HashMap<String, Option<Type>> = default_types()
            .into_iter()
            .map(|(s, t)| (s.to_string(), Some(t.clone())))
            .collect();

        let mut root_scope = Scope::default();

        for (v, t) in types::globals() {
            root_scope.insert_var(v, t)?;
        }

        for expr in &tree.expressions {
            match expr {
                // We'll do all filter-maps after all type declarations
                ast::RootExpr::FilterMap(x) => filter_maps.push(x),
                ast::RootExpr::Rib(ast::Rib {
                    ident,
                    contain_ty,
                    body,
                }) => {
                    let ty =
                        create_contains_type(&mut types, contain_ty, body)?;
                    root_scope.insert_var(
                        ident.ident.to_string(),
                        Type::Rib(Box::new(ty)),
                    )?;
                }
                ast::RootExpr::Table(ast::Table {
                    ident,
                    contain_ty,
                    body,
                }) => {
                    let ty =
                        create_contains_type(&mut types, contain_ty, body)?;
                    root_scope.insert_var(
                        ident.ident.to_string(),
                        Type::Table(Box::new(ty)),
                    )?;
                }
                ast::RootExpr::OutputStream(ast::OutputStream {
                    ident,
                    contain_ty,
                    body,
                }) => {
                    let ty =
                        create_contains_type(&mut types, contain_ty, body)?;
                    root_scope.insert_var(
                        ident.ident.to_string(),
                        Type::OutputStream(Box::new(ty)),
                    )?;
                }
                ast::RootExpr::Ty(ast::RecordTypeAssignment {
                    ident,
                    record_type,
                }) => {
                    let ty = Type::NamedRecord(
                        ident.ident.to_string(),
                        evaluate_record_type(
                            &mut types,
                            &record_type.key_values,
                        )?,
                    );
                    store_type(&mut types, ident.ident.to_string(), ty)?;
                }
            }
        }

        // Check for any undeclared types in the type declarations
        // self.types will then only contain data types with valid
        // type names.
        self.types = types
            .into_iter()
            .map(|(s, t)| {
                let Some(t) = t else {
                    return Err(format!(
                        "Did not provide declaration for type {s}"
                    ));
                };

                Ok((s, t))
            })
            .collect::<Result<_, _>>()?;

        self.detect_type_cycles()?;

        let _filter_maps: Vec<_> = filter_maps
            .into_iter()
            .map(|f| self.filter_map(&root_scope, f))
            .collect::<Result<_, _>>()?;

        Ok(())
    }

    fn fresh_var(&mut self) -> Type {
        let i = self.counter;
        self.counter += 1;
        Type::Var(i)
    }

    fn fresh_int(&mut self) -> Type {
        let i = self.counter;
        self.counter += 1;
        Type::IntVar(i)
    }

    fn fresh_record(&mut self) -> usize {
        let i = self.counter;
        self.counter += 1;
        i
    }

    fn subtype_of<'a>(&'a mut self, a: &'a Type, b: &'a Type) -> bool {
        self.subtype_inner(a, b, &mut HashMap::new())
    }

    fn subtype_inner<'a>(
        &'a mut self,
        a: &'a Type,
        b: &'a Type,
        subs: &mut HashMap<usize, Type>,
    ) -> bool {
        let mut a = self.resolve_type(a).clone();
        let b = self.resolve_type(b).clone();

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
                self.subtype_fields(a_fields, b_fields, subs)
            }
            (
                Type::RecordVar(_, a_fields),
                Type::Record(b_fields)
                | Type::NamedRecord(_, b_fields)
                | Type::RecordVar(_, b_fields),
            ) => self.subtype_fields(a_fields, b_fields, subs),
            // TODO: Named record and other stuff
            _ => false,
        }
    }

    fn subtype_fields(
        &mut self,
        a_fields: Vec<(String, Type)>,
        b_fields: Vec<(String, Type)>,
        subs: &mut HashMap<usize, Type>,
    ) -> bool {
        if a_fields.len() != b_fields.len() {
            false;
        }

        for (name, ty_a) in a_fields {
            let Some((_, ty_b)) = b_fields.iter().find(|(n, _)| n == &name)
            else {
                return false;
            };
            if !self.subtype_inner(&ty_a, ty_b, subs) {
                return false;
            }
        }
        return true;
    }

    fn unify<'a>(&'a mut self, a: &'a Type, b: &'a Type) -> TypeResult<Type> {
        let a = self.resolve_type(a).clone();
        let b = self.resolve_type(b).clone();

        Ok(match (a, b) {
            // We never recurse into NamedRecords, so they are included here.
            (a, b) if a == b => a.clone(),
            (
                Type::IntVar(a),
                b @ (Type::Primitive(
                    Primitive::U8 | Primitive::U16 | Primitive::U32,
                )
                | Type::IntVar(_)),
            ) => {
                self.int_var_map.insert(a, b.clone());
                b.clone()
            }
            (
                a @ Type::Primitive(
                    Primitive::U8 | Primitive::U16 | Primitive::U32,
                ),
                Type::IntVar(b),
            ) => {
                self.int_var_map.insert(b, a.clone());
                a.clone()
            }
            (Type::Var(a), b) => {
                // prefer binding from a -> b
                self.type_var_map.insert(a, b.clone());
                b.clone()
            }
            (a, Type::Var(b)) => {
                self.type_var_map.insert(b, a.clone());
                a.clone()
            }
            (Type::Table(a), Type::Table(b)) => {
                Type::Table(Box::new(self.unify(&a, &b)?))
            }
            (Type::OutputStream(a), Type::OutputStream(b)) => {
                Type::OutputStream(Box::new(self.unify(&a, &b)?))
            }
            (Type::Rib(a), Type::Rib(b)) => {
                Type::Rib(Box::new(self.unify(&a, &b)?))
            }
            (Type::List(a), Type::List(b)) => {
                Type::List(Box::new(self.unify(&a, &b)?))
            }
            (
                Type::RecordVar(a_var, a_fields),
                ref b @ (Type::RecordVar(_, ref b_fields)
                | Type::Record(ref b_fields)
                | Type::NamedRecord(_, ref b_fields)),
            ) => {
                self.unify_fields(a_fields, b_fields.clone())?;
                self.record_var_map.insert(a_var, b.clone());
                b.clone()
            }
            (
                ref a @ (Type::Record(ref a_fields)
                | Type::NamedRecord(_, ref a_fields)),
                Type::RecordVar(b_var, b_fields),
            ) => {
                self.unify_fields(a_fields.clone(), b_fields)?;
                self.record_var_map.insert(b_var, a.clone());
                a.clone()
            }
            (a, b) => {
                return Err(format!("cannot unify types {a:?} and {b:?}"))
            }
        })
    }

    fn unify_fields(
        &mut self,
        a_fields: Vec<(String, Type)>,
        b_fields: Vec<(String, Type)>,
    ) -> TypeResult<Vec<(String, Type)>> {
        if a_fields.len() != b_fields.len() {
            return Err(format!(
                "cannot unify types {:?} and {:?}",
                Type::Record(a_fields),
                Type::Record(b_fields),
            ));
        }

        let mut b_fields = b_fields.clone();
        let mut new_fields = Vec::new();
        for (name, a_ty) in a_fields {
            let Some(idx) = b_fields.iter().position(|(n, _)| n == &name)
            else {
                return Err(format!(
                    "Type {:?} does not have a field {name}.",
                    Type::Record(b_fields)
                ));
            };
            let (_, b_ty) = b_fields.remove(idx);
            new_fields.push((name.clone(), self.unify(&a_ty, &b_ty)?))
        }
        Ok(new_fields)
    }

    /// Resolve type vars to a type.
    ///
    /// This procedure is not recursive.
    fn resolve_type<'a>(&'a self, mut t: &'a Type) -> &'a Type {
        loop {
            match t {
                Type::Var(x) => {
                    if let Some(new_t) = self.type_var_map.get(x) {
                        t = new_t;
                    } else {
                        return t;
                    }
                }
                Type::IntVar(x) => {
                    if let Some(new_t) = self.int_var_map.get(x) {
                        t = new_t;
                    } else {
                        return t;
                    }
                }
                Type::RecordVar(x, _) => {
                    if let Some(new_t) = self.record_var_map.get(x) {
                        t = new_t;
                    } else {
                        return t;
                    }
                }
                Type::Name(x) => {
                    t = &self.types[x];
                }
                t => return t,
            }
        }
    }

    // This is probably all quite slow, but we can figure out a more efficient
    // way later.
    fn instantiate_method(&mut self, method: &Method) -> Arrow {
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
    /// To detect cycles, we do a DFS topological sort.
    fn detect_type_cycles(&self) -> TypeResult<()> {
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
    ) -> TypeResult<()> {
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
    ) -> TypeResult<()> {
        match ty {
            Type::Var(_)
            | Type::IntVar(_)
            | Type::ExplicitVar(_)
            | Type::RecordVar(_, _) => {
                Err("there should be no unresolved type variables left"
                    .into())
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
    types: &mut HashMap<String, Option<Type>>,
    k: String,
    v: Type,
) -> TypeResult<()> {
    match types.entry(k) {
        Entry::Occupied(mut entry) => {
            if entry.get().is_some() {
                return Err(format!("Declared type {} twice", entry.key()));
            }
            entry.insert(Some(v));
        }
        Entry::Vacant(entry) => {
            entry.insert(Some(v));
        }
    };
    Ok(())
}

fn create_contains_type(
    types: &mut HashMap<String, Option<Type>>,
    contain_ty: &ast::TypeIdentifier,
    body: &ast::RibBody,
) -> TypeResult<Type> {
    let ty = Type::NamedRecord(
        contain_ty.ident.to_string(),
        evaluate_record_type(types, &body.key_values)?,
    );
    store_type(types, contain_ty.to_string(), ty.clone())?;
    Ok(ty)
}

fn evaluate_record_type(
    types: &mut HashMap<String, Option<Type>>,
    fields: &[ast::RibField],
) -> TypeResult<Vec<(String, Type)>> {
    let mut type_fields = Vec::new();

    for field in fields {
        let field_ident;
        let field_type;

        match field {
            ast::RibField::PrimitiveField(TypeIdentField {
                field_name,
                ty,
            }) => {
                field_ident = field_name.ident.to_string();

                // If the type for this is unknown, we insert None,
                // which signals that the type is mentioned but not
                // yet declared. The declaration will override it
                // with Some(...) if we encounter it later.
                types.entry(ty.ident.to_string()).or_insert(None);
                field_type = Type::Name(ty.ident.to_string());
            }
            ast::RibField::RecordField(field) => {
                field_ident = field.0.ident.to_string();
                field_type = Type::Record(evaluate_record_type(
                    types,
                    &field.1.key_values,
                )?);
            }
            ast::RibField::ListField(field) => {
                field_ident = field.0.ident.to_string();
                field_type = Type::List(Box::new(Type::Name(
                    field.1.inner_type.to_string(),
                )));
            }
        }

        for (existing_ident, _) in &type_fields {
            if &field_ident == existing_ident {
                return Err(format!("The field {field_ident} occurs multiple times in a single record"));
            }
        }
        type_fields.push((field_ident, field_type))
    }
    Ok(type_fields)
}
