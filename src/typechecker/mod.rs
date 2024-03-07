//! Type checker for Roto scripts

use crate::ast::{self, TypeIdentField};
use scope::Scope;
use std::collections::{hash_map::Entry, HashMap};
use ty::Type;

mod filter_map;
mod scope;
#[cfg(test)]
mod tests;
mod ty;
mod typed;

const BUILT_IN_TYPES: &'static [(&'static str, Type)] = &[
    ("u32", Type::U32),
    ("u16", Type::U16),
    ("u8", Type::U8),
    ("bool", Type::Bool),
    ("string", Type::String),
];

#[derive(Default)]
struct TypeChecker {
    counter: usize,
    /// Map from type var index to a type (or another type var)
    ///
    /// Since type vars are indices, we can get the type easily by indexing.
    /// This map is not retroactively updated; resolving a type might need
    /// multiple hops.
    type_var_map: HashMap<usize, Type>,
    /// Map from integer literals to types
    int_var_map: HashMap<usize, Type>,
    /// Map from type names to types
    types: HashMap<String, Type>,
}

type TypeResult<T> = Result<T, String>;

impl TypeChecker {
    // This allow just reduces the noise while I'm still writing this code
    #[allow(dead_code)]
    fn check(
        &mut self,
        tree: ast::SyntaxTree,
    ) -> TypeResult<typed::SyntaxTree> {
        let mut filter_maps = Vec::new();

        // This map contains Option<Type>, where None represnts a type that
        // is referenced, but not (yet) declared. At the end of all the type
        // declarations, we check whether any nones are left to determine
        // whether any types are unresolved.
        // The builtin types are added right away.
        let mut types: HashMap<String, Option<Type>> = BUILT_IN_TYPES
            .into_iter()
            .map(|(s, t)| (s.to_string(), Some(t.clone())))
            .collect();

        let mut root_scope = Scope::default();

        for expr in tree.expressions {
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

        let filter_maps = filter_maps
            .into_iter()
            .map(|f| self.filter_map(&root_scope, *f))
            .collect::<Result<_, _>>()?;

        Ok(typed::SyntaxTree { filter_maps })
    }

    fn _fresh_var(&mut self) -> Type {
        let i = self.counter;
        self.counter += 1;
        Type::Var(i)
    }

    fn fresh_int(&mut self) -> Type {
        let i = self.counter;
        self.counter += 1;
        Type::IntVar(i)
    }

    fn unify<'a>(&'a mut self, a: &'a Type, b: &'a Type) -> TypeResult<Type> {
        let a = self.resolve_type(a).clone();
        let b = self.resolve_type(b).clone();

        Ok(match (a, b) {
            // We never recurse into NamedRecords, so they are included here.
            (a, b) if a == b => a.clone(),
            (
                Type::IntVar(a),
                b @ (Type::U8 | Type::U16 | Type::U32 | Type::IntVar(_)),
            ) => {
                self.int_var_map.insert(a, b.clone());
                b.clone()
            }
            (
                a @ (Type::U8 | Type::U16 | Type::U32),
                Type::IntVar(b)
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
            (Type::Record(a_fields), Type::Record(b_fields)) => {
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
                    let Some(idx) =
                        b_fields.iter().position(|(n, _)| n == &name)
                    else {
                        return Err(format!(
                            "Type {:?} does not have a field {name}.",
                            Type::Record(b_fields)
                        ));
                    };
                    let (_, b_ty) = b_fields.remove(idx);
                    new_fields.push((name.clone(), self.unify(&a_ty, &b_ty)?))
                }

                Type::Record(new_fields)
            }
            (a, b) => {
                return Err(format!("cannot unify types {a:?} and {b:?}"))
            }
        })
    }

    /// Resolve type vars to a type.
    ///
    /// This procedure is not recursive.
    fn resolve_type<'a>(&'a self, mut t: &'a Type) -> &'a Type {
        while let Type::Var(x) = t {
            if let Some(new_t) = self.type_var_map.get(x) {
                t = new_t;
            } else {
                return t;
            }
        }

        while let Type::IntVar(x) = t {
            if let Some(new_t) = self.int_var_map.get(x) {
                t = new_t;
            } else {
                return t;
            }
        }

        while let Type::Name(x) = t {
            t = &self.types[x];
        }

        t
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
            Type::Var(_) | Type::IntVar(_) => {
                Err("there should be no unresolved type variables left"
                    .into())
            }
            Type::Prefix
            | Type::PrefixLength
            | Type::AsNumber
            | Type::IpAddress
            | Type::U32
            | Type::U16
            | Type::U8
            | Type::String
            | Type::Bool => {
                // do nothing on primitive types
                // no need to recurse into them.
                Ok(())
            }
            Type::Table(t) | Type::OutputStream(t) | Type::Rib(t) => {
                self.visit(visited, t)
            }
            Type::NamedRecord(_, fields) | Type::Record(fields) => {
                for (_, ty) in fields {
                    self.visit(visited, ty)?;
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
    contain_ty: ast::TypeIdentifier,
    body: ast::RibBody,
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
                let _field_ident = field.0.ident.to_string();
                todo!()
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
