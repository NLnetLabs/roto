//! Type checker for Roto scripts

use std::collections::{hash_map::Entry, HashMap};

use crate::ast::{self, TypeIdentField};

#[cfg(test)]
mod tests;
mod typed;

const BUILT_IN_TYPES: &'static [(&'static str, Type)] = &[
    ("u32", Type::U32),
    ("u16", Type::U16),
    ("u8", Type::U8),
    ("bool", Type::Bool),
    ("string", Type::String),
];

enum MaybeType {
    Type(Type),
    Var(usize),
}

// This should grow to the already existing TypeDef
#[derive(Clone)]
enum Type {
    U32,
    U16,
    U8,
    String,
    Bool,
    Table(Box<Type>),
    OutputStream(Box<Type>),
    Rib(Box<Type>),
    Record(Vec<(String, Type)>),
    NamedRecord(String, Vec<(String, Type)>),
    Name(String),
}

struct TypeChecker {
    /// Map from type var index to a type (or another type var)
    ///
    /// Since type vars are indices, we can get the type easily by indexing.
    /// This map is not retroactively updated; resolving a type might need
    /// multiple hops.
    _map: Vec<MaybeType>,
    /// Map from identifier to type
    variables: HashMap<String, MaybeType>,
    /// Map from type names to types
    types: HashMap<String, Type>,
}

type TypeResult<T> = Result<T, String>;

impl TypeChecker {
    #[allow(dead_code)]
    fn new() -> Self {
        Self {
            _map: Vec::new(),
            variables: HashMap::new(),
            types: HashMap::new(),
        }
    }

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
                    self.variables.insert(
                        ident.ident.to_string(),
                        MaybeType::Type(Type::Rib(Box::new(ty))),
                    );
                }
                ast::RootExpr::Table(ast::Table {
                    ident,
                    contain_ty,
                    body,
                }) => {
                    let ty =
                        create_contains_type(&mut types, contain_ty, body)?;
                    self.variables.insert(
                        ident.ident.to_string(),
                        MaybeType::Type(Type::Table(Box::new(ty))),
                    );
                }
                ast::RootExpr::OutputStream(ast::OutputStream {
                    ident,
                    contain_ty,
                    body,
                }) => {
                    let ty =
                        create_contains_type(&mut types, contain_ty, body)?;
                    self.variables.insert(
                        ident.ident.to_string(),
                        MaybeType::Type(Type::OutputStream(Box::new(ty))),
                    );
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
                        ),
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

        Ok(typed::SyntaxTree {
            filter_maps: Vec::new(),
        })
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

    fn visit_name<'a>(&'a self, visited: &mut HashMap<&'a str, bool>, s: &'a str) -> TypeResult<()> {
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
            Type::U32 | Type::U16 | Type::U8 | Type::String | Type::Bool => {
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
            Type::Name(ident) => {
                self.visit_name(visited, &ident)
            }
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
        evaluate_record_type(types, &body.key_values),
    );
    store_type(types, contain_ty.to_string(), ty.clone())?;
    Ok(ty)
}

fn evaluate_record_type(
    types: &mut HashMap<String, Option<Type>>,
    fields: &[ast::RibField],
) -> Vec<(String, Type)> {
    fields
        .iter()
        .map(|field| {
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
                    ));
                }
                ast::RibField::ListField(field) => {
                    let _field_ident = field.0.ident.to_string();
                    todo!()
                }
            }
            (field_ident, field_type)
        })
        .collect()
}
