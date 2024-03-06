//! Type checker for Roto scripts

use std::collections::{hash_map::Entry, HashMap, VecDeque};

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
    map: Vec<MaybeType>,
    /// Map from identifier to type
    variables: HashMap<String, MaybeType>,
    /// Map from type names to types
    types: HashMap<String, Type>,
}

type TypeResult<T> = Result<T, String>;

impl TypeChecker {
    fn new() -> Self {
        Self {
            map: Vec::new(),
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
                    let ty = self
                        .create_contains_type(&mut types, contain_ty, body);
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
                    let ty = self
                        .create_contains_type(&mut types, contain_ty, body);
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
                    let ty = self
                        .create_contains_type(&mut types, contain_ty, body);
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
                        self.evaluate_record_type(
                            &record_type.key_values,
                            &mut types,
                        ),
                    );
                    types.insert(ident.to_string(), Some(ty));
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

    fn create_contains_type(
        &self,
        types: &mut HashMap<String, Option<Type>>,
        contain_ty: ast::TypeIdentifier,
        body: ast::RibBody,
    ) -> Type {
        let ty = Type::NamedRecord(
            contain_ty.ident.to_string(),
            self.evaluate_record_type(&body.key_values, types),
        );
        types.insert(contain_ty.to_string(), Some(ty.clone()));
        ty
    }

    fn evaluate_record_type(
        &self,
        fields: &[ast::RibField],
        types: &mut HashMap<String, Option<Type>>,
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
                        field_type = Type::Record(self.evaluate_record_type(
                            &field.1.key_values,
                            types,
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
    /// This function works by starting a DFS on each node. Each node is
    /// assigned an index. If we visit a node we mark it as visited along
    /// with the index of the starting node. A cycle is found when we find
    /// a node which is already visited with **the current index**. If we
    /// find a node with a lower index, we know that that sub-graph has
    /// already been checked for cycles and we do not need to traverse it
    /// again.
    ///
    /// It is similar to [Tarjan's algorithm][Tarjan] for Strongly Connected
    /// Components.
    ///
    /// [Tarjan]: https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
    fn detect_type_cycles(&mut self) -> TypeResult<()> {
        let mut visited = HashMap::new();

        for (i, (ident, ty)) in self.types.iter().enumerate() {
            match visited.entry(ident) {
                Entry::Occupied(_) => {
                    // already visited to just continue
                    continue;
                }
                Entry::Vacant(x) => x.insert(i),
            };

            let mut queue = VecDeque::new();
            queue.push_back(ty);

            while let Some(ty) = queue.pop_front() {
                match ty {
                    Type::U32
                    | Type::U16
                    | Type::U8
                    | Type::String
                    | Type::Bool => {
                        // do nothing on primitive types
                        // no need to recurse into them.
                    }
                    Type::Table(t) | Type::OutputStream(t) | Type::Rib(t) => {
                        queue.push_front(t);
                    }
                    Type::NamedRecord(_, fields) | Type::Record(fields) => {
                        for (_, ty) in fields {
                            queue.push_front(ty);
                        }
                    }
                    Type::Name(new_ident) => {
                        match visited.entry(new_ident) {
                            Entry::Occupied(x) => {
                                // TODO: Make error message better
                                if *x.get() == i {
                                    return Err(format!(
                                        "cycle: {new_ident}"
                                    ));
                                }
                            }
                            Entry::Vacant(x) => {
                                x.insert(i);
                                queue.push_front(
                                    self.types.get(new_ident).unwrap(),
                                )
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }
}
