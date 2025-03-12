//! Cycle detection for Roto types
//!
//! Roto types are not allowed to be recursive. See [`detect_type_cycles`]
//! for more information.

use super::{
    scope::ResolvedName,
    types::{EnumVariant, Type, TypeDefinition},
};
use std::collections::HashMap;

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
pub fn detect_type_cycles(
    types: &HashMap<ResolvedName, TypeDefinition>,
) -> Result<(), String> {
    let mut visited = HashMap::new();

    for name in types.keys() {
        visit_name(types, &mut visited, *name)?;
    }

    Ok(())
}

/// Visit a type name and its subtypes while traversing all types
fn visit_name(
    types: &HashMap<ResolvedName, TypeDefinition>,
    visited: &mut HashMap<ResolvedName, bool>,
    name: ResolvedName,
) -> Result<(), String> {
    match visited.get(&name) {
        Some(false) => return Err("cycle detected!".into()),
        Some(true) => return Ok(()),
        None => {}
    };

    visited.insert(name, false);
    match &types[&name] {
        TypeDefinition::Enum(_, variants) => {
            for variant in variants {
                let EnumVariant { name: _, fields } = variant;
                for field_ty in fields {
                    visit(types, visited, field_ty)?;
                }
            }
        }
        TypeDefinition::Record(_, fields) => {
            for (_, field_ty) in fields {
                visit(types, visited, field_ty)?;
            }
        }
        TypeDefinition::Runtime(_, _) => {}
        TypeDefinition::Primitive(_) => {}
    }
    visited.insert(name, true);

    Ok(())
}

/// Visit a type name and its subtypes while traversing all types
fn visit<'a>(
    types: &'a HashMap<ResolvedName, TypeDefinition>,
    visited: &mut HashMap<ResolvedName, bool>,
    ty: &'a Type,
) -> Result<(), String> {
    match ty {
        Type::Var(_) | Type::IntVar(_) | Type::RecordVar(_, _) => {
            Err("there should be no unresolved type variables left".into())
        }
        Type::Never => {
            Err("never should not appear in a type declaration".into())
        }
        Type::Function(_, _) => Ok(()),
        Type::ExplicitVar(_) => Ok(()),
        Type::Record(fields) => {
            for (_, ty) in fields {
                visit(types, visited, ty)?;
            }
            Ok(())
        }
        Type::Name(ident) => visit_name(types, visited, ident.name),
    }
}
