//! Cycle detection for Roto types
//! 
//! Roto types are not allowed to be recursive. See [`detect_type_cycles`]
//! for more information.

use std::collections::HashMap;

use crate::ast::Identifier;

use super::types::Type;

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
pub fn detect_type_cycles(types: &HashMap<Identifier, Type>) -> Result<(), String> {
    let mut visited = HashMap::new();

    for ident in types.keys() {
        visit_name(types, &mut visited, *ident)?;
    }

    Ok(())
}

fn visit_name(
    types: &HashMap<Identifier, Type>,
    visited: &mut HashMap<Identifier, bool>,
    s: Identifier,
) -> Result<(), String> {
    match visited.get(&s) {
        Some(false) => return Err("cycle detected!".into()),
        Some(true) => return Ok(()),
        None => {}
    };

    visited.insert(s, false);
    visit(types, visited, &types[&s])?;
    visited.insert(s, true);

    Ok(())
}

fn visit<'a>(
    types: &'a HashMap<Identifier, Type>,
    visited: &mut HashMap<Identifier, bool>,
    ty: &'a Type,
) -> Result<(), String> {
    match ty {
        Type::Var(_)
        | Type::IntVar(_)
        | Type::ExplicitVar(_)
        | Type::RecordVar(_, _) => {
            Err("there should be no unresolved type variables left".into())
        }
        Type::Never => {
            Err("never should not appear in a type declaration".into())
        }
        Type::Primitive(_)
        | Type::BuiltIn(_, _)
        | Type::Function(_, _)
        | Type::Filter(_)
        | Type::FilterMap(_) => {
            // do nothing on primitive types
            // no need to recurse into them.
            Ok(())
        }
        Type::Table(t)
        | Type::OutputStream(t)
        | Type::Rib(t)
        | Type::List(t) => visit(types, visited, t),
        Type::Verdict(t1, t2) => {
            visit(types, visited, t1)?;
            visit(types, visited, t2)
        }
        Type::NamedRecord(_, fields) | Type::Record(fields) => {
            for (_, ty) in fields {
                visit(types, visited, ty)?;
            }
            Ok(())
        }
        Type::Enum(_, variants) => {
            for (_, ty) in variants {
                if let Some(ty) = ty {
                    visit(types, visited, ty)?;
                }
            }
            Ok(())
        }
        Type::Name(ident) => visit_name(types, visited, *ident),
    }
}
