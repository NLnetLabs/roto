//! Cycle detection for Roto constants
//!
//! Roto's constants are not allowed to be recursive.

use crate::typechecker::scope::ValueKind;
use crate::typechecker::{TypeChecker, TypeResult};

use super::scope::ResolvedName;
use std::collections::{BTreeMap, BTreeSet};
use std::hash::Hash;

impl TypeChecker {
    /// Return the order in which the code for constants and function has to be
    /// generated or an error if there is no valid order.
    ///
    /// The simplest case of an invalid cycle is a constant defined as itself:
    ///
    /// ```roto
    /// const A: u32 = A;
    /// ```
    ///
    /// Another simple example of a cycle are mutually recursive constants:
    ///
    /// ```roto
    /// const A: u32 = B;
    /// const B: u32 = A;
    /// ```
    ///
    /// So, we need to do at least some topological sort on the constants. However,
    /// functions can be part of this analysis. Take for instance this snippet:
    ///
    /// ```roto
    /// const A: u32 = foo();
    ///
    /// fn foo() -> u32 {
    ///     A
    /// }
    /// ```
    ///
    /// But a topological sort of both functions and constants still doesn't cut it
    /// because functions themselves _are_ allowed to be recursive.
    ///
    /// Therefore, the correct approach is to find the strongly connected
    /// components (i.e. cycles) in the graph and ensure that those either consist
    /// of a single constant or multiple functions, but not both. These strongly
    /// connected components then need to be topologically sorted.
    ///
    /// Luckily, Tarjan's algorithm does both of those things.
    ///
    /// See the [Wikipedia page] for more information.
    ///
    /// [Wikipedia page]: https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
    ///
    pub fn find_compilation_order(&self) -> TypeResult<Vec<ResolvedName>> {
        for (name, refs) in &self.references.references {
            let dec = self.type_info.scope_graph.get_declaration(*name);
            if let super::scope::DeclarationKind::Value(
                ValueKind::Constant,
                _,
            ) = dec.kind
                && refs.contains(name)
            {
                // TODO: We can improve this error message by showing where
                // the reference happens.
                return Err(
                    self.error_recursive_constant(dec.name.ident, dec.id)
                );
            }
        }

        let components = tarjan(&self.references.references);

        for component in &components {
            if component.len() > 1 {
                for name in component {
                    let dec =
                        self.type_info.scope_graph.get_declaration(*name);
                    if let super::scope::DeclarationKind::Value(
                        ValueKind::Constant,
                        _,
                    ) = dec.kind
                    {
                        // TODO: We can improve this error message by a chain
                        // of references that it problematic.
                        return Err(self.error_recursive_constant(
                            dec.name.ident,
                            dec.id,
                        ));
                    }
                }
            }
        }

        // Check that none of the constants uses context
        self.context_check()?;

        Ok(components.into_iter().flatten().collect())
    }

    fn context_check(&self) -> TypeResult<()> {
        let mut visited = BTreeSet::new();
        let mut uses_context = BTreeMap::new();

        for name in self.references.references.keys() {
            let dec = self.type_info.scope_graph.get_declaration(*name);
            if let super::scope::DeclarationKind::Value(
                ValueKind::Constant,
                _,
            ) = dec.kind
                && self.determine_uses_context(
                    &mut uses_context,
                    &mut visited,
                    name,
                )
            {
                return Err(
                    self.error_constant_uses_context(dec.name.ident, dec.id)
                );
            }
        }

        Ok(())
    }

    fn determine_uses_context(
        &self,
        uses_context: &mut BTreeMap<ResolvedName, bool>,
        visited: &mut BTreeSet<ResolvedName>,
        name: &ResolvedName,
    ) -> bool {
        if let Some(b) = uses_context.get(name) {
            return *b;
        }

        // In this case, we've hit a cycle, assume there isn't a use of context,
        // but don't store that because we are not done evaluating it.
        if visited.contains(name) {
            return false;
        }

        let dec = self.type_info.scope_graph.get_declaration(*name);
        if let super::scope::DeclarationKind::Value(
            ValueKind::Context(..),
            _,
        ) = dec.kind
        {
            uses_context.insert(*name, true);
            return true;
        };

        visited.insert(*name);
        for reference in
            self.references.references.get(name).into_iter().flatten()
        {
            if self.determine_uses_context(uses_context, visited, reference) {
                uses_context.insert(*name, true);
                return true;
            }
        }

        uses_context.insert(*name, false);
        false
    }
}

#[derive(Clone, Debug)]
pub struct RefGraph {
    references: BTreeMap<ResolvedName, BTreeSet<ResolvedName>>,
}

impl RefGraph {
    pub fn new() -> Self {
        Self {
            references: BTreeMap::new(),
        }
    }

    pub fn add_node(&mut self, from: ResolvedName) {
        self.references.entry(from).or_default();
    }

    pub fn add_edge(&mut self, from: ResolvedName, to: ResolvedName) {
        self.references.entry(from).or_default().insert(to);
    }
}

struct State<V: Copy> {
    stack: Vec<V>,
    vertices: BTreeMap<V, VertexState>,
    next_index: usize,
    components: Vec<Vec<V>>,
}

struct VertexState {
    index: usize,
    lowlink: usize,
}

fn tarjan<V: Copy + Eq + Ord + Hash + std::fmt::Debug>(
    edges: &BTreeMap<V, BTreeSet<V>>,
) -> Vec<Vec<V>> {
    let mut state = State::<V>::new();

    for v in edges.keys() {
        if !state.vertices.contains_key(v) {
            strongly_connect(edges, &mut state, *v);
        }
    }

    state.components
}

fn strongly_connect<V: Copy + Eq + Ord>(
    references: &BTreeMap<V, BTreeSet<V>>,
    state: &mut State<V>,
    v: V,
) {
    let index = state.next_index;
    state.next_index += 1;
    state.vertices.insert(
        v,
        VertexState {
            index,
            lowlink: index,
        },
    );
    state.stack.push(v);

    for w in references.get(&v).into_iter().flatten() {
        if !state.vertices.contains_key(w) {
            strongly_connect(references, state, *w);

            let new = state.vertices[w].lowlink;
            state.update_lowlink(v, new);
        } else if state.stack.contains(w) {
            let new = state.vertices[w].index;
            state.update_lowlink(v, new);
        }
    }

    let v_state = &state.vertices[&v];
    if v_state.index == v_state.lowlink {
        let mut component = Vec::new();
        while let Some(w) = state.stack.pop() {
            component.push(w);
            if w == v {
                break;
            }
        }
        state.components.push(component);
    }
}

impl<V: Copy + Eq + Ord> State<V> {
    fn new() -> Self {
        Self {
            stack: Vec::new(),
            vertices: BTreeMap::new(),
            next_index: 0,
            components: Vec::new(),
        }
    }

    fn update_lowlink(&mut self, v: V, new: usize) {
        let current = &mut self.vertices.get_mut(&v).unwrap().lowlink;
        *current = (*current).min(new);
    }
}

#[cfg(test)]
mod tests {
    use super::tarjan;
    use std::collections::BTreeMap;

    #[test]
    fn one_two_three_four() {
        let mut edges = BTreeMap::new();
        edges.insert(1, [2].into());
        edges.insert(2, [3, 4].into());
        edges.insert(3, [2, 4].into());
        edges.insert(4, [].into());

        let components = tarjan(&edges);
        assert_eq!(components, &[vec![4], vec![3, 2], vec![1]])
    }

    #[test]
    fn cycle() {
        let mut edges = BTreeMap::new();
        edges.insert(1, [2].into());
        edges.insert(2, [3].into());
        edges.insert(3, [4].into());
        edges.insert(4, [1].into());

        let components = tarjan(&edges);
        assert_eq!(components, &[vec![4, 3, 2, 1]])
    }

    #[test]
    fn boop() {
        let mut edges = BTreeMap::new();
        edges.insert(1, [3].into());
        edges.insert(2, [].into());
        edges.insert(3, [4, 5].into());
        edges.insert(4, [2].into());
        edges.insert(5, [2].into());

        let components = tarjan(&edges);
        assert_eq!(components, &[vec![2], vec![4], vec![5], vec![3], vec![1]])
    }
}
