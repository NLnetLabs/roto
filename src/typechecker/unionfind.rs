use super::types::Type;

/// A simple unionfind data structure
///
/// A [`usize`] is mapped to a [`Type`], if that type is a type variable,
/// we recurse to find what it maps to. On its way out of the call stack
/// it sets each type it traversed to the final type it found to limit
/// traversal in a subsequent query.
///
/// A mapping from a type var to itself means that it is not set.
#[derive(Clone, Default)]
pub struct UnionFind {
    inner: Vec<Type>,
}

impl UnionFind {
    /// Find the type corresponding to a type var
    pub fn find(&mut self, index: usize) -> Type {
        match &self.inner[index] {
            Type::Var(i) | Type::IntVar(i) | Type::RecordVar(i, _)
                if *i != index =>
            {
                let new_t = self.find(*i);
                self.inner[index] = new_t.clone();
                new_t
            }
            t => t.clone(),
        }
    }

    /// Generate a new type var
    pub fn fresh(&mut self, f: impl FnOnce(usize) -> Type) -> Type {
        let n = self.inner.len();
        let t = f(n);
        self.inner.push(t.clone());
        t
    }

    /// The set value of a type var
    pub fn set(&mut self, index: usize, t: Type) {
        self.inner[index] = t;
    }
}
