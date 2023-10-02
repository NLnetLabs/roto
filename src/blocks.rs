use std::fmt::{Display, Formatter};

use crate::ast::ShortString;

//------------ Scope --------------------------------------------------------

// A scope identifies a roto block (its quality: Unit, Module, Filter,
// FilterMap, Module) and describes its scope.

#[derive(Debug, Eq, Clone)]
pub enum Scope {
    // The Global Scope. Holds all user-defined types,
    // global constant enums. May also get some
    // methods in the long run.
    Global,
    // The Scope of one FilterMap block, holds everyth ing
    // from its define section
    FilterMap(ShortString),
    // The Scope of a Filter, idem.
    Filter(ShortString),
}

impl Scope {
    pub fn as_str(&self) -> &str {
        match self {
            Scope::Filter(n) => n.as_str(),
            Scope::FilterMap(n) => n.as_str(),
            Scope::Global => "global",
        }
    }
}

impl Display for Scope {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Scope::Global => write!(f, "global"),
            Scope::FilterMap(name) => write!(f, "filter-map '{}'", name),
            Scope::Filter(name) => write!(f, "filter '{}'", name),
        }
    }
}

// Define equivalence for two Scopes if their names match, meaning a user
// should not be allowed to create two blocks with the same name even though
// they are different block types.
impl PartialEq for Scope {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Scope::Global => {
                matches!(other, Scope::Global)
            }
            Scope::FilterMap(n_s) => {
                if let Scope::FilterMap(o_s) = other {
                    o_s == n_s
                } else {
                    false
                }
            }
            Scope::Filter(n_s) => {
                if let Scope::Filter(o_s) = other {
                    o_s == n_s
                } else {
                    false
                }
            }
        }
    }
}

impl std::hash::Hash for Scope {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Scope::Global => ShortString::from("").hash(state),
            Scope::FilterMap(n_s) => n_s.hash(state),
            Scope::Filter(n_s) => n_s.hash(state),
        };
    }
}

impl From<&Scope> for ShortString {
    fn from(value:&Scope) -> Self {
        value.as_str().into()
    }
}