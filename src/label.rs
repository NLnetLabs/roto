//! Labels for basic blocks in the IR
//!
//! These labels are designed to be unique, but still have some structure.
//! Technically, a unique number for each label would suffice, but that would
//! make the generated code harder to debug. Therefore, the labels follow
//! a hierarchical scheme, where labels have a name, a counter and optionally
//! a parent. When they are displayed, we join the linked list formed by the
//! parent labels as separated by `::`.

use std::collections::{HashMap, hash_map::Entry};

use crate::ast::Identifier;

/// A label for a basic block
///
/// This should not be constructed directly, but only via a [`LabelStore`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Label {
    pub identifier: Identifier,
    pub internal: bool,
    pub parent: Option<LabelRef>,
    pub counter: usize,
}

/// A reference to a label in the [`LabelStore`]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct LabelRef(usize);

/// Holds all the information of labels and gives out references
#[derive(Default)]
pub struct LabelStore {
    last: HashMap<(Option<LabelRef>, Identifier), usize>,
    labels: Vec<Label>,
}

impl LabelStore {
    /// Create a new label with an identifier and without any parent label
    pub fn new_label(&mut self, identifier: Identifier) -> LabelRef {
        let counter = match self.last.entry((None, identifier)) {
            Entry::Occupied(mut e) => {
                let e = e.get_mut();
                *e += 1;
                *e
            }
            Entry::Vacant(e) => {
                e.insert(0);
                0
            }
        };
        self.labels.push(Label {
            identifier,
            internal: false,
            parent: None,
            counter,
        });
        LabelRef(self.labels.len() - 1)
    }

    /// Create a child label of a given label
    #[allow(dead_code)]
    pub fn wrap(
        &mut self,
        parent: LabelRef,
        identifier: Identifier,
    ) -> LabelRef {
        let counter = match self.last.entry((Some(parent), identifier)) {
            Entry::Occupied(mut e) => {
                let e = e.get_mut();
                *e += 1;
                *e
            }
            Entry::Vacant(e) => {
                e.insert(0);
                0
            }
        };
        self.labels.push(Label {
            identifier,
            internal: false,
            parent: Some(parent),
            counter,
        });
        LabelRef(self.labels.len() - 1)
    }

    /// Create an internal child label of a given label
    pub fn wrap_internal(
        &mut self,
        parent: LabelRef,
        identifier: Identifier,
    ) -> LabelRef {
        let counter = match self.last.entry((Some(parent), identifier)) {
            Entry::Occupied(mut e) => {
                let e = e.get_mut();
                *e += 1;
                *e
            }
            Entry::Vacant(e) => {
                e.insert(0);
                0
            }
        };
        self.labels.push(Label {
            identifier,
            internal: true,
            parent: Some(parent),
            counter,
        });
        LabelRef(self.labels.len() - 1)
    }

    /// Increment the counter of the label by one and return a reference to it
    pub fn next(&mut self, previous: LabelRef) -> LabelRef {
        let previous = &self.labels[previous.0];
        let counter = self
            .last
            .get_mut(&(previous.parent, previous.identifier))
            .unwrap();

        *counter += 1;

        self.labels.push(Label {
            counter: *counter,
            ..*previous
        });
        LabelRef(self.labels.len() - 1)
    }

    /// Get the [`Label`] corresponding to a [`LabelRef`]
    pub fn get(&self, label: LabelRef) -> &Label {
        &self.labels[label.0]
    }
}
