//! Labels for basic blocks in the IR
//!
//! These labels are designed to be unique, but still have some structure.
//! Technically, a unique number for each label would suffice, but that would
//! make the generated code harder to debug. Therefore, the labels follow
//! a hierarchical scheme, where labels have a name, a counter and optionally
//! a parent. When they are displayed, we join the linked list formed by the
//! parent labels as separated by `::`.

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
    labels: Vec<Label>,
}

impl LabelStore {
    /// Create a new label with an identifier and without any parent label
    pub fn new_label(&mut self, identifier: Identifier) -> LabelRef {
        self.labels.push(Label {
            identifier,
            internal: false,
            parent: None,
            counter: 0,
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
        self.labels.push(Label {
            identifier,
            internal: false,
            parent: Some(parent),
            counter: 0,
        });
        LabelRef(self.labels.len() - 1)
    }

    /// Create an internal child label of a given label
    pub fn wrap_internal(
        &mut self,
        parent: LabelRef,
        identifier: Identifier,
    ) -> LabelRef {
        self.labels.push(Label {
            identifier,
            internal: true,
            parent: Some(parent),
            counter: 0,
        });
        LabelRef(self.labels.len() - 1)
    }

    /// Increment the counter of the label by one and return a reference to it
    pub fn next(&mut self, previous: LabelRef) -> LabelRef {
        let previous = &self.labels[previous.0];
        self.labels.push(Label {
            counter: previous.counter + 1,
            ..*previous
        });
        LabelRef(self.labels.len() - 1)
    }

    /// Get the [`Label`] corresponding to a [`LabelRef`]
    pub fn get(&self, label: LabelRef) -> &Label {
        &self.labels[label.0]
    }
}
