//! Labels for basic blocks in the IR

use crate::ast::Identifier;

/// A label for a basic block
///
/// Should not be constructed directly, but using a [`LabelStore`].
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

    pub fn next(&mut self, previous: LabelRef) -> LabelRef {
        let previous = &self.labels[previous.0];
        self.labels.push(Label {
            counter: previous.counter + 1,
            ..*previous
        });
        LabelRef(self.labels.len() - 1)
    }

    pub fn get(&self, label: LabelRef) -> &Label {
        &self.labels[label.0]
    }
}
