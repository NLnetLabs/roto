use crate::ast::Identifier;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Label {
    pub identifier: Identifier,
    pub internal: bool,
    pub parent: Option<LabelRef>,
    pub counter: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct LabelRef(usize);

#[derive(Default)]
pub struct LabelStore {
    labels: Vec<Label>,
}

impl LabelStore {
    pub fn new_label(&mut self, identifier: Identifier) -> LabelRef {
        self.labels.push(Label {
            identifier,
            internal: false,
            parent: None,
            counter: 0,
        });
        LabelRef(self.labels.len() - 1)
    }

    pub fn wrap(&mut self, parent: LabelRef, identifier: Identifier) -> LabelRef {
        self.labels.push(Label {
            identifier,
            internal: false,
            parent: Some(parent),
            counter: 0,
        });
        LabelRef(self.labels.len() - 1)
    }

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
