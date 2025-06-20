use crate::{
    ast::Identifier,
    label::{LabelRef, LabelStore},
    typechecker::{info::TypeInfo, scope::ScopeRef},
};

pub struct IrPrinter<'a> {
    pub type_info: &'a TypeInfo,
    pub label_store: &'a LabelStore,
    pub scope: Option<ScopeRef>,
}

pub trait Printable {
    fn print(&self, printer: &IrPrinter) -> String;
}

impl Printable for Identifier {
    fn print(&self, _printer: &IrPrinter) -> String {
        self.as_str().into()
    }
}

impl Printable for ScopeRef {
    fn print(&self, printer: &IrPrinter) -> String {
        printer.type_info.scope_graph.print_scope(*self)
    }
}

impl Printable for LabelRef {
    fn print(&self, printer: &IrPrinter) -> String {
        let mut label = printer.label_store.get(*self);
        let mut strings = Vec::new();
        loop {
            let dollar = if label.internal { "$" } else { "" };
            let ident = label.identifier.print(printer);
            let counter = if label.counter > 0 {
                format!("({})", label.counter)
            } else {
                "".into()
            };
            strings.push(format!("{dollar}{ident}{counter}"));
            let Some(parent) = label.parent else {
                break;
            };
            label = printer.label_store.get(parent);
            let Some(_) = label.parent else {
                break;
            };
        }

        strings.reverse();
        strings.join(".")
    }
}
