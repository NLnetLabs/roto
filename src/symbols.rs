use std::{collections::HashMap, cell::{RefCell, Ref}, rc::Rc};

use crate::{ast::ShortString, types::{TypeValue, TypeDef}};

//------------ Symbols ------------------------------------------------------

// The only symbols we really have are variables & (user-defined) types.

#[derive(Debug)]
pub struct Symbol<'a> {
    name: ShortString,
    kind: SymbolKind,
    ty: TypeDef<'a>,
    pub value: Option<TypeValue<'a>>,
    // location: Location,
}

impl<'a> Symbol<'a> {
    pub fn get_type(&self) -> TypeDef<'a> {
        self.ty.clone()
    }

    pub fn get_kind(&self) -> SymbolKind {
        self.kind
    }

    pub fn get_name(&self) -> ShortString {
        self.name.clone()
    }

//     pub fn get_value(&self) -> Rc<Option<TypeValue<'a>>> {
//         Rc::new(self.value).clone()
//     }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum SymbolKind {
    Variable,
    AnonymousType,
    NamedType,
    Rib
}

#[derive(Debug)]
pub enum Scope {
    Global,
    Module(ShortString),
}

#[derive(Debug)]
pub struct SymbolTable<'a> {
    scope: Scope,
    pub symbols: Rc<RefCell<HashMap<ShortString, Symbol<'a>>>>,
    types: HashMap<ShortString, TypeDef<'a>>,
}

struct Location {
    name: ShortString,
    module: ShortString,
    line: usize,
}

impl<'a> SymbolTable<'a> {
    pub(crate) fn new(module: ShortString) -> Self {
        SymbolTable {
            scope: Scope::Module(module),
            symbols: Rc::new(RefCell::new(HashMap::new())),
            types: HashMap::new(),
        }
    }

    pub fn add_symbol(&mut self, name: ShortString, kind: SymbolKind, ty: TypeDef<'a>, value: Option<TypeValue<'a>>) {
        let _name = name.clone();
        self.symbols.borrow_mut().insert(_name, Symbol { name, kind, ty, value });
    }

    pub fn add_type(&mut self, name: ShortString, ty: TypeDef<'a>) {
        self.types.insert(name, ty);
    }

    // pub fn get_symbol(&self, name: &ShortString) -> Option<&Symbol<'a>> {
    //     self.symbols.clone().borrow().get(name)
    // }

    pub fn get_symbols(&self) -> Rc<RefCell<HashMap<ShortString, Symbol<'a>>>> {
        self.symbols.clone()
    }

    pub fn get_type(&self, name: &ShortString) -> Option<&TypeDef<'a>> {
        self.types.get(name)
    }
}

struct BuiltInTypes<'a>(HashMap<ShortString, TypeDef<'a>>);
