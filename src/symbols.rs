use std::{collections::HashMap, fmt::{Formatter, Display}};

use crate::{ast::ShortString, types::{TypeValue, TypeDef}};

//------------ Symbols ------------------------------------------------------

// The only symbols we really have are variables & (user-defined) types.

#[derive(Debug)]
pub struct Symbol<'a> {
    name: ShortString,
    kind: SymbolKind,
    ty: TypeDef<'a>,
    args: Vec<Symbol<'a>>,
    pub value: Option<TypeValue>,
    // location: Location,
}

impl<'a> Symbol<'a> {
    pub fn get_type(&self) -> TypeDef<'a> {
        self.ty.clone()
    }

    pub fn set_type(mut self, ty: TypeDef<'a>) -> Symbol {
        self.ty = ty;
        self
    }

    pub fn get_kind(&self) -> SymbolKind {
        self.kind
    }

    pub fn get_name(&self) -> ShortString {
        self.name.clone()
    }

    pub fn get_value(&self) -> Option<&TypeValue> {
        self.value.as_ref()
    }

    pub fn get_args(self) -> Vec<Symbol<'a>> {
        self.args
    }

    pub fn new(name: ShortString, kind: SymbolKind, ty: TypeDef<'a>, args: Vec<Symbol<'a>>) -> Self {
        Symbol {
            name,
            kind,
            ty,
            args,
            value: None,
        }
    }

    pub fn new_with_value(name: ShortString, kind: SymbolKind, value: TypeValue, args: Vec<Symbol<'a>>) -> Self {
        Symbol {
            name,
            kind,
            ty: TypeDef::None,
            args,
            value: Some(value),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum SymbolKind {
    Variable,
    Constant,
    Argument,
    AnonymousType,
    NamedType,
    Rib,
    Table,
    PrefixList,
    DataSourceMethodCall,
    BuiltInTypeMethodCall,
    GlobalMethodCall,
    FieldAccess,
    StringLiteral,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Scope {
    Global,
    Module(ShortString),
}

impl Display for Scope {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Scope::Global => write!(f, "global"),
            Scope::Module(name) => write!(f, "module: '{}'", name),
        }
    }
}

#[derive(Debug)]
pub struct SymbolTable<'a> {
    scope: Scope,
    pub symbols: HashMap<ShortString, Symbol<'a>>,
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
            symbols: HashMap::new(),
            types: HashMap::new(),
        }
    }

    pub fn add_symbol(&mut self, key: ShortString, name: Option<ShortString>, kind: SymbolKind, ty: TypeDef<'a>, args: Vec<Symbol<'a>>, value: Option<TypeValue>) -> Result<(), Box<dyn std::error::Error>> {
        let name = if let Some(name) = name {
            name
        } else {
            key.clone()
        };

        if self.symbols.contains_key(&name) {
            return Err(format!("Symbol {} already defined in scope {}", name, self.scope).into());
        }

        self.symbols.insert(key, Symbol { name, kind, ty, args, value });
        Ok(())
    }

    pub fn add_type(&mut self, name: ShortString, ty: TypeDef<'a>) {
        self.types.insert(name, ty);
    }

    pub fn get_symbol(&self, name: &ShortString) -> Result<&Symbol<'a>, Box<dyn std::error::Error>> {
        self.symbols.get(name).ok_or_else(|| format!("Symbol {} not found", name).into())
    }

    pub fn get_type(&self, name: &ShortString) -> Option<&TypeDef<'a>> {
        self.types.get(name)
    }
}

struct BuiltInTypes<'a>(HashMap<ShortString, TypeDef<'a>>);
