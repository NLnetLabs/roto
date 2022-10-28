use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Display, Formatter},
    rc::Rc, hash::Hash,
};

use crate::{
    ast::{ShortString, CompareOp},
    types::{typedef::TypeDef, typevalue::TypeValue},
};

//------------ Symbols ------------------------------------------------------

// The only symbols we really have are variables & (user-defined) types.

#[derive(Debug)]
pub(crate) struct Symbol {
    name: ShortString,
    kind: SymbolKind,
    ty: TypeDef,
    args: Vec<Symbol>,
    pub value: Option<TypeValue>,
    // location: Location,
}

impl Symbol {
    pub fn get_type(&self) -> TypeDef {
       self.ty.clone() 
    }

    pub fn set_type(mut self, ty: TypeDef) -> Symbol {
        self.ty = ty;
        self
    }

    pub fn set_kind(mut self, kind: SymbolKind) -> Symbol {
        self.kind = kind;
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

    pub fn get_args_owned(self) -> Vec<Symbol> {
        self.args
    }
 
    pub fn get_args(&self) -> &[Symbol] {
        self.args.as_slice()
    }

    pub fn new(
        name: ShortString,
        kind: SymbolKind,
        ty: TypeDef,
        args: Vec<Symbol>,
    ) -> Self {
        Symbol {
            name,
            kind,
            ty,
            args,
            value: None,
        }
    }

    pub fn new_with_value(
        name: ShortString,
        kind: SymbolKind,
        value: TypeValue,
        args: Vec<Symbol>,
    ) -> Self {
        Symbol {
            name,
            kind,
            ty: TypeDef::None,
            args,
            value: Some(value),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum SymbolKind {
    Variable,
    Constant,
    Argument,
    AnonymousType, // type of a sub-record
    NamedType, // User-defined type of a record
    RxType, // type of the incoming payload
    TxType, // type of the outgoing payload
    Rib,
    Table,
    PrefixList,
    DataSourceMethodCall,
    BuiltInTypeMethodCall,
    GlobalMethodCall,
    FieldAccess,
    StringLiteral,
    // term symbols
    LogicalExpr,
    BooleanExpr,
    CompareExpr(CompareOp),
    AndExpr,
    OrExpr,
    NotExpr,
    // apply symbols
    MatchAction,
    NegateMatchAction,
    Action
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
            Scope::Module(name) => write!(f, "module '{}'", name),
        }
    }
}

// A per-module symbol table.
#[derive(Debug)]
pub struct SymbolTable {
    scope: Scope,
    pub(crate) symbols: HashMap<ShortString, Symbol>,
    types: HashMap<ShortString, TypeDef>,
    pub(crate) terms: HashMap<ShortString, Vec<Symbol>>,
    pub(crate) actions: HashMap<ShortString, Vec<Symbol>>,
}

// The global symbol table.
pub type GlobalSymbolTable<'a> = Rc<
    RefCell<
        std::collections::HashMap<
            super::symbols::Scope,
            super::symbols::SymbolTable,
        >,
    >,
>;

struct Location {
    name: ShortString,
    module: ShortString,
    line: usize,
}

impl SymbolTable {
    pub(crate) fn new(module: ShortString) -> Self {
        SymbolTable {
            scope: Scope::Module(module),
            symbols: HashMap::new(),
            terms: HashMap::new(),
            actions: HashMap::new(),
            types: HashMap::new(),
        }
    }

    pub(crate) fn add_symbol(
        &mut self,
        key: ShortString,
        name: Option<ShortString>,
        kind: SymbolKind,
        ty: TypeDef,
        args: Vec<Symbol>,
        value: Option<TypeValue>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let name = if let Some(name) = name {
            name
        } else {
            key.clone()
        };

        if self.symbols.contains_key(&name) {
            return Err(format!(
                "Symbol {} already defined in scope {}",
                name, self.scope
            )
            .into());
        }

        self.symbols.insert(
            key,
            Symbol {
                name,
                kind,
                ty,
                args,
                value,
            },
        );
        Ok(())
    }

    pub(crate) fn add_subterm(
        &mut self,
        key: ShortString,
        name: Option<ShortString>,
        kind: SymbolKind,
        ty: TypeDef,
        args: Vec<Symbol>,
        value: Option<TypeValue>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let name = if let Some(name) = name {
            name
        } else {
            key.clone()
        };

        if self.terms.contains_key(&name) {
            return Err(format!(
                "Term {} already defined in scope {}",
                name, self.scope
            )
            .into());
        }

        if let Some(term) = self.terms.get_mut(&key) {
            term.push(Symbol {
                name,
                kind,
                ty,
                args,
                value,
            });
        } else {
            self.terms.insert(
                key,
                vec![Symbol {
                    name,
                    kind,
                    ty,
                    args,
                    value,
                }],
            );
        };

        Ok(())
    }

    pub(crate) fn add_action(
        &mut self,
        key: ShortString,
        name: Option<ShortString>,
        kind: SymbolKind,
        ty: TypeDef,
        args: Vec<Symbol>,
        value: Option<TypeValue>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let name = if let Some(name) = name {
            name
        } else {
            key.clone()
        };

        if self.actions.contains_key(&name) {
            return Err(format!(
                "Action '{}' already defined in scope {}",
                name, self.scope
            )
            .into());
        }

        if let Some(action) = self.actions.get_mut(&key) {
            action.push(Symbol {
                name,
                kind,
                ty,
                args,
                value,
            });
        } else {
            self.actions.insert(
                key,
                vec![Symbol {
                    name,
                    kind,
                    ty,
                    args,
                    value,
                }],
            );
        };
        Ok(())
    }

    pub fn add_type(&mut self, name: ShortString, ty: TypeDef) {
        self.types.insert(name, ty);
    }

    pub(crate) fn get_symbol(
        &self,
        name: &ShortString,
    ) -> Result<&Symbol, Box<dyn std::error::Error>> {
        self.symbols
            .get(name)
            .ok_or_else(|| format!("Symbol {} not found", name).into())
    }

    pub fn get_type(&self, name: &ShortString) -> Option<&TypeDef> {
        self.types.get(name)
    }
}
