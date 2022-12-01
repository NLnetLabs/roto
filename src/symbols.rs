use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    fmt::{Display, Formatter},
    hash::Hash,
    rc::Rc,
};

use crate::{
    ast::{CompareOp, ShortString},
    traits::{RotoFilter, Token},
    types::{
        builtin::BuiltinTypeValue, typedef::TypeDef, typevalue::TypeValue,
    },
};

//------------ Symbols ------------------------------------------------------

// The only symbols we really have are variables & (user-defined) types.

#[derive(Debug)]
pub(crate) struct Symbol {
    name: ShortString,
    kind: SymbolKind,
    ty: TypeDef,
    args: Vec<Symbol>,
    value: Option<TypeValue>,
    token: Option<Token>, // location: Location,
}

impl Symbol {
    // gets the type only from the `ty` field.
    pub fn get_type_and_token(
        &self,
    ) -> Result<(TypeDef, Token), Box<dyn std::error::Error>> {
        let token = self.get_token()?;
        Ok((self.ty.clone(), token))
    }

    pub fn get_builtin_type(
        &self,
    ) -> Result<TypeDef, Box<dyn std::error::Error>> {
        if !matches!(
            self.ty,
            TypeDef::Rib(_)
                | TypeDef::Table(_)
                | TypeDef::List(_)
                | TypeDef::Record(_)
                | TypeDef::None
        ) {
            (&self.ty).try_into().map(|tv: BuiltinTypeValue| tv.into())
        } else if let Some(TypeValue::Builtin(ty)) = &self.value {
            Ok(ty.into())
        } else {
            println!("get_builtin_type: {:#?}", self);
            Err(format!("a Type '{:?}' is not a builtin type", self).into())
        }
    }

    pub fn get_kind_and_type(&self) -> (SymbolKind, TypeDef) {
        (self.kind, self.ty.clone())
    }

    pub fn set_kind(mut self, kind: SymbolKind) -> Symbol {
        self.kind = kind;
        self
    }

    pub fn get_kind(&self) -> SymbolKind {
        self.kind
    }

    pub fn get_token(&self) -> Result<Token, Box<dyn std::error::Error>> {
        self.token.clone().ok_or_else(|| {
            format!("No token found for symbol '{:?}'", self).into()
        })
    }

    pub fn get_type(&self) -> TypeDef {
        self.ty.clone()
    }

    pub fn set_type(mut self, ty: TypeDef) -> Self {
        self.ty = ty;
        self
    }

    pub fn set_token(mut self, token: Token) -> Self {
        self.token = Some(token);
        self
    }

    pub fn get_name(&self) -> ShortString {
        self.name.clone()
    }

    pub fn set_name(mut self, name: ShortString) -> Self {
        self.name = name;
        self
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

    pub fn get_args_mut(&mut self) -> &mut [Symbol] {
        &mut self.args
    }

    pub fn set_args(&mut self, args: Vec<Symbol>) {
        self.args = args;
    }

    pub fn new(
        name: ShortString,
        kind: SymbolKind,
        ty: TypeDef,
        args: Vec<Symbol>,
        token: Option<Token>,
    ) -> Self {
        Symbol {
            name,
            kind,
            ty,
            args,
            value: None,
            token,
        }
    }

    pub fn new_with_value(
        name: ShortString,
        kind: SymbolKind,
        value: TypeValue,
        args: Vec<Symbol>,
        token: Token,
    ) -> Self {
        Symbol {
            name,
            kind,
            ty: (&value).into(),
            args,
            value: Some(value),
            token: Some(token),
        }
    }

    // get the return from this symbol if it's a leaf node, otherwise return
    // the type of the nested leaf node (in the 'args' field). Note that this
    // method will have to change if we're going to allow method calls with
    // trailing method calls/field accesses, like `d().e`. Currently the
    // parser won't allow it.
    pub(crate) fn get_return_type(&self) -> TypeDef {
        if self.args.is_empty() {
            self.ty.clone()
        } else if let Some(first_arg) = self.args.first() {
            println!("first arg {:?}", first_arg);
            first_arg.ty.clone()
        } else {
            unreachable!()
        }
    }

    // try to return the converted type with its value, if it's set.
    // Otherwise try to return an empty typevalue for the converted type. if
    // no conversion is available, return an error.
    pub fn try_convert_value_into(
        mut self,
        type_def: &TypeDef,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        match self.value {
            Some(TypeValue::Builtin(BuiltinTypeValue::U32(int))) => {
                self.value = Some(int.into_type(type_def)?);
            }
            Some(TypeValue::Builtin(BuiltinTypeValue::U8(int))) => {
                self.value = Some(int.into_type(type_def)?);
            }
            Some(TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(
                int,
            ))) => {
                self.value = Some(int.into_type(type_def)?);
            }
            Some(TypeValue::Builtin(BuiltinTypeValue::HexLiteral(hex))) => {
                self.value = Some(hex.into_type(type_def)?);
            }
            Some(TypeValue::Builtin(BuiltinTypeValue::PrefixLength(pl))) => {
                self.value = Some(pl.into_type(type_def)?);
            }
            Some(TypeValue::Builtin(BuiltinTypeValue::Asn(asn))) => {
                self.value = Some(asn.into_type(type_def)?);
            }
            Some(TypeValue::Builtin(BuiltinTypeValue::Prefix(prefix))) => {
                self.value = Some(prefix.into_type(type_def)?);
            }
            Some(TypeValue::Builtin(BuiltinTypeValue::IpAddress(ip))) => {
                self.value = Some(ip.into_type(type_def)?);
            }
            Some(TypeValue::Builtin(BuiltinTypeValue::Community(com))) => {
                self.value = Some(com.into_type(type_def)?);
            }
            Some(TypeValue::Builtin(BuiltinTypeValue::Boolean(bool))) => {
                self.value = Some(bool.into_type(type_def)?);
            }
            Some(TypeValue::Builtin(BuiltinTypeValue::Route(route))) => {
                self.value = Some(route.into_type(type_def)?);
            }
            Some(TypeValue::Builtin(BuiltinTypeValue::RouteStatus(
                status,
            ))) => {
                self.value = Some(status.into_type(type_def)?);
            }
            Some(TypeValue::Builtin(BuiltinTypeValue::AsPath(as_path))) => {
                self.value = Some(as_path.into_type(type_def)?);
            }
            Some(TypeValue::List(list)) => {
                self.value = Some(list.into_type(type_def)?);
            }
            Some(TypeValue::Record(rec)) => {
                self.value = Some(rec.into_type(type_def)?);
            }
            Some(TypeValue::Rib(rib)) => {
                self.value = Some(rib.into_type(type_def)?);
            }
            Some(TypeValue::Table(table)) => {
                self.value = Some(table.into_type(type_def)?);
            }
            Some(TypeValue::None) => {
                return Err("Cannot convert None into a type".into())
            }
            None => {
                self.value = Some(type_def.into());
            }
        };
        Ok(self)
    }

    // Leaf nodes have an empty `args` field. Recursively check if this
    // symbol has leaf nodes or is a leaf node itself.
    pub(crate) fn get_leaf_nodes(&self) -> Vec<&Symbol> {
        let mut leaves = vec![];

        if !self.args.is_empty() {
            for arg in self.args.iter() {
                leaves.extend(arg.get_leaf_nodes());
            }
        }

        if self.get_token().is_ok() {
            leaves.push(self);
        }

        leaves
    }

    pub(crate) fn follow_first_leaf(&self) -> &Symbol {
        if self.args.is_empty() {
            self
        } else {
            self.args.first().unwrap().follow_first_leaf()
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum SymbolKind {
    Variable, // A variable defined by the user
    Constant, // A literal value or a module-level variable
    Argument,
    BuiltInType,
    AnonymousType, // type of a sub-record
    NamedType,     // User-defined type of a record
    RxType,        // type of the incoming payload
    TxType,        // type of the outgoing payload
    Rib,
    Table,
    PrefixList,
    MethodCall,
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
    Action,
    Term,
    // A symbol that has been consumed by the compiler
    Empty,
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
    // The special symbols that will be filled in at runtime, once per filter
    // run.
    arguments: HashMap<ShortString, Symbol>,
    // The variables and constants that are defined in the module.
    variables: HashMap<ShortString, Symbol>,
    types: HashMap<ShortString, TypeDef>,
    // The evaluated `term` sections that are defined in the module.
    terms: HashMap<ShortString, Symbol>,
    // The evaluated `action` sections that are defined in the module.
    actions: HashMap<ShortString, Symbol>,
    // All the `filter` clauses in the `apply` section, the tie actions to
    // terms.
    match_actions: HashMap<ShortString, Vec<Symbol>>,
}

// The global symbol table.
#[derive(Debug)]
pub struct GlobalSymbolTable(
    Rc<
        RefCell<
            std::collections::HashMap<
                super::symbols::Scope,
                super::symbols::SymbolTable,
            >,
        >,
    >,
);

impl GlobalSymbolTable {
    pub(crate) fn borrow_mut(
        &self,
    ) -> std::cell::RefMut<'_, HashMap<Scope, SymbolTable>> {
        self.0.borrow_mut()
    }

    pub(crate) fn borrow(
        &self,
    ) -> std::cell::Ref<'_, HashMap<Scope, SymbolTable>> {
        self.0.borrow()
    }

    pub fn new() -> Self {
        GlobalSymbolTable(Rc::new(RefCell::new(HashMap::new())))
    }
}

impl Clone for GlobalSymbolTable {
    fn clone(&self) -> Self {
        GlobalSymbolTable(Rc::clone(&self.0))
    }
}

impl Default for GlobalSymbolTable {
    fn default() -> Self {
        GlobalSymbolTable::new()
    }
}

struct Location {
    name: ShortString,
    module: ShortString,
    line: usize,
}

impl SymbolTable {
    pub(crate) fn new(module: ShortString) -> Self {
        SymbolTable {
            scope: Scope::Module(module),
            arguments: HashMap::new(),
            variables: HashMap::new(),
            terms: HashMap::new(),
            actions: HashMap::new(),
            match_actions: HashMap::new(),
            types: HashMap::new(),
        }
    }

    pub(crate) fn move_symbol_into(
        &mut self,
        key: ShortString,
        mut symbol: Symbol,
    ) -> Result<(), Box<dyn std::error::Error>> {
        if self.variables.contains_key(&key) {
            return Err(format!(
                "Symbol {} already defined in scope {}",
                key, self.scope
            )
            .into());
        }

        symbol =
            symbol.set_token(Token::Variable(self.variables.len() as u8));
        self.variables.insert(key, symbol);
        Ok(())
    }

    pub(crate) fn add_variable(
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

        if self.variables.contains_key(&name) {
            return Err(format!(
                "Symbol {} already defined in scope {}",
                name, self.scope
            )
            .into());
        }

        let token_int = self.variables.len() as u8;

        let token = Some(match kind {
            SymbolKind::Rib | SymbolKind::Table | SymbolKind::PrefixList => {
                Token::DataSource(token_int)
            }
            _ => Token::Variable(token_int),
        });

        self.variables.insert(
            key,
            Symbol {
                name,
                kind,
                ty,
                args,
                value,
                token,
            },
        );
        Ok(())
    }

    pub(crate) fn add_argument(
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

        if self.arguments.contains_key(&name) {
            return Err(format!(
                "Symbol {} already defined in scope {}",
                name, self.scope
            )
            .into());
        }

        let token_int = self.arguments.len() as u8;

        let token = Some(Token::Argument(token_int));

        self.arguments.insert(
            key,
            Symbol {
                name,
                kind,
                ty,
                args,
                value,
                token,
            },
        );
        Ok(())
    }

    pub(crate) fn add_logical_formula(
        &mut self,
        term_key: ShortString,
        child_symbol: Symbol,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let term_token = Some(Token::Term(self.terms.len() as u8));

        if let Entry::Vacant(term) = self.terms.entry(term_key.clone()) {
            term.insert(Symbol {
                name: term_key.clone(),
                kind: SymbolKind::Term,
                ty: child_symbol.get_type(),
                args: vec![child_symbol],
                value: None,
                token: term_token,
            });
        } else {
            let child_args = &mut self.terms.get_mut(&term_key).unwrap().args;
            child_args.push(child_symbol);
        }

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

        let token_int = self.actions.len() as u8;
        let token = Some(Token::Action(token_int));

        let args = Symbol {
            name,
            kind,
            ty: ty.clone(),
            args,
            value: None,
            token: None,
        };

        let s = Symbol {
            name: key.clone(),
            kind: SymbolKind::Action,
            ty,
            args: vec![args],
            value,
            token,
        };

        self.actions.insert(key, s);

        Ok(())
    }

    pub(crate) fn add_match_action(
        &mut self,
        key: ShortString,
        name: Option<ShortString>,
        kind: SymbolKind,
        ty: TypeDef,
        args: Vec<Symbol>,
        value: Option<TypeValue>,
        // token is the index of the term that corresponds to the `name`
        // argument. It's a token for a term.
        token: Token,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let name = if let Some(name) = name {
            name
        } else {
            key.clone()
        };

        if self.match_actions.contains_key(&name) {
            return Err(format!(
                "Match Action '{}' already defined in scope {}",
                name, self.scope
            )
            .into());
        }

        if let Some(match_action) = self.match_actions.get_mut(&key) {
            match_action.push(Symbol {
                name,
                kind,
                ty,
                args,
                value,
                token: Some(token),
            });
        } else {
            self.match_actions.insert(
                key,
                vec![Symbol {
                    name,
                    kind,
                    ty,
                    args,
                    value,
                    token: Some(token),
                }],
            );
        };
        Ok(())
    }

    pub fn add_type(&mut self, name: ShortString, ty: TypeDef) {
        self.types.insert(name, ty);
    }

    pub fn get_type(&self, name: &ShortString) -> Option<&TypeDef> {
        self.types.get(name)
    }

    pub(crate) fn get_symbol(&self, name: &ShortString) -> Option<&Symbol> {
        if let Some(symbol) = self.variables.get(name) {
            return Some(symbol);
        }

        if let Some(symbol) = self.arguments.get(name) {
            return Some(symbol);
        }

        if let Some(symbol) = self.terms.get(name) {
            return Some(symbol);
        }

        if let Some(symbol) = self.actions.get(name) {
            return Some(symbol);
        }

        if let Some(symbol) = self.match_actions.get(name) {
            return Some(&symbol[0]);
        }

        None
    }

    pub(crate) fn get_variable(
        &self,
        name: &ShortString,
    ) -> Result<&Symbol, Box<dyn std::error::Error>> {
        self.variables
            .get(name)
            .or_else(|| self.arguments.get(name))
            .ok_or_else(|| format!("Symbol '{}' not found", name).into())
    }

    // Retrieve the symbol from the `variables` table of a module the entry
    // Used in the compile stage to build the command stack.

    // panics if the symbol is not found.
    pub(crate) fn get_variable_by_token(&mut self, token: &Token) -> &Symbol {
        match token {
            Token::Variable(_token_int) => self
                .variables
                .values()
                .find(|s| s.token == Some(token.clone()))
                .unwrap_or_else(|| {
                    panic!("Fatal: Created Token does not exist.")
                }),
            _ => panic!("Fatal: Created Token does represent a variable."),
        }
    }

    pub(crate) fn get_variable_name_by_token(&mut self, token: &Token) -> ShortString {
        match token {
            Token::Variable(_token_int) => self
                .variables
                .iter()
                .find(|s| s.1.token == Some(token.clone()))
                .unwrap_or_else(|| {
                    panic!("Fatal: Created Token does not exist.")
                })
                .0.clone(),
            _ => panic!("Fatal: Created Token does represent a variable."),
        }
    }

    pub(crate) fn get_argument(
        &self,
        name: &ShortString,
    ) -> Result<&Symbol, Box<dyn std::error::Error>> {
        self.arguments.get(name).ok_or_else(|| {
            format!("Symbol of type Argument '{}' not found", name).into()
        })
    }

    pub(crate) fn get_term(
        &self,
        name: &ShortString,
    ) -> Result<(TypeDef, Token), Box<dyn std::error::Error>> {
        self.terms
            .get(name)
            .ok_or_else(|| format!("Symbol '{}' not found", name).into())
            .map(|term| (term.ty.clone(), term.token.clone().unwrap()))
    }

    pub(crate) fn get_action(
        &self,
        name: &ShortString,
    ) -> Result<(TypeDef, Token), Box<dyn std::error::Error>> {
        self.actions
            .get(name)
            .ok_or_else(|| format!("Symbol '{}' not found", name).into())
            .map(|action| (action.ty.clone(), action.token.clone().unwrap()))
    }

    pub(crate) fn get_data_source(
        &self,
        name: &ShortString,
    ) -> Result<(TypeDef, Token), Box<dyn std::error::Error>> {
        let src: Result<&Symbol, Box<dyn std::error::Error>> = self
            .variables
            .get(name)
            .ok_or_else(|| format!("Symbol '{}' not found", name).into());

        src.map(|r| match r.get_kind() {
            SymbolKind::Rib => r.get_type_and_token(),
            SymbolKind::Table => {
                Ok((TypeDef::Table(Box::new(r.get_type())), r.get_token()?))
            }
            _ => {
                Err(format!("No data source named '{}' found.", name).into())
            }
        })?
    }

    // retrieve all the unique arguments, variables and data-sources that are
    // referenced in the terms field of a symbol table (i.e. the terms
    // sections in a module)
    pub(crate) fn get_term_deps(
        &self,
    ) -> (Vec<Token>, Vec<Token>, Vec<Token>) {
        let mut deps_vec: Vec<Token> = vec![];
        for s in self.terms.values() {
            deps_vec.extend(
                s.get_leaf_nodes()
                    .into_iter()
                    .map(|s| s.get_token().unwrap()),
            );
        }

        deps_vec.retain(|t| {
            t.is_variable() || t.is_argument() || t.is_data_source()
        });
        deps_vec.sort();
        deps_vec.dedup();

        let (args_vec, vars_srcs_vec): (Vec<Token>, Vec<Token>) =
            deps_vec.into_iter().partition(|t| t.is_argument());

        let vars_vec =
            vars_srcs_vec.into_iter().partition(|t| t.is_variable());

        (args_vec, vars_vec.0, vars_vec.1)
    }
}
