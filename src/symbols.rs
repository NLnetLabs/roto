use std::{
    cell::RefCell,
    cmp::Ordering,
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
    }, compile::CompileError,
};

//------------ Symbols ------------------------------------------------------

// The only symbols we really have are variables & (user-defined) types.

#[derive(Debug, Eq)]
pub(crate) struct Symbol {
    name: ShortString,
    kind: SymbolKind,
    ty: TypeDef,
    args: Vec<Symbol>,
    value: TypeValue,
    token: Option<Token>, // location: Location,
}

impl Symbol {
    // gets the type only from the `ty` field.
    pub fn get_kind_type_and_token(
        &self,
    ) -> Result<(SymbolKind, TypeDef, Token), CompileError> {
        let token = self.get_token()?;
        Ok((self.kind, self.ty.clone(), token))
    }

    pub fn get_builtin_type(
        &self,
    ) -> Result<TypeDef, CompileError> {
        if !matches!(
            self.ty,
            TypeDef::Rib(_)
                | TypeDef::Table(_)
                | TypeDef::List(_)
                | TypeDef::Record(_)
                | TypeDef::None
        ) {
            (&self.ty).try_into().map(|tv: BuiltinTypeValue| tv.into())
        } else if let TypeValue::Builtin(ty) = &self.value {
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

    pub fn get_token(&self) -> Result<Token, CompileError> {
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

    pub fn has_name(&self, name: &str) -> Option<&Self> {
        if self.name == name {
            Some(self)
        } else {
            None
        }
    }

    pub fn get_name(&self) -> ShortString {
        self.name.clone()
    }

    pub fn set_name(mut self, name: ShortString) -> Self {
        self.name = name;
        self
    }

    pub fn get_value(&self) -> &TypeValue {
        &self.value
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

    pub fn empty() -> Self {
        Symbol {
            name: "".into(),
            kind: SymbolKind::Empty,
            ty: TypeDef::None,
            args: vec![],
            value: TypeValue::None,
            token: None,
        }
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
            value: TypeValue::None,
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
            value,
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
    ) -> Result<Self, CompileError> {
        match self.value {
            TypeValue::Builtin(BuiltinTypeValue::U32(int)) => {
                self.value = int.into_type(type_def)?;
            }
            TypeValue::Builtin(BuiltinTypeValue::U8(int)) => {
                self.value = int.into_type(type_def)?;
            }
            TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(int)) => {
                self.value = int.into_type(type_def)?;
            }
            TypeValue::Builtin(BuiltinTypeValue::StringLiteral(str)) => {
                self.value = str.into_type(type_def)?;
            }
            TypeValue::Builtin(BuiltinTypeValue::HexLiteral(hex)) => {
                self.value = hex.into_type(type_def)?;
            }
            TypeValue::Builtin(BuiltinTypeValue::PrefixLength(pl)) => {
                self.value = pl.into_type(type_def)?;
            }
            TypeValue::Builtin(BuiltinTypeValue::Asn(asn)) => {
                self.value = asn.into_type(type_def)?;
            }
            TypeValue::Builtin(BuiltinTypeValue::Prefix(prefix)) => {
                self.value = prefix.into_type(type_def)?;
            }
            TypeValue::Builtin(BuiltinTypeValue::IpAddress(ip)) => {
                self.value = ip.into_type(type_def)?;
            }
            TypeValue::Builtin(BuiltinTypeValue::Community(com)) => {
                self.value = com.into_type(type_def)?;
            }
            TypeValue::Builtin(BuiltinTypeValue::Boolean(bool)) => {
                self.value = bool.into_type(type_def)?;
            }
            TypeValue::Builtin(BuiltinTypeValue::Route(route)) => {
                self.value = route.into_type(type_def)?;
            }
            TypeValue::Builtin(BuiltinTypeValue::RouteStatus(status)) => {
                self.value = status.into_type(type_def)?;
            }
            TypeValue::Builtin(BuiltinTypeValue::AsPath(as_path)) => {
                self.value = as_path.into_type(type_def)?;
            }
            TypeValue::List(list) => {
                self.value = list.into_type(type_def)?;
            }
            TypeValue::Record(rec) => {
                self.value = rec.into_type(type_def)?;
            }
            TypeValue::Rib(rib) => {
                self.value = rib.into_type(type_def)?;
            }
            TypeValue::Table(table) => {
                self.value = table.into_type(type_def)?;
            }
            TypeValue::None => {
                self.value = type_def.into();
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

impl std::cmp::PartialOrd for Symbol {
    fn partial_cmp(&self, other: &Symbol) -> Option<std::cmp::Ordering> {
        Some(
            self.token
                .as_ref()
                .unwrap()
                .cmp(other.token.as_ref().unwrap()),
        )
    }
}

impl std::cmp::Ord for Symbol {
    fn cmp(&self, other: &Self) -> Ordering {
        self.token
            .as_ref()
            .unwrap()
            .cmp(other.token.as_ref().unwrap())
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Symbol) -> bool {
        self.token.as_ref().unwrap() == other.token.as_ref().unwrap()
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
    // data sources
    Rib,
    Table,
    PrefixList,
    // accessors
    MethodCall,
    BuiltInTypeMethodCall,
    GlobalMethodCall,
    FieldAccess,
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
    // The input payload type of the module.
    rx_type: Symbol,
    // The output payload type of the module. If it's none its identical
    // to the input payload type.
    tx_type: Option<Symbol>,
    // The special symbols that will be filled in at runtime, once per filter
    // run.
    arguments: HashMap<ShortString, Symbol>,
    // The variables and constants that are defined in the module.
    variables: HashMap<ShortString, Symbol>,
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

pub(crate) struct DepsGraph<'a> {
    pub(crate) variables: Vec<(ShortString, &'a Symbol)>,
    pub(crate) arguments: Vec<(ShortString, &'a Symbol)>,
    pub(crate) data_sources: Vec<(ShortString, &'a Symbol)>,
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
            rx_type: Symbol::empty(),
            tx_type: None,
            arguments: HashMap::new(),
            variables: HashMap::new(),
            terms: HashMap::new(),
            actions: HashMap::new(),
            match_actions: HashMap::new(),
        }
    }

    pub(crate) fn move_symbol_into(
        &mut self,
        key: ShortString,
        mut symbol: Symbol,
    ) -> Result<(), CompileError> {
        if self.variables.contains_key(&key) {
            return Err(format!(
                "Symbol {} already defined in scope {}",
                key, self.scope
            )
            .into());
        }

        symbol = symbol.set_token(Token::Variable(self.variables.len()));
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
        value: TypeValue,
    ) -> Result<(), CompileError> {
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

        let token_int = self.variables.len();

        let token = Some(match kind {
            SymbolKind::Rib => Token::Rib(token_int),
            // Treat PrefixList like a table, they share the same methods.
            SymbolKind::Table | SymbolKind::PrefixList => {
                Token::Table(token_int)
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
        value: TypeValue,
    ) -> Result<(), CompileError> {
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

        let token_int = self.arguments.len();

        let token = match kind {
            SymbolKind::RxType => Some(Token::RxType),
            SymbolKind::TxType => Some(Token::TxType),
            _ => Some(Token::Argument(token_int)),
        };

        match kind {
            SymbolKind::RxType => {
                self.rx_type = Symbol::new(name, kind, ty, args, token);
            }
            SymbolKind::TxType => {
                self.tx_type = Some(Symbol::new(name, kind, ty, args, token));
            }
            _ => {
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
            }
        };
        Ok(())
    }

    pub(crate) fn add_logical_formula(
        &mut self,
        term_key: ShortString,
        child_symbol: Symbol,
    ) -> Result<(), CompileError> {
        let term_token = Some(Token::Term(self.terms.len() as u8));

        if let Entry::Vacant(term) = self.terms.entry(term_key.clone()) {
            term.insert(Symbol {
                name: term_key.clone(),
                kind: SymbolKind::Term,
                ty: child_symbol.get_type(),
                args: vec![child_symbol],
                value: TypeValue::None,
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
        value: TypeValue,
    ) -> Result<(), CompileError> {
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
            value: TypeValue::None,
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
        value: TypeValue,
        // token is the index of the term that corresponds to the `name`
        // argument. It's a token for a term.
        token: Token,
    ) -> Result<(), CompileError> {
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

    pub(crate) fn get_symbol(&self, name: &ShortString) -> Option<&Symbol> {
        if let Some(symbol) = self.rx_type.has_name(name) {
            return Some(symbol);
        }
        if let Some(tx_type) = self.tx_type.as_ref() {
            if let Some(symbol) = tx_type.has_name(name) {
                return Some(symbol);
            }
        }
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
    ) -> Result<&Symbol, CompileError> {
        self.variables
            .get(name)
            .or_else(|| self.arguments.get(name))
            .ok_or_else(|| format!("Symbol '{}' not found", name).into())
    }

    // Retrieve the symbol from the `variables` table of a module the entry
    // Used in the compile stage to build the command stack.

    // panics if the symbol is not found.
    pub(crate) fn get_variable_by_token(&self, token: &Token) -> &Symbol {
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

    pub(crate) fn get_variable_name_by_token(
        &mut self,
        token: &Token,
    ) -> ShortString {
        match token {
            Token::Variable(_token_int) => self
                .variables
                .iter()
                .find(|s| s.1.token == Some(token.clone()))
                .unwrap_or_else(|| {
                    panic!("Fatal: Created Token does not exist.")
                })
                .0
                .clone(),
            _ => panic!("Fatal: Created Token does represent a variable."),
        }
    }

    pub(crate) fn get_argument(
        &self,
        name: &ShortString,
    ) -> Result<&Symbol, CompileError> {
        self.arguments.get(name).ok_or_else(|| {
            format!("Symbol of type Argument '{}' not found", name).into()
        })
    }

    pub(crate) fn get_term(
        &self,
        name: &ShortString,
    ) -> Result<(TypeDef, Token), CompileError> {
        self.terms
            .get(name)
            .ok_or_else(|| format!("Symbol '{}' not found", name).into())
            .map(|term| (term.ty.clone(), term.token.clone().unwrap()))
    }

    pub(crate) fn get_terms(
        &self,
    ) -> Vec<&Symbol> {
        self.terms.values().collect::<Vec<_>>()
    }

    pub(crate) fn get_action(
        &self,
        name: &ShortString,
    ) -> Result<(TypeDef, Token), CompileError> {
        self.actions
            .get(name)
            .ok_or_else(|| format!("Symbol '{}' not found", name).into())
            .map(|action| (action.ty.clone(), action.token.clone().unwrap()))
    }

    pub(crate) fn get_data_source(
        &self,
        name: &ShortString,
    ) -> Result<(TypeDef, Token), CompileError> {
        let src: Result<&Symbol, CompileError> = self
            .variables
            .get(name)
            .ok_or_else(|| format!("Symbol '{}' not found", name).into());

        src.map(|r| match r.get_kind() {
            SymbolKind::Rib => r.get_kind_type_and_token().map(|ktt| (ktt.1, ktt.2)),
            SymbolKind::Table => {
                Ok((TypeDef::Table(Box::new(r.get_type())), r.get_token()?))
            }
            _ => {
                Err(CompileError::new(format!("No data source named '{}' found.", name)))
            }
        })?
    }

    // retrieve all the unique arguments, variables and data-sources that are
    // referenced in the terms field of a symbol table (i.e. the terms
    // sections in a module)
    pub(crate) fn create_deps_graph(
        &self,
    ) -> Result<
        (
            (ShortString, TypeDef),
            Option<(ShortString, TypeDef)>,
            DepsGraph,
        ),
        CompileError,
    > {
        // First, go over all the terms and see which variables, arguments
        // and data-sources they refer to.

        let mut deps_vec: Vec<&Symbol> = vec![];
        for s in self.terms.values() {
            deps_vec.extend(s.get_leaf_nodes().into_iter());
        }

        let DepsGraph {
            mut variables,
            mut arguments,
            mut data_sources,
        } = self._partition_deps_graph(deps_vec).map_err(|e| {
            CompileError::new(
                "can't create dependencies graph for terms".into())
        })?;

        // Second, go over all the variables that we gathered in the last
        // step and see which variables, arguments and data-sources they
        // refer to.

        let mut vars_deps_vec = vec![];
        for s in variables.iter().map(|s| s.1) {
            vars_deps_vec.extend(s.get_leaf_nodes().into_iter());
        }

        let DepsGraph {
            variables: vars_vars,
            arguments: vars_args,
            data_sources: vars_data_sources,
        } = self._partition_deps_graph(vars_deps_vec).map_err(|e| CompileError::new("can't create dependencies graph for variables".into()))?;

        variables.extend(vars_vars);
        arguments.extend(vars_args);
        data_sources.extend(vars_data_sources);

        for col in [&mut variables, &mut arguments, &mut data_sources] {
            col.sort_by(|a, b| a.1.cmp(b.1));
            col.dedup_by(|a, b| a.1.eq(b.1));
        }

        Ok((
            (self.rx_type.get_name(), self.rx_type.get_type()),
            self.tx_type.as_ref().map(|s| (s.get_name(), s.get_type())),
            DepsGraph {
                variables,
                arguments,
                data_sources,
            },
        ))
    }

    fn _partition_deps_graph<'a>(
        &'a self,
        mut deps_vec: Vec<&'a Symbol>,
    ) -> Result<DepsGraph, CompileError> {
        deps_vec.retain(|t| {
            t.get_token().unwrap().is_variable()
                || t.get_token().unwrap().is_argument()
                || t.get_token().unwrap().is_data_source()
        });

        let (args_vec, vars_srcs_vec): (Vec<&Symbol>, Vec<&Symbol>) =
            deps_vec
                .into_iter()
                .partition(|s| s.get_token().unwrap().is_argument());

        let args_vec = args_vec
            .into_iter()
            .map(|s| {
                if let Some(s) = self.arguments.get_key_value(&s.get_name()) {
                    (s.0.clone(), s.1)
                } else {
                    (s.get_name(), s)
                }
            })
            .collect::<Vec<(ShortString, &Symbol)>>();

        let vars_src_vec: (Vec<_>, Vec<_>) = vars_srcs_vec
            .into_iter()
            .partition(|s| s.get_token().unwrap().is_variable());

        let (vars_vec, data_sources_vec): (Vec<_>, Vec<_>) = (
            vars_src_vec
                .0
                .into_iter()
                .map(|s| {
                    if let Some(s) =
                        self.variables.get_key_value(&s.get_name())
                    {
                        (s.0.clone(), s.1)
                    } else {
                        (s.get_name(), s)
                    }
                })
                .collect(),
            vars_src_vec.1.iter().map(|s| (s.get_name(), *s)).collect(),
        );

        Ok(DepsGraph {
            arguments: args_vec,
            variables: vars_vec,
            data_sources: data_sources_vec,
        })
    }
}
