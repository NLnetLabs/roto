use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::{hash_map::Entry, HashMap},
    fmt::{Display, Formatter},
    hash::Hash,
    rc::Rc,
};

use crate::{
    ast::{AcceptReject, CompareOp, ShortString, Identifier},
    compile::CompileError,
    traits::{RotoType, Token},
    types::{
        builtin::BuiltinTypeValue, typedef::TypeDef, typevalue::TypeValue,
    },
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
    // gets (a clone of) kind, type, token and a optional ref to the
    // TypeValue. The last field will only contain a typevalue if it's a
    // Constant.
    pub fn get_props(
        &self,
    ) -> Result<(SymbolKind, TypeDef, Token, Option<TypeValue>), CompileError>
    {
        let token = self.get_token()?;
        Ok((
            self.kind,
            self.ty.clone(),
            token,
            self.get_value().builtin_as_cloned_type_value().ok(),
        ))
    }

    pub fn get_builtin_type(&self) -> Result<TypeDef, CompileError> {
        if !matches!(
            self.ty,
            TypeDef::Rib(_)
                | TypeDef::Table(_)
                | TypeDef::List(_)
                | TypeDef::Record(_)
                | TypeDef::Unknown
        ) {
            Ok(self.ty.clone())
        } else {
            Err(format!("Type '{:?}' is not a builtin type", self).into())
        }
    }

    pub fn get_kind_type_and_token(
        &self,
    ) -> Result<(SymbolKind, TypeDef, Token), CompileError> {
        let token = self.get_token()?;
        Ok((self.kind, self.ty.clone(), token))
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

    pub fn has_value(&self) -> bool {
        self.value != TypeValue::Unknown
    }

    pub fn get_value(&self) -> &TypeValue {
        &self.value
    }

    pub fn get_value_owned(self) -> TypeValue {
        self.value
    }

    pub fn _take_value(&mut self) -> TypeValue {
        std::mem::take(&mut self.value)
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

    // Go into the first arg of a symbol until we find a empty args vec and
    // store it there.
    pub fn add_arg(&mut self, arg: Symbol) -> usize {
        // if self.args.is_empty() {
        self.args.push(arg);
        self.args.len() - 1
        // } else {
        //     self.get_args_mut()[0].add_arg(arg)
        // }
    }

    pub fn empty() -> Self {
        Symbol {
            name: "".into(),
            kind: SymbolKind::Empty,
            ty: TypeDef::Unknown,
            args: vec![],
            value: TypeValue::Unknown,
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
            value: TypeValue::Unknown,
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
    pub(crate) fn get_recursive_return_type(&self) -> TypeDef {
        if self.args.is_empty() {
            self.ty.clone()
        } else if let Some(last_arg) = self.args.last() {
            last_arg.get_type() // get_recursive_return_type()
        } else {
            unreachable!()
        }
    }

    // try to return the converted type with its value, if it's set. This is
    // basically only the case if the kin of self, is Constant.
    // Otherwise create an empty value for the type of self, and recursively
    // try to convert that into the desired type.
    // If these two steps fail, return an error.
    pub fn try_convert_value_into(
        mut self,
        type_def: TypeDef,
    ) -> Result<Self, CompileError> {
        println!("CONVERT {:#?} -> {}", self, type_def);
        match self.ty {
            TypeDef::Rib(_) => { return Err(CompileError::new("RIB can't be converted.".into())) },
            TypeDef::Table(_) => {return Err(CompileError::new("Table can't be converted.".into()))},
            TypeDef::List(_) => {
                if let TypeValue::List(list) = self.value {
                    self.value = list.into_type(&type_def)?;
                }
            },
            TypeDef::Record(_) => {
                if let TypeValue::Record(rec) = self.value {
                    self.value = rec.into_type(&type_def)?;
                }
            },
            TypeDef::U32 => {
                if let TypeValue::Builtin(BuiltinTypeValue::U32(int)) = self.value {
                    self.value = int.into_type(&type_def)?;
                }
            },
            TypeDef::U8 => {
                if let TypeValue::Builtin(BuiltinTypeValue::U8(int)) = self.value {
                    self.value = int.into_type(&type_def)?;
                }
            },
            TypeDef::Boolean => {
                if let TypeValue::Builtin(BuiltinTypeValue::Boolean(int)) = self.value {
                    self.value = int.into_type(&type_def)?;
                }
            },
            TypeDef::String => {
                if let TypeValue::Builtin(BuiltinTypeValue::StringLiteral(str)) = self.value {
                    self.value = str.into_type(&type_def)?;
                }
            },
            TypeDef::Prefix => {
                if let TypeValue::Builtin(BuiltinTypeValue::Prefix(pfx)) = self.value {
                    self.value = pfx.into_type(&type_def)?;
                }
            },
            TypeDef::PrefixLength => {
                if let TypeValue::Builtin(BuiltinTypeValue::PrefixLength(pl)) = self.value {
                    self.value = pl.into_type(&type_def)?;
                }
            },
            TypeDef::IpAddress => {
                if let TypeValue::Builtin(BuiltinTypeValue::IpAddress(ip)) = self.value {
                    self.value = ip.into_type(&type_def)?;
                }
            },
            TypeDef::Asn => {
                if let TypeValue::Builtin(BuiltinTypeValue::Asn(asn)) = self.value {
                    self.value = asn.into_type(&type_def)?;
                }
            },
            TypeDef::AsPath => {
                if let TypeValue::Builtin(BuiltinTypeValue::AsPath(as_path)) = self.value {
                    self.value = as_path.into_type(&type_def)?;
                }
            },
            TypeDef::Hop => {
                if let TypeValue::Builtin(BuiltinTypeValue::Hop(hop)) = self.value {
                    self.value = hop.into_type(&type_def)?;
                }
            }
            TypeDef::Community => {
                if let TypeValue::Builtin(BuiltinTypeValue::Community(int)) = self.value {
                    self.value = int.into_type(&type_def)?;
                }
            },
            TypeDef::OriginType => {
                if let TypeValue::Builtin(BuiltinTypeValue::OriginType(ot)) = self.value {
                    self.value = ot.into_type(&type_def)?;
                }
            },
            TypeDef::Route => {
                if let TypeValue::Builtin(BuiltinTypeValue::Route(r)) = self.value {
                    self.value = r.into_type(&type_def)?;
                }
            },
            TypeDef::LocalPref => {
                if let TypeValue::Builtin(BuiltinTypeValue::LocalPref(r)) = self.value {
                    self.value = r.into_type(&type_def)?;
                }
            },
            TypeDef::MultiExitDisc => {
                if let TypeValue::Builtin(BuiltinTypeValue::MultiExitDisc(r)) = self.value {
                    self.value = r.into_type(&type_def)?;
                }
            },
            TypeDef::NextHop => {
                if let TypeValue::Builtin(BuiltinTypeValue::NextHop(r)) = self.value {
                    self.value = r.into_type(&type_def)?;
                }
            },
            TypeDef::AtomicAggregator => {
                if let TypeValue::Builtin(BuiltinTypeValue::AtomicAggregator(r)) = self.value {
                    self.value = r.into_type(&type_def)?;
                }
            },
            TypeDef::RouteStatus => {
                if let TypeValue::Builtin(BuiltinTypeValue::RouteStatus(rs)) = self.value {
                    self.value = rs.into_type(&type_def)?;
                }
            },
            TypeDef::HexLiteral => {
                if let TypeValue::Builtin(BuiltinTypeValue::HexLiteral(hex)) = self.value {
                    self.value = hex.into_type(&type_def)?;
                }
            },
            TypeDef::IntegerLiteral => {
                if let TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(int)) = self.value {
                    self.value = int.into_type(&type_def)?;
                }
            },
            TypeDef::StringLiteral => {
                if let TypeValue::Builtin(BuiltinTypeValue::StringLiteral(str)) = self.value {
                    self.value = str.into_type(&type_def)?;
                }
            },
            TypeDef::AcceptReject(_) => { return Err(CompileError::new("AcceptReject value can't be converted.".into())) },
            TypeDef::Unknown => { return Err(CompileError::new("Value from unknown type can't be converted.".into())) },
  
        }

        Ok(self)
    }

    pub(crate) fn flatten_nodes(&self) -> Vec<&Symbol> {
        let mut new_nodes = vec![];
        for arg in self.get_args() {
            new_nodes.extend(arg.flatten_nodes());
        }

        new_nodes.push(self);

        new_nodes
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
    // Assigment Symbols
    VariableAssignment, // A variable defined by the user

    // assigment+access receiver symbols
    // these symbols are used both for assignment and as
    // root access receivers.
    Constant, // A literal value or a module-level variable

    // Rx and Tx Types
    // The payload can be a mutable type, that comes in at the input of the
    // VM, and is mutated. In that case there is no separate Rx and Tx types:
    // they have to be the same type. These two types are the indicators for
    // that situation.
    PassThroughRxTxType, // type of the mutatable incoming & outgoing payload
    // The incoming and outgoing are separate types, this means that the
    // incoming payload is *not* mutated, but instead a new outgoing, empty
    // payload of the SplitTxType is created and filled by the specified
    // filter.
    // Note: this has the effect that specifying a `rx_tx: Route` results in
    // a different outgoing payload that specifying `rx: Route; tx: Route`.
    // In the first case, the incoming payload is mutated, that means that
    // fields in the Route instance that are left alone by the filter will be
    // there unchanged in the outgoing payload. In the second case only the
    // fields that are filled by the filter will have a (non-default) value.
    SplitRxType,   // type of the incoming payload
    SplitTxType,   // type of the outgoing payload

    // data sources access receivers
    Rib,
    Table,
    PrefixList,

    // access receiver symbols
    AccessReceiver,
    // these symbols are only used to as roots for receiving
    // data.
    Argument,      // a passed-in module or term level argument
    AnonymousType, // type of a sub-record
    NamedType,     // User-defined type of a record

    // accessor symbols
    // symbols that come after an access receiver in the args list.

    // A method call, that will either mutate the typevalue this SymolKind
    // lives on when the data field is true, otherwise it will just read
    // it (to produce a new typevalue).
    MethodCallbyRef,
    MethodCallByConsumedValue,
    // A method call on a built-in type, e.g. `AsPath.contains()`
    BuiltInTypeMethodCall,
    // A method call that's a builtin of the roto language, e.g.
    // `send-to-log`
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
    MatchAction(MatchActionType),
    Action,
    SubAction,
    Term,

    // A symbol that has been consumed by the compiler
    Empty,
}

#[derive(Debug)]
pub struct MatchAction {
    _action_type: MatchActionType,
    _quantifier: MatchActionQuantifier,
    symbol: Symbol,
}

impl MatchAction {
    pub(crate) fn get_args(&self) -> &[Symbol] {
        self.symbol.get_args()
    }

    pub(crate) fn get_kind(&self) -> SymbolKind {
        self.symbol.get_kind()
    }

    pub fn get_name(&self) -> ShortString {
        self.symbol.get_name()
    }

    pub fn get_type(&self) -> TypeDef {
        self.symbol.get_type()
    }
}

#[derive(Debug)]
pub enum MatchActionQuantifier {
    Exists,
    ExactlyOne,
    Every,
}

#[derive(Debug, Hash, Copy, Clone, Eq, PartialEq)]
pub enum MatchActionType {
    MatchAction,
    NegateMatchAction,
    EmptyAction, // An MatchAction without an action, just a accept or reject
}

impl TryFrom<SymbolKind> for MatchActionType {
    type Error = CompileError;

    fn try_from(kind: SymbolKind) -> Result<Self, CompileError> {
        if let SymbolKind::MatchAction(ma) = kind {
            Ok(ma)
        } else {
            Err(CompileError::new("Invalid Match Action Type".into()))
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct MatchActionKey((ShortString, MatchActionType));

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Scope {
    Global,
    Module(ShortString),
}

impl Scope {
    pub(crate) fn get_name(&self) -> ShortString {
        if let Scope::Module(name) = self {
            name.clone()
        } else {
            "global".into()
        }
    }
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
    // match_actions: HashMap<MatchActionKey, Vec<Symbol>>,
    match_actions: Vec<MatchAction>,
    // the action that will be activated when all of the match_actions are
    // processed and no early return has been issued
    default_action: crate::ast::AcceptReject,
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
    pub(crate) rx_type: Option<(ShortString, TypeDef)>,
    pub(crate) tx_type: Option<(ShortString, TypeDef)>,
    pub(crate) used_variables: Vec<(ShortString, &'a Symbol)>,
    pub(crate) used_arguments: Vec<(ShortString, &'a Symbol)>,
    pub(crate) used_data_sources: Vec<(ShortString, &'a Symbol)>,
}

// struct Location {
//     name: ShortString,
//     module: ShortString,
//     line: usize,
// }

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
            match_actions: vec![],
            default_action: crate::ast::AcceptReject::Accept,
        }
    }

    pub(crate) fn get_name(&self) -> ShortString {
        self.scope.get_name()
    }

    pub(crate) fn move_var_const_into(
        &mut self,
        mut symbol: Symbol,
    ) -> Result<(), CompileError> {
        let key = symbol.get_name();
        if self.variables.contains_key(&key) {
            return Err(format!(
                "Symbol {} already defined in scope {}",
                key, self.scope
            )
            .into());
        }

        symbol = match symbol.get_kind() {
            SymbolKind::VariableAssignment => {
                symbol.set_token(Token::Variable(self.variables.len()))
            }
            SymbolKind::Constant => {
                symbol.set_token(Token::Constant(Some(self.variables.len())))
            }
            _ => {
                return Err(CompileError::new(
                    "Invalid Symbol to store as Variable/Constant".into(),
                ));
            }
        };
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

        if key.as_str() == "route" { 
            println!("k {} n {} k {:?} t {} a {:?} v {} ", key, name, kind, ty, args, value);
        }

        if self.arguments.contains_key(&name) {
            return Err(format!(
                "Symbol {} already defined in scope {}",
                name, self.scope
            )
            .into());
        }

        let token_int = self.arguments.len();

        let token = match kind {
            SymbolKind::SplitRxType => Some(Token::RxType),
            SymbolKind::SplitTxType => Some(Token::TxType),
            SymbolKind::PassThroughRxTxType => Some(Token::RxType),
            _ => Some(Token::Argument(token_int)),
        };

        match kind {
            SymbolKind::SplitRxType | SymbolKind::PassThroughRxTxType => {
                self.rx_type = Symbol::new(name, kind, ty, args, token);
            }
            SymbolKind::SplitTxType => {
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
                value: TypeValue::Unknown,
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
        mut action: Symbol,
    ) -> Result<(), CompileError> {
        let token_int = self.actions.len() as u8;
        let token = Some(Token::Action(token_int));

        action.token = token;
        self.actions.insert(key, action);

        Ok(())
    }

    pub(crate) fn move_match_action_into(
        &mut self,
        symbol: Symbol,
    ) -> Result<(), CompileError> {
        if let SymbolKind::MatchAction(s) = symbol.get_kind() {
            self.match_actions.push(MatchAction {
                symbol,
                _action_type: s,
                _quantifier: MatchActionQuantifier::Exists,
            })
        }
        Ok(())
    }

    pub(crate) fn get_symbol(&self, name: &Identifier) -> Option<&Symbol> {
        let name = &name.ident;
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

    pub(crate) fn get_terms(&self) -> Vec<&Symbol> {
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

    pub(crate) fn get_actions(&self) -> Vec<&Symbol> {
        self.actions.values().collect::<Vec<_>>()
    }

    pub(crate) fn set_default_action(
        &mut self,
        default_action: AcceptReject,
    ) {
        self.default_action = default_action;
    }

    pub(crate) fn get_default_action(&self) -> AcceptReject {
        self.default_action
    }

    pub(crate) fn get_match_actions(&self) -> Vec<&MatchAction> {
        self.match_actions.iter().collect::<Vec<_>>() //.values().collect::<Vec<_>>()
    }

    pub(crate) fn get_data_source(
        &self,
        name: &ShortString,
    ) -> Result<(TypeDef, Token), CompileError> {
        let src: Result<&Symbol, CompileError> = self
            .variables
            .values()
            .into_iter()
            .find(|kv| kv.get_name() == name)
            .ok_or_else(|| format!("Symbol '{}' not found", name).into());

        src.map(|r| match r.get_token() {
            Ok(Token::Rib(_)) => {
                r.get_kind_type_and_token().map(|ktt| (ktt.1, ktt.2))
            }
            Ok(Token::Table(_)) => {
                Ok((TypeDef::Table(Box::new(r.get_type())), r.get_token()?))
            }
            _ => Err(CompileError::new(format!(
                "No data source named '{}' found.",
                name
            ))),
        })?
    }

    // retrieve all the unique arguments, variables and data-sources that are
    // referenced in the terms field of a symbol table (i.e. the terms
    // sections in a module)
    pub(crate) fn create_deps_graph(
        &self,
    ) -> Result<
            DepsGraph, // (variables, arguments, data sources)
        CompileError,
    > {
        // First, go over all the terms and see which variables, arguments
        // and data-sources they refer to.

        let mut deps_vec: Vec<&Symbol> = vec![];
        for s in self.terms.values() {
            deps_vec.extend(s.flatten_nodes()) // .into_iter().filter(|s| s.get_token().is_ok()));
        }

        let DepsGraph {
            // rx_type,
            // tx_type,
            mut used_variables,
            mut used_arguments,
            mut used_data_sources,
            ..
        } = self._partition_deps_graph(deps_vec).map_err(|_e| {
            CompileError::new(
                "can't create dependencies graph for terms".into(),
            )
        })?;

        // Second, go over all the variables that we gathered in the last
        // step and see which variables, arguments and data-sources they
        // refer to.

        let mut vars_deps_vec = vec![];
        for s in used_variables.iter().map(|s| s.1) {
            vars_deps_vec.extend(s.flatten_nodes());
        }

        let DepsGraph {
            used_variables: vars_vars,
            used_arguments: vars_args,
            used_data_sources: vars_data_sources,
            ..
        } = self._partition_deps_graph(vars_deps_vec).map_err(|_e| {
            CompileError::new(
                "can't create dependencies graph for variables".into(),
            )
        })?;

        used_variables.extend(vars_vars);
        used_arguments.extend(vars_args);
        used_data_sources.extend(vars_data_sources);

        for col in [
            &mut used_variables,
            &mut used_arguments,
            &mut used_data_sources,
        ] {
            col.sort_by(|a, b| a.1.cmp(b.1));
            col.dedup_by(|a, b| a.1.eq(b.1));
        }

        Ok(
            DepsGraph {
                rx_type: Some((self.rx_type.get_name(), self.rx_type.get_type())),
                tx_type: self.tx_type.as_ref().map(|s| (s.get_name(), s.get_type())),
                used_variables,
                used_arguments,
                used_data_sources,
            },
        )
    }

    fn _partition_deps_graph<'a>(
        &'a self,
        mut deps_vec: Vec<&'a Symbol>,
    ) -> Result<DepsGraph, CompileError> {
        deps_vec.retain(|t| match t.get_token() {
            Ok(t) => t.is_variable() || t.is_argument() || t.is_data_source(),
            _ => false,
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
            rx_type: None,
            tx_type: None,
            used_arguments: args_vec,
            used_variables: vars_vec,
            used_data_sources: data_sources_vec,
        })
    }
}
