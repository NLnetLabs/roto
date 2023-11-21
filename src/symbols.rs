use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::{hash_map::Entry, HashMap},
    hash::Hash,
    rc::Rc,
};

use log::trace;

use crate::{
    ast::{AcceptReject, CompareOp, FilterType, Identifier, ShortString},
    blocks::Scope,
    compiler::compile::CompileError,
    traits::{RotoType, Token},
    types::{
        collections::{ElementTypeValue, Record},
        enum_types::GlobalEnumTypeDef,
        typedef::TypeDef,
        typevalue::TypeValue,
    }, vm::VmError,
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
    token: Token, // location: Location,
}

impl Symbol {
    // gets (a clone of) kind, type, token and a optional ref to the
    // TypeValue. The last field will only contain a typevalue if it's a
    // Constant.
    pub fn get_props(
        &self,
    ) -> Result<(SymbolKind, TypeDef, Token, Option<TypeValue>), CompileError>
    {
        let token = self.get_token();
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
                | TypeDef::Route
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
        let token = self.get_token();
        Ok((self.kind, self.ty.clone(), token))
    }

    pub fn set_kind(mut self, kind: SymbolKind) -> Symbol {
        self.kind = kind;
        self
    }

    pub fn get_kind(&self) -> SymbolKind {
        self.kind
    }

    pub fn get_token(&self) -> Token {
        self.token.clone()
    }

    pub fn get_type(&self) -> TypeDef {
        self.ty.clone()
    }

    pub fn set_type(mut self, ty: TypeDef) -> Self {
        self.ty = ty;
        self
    }

    pub fn set_token(mut self, token: Token) -> Self {
        self.token = token;
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

    // This actually does NOT work with equality, since
    // Unknown != Unknown accoring to the PartialEq
    // impl, and that's correct!
    pub fn has_unknown_value(&self) -> bool {
        matches!(self.value, TypeValue::Unknown)
    }

    pub fn get_value(&self) -> &TypeValue {
        &self.value
    }

    fn _get_recursive_value(&self) -> Result<TypeValue, VmError> {
        trace!("self {:?}", self);
        if let TypeValue::Record(_) = self.get_value() {
            trace!("already has value {:?}", self.value);
            return Ok(self.value.clone());
        }

        let mut rec_values: Vec<(ShortString, ElementTypeValue)> = vec![];
        for arg in self.get_args() {
            trace!("Args {:?}", self.get_args());
            if let TypeDef::Record(mut _rec) = arg.get_type() {
                trace!("arg {}", arg.get_type());
                let v = arg._get_recursive_value()?;
                trace!("value {}: {:?}", arg.get_name(), v);
                rec_values.push((arg.get_name(), v.try_into()?));
            } else {
                trace!("non-record value {:?}", arg.get_value());
                rec_values
                    .push((arg.get_name(), arg.get_value().clone().try_into()?));
            }
        }

        match self.get_type() {
            TypeDef::Record(_) => Ok(TypeValue::Record(Record::new(rec_values))),
            _ => Ok(self.value.clone()),
        }
    }

    // checks to see if the arguments (`args`) in this symbol match with the
    // supplied `type_def` or if all the subtypes can be converted into the
    // subtypes of `type_def`. It will recognize anonymous sub-records.
    // It it can work this out it will return the a Vec with the name of the
    // (sub)-field, its (converted) type and its (converted) typevalue.
    pub fn get_recursive_values_primitive(
        &self,
        type_def: TypeDef,
    ) -> Result<Vec<(ShortString, TypeDef, TypeValue)>, CompileError> {
        // trace!("get_recursive_values_primitive with args {:#?} and type_def {:#?}", self.get_args(), type_def);
        let mut rec_values: Vec<(ShortString, TypeDef, TypeValue)> = vec![];
        for arg in self.get_args() {
            // trace!("arg {:?}", arg);
            if let TypeDef::Record(_rec) = arg.get_type() {
                if let Some(checked_type) =
                    type_def.get_field(&arg.get_name())
                {
                    let checked_val = arg.get_recursive_values_primitive(
                        checked_type.clone(),
                    )?;

                    let mut checked_val_vec = vec![];
                    for cv in checked_val {
                        checked_val_vec.push((cv.0.clone(), cv.2.clone().try_into().map_err(|e: VmError| CompileError::from(e.to_string()))?));
                    }

                    rec_values.push((
                        arg.get_name(),
                        checked_type,
                        TypeValue::Record(Record::new(
                            checked_val_vec
                        )),
                    ));
                    
                    
                } else {
                    return Err(
                        CompileError::from(
                            format!(
                                "The sub-field name '{}' cannot be found in field '{}'", 
                                &arg.get_name(),
                                self.get_name()
                            )
                        )
                    );
                }
            } else if let Some(checked_type) =
                type_def.get_field(&arg.get_name())
            {
                // trace!(
                //     "field with name '{}' found in type_def",
                //     arg.get_name()
                // );
                // trace!("checked type {:?}", checked_type);

                if !arg.get_type().test_type_conversion(checked_type.clone())
                {
                    return Err(CompileError::from(format!("Cannot convert value of type {} into value of type {}", arg.get_type(), checked_type)));
                };
                // let checked_val =
                //     arg.get_value().clone().into_type(&checked_type)?;
                rec_values.push((
                    arg.get_name(),
                    checked_type,
                    arg.get_value().clone(),
                ));
            } else {
                trace!("arg.get_name()=\"{}\"", arg.get_name());
                trace!(
                    "type_def.get_field()=\"{:?}\"",
                    type_def.get_field(&arg.get_name())
                );
                trace!("type_def {:?}", type_def);
                trace!("checked values {:?}", rec_values);
                return Err(CompileError::from(format!(
                    "'{}' cannot be found in '{}'",
                    arg.get_name(),
                    type_def
                )));
            }
            trace!("recursive value {:?}", rec_values);
        }

        Ok(rec_values)
    }

    pub fn get_value_owned(self) -> TypeValue {
        self.value
    }

    pub fn _take_value(&mut self) -> TypeValue {
        std::mem::take(&mut self.value)
    }

    pub fn set_value(&mut self, value: TypeValue) {
        self.value = value;
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

    pub fn empty(token: Token) -> Self {
        Symbol {
            name: "".into(),
            kind: SymbolKind::Empty,
            ty: TypeDef::Unknown,
            args: vec![],
            value: TypeValue::Unknown,
            token,
        }
    }

    pub fn new(
        name: ShortString,
        kind: SymbolKind,
        ty: TypeDef,
        args: Vec<Symbol>,
        token: Token,
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
            token,
        }
    }

    // Return the type of:
    // - this symbol if it represents a Record OR
    // - this symbol if it's a leaf node (args are empty) OR
    // - the last symbol of the args, if that's a leaf node OR
    // Used to construct the return type of a variable that is being
    // assigned.
    pub(crate) fn get_recursive_return_type(&self) -> TypeDef {
        if let TypeDef::Record(_ty) = self.get_type() {
            return self.ty.clone();
        }
        if self.args.is_empty() {
            self.ty.clone()
        } else if let Some(last_arg) = self.args.last() {
            last_arg.get_type()
        } else {
            unreachable!()
        }
    }

    // This function tries to convert a symbol with a given type into a
    // symbol with another type and set its value according to the new
    // type, if it has a a value. If the conversion is not possible
    // it returns an error. Used during the evaluation phase.
    pub fn try_convert_type_value_into(
        mut self,
        into_ty: TypeDef,
    ) -> Result<Self, CompileError> {
        if self.ty == into_ty {
            return Ok(self);
        }

        if self.ty.clone().test_type_conversion(into_ty.clone()) {
            trace!(
                "Type conversion possible from {} into {}",
                self.ty,
                into_ty
            );
            self.ty = into_ty.clone();
            if let TypeValue::Unknown = self.value {
                trace!("Value is Unknown");
            } else {
                self.value = self.value.into_type(&into_ty)?;
            }
        } else {
            trace!(
                "Type conversion NOT possible from {} into {}",
                self.ty,
                into_ty
            );
            return Err(CompileError::from(format!(
                "{} cannot be converted into {}",
                self.ty, into_ty
            )));
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
        Some(self.token.cmp(&other.token))
    }
}

impl std::cmp::Ord for Symbol {
    fn cmp(&self, other: &Self) -> Ordering {
        self.token.cmp(&other.token)
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Symbol) -> bool {
        self.token == other.token && self.name == other.name
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum SymbolKind {
    // Assignment Symbols
    VariableAssignment, // A variable defined by the user

    // assignment+access receiver symbols these symbols are used both for
    // assignment and as root access receivers.
    Constant, // A literal value or a filter-map-level variable
    // Rx and Tx Types
    // The payload can be a mutable type, that comes in at the input of the
    // VM, and is mutated. In that case there is no separate Rx and Tx types:
    // they have to be the same type. These two types are the indicators for
    // that situation.
    PassThroughRxTxType, // type of the mutable incoming & outgoing payload
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
    SplitRxType, // type of the incoming payload
    SplitTxType, // type of the outgoing payload

    // data sources access receivers
    Rib,
    Table,
    OutputStream,
    PrefixList,

    // access receiver symbols
    AccessReceiver,
    // these symbols are only used to as roots for receiving
    // data.
    Argument,      // a passed-in filter_map or term level argument
    AnonymousType, // type of a sub-record
    NamedType,     // User-defined type of a record

    GlobalEnum,  // reference to a complete globally defined enum
    EnumVariant, // one of the variants of an enum

    // accessor symbols
    // symbols that come after an access receiver in the args list.

    // A method call, that will either mutate the typevalue this SymbolKind
    // lives on when the data field is true, otherwise it will just read it
    // (to produce a new typevalue).
    MethodCallbyRef,
    MethodCallByConsumedValue,
    // A method call on a built-in type, e.g. `AsPath.contains()`
    BuiltInTypeMethodCall,
    // A method call that's a builtin of the roto language, e.g.
    // `send-to-log`
    GlobalMethodCall,
    FieldAccess,
    // (multi-)indexed access on a LazyRecord
    LazyFieldAccess,

    // term symbols
    LogicalExpr,
    BooleanExpr,
    CompareExpr(CompareOp),
    // The comparison of the one value against a list with, i.e. 'in'
    // or 'not in'
    ListCompareExpr(CompareOp),
    AndExpr,
    OrExpr,
    NotExpr,

    // apply symbols
    MatchAction(MatchActionType),
    ActionSection,
    ActionCall,
    Term,
    TermCall,

    // A symbol that has been consumed by the compiler
    Empty,
}

#[derive(Debug)]
pub struct MatchAction {
    pub(crate) _quantifier: MatchActionQuantifier,
    pub(crate) symbol: Symbol,
}

impl MatchAction {
    pub fn get_name(&self) -> ShortString {
        self.symbol.get_name()
    }

    pub fn get_type(&self) -> TypeDef {
        self.symbol.get_type()
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ActionType {
    Named,
    Empty,
}

#[derive(Debug, Copy, Clone)]
pub enum MatchActionQuantifier {
    MatchesAny,
    MatchesVariant,
    MatchesExactlyOne,
    MatchesEvery,
}

#[derive(Debug, Hash, Copy, Clone, Eq, PartialEq)]
pub enum MatchActionType {
    FilterMatchAction,
    PatternMatchAction,
    NegateMatchAction,
    // EmptyAction, // An MatchAction without an action, just a accept or reject
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

//------------ SymbolTable --------------------------------------------------

// A per-filter-map symbol table.
#[derive(Debug)]
pub struct SymbolTable {
    scope: Scope,
    // The input payload type of the filter_map.
    rx_type: Symbol,
    // The output payload type of the filter_map. If it's none its identical
    // to the input payload type.
    tx_type: Option<Symbol>,
    // The special symbols that will be filled in at runtime, once per filter
    // run.
    arguments: HashMap<ShortString, Symbol>,
    // The variables and constants that are defined in the filter_map.
    variables: HashMap<ShortString, Symbol>,
    // The evaluated `term` sections that are defined in the filter_map.
    term_sections: HashMap<ShortString, Symbol>,
    // The evaluated `action` sections that are defined in the filter_map.
    action_sections: HashMap<ShortString, Symbol>,
    // All the `filter` clauses in the `apply` section, the tie actions to
    // terms.
    match_action_sections: Vec<MatchAction>,
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
    pub(crate) used_variables: crate::compiler::compile::Variables<'a>,
    pub(crate) used_arguments: Vec<(ShortString, &'a Symbol)>,
    pub(crate) used_data_sources: crate::compiler::compile::DataSources<'a>,
}

// struct Location {
//     name: ShortString,
//     filter_map: ShortString,
//     line: usize,
// }

impl SymbolTable {
    pub(crate) fn new(filter_map: &Scope) -> Self {
        SymbolTable {
            scope: filter_map.clone(),
            rx_type: Symbol::empty(Token::NonTerminal),
            tx_type: None,
            arguments: HashMap::new(),
            variables: HashMap::new(),
            term_sections: HashMap::new(),
            action_sections: HashMap::new(),
            match_action_sections: vec![],
            default_action: crate::ast::AcceptReject::Accept,
        }
    }

    pub(crate) fn get_scope(&self) -> Scope {
        self.scope.clone()
    }

    pub(crate) fn get_type(&self) -> Result<FilterType, CompileError> {
        match self.scope {
            Scope::Filter(_) => Ok(FilterType::Filter),
            Scope::FilterMap(_) => Ok(FilterType::FilterMap),
            Scope::Global => Err(CompileError::Internal(
                "Global scope is not a filter.".into(),
            )),
        }
    }

    pub(crate) fn validate_variable_name(
        &self,
        key: &ShortString,
    ) -> Result<(), CompileError> {
        if self.variables.contains_key(key) {
            return Err(format!(
                "Symbol {} already defined in scope {}",
                key, self.scope
            )
            .into());
        }

        match GlobalEnumTypeDef::get_enum_for_variant_as_token(key) {
            // Is it the name of an existing global enum variant? btw we may
            // want to relax this some day. It's not an actual name-spacing
            // error or anything, it's just here to avoid confusion.
            Err(existing_enum_var) if !existing_enum_var.is_empty() => {
                Err(format!(
                    "Can't shadow existing enum variant: {:?} in enum {:?}",
                    key, existing_enum_var
                )
                .into())
            }
            // Is it the name of a global enum itself? This is an actual
            // name-spacing error. If we allow this a global enum gets
            // overridden and therefore unreachable.
            Ok(global_enum) => Err(format!(
                "Can't shadow global enum name: {:?} in enum {:?}",
                key, global_enum
            )
            .into()),
            Err(_) => Ok(()),
        }
    }

    pub(crate) fn move_var_or_const_into(
        &mut self,
        mut symbol: Symbol,
    ) -> Result<(), CompileError> {
        let key = symbol.get_name();

        self.validate_variable_name(&key)?;

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

        self.validate_variable_name(&name)?;

        let token_int = self.variables.len();

        let token = match kind {
            SymbolKind::Rib => Token::Rib(token_int),
            // Treat PrefixList like a table, they share the same methods.
            SymbolKind::Table | SymbolKind::PrefixList => {
                Token::Table(token_int)
            }
            SymbolKind::OutputStream => Token::OutputStream(token_int),
            _ => Token::Variable(token_int),
        };

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
            trace!(
                "k {} n {} k {:?} t {} a {:?} v {} ",
                key,
                name,
                kind,
                ty,
                args,
                value
            );
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
            SymbolKind::SplitRxType => Token::RxType(ty.clone()),
            SymbolKind::SplitTxType => Token::TxType,
            SymbolKind::PassThroughRxTxType => Token::RxType(ty.clone()),
            _ => Token::Argument(token_int),
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
        term_section_key: ShortString,
        term_section_index: usize,
        child_symbol: Symbol,
    ) -> Result<(), CompileError> {
        // let term_token = Some(Token::TermSection);

        if let Entry::Vacant(term) =
            self.term_sections.entry(term_section_key.clone())
        {
            term.insert(Symbol {
                name: term_section_key.clone(),
                kind: SymbolKind::Term,
                ty: TypeDef::Boolean,
                args: vec![child_symbol],
                value: TypeValue::Unknown,
                token: Token::TermSection(term_section_index),
            });
        } else {
            let child_args = &mut self
                .term_sections
                .get_mut(&term_section_key)
                .ok_or_else(|| {
                    CompileError::Internal(format!(
                        "Cannot add logical formula from {:?}",
                        term_section_key
                    ))
                })?
                .args;
            child_args.push(child_symbol);
        }

        Ok(())
    }

    pub(crate) fn add_action_section(
        &mut self,
        key: ShortString,
        mut action: Symbol,
    ) -> Result<(), CompileError> {
        let token_int = self.action_sections.len();
        let token = Token::ActionSection(token_int);

        action.token = token;
        self.action_sections.insert(key, action);

        Ok(())
    }

    pub(crate) fn move_match_action_into(
        &mut self,
        symbol: Symbol,
        quantifier: MatchActionQuantifier,
    ) -> Result<(), CompileError> {
        if let SymbolKind::MatchAction(_) | SymbolKind::GlobalEnum =
            symbol.get_kind()
        {
            self.match_action_sections.push(MatchAction {
                symbol,
                _quantifier: quantifier,
            })
        } else {
            return Err(CompileError::from(format!(
                "Symbol with name {} is not a match-action",
                symbol.get_name()
            )));
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

        if let Some(symbol) = self.term_sections.get(name) {
            return Some(symbol);
        }

        if let Some(symbol) = self.action_sections.get(name) {
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
            .ok_or_else(|| {
                format!("Symbol '{}' not found as variable", name).into()
            })
    }

    // Retrieve the symbol from the `variables` table of a filter_map the entry
    // Used in the compile stage to build the command stack.

    // panics if the symbol is not found.
    pub(crate) fn get_variable_by_token(
        &self,
        token: &Token,
    ) -> Result<&Symbol, CompileError> {
        match token {
            Token::Variable(_token_int) => {
                if let Some(found_var) =
                    self.variables.values().find(|s| s.token == token.clone())
                {
                    Ok(found_var)
                } else {
                    Err(CompileError::Internal(format!(
                        "Cannot find variable with token '{:?}'",
                        token
                    )))
                }
            }
            _ => Err(CompileError::Internal(format!(
                "Token '{:?}' does not represent a variable.",
                token
            ))),
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

    pub(crate) fn get_argument_mut(
        &mut self,
        name: &ShortString,
    ) -> Result<&mut Symbol, CompileError> {
        self.arguments.get_mut(name).ok_or_else(|| {
            format!("Symbol of type Argument '{}' not found", name).into()
        })
    }

    pub(crate) fn get_term_section_type_and_token(
        &self,
        name: &ShortString,
    ) -> Result<(TypeDef, Token), CompileError> {
        self.term_sections
            .get(name)
            .ok_or_else(|| {
                format!("Symbol '{}' not found as term", name).into()
            })
            .map(|term| (term.ty.clone(), term.token.clone()))
    }

    // Return the type of the first argument of the symbol that lives in this
    // hashmap, used by the evaluator when looking to resolve arguments.
    pub(crate) fn get_type_of_argument(
        &self,
        name: &ShortString,
        index: usize,
    ) -> Option<TypeDef> {
        self.term_sections
            .get(name)
            .and_then(|kv| kv.get_args().get(index).map(|kv| kv.get_type()))
    }

    pub(crate) fn get_terms(&self) -> Vec<&Symbol> {
        self.term_sections.values().collect::<Vec<_>>()
    }

    pub(crate) fn get_action_section(
        &self,
        name: &ShortString,
    ) -> Result<(TypeDef, Token), CompileError> {
        self.action_sections
            .get(name)
            .ok_or_else(|| {
                format!(
                    "Cannot find action named '{}' in the global table.",
                    name
                )
                .into()
            })
            .map(|action| (action.ty.clone(), action.token.clone()))
    }

    pub(crate) fn get_action_sections(&self) -> Vec<&Symbol> {
        self.action_sections.values().collect::<Vec<_>>()
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

    pub(crate) fn get_match_action_sections(&self) -> Vec<&MatchAction> {
        self.match_action_sections.iter().collect::<Vec<_>>() //.values().collect::<Vec<_>>()
    }

    pub(crate) fn get_data_source(
        &self,
        name: &ShortString,
    ) -> Result<(TypeDef, Token), CompileError> {
        let src: Result<&Symbol, CompileError> = self
            .variables
            .values()
            .find(|kv| kv.get_name() == name)
            .ok_or_else(|| {
                format!("Symbol '{}' not found as data source", name).into()
            });

        src.map(|r| match r.get_token() {
            Token::Rib(_) => {
                r.get_kind_type_and_token().map(|ktt| (ktt.1, ktt.2))
            }
            Token::Table(_) => {
                Ok((TypeDef::Table(Box::new(r.get_type())), r.get_token()))
            }
            _ => Err(CompileError::new(format!(
                "No data source named '{}' found.",
                name
            ))),
        })?
    }

    // retrieve all the unique arguments, variables and data-sources that are
    // referenced in the terms field of a symbol table (i.e. the terms
    // sections in a filter_map)
    pub(crate) fn create_deps_graph(
        &self,
    ) -> Result<
        DepsGraph, // (variables, arguments, data sources)
        CompileError,
    > {
        // First, go over all the match actions and see which Terms sections,
        // Action sections and Variables they refer to.

        let mut deps_set: Vec<&Symbol> = vec![];
        for ma in self
            .match_action_sections
            .iter()
            .map(|ma| &ma.symbol)
            .collect::<Vec<_>>()
        {
            deps_set.extend(ma.flatten_nodes());
        }

        // collect all the symbols in all the term sections mentioned in the
        // match actions.
        for ts in &self.term_sections {
            if deps_set
                .iter()
                .any(|ma| ts.1.get_token().is_term() && ma.name == ts.0)
            {
                deps_set.extend(ts.1.flatten_nodes());
            }
        }

        // collect all the symbols in all the action sections mentioned in the
        // match actions.
        for ts in &self.action_sections {
            if deps_set
                .iter()
                .any(|ma| ts.1.get_token().is_action() && ma.name == ts.0)
            {
                deps_set.extend(ts.1.flatten_nodes());
            }
        }

        let DepsGraph {
            // rx_type,
            // tx_type,
            mut used_variables,
            mut used_arguments,
            mut used_data_sources,
            ..
        } = self._partition_deps_graph(deps_set).map_err(|_e| {
            CompileError::Internal(
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

        Ok(DepsGraph {
            rx_type: Some((self.rx_type.get_name(), self.rx_type.get_type())),
            tx_type: self
                .tx_type
                .as_ref()
                .map(|s| (s.get_name(), s.get_type())),
            used_variables,
            used_arguments,
            used_data_sources,
        })
    }

    fn _partition_deps_graph<'a>(
        &'a self,
        mut deps_vec: Vec<&'a Symbol>,
    ) -> Result<DepsGraph, CompileError> {
        deps_vec.retain(|t| match t.get_token() {
            t => t.is_variable() || t.is_argument() || t.is_data_source(),
            _ => false,
        });

        let (args_vec, vars_srcs_vec): (Vec<&Symbol>, Vec<&Symbol>) =
            deps_vec
                .into_iter()
                .partition(|s| s.get_token().is_argument());

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
            .partition(|s| s.get_token().is_variable());

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

    pub(crate) fn create_global_methods(&mut self) {
        // self.add_variable(key, name, kind, ty, args, value);
    }
}
