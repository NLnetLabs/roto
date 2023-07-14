use log::trace;

use crate::ast::AcceptReject;
use crate::ast::LogicalExpr;
use crate::ast::ShortString;
use crate::compile::CompileError;
use crate::symbols::GlobalSymbolTable;
use crate::symbols::MatchActionType;
use crate::symbols::Scope;
use crate::symbols::Symbol;
use crate::symbols::SymbolKind;
use crate::traits::Token;
use crate::types::builtin::Boolean;
use crate::types::builtin::BuiltinTypeValue;
use crate::types::builtin::HexLiteral;
use crate::types::builtin::IntegerLiteral;
use crate::types::builtin::PrefixLength;
use crate::types::builtin::StringLiteral;
use crate::types::constant_enum::global_enums;
use crate::types::typedef::NamedTypeDef;

use super::ast;
use super::symbols;

use super::types::typedef::TypeDef;
use super::types::typevalue::TypeValue;

use std::convert::From;

impl<'a> ast::SyntaxTree {
    pub fn eval(
        &'a self,
        symbols: GlobalSymbolTable,
    ) -> Result<(), CompileError> {
        let (filter_maps, global): (Vec<_>, Vec<_>) = self
            .expressions
            .iter()
            .partition(|e| matches!(e, ast::RootExpr::FilterMap(_)));

        // First, evaluate all the non-filter-map expressions, so that they are
        // available to the filter_maps.

        // If the global symbol table does not exist, create it.
        let mut symbols_mut = symbols.borrow_mut();
        let global_scope = symbols::Scope::Global;

        let global_symbols = if symbols_mut.contains_key(&global_scope) {
            symbols_mut.get_mut(&global_scope).unwrap()
        } else {
            let mut global_table = symbols::SymbolTable::new(&global_scope);
            global_table.create_global_methods();

            symbols_mut.insert(global_scope.clone(), global_table);
            symbols_mut.get_mut(&global_scope).unwrap()
        };

        for expr in &global {
            match expr {
                ast::RootExpr::Rib(rib) => rib.eval(global_symbols)?,
                ast::RootExpr::Table(table) => table.eval(global_symbols)?,
                ast::RootExpr::OutputStream(stream) => {
                    stream.eval(global_symbols)?
                }
                ast::RootExpr::Ty(rt_assign) => {
                    rt_assign.eval(global_symbols)?
                }
                _ => {}
            };
        }

        // For each filter_map, create a new symbol table if it does not exist.
        for filter_map in &filter_maps {
            let filter_map_name = &filter_map.get_filter_map()?.ident.ident;
            let filter_map_scope = symbols::Scope::FilterMap(filter_map_name.clone());

            if let std::collections::hash_map::Entry::Vacant(e) =
                symbols_mut.entry(filter_map_scope.clone())
            {
                e.insert(symbols::SymbolTable::new(&Scope::FilterMap(
                    filter_map_name.clone(),
                )));
                symbols_mut.get_mut(&global_scope).unwrap()
            } else {
                symbols_mut.get_mut(&filter_map_scope).unwrap()
            };
        }
        drop(symbols_mut);

        // Now, evaluate all the define sections in modules, so that filter_maps
        // can use each other's types and variables.
        for filter_map in &filter_maps {
            if let ast::RootExpr::FilterMap(m) = filter_map {
                m.eval_define_header(symbols.clone())?;
            }
        }

        // Finally, evaluate all the filter_maps themselves.
        for filter_map in &filter_maps {
            if let ast::RootExpr::FilterMap(m) = filter_map {
                m.eval(symbols.clone())?;
            }
        }
        trace!("Evaluated successfully");

        Ok(())
    }
}

impl<'a> ast::Rib {
    fn eval(
        &'a self,
        symbols: &'_ mut symbols::SymbolTable,
    ) -> Result<(), CompileError> {
        let child_kvs = self.body.eval(self.ident.clone().ident, symbols)?;

        // create a new user-defined type for the record type in the RIB
        let rec_type = TypeDef::new_record_type_from_short_string(child_kvs)?;

        // add a symbol for the user-defined type, the name is derived from
        // the 'contains' clause
        symbols.add_variable(
            self.contain_ty.ident.clone(),
            None,
            symbols::SymbolKind::NamedType,
            rec_type.clone(),
            vec![],
            TypeValue::Unknown,
        )?;

        // add a symbol for the RIB itself, using the newly created record
        // type
        symbols.add_variable(
            self.ident.ident.clone(),
            None,
            symbols::SymbolKind::Rib,
            rec_type,
            vec![],
            TypeValue::Unknown,
        )?;

        Ok(())
    }
}

impl<'a> ast::Table {
    fn eval(
        &'a self,
        symbols: &'_ mut symbols::SymbolTable,
    ) -> Result<(), CompileError> {
        let child_kvs = self.body.eval(self.ident.clone().ident, symbols)?;

        // create a new user-defined type for the record type in the table
        let rec_type = TypeDef::new_record_type_from_short_string(child_kvs)?;

        // add a symbol for the user-defined type, the name is derived from
        // the 'contains' clause
        symbols.add_variable(
            self.contain_ty.ident.clone(),
            None,
            symbols::SymbolKind::NamedType,
            rec_type.clone(),
            vec![],
            TypeValue::Unknown,
        )?;

        // add a symbol for the RIB itself, using the newly created record
        // type
        symbols.add_variable(
            self.ident.ident.clone(),
            None,
            symbols::SymbolKind::Table,
            rec_type,
            vec![],
            TypeValue::Unknown,
        )?;

        Ok(())
    }
}

impl<'a> ast::OutputStream {
    fn eval(
        &'a self,
        symbols: &'_ mut symbols::SymbolTable,
    ) -> Result<(), CompileError> {
        let child_kvs = self.body.eval(self.ident.clone().ident, symbols)?;

        // create a new user-defined type for the record type in the table
        let rec_type = TypeDef::new_record_type_from_short_string(child_kvs)?;

        // add a symbol for the user-defined type, the name is derived from
        // the 'contains' clause
        symbols.add_variable(
            self.contain_ty.ident.clone(),
            None,
            symbols::SymbolKind::NamedType,
            rec_type.clone(),
            vec![],
            TypeValue::Unknown,
        )?;

        // add a symbol for the RIB itself, using the newly created record
        // type
        symbols.add_variable(
            self.ident.ident.clone(),
            None,
            symbols::SymbolKind::OutputStream,
            rec_type,
            vec![],
            TypeValue::Unknown,
        )?;

        Ok(())
    }
}

impl<'a> ast::RecordTypeAssignment {
    fn eval(
        &'a self,
        symbols: &'_ mut symbols::SymbolTable,
    ) -> Result<(), CompileError> {
        self.record_type.eval(
            self.ident.ident.clone(),
            symbols::SymbolKind::NamedType,
            symbols,
        )?;

        Ok(())
    }
}

impl<'a> ast::RibBody {
    fn eval(
        &'a self,
        parent_name: ast::ShortString,
        symbols: &'_ mut symbols::SymbolTable,
    ) -> Result<Vec<NamedTypeDef>, CompileError> {
        let mut kvs: Vec<NamedTypeDef> = vec![];

        for kv in self.key_values.iter() {
            match kv {
                ast::RibField::PrimitiveField(f) => {
                    kvs.push((
                        f.field_name.ident.as_str().into(),
                        Box::new(f.ty.clone().try_into()?),
                    ));
                }
                ast::RibField::RecordField(r) => {
                    let nested_record = ast::RecordTypeIdentifier::eval(
                        &r.1,
                        format!(
                            "{}.{}",
                            parent_name,
                            String::from(r.0.ident.as_str()).to_lowercase()
                        )
                        .as_str()
                        .into(),
                        symbols::SymbolKind::AnonymousType,
                        symbols,
                    )?;

                    kvs.push((
                        r.0.ident.as_str().into(),
                        Box::new(TypeDef::Record(nested_record)),
                    ));
                }
                ast::RibField::ListField(l) => {
                    kvs.push((
                        l.0.ident.as_str().into(),
                        Box::new(TypeDef::List(Box::new(
                            l.1.inner_type.clone().try_into()?,
                        ))),
                    ));
                }
            }
        }

        Ok(kvs)
    }
}

impl<'a> ast::RecordTypeIdentifier {
    fn eval(
        &'a self,
        name: ast::ShortString,
        kind: symbols::SymbolKind,
        symbols: &'_ mut symbols::SymbolTable,
    ) -> Result<Vec<NamedTypeDef>, CompileError> {
        let mut kvs: Vec<NamedTypeDef> = vec![];

        for kv in self.key_values.iter() {
            match kv {
                ast::RibField::PrimitiveField(f) => {
                    kvs.push((
                        f.field_name.ident.as_str().into(),
                        Box::new(f.ty.clone().try_into()?),
                    ));
                }
                ast::RibField::RecordField(r) => {
                    let nested_record = ast::RecordTypeIdentifier::eval(
                        &r.1,
                        format!(
                            "{}.{}",
                            name,
                            String::from(r.0.ident.as_str()).to_lowercase()
                        )
                        .as_str()
                        .into(),
                        symbols::SymbolKind::AnonymousType,
                        symbols,
                    )?;

                    kvs.push((
                        r.0.ident.as_str().into(),
                        Box::new(TypeDef::Record(nested_record)),
                    ));
                }
                ast::RibField::ListField(l) => {
                    kvs.push((
                        l.0.ident.as_str().into(),
                        Box::new(TypeDef::List(Box::new(
                            l.1.inner_type.clone().try_into()?,
                        ))),
                    ));
                }
            }
        }

        let record = TypeDef::Record(kvs.clone());
        symbols.add_variable(
            name,
            None,
            kind,
            record,
            vec![],
            TypeValue::Unknown,
        )?;

        Ok(kvs)
    }
}

impl ast::FilterMap {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
    ) -> Result<(), CompileError> {
        let filter_map_scope = symbols::Scope::FilterMap(self.ident.ident.clone());
        // Check the `with` clause for additional arguments.
        let with_kv: Vec<_> = self.with_kv.clone();
        let _with_ty = with_kv
            .into_iter()
            .map(|ty| {
                declare_argument(
                    ty.clone().field_name.ident,
                    ty,
                    symbols::SymbolKind::Constant,
                    symbols.clone(),
                    &symbols::Scope::FilterMap(self.ident.ident.clone()),
                )
            })
            .collect::<Vec<_>>();

        // first, parse the define section, so that other sections in this
        // filter_map can use the defined variables.
        self.body
            .define
            .eval(symbols.clone(), filter_map_scope.clone())?;

        let (terms, actions): (Vec<_>, Vec<_>) = self
            .body
            .expressions
            .iter()
            .partition(|s| matches!(s, ast::FilterMapExpr::Term(_t)));

        for term in terms.into_iter() {
            if let ast::FilterMapExpr::Term(t) = term {
                t.eval(symbols.clone(), filter_map_scope.clone())?;
            }
        }

        for action in actions.into_iter() {
            if let ast::FilterMapExpr::Action(a) = action {
                a.eval(symbols.clone(), filter_map_scope.clone())?;
            }
        }

        if let Some(apply) = &self.body.apply {
            apply.eval(symbols, filter_map_scope)?;
        }

        Ok(())
    }

    fn eval_define_header(
        &self,
        symbols: symbols::GlobalSymbolTable,
    ) -> Result<(), CompileError> {
        // Check the `with` clause for additional arguments.
        let with_kv: Vec<_> = self.body.define.with_kv.clone();

        // The `with` clause of the `define` section acts as an extra
        // argument to the whole filter_map, that can be used as a extra
        // read-only payload.
        let _with_ty = with_kv
            .into_iter()
            .map(|ty| {
                declare_argument(
                    ty.clone().field_name.ident,
                    ty,
                    symbols::SymbolKind::Argument,
                    symbols.clone(),
                    &symbols::Scope::FilterMap(self.ident.ident.clone()),
                )
            })
            .collect::<Vec<_>>();

        Ok(())
    }
}

impl ast::Define {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: symbols::Scope,
    ) -> Result<(), CompileError> {
        // The default input-argument is defined by the 'rx' keyword in the
        // `define` section. This the argument that holds the payload at
        // runtime.
        let rx_kind;
        let rx_type = match &self.body.rx_tx_type {
            ast::RxTxType::Split(rx_type, _tx_type) => {
                rx_kind = SymbolKind::SplitRxType;
                rx_type
            }
            ast::RxTxType::PassThrough(rx_tx_type) => {
                rx_kind = SymbolKind::PassThroughRxTxType;
                rx_tx_type
            }
        };

        declare_argument(
            rx_type.field_name.ident.clone(),
            rx_type.clone(),
            rx_kind,
            symbols.clone(),
            &scope,
        )?;

        // The default output-argument is defined by the 'tx' keyword in the
        // 'define' section. This is the argument that will be created by
        // this filter-filter-map on each run. We start with an empty record of
        // the specified type.
        let tx_kind;
        match &self.body.rx_tx_type {
            ast::RxTxType::Split(_rx_type, tx_type) => {
                tx_kind = SymbolKind::SplitTxType;
                declare_argument(
                    tx_type.field_name.ident.clone(),
                    tx_type.clone(),
                    tx_kind,
                    symbols.clone(),
                    &scope,
                )?;
            }
            ast::RxTxType::PassThrough(rx_tx_type) => {
                assert!(check_type_identifier(
                    rx_tx_type.ty.clone(),
                    symbols.clone(),
                    &scope
                )
                .is_ok());
            }
        };

        for assignment in &self.body.assignments {
            // rhs part of the assignment can only be an Argument Expression.
            let s = ast::ValueExpr::eval(
                &assignment.1,
                symbols.clone(),
                scope.clone(),
            )?;

            // we only allow typed record instances, an anonymous type would
            // be ambiguous, since we don't know the different contexts where
            // it will be used: the type inferrence may lead to a different
            // type in different contexts, and then the type woudln't be
            // equal to itself, which doesn't sound good (pun!).
            if let Ok(Token::AnonymousRecord) = s.get_token() {
                return Err(CompileError::from(
                    format!(
                        "Assignment to Anonymous Record type not allowed in `define` section for variable '{}'.",
                        assignment.0.ident
                )));
            }

            trace!("DECLARE VAR {} = {:#?}", assignment.0.ident, s);
            // lhs of the assignment represents the name of the variable or
            // constant.
            declare_variable_from_symbol(
                assignment.0.ident.clone(),
                s,
                symbols.clone(),
                &scope,
            )?;
        }

        Ok(())
    }
}

impl ast::Term {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: symbols::Scope,
    ) -> Result<(), CompileError> {
        let term_scopes = &self.body.scopes;
        for term in term_scopes[0].match_exprs.iter().enumerate() {
            let logical_formula = match &term.1 {
                LogicalExpr::BooleanExpr(expr) => {
                    // Boolean expressions may actually be a (sub)term that
                    // isn't a boolean at this stage. We should be able to
                    // convert it into one, though, otherwise it's an
                    // error (and not false!).
                    let expr = ast::BooleanExpr::eval(
                        expr,
                        symbols.clone(),
                        &scope,
                    )?;
                    if expr.get_type() == TypeDef::Boolean
                        || expr
                            .get_type()
                            .test_type_conversion(TypeDef::Boolean)
                    {
                        expr
                    } else {
                        return Err(CompileError::from(format!(
                            "Cannot convert value with type {} into Boolean",
                            expr.get_type()
                        )));
                    }
                }
                LogicalExpr::OrExpr(or_expr) => {
                    ast::OrExpr::eval(or_expr, symbols.clone(), &scope)?
                }
                LogicalExpr::AndExpr(and_expr) => {
                    ast::AndExpr::eval(and_expr, symbols.clone(), &scope)?
                }
                LogicalExpr::NotExpr(not_expr) => {
                    ast::NotExpr::eval(not_expr, symbols.clone(), &scope)?
                }
            };

            add_logical_formula(
                Some(self.ident.ident.clone()),
                logical_formula,
                symbols.clone(),
                &scope,
            )?;
        }
        Ok(())
    }
}

// =========== Actions ======================================================

impl ast::Action {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: symbols::Scope,
    ) -> Result<(), CompileError> {
        let _symbols = symbols.borrow();

        let mut action_exprs = vec![];

        for compute_expr in &self.body.expressions {
            // The Access Receiver may have an identifier, in which case it
            // may be the incoming or outgoing variable name.
            //
            // The incoming/outgoing payload variables are the only
            // variables that can be used in the 'action' section. The
            // incoming payload variable has either
            // SymolKind::SplitRxType/SplitTxType OR PassthroughRxTxType as
            // type.
            //
            // If the Access Receiver does not have an identifier it is
            // something global, in the context of an actions this can only
            // be a global method call.
            //
            // Method Calls on Roto Types are also allowed, e.g.
            // `String.format(..)`
            let ar_name = match compute_expr.get_receiver_ident() {
                Ok(name) => name,
                Err(_) => compute_expr.access_expr[0].get_ident().clone(),
            };

            let mut s = compute_expr.eval(
                Some(format!("sub-action-{}", ar_name).as_str().into()),
                symbols.clone(),
                scope.clone(),
            )?;

            s = s.set_kind(SymbolKind::AccessReceiver);

            action_exprs.push(s);
        }

        drop(_symbols);

        let action = symbols::Symbol::new(
            self.ident.ident.clone(),
            symbols::SymbolKind::Action,
            TypeDef::Unknown,
            action_exprs,
            None,
        );

        add_action(
            self.ident.ident.clone(),
            action,
            symbols.clone(),
            &scope,
        )?;

        Ok(())
    }
}

//------------ Apply --------------------------------------------------------

impl ast::Apply {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: symbols::Scope,
    ) -> Result<(), CompileError> {
        let mut _symbols = symbols.borrow_mut();
        let _filter_map_symbols = _symbols.get_mut(&scope).ok_or_else(|| {
            format!("No symbols found for filter-map {}", scope)
        })?;

        // There can only be one `apply` section in a filter_maps, so we can set
        // the default action from the apply section for the whole filter_map.
        if let Some(accept_reject) = self.body.accept_reject {
            _filter_map_symbols.set_default_action(accept_reject);
        } else {
            _filter_map_symbols.set_default_action(AcceptReject::Accept);
        }

        drop(_symbols);

        for a_scope in &self.body.scopes {
            let s = a_scope.eval(symbols.clone(), scope.clone())?;
            add_match_action(s.get_name(), s, symbols.clone(), &scope)?;
        }

        Ok(())
    }
}

impl ast::ApplyScope {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: symbols::Scope,
    ) -> Result<symbols::Symbol, CompileError> {
        let _symbols = symbols.borrow();
        let filter_map_symbols = _symbols.get(&scope).ok_or_else(|| {
            format!("No symbols found for filter-map {}", scope)
        })?;

        // not doing anything with the actual ApplyScope (the use statement),
        // not sure whether it is going to be needed.
        let _s_name = self.scope.clone().map(|s| s.ident);

        let term = self.filter_ident.eval(symbols.clone(), scope.clone())?;
        let (_ty, token) = filter_map_symbols.get_term(&term.get_name())?;

        let mut args_vec = vec![];
        for action in &self.actions {
            if let Some(match_action) = action.0.clone() {
                // If there's one or more actions in the filter block we
                // will store them as args in the vector.
                let match_action_name = match_action
                    .eval(symbols.clone(), scope.clone())?
                    .get_name();

                let (_ty, token) =
                    filter_map_symbols.get_action(&match_action_name)?;

                let s = symbols::Symbol::new(
                    match_action_name,
                    symbols::SymbolKind::Action,
                    TypeDef::AcceptReject(
                        action.1.unwrap_or(ast::AcceptReject::NoReturn),
                    ),
                    vec![],
                    Some(token),
                );
                args_vec.push(s);
            } else {
                // If there's no Action mentioned in a filter block, we will
                // create a MatchAction of type Empty, so that the compiler
                // can invoke the right accept/reject commands. The action
                // symbol gets to have the name of the term, but it is never
                // inspected, since it doesn't exist in any symbol map.
                let s = symbols::Symbol::new(
                    term.get_name(),
                    symbols::SymbolKind::MatchAction(MatchActionType::EmptyAction),
                    TypeDef::AcceptReject(
                        action.1.ok_or_else(
                            || CompileError::from(
                                "Encountered MatchAction without Action or Accept Reject statement."
                            )
                        )?,
                    ),
                    vec![],
                    None
                );
                args_vec.push(s);
            }
        }
        let s = symbols::Symbol::new(
            term.get_name(),
            if self.negate {
                symbols::SymbolKind::MatchAction(MatchActionType::MatchAction)
            } else {
                symbols::SymbolKind::MatchAction(
                    MatchActionType::NegateMatchAction,
                )
            },
            // The AcceptReject value from the Apply section does not end up
            // here, instead it lives on the ApplyBody, and it is saved in the
            // symboltable of the filter_map.
            TypeDef::Unknown,
            args_vec,
            Some(token),
        );

        drop(_symbols);

        Ok(s)
    }
}

// =========== Nested AST Nodes =============================================

// These are types that can be nested inside of other types, or are used
// recursively. They return the symbols that they create, unlike the types
// that go directly in the root of a SymbolTable.
// The caller needs to insert them in the right place in a entry in the symbol
// table.

//------------ ComputeExpr --------------------------------------------------

// An expression that computes a return value based on the AccessReceiver,
// (the `receiver` field), the root of the expression and its arguments
// (`args` field). Each argument is a dot-divided part of the expression.
// A compute expression may have an arbitrary number of arguments, e.g.
// the compute expression `my_var.a.b.c().d` will have 4 arguments and an
// access receiver 'my_var'.

// Access Receiver
//
// The access receiver is the data source that provides a combination of
// fields to be accessed and methods to be called. The access receiver will
// be encoded in the resulting symbol in its `kind` and `token` fields.
// The resulting symbol will always have a `kind` set to `AccessReceiver`
// and the type of the access receiever will be encoded in the token, e.g.
//
// Symbol {
//      name: 'my_var',
//      kind: SymbolKind::AccessReceiver,
//      args: [see below]
//      ...,
//      token: DataSource(1)
// }
//
// Arguments
//
// These are the expressions that are divided by dots, e.g. a.b.c().d, will
// turn up as:
//
// args: [ FieldAccessSymbolA, FieldAccessSymbolB, MethodCallSymbolC,
//         FieldAccessSymbolC, FieldAccessSymbolD ]
//

impl ast::ComputeExpr {
    pub(crate) fn eval(
        &self,
        // If no name is provided we use the ident of the access receiver
        name: Option<ShortString>,
        symbols: symbols::GlobalSymbolTable,
        scope: symbols::Scope,
    ) -> Result<symbols::Symbol, CompileError> {
        // this ar_name is only for use in error messages, the actual name
        // for the symbol that will be created can be slightly different,
        // e.g. having a prefix 'sub-action-'.
        let ar_name = self.get_receiver_ident().or_else(|_| {
            Ok::<ShortString, CompileError>(
                self.access_expr[0].get_ident().clone(),
            )
        })?;

        let ar_s = self.get_receiver();

        // The evaluation of the Access Receiver
        let mut ar_symbol =
            ar_s.eval(symbols.clone(), scope.clone())
                .or_else(|_| ar_s.eval(symbols.clone(), Scope::Global))
                .or_else(|_| global_enums(&ar_s.get_ident().unwrap().ident))
                .map_err(|ar_err| match ar_err {
                    AccessReceiverError::Var => CompileError::from(format!(
                    "Cannot find variable '{}' in {} or in the global scope.",
                    ar_name, scope
                )),
                    AccessReceiverError::Global => CompileError::from(
                        format!("Cannot find global method '{}'", ar_name),
                    ),
                    AccessReceiverError::Arg => CompileError::from(format!(
                        "Cannot find Argument '{}' for scope {}",
                        ar_name, scope
                    )),
                })?;

        let ar_token = ar_symbol.get_token().unwrap();
        let mut s = &mut ar_symbol;

        trace!("ACCESS EXPRESSION {:#?}", self.access_expr);

        // Use the type of the access receiver to put on the arguments.
        let mut ty = match ar_token {
            Token::Table(_) => TypeDef::Table(Box::new(s.get_type())),
            Token::Rib(_) => TypeDef::Rib((Box::new(s.get_type()), None)),
            Token::OutputStream(_) => {
                TypeDef::OutputStream(Box::new(s.get_type()))
            }
            _ => s.get_type(),
        };

        for a_e in &self.access_expr {
            match a_e {
                ast::AccessExpr::MethodComputeExpr(method_call) => {
                    trace!("MC symbol (s) {:#?}", s);
                    trace!("All Symbols {:#?}", symbols.borrow().get(&scope));
                    trace!("method call {:?} on type {}", method_call, ty);
                    let arg_s = method_call.eval(
                        // At this stage we don't know really whether the
                        // method call will be mutating or not, but we're
                        // setting the safe choice here (non-mutable).
                        symbols::SymbolKind::MethodCallbyRef,
                        ty,
                        symbols.clone(),
                        scope.clone(),
                    )?;
                    // propagate the type of this argument to a possible next one
                    ty = arg_s.get_type();
                    s.add_arg(arg_s);
                }
                ast::AccessExpr::FieldAccessExpr(field_access) => {
                    trace!("FA symbol (s) {:#?}", s);
                    trace!(
                        "all symbols in filter-map table {:#?}",
                        symbols.borrow().get(&scope)
                    );
                    let arg_s = field_access.eval(ty)?;
                    // propagate the type of this argument to a possible next one
                    ty = arg_s.get_type();
                    let i = s.add_arg(arg_s);
                    trace!("symbol -> {:#?}", s);
                    s = &mut s.get_args_mut()[i];
                }
            };
        }

        // The return type of a compute expression is propagated from the
        // last argument to its parent, the access receiver symbol.
        ar_symbol = ar_symbol.set_type(ty).set_token(ar_token);

        if let Some(name) = name {
            ar_symbol = ar_symbol.set_name(name);
        } else {
            ar_symbol = ar_symbol.set_name(self.get_receiver_ident()?);
        }

        trace!(
            "finished eval compute expression {:?}",
            ar_symbol.get_name()
        );
        Ok(ar_symbol)
    }
}

impl ast::MethodComputeExpr {
    pub(crate) fn eval(
        &self,
        // Parsed return type of the method call
        mut method_kind: symbols::SymbolKind,
        method_call_type: TypeDef,
        symbols: symbols::GlobalSymbolTable,
        scope: symbols::Scope,
    ) -> Result<symbols::Symbol, CompileError> {
        // self is the call receiver, e.g. in `rib-rov.longest_match()`,
        // `rib-rov` is the receiver and `longest_match` is the method call
        // name. The actual method call lives in the `args` field.
        let arguments = self.args.eval(symbols.clone(), scope)?;

        // we need to lookup the properties of the return type of the method
        // that the user wants to call, to see if it matches the arguments of
        // the supplied method call in the source code.
        let props = method_call_type.get_props_for_method(&self.ident)?;

        // If this is a "regular" method call, then we set the `consume` flag
        // from the props we retrieved to set the right MethodCall kind.
        if method_kind == SymbolKind::MethodCallbyRef {
            method_kind = match props.consume {
                false => SymbolKind::MethodCallbyRef,
                true => SymbolKind::MethodCallByConsumedValue,
            };
        }

        let parsed_args = arguments;

        if parsed_args.is_empty() && props.arg_types.is_empty() {
            return Ok(symbols::Symbol::new(
                self.ident.clone().ident,
                method_kind,
                props.return_type,
                vec![],
                Some(props.method_token),
            ));
        }

        // early return if no arguments were supplied and the method doesn't
        // take any either.
        if parsed_args.len() != props.arg_types.len() {
            return Err(format!(
                "Method '{}' on type {:?} expects {} arguments, but {} were provided.",
                self.ident,
                method_call_type,
                props.arg_types.len(),
                parsed_args.len()
            )
            .into());
        }

        let mut args = vec![];
        let _symbols = symbols.borrow();

        // go over the argument types that we got from the parsed arguments
        // in the the source code and compare those to the argument types
        // we got from the method definition.
        for (parsed_arg_type, expected_arg_type) in
            parsed_args.into_iter().zip(props.arg_types.iter())
        {
            // Compare the expected type with the type of the parsed value.
            // Either the types are the same, or the type of the parsed value
            // can be converted to the expected type, e.g. an IntegerLiteral
            // can be converted into a U8, I64, etc (as long as it fits).
            args.push(
                parsed_arg_type
                    .try_convert_type_value_into(expected_arg_type.clone())?,
            );
        }

        Ok(symbols::Symbol::new(
            self.ident.clone().ident,
            method_kind,
            props.return_type,
            args,
            Some(props.method_token),
        ))
    }
}

#[derive(Debug)]
pub enum AccessReceiverError {
    Var,
    Global,
    Arg,
}

// This is a simple identifier, it may refer to a data source, a variable,
// a record field, the name of a type, or the name of a global method.
impl ast::AccessReceiver {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: symbols::Scope,
    ) -> Result<symbols::Symbol, AccessReceiverError> {
        trace!("AccessReceiver {:#?}", self);
        let _symbols = symbols.clone();
        if let Some(search_ar) = self.get_ident() {
            // Is it the name of a builtin type?
            if let Ok(prim_ty) = TypeDef::try_from(search_ar.clone()) {
                if prim_ty.is_builtin() {
                    {
                        return Ok(symbols::Symbol::new(
                            search_ar.ident.clone(),
                            symbols::SymbolKind::AccessReceiver,
                            prim_ty,
                            vec![],
                            Some(Token::BuiltinType(0)),
                        ));
                    };
                }
            }

            // is it an filter-map-level argument?
            if let Some(Ok(arg)) = _symbols
                .borrow()
                .get(&scope)
                .map(|s| s.get_argument(&search_ar.ident))
            {
                let (_, type_def, token) = arg
                    .get_kind_type_and_token()
                    .map_err(|_| AccessReceiverError::Arg)?;
                return Ok(symbols::Symbol::new(
                    search_ar.ident.clone(),
                    symbols::SymbolKind::AccessReceiver,
                    type_def,
                    vec![],
                    Some(token),
                ));
            }

            // Is it one of:
            // - a name of a data source
            // - the name of a built-in constant
            // - variable name thas was defined in the `with` statement or
            //   earlier on in the same define section.
            let ident = &[search_ar.clone()];
            let (_kind, ty, to, val) =
                get_props_for_scoped_variable(ident, symbols, scope)
                    .map_err(|_| AccessReceiverError::Var)?;
            // Additionally check if this is a Variable or a Constant.
            // Constants need their values to be preserved.
            match val {
                // It's a Constant, clone the (builtin-typed) value into the
                // symbol.
                Some(val) => Ok(symbols::Symbol::new_with_value(
                    search_ar.ident.clone(),
                    SymbolKind::AccessReceiver,
                    val,
                    vec![],
                    to,
                )),
                // It's a Variable, create a symbol with an empty value.
                None => Ok(symbols::Symbol::new(
                    search_ar.ident.clone(),
                    SymbolKind::AccessReceiver,
                    ty,
                    vec![],
                    Some(to),
                )),
            }
        } else {
            // No Identifier, no AccessReceiver, this is for a globally
            // scoped method, which is None of our business anyway (it's the
            // caller's business).
            Err(AccessReceiverError::Global)
        }
    }
}

impl ast::ValueExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: symbols::Scope,
    ) -> Result<symbols::Symbol, CompileError> {
        match self {
            // an expression ending in a a method call (e.g. `foo.bar()`).
            // Note that the evaluation of the method call will check for
            // the existence of the method.
            ast::ValueExpr::ComputeExpr(compute_expr) => {
                trace!("compute expr {:?}", compute_expr);
                compute_expr.eval(None, symbols, scope)
            }
            ast::ValueExpr::BuiltinMethodCallExpr(builtin_call_expr) => {
                let name: ShortString = builtin_call_expr.ident.clone().ident;
                let prim_ty =
                    TypeDef::try_from(builtin_call_expr.ident.clone())?;

                if prim_ty.is_builtin() {
                    builtin_call_expr.eval(
                        symbols::SymbolKind::BuiltInTypeMethodCall,
                        prim_ty,
                        symbols,
                        scope,
                    )
                } else {
                    Err(format!("Unknown built-in method call: {}", name))?
                }
            }
            ast::ValueExpr::StringLiteral(str_lit) => {
                Ok(symbols::Symbol::new_with_value(
                    "string_lit".into(),
                    symbols::SymbolKind::Constant,
                    TypeValue::Builtin(BuiltinTypeValue::StringLiteral(
                        StringLiteral::new(str_lit.into()),
                    )),
                    vec![],
                    Token::Constant(None),
                ))
            }
            // Integers are special, we are keeping them as is, so that the
            // receiver can decide how to cast them (into u8, u32 or i64).
            ast::ValueExpr::IntegerLiteral(int_lit) => {
                Ok(symbols::Symbol::new_with_value(
                    int_lit.into(),
                    symbols::SymbolKind::Constant,
                    TypeValue::Builtin(BuiltinTypeValue::IntegerLiteral(
                        IntegerLiteral::new(int_lit.into()),
                    )),
                    vec![],
                    Token::Constant(None),
                ))
            }
            ast::ValueExpr::HexLiteral(hex_lit) => {
                Ok(symbols::Symbol::new_with_value(
                    "hex_lit".into(),
                    symbols::SymbolKind::Constant,
                    TypeValue::Builtin(BuiltinTypeValue::HexLiteral(
                        HexLiteral::new(hex_lit.into()),
                    )),
                    vec![],
                    Token::Constant(None),
                ))
            }
            ast::ValueExpr::PrefixLengthLiteral(prefix_len_lit) => {
                Ok(symbols::Symbol::new_with_value(
                    "prefix_len_lit".into(),
                    symbols::SymbolKind::Constant,
                    TypeValue::Builtin(BuiltinTypeValue::PrefixLength(
                        PrefixLength::new(prefix_len_lit.into()),
                    )),
                    vec![],
                    Token::Constant(None),
                ))
            }
            ast::ValueExpr::AsnLiteral(asn_lit) => {
                Ok(symbols::Symbol::new_with_value(
                    asn_lit.into(),
                    symbols::SymbolKind::Constant,
                    TypeValue::Builtin(BuiltinTypeValue::Asn(asn_lit.into())),
                    vec![],
                    Token::Constant(None),
                ))
            }
            ast::ValueExpr::BooleanLit(bool_lit) => {
                Ok(symbols::Symbol::new_with_value(
                    bool_lit.into(),
                    symbols::SymbolKind::Constant,
                    TypeValue::Builtin(BuiltinTypeValue::Boolean(
                        bool_lit.into(),
                    )),
                    vec![],
                    Token::Constant(None),
                ))
            }
            ast::ValueExpr::AnonymousRecordExpr(rec) => {
                let rec_value = rec.eval(symbols, scope)?;
                let type_def: Vec<_> = rec_value
                    .iter()
                    .map(|v| (v.get_name(), Box::new(v.get_type())))
                    .collect();
                Ok(symbols::Symbol::new(
                    "anonymous_record".into(),
                    symbols::SymbolKind::AnonymousType,
                    TypeDef::Record(type_def),
                    rec_value,
                    Some(Token::AnonymousRecord),
                ))
            }
            ast::ValueExpr::TypedRecordExpr(rec) => {
                let (type_id, rec_value) =
                    rec.eval(symbols.clone(), scope.clone())?;

                // see if the type on the record was actually defined by the roto user
                let checked_ty = if let TypeDef::Record(fields) =
                    check_type_identifier(type_id.clone(), symbols, &scope)?
                {
                    fields
                } else {
                    vec![]
                };

                // now check all individual fields to see if they match up,
                // meaning the (field_name, type) pairs match or can be made
                // to match by trying a type conversion on each field untill
                // we fail, or succeed for all fields. The type with the
                // conversions is stored as the actual type.
                let mut checked_values = vec![];
                for field_s in rec_value {
                    let cur_ty =
                        checked_ty.iter().find(|v| v.0 == field_s.get_name());
                    if let Some(cur_ty) = cur_ty {
                        checked_values.push(
                            field_s.try_convert_type_value_into(
                                *cur_ty.1.clone(),
                            )?,
                        );
                    } else {
                        return Err(CompileError::from(format!("The field name '{}' cannot be found in type '{}'", field_s.get_name(), type_id.ident)));
                    }
                }

                Ok(symbols::Symbol::new(
                    type_id.ident,
                    symbols::SymbolKind::NamedType,
                    TypeDef::Record(checked_ty),
                    checked_values,
                    Some(Token::TypedRecord),
                ))
            }
            ast::ValueExpr::PrefixMatchExpr(_) => todo!(),
            ast::ValueExpr::ListExpr(list_elm) => {
                let list_value = list_elm.eval(symbols, scope)?;
                let type_def = list_value[0].get_type();

                Ok(symbols::Symbol::new(
                    "anonymous_list".into(),
                    symbols::SymbolKind::AnonymousType,
                    TypeDef::List(Box::new(type_def)),
                    list_value,
                    Some(Token::List),
                ))
            }
        }
    }
}

impl ast::ArgExprList {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: symbols::Scope,
    ) -> Result<Vec<symbols::Symbol>, CompileError> {
        let mut eval_args = vec![];
        for arg in &self.args {
            let parsed_arg = arg.eval(symbols.clone(), scope.clone())?;
            eval_args.push(parsed_arg);
        }
        Ok(eval_args)
    }
}

impl ast::FieldAccessExpr {
    fn eval(
        &self,
        field_type: TypeDef,
    ) -> Result<symbols::Symbol, CompileError> {
        trace!("field access on field type {:?}", field_type);
        trace!("self field names {:?}", self.field_names);
        if let Ok((ty, to)) = field_type.has_fields_chain(&self.field_names) {
            trace!("token {:?}", to);
            let name = self.field_names.join(".");

            return Ok(symbols::Symbol::new(
                name.as_str().into(),
                symbols::SymbolKind::FieldAccess,
                ty,
                vec![],
                Some(to),
            ));
        } else {
            Err(format!("Invalid field access expression: {:?}.", self)
                .into())
        }
    }
}

impl ast::ListValueExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: symbols::Scope,
    ) -> Result<Vec<symbols::Symbol>, CompileError> {
        trace!("anonymous list");
        let mut s: Vec<symbols::Symbol> = vec![];
        for value in &self.values {
            let arg = value.eval(symbols.clone(), scope.clone())?;
            s.push(arg);
        }

        Ok(s)
    }
}

impl ast::AnonymousRecordValueExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: symbols::Scope,
    ) -> Result<Vec<symbols::Symbol>, CompileError> {
        trace!("anonymous record");
        let mut s: Vec<symbols::Symbol> = vec![];
        for (key, value) in &self.key_values {
            let arg = value.eval(symbols.clone(), scope.clone())?;
            s.push(arg.set_name(key.ident.clone()));
        }

        Ok(s)
    }
}

impl ast::TypedRecordValueExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: symbols::Scope,
    ) -> Result<(ast::TypeIdentifier, Vec<symbols::Symbol>), CompileError>
    {
        trace!("typed record");
        let mut s: Vec<symbols::Symbol> = vec![];
        for (key, value) in &self.key_values {
            let arg = value.eval(symbols.clone(), scope.clone())?;
            s.push(arg.set_name(key.ident.clone()));
        }

        Ok((self.type_id.clone(), s))
    }
}

//============ First-order Logic Evaluation (Terms) =========================

//------------ Logical Expression -------------------------------------------

// See ast::LogicalExpr for more information.

impl ast::LogicalExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: symbols::Scope,
    ) -> Result<symbols::Symbol, CompileError> {
        match self {
            LogicalExpr::BooleanExpr(expr) => {
                ast::BooleanExpr::eval(expr, symbols, &scope)
            }
            LogicalExpr::OrExpr(or_expr) => {
                ast::OrExpr::eval(or_expr, symbols, &scope)
            }
            LogicalExpr::AndExpr(and_expr) => {
                ast::AndExpr::eval(and_expr, symbols, &scope)
            }
            LogicalExpr::NotExpr(not_expr) => {
                ast::NotExpr::eval(not_expr, symbols, &scope)
            }
        }
    }
}

//------------ Boolean Expression -------------------------------------------

// A Boolean Expression is an expresion that takes an input with an arbitrary
// type and evaluates it into a boolean value, e.g. stand-alone variable of
// type boolean is a boolean expression.

impl ast::BooleanExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: &symbols::Scope,
    ) -> Result<symbols::Symbol, CompileError> {
        let _symbols = symbols.clone();

        match &self {
            ast::BooleanExpr::GroupedLogicalExpr(grouped_expr) => {
                grouped_expr.eval(symbols, scope)
            }
            ast::BooleanExpr::BooleanLiteral(bool_lit) => {
                // Leaf node, needs a TypeValue in this case a boolean
                // value.
                Ok(symbols::Symbol::new_with_value(
                    "boolean_constant".into(),
                    symbols::SymbolKind::Constant,
                    TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(
                        bool_lit.0,
                    ))),
                    vec![],
                    Token::Constant(None),
                ))
            }
            ast::BooleanExpr::CompareExpr(compare_expr) => {
                ast::CompareExpr::eval(compare_expr, symbols, scope)
            }
            ast::BooleanExpr::ComputeExpr(call_expr) => {
                // A Call Expression does not necessarily have to return a
                // boolean, as long as the compare expression it is nested in
                // has left and right hand-sides that return the same type.
                // Checking this can therefore not be done by the Call
                // Expression check here.
                let s = call_expr.eval(
                    // call_expr.get_receiver_ident()?,
                    None,
                    symbols,
                    scope.clone(),
                )?;
                Ok(s)
            }
            ast::BooleanExpr::ListCompareExpr(list_compare_expr) => {
                let s = list_compare_expr.as_ref().eval(symbols, scope)?;
                Ok(s)
            }
            ast::BooleanExpr::PrefixMatchExpr(_) => todo!(),
        }
    }
}

//----------------- Compare Expression --------------------------------------

// A Compare Expression is an expression of the form
// `<left handside symbol> <compare operator> <right handside symbol>`.

// The both outer symbols can hold an argument expression or groups of
// argument expressions. These are called an Compare Argument. In a Compare
// Expression all the Compare Arguments (both left and right) will have to
// evaluate to the same BUILTIN type and have to be leaf nodes. This means the
// user cannot compare complete lists and records to anything. The user can
// however use fields in lists and records.

// A Leaf Node is a symbol that has a value on the `value` field. Furthermore,
// a non-leaf node can have a nested leaf node in its `args` field. If the
// the `ty` field is not set, this is an indication that the symbol is not a
// not a leaf node. A node having neither a `ty` field filled, nor a `value`
// should not exist.

impl ast::CompareExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: &symbols::Scope,
    ) -> Result<symbols::Symbol, CompileError> {
        let _symbols = symbols.clone();

        // Process the left hand side of the compare expression. This is a
        // Compare Argument. It has to end in a leaf node, otherwise it's
        // an error.
        let left_s = self.left.eval(_symbols, scope)?;
        let left_type = left_s.get_type();

        let mut right_s = self.right.eval(symbols, scope)?;
        let right_type = right_s.get_type();

        // Either the left and right hand sides are of the same type OR the
        // right hand side value can be converted into a type of the left
        // hand side. For example, a comparison of PrefixLength and
        // IntegerLiteral will work in the form of `prefix.len() == 32;`, but
        // NOT reversed, i.e. `32 == prefix.len();` is INVALID.
        trace!("left_type {:#?} <-> right_type {:#?}", left_s, right_s);
        if left_type != right_type {
            right_s =
                right_s.try_convert_type_value_into(left_type.clone())?;
        }
        trace!("after conversion {} <-> {:?}", left_type, right_s);

        Ok(symbols::Symbol::new(
            "compare_expr".into(),
            symbols::SymbolKind::CompareExpr(self.op),
            TypeDef::Boolean,
            vec![left_s, right_s],
            None,
        ))
    }
}

impl ast::CompareArg {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: &symbols::Scope,
    ) -> Result<symbols::Symbol, CompileError> {
        let _symbols = symbols.clone();

        match self {
            ast::CompareArg::GroupedLogicalExpr(expr) => {
                // This is a grouped expression that will return a boolean.
                let s = expr.eval(symbols, scope)?;

                if s.get_type() == TypeDef::Boolean {
                    Ok(s)
                } else {
                    Err("Cannot return Non-Boolean in ( )".to_string().into())
                }
            }
            ast::CompareArg::ValueExpr(expr) => {
                // A simple operator.
                trace!("COMPARE VALUE EXPRESSION {:#?}", expr);
                expr.eval(symbols, scope.clone())
            }
        }
    }
}

impl ast::AndExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: &symbols::Scope,
    ) -> Result<symbols::Symbol, CompileError> {
        // An "And Expression" is a Boolean function, meaning it takes a
        // boolean as input and returns a boolean as output. That way
        // it can be composed into bigger logical expressions by combining
        // it with other boolean function, like "Or" or "Not".
        // The left and right hand in an "And Expression" must be leaf nodes,
        // meaning they have a value, not a type.
        let _symbols = symbols.clone();

        let left = self.left.eval(_symbols, scope)?;
        let right = self.right.eval(symbols, scope)?;

        is_boolean_function(&left, &right)?;

        Ok(symbols::Symbol::new(
            "and_expr".into(),
            symbols::SymbolKind::AndExpr,
            TypeDef::Boolean,
            vec![left, right],
            None,
        ))
    }
}

impl ast::OrExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: &symbols::Scope,
    ) -> Result<symbols::Symbol, CompileError> {
        let _symbols = symbols.clone();

        let left = self.left.eval(_symbols, scope)?;
        let right = self.right.eval(symbols, scope)?;

        is_boolean_function(&left, &right)?;

        Ok(symbols::Symbol::new(
            "or_expr".into(),
            symbols::SymbolKind::OrExpr,
            TypeDef::Boolean,
            vec![left, right],
            None,
        ))
    }
}

impl ast::NotExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: &symbols::Scope,
    ) -> Result<symbols::Symbol, CompileError> {
        let _symbols = symbols;

        let expr = self.expr.eval(_symbols, scope)?;

        if expr.get_type() != TypeDef::Boolean {
            return Err("Expression doesn't evaluate to a Boolean"
                .to_string()
                .into());
        };

        Ok(symbols::Symbol::new(
            "not_expr".into(),
            symbols::SymbolKind::NotExpr,
            TypeDef::Boolean,
            vec![expr],
            None,
        ))
    }
}

impl ast::GroupedLogicalExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: &symbols::Scope,
    ) -> Result<symbols::Symbol, CompileError> {
        self.expr.eval(symbols, scope.clone())
    }
}

impl ast::ListCompareExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: &symbols::Scope,
    ) -> Result<symbols::Symbol, CompileError> {
        let _symbols = symbols.clone();

        // Process the left hand side of the compare expression. This is a
        // Compare Argument. It has to end in a leaf node, otherwise it's
        // an error.
        let left_s = self.left.eval(_symbols, scope.clone())?;
        let left_type = left_s.get_type();

        let right_s = self.right.eval(symbols, scope.clone())?;

        let mut l_args = vec![];
        if let TypeDef::List(_) = right_s.get_type() {
            l_args = right_s.get_args_owned();
        } else {
            l_args.push(right_s);
        }

        let mut args = vec![left_s];

        for s in l_args {
            let right_type = s.get_type();
            // Either the left and right hand sides are of the same type OR the
            // right hand side value can be converted into a type of the left
            // hand side. For example, a comparison of PrefixLength and
            // IntegerLiteral will work in the form of `prefix.len() == 32;`, but
            // NOT reversed, i.e. `32 == prefix.len();` is INVALID.
            // trace!("left_type {:#?} <-> right_type {:#?}", left_s, right_s);
            if left_type != right_type {
                args.push(s.try_convert_type_value_into(left_type.clone())?);
            } else {
                args.push(s);
            }
        }
        trace!("after conversion {} <-> {:?}", left_type, &args);

        Ok(symbols::Symbol::new(
            "list_compare_expr".into(),
            symbols::SymbolKind::ListCompareExpr(self.op),
            TypeDef::Boolean,
            args,
            None,
        ))
    }
}

//============ Helper functions =============================================

fn check_type_identifier(
    ty: ast::TypeIdentifier,
    symbols: symbols::GlobalSymbolTable,
    scope: &symbols::Scope,
) -> Result<TypeDef, CompileError> {
    let symbols = symbols.borrow();
    // is it a builtin type?
    if let Ok(builtin_ty) = TypeDef::try_from(ty.clone()) {
        return Ok(builtin_ty);
    }

    // is it in the global table?
    let global_ty = symbols.get(&symbols::Scope::Global).and_then(|gt| {
        gt.get_variable(&ty.ident)
            .ok()
            .map(|s| (s.get_type(), s.get_kind()))
    });
    if let Some(ty) = global_ty {
        if ty.1 == symbols::SymbolKind::AnonymousType
            || ty.1 == symbols::SymbolKind::NamedType
        {
            return Ok(ty.0);
        }
    }

    match &scope {
        symbols::Scope::FilterMap(filter_map) => {
            // is it in the symbol table for this scope?
            let filter_map_ty = symbols
                .get(scope)
                .and_then(|gt| {
                    gt.get_variable(&ty.ident)
                        .ok()
                        .map(|s| (s.get_type(), s.get_kind()))
                })
                .ok_or(format!(
                    "No type named '{}' found in filter-map '{}'",
                    ty.ident, filter_map
                ));

            if let Ok(ty) = filter_map_ty {
                if ty.1 == symbols::SymbolKind::AnonymousType
                    || ty.1 == symbols::SymbolKind::NamedType
                {
                    return Ok(ty.0);
                }
            }
        }
        symbols::Scope::Global => {
            return Err(format!(
                "No type named '{}' found in global scope.",
                ty.ident
            )
            .into());
        }
    }

    // sorry, we don't have the type the user's asking for.
    Err(format!(
        "No type named '{}' found in scope '{}'",
        ty.ident.as_str(),
        scope
    )
    .into())
}

// This function checks if a variable exists in the scope of the filter_map, but
// not in the global scope (variables in the global scope are not allowed).
// The variables can be of form:
// <var_name>
// <var of type Record>[.<field>]+
//
// In the last case the whole form may live in the filter_map scope as an
// anonymous type (deducted from user-defined record-types), but in the case
// of a primitive type they live in the user-defined record-type itself.
//
// The last value in the return tuple is a builtin-typed value in case it's
// present in the symbol, this should only be the case with a Constant,
// containing a literal value.
fn get_props_for_scoped_variable(
    fields: &[ast::Identifier],
    symbols: GlobalSymbolTable,
    scope: symbols::Scope,
) -> Result<(SymbolKind, TypeDef, Token, Option<TypeValue>), CompileError> {
    // Implicit early return. Are there any actual fields? If not then we're
    // done, and there's nothing here.
    let first_field_name = &fields
        .first()
        .ok_or_else(|| "No name found for variable reference".to_string())?;

    let symbols = symbols.borrow();
    let search_str = fields.join(".");

    match &scope {
        symbols::Scope::FilterMap(filter_map) => {
            // 1. is the whole dotted name in the symbol table for this scope?
            return symbols
                .get(&symbols::Scope::Global)
                .and_then(|gt| {
                    gt.get_variable(
                        &search_str.as_str().into()).map(
                            |s| s.get_props()
                            .unwrap_or_else(
                            |_| panic!(
                                "No token found for variable '{}' in filter-map '{}'",
                                search_str, filter_map
                            )
                        )).ok()
                })
                .map_or_else(
                    // No, let's go over the chain of fields to see if it's
                    // a previously defined variable, constant or data-source.
                    || {
                        let var_ty_to = symbols
                            .get(&scope).and_then(|gt|
                            gt.get_symbol(first_field_name) ).map(|s| s.get_props())
                            .ok_or_else(|| format!(
                                "___ No variable named '{}' found in filter-map '{}'",
                                first_field_name, filter_map
                            ))?;

                        let var_ty_to = var_ty_to?;
                        // This checks if `field` is present in the type
                        // variable rhs, if no field is present, it will
                        // return the whole type definition and a token
                        // FieldAcces([]).
                        // Note that checking if the rhs of this
                        // assignment completely matches the assigned
                        // type is not done here.
                        let field_ty = var_ty_to
                            .1.has_fields_chain(&fields[1..])
                            .map_err(|err| {
                            trace!(
                                "{} on field '{}' for variable '{}' found in filter-map '{}'",
                                err, fields[1], fields[0].ident, filter_map
                            );
                            err
                        })?;

                        // return the type of the last field, but the token 
                        // of the var/constant/data-source
                        Ok((var_ty_to.0, field_ty.0, var_ty_to.2, var_ty_to.3))
                    },
                    // yes, it is:
                    Ok,
                );
        }
        // There is NO global scope for variables. All vars are always
        // in the namespace of a filter_map.
        symbols::Scope::Global => Err(format!(
            "=== No variable named '{}' found in global scope.",
            fields.join(".").as_str()
        )
        .into()),
    }
}

fn _declare_variable(
    name: ShortString,
    type_ident: ast::TypeIdentField,
    kind: symbols::SymbolKind,
    symbols: symbols::GlobalSymbolTable,
    scope: &symbols::Scope,
) -> Result<(), CompileError> {
    let _symbols = symbols.clone();

    // There is NO global scope for variables.  All vars are all local to a
    // filter_map.

    match &scope {
        symbols::Scope::FilterMap(filter_map) => {
            // Does the supplied type exist in our scope?
            let ty = check_type_identifier(type_ident.ty, _symbols, scope)?;

            // Apparently, we have a type.  Let's add it to the symbol table.
            let mut _symbols = symbols.borrow_mut();
            let filter_map = _symbols
                .get_mut(scope)
                .ok_or(format!("No filter-map named '{}' found.", filter_map))?;

            filter_map.add_variable(
                type_ident.field_name.ident,
                Some(name),
                kind,
                ty,
                vec![],
                TypeValue::Unknown,
            )
        }
        symbols::Scope::Global => {
            Err(format!(
                "Can't create a variable in the global scope (NEVER). Variable '{}'",
                type_ident.field_name
            )
            .into())
        }
    }
}

fn declare_argument(
    name: ShortString,
    type_ident: ast::TypeIdentField,
    kind: symbols::SymbolKind,
    symbols: symbols::GlobalSymbolTable,
    scope: &symbols::Scope,
) -> Result<(), CompileError> {
    let _symbols = symbols.clone();

    // There is NO global scope for variables.  All vars are all local to a
    // filter_map.

    match &scope {
        symbols::Scope::FilterMap(filter_map) => {
            // Does the supplied type exist in our scope?
            let ty = check_type_identifier(type_ident.ty, _symbols, scope)?;

            // Apparently, we have a type.  Let's add it to the symbol table.
            let mut _symbols = symbols.borrow_mut();
            let filter_map = _symbols
                .get_mut(scope)
                .ok_or(format!("No filter-map named '{}' found.", filter_map))?;

            filter_map.add_argument(
                type_ident.field_name.ident,
                Some(name),
                kind,
                ty,
                vec![],
                TypeValue::Unknown,
            )
        }
        symbols::Scope::Global => {
            Err(format!(
                "Can't create a variable in the global scope (NEVER). Variable '{}'",
                type_ident.field_name
            )
            .into())
        }
    }
}

// This method stores the variable in the right place in the specified
// scope. The symbol that was passed in by the caller will be put in the
// `args` field of a newly created symbol. The new symbol will get the
// return type from the symbol that was passed in.
fn declare_variable_from_symbol(
    key: ast::ShortString,
    arg_symbol: symbols::Symbol,
    symbols: symbols::GlobalSymbolTable,
    scope: &symbols::Scope,
) -> Result<(), CompileError> {
    let _symbols = symbols.clone();

    // There is NO global scope for variables.  All vars are all local to a
    // filter_map.
    match &scope {
        symbols::Scope::FilterMap(filter_map) => {

            let mut _symbols = symbols.borrow_mut();
            let filter_map = _symbols
                .get_mut(scope)
                .ok_or(format!("No filter-map named '{}' found.", filter_map))?;

            match arg_symbol.has_unknown_value() {
                // This is a variable, create an empty value on the symbol.
                true => {
                    let type_def = arg_symbol.get_recursive_return_type();

                    match arg_symbol.get_token()? {
                        Token::TypedRecord | Token::AnonymousRecord => {
                            let fields =
                                arg_symbol
                                    .get_recursive_values_primitive(type_def.clone())
                                    .map_err(
                                        |e| format!("{} in type '{}'",e, arg_symbol.get_name())
                                    )?;
                            trace!("fields {:?}", fields);
                        },
                        _ => {}
                    }

                    let symbol = symbols::Symbol::new(
                        key,
                        symbols::SymbolKind::VariableAssignment,
                        type_def,
                        vec![arg_symbol],
                        None,
                    );

                    filter_map.move_var_or_const_into(
                        symbol
                    )
                }
                // This is a constant, move the value into the symbol we're
                // storing. This can only be a builtin-typed value.
                false => {
                    let symbol = symbols::Symbol::new_with_value(
                        key,
                        symbols::SymbolKind::Constant,
                        arg_symbol.get_value_owned(),
                        vec![],
                        Token::Constant(None),
                    );
                    filter_map.move_var_or_const_into(
                        symbol
                    )
                }
            }

        }
        symbols::Scope::Global => {
            Err(format!(
                "Can't create a variable in the global scope (NEVER). Variable '{}'",
                arg_symbol.get_name()
            )
            .into())
        }
    }
}

// fn declare_constant_from_symbol(
//     key: Option<ast::ShortString>,
//     arg_symbol: symbols::Symbol,
//     symbols: symbols::GlobalSymbolTable,
//     scope: &symbols::Scope,
// ) -> Result<(), CompileError> {
//     // There is NO global scope for variables.  All vars are all local to a
//     // filter_map.
//     match &scope {
//         symbols::Scope::FilterMap(filter_map) => {

//             let mut _symbols = symbols.borrow_mut();
//             let filter_map = _symbols
//                 .get_mut(scope)
//                 .ok_or(format!(No filter-map named '{}' found.", filter_map))?;

//             let name = arg_symbol.get_name();
//             let symbol = symbols::Symbol::new_with_value(
//                 "const".into(),
//                 symbols::SymbolKind::Constant,
//                 arg_symbol.get_value_owned(),
//                 vec![],
//                 Token::Constant,
//             );

//             filter_map.move_symbol_into(
//                 key.unwrap_or(name),
//                 symbol
//             )
//         }
//         symbols::Scope::Global => {
//             Err(format!(
//                 "Can't create a variable in the global scope (NEVER). Variable '{}'",
//                 arg_symbol.get_name()
//             )
//             .into())
//         }
//     }
// }

// Terms will be added as a vec of Logical Formulas to the `term` hashmap in
// a filter_map's symbol table. So, a subterm is one element of the vec.
fn add_logical_formula(
    key: Option<ast::ShortString>,
    symbol: symbols::Symbol,
    symbols: symbols::GlobalSymbolTable,
    scope: &symbols::Scope,
) -> Result<(), CompileError> {
    let _symbols = symbols.clone();

    match &scope {
        symbols::Scope::FilterMap(filter_map) => {
            drop(_symbols);

            let mut _symbols = symbols.borrow_mut();
            let filter_map = _symbols
                .get_mut(scope)
                .ok_or(format!("No filter-map named '{}' found.", filter_map))?;

            filter_map.add_logical_formula(
                key.unwrap(),
                symbol
            )
        }
        symbols::Scope::Global => {
            Err(format!(
                "Can't create a (sub-)term in the global scope (NEVER). Term '{}'",
                symbol.get_name()
            )
            .into())
        }
    }
}

fn add_action(
    name: ShortString,
    action: symbols::Symbol,
    symbols: symbols::GlobalSymbolTable,
    scope: &symbols::Scope,
) -> Result<(), CompileError> {
    match &scope {
        symbols::Scope::FilterMap(filter_map) => {
            let mut _symbols = symbols.borrow_mut();
            let filter_map = _symbols
                .get_mut(scope)
                .ok_or(format!("No filter-map named '{}' found.", filter_map))?;

            let action = action.set_name(name.clone());

            filter_map.add_action(name, action)
        }
        symbols::Scope::Global => Err(format!(
            "Can't create an action in the global scope (NEVER). Action '{}'",
            action.get_name()
        )
        .into()),
    }
}

fn add_match_action(
    name: ShortString,
    match_action: symbols::Symbol,
    symbols: symbols::GlobalSymbolTable,
    scope: &symbols::Scope,
) -> Result<(), CompileError> {
    match &scope {
        symbols::Scope::FilterMap(filter_map) => {
            let mut _symbols = symbols.borrow_mut();
            let filter_map = _symbols
                .get_mut(scope)
                .ok_or(format!("No filter-map named '{}' found.", filter_map))?;

            let token = match_action.get_token()?;

            filter_map.move_match_action_into(Symbol::new(
                name,
                match_action.get_kind(),
                match_action.get_type(),
                match_action.get_args_owned(),
                Some(token)
            ))
        }
        symbols::Scope::Global => Err(format!(
            "Can't create a match action in the global scope (NEVER). Action '{}'",
            match_action.get_name()
        )
        .into()),
    }
}

trait BooleanExpr
where
    Self: std::fmt::Debug,
{
    fn get_args(&self) -> &[symbols::Symbol];
    fn get_type(&self) -> TypeDef;
    fn get_builtin_type(&self) -> Result<TypeDef, CompileError>;
    fn get_token(&self) -> Result<Token, CompileError>;
}

impl BooleanExpr for symbols::Symbol {
    fn get_args(&self) -> &[symbols::Symbol] {
        symbols::Symbol::get_args(self)
    }

    fn get_type(&self) -> TypeDef {
        symbols::Symbol::get_type(self)
    }

    fn get_token(&self) -> Result<Token, CompileError> {
        self.get_token()
    }

    fn get_builtin_type(&self) -> Result<TypeDef, CompileError> {
        symbols::Symbol::get_builtin_type(self)
    }
}

// Since we're only accepting binary boolean functions, we only have to test
// a left and right side to comply.
fn is_boolean_function(
    left: &impl BooleanExpr,
    right: &impl BooleanExpr,
) -> Result<(), CompileError> {
    let left = (
        left.get_builtin_type()? == TypeDef::Boolean,
        left.get_args().get(0).map(|a| a.get_value()),
    );
    let right = (
        right.get_builtin_type()? == TypeDef::Boolean,
        right.get_args().get(0).map(|a| a.get_value()),
    );

    match (left, right) {
        ((false, None), (false, None)) => Err("Right and Left hand expressions don't evaluate to boolean functions".into()),
        ((_, _), (false, None)) => Err("Right hand expression doesn't evaluate to a boolean function".into()),
        ((false, None), (_, _)) => Err("Left hand expression doesn't evaluate to a boolean function".into()),
        // Only accept leaf-nodes for now. Can't think of a reason to accept these, but who knows.
        // ((_, None), (_, None)) => Err("not accepting non-leaf nodes as boolean function".into()),
        _ => Ok(()),
    }
}

// A boolean expression only accepts on expression, that should return a
// boolean value.
fn _is_boolean_expression(
    expr: &impl BooleanExpr,
) -> Result<(), CompileError> {
    if expr.get_type() == TypeDef::Boolean {
        return Ok(());
    };

    if let Some(value) = expr.get_args().get(0).map(|a| a.get_value()) {
        if value.is_boolean_type() {
            return Ok(());
        };
    };

    Err("Expression doesn't evaluate to a Boolean"
        .to_string()
        .into())
}

fn _declare_variable_from_typedef(
    ident: &str,
    name: ast::ShortString,
    ty: TypeDef,
    kind: symbols::SymbolKind,
    _args: Option<ast::ArgExprList>,
    symbols: symbols::GlobalSymbolTable,
    scope: &symbols::Scope,
) -> Result<(), CompileError> {
    // There is NO global scope for variables.  All vars are all local to a
    // filter_map.

    match &scope {
        symbols::Scope::FilterMap(filter_map) => {
            // drop(_symbols);

            // Apparently, we have a type.  Let's add it to the symbol table.
            let mut _symbols = symbols.borrow_mut();
            let filter_map = _symbols
                .get_mut(scope)
                .ok_or(format!("No filter-map named '{}' found.", filter_map))?;

            filter_map.add_variable(
                ident.into(),
                Some(name),
                kind,
                ty,
                vec![],
                TypeValue::Unknown,
            )
        }
        symbols::Scope::Global => {
            Err(format!(
                "Can't create a variable in the global scope (NEVER). Variable '{}'",
                ident
            )
            .into())
        }
    }
}
