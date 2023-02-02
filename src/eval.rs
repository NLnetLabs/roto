use crate::ast::AcceptReject;
use crate::ast::LogicalExpr;
use crate::ast::ShortString;
use crate::compile::CompileError;
use crate::symbols::GlobalSymbolTable;
use crate::symbols::MatchActionType;
use crate::symbols::SymbolKind;
use crate::traits::Token;
use crate::types::builtin::Boolean;
use crate::types::builtin::BuiltinTypeValue;
use crate::types::builtin::HexLiteral;
use crate::types::builtin::IntegerLiteral;
use crate::types::builtin::PrefixLength;
use crate::types::builtin::StringLiteral;
use crate::types::datasources::NamedTypeDef;

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
        let (modules, global): (Vec<_>, Vec<_>) = self
            .expressions
            .iter()
            .partition(|e| matches!(e, ast::RootExpr::Module(_)));

        // First, evaluate all the non-module expressions, so that they are
        // available to the modules.

        // If the global symbol table does not exist, create it.
        let mut symbols_mut = symbols.borrow_mut();
        let global_scope = symbols::Scope::Global;

        let global_symbols = if symbols_mut.contains_key(&global_scope) {
            symbols_mut.get_mut(&global_scope).unwrap()
        } else {
            symbols_mut.insert(
                global_scope.clone(),
                symbols::SymbolTable::new("global".into()),
            );
            symbols_mut.get_mut(&global_scope).unwrap()
        };

        for expr in &global {
            if let ast::RootExpr::Rib(rib) = expr {
                rib.eval(global_symbols)?;
            }

            if let ast::RootExpr::Table(table) = expr {
                table.eval(global_symbols)?;
            }
        }

        // For each module, create a new symbol table if it does not exist.
        for module in &modules {
            let module_name = &module.get_module()?.ident.ident;
            let module_scope = symbols::Scope::Module(module_name.clone());

            if let std::collections::hash_map::Entry::Vacant(e) =
                symbols_mut.entry(module_scope.clone())
            {
                e.insert(symbols::SymbolTable::new(module_name.clone()));
                symbols_mut.get_mut(&global_scope).unwrap()
            } else {
                symbols_mut.get_mut(&module_scope).unwrap()
            };
        }
        drop(symbols_mut);

        // Now, evaluate all the define sections in modules, so that modules
        // can use each other's types and variables.
        for module in &modules {
            if let ast::RootExpr::Module(m) = module {
                m.eval_define_header(symbols.clone())?;
            }
        }

        // Finally, evaluate all the modules themselves.
        for module in &modules {
            if let ast::RootExpr::Module(m) = module {
                m.eval(symbols.clone())?;
            }
        }
        println!("Evaluated successfully");

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

impl ast::Module {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
    ) -> Result<(), CompileError> {
        let module_scope = symbols::Scope::Module(self.ident.ident.clone());
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
                    &symbols::Scope::Module(self.ident.ident.clone()),
                )
            })
            .collect::<Vec<_>>();

        // first, parse the define section, so that other sections in this
        // module can use the defined variables.
        self.body
            .define
            .eval(symbols.clone(), module_scope.clone())?;

        let (terms, actions): (Vec<_>, Vec<_>) = self
            .body
            .expressions
            .iter()
            .partition(|s| matches!(s, ast::ModuleExpr::Term(_t)));

        for term in terms.into_iter() {
            if let ast::ModuleExpr::Term(t) = term {
                t.eval(symbols.clone(), module_scope.clone())?;
            }
        }

        for action in actions.into_iter() {
            if let ast::ModuleExpr::Action(a) = action {
                a.eval(symbols.clone(), module_scope.clone())?;
            }
        }

        if let Some(apply) = &self.body.apply {
            apply.eval(symbols, module_scope)?;
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
        // argument to the whole module, that can be used as a extra
        // read-only payload.
        let _with_ty = with_kv
            .into_iter()
            .map(|ty| {
                declare_argument(
                    ty.clone().field_name.ident,
                    ty,
                    symbols::SymbolKind::Argument,
                    symbols.clone(),
                    &symbols::Scope::Module(self.ident.ident.clone()),
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
        declare_argument(
            self.body.rx_type.field_name.ident.clone(),
            self.body.rx_type.clone(),
            symbols::SymbolKind::RxType,
            symbols.clone(),
            &scope,
        )?;

        // The default output-argument is defined by the 'tx' keyword in the
        // 'define' section. This is the argument that will be created by
        // this filter-module on each run. We start with an empty record of
        // the specified type.
        declare_argument(
            self.body.tx_type.field_name.ident.clone(),
            self.body.tx_type.clone(),
            symbols::SymbolKind::TxType,
            symbols.clone(),
            &scope,
        )?;

        for assignment in &self.body.assignments {
            // rhs part of the assignment can only be an Argument Expression.
            let s = ast::ValueExpr::eval(
                &assignment.1,
                symbols.clone(),
                scope.clone(),
            )?;

            println!("DECLARE VAR {} = {:#?}", assignment.0.ident, s);
            // lhs of the assignment represents the name of the variable or
            // constant.
            declare_variable_from_symbol(
                Some(assignment.0.ident.clone()),
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
                    ast::BooleanExpr::eval(expr, symbols.clone(), &scope)?
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
        let module_symbols = _symbols.get(&scope).ok_or_else(|| {
            format!("no symbols found for module {}", scope)
        })?;

        let mut sub_actions_vec = vec![];

        for call_expr in &self.body.expressions {
            // The incoming payload variable is the only variable that can be
            // used in the 'action' section. The incoming payload variable has
            // a SymolKind::RxType.
            let payload_var_name = call_expr.get_receiver().clone().ident;

            let s = module_symbols
                .get_symbol(&payload_var_name.ident)
                .ok_or_else(|| {
                    format!(
                        "for action: no variable '{}' found in {}",
                        payload_var_name.ident, scope
                    )
                })?;

            if !(s.get_kind() == symbols::SymbolKind::RxType) {
                return Err(format!(
                    "variable '{}' is not the rx type of {}",
                    payload_var_name.ident, scope
                )
                .into());
            };

            let mut s = call_expr.eval(
                format!("sub-action-{}", s.get_name()).as_str().into(),
                symbols.clone(),
                scope.clone(),
            )?;

            s = s.set_kind(SymbolKind::SubAction);

            sub_actions_vec.push(s);
        }

        drop(_symbols);

        let action = symbols::Symbol::new(
            self.ident.ident.clone(),
            symbols::SymbolKind::Action,
            TypeDef::Unknown,
            sub_actions_vec,
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
        let _module_symbols = _symbols.get_mut(&scope).ok_or_else(|| {
            format!("no symbols found for module {}", scope)
        })?;

        // There can only be one `apply` section in a modules, so we can set
        // the default action from the apply section for the whole module.
        if let Some(accept_reject) = self.body.accept_reject {
            _module_symbols.set_default_action(accept_reject);
        } else {
            _module_symbols.set_default_action(AcceptReject::Accept);
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
        let module_symbols = _symbols.get(&scope).ok_or_else(|| {
            format!("no symbols found for module {}", scope)
        })?;

        // not doing anything with the actual ApplyScope (the use statement),
        // not sure whether it is going to be needed.
        let _s_name = self.scope.clone().ident;

        let term = self.filter_ident.eval(symbols.clone(), scope.clone())?;
        let (_ty, token) = module_symbols.get_term(&term.get_name())?;

        let mut args_vec = vec![];
        for action in &self.actions {
            let match_action =
                action.0.eval(symbols.clone(), scope.clone())?;

            let (_ty, token) =
                module_symbols.get_action(&match_action.get_name())?;

            let s = symbols::Symbol::new(
                match_action.get_name(),
                symbols::SymbolKind::Action,
                TypeDef::AcceptReject(
                    action.1.unwrap_or(ast::AcceptReject::NoReturn),
                ),
                vec![],
                Some(token),
            );
            args_vec.push(s);
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
            // symboltable of the module.
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

// An Expression that computes a return value based on the AccessReceiver, 
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
        name: ShortString,
        symbols: symbols::GlobalSymbolTable,
        scope: symbols::Scope,
    ) -> Result<symbols::Symbol, CompileError> {
        let mut symbol =
            self.get_receiver().eval(symbols.clone(), scope.clone())?;

        let token = symbol.get_token().unwrap();
        let mut s = &mut symbol;

        println!("ACCESS EXPRESSION {:#?}", self.access_expr);
        for a_e in &self.access_expr {
            // Data sources are different from other access receivers: their
            // methods come from the data source (the container), not 
            // from the type contained in the data source. They don't have
            // field access.
            let ty = match token {
                Token::Table(_) => TypeDef::Table(Box::new(s.get_type())),
                Token::Rib(_) => TypeDef::Rib(Box::new(s.get_type())),
                _ => s.get_type(),
            };

            match a_e {
                ast::AccessExpr::MethodComputeExpr(method_call) => {
                    println!("MethodComputeExpr in ComputeExpr {:?} on type {:?}", method_call, ty);
                    println!("symbol (s) {:#?}", s);
                    println!("{:#?}", symbols.borrow().get(&scope));
                    let child_s = method_call.eval(
                        // At this stage we don't know really whether the
                        // method call will be mutating or not, but we're
                        // setting the safe choice here.
                        symbols::SymbolKind::MethodCallbyRef,
                        ty,
                        symbols.clone(),
                        scope.clone(),
                    )?;
                    s.add_arg(child_s);
                }
                ast::AccessExpr::FieldAccessExpr(field_access) => {
                    println!("FieldAccessExpr in ComputeExpr {:?}", field_access);
                    // println!("symbol already has args {:?}", s.get_args());
                    let child_s = field_access.eval(ty)?;
                    let (k, ty, to) = child_s.get_kind_type_and_token()?;
                    let i = s.add_arg(child_s);
                    println!("symbol -> {:#?}", s);
                    s = &mut s.get_args_mut()[i];
                }
            };
            
        }

        // The type of a compute expression is 
        let s_type = match s.get_kind() {
            SymbolKind::BuiltInTypeMethodCall | 
            SymbolKind::MethodCallByConsumedValue | 
            SymbolKind::MethodCallbyRef |
            SymbolKind::GlobalMethodCall  => {
                s.get_type()
            }
            _ => {
                s.follow_last_leaf().get_type()
            }
        };

        symbol = symbol
            .set_type(s_type)
            .set_name(name)
            .set_token(token);
        Ok(symbol)
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

        // If this is a "regular" method call, then we set use the `consume` flag
        // from the props we retrieved to set the right MethodCall kind.
        if method_kind == SymbolKind::MethodCallbyRef {
            method_kind = match props.consume {
                false => SymbolKind::MethodCallbyRef,
                true => SymbolKind::MethodCallByConsumedValue
            };
        }

        let parsed_args = arguments;

        if parsed_args.is_empty() && props.arg_types.is_empty() {
            return Ok(symbols::Symbol::new_with_value(
                self.ident.clone().ident,
                method_kind,
                props.return_type_value,
                vec![],
                props.method_token,
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
                parsed_arg_type.try_convert_value_into(expected_arg_type)?,
            );
        }

        Ok(symbols::Symbol::new_with_value(
            self.ident.clone().ident,
            method_kind,
            props.return_type_value,
            args,
            props.method_token,
        ))
    }
}

// This is a simple identifier, it may refer to a data source, a variable,
// a record field, or the name of a type.
impl ast::AccessReceiver {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable,
        scope: symbols::Scope,
    ) -> Result<symbols::Symbol, CompileError> {
        println!("AccessReceiver {:#?}", self);
        let _symbols = symbols.clone();
        let search_var = self.get_ident().ident.clone();

        // Is it the name of a builtin type?
        if let Ok(TypeValue::Builtin(prim_ty)) =
            search_var.as_str().try_into()
        {
            let ty: TypeDef = prim_ty.into();

            return Ok(symbols::Symbol::new(
                search_var.as_str().into(),
                symbols::SymbolKind::AccessReceiver,
                ty,
                vec![],
                Some(Token::BuiltinType(0)),
            ));
        };

        // is it an argument?
        if let Some(Ok(arg)) = _symbols
            .borrow()
            .get(&scope)
            .map(|s| s.get_argument(&search_var))
        {
            let (_, type_def, token) = arg.get_kind_type_and_token()?;
            return Ok(symbols::Symbol::new(
                search_var.as_str().into(),
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
        let ident = &[self.get_ident().clone()];
        let (_kind, ty, to, val) =
            get_props_for_scoped_variable(ident, symbols, scope)?;
        // Additionally check if this is a Variable or a Constant.
        // Constants need their values to be preserved.
        match val {
            // It's a Constant, clone the (builtin-typed) value into the the
            // symbol.
            Some(val) => Ok(symbols::Symbol::new_with_value(
                search_var.as_str().into(),
                SymbolKind::AccessReceiver,
                val,
                vec![],
                to,
            )),
            // It's a Variable, create a symbol with an empty value.
            None => Ok(symbols::Symbol::new(
                search_var.as_str().into(),
                SymbolKind::AccessReceiver,
                ty,
                vec![],
                Some(to),
            )),
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
            ast::ValueExpr::ComputeExpr(call_expr) => {
                println!("VALUE COMPUTE EXPRESSION {:?}", call_expr.get_receiver());

                call_expr.eval(
                    call_expr.get_receiver().ident.ident,
                    symbols,
                    scope,
                )
            }
            ast::ValueExpr::BuiltinMethodCallExpr(builtin_call_expr) => {
                let name: ShortString = builtin_call_expr.ident.clone().ident;
                let mut ty = TypeDef::Unknown;
                if let Ok(TypeValue::Builtin(prim_ty)) =
                    name.as_str().try_into()
                {
                    ty = prim_ty.into();
                } else {
                    Err(format!("Unknown built-in method call: {}", name))?;
                }

                builtin_call_expr.eval(
                    symbols::SymbolKind::BuiltInTypeMethodCall,
                    ty,
                    symbols,
                    scope,
                )
            }
            ast::ValueExpr::StringLiteral(str_lit) => {
                Ok(symbols::Symbol::new_with_value(
                    str_lit.into(),
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
            _ => {
                Err(format!("xx Invalid argument expression {:?}", self)
                    .into())
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

        println!("FieldAccessExpr {:?}", self);

        if let Ok((ty, to)) = field_type.has_fields_chain(&self.field_names) {

            let name = self.field_names.join(".");

            return Ok(symbols::Symbol::new(
                name.as_str().into(),
                symbols::SymbolKind::FieldAccess,
                ty,
                vec![],
                Some(to),
            ));
        } else {
            Err(format!("Invalid field access expression: {:?}", self).into())
        }
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
                        Some(bool_lit.0),
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
                    call_expr.get_receiver().ident.ident,
                    symbols,
                    scope.clone(),
                )?;
                Ok(s)
            }
            ast::BooleanExpr::SetCompareExpr(_) => todo!(),
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

        // Proces the left hand side of the compare expression. This is a
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
        println!("left_type {:#?} <-> right_type {:#?}", left_s, right_s);
        if left_type != right_type {
            right_s = right_s.try_convert_value_into(&left_type)?;
        }
        println!("after conversion {} <-> {:?}", left_type, right_s);

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
                println!("COMPARE VALUE EXPRESSION {:#?}", expr);
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

//============ Helper functions =============================================

fn check_type_identifier(
    ty: ast::TypeIdentifier,
    symbols: symbols::GlobalSymbolTable,
    scope: &symbols::Scope,
) -> Result<TypeDef, CompileError> {
    let symbols = symbols.borrow();
    // is it a builtin type?
    let builtin_ty = TypeDef::try_from(ty.clone());
    if BuiltinTypeValue::try_from(ty.ident.as_str()).is_ok() {
        return Ok(builtin_ty.unwrap());
    };

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
        symbols::Scope::Module(module) => {
            // is it in the symbol table for this scope?
            let module_ty = symbols
                .get(scope)
                .and_then(|gt| {
                    gt.get_variable(&ty.ident)
                        .ok()
                        .map(|s| (s.get_type(), s.get_kind()))
                })
                .ok_or(format!(
                    "No type named '{}' found in module '{}'",
                    ty.ident, module
                ));

            if let Ok(ty) = module_ty {
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

// This function checks if a variable exists in the scope of the module, but
// not in the global scope (variables in the global scope are not allowed).
// The variables can be of form:
// <var_name>
// <var of type Record>[.<field>]+
//
// In the last case the whole form may live in the module scope as an
// anonymous type (deducted from user-defined record-types), but in the case
// of a primitive type they live in the user-defined record-type itself.
//
// The last value in the return tuple is a builtin-typed value in case it's
// present in the symbol, this should only be the case with a Constant,
// containing a literral value.
fn get_props_for_scoped_variable(
    fields: &[ast::Identifier],
    symbols: GlobalSymbolTable,
    scope: symbols::Scope,
) -> Result<(SymbolKind, TypeDef, Token, Option<TypeValue>), CompileError> {
    // Implicit early return. Are there any actual fields? If not then we're
    // done, and there's nothing here.
    let first_field_name = &fields
        .first()
        .ok_or_else(|| "No name found for variable reference".to_string())?
        .ident;

    let symbols = symbols.borrow();
    let search_str = fields.join(".");

    match &scope {
        symbols::Scope::Module(module) => {
            // 1. is the whole dotted name in the symbol table for this scope?
            return symbols
                .get(&symbols::Scope::Global)
                .and_then(|gt| {
                    gt.get_variable(
                        &search_str.as_str().into()).map(
                            |s| s.get_props()
                            .unwrap_or_else(
                            |_| panic!(
                                "No token found for variable '{}' in module '{}'",
                                search_str, module
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
                                "___ No variable named '{}' found in module '{}'",
                                first_field_name, module
                            ))?;

                        let var_ty_to = var_ty_to?;
                        let field_ty = var_ty_to
                            .1.has_fields_chain(&fields[1..])
                            .map_err(|err| format!(
                                "{} on field '{}' for variable '{}' found in module '{}'",
                                err, fields[1], fields[0].ident, module
                            ))?;

                        // return the type of the last field, but the token 
                        // of the var/constant/data-source
                        Ok((var_ty_to.0, field_ty.0, var_ty_to.2, var_ty_to.3))
                    },
                    // yes, it is:
                    Ok,
                );
        }
        // There is NO global scope for variables. All vars are always
        // in the namespace of a module.
        symbols::Scope::Global => Err(format!(
            "=== No variable named '{}' found in global scope.",
            fields.join(".").as_str()
        )
        .into()),
    }
}

// fn _get_data_source_for_ident(
//     ident: ast::Identifier,
//     symbols: symbols::GlobalSymbolTable,
// ) -> Result<(TypeDef, Token), CompileError> {
//     let _symbols = symbols.borrow();

//     let src = _symbols
//         .get(&symbols::Scope::Global)
//         .ok_or("No global symbol table")?
//         .get_data_source(&ident.ident);

//     drop(_symbols);

//     src
// }

fn _declare_variable(
    name: ShortString,
    type_ident: ast::TypeIdentField,
    kind: symbols::SymbolKind,
    symbols: symbols::GlobalSymbolTable,
    scope: &symbols::Scope,
) -> Result<(), CompileError> {
    let _symbols = symbols.clone();

    // There is NO global scope for variables.  All vars are all local to a
    // module.

    match &scope {
        symbols::Scope::Module(module) => {
            // Does the supplied type exist in our scope?
            let ty = check_type_identifier(type_ident.ty, _symbols, scope)?;

            // Apparently, we have a type.  Let's add it to the symbol table.
            let mut _symbols = symbols.borrow_mut();
            let module = _symbols
                .get_mut(scope)
                .ok_or(format!("No module named '{}' found.", module))?;

            module.add_variable(
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
    // module.

    match &scope {
        symbols::Scope::Module(module) => {
            // Does the supplied type exist in our scope?
            let ty = check_type_identifier(type_ident.ty, _symbols, scope)?;

            // Apparently, we have a type.  Let's add it to the symbol table.
            let mut _symbols = symbols.borrow_mut();
            let module = _symbols
                .get_mut(scope)
                .ok_or(format!("No module named '{}' found.", module))?;

            module.add_argument(
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

// This methods stores the variable in the right place in the specified
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
    // module.
    match &scope {
        symbols::Scope::Module(module) => {

            let mut _symbols = symbols.borrow_mut();
            let module = _symbols
                .get_mut(scope)
                .ok_or(format!("No module named '{}' found.", module))?;

            match arg_symbol.has_value() {
                // This is a variable, create an empty value on the symbol.
                false => {
                    let symbol = symbols::Symbol::new(
                        key,
                        symbols::SymbolKind::VariableAssignment,
                        arg_symbol.get_recursive_return_type(),
                        vec![arg_symbol],
                        None,
                    );

                    module.move_var_const_into(
                        symbol
                    )
                }
                // This is a constant, move the value into the symbol we're
                // storing. This can only be a builtin-typed value.
                true => {
                    let symbol = symbols::Symbol::new_with_value(
                        key,
                        symbols::SymbolKind::Constant,
                        arg_symbol.get_value_owned(),
                        vec![],
                        Token::Constant(None),
                    );
                    module.move_var_const_into(
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
//     // module.
//     match &scope {
//         symbols::Scope::Module(module) => {

//             let mut _symbols = symbols.borrow_mut();
//             let module = _symbols
//                 .get_mut(scope)
//                 .ok_or(format!("No module named '{}' found.", module))?;

//             let name = arg_symbol.get_name();
//             let symbol = symbols::Symbol::new_with_value(
//                 "const".into(),
//                 symbols::SymbolKind::Constant,
//                 arg_symbol.get_value_owned(),
//                 vec![],
//                 Token::Constant,
//             );

//             module.move_symbol_into(
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
// a module's symbol table. So, a subterm is one element of the vec.
fn add_logical_formula(
    key: Option<ast::ShortString>,
    symbol: symbols::Symbol,
    symbols: symbols::GlobalSymbolTable,
    scope: &symbols::Scope,
) -> Result<(), CompileError> {
    let _symbols = symbols.clone();

    match &scope {
        symbols::Scope::Module(module) => {
            drop(_symbols);

            let mut _symbols = symbols.borrow_mut();
            let module = _symbols
                .get_mut(scope)
                .ok_or(format!("No module named '{}' found.", module))?;

            module.add_logical_formula(
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
        symbols::Scope::Module(module) => {
            let mut _symbols = symbols.borrow_mut();
            let module = _symbols
                .get_mut(scope)
                .ok_or(format!("No module named '{}' found.", module))?;

            let action = action.set_name(name.clone());

            module.add_action(name, action)
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
        symbols::Scope::Module(module) => {
            let mut _symbols = symbols.borrow_mut();
            let module = _symbols
                .get_mut(scope)
                .ok_or(format!("No module named '{}' found.", module))?;

            let token = match_action.get_token()?;

            module.add_match_action(
                name,
                Some(match_action.get_name()),
                match_action.get_kind(),
                match_action.get_type(),
                match_action.get_args_owned(),
                TypeValue::Unknown,
                token
            )
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
    // module.

    match &scope {
        symbols::Scope::Module(module) => {
            // drop(_symbols);

            // Apparently, we have a type.  Let's add it to the symbol table.
            let mut _symbols = symbols.borrow_mut();
            let module = _symbols
                .get_mut(scope)
                .ok_or(format!("No module named '{}' found.", module))?;

            module.add_variable(
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
