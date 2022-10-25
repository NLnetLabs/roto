use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::LogicalExpr;
use crate::ast::ShortString;
use crate::types::builtin::Boolean;
use crate::types::builtin::BuiltinTypeValue;

use super::ast;
use super::symbols;

use super::types::typedef::TypeDef;
use super::types::typevalue::TypeValue;

use std::convert::From;

impl<'a> ast::Root {
    pub fn eval(
        &'a self,
        symbols: Rc<RefCell<HashMap<symbols::Scope, symbols::SymbolTable>>>,
    ) -> Result<(), Box<dyn std::error::Error>> {
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
    ) -> Result<(), Box<dyn std::error::Error>> {
        let child_kvs = self.body.eval(self.ident.clone().ident, symbols)?;

        // create a new user-defined type for the record type in the RIB
        let rec_type = TypeDef::new_record_type_from_short_string(child_kvs)?;

        // add a symbol for the user-defined type, the name is derived from
        // the 'contains' clause
        symbols.add_symbol(
            self.contain_ty.ident.clone(),
            None,
            symbols::SymbolKind::NamedType,
            rec_type.clone(),
            vec![],
            None,
        )?;

        // add a symbol for the RIB itself, using the newly created record
        // type
        symbols.add_symbol(
            self.ident.ident.clone(),
            None,
            symbols::SymbolKind::Rib,
            rec_type,
            vec![],
            None,
        )?;

        Ok(())
    }
}

impl<'a> ast::Table {
    fn eval(
        &'a self,
        symbols: &'_ mut symbols::SymbolTable,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let child_kvs = self.body.eval(self.ident.clone().ident, symbols)?;

        // create a new user-defined type for the record type in the table
        let rec_type = TypeDef::new_record_type_from_short_string(child_kvs)?;

        // add a symbol for the user-defined type, the name is derived from
        // the 'contains' clause
        symbols.add_symbol(
            self.contain_ty.ident.clone(),
            None,
            symbols::SymbolKind::NamedType,
            rec_type.clone(),
            vec![],
            None,
        )?;

        // add a symbol for the RIB itself, using the newly created record
        // type
        symbols.add_symbol(
            self.ident.ident.clone(),
            None,
            symbols::SymbolKind::Table,
            rec_type,
            vec![],
            None,
        )?;

        Ok(())
    }
}

impl<'a> ast::RibBody {
    fn eval(
        &'a self,
        parent_name: ast::ShortString,
        symbols: &'_ mut symbols::SymbolTable,
    ) -> Result<Vec<(ShortString, Box<TypeDef>)>, Box<dyn std::error::Error>>
    {
        let mut kvs: Vec<(ShortString, Box<TypeDef>)> = vec![];

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
    ) -> Result<Vec<(ShortString, Box<TypeDef>)>, Box<dyn std::error::Error>>
    {
        let mut kvs: Vec<(ShortString, Box<TypeDef>)> = vec![];

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
        symbols.add_symbol(name, None, kind, record, vec![], None)?;

        Ok(kvs)
    }
}

impl ast::Module {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable<'_>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let module_scope = symbols::Scope::Module(self.ident.ident.clone());
        // Check the `with` clause for additional arguments.
        let with_kv: Vec<_> = self.with_kv.clone();

        let with_ty = with_kv
            .into_iter()
            .map(|ty| {
                declare_variable(
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

        // for action in actions.into_iter() {
        //     if let ast::ModuleExpr::Action(a) = action {
        //         a.eval(symbols.clone(), module_scope.clone());
        //     }
        // }

        Ok(())
    }

    fn eval_define_header<'a>(
        &'a self,
        symbols: symbols::GlobalSymbolTable<'a>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        // Check the `with` clause for additional arguments.
        let with_kv: Vec<_> = self.body.define.with_kv.clone();

        let with_ty = with_kv
            .into_iter()
            .map(|ty| {
                declare_variable(
                    ty,
                    symbols::SymbolKind::Argument,
                    symbols.clone(),
                    &symbols::Scope::Module(self.ident.ident.clone()),
                )
            })
            .collect::<Vec<_>>();

        // println!("define for type {:#?}", for_ty);
        println!("define with type {:#?}", with_ty);
        // for_ty
        Ok(())
    }
}

impl<'a> ast::Define {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable<'a>,
        scope: symbols::Scope,
    ) -> Result<(), Box<dyn std::error::Error>> {
        declare_variable(
            self.body.rx_type.clone(),
            symbols::SymbolKind::Argument,
            symbols.clone(),
            &scope,
        )?;

        declare_variable(
            self.body.tx_type.clone(),
            symbols::SymbolKind::Argument,
            symbols.clone(),
            &scope,
        )?;

        // Assignments can only be to method calls, there is no way to assign
        // a variable to a field (that would be aliasing basically).
        for assignment in &self.body.assignments {
            let (_name, s) = ast::CallExpr::eval(
                &assignment.1,
                assignment.0.ident.clone(),
                symbols.clone(),
                scope.clone(),
            )?;

            println!("symbol assigned {:?}", s);
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

impl<'a> ast::Term {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable<'a>,
        scope: symbols::Scope,
    ) -> Result<(), Box<dyn std::error::Error>> {
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

            println!(
                "logical formula: {} -> {:?}",
                self.ident.ident, logical_formula
            );
            add_subterm(
                Some(self.ident.ident.clone()),
                logical_formula,
                symbols.clone(),
                &scope,
            )?;
        }
        Ok(())
    }
}

impl<'a> ast::Action {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable<'a>,
        scope: symbols::Scope,
    ) {
        todo!()
    }
}

// =========== Nested AST Nodes =============================================

// These are types that can be nested inside of other types, or are used
// recursively. They return the symbols that they create, unlike the types
// that go directly in the root of a SymbolTable.
// The caller needs to insert them in the right place in a enry in the symbol
// table.

//     Identifier {
//         ident: "found_prefix",
//     },
//     CallExpr {
//         receiver: Some(
//             AccessReceiver {
//                 ident: Identifier {
//                     ident: "rib-rov",
//                 },
//                 fields: None,
//             },
//         ),
//         method_call: MethodCallExpr {
//             ident: Identifier {
//                 ident: "longest_match",
//             },
//             args: ArgExprList {
//                 args: [
//                     CallReceiver(
//                         AccessReceiver {
//                             ident: Identifier {
//                                 ident: "route",
//                             },
//                             fields: Some(
//                                 FieldAccessExpr {
//                                     field_names: [
//                                         Identifier {
//                                             ident: "prefix",
//                                         },
//                                     ],
//                                 },
//                             ),
//                         },
//                     ),
//                 ],
//             },
//         },
//     }

impl<'a> ast::CallExpr {
    pub(crate) fn eval(
        &self,
        base_name_ident: ShortString,
        symbols: symbols::GlobalSymbolTable<'a>,
        scope: symbols::Scope,
    ) -> Result<(ShortString, symbols::Symbol), Box<dyn std::error::Error>>
    {
        // assignments are always on these methods calls:
        // 1. built-in method calls, `base_method(arguments)`,
        // 2. method calls on Buitin types, `TypeName.method(arguments)`,
        //    or a method call or a field name on a record type, `var_of_record_type.method(arguments)`,
        // 3. method calls on a data source, e.g. `rib-rov.longest_match(route.prefix)`,
        // 4. method calls on a variable, `var.method(arguments)`.

        match &self.get_receiver() {
            // Case 1. Built-in method calls
            // `base_method(arguments)`
            None => {
                Err(format!("Unknown built-in method {}", self.get_ident())
                    .into())
            }
            // Case 2. Method calls on Builtin type or Record types
            Some(receiver) => {
                let receiver_ident = receiver.get_ident().clone().ident;
                // Case 2a. method calls on Builtin Type itself.
                // e.g., `AsPathFilter.first()`
                if !receiver.has_field_access() {
                    if let Ok(TypeValue::Builtin(prim_ty)) =
                        TypeValue::from_literal(&receiver_ident)
                    {
                        let ty: TypeDef = prim_ty.into();
                        return Ok((
                            receiver_ident.clone(),
                            symbols::Symbol::new(
                                base_name_ident,
                                symbols::SymbolKind::BuiltInTypeMethodCall,
                                ty.clone(),
                                vec![ast::MethodCallExpr::eval(
                                    self.get_method_call(),
                                    ty,
                                    symbols,
                                    scope,
                                )?],
                            ),
                        ));
                    }
                }

                // Case 3. Method calls on a data source
                return match (
                    get_data_source_for_ident(
                        receiver.ident.clone(),
                        symbols.clone(),
                    ),
                    receiver.get_fields(),
                ) {
                    // Yes, but fields were referenced following it.
                    (Ok(_data_type), Some(fields)) => {
                        println!(
                            "[[[ fields: {:#?} receiver {}",
                            fields,
                            receiver.get_ident()
                        );
                        let field_access = ast::FieldAccessExpr::eval(
                            fields,
                            receiver.get_ident(),
                            symbols.clone(),
                            scope.clone(),
                        )?;

                        let method_call = ast::MethodCallExpr::eval(
                            self.get_method_call(),
                            field_access.get_type(),
                            symbols,
                            scope,
                        )?;

                        let name = field_access.get_name();
                        let s = symbols::Symbol::new(
                            method_call.get_name(),
                            method_call.get_kind(),
                            method_call.get_type(),
                            vec![field_access],
                        );
                        return Ok((name, s));
                    }
                    // Yes, and no fields were referenced following it, so
                    // this is a method call on a data source, e.g.
                    // `rib-rov.longest_match()`
                    (Ok(data_type), None) => {
                        println!(
                            "!!! {:?} {:?}",
                            self.get_receiver(),
                            data_type
                        );
                        let method_call = ast::MethodCallExpr::eval(
                            self.get_method_call(),
                            data_type.clone(),
                            symbols,
                            scope,
                        )?;

                        return Ok((
                            receiver_ident.clone(),
                            symbols::Symbol::new(
                                receiver_ident,
                                symbols::SymbolKind::Rib,
                                data_type,
                                vec![method_call],
                            ),
                        ));
                    }
                    // No, there is no data source referenced, so maybe:
                    // Case 4. Method calls on a variable
                    // 4a. on a variable without fields
                    (Err(_), None) => {
                        println!(";;; {:?}", receiver);
                        match get_type_for_scoped_variable(
                            &[receiver.ident.clone()],
                            symbols.clone(),
                            scope.clone(),
                        ) {
                            Ok(var_type) => {
                                println!("??? variable {:?}", var_type);

                                Ok((
                                    receiver_ident.clone(),
                                    symbols::Symbol::new(
                                        receiver_ident,
                                        symbols::SymbolKind::Variable,
                                        var_type.clone(),
                                        vec![ast::MethodCallExpr::eval(
                                            self.get_method_call(),
                                            var_type,
                                            symbols,
                                            scope,
                                        )?],
                                    ),
                                ))
                            }
                            Err(_err) => {
                                return Err(format!("YY No data source or variable named '{}' found.",&receiver_ident ).into());
                            }
                        }
                    }
                    // Case 4a. On a variable with fields
                    (Err(_), Some(fields)) => {
                        println!("||| {:?}", receiver);

                        let field_access = ast::FieldAccessExpr::eval(
                            fields,
                            receiver.get_ident(),
                            symbols.clone(),
                            scope.clone(),
                        )?;

                        match get_type_for_scoped_variable(
                            &[receiver.ident.clone()],
                            symbols.clone(),
                            scope.clone(),
                        ) {
                            Ok(var_type) => {
                                println!("+++ variable {:?}", var_type);

                                Ok((
                                    receiver_ident,
                                    symbols::Symbol::new(
                                        field_access
                                            .get_name()
                                            .as_str()
                                            .into(),
                                        symbols::SymbolKind::Variable,
                                        field_access.get_type(),
                                        vec![ast::MethodCallExpr::eval(
                                            self.get_method_call(),
                                            field_access.get_type(),
                                            symbols,
                                            scope,
                                        )?],
                                    ),
                                ))
                            }
                            Err(_err) => {
                                return Err(format!("YY No data source or variable named '{}' found.",&receiver_ident ).into());
                            }
                        }
                    }
                };
            }
            _ => Err("Invalid method call".into()),
        }
    }
}

impl ast::MethodCallExpr {
    pub(crate) fn eval(
        &self,
        // Type of the data source this call should be implemented on.
        parent_ty: TypeDef,
        symbols: symbols::GlobalSymbolTable<'_>,
        scope: symbols::Scope,
    ) -> Result<symbols::Symbol, Box<dyn std::error::Error>> {
        let args = self.args.eval(symbols, scope)?;
        // we need to lookup the type that is the return type
        // of the method that the user wants to call.
        let method_result_ty = parent_ty.get_props_for_method(&self.ident)?.1;

        Ok(symbols::Symbol::new_with_value(
            self.ident.clone().ident,
            symbols::SymbolKind::DataSourceMethodCall,
            method_result_ty,
            args,
        ))
    }
}

// This is a (datasource + field access expression). We need to return one
// symbol that describes the type and value of the field access.
impl ast::AccessReceiver {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable<'_>,
        scope: symbols::Scope,
    ) -> Result<symbols::Symbol, Box<dyn std::error::Error>> {
        println!("AccessReceiver::eval() {:?}", self);
        let _symbols = symbols.clone();

        let mut search_var = self.get_ident().to_string();
        let mut ty = TypeDef::None;

        if let Some(fields) = &self.get_fields() {
            let field_access = ast::FieldAccessExpr::eval(
                fields,
                self.get_ident(),
                symbols,
                scope,
            )?;
            search_var = field_access.get_name().to_string();
            ty = field_access.get_type();
        }

        Ok(symbols::Symbol::new(
            search_var.as_str().into(),
            symbols::SymbolKind::FieldAccess,
            ty,
            vec![],
        ))
    }
}

impl ast::ArgExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable<'_>,
        scope: symbols::Scope,
    ) -> Result<symbols::Symbol, Box<dyn std::error::Error>> {
        match self {
            ast::ArgExpr::CallExpr(call_expr) => {
                println!("arg base_name_ident {:?}", call_expr);

                Ok(call_expr
                    .eval(
                        call_expr.get_ident().clone().ident,
                        symbols,
                        scope,
                    )?
                    .1)
            }
            ast::ArgExpr::AccessReceiver(call_receiver) => {
                println!(
                    "access receiver arg base_name_ident {:?}",
                    call_receiver
                );
                Ok(call_receiver.eval(symbols, scope)?)
            }
            ast::ArgExpr::StringLiteral(str_lit) => Ok(symbols::Symbol::new(
                str_lit.into(),
                symbols::SymbolKind::StringLiteral,
                TypeDef::String,
                vec![],
            )),
            ast::ArgExpr::IntegerLiteral(int_lit) => {
                Ok(symbols::Symbol::new(
                    int_lit.into(),
                    symbols::SymbolKind::Constant,
                    TypeDef::U32,
                    vec![],
                ))
            }
            _ => {
                Err(format!("Invalid argument expression {:?}", self).into())
            } // Identifier(Identifier),
              // TypeIdentifier(TypeIdentifier),
              // StringLiteral(StringLiteral),
              // Bool(bool),
              // CallExpr(CallExpr),
              // PrefixMatchExpr(PrefixMatchExpr),

              // eval_args.push(arg.eval(symbols.clone(), data_srcs, scope.clone())?);
        }
    }
}

impl ast::ArgExprList {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable<'_>,
        scope: symbols::Scope,
    ) -> Result<Vec<symbols::Symbol>, Box<dyn std::error::Error>> {
        let mut eval_args = vec![];
        for arg in &self.args {
            match arg {
                ast::ArgExpr::CallExpr(call_expr) => {
                    println!("arg base_name_ident {:?}", call_expr);
                    eval_args.push(
                        call_expr
                            .eval(
                                call_expr.get_ident().clone().ident,
                                symbols.clone(),
                                scope.clone(),
                            )?
                            .1,
                    );
                }
                ast::ArgExpr::AccessReceiver(call_receiver) => {
                    eval_args.push(
                        call_receiver.eval(symbols.clone(), scope.clone())?,
                    );
                }
                ast::ArgExpr::StringLiteral(str_lit) => {
                    eval_args.push(symbols::Symbol::new(
                        str_lit.into(),
                        symbols::SymbolKind::StringLiteral,
                        TypeDef::String,
                        vec![],
                    ));
                }
                ast::ArgExpr::IntegerLiteral(int_lit) => {
                    eval_args.push(symbols::Symbol::new(
                        int_lit.into(),
                        symbols::SymbolKind::Constant,
                        TypeDef::U32,
                        vec![],
                    ));
                }
                _ => {
                    return Err(format!(
                        "Invalid argument expression {:?}",
                        arg
                    )
                    .into());
                } // Identifier(Identifier),
                  // TypeIdentifier(TypeIdentifier),
                  // StringLiteral(StringLiteral),
                  // Bool(bool),
                  // CallExpr(CallExpr),
                  // PrefixMatchExpr(PrefixMatchExpr),

                  // eval_args.push(arg.eval(symbols.clone(), data_srcs, scope.clone())?);
            }
        }
        Ok(eval_args)
    }
}

impl ast::FieldAccessExpr {
    fn eval(
        &self,
        receiver: &ast::Identifier,
        symbols: symbols::GlobalSymbolTable<'_>,
        scope: symbols::Scope,
    ) -> Result<symbols::Symbol, Box<dyn std::error::Error>> {
        let _symbols = symbols.clone();

        let mut search_var = receiver.to_string();
        let mut search_vec = vec![receiver.clone()];
        let mut ty = TypeDef::None;

        let rec_type = get_type_for_scoped_variable(
            &[receiver.clone()],
            symbols.clone(),
            scope.clone(),
        )?;

        // First, check if the complete field expression is a built-in type,
        // if so we can return it right away.
        if let Ok(field_type) = rec_type.has_fields_chain(&self.field_names) {
            if BuiltinTypeValue::try_from(&field_type).is_ok() {
                let name = self.field_names.join(".");
                return Ok(symbols::Symbol::new(
                    format!("{}.{}", search_var, name).as_str().into(),
                    symbols::SymbolKind::FieldAccess,
                    field_type,
                    vec![],
                ));
            }
        };

        // Second. No, it isn't a built-in type, it has to live in a symbol
        // table specified as `<receiver_name>.<field_name>[.<fieldname>]*`.
        // Even so, we also need to check if all the intermediate fields
        // exist in the record type. These intermediates type should all have
        // an entry in the symbol table as well.
        for field_name in &self.field_names {
            search_var = format!("{}.{}", search_var.clone(), field_name);
            search_vec.push(field_name.clone());

            let _symbols = symbols.clone();
            ty = get_type_for_scoped_variable(
                &search_vec,
                _symbols,
                scope.clone(),
            )?;
        }

        Ok(symbols::Symbol::new(
            search_var.as_str().into(),
            symbols::SymbolKind::FieldAccess,
            ty,
            vec![],
        ))
    }
}

//============ First-order Logic Evaluation (Terms) =========================

//------------ Logical Expression -------------------------------------------

impl ast::LogicalExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable<'_>,
        scope: symbols::Scope,
    ) -> Result<symbols::Symbol, Box<dyn std::error::Error>> {
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

impl ast::BooleanExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable<'_>,
        scope: &symbols::Scope,
    ) -> Result<symbols::Symbol, Box<dyn std::error::Error>> {
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
                ))
            }
            ast::BooleanExpr::CompareExpr(compare_expr) => {
                ast::CompareExpr::eval(compare_expr, symbols, scope)
            }
            ast::BooleanExpr::CallExpr(call_expr) => {
                // A Call Expression does not necessarily return a boolean, as
                // long as the compare expression it is nested in has left and
                // right hand-sides that return the same type. Checking this
                // can therefore not be done by the Call Expression check here.
                let s = call_expr.eval(
                    call_expr.get_ident().clone().ident,
                    symbols,
                    scope.clone(),
                )?;
                Ok(s.1)
            }
            ast::BooleanExpr::SetCompareExpr(_) => todo!(),
            ast::BooleanExpr::PrefixMatchExpr(_) => todo!(),
            ast::BooleanExpr::AccessReceiver(ident) => {
                let _symbols = symbols.borrow();
                let gt = _symbols
                    .get(scope)
                    .ok_or(format!("Could not find scope {:?}", scope))?;
                let var = gt.get_symbol(&ident.ident.ident)?;

                is_boolean_expression(var)?;

                Ok(symbols::Symbol::new(
                    ident.ident.ident.clone(),
                    symbols::SymbolKind::Variable,
                    var.get_type(),
                    vec![symbols::Symbol::new_with_value(
                        var.get_args()[0].get_name(),
                        var.get_args()[0].get_kind(),
                        TypeValue::Builtin(BuiltinTypeValue::Boolean(Boolean(
                            None,
                        ))),
                        vec![],
                    )],
                ))
            }
        }
    }
}

impl ast::CompareExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable<'_>,
        scope: &symbols::Scope,
    ) -> Result<symbols::Symbol, Box<dyn std::error::Error>> {
        println!("comparison expression: {:?}", self);
        let _symbols = symbols.clone();

        // A non-leaf node must have a type (in the `ty` field in the symbol
        // itself), but a leaf node can't have one, instead it has the
        // `value` field filled (with a TypeValue) somewhere in the `args`
        // field of the symbol. must evaluate the left and right hand side of
        // the expression to determine the return type of the expression.
        let left_args = self.left.eval(_symbols, scope)?.get_args_owned();
        let mut left_value = None;

        let right: Box<TypeValue> =
            (&self.right.eval(symbols.clone(), scope)?.get_type()).into();
        let left: Box<TypeValue> =
            (&self.left.eval(symbols.clone(), scope)?.get_type()).into();
        println!("right {:?}", right);
        if !left_args.is_empty()
            && left_args[0].get_kind()
                == symbols::SymbolKind::DataSourceMethodCall
        {
            left_value = Some(Box::new(left_args[0].get_value().unwrap()));
        }

        let right_args =
            self.right.eval(symbols.clone(), scope)?.get_args_owned();
        let mut right_value: Option<Box<&TypeValue>> = None;

        if !right_args.is_empty()
            && right_args[0].get_kind()
                == symbols::SymbolKind::DataSourceMethodCall
        {
            right_value = Some(Box::new(right_args[0].get_value().unwrap()));
        }

        let (left, right) = match (left_value, right_value) {
            (None, None) => (left.as_ref(), right.as_ref()),
            (Some(left), None) => (*left, right.as_ref()),
            (None, Some(right)) => (left.as_ref(), *right),
            (Some(left), Some(right)) => (*left, *right),
        };

        // For now we're assuming the left hand-side and the right-hand side
        // return the same type.
        if left != right {
            return Err(
                format!("Cannot compare {} with {}", left, right).into()
            );
        }

        Ok(symbols::Symbol::new(
            "compare_expr".into(),
            symbols::SymbolKind::CompareExpr(self.op),
            TypeDef::Boolean,
            vec![
                self.left
                    .eval(symbols.clone(), scope)?
                    .set_kind(symbols::SymbolKind::LogicalExpr),
                self.right.eval(symbols, scope)?,
            ],
        ))
    }
}

impl ast::CompareArg {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable<'_>,
        scope: &symbols::Scope,
    ) -> Result<symbols::Symbol, Box<dyn std::error::Error>> {
        let _symbols = symbols.clone();

        println!("comparison argument: {:?}", self);
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
            ast::CompareArg::ArgExpr(expr) => {
                // A simple operator.
                println!("arg expr: {:?}", expr);
                expr.eval(symbols, scope.clone())
            }
        }
    }
}

impl ast::AndExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable<'_>,
        scope: &symbols::Scope,
    ) -> Result<symbols::Symbol, Box<dyn std::error::Error>> {
        // An "And Expression" is a Boolean function, meaning it takes a
        // boolean as input and returns a boolean as output. That way
        // it can be composed into bigger logical expressions by combining
        // it with other boolean function, like "Or" or "Not".
        // The left and right hand in an "And Expression" must be leaf nodes,
        // meaning they have a value, not a type.
        let _symbols = symbols.clone();

        println!("and expr {:?}", self);
        let left = self.left.eval(_symbols, scope)?;
        let right = self.right.eval(symbols, scope)?;

        is_boolean_function(&left, &right)?;

        Ok(symbols::Symbol::new(
            "and_expr".into(),
            symbols::SymbolKind::LogicalExpr,
            TypeDef::Boolean,
            vec![left, right],
        ))
    }
}

impl ast::OrExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable<'_>,
        scope: &symbols::Scope,
    ) -> Result<symbols::Symbol, Box<dyn std::error::Error>> {
        println!("or expr {:?}", self);
        let _symbols = symbols.clone();

        let left = self.left.eval(_symbols, scope)?;
        let right = self.right.eval(symbols, scope)?;

        is_boolean_function(&left, &right)?;

        Ok(symbols::Symbol::new(
            "or_expr".into(),
            symbols::SymbolKind::LogicalExpr,
            TypeDef::Boolean,
            vec![left, right],
        ))
    }
}

impl ast::NotExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable<'_>,
        scope: &symbols::Scope,
    ) -> Result<symbols::Symbol, Box<dyn std::error::Error>> {
        let _symbols = symbols.clone();

        let expr = self.expr.eval(_symbols, scope)?;

        if expr.get_type() != TypeDef::Boolean {
            return Err("Expression doesn't evaluate to a Boolean"
                .to_string()
                .into());
        };

        Ok(symbols::Symbol::new(
            "not_expr".into(),
            symbols::SymbolKind::Variable,
            TypeDef::Boolean,
            vec![expr],
        ))
    }
}

impl ast::GroupedLogicalExpr {
    fn eval(
        &self,
        symbols: symbols::GlobalSymbolTable<'_>,
        scope: &symbols::Scope,
    ) -> Result<symbols::Symbol, Box<dyn std::error::Error>> {
        println!("grouped logical expr: {:?}", self);
        self.expr.eval(symbols, scope.clone())
    }
}

//============ Helper functions =============================================

fn check_type(
    ty: ast::TypeIdentifier,
    symbols: symbols::GlobalSymbolTable,
    scope: &symbols::Scope,
) -> Result<TypeDef, Box<dyn std::error::Error>> {
    let symbols = symbols.borrow();
    // is it a builtin type?
    let builtin_ty = TypeDef::try_from(ty.clone());
    if BuiltinTypeValue::try_from(ty.ident.as_str()).is_ok() {
        return Ok(builtin_ty.unwrap());
    };

    // is it in the global table?
    let global_ty = symbols.get(&symbols::Scope::Global).and_then(|gt| {
        gt.symbols
            .get(&ty.ident)
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
                    gt.symbols
                        .get(&ty.ident)
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
fn get_type_for_scoped_variable(
    fields: &[ast::Identifier],
    symbols: symbols::GlobalSymbolTable<'_>,
    scope: symbols::Scope,
) -> Result<TypeDef, Box<dyn std::error::Error>> {
    let symbols = symbols.borrow();
    let search_str = fields.join(".");
    println!("search_str: {}", search_str);
    match &scope {
        symbols::Scope::Module(module) => {
            // 1. is it in the symbol table for this scope?
            return symbols
                .get(&scope)
                .and_then(|gt| {
                    gt.symbols.get(search_str.as_str()).map(|s| s.get_type())
                })
                .map_or_else(
                    // no, let's go over the chain of fields to see if it's
                    // a primitive type.
                    || {
                        let data_src_type = symbols
                            .get(&scope)
                            .and_then(|gt| {
                                gt.symbols
                                    .get(&fields[0].ident)
                                    .map(|s| s.get_type())
                            })
                            .ok_or(format!(
                                "No data source named '{}' found in module '{}' (for variable '{}')",
                                fields[0], module, search_str
                            ))?;

                        println!("data_src_type: {:?}", data_src_type);
                        println!("field {:?}", fields[1]);
                        let field_ty = data_src_type.has_fields_chain(&fields[1..]).map_err(|err| format!(
                            "{} on field '{}' for variable '{}' found in module '{}'",
                            err, fields[1], fields[0].ident, module
                        ))?;

                        Ok(field_ty)
                    },
                    // yes, it is:
                    Ok,
                );
        }
        // There is NO global scope for variables.  All vars are all local to
        // a module.
        symbols::Scope::Global => Err(format!(
            "No variable named '{}' found in global scope.",
            fields.join(".").as_str()
        )
        .into()),
    }
}

fn get_data_source_for_ident(
    ident: ast::Identifier,
    symbols: symbols::GlobalSymbolTable,
) -> Result<TypeDef, Box<dyn std::error::Error>> {
    let _symbols = symbols.borrow();

    let src = _symbols
        .get(&symbols::Scope::Global)
        .ok_or("No global symbol table")?
        .get_symbol(&ident.ident)
        .map(|r| match r.get_kind() {
            symbols::SymbolKind::Rib => Ok(r.get_type()),
            symbols::SymbolKind::Table => {
                Ok(TypeDef::Table(Box::new(r.get_type())))
            }
            _ => {
                Err(format!("No data source named '{}' found.", ident.ident)
                    .into())
            }
        })?;

    drop(_symbols);

    src
}

fn declare_variable(
    type_ident: ast::TypeIdentField,
    kind: symbols::SymbolKind,
    symbols: symbols::GlobalSymbolTable,
    scope: &symbols::Scope,
) -> Result<(), Box<dyn std::error::Error>> {
    let _symbols = symbols.clone();

    // There is NO global scope for variables.  All vars are all local to a
    // module.

    match &scope {
        symbols::Scope::Module(module) => {
            // Does the supplied type exist in our scope?
            let ty = check_type(type_ident.ty, _symbols, scope)?;

            // drop(_symbols);

            // Apparently, we have a type.  Let's add it to the symbol table.
            let mut _symbols = symbols.borrow_mut();
            let module = _symbols
                .get_mut(scope)
                .ok_or(format!("No module named '{}' found.", module))?;

            module.add_symbol(
                type_ident.field_name.ident,
                None,
                kind,
                ty,
                vec![],
                None,
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

fn declare_variable_from_symbol(
    key: Option<ast::ShortString>,
    symbol: symbols::Symbol,
    symbols: symbols::GlobalSymbolTable<'_>,
    scope: &symbols::Scope,
) -> Result<(), Box<dyn std::error::Error>> {
    let _symbols = symbols.clone();

    // There is NO global scope for variables.  All vars are all local to a
    // module.

    match &scope {
        symbols::Scope::Module(module) => {
            drop(_symbols);

            // Apparently, we have a type.  Let's add it to the symbol table.
            let mut _symbols = symbols.borrow_mut();
            let module = _symbols
                .get_mut(scope)
                .ok_or(format!("No module named '{}' found.", module))?;

            module.add_symbol(
                key.unwrap_or_else(|| symbol.get_name()),
                Some(symbol.get_name()),
                symbol.get_kind(),
                symbol.get_type(),
                symbol.get_args_owned(),
                None,
            )
        }
        symbols::Scope::Global => {
            Err(format!(
                "Can't create a variable in the global scope (NEVER). Variable '{}'",
                symbol.get_name()
            )
            .into())
        }
    }
}


// Terms will be added as a vec of Logical Formulas to the `term` hashmap in
// a module's symbol table. So, a subterm is one element of the vec.
fn add_subterm(
    key: Option<ast::ShortString>,
    symbol: symbols::Symbol,
    symbols: symbols::GlobalSymbolTable<'_>,
    scope: &symbols::Scope,
) -> Result<(), Box<dyn std::error::Error>> {
    let _symbols = symbols.clone();

    match &scope {
        symbols::Scope::Module(module) => {
            drop(_symbols);

            let mut _symbols = symbols.borrow_mut();
            let module = _symbols
                .get_mut(scope)
                .ok_or(format!("No module named '{}' found.", module))?;

            module.add_subterm(
                key.unwrap_or_else(|| symbol.get_name()),
                Some(symbol.get_name()),
                symbol.get_kind(),
                symbol.get_type(),
                symbol.get_args_owned(),
                None,
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

trait BooleanExpr
where
    Self: std::fmt::Debug,
{
    fn get_args(&self) -> &[symbols::Symbol];
    fn get_type(&self) -> TypeDef;
}

impl BooleanExpr for symbols::Symbol {
    fn get_args(&self) -> &[symbols::Symbol] {
        symbols::Symbol::get_args(self)
    }

    fn get_type(&self) -> TypeDef {
        symbols::Symbol::get_type(self)
    }
}

// Since we're only accepting binary boolean functions, we only have to test
// a left and right side to comply.
fn is_boolean_function(
    left: &impl BooleanExpr,
    right: &impl BooleanExpr,
) -> Result<(), Box<dyn std::error::Error>> {
    println!("left: {:?}", left);
    println!("right: {:?}", right);

    let left = (
        left.get_type() == TypeDef::Boolean,
        left.get_args().get(0).and_then(|a| a.get_value()),
    );
    let right = (
        right.get_type() == TypeDef::Boolean,
        right.get_args().get(0).and_then(|a| a.get_value()),
    );


    println!("left value: {:?}", left);
    println!("right value: {:?}", right);

    match (left, right) {
        ((false, None), (false, None)) => Err("Right and Left hand expressions don't evaluate to boolean functions".into()),
        ((_, _), (false, None)) => Err("Right hand expression doesn't evaluate to a boolean function".into()),
        ((false, None), (_, _)) => Err("Left hand expression doesn't evaluate to a boolean function".into()),
        // Only accept leaf-nodes for now. Can't think of a reason to accept these, but who knows.
        ((_, None), (_, None)) => Err("not accepting non-leaf nodes as boolean function".into()),
        _ => Ok(()),
    }
}


// A boolean expression only accepts on expression, that should return a
// boolean value.
fn is_boolean_expression(
    expr: &impl BooleanExpr,
) -> Result<(), Box<dyn std::error::Error>> {
    if expr.get_type() == TypeDef::Boolean {
        return Ok(());
    };

    if let Some(value) = expr.get_args().get(0).and_then(|a| a.get_value()) {
        if value.is_boolean_type() {
            return Ok(());
        };
    };

    Err("Expression doesn't evaluate to a Boolean"
        .to_string()
        .into())
}

fn declare_variable_from_typedef<'a>(
    ident: &str,
    name: ast::ShortString,
    ty: TypeDef,
    kind: symbols::SymbolKind,
    _args: Option<ast::ArgExprList>,
    symbols: symbols::GlobalSymbolTable<'a>,
    scope: &symbols::Scope,
) -> Result<(), Box<dyn std::error::Error>> {
    // let _symbols = symbols.clone();

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

            module.add_symbol(
                ident.into(),
                Some(name),
                kind,
                ty,
                vec![],
                None,
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
