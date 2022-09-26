use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::ShortString;
use crate::ast::TypeIdentField;
use crate::types::BuiltinTypeValue;

use super::ast;
use super::symbols;
use super::types;

impl<'a> ast::Root {
    pub fn eval(
        &'a self,
        symbols: Rc<
            RefCell<HashMap<ast::ShortString, symbols::SymbolTable<'a>>>,
        >,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let (modules, global): (Vec<_>, Vec<_>) = self
            .expressions
            .iter()
            .partition(|e| matches!(e, ast::RootExpr::Module(_)));

        // First, evaluate all the non-module expressions, so that they are
        // available to the modules.

        // If the global symbol table does not exist, create it.
        let mut symbols_mut = symbols.borrow_mut();

        let global_symbols = if symbols_mut.contains_key("global") {
            symbols_mut.get_mut("global").unwrap()
        } else {
            symbols_mut.insert(
                "global".into(),
                symbols::SymbolTable::new("global".into()),
            );
            symbols_mut.get_mut("global").unwrap()
        };

        for expr in &global {
            expr.eval(global_symbols)?;
        }

        // For each module, create a new symbol table if it does not exist.
        for module in &modules {
            let module_name = &module.get_module()?.ident.ident;

            if symbols_mut.contains_key(module_name) {
                symbols_mut.get_mut(module_name).unwrap()
            } else {
                symbols_mut.insert(
                    module_name.clone(),
                    symbols::SymbolTable::new(module_name.clone()),
                );
                symbols_mut
                    .get_mut(&module.get_module()?.ident.ident)
                    .unwrap()
            };
        }
        drop(symbols_mut);

        // Now, evaluate all the define sections in modules, so that modules
        // can use each other's types and variables.
        for module in &modules {
            module.eval_define(symbols.clone())?;
        }

        Ok(())
    }
}

impl<'a> ast::RootExpr {
    fn eval(
        &'a self,
        symbols: &'_ mut symbols::SymbolTable<'a>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        match self {
            ast::RootExpr::Module(m) => m.eval(symbols),
            ast::RootExpr::Rib(t) => t.eval(symbols),
        }
    }

    fn eval_define(
        &'a self,
        symbols: Rc<
            RefCell<
                std::collections::HashMap<
                    ast::ShortString,
                    symbols::SymbolTable<'a>,
                >,
            >,
        >,
    ) -> Result<types::TypeDef<'a>, Box<dyn std::error::Error>> {
        if let ast::RootExpr::Module(m) = self {
            m.eval_define(symbols.clone())
        } else {
            Err("Cannot evaluate a rib. No define section found.".into())
        }
    }
}

impl<'a> ast::Rib {
    fn eval(
        &'a self,
        symbols: &'_ mut symbols::SymbolTable<'a>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let child_kvs = self.body.eval(symbols)?;

        // create a new user-defined type for the record type in the RIB
        let rec_type = types::TypeDef::new_record_type(child_kvs)?;

        // add a symbol for the user-defined type, the name is derived from
        // the 'contains' clause
        symbols.add_symbol(
            self.contain_ty.ident.clone(),
            symbols::SymbolKind::NamedType,
            rec_type.clone(),
            None,
        );

        // add a symbol for the RIB itself, using the newly created record
        // type
        symbols.add_symbol(
            self.ident.ident.clone(),
            symbols::SymbolKind::Rib,
            rec_type,
            None,
        );

        Ok(())
    }
}

impl<'a> ast::RibBody {
    fn eval(
        &'a self,
        symbols: &'_ mut symbols::SymbolTable<'a>,
    ) -> Result<
        Vec<(&'a str, Box<types::TypeDef<'a>>)>,
        Box<dyn std::error::Error>,
    > {
        let mut kvs: Vec<(&str, Box<types::TypeDef>)> = vec![];

        for kv in self.key_values.iter() {
            match kv {
                ast::RibField::PrimitiveField(f) => {
                    kvs.push((
                        f.field_name.ident.as_str(),
                        Box::new(f.ty.clone().try_into()?),
                    ));
                }
                ast::RibField::RecordField(r) => {
                    let nested_record = ast::RecordTypeIdentifier::eval(
                        &r.1,
                        String::from(r.0.ident.as_str())
                            .to_uppercase()
                            .as_str()
                            .into(),
                        symbols::SymbolKind::AnonymousType,
                        symbols,
                    )?;

                    kvs.push((
                        r.0.ident.as_str(),
                        Box::new(types::TypeDef::Record(nested_record)),
                    ));
                }
                ast::RibField::ListField(l) => {
                    kvs.push((
                        l.0.ident.as_str(),
                        Box::new(types::TypeDef::List(Box::new(
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
        symbols: &'_ mut symbols::SymbolTable<'a>,
    ) -> Result<
        Vec<(&'a str, Box<types::TypeDef<'a>>)>,
        Box<dyn std::error::Error>,
    > {
        let mut kvs: Vec<(&str, Box<types::TypeDef>)> = vec![];

        for kv in self.key_values.iter() {
            match kv {
                ast::RibField::PrimitiveField(f) => {
                    kvs.push((
                        f.field_name.ident.as_str(),
                        Box::new(f.ty.clone().try_into()?),
                    ));
                }
                ast::RibField::RecordField(r) => {
                    let nested_record = ast::RecordTypeIdentifier::eval(
                        &r.1,
                        String::from(r.0.ident.as_str())
                            .to_uppercase()
                            .as_str()
                            .into(),
                        symbols::SymbolKind::AnonymousType,
                        symbols,
                    )?;

                    kvs.push((
                        r.0.ident.as_str(),
                        Box::new(types::TypeDef::Record(nested_record)),
                    ));
                }
                ast::RibField::ListField(l) => {
                    kvs.push((
                        l.0.ident.as_str(),
                        Box::new(types::TypeDef::List(Box::new(
                            l.1.inner_type.clone().try_into()?,
                        ))),
                    ));
                }
            }
        }

        let record = types::TypeDef::Record(kvs.clone());
        symbols.add_symbol(name, kind, record, None);

        Ok(kvs)
    }
}

struct ModuleProperties<'a> {
    input_type: symbols::Symbol<'a>,
    arguments: Vec<symbols::Symbol<'a>>,
    data_sets: Vec<symbols::Symbol<'a>>,
    variables: Vec<symbols::Symbol<'a>>,
}

impl ast::Module {
    fn eval(
        &'_ self,
        symbols: &'_ mut symbols::SymbolTable<'_>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        // for expr in self.body.iter() {
        //     expr.eval(symbols)?;
        // }

        // symbols.add_symbol(
        //     self.ident.ident.clone(),
        //     symbols::SymbolKind::Module,
        //     types::TypeDef::Module,
        //     Some(symbols),
        // );

        Ok(())
    }

    fn eval_define<'a>(
        &'a self,
        symbols: Rc<
            RefCell<
                std::collections::HashMap<
                    ast::ShortString,
                    symbols::SymbolTable<'a>,
                >,
            >,
        >,
    ) -> Result<types::TypeDef<'a>, Box<dyn std::error::Error>> {
        // First, check the `define for` clause to see if we actulally have
        // the type the user's asking for.
        let for_ty = if let Some(record_type) = &self.body.define.for_kv {
            check_type(
                record_type.ty.clone(),
                symbols.clone(),
                &symbols::Scope::Module(self.ident.ident.clone()),
            )
        } else {
            return Err("No type specified in `for` clause".into());
        };

        // Check the `with` clause for additional arguments.
        let with_kv: Vec<_> = self.body.define.with_kv.clone();

        let with_ty = with_kv
            .into_iter()
            .map(|ty| {
                declare_variable(
                    ty,
                    symbols::SymbolKind::Argument,
                    symbols.clone(),
                    symbols::Scope::Module(self.ident.ident.clone()),
                )
            })
            .collect::<Vec<_>>();

        println!("define for type {:#?}", for_ty);
        println!("define with type {:#?}", with_ty);
        for_ty
    }
}

impl ast::ModuleBody {
    fn eval_define(
        &self,
        symbols: &mut symbols::SymbolTable,
    ) -> Result<bool, Box<dyn std::error::Error>> {
        let mut result = false;

        // for expr in self.body.iter() {
        //     result = expr.eval_define(symbols)?;
        // }

        Ok(result)
    }
}

fn check_type<'a>(
    ty: ast::TypeIdentifier,
    symbols: Rc<
        RefCell<
            std::collections::HashMap<
                ast::ShortString,
                symbols::SymbolTable<'a>,
            >,
        >,
    >,
    scope: &symbols::Scope,
) -> Result<types::TypeDef<'a>, Box<dyn std::error::Error>> {
    let symbols = symbols.borrow();
    // is it a builtin type?
    let builtin_ty = types::TypeDef::try_from(ty.clone());
    if BuiltinTypeValue::try_from(ty.ident.as_str()).is_ok() {
        return Ok(builtin_ty.unwrap());
    };

    // is it in the global table?
    let global_ty = symbols.get("global").and_then(|gt| {
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
                .get(module)
                .and_then(|gt| {
                    gt.symbols
                        .get(&ty.ident)
                        .map(|s| (s.get_type(), s.get_kind()))
                })
                .ok_or(format!(
                    "No type named {} found in module {}",
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
                "No type named {} found in global scope.",
                ty.ident
            )
            .into());
        }
    }

    // sorry, we don't have the type the user's asking for.
    return Err(format!(
        "No type named '{}' found in scope {:?}",
        ty.ident.as_str(),
        scope
    )
    .into());
}

fn get_scoped_variable(
    ident: ast::ShortString,
    symbols: Rc<
        RefCell<
            std::collections::HashMap<ast::ShortString, symbols::SymbolTable>,
        >,
    >,
    scope: symbols::Scope,
) -> Result<
    (ShortString, types::TypeDef, symbols::SymbolKind),
    Box<dyn std::error::Error>,
> {
    let symbols = symbols.borrow();
    // There is NO global scope for variables.  All vars are all local to a
    // module.

    match &scope {
        symbols::Scope::Module(module) => {
            // is it in the symbol table for this scope?
            let module_var = symbols
                .get(module)
                .and_then(|gt| {
                    gt.symbols
                        .get(&ident)
                        .map(|s| (s.get_name(), s.get_type(), s.get_kind()))
                })
                .ok_or(format!(
                    "No variable named {} found in module {}",
                    ident, module
                ));

            if let Ok(var) = module_var {
                if var.2 == symbols::SymbolKind::Variable {
                    return Ok(var);
                }
            }
        }
        symbols::Scope::Global => {
            return Err(format!(
                "No variable named {} found in global scope.",
                ident
            )
            .into());
        }
    }

    // sorry, we don't have the type the user's asking for.
    return Err(format!(
        "No variable named '{}' found in scope {:?}",
        ident.as_str(),
        scope
    )
    .into());
}

fn declare_variable(
    type_ident: ast::TypeIdentField,
    kind: symbols::SymbolKind,
    symbols: Rc<
        RefCell<
            std::collections::HashMap<ast::ShortString, symbols::SymbolTable>,
        >,
    >,
    scope: symbols::Scope,
) -> Result<(), Box<dyn std::error::Error>> {
    let _symbols = symbols.clone();

    // There is NO global scope for variables.  All vars are all local to a
    // module.

    match &scope {
        symbols::Scope::Module(module) => {
            // Does the supplied type exist in our scope?
            let ty = check_type(
                type_ident.ty,
                _symbols.clone(),
                &scope,
            )?;

            drop(_symbols);

            // Apparently, we have a type.  Let's add it to the symbol table.
            let mut _symbols = symbols.borrow_mut();
            let module = _symbols
                .get_mut(module)
                .ok_or(format!("No module named {} found.", module))?;

            module.add_symbol(type_ident.field_name.ident, kind, ty, None);
            Ok(())
        }
        symbols::Scope::Global => {
            return Err(format!(
                "Can't create a variable in the global scope (NEVER). Variable {}",
                type_ident.field_name
            )
            .into());
        }
    }
}
