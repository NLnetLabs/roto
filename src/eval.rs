use std::collections::HashMap;
use std::ops::Deref;

use crate::types::Record;

use super::ast;
use super::symbols;
use super::types;

impl<'a> ast::Root {
    pub fn eval(
        &'a self,
        symbols: &mut HashMap<ast::ShortString, symbols::SymbolTable<'a>>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let (modules, global): (Vec<_>, Vec<_>) = self
            .expressions
            .iter()
            .partition(|e| matches!(e, ast::RootExpr::Module(_)));

        // First, evaluate all the non-module expressions, so that they are
        // available to the modules.

        // If the global symbol table does not exist, create it.
        let global_symbols = if symbols.contains_key("global") {
            symbols.get_mut("global").unwrap()
        } else {
            symbols.insert(
                "global".into(),
                symbols::SymbolTable::new("global".into()),
            );
            symbols.get_mut("global").unwrap()
        };

        for expr in &global {
            expr.eval(global_symbols)?;
        }

        // Now, evaluate all the modules.
        for module in &modules {
            let module_symbols = if symbols
                .contains_key(&module.get_module()?.ident.ident)
            {
                symbols
                    .get_mut(&module.get_module()?.ident.ident.clone())
                    .unwrap()
            } else {
                symbols.insert(
                    module.get_module()?.ident.ident.clone(),
                    symbols::SymbolTable::new(
                        module.get_module()?.ident.ident.clone(),
                    ),
                );
                symbols.get_mut(&module.get_module()?.ident.ident).unwrap()
            };

            for module in &modules {
                module.eval(module_symbols)?;
            }
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
}

impl<'a> ast::Rib {
    fn eval(
        &'a self,
        symbols: &'_ mut symbols::SymbolTable<'a>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        // let kvs: Vec<(&str, Box<types::TypeDef>)> = vec![];

        self.body.eval(
            self.ident.ident.clone(),
            symbols::SymbolKind::Rib,
            symbols,
        )?;

        // let record = types::TypeDef::Record(kvs);

        // symbols.add_symbol(
        //     self.ident.ident.clone(),
        //     symbols::SymbolKind::Rib,
        //     record,
        //     None,
        // );

        Ok(())
    }
}

impl<'a> ast::RibBody {
    fn eval(
        &'a self,
        name: ast::ShortString,
        kind: symbols::SymbolKind,
        // mut nested_record: Vec<(&'a str, Box<types::TypeDef<'a>>)>,
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
                    let nested_record = ast::RibBody::eval(
                        &r.1,
                        String::from(r.0.ident.as_str()).to_uppercase().as_str().into(),
                        symbols::SymbolKind::AnonymousType,
                        symbols,
                    )?;

                    kvs.push((
                        r.0.ident.as_str(),
                        Box::new(types::TypeDef::Record(nested_record)),
                    ));
                }
            }
        }

        let record = types::TypeDef::Record(kvs.clone());
        symbols.add_symbol(name, kind, record, None);

        Ok(kvs)
    }
}

impl ast::Module {
    fn eval(
        &self,
        symbols: &mut symbols::SymbolTable,
    ) -> Result<(), Box<dyn std::error::Error>> {
        // let mut module_symbols = symbols::SymbolTable::new(self.ident.into());

        // for expr in &self.body {
        //     expr.eval(&mut module_symbols)?;
        // }

        // symbols.add_module(self.name.clone(), module_symbols);

        Ok(())
    }
}
