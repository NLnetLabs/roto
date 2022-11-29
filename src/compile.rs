use std::sync::Arc;

use crate::{
    ast::{AcceptReject, ShortString},
    symbols::{SymbolTable, GlobalSymbolTable},
    types::typevalue::TypeValue, vm::Command,
};

struct VecPayload(Vec<(ShortString, TypeValue)>);

//============ The Compiler (Filter creation time) ==========================

// Our starting point here is the Global Symbol Table that falls out of the
// evaluation of the AST.
// First, the `arguments` map is left alone, all of the values for the
// arguments supplied by the caller at filter runtime. All the entries in the
// `variables` map could now be compiled to their tokenized values. But, that
// may be too much, since a user can declare variables that are never used
// anywhere. Therefore we start from the other side, and go over the terms
// first and then tokenize all the arguments encountered there. That way we
// can signal to the user which variables are actually not used.

//============ The Virtual Machine (Filter RunTime) =========================

// The virtual machine starts at each run of a filter and will be called with
// the input payload and the with arguments from the `define` section of the
// filter-module.

// Our starting point here is the Global Symbol Table that falls out of the
// evaluation of the AST.
// First, the `arguments` map are left alone, all of their values should be
// supplied by the caller. All the entries in the `variables` map could now
// be interpreted to their final values. But that could be too much work,
// since users may specify any number of variables that are not actually used
// in the filter run-time. If we start from interpreting the terms then we'll
// bump only into the variables that are actually used.

//========================== Compiler ========================================

// create MIR

//
// Apply section:
// - go over all match_actions
//    - evalulate the referenced term
//        - evaluate all sub-terms
//            - CompareExpr
//            - LogicalExpr (BooleanExpr/NotExpr/AndExpr/OrExpr)
//            - evaluate the left side
//                - FieldAccess -> Data source -> lookup field
//                              -> Variable -> lookup variable -> ...
//                              -> Argument -> lookup argument -> ...
//                - Constant -> has value -> load the constant
//                - Variable -> load the variable from the symbol table
//            - evaluate the right side
pub struct Mir {
    command_stack: Vec<Command>
}

pub fn compile(symbols: GlobalSymbolTable) -> Result<(), Box<dyn std::error::Error>> {
    println!("Start compiling...");
    let _global = symbols.borrow();
    let modules = _global.keys().collect::<Vec<_>>();
    for module in modules {
        let _module = _global.get(module).unwrap();
        let (args, vars) = _module.get_term_deps();

        println!("args: {:?}", args);
        println!("vars: {:?}", vars);
    }

    Ok(())
}

//========================== Virtual Machine =================================


pub trait Payload {
    fn set(&mut self, field: ShortString, value: TypeValue);
    fn get(&self, field: ShortString) -> Option<&Vec<u8>>;
}

impl VecPayload {
    fn set(&mut self, field: ShortString, data: TypeValue) {
        let field = &mut self.0.iter().find(|k| k.0 == field);
        let key = field.unwrap().0.clone();
        let _old = std::mem::replace(field, Some(&(key, data)));
    }

    fn get(&self, field: ShortString) -> Option<&TypeValue> {
        self.0.iter().find(|k| k.0 == field).map(|kv| &kv.1)
    }
}

trait ExtSource {
    fn get(&self, key: &str) -> Option<TypeValue>;
}

#[derive(Clone)]
pub struct ExtSources(Vec<Arc<dyn ExtSource>>);

pub fn compile_filter<'a, I: Payload, O: Payload>(
    symbols: &mut SymbolTable,
    available_sources: ExtSources,
) -> impl Fn(&mut I, &mut O) -> Result<AcceptReject, Box<dyn std::error::Error>> + 'a
{
    let _used_source = available_sources.0[0].clone();

    move |input: &mut I,
          output: &mut O|
          -> Result<AcceptReject, Box<dyn std::error::Error>> {
        input.set("bla".into(), TypeValue::try_from("blub").unwrap());
        output.set("bla".into(), TypeValue::try_from("blub").unwrap());
        Ok(AcceptReject::Accept)
    }
}

