use std::sync::Arc;

use crate::{
    ast::{AcceptReject, ShortString},
    symbols::{GlobalSymbolTable, Scope, SymbolTable},
    traits::Token,
    types::typevalue::TypeValue,
    vm::{Arg, Command, OpCode},
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

#[derive(Debug)]
pub struct Mir {
    command_stack: Vec<Command>,
}

impl Mir {
    pub fn new() -> Self {
        Mir {
            command_stack: Vec::new(),
        }
    }

    pub fn push_command(&mut self, command: Command) {
        self.command_stack.push(command);
    }

    pub fn compile(&mut self, ast: &AcceptReject) {
        todo!()
    }
}

pub fn compile(
    symbols: GlobalSymbolTable,
) -> Result<(), Box<dyn std::error::Error>> {
    println!("Start compiling...");

    // get all symbols that are used in the filter terms.
    let mut _global = symbols.borrow_mut();
    let (_global_mod, modules) = Some::<(Vec<Scope>, Vec<Scope>)>(
        _global.keys().cloned().partition(|m| *m == Scope::Global),
    )
    .unwrap();

    drop(_global);
    let mut _global = symbols.borrow_mut();

    println!("Found modules: {:?}", modules);
    let mut args;
    let mut vars;
    let mut data_sources;

    // initialize the command stack
    let mut mir: Vec<Mir> = vec![];

    for module in modules {
        let _module = _global.get_mut(&module).unwrap();
        (args, vars, data_sources) = _module.get_term_deps();

        println!("args: {:?}", args);
        println!("vars: {:?}", vars);
        println!("data_sources: {:?}", data_sources);



        // compile the used vars
        for var in vars {
            // a new block
            let mut mir_block = Mir {
                command_stack: Vec::new(),
            };

            let s = _module.get_variable_by_token(var);
            print!("\nvar: {:?}  ", s.get_name());

            let leaves = s.get_leaf_nodes();

            for arg in leaves {
                print!("a");
                match arg.get_token().unwrap() {
                    Token::Constant => {
                        let val = arg.get_value().unwrap();
                        mir_block.push_command(Command::new(
                            OpCode::LoadConstant,
                            vec![Arg::Constant(val.as_builtin_type()?)],
                        ));
                    }
                    Token::Variable(var) => {
                        mir_block.push_command(Command::new(
                            OpCode::StoreVar,
                            vec![Arg::Register(var as usize)],
                        ));
                    },
                    Token::Method(method) => {
                        mir_block.push_command(Command::new(
                            OpCode::ExecuteMethod,
                            vec![Arg::Register(method as usize)],
                        ))
                    },
                    Token::Argument(arg) => {
                        mir_block.push_command(Command::new(
                            OpCode::LoadArgument,
                            vec![Arg::Register(arg as usize)],
                        ));
                    },
                    Token::DataSource(ds) => {
                        mir_block.push_command(Command::new(
                            OpCode::DataSourceCommand,
                            vec![Arg::Register(ds as usize)],
                        ));
                    },
                    Token::FieldAccess(fa) => {
                        mir_block.push_command(Command::new(
                            OpCode::LoadField,
                            fa.iter().map(|t| Arg::Register(*t as usize)).collect(),
                        ));
                    },
                    Token::Term(_) => todo!(),
                    Token::Action(_) => todo!(),
                    Token::MatchAction(_) => todo!(),
                    Token::BuiltinType(_) => todo!(),
                }
            }
            mir.push(mir_block);
        }

        for m in &mir {
            println!("\nMIR_block: {:?}", m);
        }
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
