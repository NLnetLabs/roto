use std::{
    collections::VecDeque,
    fmt::{Display, Formatter},
    sync::Arc,
};

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

impl Display for Mir {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (i, command) in self.command_stack.iter().enumerate() {
            writeln!(f, "{:3}: {}", i, command)?;
        }
        Ok(())
    }
}

pub fn compile(
    symbols: GlobalSymbolTable,
) -> Result<(), Box<dyn std::error::Error>> {
    fn unwind(
        mut stack: std::collections::VecDeque<Command>,
    ) -> Vec<Command> {
        let mut v = vec![];
        while let Some(c) = stack.pop_front() {
            v.push(c);
        }
        v
    }

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
        println!(
            "vars: \n{:?}",
            vars.iter().for_each(|t| {
                println!(
                    "{:?} {:?}",
                    t,
                    _module.get_variable_name_by_token(t)
                );
            })
        );
        println!("data_sources: {:?}", data_sources);
        // compile the used vars
        for (mem_block, var) in (0_u32..).zip(vars.into_iter()) {
            // a new block
            let mut mir_block = Mir {
                command_stack: Vec::new(),
            };

            let s = _module.get_variable_by_token(&var);
            print!("\nvar: {:?} ", s.get_token());

            let leaves = s.get_leaf_nodes();
            let mut local_stack =
                std::collections::VecDeque::<Command>::new();

            let mut leaves = leaves.into_iter().peekable();
            println!("\nleaves");
            println!(
                "{:?}",
                leaves.clone().for_each(|lv| {
                    println!(
                        "{:3?} {}",
                        lv.get_token().unwrap(),
                        lv.get_name()
                    )
                })
            );
            while let Some(arg) = leaves.next() {
                print!("a");
                match arg.get_token().unwrap() {
                    // assignment
                    Token::Variable(var) if arg.get_name() == "var" => {
                        print!("V");
                        local_stack.push_front(Command::new(
                            OpCode::MemPosRef,
                            vec![
                                Arg::Variable(var as usize),
                                Arg::MemPos(mem_block),
                            ],
                        ));
                        print!(" {:?}", local_stack);
                        let mut local_st_v = unwind(local_stack);
                        local_st_v.reverse();
                        mir_block.command_stack.extend(local_st_v);
                        local_stack = VecDeque::new();
                    }
                    // concrete value already.
                    Token::Constant => {
                        let val = arg.get_value().unwrap();
                        local_stack.push_front(Command::new(
                            OpCode::PushStack,
                            vec![Arg::Constant(val.as_builtin_type()?)],
                        ));
                    }
                    // external calls
                    Token::Method(method) => {
                        println!(
                            "m {} for {:?}",
                            method,
                            leaves.peek().unwrap().get_type()
                        );
                        let next_arg = leaves.peek().unwrap();

                        let (opcode, mut args) = match next_arg.get_token() {
                            Ok(Token::DataSource(ds)) => (
                                OpCode::ExecuteDataStoreMethod,
                                vec![
                                    Arg::DataSource(ds as usize),
                                    Arg::Method(method as usize),
                                ],
                            ),
                            _ => (
                                OpCode::ExecuteTypeMethod,
                                vec![
                                    Arg::Type(next_arg.get_type()),
                                    Arg::Method(method as usize),
                                ],
                            ),
                        };
                        args.push(Arg::MemPos(mem_block));

                        local_stack.push_front(Command::new(
                            OpCode::PushStack,
                            vec![Arg::MemPos(mem_block)],
                        ));
                        local_stack.push_front(Command::new(opcode, args))
                    }
                    Token::FieldAccess(fa) => {
                        let mut args = vec![];
                        local_stack.push_front(Command::new(
                            OpCode::PushStack,
                            vec![Arg::MemPos(mem_block)],
                        ));
                        args.push(Arg::MemPos(mem_block));
                        args.extend(
                            fa.iter()
                                .map(|t| Arg::FieldAccess(*t as usize))
                                .collect::<Vec<_>>(),
                        );
                        local_stack.push_front(Command::new(
                            OpCode::MemPosOffset,
                            args,
                        ));
                        local_stack.push_front(Command::new(
                            OpCode::PopStack,
                            vec![Arg::MemPos(mem_block)],
                        ));
                    }
                    // roots
                    Token::Variable(var) => {
                        local_stack.push_front(Command::new(
                            OpCode::PushStack,
                            vec![Arg::Variable(var as usize)],
                        ));
                        mir_block.command_stack.extend(unwind(local_stack));
                        local_stack = VecDeque::new();
                    }
                    Token::Argument(arg) => {
                        local_stack.push_front(Command::new(
                            OpCode::PushStack,
                            vec![Arg::Argument(arg as usize)],
                        ));
                        mir_block.command_stack.extend(unwind(local_stack));
                        local_stack = VecDeque::new();
                    }
                    Token::DataSource(_) => {
                        // No further action for a data-source, it's only
                        // used to call methods on, which should already have
                        // been peeked into, by the command before this one.

                        // we are unwinding the local stack, though
                        mir_block.command_stack.extend(unwind(local_stack));
                        local_stack = VecDeque::new();
                    }
                    Token::BuiltinType(_) => {
                        // No further action for a builtin type, it's only
                        // used to call methods on, which should already have
                        // been peeked into, by the command before this one.

                        // we are unwinding the local stack, though
                        mir_block.command_stack.extend(unwind(local_stack));
                        local_stack = VecDeque::new();
                    }
                    Token::Term(_) => todo!(),
                    Token::Action(_) => todo!(),
                    Token::MatchAction(_) => todo!(),
                }
            }
            // mir_block.command_stack.extend(local_stack);
            mir.push(mir_block);
            local_stack.clear();
        }

        for m in &mir {
            println!("\nMIR_block: \n{}", m);
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
