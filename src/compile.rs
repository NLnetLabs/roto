use std::{
    collections::VecDeque,
    fmt::{Display, Formatter},
};

use crate::{
    ast::{AcceptReject, ShortString},
    symbols::{DepsGraph, GlobalSymbolTable, Scope, Symbol},
    traits::Token,
    types::typedef::TypeDef,
    vm::{Arg, Command, OpCode},
};

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
pub struct RotoPack {
    pub mir: Vec<MirBlock>,
    pub rx_type: TypeDef,
    pub tx_type: Option<TypeDef>,
    pub arguments: Vec<(usize, TypeDef)>,
    pub data_sources: Vec<(usize, TypeDef)>,
}

impl RotoPack {
    fn new(
        mir: Vec<MirBlock>,
        rx_type: TypeDef,
        tx_type: Option<TypeDef>,
        arguments: Vec<(usize, TypeDef)>,
        data_sources: Vec<(usize, TypeDef)>,
    ) -> Self {
        RotoPack {
            mir,
            rx_type,
            tx_type,
            arguments,
            data_sources,
        }
    }
}

#[derive(Debug)]
pub struct MirBlock {
    pub command_stack: Vec<Command>,
}

impl MirBlock {
    pub fn new() -> Self {
        MirBlock {
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

impl Display for MirBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (i, command) in self.command_stack.iter().enumerate() {
            writeln!(f, "{:3}: {}", i, command)?;
        }
        Ok(())
    }
}

pub fn compile(
    symbols: GlobalSymbolTable,
) -> Result<RotoPack, Box<dyn std::error::Error>> {
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
    let arguments: Vec<(&ShortString, &Symbol)> = vec![];
    let variables: Vec<(&ShortString, &Symbol)> = vec![];
    let data_sources: Vec<(ShortString, &Symbol)> = vec![];

    // initialize the command stack
    let mut mir: Vec<MirBlock> = vec![];

    for module in modules {
        let _module = _global.get(&module).unwrap();
        let (
            rx_type,
            tx_type,
            DepsGraph {
                arguments,
                variables,
                data_sources,
            },
        ) = _module.create_terms_graph()?;

        println!(
            "======== dependencies for module: {:?} ===========",
            module
        );

        println!("___args");
        arguments.iter().for_each(|s| {
            println!("{:?}: {:?}", s.1.get_token().unwrap(), s.0)
        });

        println!("___vars");

        variables.iter().for_each(|s| {
            println!("{:?} {:?}", s.1.get_token().unwrap(), s.0);
        });

        println!("___data_sources");

        data_sources.iter().for_each(|t| {
            println!("{:?} {:?}", t.1.get_token().unwrap(), t.0);
        });

        println!("=================================================");

        // compile the used vars,
        for (mut mem_pos, var) in (2_u32..)
            .zip(variables.into_iter().map(|s| s.1.get_token().unwrap()))
        {
            // a new block
            let mut mir_block = MirBlock {
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
            while let Some(arg) = &mut leaves.next() {
                print!("a");
                let token = arg.get_token().unwrap();
                match token {
                    // assignment
                    Token::Variable(var) if arg.get_name() == "var" => {
                        print!("V");
                        local_stack.push_front(Command::new(
                            OpCode::MemPosRef,
                            vec![
                                Arg::Variable(var as usize),
                                Arg::MemPos(mem_pos),
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
                        let val = arg.get_value();
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
                                    Arg::Method(token.into()),
                                ],
                            ),
                            Ok(Token::BuiltinType(_)) => (
                                OpCode::ExecuteTypeMethod,
                                vec![
                                    Arg::Type(next_arg.get_type()),
                                    Arg::Method(token.into()),
                                ],
                            ),
                            Ok(Token::Variable(_)) => {
                                (
                                    OpCode::ExecuteValueMethod,
                                    vec![
                                        Arg::MemPos(mem_pos),
                                        Arg::Method(token.into()),
                                    ],
                                )
                            },
                            Ok(Token::FieldAccess(_)) => (
                                OpCode::ExecuteValueMethod,
                                vec![
                                    Arg::MemPos(mem_pos),
                                    Arg::Method(token.into()),
                                ],
                            ),
                            _ => {
                                return Err(
                                    format!(
                                        "Invalid token for method call: {:?}",
                                        next_arg.get_token()
                                    ).into()
                                )
                            }
                        };
                        args.push(Arg::MemPos(mem_pos));

                        local_stack.push_front(Command::new(
                            OpCode::PushStack,
                            vec![Arg::MemPos(mem_pos)],
                        ));
                        local_stack.push_front(Command::new(opcode, args))
                    }
                    Token::FieldAccess(fa) => {
                        let mut args = vec![];
                        // local_stack.push_front(Command::new(
                        //     OpCode::PushStack,
                        //     vec![Arg::MemPos(mem_pos)],
                        // ));
                        // args.push(Arg::MemPos(mem_pos));
                        args.extend(
                            fa.iter()
                                .map(|t| Arg::FieldAccess(*t as usize))
                                .collect::<Vec<_>>(),
                        );
                        local_stack.push_front(Command::new(
                            OpCode::StackOffset,
                            args,
                        ));
                        // local_stack.push_front(Command::new(
                        //     OpCode::PopStack,
                        //     vec![Arg::MemPos(mem_pos)],
                        // ));
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
                            OpCode::ArgToMemPos,
                            vec![
                                Arg::Argument(arg as usize),
                                Arg::MemPos(mem_pos),
                            ],
                        ));
                        mem_pos += 1;
                        mir_block.command_stack.extend(unwind(local_stack));
                        local_stack = VecDeque::new();
                    }
                    Token::RxType => { local_stack.push_front(Command::new(
                        OpCode::PushStack,
                        vec![
                            Arg::MemPos(0),
                        ],
                    ));
                    mir_block.command_stack.extend(unwind(local_stack));
                    local_stack = VecDeque::new();
                } ,
                    Token::TxType => { local_stack.push_front(Command::new(
                        OpCode::PushStack,
                        vec![
                            Arg::MemPos(1),
                        ],
                    ));
                    // mir_block.command_stack.extend(unwind(local_stack));
                    // local_stack = VecDeque::new();
                },
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

    println!("args before packing:  {:?}", arguments);

    let args = arguments
        .iter()
        .map(|a| {
            (
                a.1.get_token()
                    .unwrap_or_else(|_| {
                        panic!("Fatal: Cannot find Token for Argument.");
                    })
                    .into(),
                a.1.get_type(),
            )
        })
        .collect::<Vec<_>>();

    let data_sources = data_sources
        .iter()
        .map(|ds| {
            (
                ds.1.get_token()
                    .unwrap_or_else(|_| {
                        panic!("Fatal: Cannot find Token for data source.");
                    })
                    .into(),
                ds.1.get_type(),
            )
        })
        .collect::<Vec<_>>();

    Ok(RotoPack::new(mir, TypeDef::None, None, args, data_sources))
}
