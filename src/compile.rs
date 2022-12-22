use std::{
    collections::VecDeque,
    fmt::{Display, Formatter},
};

use nom::error::VerboseError;

use crate::{
    ast::{self, AcceptReject, ShortString, SyntaxTree},
    symbols::{
        DepsGraph, GlobalSymbolTable, Scope, Symbol, SymbolKind, SymbolTable,
    },
    traits::Token,
    types::{
        builtin::{Boolean, BuiltinTypeValue},
        typedef::TypeDef,
        typevalue::TypeValue,
    },
    vm::{Arg, Command, ExtDataSource, OpCode, VariablesMap},
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

#[derive(Debug, Default)]
pub struct RotoPack {
    pub mir: Vec<MirBlock>,
    pub rx_type: TypeDef,
    pub tx_type: Option<TypeDef>,
    pub arguments: Vec<(usize, TypeDef)>,
    pub data_sources: Vec<ExtDataSource>,
}

impl RotoPack {
    fn new(
        mir: Vec<MirBlock>,
        rx_type: TypeDef,
        tx_type: Option<TypeDef>,
        arguments: Vec<(usize, TypeDef)>,
        data_sources: Vec<ExtDataSource>,
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

#[derive(Debug, Default)]
pub struct CompileError {
    message: String,
}

impl CompileError {
    pub fn new(message: String) -> Self {
        CompileError { message }
    }
}

impl From<String> for CompileError {
    fn from(message: String) -> Self {
        CompileError { message }
    }
}

impl From<&str> for CompileError {
    fn from(message: &str) -> Self {
        CompileError {
            message: message.to_string(),
        }
    }
}

impl Display for CompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

type Arguments<'a> = Vec<(ShortString, &'a Symbol)>;
type DataSources<'a> = Vec<(ShortString, &'a Symbol)>;
type Variables<'a> = Vec<(ShortString, &'a Symbol)>;
type Terms<'a> = Vec<(ShortString, &'a Symbol)>;
type Term<'a> = &'a Symbol;

#[derive(Debug)]
struct CompilerState<'a> {
    cur_module: &'a SymbolTable,
    local_vars: VariablesMap,
    used_variables: Variables<'a>,
    used_data_sources: DataSources<'a>,
    used_arguments: Arguments<'a>,
    cur_mir_block: MirBlock,
    mem_pos: u32,
}

#[derive(Debug, Default)]
pub struct Compiler<'a> {
    source_code: &'a str,
    ast: SyntaxTree,
    symbols: GlobalSymbolTable,
    mir: Vec<MirBlock>,
}

impl<'a> Compiler<'a> {
    pub fn new(source_code: &'a str) -> Self {
        Compiler {
            source_code,
            ast: SyntaxTree::default(),
            symbols: GlobalSymbolTable::new(),
            mir: Vec::new(),
        }
    }

    pub fn parse_source_code(
        &mut self,
        source_code: &'a str,
    ) -> Result<(), VerboseError<&'a str>> {
        self.source_code = source_code;
        self.ast = SyntaxTree::parse_str(self.source_code)?.1;

        Ok(())
    }

    pub fn eval_ast(&mut self) -> Result<(), CompileError> {
        self.ast.eval(self.symbols.clone())?;
        Ok(())
    }

    pub fn compile(self) -> Vec<Result<RotoPack, CompileError>> {
        println!("Start compiling...");

        // get all symbols that are used in the filter terms.
        let mut _global = self.symbols.borrow_mut();
        let (_global_mod, modules) = Some::<(Vec<Scope>, Vec<Scope>)>(
            _global.keys().cloned().partition(|m| *m == Scope::Global),
        )
        .unwrap();

        drop(_global);
        let mut _global = self.symbols.borrow_mut();

        // each module outputs one roto-pack with its own MIR (composed of MIR blocks),
        // its used arguments and used data sources.
        let mut roto_packs = vec![];

        for module in modules {
            let _module = _global.get(&module).unwrap();
            roto_packs.push(compile_module(_module));
        }

        roto_packs
    }

    pub fn build(
        source_code: &'a str,
    ) -> Vec<Result<RotoPack, CompileError>> {
        let mut compiler = Compiler::new(source_code);
        compiler.parse_source_code(source_code).unwrap();
        compiler.eval_ast().unwrap();
        println!("symbols = {:#?}", compiler.symbols);
        compiler.compile()
    }
}

#[derive(Debug, Default)]
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
}

impl Display for MirBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (i, command) in self.command_stack.iter().enumerate() {
            writeln!(f, "{:3}: {}", i, command)?;
        }
        Ok(())
    }
}

// pub fn compile(
//     symbols: GlobalSymbolTable,
// ) -> Vec<Result<RotoPack, CompileError>> {
//     println!("Start compiling...");

//     // get all symbols that are used in the filter terms.
//     let mut _global = symbols.borrow_mut();
//     let (_global_mod, modules) = Some::<(Vec<Scope>, Vec<Scope>)>(
//         _global.keys().cloned().partition(|m| *m == Scope::Global),
//     )
//     .unwrap();

//     drop(_global);
//     let mut _global = symbols.borrow_mut();

//     // each module outputs one roto-pack with its own MIR (composed of MIR blocks),
//     // its used arguments and used data sources.
//     let mut roto_packs = vec![];

//     for module in modules {
//         let _module = _global.get(&module).unwrap();
//         roto_packs.push(compile_module(_module));
//     }

//     roto_packs
// }

fn unwind_stack(
    mut stack: std::collections::VecDeque<Command>,
) -> Vec<Command> {
    let mut v = vec![];
    while let Some(c) = stack.pop_front() {
        v.push(c);
    }
    v
}

fn compile_module(_module: &SymbolTable) -> Result<RotoPack, CompileError> {
    let (
        _rx_type,
        _tx_type,
        DepsGraph {
            arguments,
            variables,
            data_sources,
        },
    ) = _module.create_terms_graph()?;

    let mut state = CompilerState {
        cur_module: _module,
        local_vars: VariablesMap::default(),
        used_variables: variables,
        used_data_sources: data_sources,
        used_arguments: arguments,
        cur_mir_block: MirBlock::new(),
        mem_pos: 0,
    };

    // let mut used_arguments: Arguments = vec![];
    // let mut used_data_sources: DataSources = vec![];

    // initialize the command stack
    let mut mir: Vec<MirBlock> = vec![];

    println!(
        "======== dependencies for module: {:?} ===========",
        _module
    );

    println!("___args");
    state
        .used_arguments
        .iter()
        .for_each(|s| println!("{:?}: {:?}", s.1.get_token().unwrap(), s.0));

    println!("___vars");

    state.used_variables.iter().for_each(|s| {
        println!("{:?} {:?}", s.1.get_token().unwrap(), s.0);
    });

    println!("___data_sources");

    state.used_data_sources.iter().for_each(|t| {
        println!("{:?} {:?}", t.1.get_token().unwrap(), t.0);
    });

    println!("=================================================");

    // compile the variables used in the terms
    (mir, state) = compile_vars(mir, state)?;

    state.mem_pos += 1;

    (mir, state) = compile_terms(mir, state)?;

    for m in &mir {
        println!("\nMIR_block: \n{}", m);
    }

    let args = state
        .used_arguments
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

    let data_sources = state
        .used_data_sources
        .iter()
        .map(|ds| {
            ExtDataSource::new(
                &ds.1.get_name(),
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

fn compile_var<'a>(
    symbol: &'a Symbol,
    mut state: CompilerState<'a>,
) -> Result<CompilerState<'a>, CompileError> {
    let leaves = symbol.get_leaf_nodes();
    let mut local_stack = std::collections::VecDeque::<Command>::new();

    let mut leaves = leaves.into_iter().peekable();
    println!("\nleaves");
    println!(
        "{:?}",
        leaves.clone().for_each(|lv| {
            println!("{:3?} {}", lv.get_token().unwrap(), lv.get_name())
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
                    vec![Arg::MemPos(state.mem_pos), Arg::Variable(var)],
                ));
                println!("\nlocal_stack {:?}", local_stack);
                state.local_vars.set(var, state.mem_pos, 0).unwrap();
                println!("local_vars {:?}", state.local_vars);
                state.cur_mir_block.command_stack.extend(local_stack);
                local_stack = VecDeque::new();
            }
            // concrete value already.
            Token::Constant => {
                let val = arg.get_value();
                local_stack.push_front(Command::new(
                    OpCode::MemPosSet,
                    vec![
                        Arg::MemPos(state.mem_pos),
                        Arg::Constant(val.as_cloned_builtin()?),
                    ],
                ));
                local_stack.push_front(Command::new(
                    OpCode::PushStack,
                    vec![Arg::MemPos(state.mem_pos)],
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

                let (opcode, args) = match next_arg.get_token() {
                    Ok(Token::Rib(ds) | Token::Table(ds)) => {
                        state
                            .used_data_sources
                            .push((next_arg.get_name(), next_arg));
                        (
                            OpCode::ExecuteDataStoreMethod,
                            vec![
                                Arg::DataSource(ds),
                                Arg::Method(method),
                                Arg::MemPos(state.mem_pos),
                            ],
                        )
                    }
                    Ok(Token::BuiltinType(_)) => {
                        println!("arg : {:?}", arg);
                        println!("next_arg : {:?}", next_arg);

                        (
                            // args: [ call_type, method_call, return_type ]
                            OpCode::ExecuteTypeMethod,
                            vec![
                                Arg::Type(arg.get_type()),
                                Arg::Method(token.into()),
                                Arg::Type(next_arg.get_type()),
                                Arg::MemPos(state.mem_pos),
                            ],
                        )
                    }
                    Ok(Token::Variable(_)) => (
                        // args: [ method_call, return_type ]
                        OpCode::ExecuteValueMethod,
                        vec![
                            Arg::Method(token.into()),
                            Arg::Type(arg.get_type()),
                            Arg::MemPos(state.mem_pos),
                        ],
                    ),
                    Ok(Token::FieldAccess(_)) => (
                        // args: [ method_call, return_type ]
                        OpCode::ExecuteValueMethod,
                        vec![
                            Arg::Method(token.into()),
                            Arg::Type(arg.get_type()),
                            Arg::MemPos(state.mem_pos),
                        ],
                    ),
                    _ => {
                        return Err(format!(
                            "Invalid token for method call: {:?}",
                            next_arg.get_token()
                        )
                        .into())
                    }
                };

                local_stack.push_front(Command::new(
                    OpCode::PushStack,
                    vec![Arg::MemPos(state.mem_pos)],
                ));
                local_stack.push_front(Command::new(opcode, args))
            }
            Token::FieldAccess(fa) => {
                let mut args = vec![];
                args.extend(
                    fa.iter()
                        .map(|t| Arg::FieldAccess(*t as usize))
                        .collect::<Vec<_>>(),
                );
                local_stack
                    .push_front(Command::new(OpCode::StackOffset, args));
            }
            // roots
            Token::Variable(var) => {
                println!("get var: {:?}", var);
                println!("local vars {:?}", state.local_vars);
                let mem_pos =
                    state.local_vars.get_by_token_value(var).unwrap();
                local_stack.push_front(Command::new(
                    OpCode::PushStack,
                    vec![Arg::MemPos(mem_pos.mem_pos)],
                ));
                state
                    .cur_mir_block
                    .command_stack
                    .extend(unwind_stack(local_stack));
                local_stack = VecDeque::new();
            }
            Token::Argument(arg_arg) => {
                state.used_arguments.push((arg.get_name(), arg));
                local_stack.push_front(Command::new(
                    OpCode::ArgToMemPos,
                    vec![Arg::Argument(arg_arg), Arg::MemPos(state.mem_pos)],
                ));
                state
                    .cur_mir_block
                    .command_stack
                    .extend(unwind_stack(local_stack));
                local_stack = VecDeque::new();
            }
            Token::RxType => {
                local_stack.push_front(Command::new(
                    OpCode::PushStack,
                    vec![Arg::MemPos(0)],
                ));
                state
                    .cur_mir_block
                    .command_stack
                    .extend(unwind_stack(local_stack));
                local_stack = VecDeque::new();
            }
            Token::TxType => {
                local_stack.push_front(Command::new(
                    OpCode::PushStack,
                    vec![Arg::MemPos(1)],
                ));
            }
            Token::Rib(_) | Token::Table(_) => {
                // No further action for a data-source, it's only
                // used to call methods on, which should already have
                // been peeked into, by the command before this one.

                // we are unwinding the local stack, though
                state
                    .cur_mir_block
                    .command_stack
                    .extend(unwind_stack(local_stack));
                local_stack = VecDeque::new();
            }
            Token::BuiltinType(_) => {
                // No further action for a builtin type, it's only
                // used to call methods on, which should already have
                // been peeked into, by the command before this one.

                // we are unwinding the local stack, though
                state
                    .cur_mir_block
                    .command_stack
                    .extend(unwind_stack(local_stack));
                local_stack = VecDeque::new();
            }
            Token::Term(_) => todo!(),
            Token::Action(_) => todo!(),
            Token::MatchAction(_) => todo!(),
        }
    }
    Ok(state)
}

fn compile_vars(
    mut mir: Vec<MirBlock>,
    mut state: CompilerState<'_>,
) -> Result<(Vec<MirBlock>, CompilerState<'_>), CompileError> {
    // a new block
    state.cur_mir_block = MirBlock {
        command_stack: Vec::new(),
    };

    let _module = state.cur_module;

    // compile the used vars. Since the rx and tx value live in memory
    // positions 0 and 1, we start with memory position 2.
    for (mem_pos, var) in (2_u32..).zip(
        state
            .used_variables
            .clone()
            .iter()
            .map(|s| s.1.get_token().unwrap()),
    ) {
        state.mem_pos = mem_pos;
        let s = _module.get_variable_by_token(&var);
        print!("\nvar: {:?} ", s.get_token());


        state = compile_var(s, state)?;

        mir.push(state.cur_mir_block);
        state.cur_mir_block = MirBlock {
            command_stack: Vec::new(),
        };
    }

    Ok((mir, state))
}

fn compile_terms(
    mut mir: Vec<MirBlock>,
    mut state: CompilerState<'_>,
) -> Result<(Vec<MirBlock>, CompilerState<'_>), CompileError> {
    let _module = state.cur_module;

    // compile the terms.
    for term in _module.get_terms() {
        let sub_terms = term.get_args();

        let mut sub_terms = sub_terms.iter().peekable();
        println!("\nsub terms {}", term.get_name());
        println!(
            "{:?}",
            sub_terms.clone().for_each(|lv| {
                println!("{:3?} {:3?}", lv.get_name(), lv.get_kind());
            })
        );

        while let Some(arg) = &mut sub_terms.next() {
            print!("a");

            (mir, state) = compile_term(arg, state, mir)?;

            state
                .cur_mir_block
                .command_stack
                .push(Command::new(OpCode::ReturnIfFalse, vec![]));

            mir.push(state.cur_mir_block);

            state.cur_mir_block = MirBlock {
                command_stack: Vec::new(),
            };
        }
    }

    Ok((mir, state))
}

fn compile_term<'a>(
    sub_term: Term<'a>,
    mut state: CompilerState<'a>,
    mut mir: Vec<MirBlock>,
) -> Result<(Vec<MirBlock>, CompilerState<'a>), CompileError> {
    println!(
        "sub-term {:?} {:?}",
        sub_term.get_name(),
        sub_term.get_kind()
    );
    let saved_mem_pos = state.mem_pos;
    match sub_term.get_kind() {
        SymbolKind::CompareExpr(op) => {
            let args = sub_term.get_args();
            state = compile_var(&args[0], state)?;
            state.mem_pos += 1;
            state = compile_var(&args[1], state)?;

            state
                .cur_mir_block
                .command_stack
                .push(Command::new(OpCode::Cmp, vec![op.into()]));
            state
                .cur_mir_block
                .command_stack
                .push(Command::new(OpCode::ReturnIfFalse, vec![]));
        }
        SymbolKind::LogicalExpr if sub_term.get_name() == "or_expr" => {
            let args = sub_term.get_args();
            (mir, state) = compile_term(&args[0], state, mir)?;
            state.mem_pos += 1;
            (mir, state) = compile_term(&args[1], state, mir)?;

            state.cur_mir_block.command_stack.push(Command::new(
                OpCode::Cmp,
                vec![Arg::CompareOp(ast::CompareOp::Or)],
            ));
            state
                .cur_mir_block
                .command_stack
                .push(Command::new(OpCode::ReturnIfFalse, vec![]));
        }
        SymbolKind::LogicalExpr if sub_term.get_name() == "and_expr" => {
            let args = sub_term.get_args();
            (mir, state) = compile_term(&args[0], state, mir)?;
            state.mem_pos += 1;
            (mir, state) = compile_term(&args[1], state, mir)?;

            state.cur_mir_block.command_stack.push(Command::new(
                OpCode::Cmp,
                vec![Arg::CompareOp(ast::CompareOp::And)],
            ));
            state
                .cur_mir_block
                .command_stack
                .push(Command::new(OpCode::ReturnIfFalse, vec![]));
        }
        SymbolKind::NotExpr => {}
        SymbolKind::AndExpr => {}
        SymbolKind::OrExpr => {}
        _ => {
            state = compile_var(sub_term, state)?;
        }
    }

    mir.push(state.cur_mir_block);

    // a new block
    state.cur_mir_block = MirBlock {
        command_stack: Vec::new(),
    };

    // restore old mem_pos, let's not waste memory
    state.mem_pos = saved_mem_pos;

    Ok((mir, state))
}
