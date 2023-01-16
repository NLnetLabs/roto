use std::{
    collections::VecDeque,
    fmt::{Display, Formatter},
};

use nom::error::VerboseError;

use crate::{
    ast::{self, ShortString, SyntaxTree},
    symbols::{
        DepsGraph, GlobalSymbolTable, MatchActionType, Scope, Symbol,
        SymbolKind, SymbolTable,
    },
    traits::Token,
    types::typedef::TypeDef,
    vm::{
        Arg, Command, ExtDataSource, OpCode, StackRefPos, VariablesMap,
    },
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
type Terms<'a> = Vec<(ShortString, StackRefPos)>;
type Term<'a> = &'a Symbol;
type Action<'a> = &'a Symbol;

#[derive(Debug)]
struct CompilerState<'a> {
    cur_module: &'a SymbolTable,
    local_vars: VariablesMap,
    used_variables: Variables<'a>,
    used_data_sources: DataSources<'a>,
    used_arguments: Arguments<'a>,
    cur_mir_block: MirBlock,
    mem_pos: u32,
    computed_terms: Terms<'a>,
}

#[derive(Debug, Default)]
pub struct Compiler<'a> {
    source_code: &'a str,
    ast: SyntaxTree,
    symbols: GlobalSymbolTable,
}

impl<'a> Compiler<'a> {
    pub fn new(source_code: &'a str) -> Self {
        Compiler {
            source_code,
            ast: SyntaxTree::default(),
            symbols: GlobalSymbolTable::new(),
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
    ) = _module.create_deps_graph()?;

    let mut state = CompilerState {
        cur_module: _module,
        local_vars: VariablesMap::default(),
        used_variables: variables,
        used_data_sources: data_sources,
        used_arguments: arguments,
        computed_terms: Terms::new(),
        cur_mir_block: MirBlock::new(),
        mem_pos: 0,
    };

    // initialize the command stack
    let mut mir: Vec<MirBlock> = vec![];

    println!(
        "======== dependencies for module: {:?} ===========",
        _module
    );

    println!("___used args");
    state
        .used_arguments
        .iter()
        .for_each(|s| println!("{:?}: {:?}", s.1.get_token().unwrap(), s.0));

    println!("___used vars");

    state.used_variables.iter().for_each(|s| {
        println!("{:?} {:?}", s.1.get_token().unwrap(), s.0);
    });

    println!("___used data_sources");

    state.used_data_sources.iter().for_each(|t| {
        println!("{:?} {:?}", t.1.get_token().unwrap(), t.0);
    });

    println!("=================================================");

    // compile the variables used in the terms
    (mir, state) = compile_assignments(mir, state)?;

    state.mem_pos += 1;

    (mir, state) = compile_apply(mir, state)?;

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
                ds.1.get_token().unwrap_or_else(|_| {
                    panic!("Fatal: Cannot find Token for data source.");
                }),
                ds.1.get_type(),
            )
        })
        .collect::<Vec<_>>();

    Ok(RotoPack::new(mir, TypeDef::None, None, args, data_sources))
}

fn compile_expr<'a>(
    symbol: &'a Symbol,
    mut state: CompilerState<'a>,
    // The posision at which to write the Variable assignment. This allows for the use of the
    // state.mem_pos to be used for temp variables.
    var_assign_mem_pos: u32
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
            // assignment to a variable
            Token::Variable(var) if arg.get_name() == "var" => {
                print!("V");

                local_stack.push_front(Command::new(OpCode::ClearStack, vec![]));
                local_stack.push_front(Command::new(
                    OpCode::MemPosRef,
                    vec![Arg::MemPos(var_assign_mem_pos), Arg::Variable(var)],
                ));

                println!("\nlocal_stack {:?}", local_stack);
                state.local_vars.set(var, state.mem_pos, 0).unwrap();
                println!("local_vars {:?}", state.local_vars);

                state.cur_mir_block.command_stack.extend(local_stack);
                local_stack = VecDeque::new();
            }
            // assignment to a constant
            Token::Constant(_) => {
                println!("C");
                state.mem_pos += 1;

                let val = arg.get_value();
                local_stack.push_front(Command::new(
                    OpCode::PushStack,
                    vec![Arg::MemPos(state.mem_pos)],
                ));
                local_stack.push_front(Command::new(
                    OpCode::MemPosSet,
                    vec![
                        Arg::MemPos(state.mem_pos),
                        Arg::Constant(val.as_cloned_builtin()?),
                    ],
                ));

                state.cur_mir_block.command_stack.extend(local_stack);
                local_stack = VecDeque::new();
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
                    Ok(Token::Rib(ds)) => (
                        OpCode::ExecuteDataStoreMethod,
                        vec![
                            Arg::DataSourceRib(ds),
                            Arg::Method(method),
                            Arg::MemPos(state.mem_pos),
                        ],
                    ),
                    Ok(Token::Table(ds)) => (
                        OpCode::ExecuteDataStoreMethod,
                        vec![
                            Arg::DataSourceTable(ds),
                            Arg::Method(method),
                            Arg::MemPos(state.mem_pos),
                        ],
                    ),
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
                    Ok(Token::FieldAccess(_)) => {
                        println!("FIELD_ACCESS arg: {:?}", arg);
                        println!("next_arg: {:?}", next_arg);

                        (
                            // args: [ method_call, return_type ]
                            OpCode::ExecuteValueMethod,
                            vec![
                                Arg::Method(token.into()),
                                Arg::Type(arg.get_type()),
                                Arg::MemPos(state.mem_pos),
                            ],
                        )
                    }
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
            // An RxType in a sub-action can only be used to modify a field on the
            // instance.
            Token::RxType if arg.get_kind() == SymbolKind::SubAction => {
                // the first child of RxType root should hold a FieldAccess,
                // specifying a field of the Rx record, or a MethodCall on
                // the type of the RxType directly.
                println!("got rxtype");

                let first_child = &arg.get_args()[0];
                let mut args_vec = vec![];

                match first_child.get_kind() {
                    SymbolKind::FieldAccess => {
                        let mut field_accesses =
                            if let Token::FieldAccess(v) =
                                first_child.get_token()?
                            {
                                v
                            } else {
                                vec![]
                            };

                        // push in reverse order.
                        local_stack.push_front(Command::new(
                            OpCode::Label,
                            vec![Arg::Label("end-set-rx-type".into())],
                        ));

                        local_stack.push_front(Command::new(
                            OpCode::SetRxField,
                            vec![],
                        ));

                        // explode the FieldAccess vec into separate PushStack commands.
                        field_accesses.reverse();
                        for fa in field_accesses.iter() {
                            local_stack.push_front(Command::new(
                                OpCode::StackOffset,
                                vec![Arg::FieldAccess(*fa as usize)],
                            ));
                            args_vec.push(Arg::FieldAccess(*fa as usize));
                        }

                        // copy Rx to top of the stack
                        local_stack.push_front(Command::new(
                            OpCode::PushStack,
                            vec![Arg::MemPos(0)],
                        ));

                        local_stack.push_front(Command::new(
                            OpCode::Label,
                            vec![Arg::Label("start-set-rx-type".into())],
                        ));
                    }
                    SymbolKind::MethodCall => {
                        todo!();
                    }
                    _ => {
                        return Err(CompileError::new(
                            "Unknown RxType fieldname or method.".into(),
                        ))
                    }
                };

                state
                    .cur_mir_block
                    .command_stack
                    .extend(unwind_stack(local_stack));
                local_stack = VecDeque::new();
            }
            Token::RxType => {
                // RxType instance always lives at MemPos(0). retrieve it
                // and push to stack.
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

fn compile_assignments(
    mut mir: Vec<MirBlock>,
    mut state: CompilerState<'_>,
) -> Result<(Vec<MirBlock>, CompilerState<'_>), CompileError> {
    let _module = state.cur_module;

    // a new block
    state.cur_mir_block = MirBlock {
        command_stack: Vec::new(),
    };

    // compile the used variable assignments. Since the rx and tx value live
    // in memory positions 0 and 1, we start with memory position 2.
    for (mem_pos, var) in
        (2_u32..).zip(state.used_variables.clone().iter())
    {
        // set the mem_pos in state to the counter we use here. Recursive
        // `compile_expr` may increase state.mem_pos to temporarily store
        // argument variables.
        state.mem_pos = mem_pos;

        let s = _module.get_variable_by_token(&var.1.get_token()?);
        print!("\nvar: {:?} ({})", s.get_token(), mem_pos);

        state.cur_mir_block.command_stack.push(Command::new(
            OpCode::Label,
            vec![Arg::Label(format!("ASSIGNMENT {}", var.0).as_str().into())],
        ));

        state = compile_expr(s, state, mem_pos)?;

        mir.push(state.cur_mir_block);
        state.cur_mir_block = MirBlock {
            command_stack: Vec::new(),
        };
    }

    Ok((mir, state))
}

// This is not used currently, it will compile *ALL* terms, regardless of
// whether they are actually consumed by the (match) actions.
fn _compile_terms(
    mut mir: Vec<MirBlock>,
    mut state: CompilerState<'_>,
) -> Result<(Vec<MirBlock>, CompilerState<'_>), CompileError> {
    let _module = state.cur_module;

    // a new block
    state.cur_mir_block = MirBlock {
        command_stack: Vec::new(),
    };

    let terms = _module.get_terms();
    let mut terms_iter = terms.iter().peekable();

    // compile all the terms.
    while let Some(term) = &mut terms_iter.next() {
        state = compile_term(term, state)?;

        // store the resulting value into a variable so that future references
        // to this term can directly use the result instead of doing the whole
        // computation again.
        state
            .computed_terms
            .push((term.get_name(), state.mem_pos.into()));

        state.mem_pos += 1;

        // move the current mir block to the end of all the collected MIR.
        mir.push(state.cur_mir_block);

        // continue with a fresh block
        state.cur_mir_block = MirBlock {
            command_stack: Vec::new(),
        };
    }

    Ok((mir, state))
}

fn compile_apply(
    mut mir: Vec<MirBlock>,
    mut state: CompilerState<'_>,
) -> Result<(Vec<MirBlock>, CompilerState), CompileError> {
    let match_actions = state.cur_module.get_match_actions();

    // Collect the terms that we need to compile.
    for match_action in match_actions {
        let term_name = match_action[0].get_name();

        // See if it was already compiled earlier on.
        let term = state.computed_terms.iter().find(|t| t.0 == term_name);

        match term {
            // yes, it was, create a reference to the result on the stack
            Some((.., stack_ref_pos)) => {
                if let StackRefPos::MemPos(mem_pos) = stack_ref_pos {
                    state.cur_mir_block.command_stack.extend(vec![
                        Command::new(
                            OpCode::Label,
                            vec![Arg::Label(
                                format!(
                                    "COPY TERM RESULT {}",
                                    term_name.clone()
                                )
                                .as_str()
                                .into(),
                            )],
                        ),
                        Command::new(
                            OpCode::PushStack,
                            vec![Arg::MemPos(*mem_pos)],
                        ),
                    ]);
                };
            }
            // no, compile the term.
            _ => {
                let _module = state.cur_module;
                let terms = _module.get_terms();
                let term =
                    terms.iter().find(|t| t.get_name() == term_name).unwrap();
                state = compile_term(term, state)?;

                // store the resulting value into a variable so that future references
                // to this term can directly use the result instead of doing the whole
                // computation again.
                state
                    .computed_terms
                    .push((term_name.clone(), state.mem_pos.into()));
            }
        };

        state.mem_pos += 1;

        // move the current mir block to the end of all the collected MIR.
        mir.push(state.cur_mir_block);

        // continue with a fresh block
        state.cur_mir_block = MirBlock {
            command_stack: Vec::new(),
        };

        if let SymbolKind::MatchAction(ma) = match_action[0].get_kind() {
            match ma {
                MatchActionType::MatchAction => {
                    state.cur_mir_block.command_stack.extend([
                        Command::new(
                            OpCode::Label,
                            vec![Arg::Label(
                                format!(
                                    "MATCH ACTION {}-{:?}",
                                    term_name.clone(),
                                    match_action[0].get_kind()
                                )
                                .as_str()
                                .into(),
                            )],
                        ),
                        Command::new(OpCode::CondFalseSkipToEOB, vec![]),
                    ]);
                }
                MatchActionType::NegateMatchAction => {
                    state.cur_mir_block.command_stack.extend(vec![
                        Command::new(
                            OpCode::Label,
                            vec![Arg::Label(
                                format!(
                                    "MATCH ACTION NEGATE {}-{:?}",
                                    term_name,
                                    match_action[0].get_kind()
                                )
                                .as_str()
                                .into(),
                            )],
                        ),
                        Command::new(OpCode::CondTrueSkipToEOB, vec![]),
                    ]);
                }
            }
        } else {
            return Err(CompileError::new("invalid match action".into()));
        };

        // collect all actions included in this match_action and compile them
        for action in match_action[0].get_args() {
            let _module = state.cur_module;
            let action_name = action.get_name();
            let actions = _module.get_actions();
            let action = actions
                .iter()
                .find(|t| t.get_name() == action_name)
                .unwrap();

            state = compile_action(action, state)?;
        }

        // move the current mir block to the end of all the collected MIR.
        mir.push(state.cur_mir_block);

        // continue with a fresh block
        state.cur_mir_block = MirBlock {
            command_stack: Vec::new(),
        };
    }

    Ok((mir, state))
}

fn compile_action<'a>(
    action: Action<'a>,
    mut state: CompilerState<'a>,
) -> Result<CompilerState<'a>, CompileError> {
    println!("-> action {:#?}", action);

    for sub_action in action.get_args() {
        state = compile_sub_action(sub_action, state)?;
        // state = compile_var(action, state)?;
    }

    Ok(state)
}

fn compile_sub_action<'a>(
    sub_action: Action<'a>,
    mut state: CompilerState<'a>,
) -> Result<CompilerState<'a>, CompileError> {
    println!(
        "sub-action {:?} {:?}",
        sub_action.get_name(),
        sub_action.get_kind()
    );

    match sub_action.get_kind() {
        SymbolKind::SubAction => {
            println!("constant in action {:#?}", sub_action);
            let start_pos = state.mem_pos;
            state = compile_expr(sub_action, state, start_pos)?;
            state.mem_pos += 1;
            println!("ACTION BLOCK {:?}", state.cur_mir_block.command_stack);
        }
        _ => {
            return Err(CompileError::new(format!(
                "Invalid sub-action {}",
                sub_action.get_name()
            )))
        }
    }

    Ok(state)
}

fn compile_term<'a>(
    term: Term<'a>,
    mut state: CompilerState<'a>,
) -> Result<CompilerState<'a>, CompileError> {
    println!("term {:?} {:?}", term.get_name(), term.get_kind());

    // Set a Label so that each term block is identifieble for humans.
    state.cur_mir_block.command_stack.push(Command::new(
        OpCode::Label,
        vec![Arg::Label(
            format!("TERM {}", term.get_name()).as_str().into(),
        )],
    ));

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
        print!("term {:?}", arg);

        state = compile_sub_term(arg, state)?;

        assert_ne!(state.cur_mir_block.command_stack.len(), 0);

        // Since sub-terms are ANDed we can create an early return after
        // each sub-term that isn't last.
        if sub_terms.peek().is_some() {
            state
                .cur_mir_block
                .command_stack
                .push(Command::new(OpCode::CondFalseSkipToEOB, vec![]));
        }
    }

    Ok(state)
}

fn compile_sub_term<'a>(
    sub_term: Term<'a>,
    mut state: CompilerState<'a>,
) -> Result<CompilerState<'a>, CompileError> {
    println!(
        "sub-term {:?} {:?}",
        sub_term.get_name(),
        sub_term.get_kind()
    );

    let saved_mem_pos = state.mem_pos;
    match sub_term.get_kind() {
        SymbolKind::CompareExpr(op) => {
            let args = sub_term.get_args();
            let start_pos = state.mem_pos;
            state = compile_expr(&args[0], state, start_pos)?;
            state.mem_pos += 1;
            state = compile_expr(&args[1], state, start_pos)?;

            state
                .cur_mir_block
                .command_stack
                .push(Command::new(OpCode::Cmp, vec![op.into()]));
        }
        SymbolKind::OrExpr => {
            let args = sub_term.get_args();
            state = compile_sub_term(&args[0], state)?;
            state.mem_pos += 1;
            state = compile_sub_term(&args[1], state)?;

            state.cur_mir_block.command_stack.push(Command::new(
                OpCode::Cmp,
                vec![Arg::CompareOp(ast::CompareOp::Or)],
            ));
        }
        SymbolKind::AndExpr => {
            let args = sub_term.get_args();
            state = compile_sub_term(&args[0], state)?;
            state.mem_pos += 1;
            state = compile_sub_term(&args[1], state)?;

            state.cur_mir_block.command_stack.push(Command::new(
                OpCode::Cmp,
                vec![Arg::CompareOp(ast::CompareOp::And)],
            ));
        }
        SymbolKind::NotExpr => {
            panic!("NOT NOT!");
        }
        _ => {
            println!(
                "compile-var sub-term {:?} {:?}",
                sub_term.get_name(),
                sub_term.get_kind()
            );
            let start_pos = state.mem_pos;
            state = compile_expr(sub_term, state, start_pos)?;
            println!("command_stack {:?}", state.cur_mir_block.command_stack);
        }
    };

    // restore old mem_pos, let's not waste memory
    state.mem_pos = saved_mem_pos;

    Ok(state)
}
