use std::fmt::{Display, Formatter};

use nom::error::VerboseError;

use crate::{
    ast::{self, AcceptReject, ShortString, SyntaxTree},
    symbols::{
        DepsGraph, GlobalSymbolTable, MatchActionType, Scope, Symbol,
        SymbolKind, SymbolTable,
    },
    traits::Token,
    types::typedef::TypeDef,
    vm::{Arg, Command, ExtDataSource, OpCode, StackRefPos, VariablesMap},
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
    // the vec of variables that were referenced in the actions -> terms
    // chain in the source code
    used_variables: Variables<'a>,
    // map of variable tokens -> memory positions to use when reading a
    // variable
    local_variables: VariablesMap,
    used_data_sources: DataSources<'a>,
    used_arguments: Arguments<'a>,
    cur_mir_block: MirBlock,
    cur_mem_pos: u32,
    var_read_only: bool,
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
            roto_packs.push(compile_module(
                _module,
                _global.get(&Scope::Global).unwrap(),
            ));
        }

        roto_packs
    }

    pub fn build(
        source_code: &'a str,
    ) -> Vec<Result<RotoPack, CompileError>> {
        let mut compiler = Compiler::new(source_code);
        compiler.parse_source_code(source_code).unwrap();
        compiler.eval_ast().unwrap();
        compiler.compile()
    }
}

#[derive(Debug, PartialEq)]
pub enum MirBlockType {
    // A block that contains a variable assignment in the `define` section
    Assignment,
    // One complete `term` section
    Term,
    // a single match action from the `apply` section
    MatchAction,
    // A block that should be inserted whenever the variable/data souce is
    // referenced outside of the `define` section.
    Alias,
    // Exit statements
    Terminator,
}

#[derive(Debug)]
pub struct MirBlock {
    pub command_stack: Vec<Command>,
    pub ty: MirBlockType,
}

impl MirBlock {
    pub fn new(ty: MirBlockType) -> Self {
        MirBlock {
            command_stack: Vec::new(),
            ty,
        }
    }

    pub fn push_command(&mut self, command: Command) {
        self.command_stack.push(command);
    }

    pub fn last(&self) -> &Command {
        self.command_stack.last().unwrap()
    }

    pub fn pop_last(&mut self) -> Command {
        self.command_stack.pop().unwrap()
    }

    // Post-process this block to filter out any PushStack and StackOffset
    // commands beyond the last *MethodCall command and to change the memory
    // position the result of the last *MethodCall command into the position
    // that was passed in as an argument.
    //
    // This is used by a block that computes a variable and needs to store it
    // in a memory position. Only newly created values can be stored in a
    // memory postion, and those can only be the result of a method. Field
    // indexes do *not* create new values. This, we store only the result of
    // the last MethodCall command and we the consumer of `into_assign_block`
    // will have to keep a map of field indexes for each variable to insert
    // those when reading a variable.
    //
    // Returns the changed block and the memory position a reader of this
    // variable should use. The memory position will be changed if there are
    // no *MethodCall commands in the command stack, meaning the variable
    // is an alias to a field on some access receiver.
    pub fn into_assign_block(
        mut self,
        var_mem_pos: usize,
    ) -> (Self, usize, Vec<usize>) {
        let mut field_indexes = vec![];

        self.command_stack.reverse();

        let mut method_encountered = false;
        let mut mem_pos = var_mem_pos;

        let mut c_stack = self
            .command_stack
            .into_iter()
            .filter_map(|mut c| match c.op {
                OpCode::PushStack if !method_encountered => {
                    mem_pos = c.args.first().unwrap().clone().into();
                    None
                }
                OpCode::StackOffset if !method_encountered => {
                    field_indexes.push(c.args[0].clone().into());
                    None
                }
                OpCode::ExecuteValueMethod
                | OpCode::ExecuteDataStoreMethod
                | OpCode::ExecuteConsumeValueMethod
                | OpCode::ExecuteTypeMethod => {
                    // rewrite the memory position if this is the last
                    // MethodCall we've encountered.
                    if !method_encountered {
                        let last_i = c.args.len() - 1;
                        c.args[last_i] = Arg::MemPos(var_mem_pos as u32);
                    }

                    method_encountered = true;
                    Some(c)
                }
                _ => Some(c),
            })
            .collect::<Vec<_>>();

        // No MethodCall encountered? Then it's an alias
        mem_pos = if !method_encountered {
            mem_pos
        } else {
            var_mem_pos
        };

        c_stack.reverse();

        self.command_stack = c_stack;

        (self, mem_pos, field_indexes)
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

// Cloning a MirBlock only works if a Constant argument contains a TypeValue
// that can be converted into a BuiltinTypeValue, meaning that the constant
// doesn't hold a Record or a Value.
impl Clone for MirBlock {
    fn clone(&self) -> Self {
        let c_stack = self
            .command_stack
            .iter()
            .map(|c| {
                let args = c.args.to_vec();
                Command::new(c.op, args)
            })
            .collect::<Vec<_>>();

        MirBlock {
            ty: MirBlockType::Alias,
            command_stack: c_stack,
        }
    }
}

fn compile_module(
    module: &SymbolTable,
    global_table: &SymbolTable,
) -> Result<RotoPack, CompileError> {
    println!("SYMBOL MAP\n{:#?}", module);

    let (
        _rx_type,
        _tx_type,
        DepsGraph {
            used_arguments,
            used_variables,
            used_data_sources,
        },
    ) = module.create_deps_graph()?;

    let mut state = CompilerState {
        cur_module: module,
        local_variables: VariablesMap::default(),
        used_variables,
        used_data_sources,
        used_arguments,
        computed_terms: Terms::new(),
        cur_mir_block: MirBlock::new(MirBlockType::Assignment),
        var_read_only: false,
        cur_mem_pos: 0,
    };

    // initialize the command stack
    let mut mir: Vec<MirBlock> = vec![];

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

    (mir, state) = compile_apply(mir, state)?;

    state.cur_mir_block = MirBlock::new(MirBlockType::Terminator);
    state.cur_mir_block.command_stack.push(Command::new(
        OpCode::Exit(state.cur_module.get_default_action()),
        vec![],
    ));

    mir.push(state.cur_mir_block);

    println!("\n");
    for m in &mir {
        println!("MIR_block ({:?}): \n{}", m.ty, m);
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

    // Lookup all the data sources in the global symbol table. The module
    // table does not have (the right) typedefs for data sources.
    let data_sources = state
        .used_data_sources
        .iter()
        .map(|ds| {
            let name = ds.1.get_name();
            let resolved_ds =
                global_table.get_data_source(&name).unwrap_or_else(|_| {
                    panic!("Fatal: Cannot find Token for data source.");
                });
            ExtDataSource::new(&name, resolved_ds.1, resolved_ds.0)
        })
        .collect::<Vec<_>>();

    Ok(RotoPack::new(
        mir,
        TypeDef::Unknown,
        None,
        args,
        data_sources,
    ))
}

fn compile_compute_expr<'a>(
    symbol: &'a Symbol,
    mut state: CompilerState<'a>,
    // the token of the parent (the holder of the `args` field), 
    // needed to retrieve methods from.
    mut parent_token: Option<Token>,
) -> Result<CompilerState<'a>, CompileError> {
    // Compute expression trees always should have the form:
    //
    // AccessReceiver (args) -> (MethodCall | FieldAccess)*
    //
    // so always starting with an AccessReceiver. Furthermore, a variable
    // reference or assignment should always be an AccessReceiver.
    let is_ar = symbol.get_kind() == SymbolKind::AccessReceiver;
    let token = symbol.get_token()?;
    let kind = symbol.get_kind();

    match token {
        // ACCESS RECEIVERS

        // Assign a variable
        Token::Variable(var_to) => {
            assert!(is_ar);

            let var_ref =
                state.local_variables.get_by_token_value(var_to).unwrap();

            // when writing, push the content of the referenced variable to the stack,
            // note this might be the same as the assigned mem position, but it may also
            // be different!
            state.cur_mir_block.command_stack.push(Command::new(
                OpCode::PushStack,
                vec![Arg::MemPos(var_ref.mem_pos)],
            ));

            for field_index in &var_ref.field_index {
                state.cur_mir_block.command_stack.push(Command::new(
                    OpCode::StackOffset,
                    vec![Arg::FieldAccess(*field_index)],
                ));
            }
        }
        // a user-defined argument (module or term)
        Token::Argument(arg_to) => {
            assert!(is_ar);

            state.cur_mir_block.command_stack.push(Command::new(
                OpCode::ArgToMemPos,
                vec![Arg::Argument(arg_to), Arg::MemPos(state.cur_mem_pos)],
            ));
        }
        // rx instance reference
        Token::RxType => {
            assert!(is_ar);

            state
                .cur_mir_block
                .command_stack
                .push(Command::new(OpCode::PushStack, vec![Arg::MemPos(0)]));
        }
        // tx instance reference
        Token::TxType => {
            assert!(is_ar);

            state
                .cur_mir_block
                .command_stack
                .push(Command::new(OpCode::PushStack, vec![Arg::MemPos(1)]));
        }
        // a constant value (not a reference!)
        Token::Constant(_) => {

            let val = symbol.get_value();

            state.cur_mir_block.command_stack.push(Command::new(
                OpCode::MemPosSet,
                vec![
                    Arg::MemPos(state.cur_mem_pos),
                    Arg::Constant(val.as_cloned_builtin()?),
                ],
            ));
            state.cur_mir_block.command_stack.push(Command::new(
                OpCode::PushStack,
                vec![Arg::MemPos(state.cur_mem_pos)],
            ));
        }
        // Data sources
        Token::Table(_) | Token::Rib(_) => {
            assert!(is_ar);
        }
        Token::BuiltinType(_) => {
        }

        // ARGUMENTS ON ACCESS RECEIVERS

        // Non-builtin methods can't be access receivers
        Token::Method(m_to) if !is_ar => {
            // First retrieve all arguments and the recursively compile 
            // them. The result of each of them will end up on the stack 
            // (when executed in the vm).
            //
            // The arguments will start out as a fresh recursion, that is to
            // say, without a parent. After the first argument we're passing
            // in the its token to its argument sibling.
            //
            // (parent, argument)  : (None, arg1), (Token(arg1), arg2) ->
            // (token(arg2), arg3), etc.
            let mut arg_parent_token = None;
            for arg in symbol.get_args() {
                state = compile_compute_expr(
                    arg,
                    state,
                    arg_parent_token,
                )?;
                arg_parent_token = arg.get_token().ok();
            }

            // This symbol is a method, but what is the parent?
            match parent_token.unwrap() {
                // The parent is a table, so this symbol is a method on a
                // table.
                Token::Table(t_to) => {
                    state.cur_mir_block.command_stack.push(Command::new(
                        OpCode::ExecuteDataStoreMethod,
                        vec![
                            Arg::DataSourceTable(t_to),
                            Arg::Method(m_to),
                            Arg::Arguments(
                                symbol
                                    .get_args()
                                    .iter()
                                    .map(|s| s.get_type())
                                    .collect::<Vec<_>>(),
                            ), // argument types and number
                            Arg::MemPos(state.cur_mem_pos),
                        ],
                    ));
                }
                // The parent is a RIB, so this symbol is a method on a rib
                Token::Rib(r_to) => {
                    state.cur_mir_block.command_stack.push(Command::new(
                        OpCode::ExecuteDataStoreMethod,
                        vec![
                            Arg::DataSourceRib(r_to),
                            Arg::Method(m_to),
                            Arg::Arguments(
                                symbol
                                    .get_args()
                                    .iter()
                                    .map(|s| s.get_type())
                                    .collect::<Vec<_>>(),
                            ), // argument types and number
                            Arg::MemPos(state.cur_mem_pos),
                        ],
                    ));
                }
                // The parent is a built-tin method, so this symbol is a 
                // method on a built-in method.
                Token::BuiltinType(_b_to) => {
                    state.cur_mir_block.command_stack.push(Command::new(
                        OpCode::ExecuteTypeMethod,
                        vec![
                            Arg::Type(symbol.get_type()), // return type
                            Arg::Method(token.into()),    // method token
                            Arg::Arguments(
                                symbol
                                    .get_args()
                                    .iter()
                                    .map(|s| s.get_type())
                                    .collect::<Vec<_>>(),
                            ), // argument types and number
                            Arg::MemPos(state.cur_mem_pos),
                        ],
                    ));
                }
                // The parent is a Field Access, this symbol is one of:
                //
                // a Field Access, e.g. `my_field.method()`
                // a Method on a method, `my_method().my_method2()`
                // a method a user-defined var, or argument, or rxtype, 
                // or txtype, e.g. `my_var.method()`
                // a method on a constant, e.g. `24.to_prefix_length()`
                Token::FieldAccess(_) | Token::Method(_) | Token::Variable(_) |
                Token::Argument(_) | Token::RxType | Token::TxType | 
                Token::Constant(_) => {
                    match kind {
                        SymbolKind::MethodCallbyRef => {
                            // args: [ method_call, type, arguments, 
                            //         return_type ]
                            state.cur_mir_block.command_stack.push(
                                Command::new(
                                    OpCode::ExecuteValueMethod,
                                    vec![
                                        Arg::Method(token.into()),
                                        Arg::Type(symbol.get_type()),
                                        Arg::Arguments(
                                            symbol
                                                .get_args()
                                                .iter()
                                                .map(|s| s.get_type())
                                                .collect::<Vec<_>>(),
                                        ),
                                        // argument types and number
                                        Arg::MemPos(state.cur_mem_pos),
                                    ],
                                ),
                            );
                        }
                        SymbolKind::MethodCallByConsumedValue => {
                            // args: [ method_call, type, arguments,
                            //         return_type ]
                            state.cur_mir_block.command_stack.push(
                                Command::new(
                                    OpCode::ExecuteConsumeValueMethod,
                                    vec![
                                        Arg::Method(token.into()),
                                        Arg::Type(symbol.get_type()),
                                        Arg::Arguments(
                                            symbol
                                                .get_args()
                                                .iter()
                                                .map(|s| s.get_type())
                                                .collect::<Vec<_>>(),
                                        ),
                                        // argument types and number
                                        Arg::MemPos(state.cur_mem_pos),
                                    ],
                                ),
                            );
                        }
                        _ => {
                            panic!("PANIC!");
                        }
                    };
                },
                Token::Term(parent_to) |
                Token::Action(parent_to) |
                Token::MatchAction(parent_to) => {
                    return Err(CompileError::new(format!(
                        "Invalid data source: {:?} {:?}",
                        token, parent_to
                    ))); 
                }
            };

            // Push the result to the stack for an (optional) next Accessor
            // to be used.
            state.cur_mir_block.command_stack.push(Command::new(
                OpCode::PushStack,
                vec![Arg::MemPos(state.cur_mem_pos)],
            ));

            state.cur_mem_pos += 1;

            // Since we already have compiled in the arguments of this symbol
            // we will return here, to avoind doing it again.
            return Ok(state);
        }
        // built-in methods
        Token::Method(_m) => {
            assert!(is_ar);
        }
        Token::FieldAccess(ref fa) => {
            assert!(!is_ar);

            let mut args = vec![];
            args.extend(
                fa.iter()
                    .map(|t| Arg::FieldAccess(*t as usize))
                    .collect::<Vec<_>>(),
            );

            state
                .cur_mir_block
                .command_stack
                .push(Command::new(OpCode::StackOffset, args));
        }
        Token::Term(to) | Token::Action(to) | Token::MatchAction(to) => {
            return Err(CompileError::new(format!(
                "Invalid Token encountered in compute expression: {}",
                to
            )));
        }
    };

    // Arguments

    // The arguments are recursively compiled, similar (but not the same!) as
    // the argument compilation for methods. If the token of the current
    // symbol was Method(_) then the Token::Method match pattern above
    // already compiled the arguments and this section will be skipped.
    //
    // The argument compilation will *not* start with a fresh recursion.
    // Instead it will start with the token of the access receiver
    // and that will be passed as the parent token of the first recursion.
    // After that the parent token will be the token of the predecessing
    // sibling of the current symbol:
    //
    // (parent, argument)  : (parent token, arg1), (Token(arg1), arg2) ->
    // (token(arg2), arg3), etc.


    parent_token = if parent_token.is_some() { parent_token } else { symbol.get_token().ok() };
    for arg in symbol.get_args() {
        state = compile_compute_expr(arg, state, parent_token)?;
        parent_token = arg.get_token().ok();
    }

    Ok(state)
}

fn compile_assignments(
    mut mir: Vec<MirBlock>,
    mut state: CompilerState<'_>,
) -> Result<(Vec<MirBlock>, CompilerState<'_>), CompileError> {
    let _module = state.cur_module;

    // a new block
    state.cur_mir_block = MirBlock::new(MirBlockType::Assignment);

    // reset the alias detector.
    // state.cur_mir_block_has_alias = true;

    // compile the used variable assignments. Since the rx and tx value live
    // in memory positions 0 and 1, we start with memory position 2 plus the
    // number of variables that we need to resolve.
    for (var_mem_pos, var) in state.used_variables.clone().iter().enumerate()
    {
        // set the mem_pos in state to the counter we use here. Recursive
        // `compile_expr` may increase state.mem_pos to temporarily store
        // argument variables.
        // state.cur_mem_pos = mem_pos;
        state.cur_mem_pos = 2 + state.used_variables.len() as u32;
        println!(
            "VAR {:?} MEM POS {} TEMP POS START {}",
            var.0,
            var_mem_pos + 2,
            state.cur_mem_pos
        );

        let s = _module.get_variable_by_token(&var.1.get_token()?);

        state.cur_mir_block.command_stack.push(Command::new(
            OpCode::Label,
            vec![Arg::Label(format!("VAR {}", var.0).as_str().into())],
        ));

        state =
            compile_compute_expr(s.get_args().get(0).unwrap(), state, None)?;

        state
            .cur_mir_block
            .command_stack
            .push(Command::new(OpCode::ClearStack, vec![]));

        let (cur_mir_block, cur_mem_pos, field_indexes) =
            state.cur_mir_block.into_assign_block(var_mem_pos + 2);

        state.local_variables.set(
            var.1.get_token()?.into(),
            cur_mem_pos as u32,
            field_indexes,
        )?;

        mir.push(cur_mir_block);
        state.cur_mir_block = MirBlock::new(MirBlockType::Assignment);
    }

    state.cur_mem_pos = 2 + state.used_variables.len() as u32;
    println!("local variables map");
    println!("{:#?}", state.local_variables);

    Ok((mir, state))
}

fn compile_apply(
    mut mir: Vec<MirBlock>,
    mut state: CompilerState<'_>,
) -> Result<(Vec<MirBlock>, CompilerState), CompileError> {
    state.cur_mir_block = MirBlock::new(MirBlockType::MatchAction);

    let match_actions = state.cur_module.get_match_actions();

    // Collect the terms that we need to compile.
    for match_action in match_actions {
        let term_name = match_action.get_name();

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
                    .push((term_name.clone(), state.cur_mem_pos.into()));
            }
        };

        state.cur_mem_pos += 1;

        // move the current mir block to the end of all the collected MIR.
        mir.push(state.cur_mir_block);

        // continue with a fresh block
        state.cur_mir_block = MirBlock::new(MirBlockType::MatchAction);

        if let SymbolKind::MatchAction(ma) = match_action.get_kind() {
            match ma {
                MatchActionType::MatchAction => {
                    state.cur_mir_block.command_stack.extend([
                        Command::new(
                            OpCode::Label,
                            vec![Arg::Label(
                                format!(
                                    "MATCH ACTION {}-{:?}",
                                    term_name.clone(),
                                    match_action.get_kind()
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
                                    match_action.get_kind()
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
        for action in match_action.get_args() {
            let _module = state.cur_module;
            let action_name = action.get_name();
            let accept_reject = if let TypeDef::AcceptReject(accept_reject) =
                action.get_type()
            {
                accept_reject
            } else {
                panic!("NO ACCEPT REJECT {:?}", action);
            };
            let actions = _module.get_actions();
            let action = actions
                .iter()
                .find(|t| t.get_name() == action_name)
                .unwrap();

            state = compile_action(action, state)?;

            // Add an early return if the type of the match action is either `Reject` or
            // `Accept`.
            if accept_reject != AcceptReject::NoReturn {
                state
                    .cur_mir_block
                    .command_stack
                    .push(Command::new(OpCode::Exit(accept_reject), vec![]))
            }
        }

        // move the current mir block to the end of all the collected MIR.
        mir.push(state.cur_mir_block);

        // continue with a fresh block
        state.cur_mir_block = MirBlock::new(MirBlockType::MatchAction);
    }

    Ok((mir, state))
}

fn compile_action<'a>(
    action: Action<'a>,
    mut state: CompilerState<'a>,
) -> Result<CompilerState<'a>, CompileError> {
    for sub_action in action.get_args() {
        state = compile_sub_action(sub_action, state)?;
    }

    Ok(state)
}

fn compile_sub_action<'a>(
    sub_action: Action<'a>,
    mut state: CompilerState<'a>,
) -> Result<CompilerState<'a>, CompileError> {
    match sub_action.get_kind() {
        // A symbol with an RxType token, should be an access receiver.
        SymbolKind::AccessReceiver => {
            state = compile_compute_expr(sub_action, state, None)?;
            state.cur_mem_pos += 1;
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
    // do not create assignment block, only produce code to read variables
    // when referenced.
    state.var_read_only = true;
    state.cur_mir_block = MirBlock::new(MirBlockType::Term);

    // Set a Label so that each term block is identifiable for humans.
    state.cur_mir_block.command_stack.push(Command::new(
        OpCode::Label,
        vec![Arg::Label(
            format!("TERM {}", term.get_name()).as_str().into(),
        )],
    ));

    let sub_terms = term.get_args();
    let mut sub_terms = sub_terms.iter().peekable();

    while let Some(arg) = &mut sub_terms.next() {
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
    let saved_mem_pos = state.cur_mem_pos;
    match sub_term.get_kind() {
        SymbolKind::CompareExpr(op) => {
            let args = sub_term.get_args();
            state = compile_compute_expr(&args[0], state, None)?;
            state.cur_mem_pos += 1;
            state = compile_compute_expr(&args[1], state, None)?;

            state
                .cur_mir_block
                .command_stack
                .push(Command::new(OpCode::Cmp, vec![op.into()]));
        }
        SymbolKind::OrExpr => {
            let args = sub_term.get_args();
            state = compile_sub_term(&args[0], state)?;
            state.cur_mem_pos += 1;
            state = compile_sub_term(&args[1], state)?;

            state.cur_mir_block.command_stack.push(Command::new(
                OpCode::Cmp,
                vec![Arg::CompareOp(ast::CompareOp::Or)],
            ));
        }
        SymbolKind::AndExpr => {
            let args = sub_term.get_args();
            state = compile_sub_term(&args[0], state)?;
            state.cur_mem_pos += 1;
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
            // let start_pos = state.cur_mem_pos;
            state = compile_compute_expr(sub_term, state, None)?;
        }
    };

    // restore old mem_pos, let's not waste memory
    state.cur_mem_pos = saved_mem_pos;

    Ok(state)
}
