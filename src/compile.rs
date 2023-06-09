use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
    sync::Arc,
};

use log::{log_enabled, trace, Level};
use nom::error::VerboseError;

use crate::{
    ast::{self, AcceptReject, ShortString, SyntaxTree},
    symbols::{
        DepsGraph, GlobalSymbolTable, MatchActionType, Scope, Symbol,
        SymbolKind, SymbolTable,
    },
    traits::Token,
    types::{
        collections::{ElementTypeValue, List, Record},
        datasources::DataSource,
        typevalue::TypeValue,
    },
    types::{datasources::Table, typedef::TypeDef},
    vm::{
        Command, CommandArg, ExtDataSource, ModuleArg, ModuleArgsMap, OpCode,
        StackRefPos, VariablesMap,
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

#[derive(Debug)]
pub struct Rotolo {
    packs: Vec<RotoPack>,
    mis_compilations: Vec<(ShortString, CompileError)>,
}

impl Rotolo {
    pub fn inspect_all_arguments(
        &self,
    ) -> HashMap<ShortString, Vec<(&str, TypeDef)>> {
        let mut res = HashMap::<ShortString, Vec<(&str, TypeDef)>>::new();
        for rp in self.packs.iter() {
            res.insert(
                rp.module_name.clone(),
                rp.arguments
                    .iter()
                    .map(|a| (a.get_name(), a.get_type()))
                    .collect::<Vec<_>>(),
            );
        }

        res
    }

    pub fn is_success(&self) -> bool {
        self.mis_compilations.is_empty()
    }

    pub fn get_mis_compilations(&self) -> &Vec<(ShortString, CompileError)> {
        &self.mis_compilations
    }

    fn _take_pack_by_name(&mut self, name: &str) -> Option<RotoPack> {
        let idx = self.packs.iter().position(|p| p.module_name == name);
        if let Some(idx) = idx {
            let p = self.packs.remove(idx);
            Some(p)
        } else {
            None
        }
    }

    pub fn retrieve_public_as_arcs(
        &self,
        name: &str,
    ) -> Result<RotoPackArc, CompileError> {
        let mp = self
            .get_mis_compilations()
            .iter()
            .map(|mp| (mp.0.clone(), Err(mp.1.clone())));
        let mut p = self
            .packs
            .iter()
            .map(|p| (p.module_name.clone(), Ok(p)))
            .chain(mp);
        p.find(|p| p.0 == name)
            .ok_or_else(|| {
                CompileError::from(format!(
                    "Can't find module with specified name in this pack: {}",
                    name
                ))
            })
            .and_then(|p| {
                if let Ok(p) = p.1 {
                    Ok(PublicRotoPack::<
                        Arc<[MirBlock]>,
                        Arc<[(&str, TypeDef)]>,
                        Arc<[ExtDataSource]>,
                    > {
                        module_name: p.module_name.as_str(),
                        arguments: p
                            .arguments
                            .inspect_arguments()
                            .clone()
                            .into(),
                        rx_type: p.rx_type.clone(),
                        tx_type: p.tx_type.clone(),
                        data_sources: p.data_sources.as_slice().into(),
                        mir: p.mir.clone().into(),
                    })
                } else {
                    Err(p.1.err().unwrap())
                }
            })
    }

    pub fn retrieve_public_as_refs(
        &self,
        name: &str,
    ) -> Result<RotoPackRef, CompileError> {
        let mp = self
            .get_mis_compilations()
            .iter()
            .map(|mp| (mp.0.clone(), Err(mp.1.clone())));
        let mut p = self
            .packs
            .iter()
            .map(|p| (p.module_name.clone(), Ok(p)))
            .chain(mp);
        p.find(|p| p.0 == name)
            .ok_or_else(|| {
                CompileError::from(format!(
                    "Can't find module with specified name in this pack: {}",
                    name
                ))
            })
            .and_then(|p| {
                if let Ok(p) = p.1 {
                    Ok(PublicRotoPack {
                        module_name: p.module_name.as_str(),
                        arguments: p.arguments.inspect_arguments(),
                        rx_type: p.rx_type.clone(),
                        tx_type: p.tx_type.clone(),
                        data_sources: p.data_sources.clone(),
                        mir: p.mir.as_slice(),
                    })
                } else {
                    Err(p.1.err().unwrap())
                }
            })
    }

    pub fn compile_all_arguments(
        &self,
        mut args: HashMap<ShortString, Vec<(&str, TypeValue)>>,
    ) -> HashMap<ShortString, ModuleArgsMap> {
        let mut res = HashMap::<ShortString, ModuleArgsMap>::new();
        for pack in self.packs.iter() {
            let args =
                std::mem::take(args.get_mut(&pack.module_name).unwrap());
            let cp = pack.arguments.compile_arguments(args);
            if let Ok(map) = cp {
                res.insert(pack.module_name.clone(), map);
            }
        }

        res
    }

    pub fn compile_arguments(
        &self,
        name: &str,
        args: Vec<(&str, TypeValue)>,
    ) -> Result<ModuleArgsMap, CompileError> {
        let pack = self.packs.iter().find(|p| p.module_name == name);
        if let Some(pack) = pack {
            let cp = pack.arguments.compile_arguments(args);

            match cp {
                Ok(map) => Ok(map),
                Err(err) => Err(err),
            }
        } else {
            Err(format!("Can't find with specified module name: {}", name)
                .into())
        }
    }
}

// pub type RotoloArc = Rotolo<Arc<Vec<MirBlock>>>;
// pub type RotoloRef<'a> = Rotolo<[&'a MirBlock]>;
#[derive(Debug)]
pub struct PublicRotoPack<
    'a,
    M: AsRef<[MirBlock]>,
    A: AsRef<[(&'a str, TypeDef)]>,
    EDS: AsRef<[ExtDataSource]>,
> {
    pub module_name: &'a str,
    pub mir: M,
    pub rx_type: TypeDef,
    pub tx_type: Option<TypeDef>,
    pub arguments: A,
    pub data_sources: EDS,
}

type RotoPackArc<'a> = PublicRotoPack<
    'a,
    Arc<[MirBlock]>,
    Arc<[(&'a str, TypeDef)]>,
    Arc<[ExtDataSource]>,
>;

type RotoPackRef<'a> = PublicRotoPack<
    'a,
    &'a [MirBlock],
    Vec<(&'a str, TypeDef)>,
    Vec<ExtDataSource>,
>;

impl<
        'a,
        M: AsRef<[MirBlock]>,
        A: AsRef<[(&'a str, TypeDef)]>,
        EDS: AsRef<[ExtDataSource]>,
    > PublicRotoPack<'a, M, A, EDS>
{
    pub fn get_arguments(&'a self) -> &'a [(&str, TypeDef)] {
        self.arguments.as_ref()
    }

    pub fn get_mir(&'a self) -> &'a [MirBlock] {
        self.mir.as_ref()
    }

    pub fn get_data_sources(&'a self) -> &[ExtDataSource] {
        self.data_sources.as_ref()
    }

    pub fn set_source(
        &mut self,
        source: DataSource,
    ) -> Result<(), CompileError> {
        let name = source.get_name();

        let f_ds = self
            .data_sources
            .as_ref()
            .iter()
            .find(|ds| ds.get_name() == name);
        let s_ty = source.get_type();

        let f_ds = if let Some(ds) = f_ds {
            if ds.get_value_type() != s_ty {
                trace!("{:?} != {:?}", ds.get_value_type(), s_ty);
                return Err(CompileError::from(
                    format!(
                        "Fatal: Data source with name {} has the wrong content type, expected {}, but found {}",
                        name,
                        s_ty,
                        ds.get_value_type(),
                    )
                ));
            }
            ds
        } else {
            return Err(CompileError::from(format!(
                "Fatal: Cannot find data source with name: {} in source code",
                name
            )));
        };

        f_ds.get_source().store(match source {
            DataSource::Table(ref t) => Some(
                DataSource::Table(Table {
                    name,
                    ty: s_ty,
                    records: t.records.clone(),
                })
                .into(),
            ),
            DataSource::Rib(ref r) => Some(DataSource::Rib(r.clone()).into()),
        });

        Ok(())
    }
}

#[derive(Debug)]
struct RotoPack {
    module_name: ShortString,
    mir: Vec<MirBlock>,
    rx_type: TypeDef,
    tx_type: Option<TypeDef>,
    arguments: ModuleArgsMap,
    data_sources: Vec<ExtDataSource>,
}

impl RotoPack {
    fn new(
        module_name: ShortString,
        mir: Vec<MirBlock>,
        rx_type: TypeDef,
        tx_type: Option<TypeDef>,
        arguments: ModuleArgsMap,
        data_sources: Vec<ExtDataSource>,
    ) -> Self {
        RotoPack {
            module_name,
            mir,
            rx_type,
            tx_type,
            arguments,
            data_sources,
        }
    }

    // fn compile_arguments(
    //     &self,
    //     args: Vec<(&str, TypeValue)>,
    // ) -> Result<ModuleArgsMap, CompileError> {
    //     // Walk over all the module arguments that were supplied and see if
    //     // they match up with the ones in the source code.
    //     let mut arguments_map = ModuleArgsMap::new();
    //     let len = args.len();
    //     for supplied_arg in args {
    //         match self
    //             .arguments
    //             .iter()
    //             .find(|a| supplied_arg.0 == a.get_name())
    //         {
    //             // The argument is in the source code
    //             Some(found_arg) => {
    //                 // nice, but do the types match?
    //                 if found_arg.get_type() == supplied_arg.1 {
    //                     // yes, they match
    //                     arguments_map.insert(
    //                         found_arg.get_name(),
    //                         found_arg.get_index(),
    //                         found_arg.get_type(),
    //                         supplied_arg.1,
    //                     )
    //                 } else {
    //                     // Ok, but maybe we can convert into the type we
    //                     // need? Note that we can only try to convert if
    //                     // it's a builtin type.
    //                     match supplied_arg.1.into_builtin().and_then(|t| {
    //                         t.try_into_type(&found_arg.get_type())
    //                     }) {
    //                         Ok(arg) => arguments_map.insert(
    //                             found_arg.get_name(),
    //                             found_arg.get_index(),
    //                             found_arg.get_type(),
    //                             arg,
    //                         ),
    //                         Err(_) => {
    //                             return Err(format!("An invalid type was specified for argument: {}", supplied_arg.0).into());
    //                         }
    //                     };
    //                 }
    //             }
    //             // The supplied argument is not in the source code.
    //             None => {
    //                 return Err(format!(
    //                     "Can't find argument in source: {}",
    //                     supplied_arg.0
    //                 )
    //                 .into())
    //             }
    //         }
    //     }

    //     // See if we got all the required arguments in the source code
    //     // covered.
    //     if arguments_map.len() != len {
    //         let missing_args = self
    //             .arguments
    //             .iter()
    //             .filter(|a| {
    //                 arguments_map.get_by_token_value(a.get_index()).is_none()
    //             })
    //             .map(|a| a.get_name())
    //             .collect::<Vec<&str>>();

    //         return Err(format!(
    //             "Some arguments are missing: {:?}",
    //             missing_args
    //         )
    //         .into());
    //     }

    //     Ok(arguments_map)
    // }

    // fn inspect_arguments(&self) -> Arc<Vec<(&str, TypeDef)>> {
    //     Arc::new(
    //         self.arguments
    //             .iter()
    //             .map(|a| (a.get_name(), a.get_type()))
    //             .collect::<Vec<_>>(),
    //     )
    // }
}

// impl<'a> From<&'a RotoPack for PublicRotoPack<'a, Arc<Vec<MirBlock>>> {
//     fn from(rp: &'a RotoPack<Arc<Vec<MirBlock>>>) -> Self {
//         PublicRotoPack {
//             module_name: rp.module_name.as_str(),
//             mir: Arc::clone(&rp.mir),
//             rx_type: rp.rx_type.clone(),
//             tx_type: rp.tx_type.clone(),
//             arguments: Arc::clone(&rp.arguments.inspect_arguments()),
//             data_sources: rp.data_sources.clone(),
//         }
//     }
// }

// impl<'a> From<&'a RotoPack<&'a Vec<MirBlock>>> for PublicRotoPack<'a, &'a Vec<MirBlock>> {
//     fn from(rp: &'a RotoPack<&'a Vec<MirBlock>>) -> Self {
//         PublicRotoPack {
//             module_name: rp.module_name.as_str(),
//             mir: rp.mir,
//             rx_type: rp.rx_type.clone(),
//             tx_type: rp.tx_type.clone(),
//             arguments: Arc::clone(&rp.arguments.inspect_arguments()),
//             data_sources: rp.data_sources.clone(),
//         }
//     }
// }

#[derive(Debug, Default, Clone)]
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

impl From<CompileError> for Box<dyn std::error::Error> {
    fn from(value: CompileError) -> Self {
        value.message.into()
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
pub struct Compiler {
    pub ast: SyntaxTree,
    symbols: GlobalSymbolTable,
    // Compile time arguments
    arguments: Vec<(ShortString, Vec<(ShortString, TypeValue)>)>,
    // data_sources: Vec<(&'a str, Arc<DataSource>)>,
}

impl<'a> Compiler {
    pub fn new() -> Self {
        Compiler {
            ast: SyntaxTree::default(),
            symbols: GlobalSymbolTable::new(),
            arguments: vec![],
            // data_sources: vec![],
        }
    }

    pub fn parse_source_code(
        &mut self,
        source_code: &'a str,
    ) -> Result<(), VerboseError<&'a str>> {
        self.ast = SyntaxTree::parse_str(source_code)?.1;

        Ok(())
    }

    pub fn eval_ast(&mut self) -> Result<(), CompileError> {
        self.ast.eval(self.symbols.clone())?;
        Ok(())
    }

    pub fn inject_compile_time_arguments(
        &mut self,
    ) -> Result<(), CompileError> {
        trace!("compile time arguments: {:?}", self.arguments);
        let mut module = self.symbols.borrow_mut();
        for (module_name, args) in self.arguments.iter() {
            let _module = module
                .get_mut(&crate::symbols::Scope::Module(module_name.clone()))
                .ok_or_else(|| {
                    CompileError::from(format!(
                        "Cannot find module with name '{}'",
                        module_name
                    ))
                })?;
            for arg in args {
                let mut _arg =
                    _module.get_argument_mut(&arg.0.clone()).map_err(|_| CompileError::from(
                        format!(
                            "Cannot find argument with name '{}' for module '{}'",
                            arg.0,
                            module_name
                        )
                    ))?;

                if _arg.get_type() == arg.1 {
                    _arg.set_value(arg.1.clone());
                } else {
                    return Err(
                        CompileError::from(
                            format!(
                                "Argument '{}' has the wrong type, expected '{}', got '{}'",
                                arg.0,
                                _arg.get_type(),
                                TypeDef::from(&arg.1),
                            )
                        )
                    );
                }
            }
        }

        Ok(())
    }

    pub fn compile(self) -> Rotolo {
        trace!("Start compiling...");

        // get all symbols that are used in the filter terms.
        let mut _global = self.symbols.borrow_mut();
        let (_global_mod, modules): (Vec<Scope>, Vec<Scope>) =
            _global.keys().cloned().partition(|m| *m == Scope::Global);

        drop(_global);
        let mut _global = self.symbols.borrow_mut();

        // each module outputs one roto-pack with its own MIR (composed of MIR blocks),
        // its used arguments and used data sources.
        let mut packs = vec![];
        let mut miscompilations = vec![];

        for module in modules {
            let _module = _global.get(&module).unwrap();
            let compile_result = compile_module(
                _module,
                _global.get(&Scope::Global).unwrap(),
                // self.data_sources
                //     .iter()
                //     .map(|ds| (ds.0, Arc::clone(&ds.1)))
                //     .collect::<Vec<_>>(),
            );

            match compile_result {
                Ok(pack) => {
                    packs.push(pack);
                }
                Err(err) => {
                    miscompilations.push((module.get_name(), err));
                }
            }
        }

        drop(_global);

        Rotolo {
            packs,
            mis_compilations: miscompilations,
        }
    }

    pub fn with_arguments(
        &mut self,
        module_name: &str,
        args: Vec<(&str, TypeValue)>,
    ) -> Result<(), CompileError> {
        self.arguments.push((
            module_name.into(),
            args.into_iter().map(|a| (a.0.into(), a.1)).collect(),
        ));
        // let mut module = self.symbols.borrow_mut();

        // for arg in args.into_iter() {
        //     let _module = module
        //         .get_mut(&crate::symbols::Scope::Module(module_name.into()))
        //         .ok_or_else(|| CompileError::from(format!("Cannot find module with name '{}'", module_name)))?;

        //     let mut _arg = _module.get_argument_mut(&arg.0.into()).unwrap();

        //     _arg.set_value(arg.1);
        // }
        Ok(())
    }

    pub fn build(source_code: &'a str) -> Result<Rotolo, String> {
        let mut compiler: Compiler = Compiler::new();
        compiler
            .parse_source_code(source_code)
            .map_err(|err| format!("Parse error: {err}"))?;
        compiler
            .eval_ast()
            .map_err(|err| format!("Eval error: {err}"))?;
        compiler
            .inject_compile_time_arguments()
            .map_err(|op| format!("Argument error: {}", op))?;
        Ok(compiler.compile())
    }

    pub fn build_from_compiler(
        mut self,
        source_code: &'a str,
    ) -> Result<Rotolo, String> {
        self.parse_source_code(source_code)
            .map_err(|err| format!("Parse error: {err}"))?;
        self.eval_ast()
            .map_err(|err| format!("Eval error: {err}"))?;
        self.inject_compile_time_arguments()
            .map_err(|op| format!("Argument error: {}", op))?;
        Ok(self.compile())
    }
}

//------------ MirBlock & Mir -----------------------------------------------

// #[derive(Debug)]
// pub struct MirRef<MB: AsRef<Vec<MirBlock>>>(pub MB);

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
    // position to the result of the last *MethodCall command into the
    // position that was passed in as an argument.
    //
    // This is used by a block that computes a variable and needs to store it
    // in a memory position. Only newly created values can be stored in a
    // memory position, and those can only be the result of a method. Field
    // indexes do *not* create new values. Thus, we store only the result of
    // the last MethodCall command and the consumer of `into_assign_block`
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
                    mem_pos = c.args.first().unwrap().into();
                    None
                }
                OpCode::StackOffset if !method_encountered => {
                    field_indexes.push((&c.args[0]).into());
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
                        c.args[last_i] =
                            CommandArg::MemPos(var_mem_pos as u32);
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
    // data_sources: Vec<(&str, Arc<DataSource>)>,
) -> Result<RotoPack, CompileError> {
    trace!("SYMBOL MAP\n{:#?}", module);

    let DepsGraph {
        rx_type,
        tx_type,
        used_arguments,
        used_variables,
        used_data_sources,
    } = module.create_deps_graph()?;

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
    let mut mir = vec![];
    if log_enabled!(Level::Trace) {
        trace!("___used args");
        state.used_arguments.iter().for_each(|s| {
            trace!("{:?}: {:?}", s.1.get_token().unwrap(), s.0)
        });

        trace!("___used vars");

        state.used_variables.iter().for_each(|s| {
            trace!("{:?} {:?}", s.1.get_token().unwrap(), s.0);
        });

        trace!("___used data_sources");

        state.used_data_sources.iter().for_each(|t| {
            trace!("{:?} {:?}", t.1.get_token().unwrap(), t.0);
        });

        trace!("___rx tx types");
        trace!("Rx {:?}", rx_type);
        trace!("Tx {:?}", tx_type);

        trace!("=================================================");
    }

    // compile the variables used in the terms
    (mir, state) = compile_assignments(mir, state)?;

    (mir, state) = compile_apply(mir, state)?;

    state.cur_mir_block = MirBlock::new(MirBlockType::Terminator);
    state.cur_mir_block.command_stack.push(Command::new(
        OpCode::Exit(state.cur_module.get_default_action()),
        vec![],
    ));

    mir.push(state.cur_mir_block);

    trace!("\n");
    let args = state
        .used_arguments
        .iter_mut()
        .map(|a| {
            ModuleArg::new(
                &a.1.get_name(),
                a.1.get_token()
                    .unwrap_or_else(|_| {
                        panic!("Fatal: Cannot find Token for Argument.");
                    })
                    .into(),
                a.1.get_type(),
                TypeValue::UnInit,
            )
        })
        .collect::<Vec<_>>()
        .into();

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

            // W're only creating a data source with the name and token found
            // in the declaration in the source code. he actual source data
            // will only be needed at run-time
            ExtDataSource::new(&name, resolved_ds.1, resolved_ds.0)
        })
        .collect::<Vec<_>>();

    Ok(RotoPack::new(
        module.get_name(),
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
    inc_mem_pos: bool,
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
                vec![CommandArg::MemPos(var_ref.mem_pos)],
            ));

            for field_index in &var_ref.field_index {
                state.cur_mir_block.command_stack.push(Command::new(
                    OpCode::StackOffset,
                    vec![CommandArg::FieldAccess(*field_index)],
                ));
            }

            if inc_mem_pos {
                state.cur_mem_pos += 1;
            }
        }
        // a user-defined argument (module or term)
        Token::Argument(arg_to) => {
            assert!(is_ar);

            // if the Argument has a value, then it was set by the argument
            // injection after eval(), that means that we can just store
            // the (literal) value in the mem pos
            if let Some(arg) = state.used_arguments.iter().find(|a| {
                a.1.get_token().unwrap() == Token::Argument(arg_to)
                    && a.1.has_value()
            }) {
                state.cur_mir_block.command_stack.push(Command::new(
                    OpCode::MemPosSet,
                    vec![
                        CommandArg::MemPos(state.cur_mem_pos),
                        CommandArg::Constant(arg.1.get_value().clone()),
                    ],
                ));
            } else {
                state.cur_mir_block.command_stack.push(Command::new(
                    OpCode::ArgToMemPos,
                    vec![
                        CommandArg::Argument(arg_to),
                        CommandArg::MemPos(state.cur_mem_pos),
                    ],
                ));
            }

            state.cur_mir_block.command_stack.push(Command::new(
                OpCode::PushStack,
                vec![CommandArg::MemPos(state.cur_mem_pos)],
            ));

            if inc_mem_pos {
                state.cur_mem_pos += 1;
            }
        }
        // rx instance reference
        Token::RxType => {
            assert!(is_ar);

            state.cur_mir_block.command_stack.push(Command::new(
                OpCode::PushStack,
                vec![CommandArg::MemPos(0)],
            ));
        }
        // tx instance reference
        Token::TxType => {
            assert!(is_ar);

            state.cur_mir_block.command_stack.push(Command::new(
                OpCode::PushStack,
                vec![CommandArg::MemPos(1)],
            ));
        }
        // a constant value (not a reference!)
        Token::Constant(_) => {
            let val = symbol.get_value();

            state.cur_mir_block.command_stack.push(Command::new(
                OpCode::MemPosSet,
                vec![
                    CommandArg::MemPos(state.cur_mem_pos),
                    CommandArg::Constant(val.builtin_as_cloned_type_value()?),
                ],
            ));

            state.cur_mir_block.command_stack.push(Command::new(
                OpCode::PushStack,
                vec![CommandArg::MemPos(state.cur_mem_pos)],
            ));

            if inc_mem_pos {
                state.cur_mem_pos += 1;
            }
        }
        // Data sources
        Token::Table(_) | Token::Rib(_) | Token::OutputStream(_) => {
            assert!(is_ar);
        }
        // The AccessReceiver is a Built-in Type
        Token::BuiltinType(_b_to) => {
            assert!(is_ar);
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
                    inc_mem_pos,
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
                            CommandArg::DataSourceTable(t_to),
                            CommandArg::Method(m_to),
                            CommandArg::Arguments(
                                symbol
                                    .get_args()
                                    .iter()
                                    .map(|s| s.get_type())
                                    .collect::<Vec<_>>(),
                            ), // argument types and number
                            CommandArg::MemPos(state.cur_mem_pos),
                        ],
                    ));
                }
                // The parent is a RIB, so this symbol is a method on a rib
                Token::Rib(r_to) => {
                    state.cur_mir_block.command_stack.push(Command::new(
                        OpCode::ExecuteDataStoreMethod,
                        vec![
                            CommandArg::DataSourceRib(r_to),
                            CommandArg::Method(m_to),
                            CommandArg::Arguments(
                                symbol
                                    .get_args()
                                    .iter()
                                    .map(|s| s.get_type())
                                    .collect::<Vec<_>>(),
                            ), // argument types and number
                            CommandArg::MemPos(state.cur_mem_pos),
                        ],
                    ));
                }
                // The parent is a OutputStream, so this symbol is a
                // method on the OutputStream type
                Token::OutputStream(o_s) => {
                    state.cur_mir_block.command_stack.push(Command::new(
                        OpCode::ExecuteDataStoreMethod,
                        vec![
                            CommandArg::OutputStream(o_s),
                            CommandArg::Method(o_s),
                            CommandArg::Arguments(
                                symbol
                                    .get_args()
                                    .iter()
                                    .map(|s| s.get_type())
                                    .collect::<Vec<_>>(),
                            ), // argument types and number
                            CommandArg::MemPos(state.cur_mem_pos),
                        ],
                    ));
                }
                // The parent is a built-in method, so this symbol is a
                // method on a built-in method.
                Token::BuiltinType(_b_to) => {
                    state.cur_mir_block.command_stack.push(Command::new(
                        OpCode::ExecuteTypeMethod,
                        vec![
                            CommandArg::Type(symbol.get_type()), // return type
                            CommandArg::Method(token.into()), // method token
                            CommandArg::Arguments(
                                symbol
                                    .get_args()
                                    .iter()
                                    .map(|s| s.get_type())
                                    .collect::<Vec<_>>(),
                            ), // argument types and number
                            CommandArg::MemPos(state.cur_mem_pos),
                        ],
                    ));
                }
                // The parent is a List
                Token::List => {
                    trace!("LIST PARENT ARGS {:#?}", symbol.get_args());
                }
                // The parent is a Record
                Token::AnonymousRecord => {
                    trace!("ANONYMOUS RECORD PARENT ARGS {:#?}", symbol.get_args());
                }
                Token::TypedRecord => {
                    trace!("TYPED RECORD PARENT ARGS {:#?}", symbol.get_args());
                }
                // The parent is a Field Access, this symbol is one of:
                //
                // a Field Access, e.g. `my_field.method()`
                // a Method on a method, `my_method().my_method2()`
                // a method a user-defined var, or argument, or rxtype,
                // or txtype, e.g. `my_var.method()`
                // a method on a constant, e.g. `24.to_prefix_length()`
                Token::FieldAccess(_)
                | Token::Method(_)
                | Token::Variable(_)
                | Token::Argument(_)
                | Token::RxType
                | Token::TxType
                | Token::Constant(_) => {
                    match kind {
                        SymbolKind::MethodCallbyRef => {
                            // args: [ method_call, type, arguments,
                            //         return_type ]
                            state.cur_mir_block.command_stack.push(
                                Command::new(
                                    OpCode::ExecuteValueMethod,
                                    vec![
                                        CommandArg::Method(token.into()),
                                        CommandArg::Type(symbol.get_type()),
                                        CommandArg::Arguments(
                                            symbol
                                                .get_args()
                                                .iter()
                                                .map(|s| s.get_type())
                                                .collect::<Vec<_>>(),
                                        ),
                                        // argument types and number
                                        CommandArg::MemPos(state.cur_mem_pos),
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
                                        CommandArg::Method(token.into()),
                                        CommandArg::Type(symbol.get_type()),
                                        CommandArg::Arguments(
                                            symbol
                                                .get_args()
                                                .iter()
                                                .map(|s| s.get_type())
                                                .collect::<Vec<_>>(),
                                        ),
                                        // argument types and number
                                        CommandArg::MemPos(state.cur_mem_pos),
                                    ],
                                ),
                            );
                        }
                        _ => {
                            panic!("PANIC!");
                        }
                    };
                }
                Token::Term(parent_to)
                | Token::Action(parent_to)
                | Token::MatchAction(parent_to) => {
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
                vec![CommandArg::MemPos(state.cur_mem_pos)],
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
            trace!("FieldAccess {:?}", fa);

            let mut args = vec![];
            args.extend(
                fa.iter()
                    .map(|t| CommandArg::FieldAccess(*t as usize))
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
        // This record is defined without a type and used direcly, mainly in
        // as a n argument for a method. The inferred type is unambiguous.
        Token::AnonymousRecord => {
            assert!(!is_ar);

            trace!("ANONYMOUS RECORD FIELDS {:#?}", symbol.get_args());
            for arg in symbol.get_args() {
                state = compile_compute_expr(arg, state, None, true)?;
            }
            return Ok(state);
        }
        // This is a record that appears in a variable assigment that creates
        // a record in the `Define` section.
        Token::TypedRecord => {
            assert!(!is_ar);

            trace!("TYPED RECORD FIELDS {:#?}", symbol.get_args());
            trace!("Checked Type {:#?}", symbol.get_type());
   
            let values = symbol
                .get_args()
                .iter()
                .map(|v| (v.get_name(), v.get_value().clone().into()))
                .collect::<Vec<(ShortString, ElementTypeValue)>>();

            // let value_type_def = TypeDef::Record(symbol.get_args().iter().map(|v| (v.get_name(), Box::new(v.get_type()))).collect::<Vec<_>>());
            let value_type = Record(values);

            trace!("Actual type from values {:#?}", value_type);

            if symbol.get_type() != TypeValue::Record(value_type.clone()) {
                return Err(CompileError::from(
                    format!(
                        "This record: {} is of type {}, but we got a record with type {}. It's not the same and cannot convert.",
                        value_type,
                        symbol.get_type(),
                        TypeDef::Record(symbol.get_args().iter().map(|v| (v.get_name(), Box::new(v.get_type()))).collect::<Vec<_>>())
                    )
                ));
            }
            state.cur_mir_block.command_stack.push(Command::new(
                OpCode::MemPosSet,
                vec![
                    CommandArg::MemPos(state.cur_mem_pos),
                    CommandArg::Record(value_type),
                ],
            ));
            return Ok(state);
        }
        // This is used in variable assignments where a var is assigned to
        // a list. On arrival here all the elements of the defined list will
        // be in the `args` fields. We are wrapping them all up in an actual
        // `List` and storing that in *one* memory position. 
        // Anonymous Lists (lists that are defined and used immediately
        // outside of the `Define` section) don't take this code path. Those
        // appear in a ListCompareExpr and are already packed as a List.
        Token::List => {
            assert!(!is_ar);

            trace!("LIST VALUES {:?}", symbol.get_args());
            let values = symbol
                .get_args()
                .iter()
                .map(|v| v.get_value().clone().into())
                .collect::<Vec<ElementTypeValue>>();
            state.cur_mir_block.command_stack.push(Command::new(
                OpCode::MemPosSet,
                vec![
                    CommandArg::MemPos(state.cur_mem_pos),
                    CommandArg::List(List(values)),
                ],
            ));

            return Ok(state);
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

    parent_token = if parent_token.is_some() {
        parent_token
    } else {
        symbol.get_token().ok()
    };
    for arg in symbol.get_args() {
        state = compile_compute_expr(arg, state, parent_token, inc_mem_pos)?;
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
        state.cur_mem_pos = 1 + state.used_variables.len() as u32;
        trace!(
            "VAR {:?} MEM POS {} TEMP POS START {}",
            var.0,
            var_mem_pos + 2,
            state.cur_mem_pos
        );

        let s = _module.get_variable_by_token(&var.1.get_token()?);

        state.cur_mir_block.command_stack.push(Command::new(
            OpCode::Label,
            vec![CommandArg::Label(format!("VAR {}", var.0).as_str().into())],
        ));

        state = compile_compute_expr(
            s.get_args().get(0).unwrap(),
            state,
            None,
            false,
        )?;

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

    state.cur_mem_pos = 1 + state.used_variables.len() as u32;
    trace!("local variables map");
    trace!("{:#?}", state.local_variables);

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
                            vec![CommandArg::Label(
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
                            vec![CommandArg::MemPos(*mem_pos)],
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
                MatchActionType::MatchAction
                | MatchActionType::EmptyAction => {
                    state.cur_mir_block.command_stack.extend([
                        Command::new(
                            OpCode::Label,
                            vec![CommandArg::Label(
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
                            vec![CommandArg::Label(
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
            if let Some(action) =
                actions.iter().find(|t| t.get_name() == action_name)
            {
                state = compile_action(action, state)?;
            }

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
            state = compile_compute_expr(sub_action, state, None, false)?;
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
        vec![CommandArg::Label(
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
            state = compile_compute_expr(&args[0], state, None, false)?;
            state.cur_mem_pos += 1;
            state = compile_compute_expr(&args[1], state, None, false)?;

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
                vec![CommandArg::CompareOp(ast::CompareOp::Or)],
            ));
        }
        SymbolKind::AndExpr => {
            let args = sub_term.get_args();
            state = compile_sub_term(&args[0], state)?;
            state.cur_mem_pos += 1;
            state = compile_sub_term(&args[1], state)?;

            state.cur_mir_block.command_stack.push(Command::new(
                OpCode::Cmp,
                vec![CommandArg::CompareOp(ast::CompareOp::And)],
            ));
        }
        SymbolKind::NotExpr => {
            panic!("NOT NOT!");
        }
        SymbolKind::ListCompareExpr(op) => {
            let args = sub_term.get_args();
            state.cur_mem_pos += 1;
            let orig_mem_pos = state.cur_mem_pos;

            for arg in &args[1..] {
                // retrieve the left hand assignment and put it on the stack
                state = compile_compute_expr(&args[0], state, None, false)?;
                state.cur_mem_pos += 1;

                // retrieve the next value from the right hand list
                state = compile_compute_expr(arg, state, None, false)?;

                state.cur_mir_block.command_stack.push(Command::new(
                    OpCode::Cmp,
                    vec![CommandArg::CompareOp(op)],
                ));

                state
                    .cur_mir_block
                    .command_stack
                    .push(Command::new(OpCode::CondTrueSkipToEOB, vec![]));

                // restore old mem_pos, let's not waste memory
                state.cur_mem_pos = orig_mem_pos;
            }
        }
        _ => {
            state = compile_compute_expr(sub_term, state, None, false)?;
        }
    };

    // restore old mem_pos, let's not waste memory
    state.cur_mem_pos = saved_mem_pos;

    Ok(state)
}
