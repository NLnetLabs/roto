use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
    sync::Arc,
};

use log::{log_enabled, trace, Level};
use nom::error::VerboseError;
use smallvec::SmallVec;

use crate::{
    ast::{self, AcceptReject, FilterType, ShortString, SyntaxTree},
    blocks::Scope,
    symbols::{
        self, DepsGraph, GlobalSymbolTable, MatchActionType, Symbol,
        SymbolKind, SymbolTable,
    },
    traits::Token,
    types::{
        collections::{ElementTypeValue, List, Record},
        datasources::DataSource,
        typevalue::TypeValue,
    },
    types::{
        datasources::Table, lazyrecord_types::LazyRecordTypeDef,
        typedef::TypeDef,
    },
    vm::{
        Command, CommandArg, ExtDataSource, FilterMapArg, FilterMapArgs,
        OpCode, StackRefPos, VariablesRefTable,
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
// filter-filter-map.

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
    mis_compilations: Vec<(Scope, CompileError)>,
}

impl Rotolo {
    pub fn inspect_all_arguments(
        &self,
    ) -> HashMap<Scope, Vec<(&str, TypeDef)>> {
        let mut res = HashMap::<Scope, Vec<(&str, TypeDef)>>::new();
        for rp in self.packs.iter() {
            res.insert(
                rp.filter_map_name.clone(),
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

    pub fn get_mis_compilations(&self) -> &Vec<(Scope, CompileError)> {
        &self.mis_compilations
    }

    fn _take_pack_by_name(&mut self, name: &Scope) -> Option<RotoPack> {
        let idx = self.packs.iter().position(|p| p.filter_map_name == *name);
        if let Some(idx) = idx {
            let p = self.packs.remove(idx);
            Some(p)
        } else {
            None
        }
    }

    fn iter_all_filter_maps(
        &self,
    ) -> impl Iterator<Item = (Scope, Result<&RotoPack, CompileError>)> {
        let mp = self
            .get_mis_compilations()
            .iter()
            .map(|mp| (mp.0.clone(), Err(mp.1.clone())));
        self.packs
            .iter()
            .map(|p| (p.filter_map_name.clone(), Ok(p)))
            .chain(mp)
    }

    pub fn retrieve_public_as_arcs(
        &self,
        name: Scope,
    ) -> Result<RotoPackArc, CompileError> {
        if !self.mis_compilations.is_empty() {
            return Err(self.mis_compilations[0].1.clone());
        }
        self.iter_all_filter_maps()
            .find(|p| p.0 == name)
            .ok_or_else(|| {
                CompileError::from(format!(
                    "Can't find filter-map with specified name in this pack: {}",
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
                        filter_map_name: &p.filter_map_name,
                        filter_type: p.filter_type,
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
        name: &Scope,
    ) -> Result<RotoPackRef, CompileError> {
        self.iter_all_filter_maps()
            .find(|p| p.0 == *name)
            .ok_or_else(|| {
                CompileError::from(format!(
                    "Can't find filter-map with specified name in this pack: {}",
                    name
                ))
            })
            .and_then(|p| {
                if let Ok(p) = p.1 {
                    Ok(PublicRotoPack {
                        filter_map_name: &p.filter_map_name,
                        filter_type: p.filter_type,
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

    pub fn retrieve_first_public_as_arcs(
        &self,
    ) -> Result<RotoPackArc, CompileError> {
        self.iter_all_filter_maps()
            .take(1)
            .next()
            .ok_or_else(|| {
                CompileError::from(
                    "No filter-maps are available in this pack",
                )
            })
            .and_then(|p| {
                if let Ok(p) = p.1 {
                    Ok(PublicRotoPack::<
                        Arc<[MirBlock]>,
                        Arc<[(&str, TypeDef)]>,
                        Arc<[ExtDataSource]>,
                    > {
                        filter_map_name: &p.filter_map_name,
                        filter_type: p.filter_type,
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

    pub fn compile_all_arguments(
        &self,
        mut args: HashMap<Scope, Vec<(&str, TypeValue)>>,
    ) -> HashMap<Scope, FilterMapArgs> {
        let mut res = HashMap::<Scope, FilterMapArgs>::new();
        for pack in self.packs.iter() {
            let args =
                std::mem::take(args.get_mut(&pack.filter_map_name).unwrap());
            let cp = pack.arguments.compile_arguments(args);
            if let Ok(map) = cp {
                res.insert(pack.filter_map_name.clone(), map);
            }
        }

        res
    }

    pub fn compile_arguments(
        &self,
        name: &Scope,
        args: Vec<(&str, TypeValue)>,
    ) -> Result<FilterMapArgs, CompileError> {
        let pack = self.packs.iter().find(|p| p.filter_map_name == *name);
        if let Some(pack) = pack {
            let cp = pack.arguments.compile_arguments(args);

            match cp {
                Ok(map) => Ok(map),
                Err(err) => Err(err),
            }
        } else {
            Err(format!(
                "Can't find with specified filter-map name: {}",
                name
            )
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
    pub filter_map_name: &'a Scope,
    pub filter_type: FilterType,
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

    pub fn check_rx_payload_type(&self, payload: &TypeValue) -> bool {
        trace!("compare payload {} with type {:?}", payload, self.rx_type);
        self.rx_type == *payload
    }

    pub fn check_tx_payload_type(&self, payload: &TypeValue) -> Option<bool> {
        self.tx_type.as_ref().map(|tx| tx == payload)
    }
}

#[derive(Debug)]
struct RotoPack {
    filter_map_name: Scope,
    filter_type: FilterType,
    mir: Vec<MirBlock>,
    rx_type: TypeDef,
    tx_type: Option<TypeDef>,
    arguments: FilterMapArgs,
    data_sources: Vec<ExtDataSource>,
}

impl RotoPack {
    fn new(
        filter_map_name: Scope,
        filter_type: FilterType,
        mir: Vec<MirBlock>,
        rx_type: TypeDef,
        tx_type: Option<TypeDef>,
        arguments: FilterMapArgs,
        data_sources: Vec<ExtDataSource>,
    ) -> Self {
        RotoPack {
            filter_map_name,
            filter_type,
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
    // ) -> Result<FilterMapArgsMap, CompileError> {
    //     // Walk over all the filter_map arguments that were supplied and see if
    //     // they match up with the ones in the source code.
    //     let mut arguments_map = FilterMapArgsMap::new();
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
//             filter_map_name: rp.filter_map_name.as_str(),
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
//             filter_map_name: rp.filter_map_name.as_str(),
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

pub(crate) type Arguments<'a> =
    Vec<(Token, &'a Symbol, Vec<Command>)>;
pub(crate) type DataSources<'a> = Vec<(ShortString, &'a Symbol)>;
pub(crate) type Variables<'a> = Vec<(ShortString, &'a Symbol)>;
pub(crate) type TermSections<'a> = Vec<(ShortString, StackRefPos)>;
pub(crate) type ActionSections<'a> = Vec<(usize, StackRefPos)>;
pub(crate) type Term<'a> = &'a Symbol;
pub(crate) type Action<'a> = &'a Symbol;

#[derive(Debug)]
struct CompilerState<'a> {
    cur_filter_map: &'a SymbolTable,
    // the vec of symbols that hold all the variables that were referenced in
    // the actions -> terms -> define chain in the source code, i.e. only
    // actions that reference terms that reference variables that we're
    // defined in the `Define` section are stored here (as symbols). These
    // variables are guarenteed to be necessary, no matter how the code
    // branches. The compiler will unconditionally compile these symbols and
    // store them at the start of the MIR code.
    used_variables: Variables<'a>,
    // map of variable tokens -> memory positions to use when reading a
    // variable, filled by the compiler when compiling the `used_variables`.
    variable_ref_table: VariablesRefTable,
    used_data_sources: DataSources<'a>,
    // The cache of all arguments, global or local with their associated
    // symbol and code block to generate the retrieval of its value. Note
    // that this cache cannot be used to figure out if a argument is defined
    // in a particular scope. For the latter purpose the local_scope vec that
    // is passed around from eval to nested eval method is used. When the
    // compiler kicks in, this should already been all solved by the
    // evaluator. Having said that each argument for each scope has a unique
    // index, the first element in the tuples it stores is a 
    // Token::ActionArgument or Token::TermArgument. That token has *two*
    // usizes in it, one that corresponds to the ACtionSection or TermSection
    // it was defined in (it is the value of the token of the section, which
    // is an index from the enumration of sections in the source code). The
    // second usize is the index of the `with` argument of the enumeration of
    // all the `with` arguments in that section. Currently only one is
    // allowed, btw.
    used_arguments: Arguments<'a>,
    cur_mir_block: MirBlock,
    // the memory position that is currently empty and available to be
    // written to.
    cur_mem_pos: u32,
    var_read_only: bool,
    compiled_terms: TermSections<'a>,
    compiled_action_sections: ActionSections<'a>,
}

#[derive(Debug, Default)]
pub struct Compiler {
    pub ast: SyntaxTree,
    symbols: GlobalSymbolTable,
    // Compile time arguments
    arguments: Vec<(Scope, Vec<(ShortString, TypeValue)>)>,
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
        let mut filter_map = self.symbols.borrow_mut();
        for (filter_map_scope, args) in self.arguments.iter() {
            let filter_map_symbols =
                filter_map.get_mut(filter_map_scope).ok_or_else(|| {
                    CompileError::from(format!(
                        "Cannot find filter-map with name '{}'",
                        filter_map_scope
                    ))
                })?;
            for arg in args {
                let mut _arg =
                    filter_map_symbols.get_argument_mut(&arg.0.clone()).map_err(|_| CompileError::from(
                        format!(
                            "Cannot find argument with name '{}' for filter-map '{}'",
                            arg.0,
                            filter_map_scope
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
        let (_global_mod, filter_maps): (Vec<Scope>, Vec<Scope>) =
            _global.keys().cloned().partition(|m| *m == Scope::Global);

        drop(_global);
        let mut _global = self.symbols.borrow_mut();

        // each filter_map outputs one roto-pack with its own MIR (composed of MIR blocks),
        // its used arguments and used data sources.
        let mut packs = vec![];
        let mut mis_compilations = vec![];

        for filter_map in filter_maps {
            let _filter_map = _global.get(&filter_map).unwrap();
            let compile_result = compile_filter_map(
                _filter_map,
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
                    mis_compilations.push((filter_map, err));
                }
            }
        }

        drop(_global);

        Rotolo {
            packs,
            mis_compilations,
        }
    }

    pub fn with_arguments(
        &mut self,
        filter_map_scope: &Scope,
        args: Vec<(&str, TypeValue)>,
    ) -> Result<(), CompileError> {
        self.arguments.push((
            filter_map_scope.clone(),
            args.into_iter().map(|a| (a.0.into(), a.1)).collect(),
        ));
        // let mut filter_map = self.symbols.borrow_mut();

        // for arg in args.into_iter() {
        //     let _filter_map = filter_map
        //         .get_mut(&crate::symbols::Scope::FilterMap(filter_map_name.into()))
        //         .ok_or_else(|| CompileError::from(format!(Cannot find filter-map with name '{}'", filter_map_name)))?;

        //     let mut _arg = _filter_map.get_argument_mut(&arg.0.into()).unwrap();

        //     _arg.set_value(arg.1);
        // }
        Ok(())
    }

    pub fn build(source_code: &'a str) -> Result<Rotolo, CompileError> {
        Compiler::new().build_from_compiler(source_code)
    }

    pub fn build_from_compiler(
        mut self,
        source_code: &'a str,
    ) -> Result<Rotolo, CompileError> {
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
    command_stack: Vec<Command>,
    ty: MirBlockType,
}

impl MirBlock {
    pub fn new(ty: MirBlockType) -> Self {
        MirBlock {
            command_stack: Vec::new(),
            ty,
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Command> + '_ {
        self.command_stack.iter()
    }

    pub fn get_cur_type(&self) -> &MirBlockType {
        &self.ty
    }

    pub fn push_command(&mut self, command: Command) {
        self.command_stack.push(command);
    }

    pub fn extend(&mut self, commands: Vec<Command>) {
        self.command_stack.extend(commands);
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
    ) -> (Self, usize, SmallVec<[usize; 8]>) {
        let mut field_indexes = smallvec::smallvec![];

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

// search basically everywhere in the current global state for the value that
// corresponds to the given token. Used to look up the enum instance in a
// match expression & generate the VM code to retrieve it & put it on the
// stack.
fn generate_code_for_token_value(
    state: &CompilerState,
    token: Token,
) -> Vec<Command> {
    match token {
        Token::RxType => {
            vec![Command::new(OpCode::PushStack, vec![CommandArg::MemPos(0)])]
        }
        Token::TxType => {
            vec![Command::new(OpCode::PushStack, vec![CommandArg::MemPos(1)])]
        }
        Token::Variable(var_to) => {
            if let Some(var) = state
                .used_variables
                .iter()
                .find(|(_, var)| var_to == var.get_token().unwrap().into())
            {
                vec![Command::new(
                    OpCode::PushStack,
                    vec![CommandArg::Constant(var.1.get_value().clone())],
                )]
            } else {
                vec![]
            }
        }
        Token::Method(_) => todo!(),
        Token::Variant(_) => todo!(),
        Token::Argument(_) => {
            if let Some((_, _, code_block)) = state
                .used_arguments
                .iter()
                .find(|(to, _, _)| to == &token)
            {
                code_block.clone()
            } else {
                vec![]
            }
        }
        Token::ActionArgument(_, _) => {
            if let Some((_, _, code_block)) = state
                .used_arguments
                .iter()
                .find(|(to, _, _)| to == &token)
            {
                code_block.clone()
            } else {
                vec![]
            }
        }
        Token::TermArgument(_, _) => {
            if let Some((_, _, code_block)) = state
                .used_arguments
                .iter()
                .find(|(to, _, _)| to == &token)
            {
                code_block.clone()
            } else {
                vec![]
            }
        }
        Token::Table(_) => todo!(),
        Token::Rib(_) => todo!(),
        Token::OutputStream(_) => todo!(),
        Token::FieldAccess(_) => todo!(),
        Token::TermSection(_) => todo!(),
        Token::AnonymousTerm => todo!(),
        Token::ActionSection(_) => todo!(),
        Token::NoAction => todo!(),
        Token::MatchAction(_) => todo!(),
        Token::Constant(_) => todo!(),
        Token::AnonymousRecord => todo!(),
        Token::TypedRecord => todo!(),
        Token::List => todo!(),
        Token::BuiltinType(_) => todo!(),
        Token::Enum(_) => todo!(),
        Token::ConstEnumVariant => todo!(),
    }
}

fn compile_filter_map(
    filter_map: &SymbolTable,
    global_table: &SymbolTable,
    // data_sources: Vec<(&str, Arc<DataSource>)>,
) -> Result<RotoPack, CompileError> {
    trace!("SYMBOL MAP\n{:#?}", filter_map);

    let DepsGraph {
        rx_type,
        tx_type,
        used_arguments,
        used_variables,
        used_data_sources,
    } = filter_map.create_deps_graph()?;

    let mut state = CompilerState {
        cur_filter_map: filter_map,
        variable_ref_table: VariablesRefTable::default(),
        used_variables,
        used_data_sources,
        used_arguments: used_arguments
            .into_iter()
            .map(|(_n, s)| (s.get_token().unwrap(), s, vec![]))
            .collect::<Vec<_>>(),
        compiled_terms: TermSections::new(),
        compiled_action_sections: ActionSections::new(),
        cur_mir_block: MirBlock::new(MirBlockType::Assignment),
        var_read_only: false,
        cur_mem_pos: 0,
    };

    // initialize the command stack
    let mut mir = vec![];
    if log_enabled!(Level::Trace) {
        trace!("___filter type");
        trace!("{:?}", filter_map.get_type());

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

    (mir, state) = compile_apply_section(mir, state)?;

    state.cur_mir_block = MirBlock::new(MirBlockType::Terminator);
    state.cur_mir_block.push_command(Command::new(
        OpCode::Exit(state.cur_filter_map.get_default_action()),
        vec![],
    ));

    mir.push(state.cur_mir_block);

    trace!("\n");

    let args = state
        .used_arguments
        .iter_mut()
        .map(|a| {
            FilterMapArg::new(
                &a.1.get_name(),
                a.1.get_token().unwrap_or_else(|_| {
                    panic!("Fatal: Cannot find Token for Argument.");
                }),
                a.1.get_type(),
                TypeValue::UnInit,
            )
        })
        .collect::<Vec<_>>()
        .into();

    // Lookup all the data sources in the global symbol table. The filter_map
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
        filter_map.get_scope(),
        filter_map.get_type(),
        mir,
        rx_type.map_or(TypeDef::Unknown, |rx| rx.1),
        tx_type.map(|tx| tx.1),
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
    // whether to increase the cur_mem_pos value in the CompilerState.
    // Setting this to false, allows for creating code recursively that
    // modifies the current memory position.
    inc_mem_pos: bool,
) -> Result<CompilerState<'a>, CompileError> {
    // Compute expression trees always should have the form:
    //
    // AccessReceiver (args) -> (MethodCall | FieldAccess)*
    //
    // so always starting with an AccessReceiver. Furthermore, a variable
    // reference or assignment should always be an AccessReceiver.
    trace!("compile compute expr {:#?}", symbol);
    let is_ar = symbol.get_kind() == SymbolKind::AccessReceiver;
    let token = symbol.get_token()?;
    let kind = symbol.get_kind();

    match token {
        // ACCESS RECEIVERS

        // Assign a variable
        Token::Variable(var_to) => {
            assert!(is_ar);
            let var_ref =
                state.variable_ref_table.get_by_token_value(var_to).unwrap();

            // when writing, push the content of the referenced variable to the stack,
            // note this might be the same as the assigned mem position, but it may also
            // be different!
            state.cur_mir_block.push_command(Command::new(
                OpCode::PushStack,
                vec![CommandArg::MemPos(var_ref.mem_pos)],
            ));

            for field_index in &var_ref.field_index {
                state.cur_mir_block.push_command(Command::new(
                    OpCode::StackOffset,
                    vec![CommandArg::FieldAccess(*field_index)],
                ));
            }

            if inc_mem_pos {
                state.cur_mem_pos += 1;
            }
        }
        // a user-defined argument (filter_map or term)
        Token::Argument(arg_to) => {
            assert!(is_ar);

            // if the Argument has a value, then it was set by the argument
            // injection after eval(), that means that we can just store
            // the (literal) value in the mem pos
            if let Some((_, arg, _)) = state
                .used_arguments
                .iter()
                .find(|(to, arg, _)| to == &token && !arg.has_unknown_value())
            {
                state.cur_mir_block.push_command(Command::new(
                    OpCode::MemPosSet,
                    vec![
                        CommandArg::MemPos(state.cur_mem_pos),
                        CommandArg::Constant(arg.get_value().clone()),
                    ],
                ));
            } else {
                state.cur_mir_block.push_command(Command::new(
                    OpCode::ArgToMemPos,
                    vec![
                        CommandArg::Argument(arg_to),
                        CommandArg::MemPos(state.cur_mem_pos),
                    ],
                ));
            }

            state.cur_mir_block.push_command(Command::new(
                OpCode::PushStack,
                vec![CommandArg::MemPos(state.cur_mem_pos)],
            ));

            if inc_mem_pos {
                state.cur_mem_pos += 1;
            }
        }
        // The argument passed into an action in a 'with' statement. The
        // assumption is that the arguments live on the top of the stack at
        // the moment the action is called.
        Token::ActionArgument(_arg_index, _)
        | Token::TermArgument(_arg_index, _) => {
            assert!(is_ar);
        }
        // An enum variant mentioned in an arm of a match expression
        Token::Variant(_var_to) => {
            assert!(
                symbol.get_kind() == SymbolKind::EnumVariant
                    || symbol.get_kind() == SymbolKind::AccessReceiver
            );
            trace!("VARIANT {}", _var_to);
        }
        // rx instance reference
        Token::RxType => {
            assert!(is_ar);

            state.cur_mir_block.push_command(Command::new(
                OpCode::PushStack,
                vec![CommandArg::MemPos(0)],
            ));
        }
        // tx instance reference
        Token::TxType => {
            assert!(is_ar);

            state.cur_mir_block.push_command(Command::new(
                OpCode::PushStack,
                vec![CommandArg::MemPos(1)],
            ));
        }
        // a constant value (not a reference!)
        Token::Constant(_) => {
            let val = symbol.get_value();

            state.cur_mir_block.push_command(Command::new(
                OpCode::MemPosSet,
                vec![
                    CommandArg::MemPos(state.cur_mem_pos),
                    CommandArg::Constant(val.builtin_as_cloned_type_value()?),
                ],
            ));

            state.cur_mir_block.push_command(Command::new(
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
        // A Global Enum that is referenced with its Fully Qualified name,
        // e.g. AFI.IPV4. The Global Enum is the part before the dot ("AFI"),
        // for compilation it can be safely ignored. Mainly useful to
        // disambiguate variant of Global Enums that have the same name.
        Token::Enum(_) => {
            trace!("ENUM {:?}", symbol);
            trace!("ENUM VALUES {:?}", symbol.get_args());
            assert!(is_ar);
        }
        Token::ConstEnumVariant => {
            trace!("ENUM VARIANT VALUE {:?}", symbol.get_value());

            let val = symbol.get_value();

            state.cur_mir_block.push_command(Command::new(
                OpCode::MemPosSet,
                vec![
                    CommandArg::MemPos(state.cur_mem_pos),
                    CommandArg::Constant(val.builtin_as_cloned_type_value()?),
                ],
            ));

            state.cur_mir_block.push_command(Command::new(
                OpCode::PushStack,
                vec![CommandArg::MemPos(state.cur_mem_pos)],
            ));

            if inc_mem_pos {
                state.cur_mem_pos += 1;
            }
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
                    state.cur_mir_block.push_command(Command::new(
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
                    state.cur_mir_block.push_command(Command::new(
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
                    state.cur_mir_block.push_command(Command::new(
                        OpCode::PushOutputStreamQueue,
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
                    state.cur_mir_block.push_command(Command::new(
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
                    trace!(
                        "ANONYMOUS RECORD PARENT ARGS {:#?}",
                        symbol.get_args()
                    );
                }
                Token::TypedRecord => {
                    trace!(
                        "TYPED RECORD PARENT ARGS {:#?}",
                        symbol.get_args()
                    );
                }
                Token::Enum(_) => {
                    trace!("ENUM PARENT ARGS {:#?}", symbol.get_args());
                }
                Token::Variant(_) => {
                    trace!("VARIANT {:?}", symbol.get_args());
                }
                Token::ConstEnumVariant => {
                    trace!(
                        "CONST ENUM VARIANT PARENT ARGS {:#?}",
                        symbol.get_args()
                    );
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
                | Token::ActionArgument(_, _)
                | Token::TermArgument(_, _)
                | Token::RxType
                | Token::TxType
                | Token::Constant(_) => {
                    match kind {
                        SymbolKind::MethodCallbyRef => {
                            // args: [ method_call, type, arguments,
                            //         return_type ]
                            state.cur_mir_block.push_command(Command::new(
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
                            ));
                        }
                        SymbolKind::MethodCallByConsumedValue => {
                            // args: [ method_call, type, arguments,
                            //         return_type ]
                            state.cur_mir_block.push_command(Command::new(
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
                            ));
                        }
                        _ => {
                            panic!("PANIC!");
                        }
                    };
                }
                Token::TermSection(_) | Token::AnonymousTerm => {
                    return Err(CompileError::new(
                        "Invalid Data Source with \
                    (Anonymous)Term token "
                            .into(),
                    ));
                }
                Token::ActionSection(parent_to)
                | Token::MatchAction(parent_to) => {
                    return Err(CompileError::new(format!(
                        "Invalid data source: {:?} {:?}",
                        token, parent_to
                    )));
                }
                Token::NoAction => {
                    return Err(CompileError::from(
                        "Invalid data source: NoAction",
                    ))
                }
            };

            // Push the result to the stack for an (optional) next Accessor
            // to be used.
            state.cur_mir_block.push_command(Command::new(
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
        // A symbol with Token::FieldAccess can be of the kind SymbolKind::
        // FieldAccess, or SymbolKind::LazyFieldAccess. The latter indicates
        // that the field access has to happen on a LazyRecord. FieldAccess
        // on a LazyRecord requires the compiler to make sure that a fresh
        // copy of that LazyRecord is on the stack for the LoadLazyValue
        // command to work on. Since a LazyRecord may be queried several
        // times in a (action/term) block, it may have been indexed already.
        Token::FieldAccess(ref fa) => {
            assert!(!is_ar);
            trace!("FieldAccess {:?}", fa);
            match parent_token {
                // This a match arm, a match arm can only have a
                // LazyFieldAccess as its kind.
                Some(Token::Variant(_var_to)) => {
                    assert_eq!(
                        symbol.get_kind(),
                        SymbolKind::LazyFieldAccess
                    );
                    trace!(
                        "FieldAccess {:?} for Variant w/ parent token {:?}",
                        fa,
                        parent_token
                    );

                    // args: [field_index_0, field_index_1, ...,
                    // lazy_record_type, variant_token, return type, store
                    // memory position]
                    let args = vec![
                        CommandArg::FieldIndex(
                            fa.iter()
                                .map(|t| (*t as usize))
                                .collect::<SmallVec<_>>(),
                        ),
                        CommandArg::Type(TypeDef::LazyRecord(
                            LazyRecordTypeDef::from(_var_to),
                        )),
                        CommandArg::Type(symbol.get_type()),
                        CommandArg::MemPos(state.cur_mem_pos),
                    ];

                    state
                        .cur_mir_block
                        .command_stack
                        .push(Command::new(OpCode::LoadLazyFieldValue, args));

                    // Push the computed variant value from the memory
                    // position onto the stack.
                    state.cur_mir_block.push_command(Command::new(
                        OpCode::PushStack,
                        vec![CommandArg::MemPos(state.cur_mem_pos)],
                    ));

                    state.cur_mir_block.push_command(Command::new(
                        OpCode::CondUnknownSkipToLabel,
                        vec![],
                    ));
                }
                // The `with` argument of an Action can be of a regular
                // FieldAccess, or a LazyFieldAccess kind.
                Some(Token::ActionArgument(_, _))
                | Some(Token::TermArgument(_, _)) => {
                    trace!("name {}", symbol.get_name());
                    trace!(
                        "LazyFieldAccess {:?} with action argument {:?}",
                        fa,
                        parent_token
                    );

                    trace!("used arguments");
                    trace!("{:#?}", state.used_arguments);
                    let argument_s = state
                        .used_arguments
                        .iter()
                        .find(|(to, _, _)| {
                            to == parent_token.as_ref().unwrap()
                        })
                        .map(|a| a.1)
                        .unwrap();

                    trace!("stored argument {:?}", argument_s);
                    trace!("state arguments {:#?}", state.used_arguments);
                    state.cur_mir_block.extend(
                        generate_code_for_token_value(
                            &state,
                            argument_s.get_token()?,
                        ),
                    );

                    match symbol.get_kind() {
                        SymbolKind::LazyFieldAccess => {
                            let args = vec![
                                CommandArg::FieldIndex(
                                    fa.iter()
                                        .map(|t| (*t as usize))
                                        .collect::<SmallVec<_>>(),
                                ),
                                CommandArg::Type(argument_s.get_type()),
                                CommandArg::Type(symbol.get_type()),
                                CommandArg::MemPos(state.cur_mem_pos),
                            ];

                            state.cur_mir_block.command_stack.push(
                                Command::new(
                                    OpCode::LoadLazyFieldValue,
                                    args,
                                ),
                            );

                            state.cur_mir_block.push_command(Command::new(
                                OpCode::PushStack,
                                vec![CommandArg::MemPos(state.cur_mem_pos)],
                            ));

                            state.cur_mem_pos += 1;
                        }
                        SymbolKind::FieldAccess => {
                            todo!();
                        }
                        _ => {
                            return Err(CompileError::from(format!(
                                "Invalid FieldAccess Kind in {:#?}",
                                symbol.get_name()
                            )));
                        }
                    };
                }
                // This is a regular field access.
                _ => {
                    trace!("FIELD ACCESS PARENT TOKEN {:#?}", parent_token);
                    trace!("current symbol {:#?}", symbol);
                    trace!("fa {:?}", fa);
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
            }
        }
        // A NamedTerm shoould be compiled with the `compile_term` method,
        // that will make sure it gets compiled and stored in the compiler
        // state `terms` hashmap, so that it can be inlined in multiple
        // places. It ending up here is an error.
        Token::TermSection(_) => {
            return Err(CompileError::new(
                "Found invalid Token of variant NamedTerm for compute \
                expression"
                    .into(),
            ));
        }
        // Anonymous terms are compile in-line as a one off, only used in
        // match expressions (at least for now).
        Token::AnonymousTerm => {
            trace!("TOKEN ANONYMOUS SYMBOL {:#?}", symbol);
            let sub_terms = symbol.get_args();

            for sub_term in sub_terms {
                state = compile_term(sub_term, state)?;
            }
            return Ok(state);
        }
        Token::MatchAction(_) => {
            return Err(CompileError::new(
                "Found invalid Token of variant MatchAction for compute \
                expression"
                    .into(),
            ));
        }
        Token::ActionSection(_) => {
            return Err(CompileError::new(
                "Found invalid Token of variant ActionSection for compute \
                expression"
                    .into(),
            ));
        }
        Token::NoAction => {
            trace!("TOKEN ACTION {:#?}", symbol);
            // This a match arm
            if let Some(Token::Variant(_var_to)) = parent_token {
                trace!("Unpack Variant w/ parent token {:?}", parent_token);

                // get the type of variant, stored in the parent `ty` field

                // args: [field_index_0, field_index_1, ...,
                // lazy_record_type, variant_token, return type, store
                // memory position]
                let args = vec![
                    CommandArg::FieldIndex(SmallVec::new()),
                    CommandArg::Type(TypeDef::LazyRecord(
                        LazyRecordTypeDef::from(_var_to),
                    )),
                    CommandArg::Type(TypeDef::LazyRecord(
                        LazyRecordTypeDef::from(_var_to),
                    )),
                    CommandArg::MemPos(state.cur_mem_pos),
                ];

                state
                    .cur_mir_block
                    .command_stack
                    .push(Command::new(OpCode::LoadLazyFieldValue, args));

                state.cur_mir_block.push_command(Command::new(
                    OpCode::PushStack,
                    vec![CommandArg::MemPos(state.cur_mem_pos)],
                ));

                state.cur_mir_block.push_command(Command::new(
                    OpCode::CondUnknownSkipToLabel,
                    vec![],
                ));
            };
        }
        // This record is defined without a type and used directly, mainly as
        // an argument for a method. The inferred type is unambiguous.
        Token::AnonymousRecord => {
            assert!(!is_ar);

            // Re-order the args vec on this symbol to reflect the ordering
            // on the type definition for the record. The original ordering
            // is the order in which it was specificed in the source code,
            // the resulting order is alphabetically on the names of the
            // fields. We're using a bit of a trick to re-order, since the
            // `symbol` variable is not mutable. So create a Vec<&Symbol>
            // from the original &[Symbol]. The vec can then be re-ordered
            // and we're writing the state in the order of the vec elements.
            let mut field_symbols =
                symbol.get_args().iter().collect::<Vec<_>>();
            field_symbols.sort_by_key(|a| a.get_name());
            trace!("ANONYMOUS RECORD FIELDS {:#?}", field_symbols);

            for arg in field_symbols {
                state = compile_compute_expr(arg, state, None, true)?;
            }

            return Ok(state);
        }
        // This is a record that appears in a variable assigment that creates
        // a record in the `Define` section or it is an argument to a method
        // call.
        Token::TypedRecord => {
            assert!(!is_ar);

            trace!("TYPED RECORD FIELDS {:#?}", symbol.get_args());
            trace!("Checked Type {:#?}", symbol.get_type());
            let values = symbol
                .get_recursive_values_primitive(symbol.get_type())?
                .iter()
                .map(|v| (v.0.clone(), v.2.clone().into()))
                .collect::<Vec<_>>();

            trace!("values {:?}", values);
            let unresolved_values = values
                .clone()
                .into_iter()
                .filter(|v| v.1 == TypeValue::Unknown)
                .collect::<Vec<_>>();
            let unresolved_symbols = unresolved_values
                .into_iter()
                .map(|v| {
                    symbol.get_args().iter().find(|s| s.get_name() == v.0)
                })
                .collect::<Vec<_>>();
            trace!("unresolved symbols {:#?}", unresolved_symbols);
            let value_type = Record::new(values);
            trace!("value_type {:?}", value_type);

            if symbol.get_type() != TypeValue::Record(value_type.clone()) {
                return Err(CompileError::from(format!(
                    "This record: {} is of type {}, but we got a record with \
                    type {}. It's not the same and cannot be converted.",
                    value_type,
                    symbol.get_type(),
                    TypeDef::Record(
                        symbol
                            .get_args()
                            .iter()
                            .map(|v| (v.get_name(), Box::new(v.get_type())))
                            .collect::<Vec<_>>()
                            .into()
                    )
                )));
            }
            state.cur_mir_block.push_command(Command::new(
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
            state.cur_mir_block.push_command(Command::new(
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

    trace!("parent token {:?}", parent_token);
    trace!("current token {:?}", symbol.get_token());

    if let Ok(Token::ActionArgument(_, _)) = symbol.get_token() {
        parent_token = symbol.get_token().ok();
    } else {
        parent_token = if parent_token.is_some() {
            parent_token
        } else {
            symbol.get_token().ok()
        };
    }
    trace!("resulting token {:?}", parent_token);

    for arg in symbol.get_args() {
        state = compile_compute_expr(arg, state, parent_token, inc_mem_pos)?;
        parent_token = arg.get_token().ok();
    }

    Ok(state)
}

// Compiles the variable assigments, creates a MirBlock that retrieves and/or
// computes the value of the variable and stores it in the
// `variables_ref_table` map. Note that in cases where the variables
// assignments points to a field access, the access receiver of the
// assignment is stored together with the ield_index on the access receiver
// that points to the actual variable assignment.
fn compile_assignments(
    mut mir: Vec<MirBlock>,
    mut state: CompilerState<'_>,
) -> Result<(Vec<MirBlock>, CompilerState<'_>), CompileError> {
    let _filter_map = state.cur_filter_map;

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

        let s = _filter_map.get_variable_by_token(&var.1.get_token()?);

        state.cur_mir_block.push_command(Command::new(
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

        state.variable_ref_table.set(
            var.1.get_token()?.into(),
            cur_mem_pos as u32,
            field_indexes,
        )?;

        mir.push(cur_mir_block);
        state.cur_mir_block = MirBlock::new(MirBlockType::Assignment);
    }

    state.cur_mem_pos = 1 + state.used_variables.len() as u32;
    trace!("local variables map");
    trace!("{:#?}", state.variable_ref_table);

    Ok((mir, state))
}

fn compile_apply_section(
    mut mir: Vec<MirBlock>,
    mut state: CompilerState<'_>,
) -> Result<(Vec<MirBlock>, CompilerState), CompileError> {
    state.cur_mir_block = MirBlock::new(MirBlockType::MatchAction);

    let match_action_sections =
        state.cur_filter_map.get_match_action_sections();

    trace!(
        "compiling MATCH ACTION SECTIONS {:#?}",
        match_action_sections
    );
    // Collect the terms that we need to compile.
    for match_action in match_action_sections {
        let ma_name = match_action.get_name();
        let match_action = &match_action.symbol;

        match match_action.get_kind() {
            // A Pattern Match Action
            // similar to compile_compute_expr GlobalEnum handling
            symbols::SymbolKind::GlobalEnum => {
                trace!(
                    "compiling ENUM MATCH ACTION EXPRESSION {} {:?}",
                    match_action.get_name(),
                    match_action.get_kind_type_and_token()
                );

                // SSA and Ordered Types

                // This is my take on Static Single-Assignment form (SSA). Although
                // variables in Roto are immutable, pointing to indexes of a Record
                // or enum instance mutates the original value (the record or enum
                // instance): once a record is indexed or an enum is unpacked into
                // its variant and that gets indexed, the original value is lost to
                // the compiler beyond that point, it simply can't know where to
                // retrieve the original value anymore. So, we need to keep track
                // of each time the original value is referenced and supply the
                // compiler with a fresh copy of the original value. This is what
                // SSA is supposed to do, or, in other words, the MIR language can
                // only deal with Ordered Types (every variable is of a unique type
                // that can only be used once and in the order of appearance). The
                // solution is fairly easy, just before one of these values is
                // referenced, we put a block of code in front of it that pushed
                // the original value (from a protected memory position) onto the
                // stack. This is that code for the action argument ('with' clause)
                // of an action section
                let enum_instance_code_block = generate_code_for_token_value(
                    &state,
                    match_action.get_token()?,
                );

                state.cur_mem_pos += 1;
                let orig_mem_pos = state.cur_mem_pos;

                state.cur_mir_block.push_command(Command::new(
                    OpCode::Label,
                    vec![CommandArg::Label(
                        format!("ENUM {}", match_action.get_name())
                            .as_str()
                            .into(),
                    )],
                ));

                // push the enum instance to the stack.
                state.cur_mir_block.extend(enum_instance_code_block.clone());
                let mut first_variant_done = false;

                for variant in match_action.get_args() {
                    trace!(
                        "compiling variant {} for enum {}...",
                        variant.get_name(),
                        match_action.get_name()
                    );
                    // The variant here is a symbol that must be of kind
                    // EnumVariant, must have a Token::Variant as its token.
                    assert_eq!(
                        variant.get_kind(),
                        symbols::SymbolKind::EnumVariant
                    );
                    state.cur_mir_block.push_command(Command::new(
                        OpCode::Label,
                        vec![CommandArg::Label(
                            format!("VARIANT {}", variant.get_name())
                                .as_str()
                                .into(),
                        )],
                    ));

                    // Unpack the enum instance into the variant that we're
                    // handling currently. StackUnPackAsVariant will set
                    // TypeValue::Unknown if this is not the variant we're
                    // looking for at runtime.
                    if let Ok(Token::Variant(variant_index)) =
                        variant.get_token()
                    {
                        // if this is *not* the first variant we first want
                        // to pop the top of the stack, since that will
                        // contain the bool value of the last variant
                        // evaluation, and we don't want that. We want the
                        // enum instance underneath it.
                        if first_variant_done {
                            state.cur_mir_block.push_command(Command::new(
                                OpCode::PopStack,
                                vec![],
                            ))
                        }
                        state.cur_mir_block.push_command(Command::new(
                            OpCode::StackIsVariant,
                            vec![CommandArg::Variant(variant_index)],
                        ));
                        state.cur_mir_block.push_command(Command::new(
                            OpCode::CondFalseSkipToEOB,
                            vec![CommandArg::Variant(variant_index)],
                        ));
                        // If we're continuing then remove the variant
                        // boolean indication.
                        state.cur_mir_block.push_command(Command::new(
                            OpCode::PopStack,
                            vec![],
                        ));
                    } else {
                        return Err(CompileError::from(format!(
                            "Expected an Enum Variant, but got a {:?}",
                            variant.get_token()
                        )));
                    }

                    // the args of this variant symbol must be of kind
                    // ActionCall and, all have tokens
                    // Token::ActionSection or Token::NoAction.

                    for action_section in variant.get_args() {
                        // Check if the user defined a guard for this variant

                        // GUARD START ---------------------------------------

                        if action_section.get_kind() == SymbolKind::TermCall {
                            trace!("found guard");
                            // See if it was already compiled earlier on.
                            // See if it was already compiled earlier on.
                            let term_name = state
                                .compiled_terms
                                .iter()
                                .find(|t| t.0 == action_section.get_name());

                            match term_name {
                                // yes, it was, create a reference to the result on the stack
                                Some((_, stack_ref_pos)) => {
                                    if let StackRefPos::MemPos(mem_pos) =
                                        stack_ref_pos
                                    {
                                        state
                                            .cur_mir_block
                                            .command_stack
                                            .extend(vec![
                                                Command::new(
                                                    OpCode::Label,
                                                    vec![CommandArg::Label(
                                                        format!(
                                        "COPY TERM SECTION RESULT {}",
                                        action_section.get_name().clone()
                                    )
                                                        .as_str()
                                                        .into(),
                                                    )],
                                                ),
                                                Command::new(
                                                    OpCode::PushStack,
                                                    vec![CommandArg::MemPos(
                                                        *mem_pos,
                                                    )],
                                                ),
                                            ]);
                                    };
                                }
                                // no, compile the term.
                                _ => {
                                    trace!("not found, compile it");
                                    let _filter_map = state.cur_filter_map;
                                    let terms = _filter_map.get_terms();
                                    let term = terms
                                        .iter()
                                        .find(|t| {
                                            t.get_name()
                                                == action_section.get_name()
                                        })
                                        .unwrap();
                                    trace!("term {:#?}", term);
                                    state = compile_term_section(
                                        term,
                                        state,
                                        &enum_instance_code_block,
                                    )?;

                                    state.cur_mir_block.push_command(
                                        Command::new(
                                            OpCode::CondFalseSkipToEOB,
                                            vec![],
                                        ),
                                    );
                                    trace!("compile_term_section");
                                    trace!("{:#?}", state.cur_mir_block);

                                    // store the resulting value into a variable so that future references
                                    // to this term can directly use the result instead of doing the whole
                                    // computation again.
                                    state.compiled_terms.push((
                                        action_section.get_name().clone(),
                                        state.cur_mem_pos.into(),
                                    ));
                                }
                            };

                            state.cur_mem_pos += 1;
                            continue;
                        }
                        // GUARD END _-------------------------------------------------------------

                        trace!("ACTION SECTION {:#?}", action_section);

                        assert!(
                            action_section.get_type()
                                == TypeDef::AcceptReject(
                                    AcceptReject::Accept
                                )
                                || action_section.get_type()
                                    == TypeDef::AcceptReject(
                                        AcceptReject::Reject
                                    )
                                || action_section.get_type()
                                    == TypeDef::AcceptReject(
                                        AcceptReject::NoReturn
                                    )
                        );
                        assert_eq!(
                            action_section.get_kind(),
                            symbols::SymbolKind::ActionCall
                        );

                        match action_section.get_token()? {
                            Token::ActionSection(as_id) => {
                                let as_name = state
                                    .compiled_action_sections
                                    .iter()
                                    .find(|t| t.0 == as_id);

                                match as_name {
                                    // yes, it was, create a reference to the result on the stack
                                    Some((_, stack_ref_pos)) => {
                                        if let StackRefPos::MemPos(mem_pos) =
                                            stack_ref_pos
                                        {
                                            state.cur_mir_block.command_stack.extend(vec![
                                                Command::new(
                                                    OpCode::Label,
                                                    vec![CommandArg::Label(
                                                        format!(
                                                            "COPY ACTION SECTION RESULT {}",
                                                            ma_name.clone()
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
                                    // no, compile the action section.
                                    _ => {
                                        let action_sections = state
                                            .cur_filter_map
                                            .get_action_sections();
                                        let a_s = action_sections
                                            .iter()
                                            .find(|t| {
                                                t.get_token().unwrap()
                                                    == Token::ActionSection(
                                                        as_id,
                                                    )
                                            })
                                            .unwrap();
                                        state = compile_action_section(
                                            a_s,
                                            &enum_instance_code_block,
                                            state,
                                        )?;

                                        // store the resulting value into a
                                        // variable so that future references
                                        // to this actionsection can directly
                                        // use the result instead of doing
                                        // the whole computation again.
                                        state.compiled_action_sections.push(
                                            (as_id, state.cur_mem_pos.into()),
                                        );
                                        state.cur_mem_pos += 1;
                                    }
                                };
                            }
                            Token::NoAction => {
                                trace!("No action defined. Nothing to do.");
                            }
                            _ => {
                                panic!("Invalid action section token found.");
                            }
                        };

                        let accept_reject = if let TypeDef::AcceptReject(
                            accept_reject,
                        ) = action_section.get_type()
                        {
                            accept_reject
                        } else {
                            panic!("No Accept or Reject found in Action Section.");
                        };

                        // Add an early return if the type of the match
                        // action is either `Reject` or
                        // `Accept`.
                        if accept_reject != AcceptReject::NoReturn {
                            state.cur_mir_block.command_stack.push(
                                Command::new(
                                    OpCode::Exit(accept_reject),
                                    vec![],
                                ),
                            )
                        }
                    }

                    // move the current mir block to the end of all the collected MIR.
                    mir.push(state.cur_mir_block);

                    // continue with a fresh block
                    state.cur_mir_block =
                        MirBlock::new(MirBlockType::MatchAction);
                    // restore old mem_pos, let's not waste memory
                    state.cur_mem_pos = orig_mem_pos;

                    first_variant_done = true;
                }
            }
            // A Filter Match Action
            symbols::SymbolKind::MatchAction(ma) => {
                // See if it was already compiled earlier on.
                let term_name =
                    state.compiled_terms.iter().find(|t| t.0 == ma_name);

                match term_name {
                    // yes, it was, create a reference to the result on the stack
                    Some((_, stack_ref_pos)) => {
                        if let StackRefPos::MemPos(mem_pos) = stack_ref_pos {
                            state.cur_mir_block.command_stack.extend(vec![
                                Command::new(
                                    OpCode::Label,
                                    vec![CommandArg::Label(
                                        format!(
                                            "COPY TERM SECTION RESULT {}",
                                            ma_name.clone()
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
                        let _filter_map = state.cur_filter_map;
                        let terms = _filter_map.get_terms();
                        let term = terms
                            .iter()
                            .find(|t| t.get_name() == ma_name)
                            .unwrap();

                        state.cur_mir_block =
                            MirBlock::new(MirBlockType::Term);
                        state = compile_term_section(term, state, &[])?;

                        // store the resulting value into a variable so that future references
                        // to this term can directly use the result instead of doing the whole
                        // computation again.
                        state.compiled_terms.push((
                            ma_name.clone(),
                            state.cur_mem_pos.into(),
                        ));
                    }
                };

                state.cur_mem_pos += 1;

                // move the current mir block to the end of all the collected MIR.
                mir.push(state.cur_mir_block);

                // continue with a fresh block
                state.cur_mir_block =
                    MirBlock::new(MirBlockType::MatchAction);

                match ma {
                    MatchActionType::FilterMatchAction => {
                        state.cur_mir_block.command_stack.extend([
                            Command::new(
                                OpCode::Label,
                                vec![CommandArg::Label(
                                    format!(
                                        "MATCH ACTION {}-{:?}",
                                        ma_name.clone(),
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
                                        ma_name,
                                        match_action.get_kind()
                                    )
                                    .as_str()
                                    .into(),
                                )],
                            ),
                            Command::new(OpCode::CondTrueSkipToEOB, vec![]),
                        ]);
                    }
                    MatchActionType::PatternMatchAction => {
                        panic!("Illegal Value for Match Action Type encountered. This is fatal.")
                    }
                }

                state = compile_match_action(match_action, vec![], state)?;
            }
            _ => {
                return Err(CompileError::new("invalid match action".into()));
            }
        };

        trace!("Accept Reject");
        let accept_reject = if let TypeDef::AcceptReject(accept_reject) =
            match_action.get_type()
        {
            accept_reject
        } else {
            AcceptReject::NoReturn
        };

        // Add an early return if the type of the match action is either `Reject` or
        // `Accept`.
        if accept_reject != AcceptReject::NoReturn {
            state
                .cur_mir_block
                .command_stack
                .push(Command::new(OpCode::Exit(accept_reject), vec![]))
        }

        // move the current mir block to the end of all the collected MIR.
        mir.push(state.cur_mir_block);

        // continue with a fresh block
        state.cur_mir_block = MirBlock::new(MirBlockType::MatchAction);
    }
    trace!("done compiling apply section");

    Ok((mir, state))
}

fn compile_match_action<'a>(
    match_action: &'a symbols::Symbol,
    enum_instance_code_block: Vec<Command>,
    mut state: CompilerState<'a>,
) -> Result<CompilerState<'a>, CompileError> {
    // collect all actions referenced in this match_action and compile them.
    for action_call in match_action.get_args() {
        let filter_map = state.cur_filter_map;
        let action_name = action_call.get_name();

        let accept_reject = match action_call.get_type() {
            // A FilterMatchAction should only appear here with a
            // AcceptReject value as its type
            TypeDef::AcceptReject(accept_reject) => accept_reject,
            // An PatternMatchAction has as its type Enum, which doesn't have
            // an AcceptReject.
            TypeDef::GlobalEnum(_) => AcceptReject::NoReturn,
            _ => {
                return Err(CompileError::from(format!(
                    "Cannot convert `{}` with type {} into match action type",
                    action_call.get_name(),
                    action_call.get_type()
                )));
            }
        };

        if let Some(action) = filter_map
            .get_action_sections()
            .iter()
            .find(|t| t.get_name() == action_name)
        {
            state = compile_action_section(
                action,
                &enum_instance_code_block,
                state,
            )?;
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

    Ok(state)
}

fn compile_action_section<'a>(
    action_section: &'a symbols::Symbol,
    // A block of code that retrieves the action argument and puts it on the
    // stack (if any arguments are available in the ActionSection).
    argument_code_block: &[Command],
    mut state: CompilerState<'a>,
) -> Result<CompilerState<'a>, CompileError> {
    // do not create assignment block, only produce code to read variables
    // when referenced.
    state.var_read_only = true;

    // Set a Label so that each term block is identifiable for humans.
    trace!("compiling ACTION SECTION {}...", action_section.get_name());

    for action in action_section.get_args() {
        state = compile_action(action, argument_code_block, state)?;
    }

    Ok(state)
}

fn compile_action<'a>(
    action: Action<'a>,
    // The argument code block is needed each time a argument is referenced
    // in the action.
    argument_code_block: &[Command],
    mut state: CompilerState<'a>,
) -> Result<CompilerState<'a>, CompileError> {
    match action.get_kind() {
        // A symbol with an RxType token, should be an access receiver.
        SymbolKind::AccessReceiver => {
            trace!("compiling ACTION {:#?}", action);
            state = compile_compute_expr(action, state, None, false)?;
            state.cur_mem_pos += 1;
        }
        // Variable arguments that are passed in into an action appear as
        // Constants in the args of the ActionSection, so they end up
        // here.
        SymbolKind::Argument => {
            state.used_arguments.push((
                action.get_token()?,
                action,
                argument_code_block.to_vec(),
            ));
        }
        _ => {
            trace!("Faulty ACTION {:#?}", action);
            return Err(CompileError::new(format!(
                "Invalid action {}",
                action.get_name()
            )));
        }
    }

    Ok(state)
}

fn compile_term_section<'a>(
    term_section: &'a symbols::Symbol,
    mut state: CompilerState<'a>,
    // The argument code block is needed each time a argument is referenced
    // in the term section.
    argument_code_block: &[Command],
) -> Result<CompilerState<'a>, CompileError> {
    // do not create assignment block, only produce code to read variables
    // when referenced.
    state.var_read_only = true;

    // Set a Label so that each term block is identifiable for humans.
    trace!("TERM SECTION {}", term_section.get_name());

    // Push the code block that retrieves the argument for this section to
    // the cache.
    if let Some(s) = term_section.get_args().get(0) {
        if let Ok(token) = s.get_token() {
            state.used_arguments.push((
                token,
                s,
                argument_code_block.to_vec(),
            ));
        }
    }

    state.cur_mir_block.push_command(Command::new(
        OpCode::Label,
        vec![CommandArg::Label(
            format!("TERM SECTION {}", term_section.get_name())
                .as_str()
                .into(),
        )],
    ));

    let terms = term_section.get_args();
    trace!("TERMS {:#?}", terms);
    let mut terms = terms.iter().peekable();

    while let Some(arg) = &mut terms.next() {
        state = compile_term(arg, state)?;

        assert_ne!(state.cur_mir_block.command_stack.len(), 0);

        // Since terms are ANDed we can create an early return after
        // each term that isn't last.
        if terms.peek().is_some() {
            state
                .cur_mir_block
                .command_stack
                .push(Command::new(OpCode::CondFalseSkipToEOB, vec![]));
        }
    }

    Ok(state)
}

fn compile_term<'a>(
    term: Term<'a>,
    mut state: CompilerState<'a>,
) -> Result<CompilerState<'a>, CompileError> {
    let saved_mem_pos = state.cur_mem_pos;
    match term.get_kind() {
        SymbolKind::CompareExpr(op) => {
            let args = term.get_args();
            state = compile_compute_expr(&args[0], state, None, false)?;
            state.cur_mem_pos += 1;
            state = compile_compute_expr(&args[1], state, None, false)?;

            state
                .cur_mir_block
                .command_stack
                .push(Command::new(OpCode::Cmp, vec![op.into()]));
        }
        SymbolKind::OrExpr => {
            let args = term.get_args();
            state = compile_term(&args[0], state)?;
            state.cur_mem_pos += 1;
            state = compile_term(&args[1], state)?;

            state.cur_mir_block.push_command(Command::new(
                OpCode::Cmp,
                vec![CommandArg::CompareOp(ast::CompareOp::Or)],
            ));
        }
        SymbolKind::AndExpr => {
            let args = term.get_args();
            state = compile_term(&args[0], state)?;
            state.cur_mem_pos += 1;
            state = compile_term(&args[1], state)?;

            state.cur_mir_block.push_command(Command::new(
                OpCode::Cmp,
                vec![CommandArg::CompareOp(ast::CompareOp::And)],
            ));
        }
        SymbolKind::NotExpr => {
            todo!();
        }
        SymbolKind::ListCompareExpr(op) => {
            let args = term.get_args();
            state.cur_mem_pos += 1;
            let orig_mem_pos = state.cur_mem_pos;

            for arg in &args[1..] {
                // retrieve the left hand assignment and put it on the stack
                state = compile_compute_expr(&args[0], state, None, false)?;
                state.cur_mem_pos += 1;

                // retrieve the next value from the right hand list
                state = compile_compute_expr(arg, state, None, false)?;

                state.cur_mir_block.push_command(Command::new(
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
        SymbolKind::GlobalEnum => {
            trace!(
                "ENUM TERM EXPRESSION {} {:?}",
                term.get_name(),
                term.get_kind_type_and_token()
            );

            let variants = term.get_args();
            state.cur_mem_pos += 1;
            let orig_mem_pos = state.cur_mem_pos;

            state.cur_mir_block.push_command(Command::new(
                OpCode::Label,
                vec![CommandArg::Label(
                    format!("ENUM {}", term.get_name()).as_str().into(),
                )],
            ));

            trace!("enum token {:?}", term.get_token());

            for variant in variants {
                state.cur_mir_block.push_command(Command::new(
                    OpCode::Label,
                    vec![CommandArg::Label(
                        format!("VARIANT_{}", variant.get_name())
                            .as_str()
                            .into(),
                    )],
                ));

                // push the enum instance to the stack.
                state.cur_mir_block.extend(generate_code_for_token_value(
                    &state,
                    term.get_token()?,
                ));

                // compile the variants.
                state = compile_compute_expr(variant, state, None, false)?;

                state
                    .cur_mir_block
                    .command_stack
                    .push(Command::new(OpCode::CondTrueSkipToEOB, vec![]));
            }

            // restore old mem_pos, let's not waste memory
            state.cur_mem_pos = orig_mem_pos;
        }
        _ => {
            trace!(
                "RESIDUAL TERM EXPRESSION {} {:?}",
                term.get_name(),
                term.get_kind_type_and_token()
            );
            state = compile_compute_expr(term, state, None, false)?;
        }
    };

    // restore old mem_pos, let's not waste memory
    state.cur_mem_pos = saved_mem_pos;

    Ok(state)
}
