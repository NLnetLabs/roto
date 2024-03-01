//! # The Compiler (Filter creation time)
//!
//! Our starting point here is the Global Symbol Table that falls out of the
//! evaluation of the AST.
//!
//! First, the `arguments` map is left alone, all of the values for the
//! arguments supplied by the caller at filter runtime. All the entries in the
//! `variables` map could now be compiled to their tokenized values. But, that
//! may be too much, since a user can declare variables that are never used
//! anywhere. Therefore we start from the other side, and go over the terms
//! first and then tokenize all the arguments encountered there. That way we
//! can signal to the user which variables are actually not used.
//!
//! # The Virtual Machine (Filter RunTime)
//!
//! The virtual machine starts at each run of a filter and will be called with
//! the input payload and the with arguments from the `define` section of the
//! filter-filter-map.
//!
//! Our starting point here is the Global Symbol Table that falls out of the
//! evaluation of the AST.
//!
//! First, the `arguments` map is left alone, all of its values should be
//! supplied by the caller. All the entries in the `variables` map could now
//! be interpreted to their final values. But that could be too much work,
//! since users may specify any number of variables that are not actually used
//! in the filter run-time. If we start from interpreting the terms then we'll
//! bump only into the variables that are actually used.

use std::{
    collections::{HashMap, VecDeque},
    fmt::{Display, Formatter},
    sync::Arc,
};

use log::{log_enabled, trace, Level};
use nom::error::VerboseError;

use crate::{
    ast::{self, AcceptReject, FilterType, ShortString, SyntaxTree},
    blocks::Scope,
    compiler::recurse_compile::recurse_compile,
    symbols::{
        self, DepsGraph, GlobalSymbolTable, MatchActionType, Symbol,
        SymbolKind, SymbolTable,
    },
    traits::Token,
    types::{datasources::DataSource, typevalue::TypeValue},
    types::{
        datasources::Table,
        typedef::{RecordTypeDef, TypeDef},
    },
    vm::{
        compute_hash, Command, CommandArg, CompiledCollectionField,
        CompiledField, CompiledPrimitiveField, CompiledVariable,
        ExtDataSource, FilterMapArg, FilterMapArgs, OpCode, StackRefPos,
        VariablesRefTable, FieldIndex,
    },
};

pub use crate::compiler::error::CompileError;

//========================== Compiler ========================================

//------------ Rotolo --------------------------------------------------------

/// Rotolo is the collection of compiled packages, called RotoPacks.
///
/// Not all filter maps may have succeeded to compile, so there is also a vec
/// with the names and error of the Filter(Map)s that couldn't be compiled.
///
/// Rotolo holds all the attributes of the compilation as owned. There are
/// public methods to retrieve and iter over the packs filled with references
/// or with arcs.

#[derive(Debug, Clone)]
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

    pub fn take_pack_by_name(
        &mut self,
        name: &Scope,
    ) -> Result<RotoPack, CompileError> {
        let idx = self.packs.iter().position(|p| p.filter_map_name == *name);
        if let Some(idx) = idx {
            let p = self.packs.remove(idx);
            Ok(p)
        } else {
            Err(CompileError::from(format!(
                "Cannot find roto pack with name '{:?}'",
                name
            )))
        }
    }

    pub fn packs_to_owned(&mut self) -> Vec<RotoPack> {
        std::mem::take(&mut self.packs)
    }

    pub fn clean_packs_to_owned(mut self) -> Vec<RotoPack> {
        self.packs.retain(|p| {
            matches!(
                p.filter_type,
                FilterType::Filter | FilterType::FilterMap
            )
        });
        self.packs
    }

    pub fn get_scopes(&self) -> Vec<Scope> {
        self.iter_as_refs().map(|p| p.0).collect::<Vec<_>>()
    }

    // this iterator is not public because that would leak the (private)
    // RotoPack.
    fn iter<'a, T: From<&'a RotoPack>>(
        &'a self,
    ) -> impl Iterator<Item = (Scope, Result<T, CompileError>)> + 'a {
        let mp = self
            .get_mis_compilations()
            .iter()
            .map(|mp| (mp.0.clone(), Err(mp.1.clone())));
        self.packs
            .iter()
            .map(|p| (p.filter_map_name.clone(), Ok(p.into())))
            .chain(mp)
    }

    // this iterator is not public because that would leak the (private)
    // RotoPack.
    fn iter_clean<'a, T: From<&'a RotoPack>>(
        &'a self,
    ) -> impl Iterator<Item = T> + 'a {
        self.packs
            .iter()
            .filter(|p| {
                matches!(
                    p.filter_type,
                    FilterType::Filter | FilterType::FilterMap
                )
            })
            .map(|p| p.into())
    }

    /// Returns an iterator that goes over all the mis-compilations and packs.
    /// The items returned from this Iterator are Results over RotoPacks
    /// filled with `Arc<T>` over all collection-type attributes T inside the
    /// pack, or, they are a Compile Error, indicating a mis-compilation.
    pub fn iter_as_arcs(
        &self,
    ) -> impl Iterator<Item = (Scope, Result<RotoPackArc, CompileError>)>
    {
        self.iter::<RotoPackArc>()
    }

    /// Returns an iterator that goes over all the mis-compilations and packs.
    /// The items returned from this Iterator are Results over RotoPacks
    /// filled with `Arc<T>` over all collection-type attributes T inside the
    /// pack, or, they are a Compile Error, indicating a mis-compilation.
    pub fn iter_clean_as_arcs(&self) -> impl Iterator<Item = RotoPackArc> {
        self.iter_clean::<RotoPackArc>()
    }

    /// Returns an iterator that goes over all the mis-compilations and packs.
    /// The items returned from this Iterator are RotoPacks filled with &T
    /// over all collection-type attributes T inside the pack, or, they are a
    /// Compile Error, indicating a mis-compilation.
    pub fn iter_as_refs(
        &self,
    ) -> impl Iterator<Item = (Scope, Result<RotoPackRef, CompileError>)>
    {
        self.iter::<RotoPackRef>()
    }

    // Not public, because it would leak the (private) RotoPack.
    fn retrieve_pack<'a, T: From<&'a RotoPack>>(
        &'a self,
        name: &'a Scope,
    ) -> Result<T, CompileError> {
        if !self.mis_compilations.is_empty() {
            return Err(self.mis_compilations[0].1.clone());
        }
        self.iter::<&RotoPack>()
            .find(|p| p.0 == *name)
            .ok_or_else(|| {
                CompileError::from(format!(
                    "Can't find filter-map with specified name in this pack: {}",
                    name
                ))
            })
            .and_then(|p| {
                if let Ok(p) = p.1 {
                    Ok(p.into())
                } else {
                    Err(p.1.err().unwrap_or(CompileError::from(format!(
                        "Can't retrieve filter-map name: {} for this roto pack.",
                        name
                    ))))
                }
            })
    }

    /// Retrieves a pack by name, returns a Result over a pack that contains
    /// `&T` for every collection-type attribute: T inside the pack. An error
    /// indicates a mis-compilation for this Filter(Map).
    pub fn retrieve_pack_as_refs<'a>(
        &'a self,
        name: &'a Scope,
    ) -> Result<RotoPackRef, CompileError> {
        self.retrieve_pack::<RotoPackRef<'a>>(name)
    }

    /// Retrieves a pack by name, returns a Result over a pack that contains
    /// `Arc<T>` for every collection-type attribute: T inside the pack. An
    /// error indicates a mis-compilation for this Filter(Map).
    pub fn retrieve_pack_as_arcs<'a>(
        &'a self,
        name: &'a Scope,
    ) -> Result<RotoPackArc, CompileError> {
        self.retrieve_pack::<RotoPackArc<'a>>(name)
    }

    /// Retrieves the first pack in this rotolo, either a pack that contains
    /// `&T` for every collection-type attribute: T inside the pack, or an
    /// error indicating a mis-compilation for this Filter(Map).
    pub fn retrieve_first_pack_as_arcs(
        &self,
    ) -> Result<RotoPackArc, CompileError> {
        self.iter::<&RotoPack>()
            .take(1)
            .next()
            .ok_or_else(|| {
                CompileError::from(
                    "No filter-maps are available in this pack",
                )
            })
            .and_then(|p| {
                if let Ok(p) = p.1 {
                    Ok(p.into())
                } else {
                    Err(p.1.err().unwrap_or(CompileError::from(
                        "Can't find filter-map with specified name in this pack"
                    )))
                }
            })
    }

    pub fn compile_all_arguments(
        &self,
        mut args: HashMap<Scope, Vec<(&str, TypeValue)>>,
    ) -> Result<HashMap<Scope, FilterMapArgs>, CompileError> {
        let mut res = HashMap::<Scope, FilterMapArgs>::new();
        for pack in self.packs.iter() {
            let args = std::mem::take(
                args.get_mut(&pack.filter_map_name).ok_or_else(|| {
                    CompileError::Internal(format!(
                        "Cannot compile arguments: {:?}",
                        pack.filter_map_name
                    ))
                })?,
            );
            let cp = pack.arguments.compile_arguments(args);
            if let Ok(map) = cp {
                res.insert(pack.filter_map_name.clone(), map);
            }
        }

        Ok(res)
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

//------------ InternalPack -------------------------------------------------

/// A compiled Filter(Map)

/// RotoPacks are the public representation of the compiled packs, where the
/// collection-type fields can be `&T`, `Arc<T>`, or really any other
/// `T: AsRef<[T]>.`.
#[derive(Debug)]
pub struct InternalPack<
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
    pub hash_id: u64,
}

pub type RotoPackArc<'a> = InternalPack<
    'a,
    Arc<[MirBlock]>,
    Arc<[(&'a str, TypeDef)]>,
    Arc<[ExtDataSource]>,
>;

pub type RotoPackRef<'a> = InternalPack<
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
    > InternalPack<'a, M, A, EDS>
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

//------------ RotoPack -----------------------------------------------------

// The internal representation of a RotoPack, where all values are owned.

#[derive(Debug, Clone)]
pub struct RotoPack {
    filter_map_name: Scope,
    filter_type: FilterType,
    mir: Vec<MirBlock>,
    rx_type: TypeDef,
    tx_type: Option<TypeDef>,
    arguments: FilterMapArgs,
    data_sources: Vec<ExtDataSource>,
    hash_id: u64,
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
        // This hash is used to determine, whether reloading or replacing mir
        // code is actually necessary.
        let hash_id = compute_hash(&mir, &data_sources);

        RotoPack {
            filter_map_name,
            filter_type,
            mir,
            rx_type,
            tx_type,
            arguments,
            data_sources,
            hash_id,
        }
    }

    pub fn get_mir(&self) -> &[MirBlock] {
        self.mir.as_slice()
    }

    pub fn get_data_sources(&self) -> &[ExtDataSource] {
        self.data_sources.as_slice()
    }

    pub fn get_filter_map_name(&self) -> ShortString {
        (&self.filter_map_name).into()
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

// PublicRotoPack<&[MirBlock], &[(&'a str, TypeDef)], &[ExtDataSource]>

impl<'a> From<&'a RotoPack> for RotoPackArc<'a> {
    fn from(rp: &'a RotoPack) -> Self {
        RotoPackArc {
            filter_map_name: &rp.filter_map_name,
            mir: rp.mir.as_slice().into(),
            rx_type: rp.rx_type.clone(),
            tx_type: rp.tx_type.clone(),
            arguments: rp.arguments.inspect_arguments().into(),
            data_sources: rp.data_sources.clone().into(),
            filter_type: rp.filter_type,
            hash_id: rp.hash_id,
        }
    }
}

impl<'a> From<&'a RotoPack> for RotoPackRef<'a> {
    fn from(rp: &'a RotoPack) -> Self {
        RotoPackRef {
            filter_map_name: &rp.filter_map_name,
            mir: rp.mir.as_slice(),
            rx_type: rp.rx_type.clone(),
            tx_type: rp.tx_type.clone(),
            arguments: rp.arguments.inspect_arguments(),
            data_sources: rp.data_sources.clone(),
            filter_type: rp.filter_type,
            hash_id: rp.hash_id,
        }
    }
}

pub(crate) type Arguments<'a> = Vec<(Token, &'a Symbol, Vec<Command>)>;
pub(crate) type DataSources<'a> = Vec<(ShortString, &'a Symbol)>;
pub(crate) type Variables<'a> = Vec<(ShortString, &'a Symbol)>;
pub(crate) type TermSections<'a> = Vec<(ShortString, StackRefPos)>;
pub(crate) type ActionSections<'a> = Vec<(usize, StackRefPos)>;
pub(crate) type Term<'a> = &'a Symbol;
pub(crate) type Action<'a> = &'a Symbol;

//------------ CompilerState ------------------------------------------------

#[derive(Debug)]
pub(crate) struct CompilerState<'a> {
    cur_filter_map: &'a SymbolTable,
    /// the vec of symbols that hold all the variables that were referenced in
    /// the actions -> terms -> define chain in the source code, i.e. only
    /// actions that reference terms that reference variables that we're
    /// defined in the `Define` section are stored here (as symbols). These
    /// variables are guaranteed to be necessary, no matter how the code
    /// branches. The compiler will unconditionally compile these symbols and
    /// store them at the start of the MIR code.
    used_variables: Variables<'a>,
    /// map of variable tokens -> memory positions to use when reading a
    /// variable, filled by the compiler when compiling the `used_variables`.
    pub(crate) variable_ref_table: VariablesRefTable,
    used_data_sources: DataSources<'a>,
    /// The cache of all arguments, global or local with their associated
    /// symbol and code block to generate the retrieval of its value. Note
    /// that this cache cannot be used to figure out if a argument is defined
    /// in a particular scope. For the latter purpose the local_scope vec that
    /// is passed around from eval to nested eval method is used. When the
    /// compiler kicks in, this should already been all solved by the
    /// evaluator. Having said that each argument for each scope has a unique
    /// index, the first element in the tuples it stores is a
    /// Token::ActionArgument or Token::TermArgument. That token has *two*
    /// usize in it, one that corresponds to the ACtionSection or TermSection
    /// it was defined in (it is the value of the token of the section, which
    /// is an index from the enumeration of sections in the source code). The
    /// second usize is the index of the `with` argument of the enumeration of
    /// all the `with` arguments in that section. Currently only one is
    /// allowed, btw.
    pub(crate) used_arguments: Arguments<'a>,
    pub(crate) cur_mir_block: MirBlock,
    /// This holds the flattened stack-friendly data-structure for a variable,
    /// up until the current field index, if the variable is a scalar, then the
    /// field index is an empty (small)vec.
    pub(crate) cur_partial_variable: Option<CompiledVariable>,
    /// This keeps track of the depth in the original record, while compiling
    /// recursively.
    pub(crate) cur_record_depth: usize,
    /// This keeps track of the current field we are recursively compiling,
    pub(crate) cur_record_field_index: FieldIndex,
    /// The name of the Record we are currently (recursively) compiling.
    pub(crate) cur_record_field_name: Option<ShortString>,
    /// The type of the Record we are currently (recursively) compiling.
    pub(crate) cur_record_type: Option<RecordTypeDef>,
    /// the memory position that is currently empty and available to be
    /// written to.
    pub(crate) cur_mem_pos: u32,
    var_read_only: bool,
    compiled_terms: TermSections<'a>,
    compiled_action_sections: ActionSections<'a>,
}

impl<'a> CompilerState<'a> {
    pub(crate) fn push_command(&mut self, op: OpCode, args: Vec<CommandArg>) {
        if let Some(cur_rec_var) = &mut self.cur_partial_variable {
            match cur_rec_var.iter_mut().last() {
                Some(CompiledField::Primitive(p)) => {
                    if let Some(
                        CommandArg::ConstantValue(_)
                        | CommandArg::FieldIndex(_),
                    ) = args.first()
                    {
                        if let Some(_field_name) =
                            self.cur_record_field_name.clone()
                        {
                            cur_rec_var.append_primitive(
                                CompiledPrimitiveField::new(
                                    vec![Command::new(op, args.clone())],
                                    self.cur_record_field_index.clone(),
                                ),
                            )
                        };
                    } else {
                        p.push_command(op, args.clone());
                    }
                }
                Some(CompiledField::Collection(_)) | None => {
                    if let Some(_field_name) =
                        self.cur_record_field_name.clone()
                    {
                        cur_rec_var.append_primitive(
                            CompiledPrimitiveField::new(
                                vec![Command::new(op, args.clone())],
                                self.cur_record_field_index.clone(),
                            ),
                        );
                    }
                }
            }
        }
        trace!("after push cur_rec_var {:?}", self.cur_partial_variable);
        self.cur_mir_block
            .command_stack
            .push_back(Command::new(op, args));
        trace!("after push cur_mir_block {:?}", self.cur_mir_block);
    }

    pub(crate) fn extend_commands(&mut self, commands: Vec<Command>) {
        if let Some(cur_rec_var) = &mut self.cur_partial_variable {
            match cur_rec_var.iter_mut().last() {
                Some(CompiledField::Primitive(p)) => {
                    p.extend_commands(commands.clone());
                }
                Some(CompiledField::Collection(_)) | None => {
                    cur_rec_var.append_primitive(
                        CompiledPrimitiveField::new(
                            commands.clone(),
                            self.cur_record_field_index.clone(),
                        ),
                    );
                }
            }
        }
        self.cur_mir_block.extend(commands);
    }

    pub(crate) fn init_current_record_tracker_with_collection(&mut self) {
        let mut compile_var = CompiledVariable::new();
        compile_var
            .append_collection(CompiledCollectionField::new(vec![].into()));
        self.cur_partial_variable = Some(compile_var);
        self.cur_record_depth = 0;
        self.cur_record_field_index = vec![].into();
    }

    pub(crate) fn init_current_record_tracker_with_primitive(&mut self) {
        let mut compile_var = CompiledVariable::new();
        compile_var.append_primitive(CompiledPrimitiveField::new(
            vec![],
            vec![].into(),
        ));
        self.cur_partial_variable = Some(compile_var);
        self.cur_record_depth = 0;
        self.cur_record_field_index = vec![].into();
    }

    pub(crate) fn destroy_current_record_tracker(&mut self) {
        self.cur_partial_variable = None;
        self.cur_record_depth = 0;
        self.cur_record_field_index = vec![].into();
        self.cur_record_field_name = None;
    }

    pub(crate) fn inc_record_field_depth(
        &mut self,
        name: ShortString,
    ) -> Result<(), CompileError> {
        trace!("inc depth level");
        match self.cur_partial_variable {
            // Init the new record
            None => {
                self.cur_record_depth = 0;
                self.cur_record_field_index = vec![].into();
            }
            Some(_) => {
                self.cur_record_depth += 1;
            }
        }
        self.cur_record_field_name = Some(name);

        Ok(())
    }

    pub(crate) fn append_collection_to_record_tracker(
        &mut self,
        collection: CompiledCollectionField,
    ) {
        if let Some(var) = self.cur_partial_variable.as_mut() {
            var.append_collection(collection);
        }
    }
}

/// The `roto` compiler
///
/// The roto compiler is a single-stage, three phase compiler. it has
/// separate public methods for each phase, as well as a method that bundles
/// all phases.
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

                if _arg.ty == arg.1 {
                    _arg.value = arg.1.clone();
                } else {
                    return Err(
                        CompileError::from(
                            format!(
                                "Argument '{}' has the wrong type, expected '{}', got '{}'",
                                arg.0,
                                _arg.ty,
                                TypeDef::from(&arg.1),
                            )
                        )
                    );
                }
            }
        }

        Ok(())
    }

    pub fn compile(self) -> Result<Rotolo, CompileError> {
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
            let _filter_map = _global.get(&filter_map).ok_or_else(|| {
                CompileError::from(format!(
                    "Cannot find filter_map {}",
                    filter_map
                ))
            })?;
            let compile_result = compile_filter_map(
                _filter_map,
                _global.get(&Scope::Global).ok_or_else(|| {
                    CompileError::Internal("Cannot find global scope".into())
                })?,
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

        Ok(Rotolo {
            packs,
            mis_compilations,
        })
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
        self.compile()
    }
}

//------------ MirBlock & Mir -----------------------------------------------

#[derive(Debug)]
pub struct MirBlock {
    command_stack: VecDeque<Command>,
}

impl MirBlock {
    fn new() -> Self {
        MirBlock {
            command_stack: VecDeque::new(),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &Command> + '_ {
        self.command_stack.iter()
    }

    pub fn extend(&mut self, commands: Vec<Command>) {
        self.command_stack.extend(commands);
    }
}

impl std::hash::Hash for MirBlock {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for c in &self.command_stack {
            c.hash(state);
        }
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
                // let args = c.args.into_vec();
                Command {
                    op: c.op,
                    args: c.args.clone(),
                }
            })
            .collect::<VecDeque<_>>();

        MirBlock {
            // ty: MirBlockType::Alias,
            command_stack: c_stack,
        }
    }
}

// search basically everywhere in the current global state for the value that
// corresponds to the given token. Used to look up the enum instance in a
// match expression & generate the VM code to retrieve it & put it on the
// stack.
pub(crate) fn generate_code_for_token_value(
    state: &CompilerState,
    token: &Token,
) -> Vec<Command> {
    match token {
        Token::RxType(_) => {
            vec![Command::new(OpCode::PushStack, vec![CommandArg::MemPos(0)])]
        }
        Token::TxType => {
            vec![Command::new(OpCode::PushStack, vec![CommandArg::MemPos(1)])]
        }
        Token::Variable(var_to) => {
            if let Some(var) = state
                .used_variables
                .iter()
                .find(|(_, var)| { 
                    var.token.clone().try_into().is_ok_and(|var: usize| var == *var_to) })
            {
                vec![Command::new(
                    OpCode::PushStack,
                    vec![CommandArg::ConstantIndex(
                        var.1.value.clone(),
                    )],
                )]
            } else {
                vec![]
            }
        }
        Token::Method(_) => todo!(),
        Token::Variant(_) => todo!(),
        Token::Argument(_) => {
            if let Some((_, _, code_block)) =
                state.used_arguments.iter().find(|(to, _, _)| to == token)
            {
                code_block.clone()
            } else {
                vec![]
            }
        }
        Token::ActionArgument(_, _) => {
            if let Some((_, _, code_block)) =
                state.used_arguments.iter().find(|(to, _, _)| to == token)
            {
                code_block.clone()
            } else {
                vec![]
            }
        }
        Token::TermArgument(_, _) => {
            if let Some((_, _, code_block)) =
                state.used_arguments.iter().find(|(to, _, _)| to == token)
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
        Token::AnonymousEnum => todo!(),
        Token::NonTerminal => todo!(),
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
            .map(|(_n, s)| (s.token.clone(), s, vec![]))
            .collect::<Vec<_>>(),
        compiled_terms: TermSections::new(),
        compiled_action_sections: ActionSections::new(),
        cur_mir_block: MirBlock::new(),
        cur_record_depth: 0,
        cur_record_field_index: vec![].into(),
        cur_partial_variable: None,
        cur_record_field_name: None,
        cur_record_type: None,
        var_read_only: false,
        cur_mem_pos: 0,
    };

    // initialize the command stack
    let mut mir = vec![];
    if log_enabled!(Level::Trace) {
        trace!("___filter type");
        trace!("{:?}", filter_map.get_type());

        trace!("___used args");
        state
            .used_arguments
            .iter()
            .for_each(|s| trace!("{:?}: {:?}", s.1.token, s.0));

        trace!("___used vars");
        state.used_variables.iter().for_each(|s| {
            trace!("{:?} {:?}", s.1.token, s.0);
        });

        trace!("___used data_sources");
        state.used_data_sources.iter().for_each(|t| {
            trace!("{:?} {:?}", t.1.token, t.0);
        });

        trace!("___rx tx types");
        trace!("Rx {:?}", rx_type);
        trace!("Tx {:?}", tx_type);

        trace!("=================================================");
    }

    // compile the variables used in the terms
    state = compile_assignments(state)?;

    if state.cur_mem_pos == 0 { state.cur_mem_pos = 2 };
    (mir, state) = compile_apply_section(mir, state)?;

    state.cur_mir_block = MirBlock::new();
    state.push_command(
        OpCode::Exit(state.cur_filter_map.get_default_action()),
        vec![],
    );

    mir.push(state.cur_mir_block);

    trace!("\n");

    let args = state
        .used_arguments
        .iter_mut()
        .map(|a| {
            FilterMapArg::new(
                &a.1.name,
                a.1.token.clone(),
                a.1.ty.clone(),
                TypeValue::UnInit,
            )
        })
        .collect::<Vec<_>>()
        .into();

    // Lookup all the data sources in the global symbol table. The filter_map
    // table does not have (the right) type defs for data sources.
    let mut data_sources = vec![];
    for ds in state.used_data_sources {
        let name = &ds.1.name;
        let resolved_ds = if let Ok(ds) = global_table.get_data_source(name)
        {
            ds
        } else {
            return Err(CompileError::from(format!(
                "Cannot find data source '{}' in the Global Table",
                name
            )));
        };

        // W're only creating a data source with the name and token found
        // in the declaration in the source code. he actual source data
        // will only be needed at run-time
        data_sources.push(ExtDataSource::new(
            name,
            resolved_ds.1,
            resolved_ds.0,
        )?)
    }

    Ok(RotoPack::new(
        filter_map.get_scope(),
        filter_map.get_type()?,
        mir,
        rx_type.map_or(TypeDef::Unknown, |rx| rx.1),
        tx_type.map(|tx| tx.1),
        args,
        data_sources,
    ))
}

/// Compiles the variable assignments, creates a MirBlock that retrieves and/or
/// computes the value of the variable and stores it in the
/// `variables_ref_table` map. Note that in cases where the variables
/// assignments points to a field access, the access receiver of the assignment
/// is stored together with the field_index on the access receiver that points
/// to the actual variable assignment.
fn compile_assignments(
    mut state: CompilerState<'_>,
) -> Result<CompilerState<'_>, CompileError> {
    trace!("COMPILE ASSIGNMENTS");
    let _filter_map = state.cur_filter_map;

    // compile the used variable assignments. Since the rx and tx value live
    // in memory positions 0 and 1, we start with memory position 2 plus the
    // number of variables that we need to resolve.
    for (var_mem_pos, var) in state.used_variables.clone().iter().enumerate()
    {
        // set the mem_pos in state to the counter we use here. Recursive
        // `compile_expr` may increase state.mem_pos to temporarily store
        // argument variables. If we already have variables set here, then we
        // can just increase it, but if there are none, we're going to skip
        // over 0 and 1, since they should host the RxType and TxType
        // respectively.
        state.cur_mem_pos = u32::max(2, 1 + state.used_variables.len() as u32);
        trace!(
            "VAR {:?} MEM POS {} TEMP POS START {}",
            var.0,
            var_mem_pos + 2,
            state.cur_mem_pos
        );

        let s = _filter_map.get_variable_by_token(&var.1.token)?;

        for arg in &s.args {
            trace!(
                "arg {:?} {:?} {:?} {:?}",
                arg.name,
                arg.token,
                arg.ty,
                arg.kind
            );

            match &arg.ty {
                TypeDef::Record(_rec_type) => {
                    trace!("RECORD TYPE");
                    trace!("{:#?}", arg);
                    // reset the current variable
                    state.init_current_record_tracker_with_collection();

                    state.cur_record_type =
                        if let TypeDef::Record(rec_type) = &arg.ty {
                            Some(rec_type.clone())
                        } else {
                            None
                        };

                    state = recurse_compile(arg, state, None, false)?;

                    trace!("inserting {:#?}", var.1.token);
                    state.variable_ref_table.insert(
                        var.1.token.clone(),
                        state.cur_partial_variable.clone().ok_or_else(
                            || {
                                CompileError::Internal(format!(
                                    "Cannot compile variable: {:?}",
                                    var.1.name
                                ))
                            },
                        )?,
                    )?;
                }
                TypeDef::List(_list_type) => {
                    trace!("LIST TYPE");

                    // reset the current variable
                    state.init_current_record_tracker_with_primitive();

                    state = recurse_compile(arg, state, None, false)?;

                    trace!("inserting {:#?}", var.1.token);
                    state.variable_ref_table.insert(
                        var.1.token.clone(),
                        state.cur_partial_variable.clone().ok_or_else(
                            || {
                                CompileError::Internal(format!(
                                    "Cannot compile list: {:?}",
                                    var.1.name
                                ))
                            },
                        )?,
                    )?;
                }
                _ty => {
                    state.init_current_record_tracker_with_primitive();

                    state = recurse_compile(arg, state, None, false)?;

                    trace!("scalar type {}", _ty);
                    trace!("cur_mir_block {:?}", state.cur_mir_block);
                    trace!(
                        "cur_record_variable {:?}",
                        state.cur_partial_variable
                    );
                    state.variable_ref_table.set_primitive(
                        var.1.token.clone().try_into()?,
                        state
                            .cur_partial_variable
                            .clone()
                            .ok_or_else(|| {
                                CompileError::Internal(format!(
                                    "Cannot compile record: {:?}",
                                    var.1.name
                                ))
                            })?
                            .get_accumulated_commands(),
                        vec![].into(),
                    )?;
                }
            }
        }
    }

    state.destroy_current_record_tracker();
    trace!("local variables map");
    trace!("{:#?}", state.variable_ref_table);

    Ok(state)
}

fn compile_apply_section(
    mut mir: Vec<MirBlock>,
    mut state: CompilerState<'_>,
) -> Result<(Vec<MirBlock>, CompilerState), CompileError> {
    state.cur_mir_block = MirBlock::new();

    let match_action_sections =
        state.cur_filter_map.get_match_action_sections();

    trace!(
        "compiling MATCH ACTION SECTIONS {:#?}",
        match_action_sections
    );
    // Collect the terms that we need to compile.
    for match_action in match_action_sections {
        let ma_name = match_action.get_name();
        // let match_action = &match_action.symbol;

        match match_action.get_kind() {
            // A Pattern Match Action
            // similar to recurse_compile GlobalEnum handling
            symbols::SymbolKind::GlobalEnum => {
                trace!(
                    "compiling ENUM MATCH ACTION EXPRESSION {} {:?}",
                    match_action.get_name(),
                    match_action.get_kind_type_and_token()
                );

                // SSA and Ordered Types

                // This is my take on Static Single-Assignment form (SSA).
                // Although variables in Roto are immutable, pointing to
                // indexes of a Record or enum instance mutates the original
                // value (the record or enum instance): once a record is
                // indexed or an enum is unpacked into its variant and that
                // gets indexed, the original value is lost to the compiler
                // beyond that point, it simply can't know where to retrieve
                // the original value any more. So, we need to keep track of
                // each time the original value is referenced and supply the
                // compiler with a fresh copy of the original value. This is
                // what SSA is supposed to do, or, in other words, the MIR
                // language can only deal with Ordered Types (every variable
                // is of a unique type that can only be used once and in the
                // order of appearance). The solution is fairly easy, just
                // before one of these values is referenced, we put a block of
                // code in front of it that pushed the original value (from a
                // protected memory position) onto the stack. This is that
                // code for the action argument ('with' clause) of an action
                // section
                let enum_instance_code_block = generate_code_for_token_value(
                    &state,
                    &match_action.get_token(),
                );

                state.cur_mem_pos += 1;
                let orig_mem_pos = state.cur_mem_pos;

                state.cur_mir_block.command_stack.push_back(Command::new(
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
                        variant.name,
                        match_action.get_name()
                    );
                    // The variant here is a symbol that must be of kind
                    // EnumVariant, must have a Token::Variant as its token.
                    assert_eq!(
                        variant.kind,
                        symbols::SymbolKind::EnumVariant
                    );
                    state.push_command(
                        OpCode::Label,
                        vec![CommandArg::Label(
                            format!("VARIANT {}", variant.name)
                                .as_str()
                                .into(),
                        )],
                    );

                    // Unpack the enum instance into the variant that we're
                    // handling currently. StackUnPackAsVariant will set
                    // TypeValue::Unknown if this is not the variant we're
                    // looking for at runtime.
                    if let Token::Variant(variant_index) = variant.token
                    {
                        // if this is *not* the first variant we first want
                        // to pop the top of the stack, since that will
                        // contain the bool value of the last variant
                        // evaluation, and we don't want that. We want the
                        // enum instance underneath it.
                        if first_variant_done {
                            state.push_command(OpCode::PopStack, vec![]);
                        }
                        state.push_command(
                            OpCode::StackIsVariant,
                            vec![CommandArg::Variant(variant_index)],
                        );
                        state.push_command(
                            OpCode::CondFalseSkipToEOB,
                            vec![CommandArg::Variant(variant_index)],
                        );
                        // If we're continuing then remove the variant
                        // boolean indication.
                        state.push_command(OpCode::PopStack, vec![]);
                    } else {
                        return Err(CompileError::from(format!(
                            "Expected an Enum Variant, but got a {:?}",
                            variant.token
                        )));
                    }

                    // the args of this variant symbol must be of kind
                    // ActionCall and, all have tokens
                    // Token::ActionSection or Token::NoAction.

                    for action_section in &variant.args {
                        // Check if the user defined a guard for this variant

                        // GUARD START ---------------------------------------

                        if action_section.kind == SymbolKind::TermCall {
                            trace!("found guard");
                            // See if it was already compiled earlier on.
                            let term_name = state
                                .compiled_terms
                                .iter()
                                .find(|t| t.0 == action_section.name);

                            match term_name {
                                // yes, it was, create a reference to the
                                // result on the stack
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
                                        action_section.name.clone()
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
                                            t.name
                                                == action_section.name
                                        })
                                        .ok_or_else(|| {
                                            CompileError::Internal(format!(
                                                "Cannot compile terms: {:?}",
                                                terms
                                            ))
                                        })?;
                                    trace!("term {:#?}", term);
                                    state = compile_term_section(
                                        term,
                                        state,
                                        &enum_instance_code_block,
                                    )?;

                                    state.push_command(
                                        OpCode::CondFalseSkipToEOB,
                                        vec![],
                                    );
                                    trace!("compile_term_section");
                                    trace!("{:#?}", state.cur_mir_block);

                                    // store the resulting value into a variable so that future references
                                    // to this term can directly use the result instead of doing the whole
                                    // computation again.
                                    state.compiled_terms.push((
                                        action_section.name.clone(),
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
                            action_section.ty
                                == TypeDef::AcceptReject(
                                    AcceptReject::Accept
                                )
                                || action_section.ty
                                    == TypeDef::AcceptReject(
                                        AcceptReject::Reject
                                    )
                                || action_section.ty
                                    == TypeDef::AcceptReject(
                                        AcceptReject::NoReturn
                                    )
                        );
                        assert_eq!(
                            action_section.kind,
                            symbols::SymbolKind::ActionCall
                        );

                        match action_section.token {
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
                                            state.extend_commands(vec![
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
                                                t.token
                                                    == Token::ActionSection(
                                                        as_id
                                                    )
                                            })
                                            .ok_or_else(||
                                                CompileError::Internal(format!(
                                                    "Cannot compile action section: {:?}", as_name
                                                ))
                                            )?;
                                        state = compile_action_section(
                                            a_s,
                                            &enum_instance_code_block,
                                            state,
                                        )?;

                                        // store the resulting value into a
                                        // variable so that future references
                                        // to this action section can directly
                                        // use the result instead of doing the
                                        // whole computation again.
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
                                return Err(CompileError::Internal(
                                    format!("No token found for action section: '{}'", action_section.name)
                                ));
                            }
                        };

                        let accept_reject = if let TypeDef::AcceptReject(
                            accept_reject,
                        ) = action_section.ty
                        {
                            accept_reject
                        } else {
                            return Err(CompileError::Internal(
                                format!("No Accept or Reject found in action section: '{}'", action_section.name)
                            ));
                        };

                        // Add an early return if the type of the match
                        // action is either `Reject` or
                        // `Accept`.
                        if accept_reject != AcceptReject::NoReturn {
                            state.push_command(
                                OpCode::Exit(accept_reject),
                                vec![],
                            );
                        }
                    }

                    // move the current mir block to the end of all the collected MIR.
                    mir.push(state.cur_mir_block);

                    // continue with a fresh block
                    state.cur_mir_block = MirBlock::new();
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
                            state.extend_commands(vec![
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
                            .find(|t| t.name == ma_name)
                            .ok_or_else(|| {
                                CompileError::Internal(format!(
                                    "Cannot compile terms: {:?}",
                                    ma_name
                                ))
                            })?;

                        state.cur_mir_block = MirBlock::new();
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
                state.cur_mir_block = MirBlock::new();

                match ma {
                    MatchActionType::Filter => {
                        state.extend_commands(vec![
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
                    MatchActionType::Negate => {
                        state.extend_commands(vec![
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
                }

                state = compile_match_action(match_action.get_match_action(), vec![], state)?;
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
            state.push_command(OpCode::Exit(accept_reject), vec![]);
        }

        // move the current mir block to the end of all the collected MIR.
        mir.push(state.cur_mir_block);

        // continue with a fresh block
        state.cur_mir_block = MirBlock::new();
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
    for action_call in &match_action.args {
        let filter_map = state.cur_filter_map;
        let action_name = &action_call.name;

        let accept_reject = match action_call.ty {
            // A FilterMatchAction should only appear here with a
            // AcceptReject value as its type
            TypeDef::AcceptReject(accept_reject) => accept_reject,
            // An PatternMatchAction has as its type Enum, which doesn't have
            // an AcceptReject.
            TypeDef::GlobalEnum(_) => AcceptReject::NoReturn,
            _ => {
                return Err(CompileError::from(format!(
                    "Cannot convert `{}` with type {} into match action type",
                    action_call.name,
                    action_call.ty
                )));
            }
        };

        if let Some(action) = filter_map
            .get_action_sections()
            .iter()
            .find(|t| t.name == action_name)
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
            state.push_command(OpCode::Exit(accept_reject), vec![]);
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
    trace!("compiling ACTION SECTION {}...", action_section.name);

    for action in &action_section.args {
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
    match action.kind {
        // A symbol with an RxType token, should be an access receiver.
        SymbolKind::AccessReceiver => {
            trace!("compiling ACTION {:#?}", action);
            state = recurse_compile(action, state, None, false)?;
            state.cur_mem_pos += 1;
        }
        // Variable arguments that are passed in into an action appear as
        // Constants in the args of the ActionSection, so they end up
        // here.
        SymbolKind::Argument => {
            state.used_arguments.push((
                action.token.clone(),
                action,
                argument_code_block.to_vec(),
            ));
        }
        _ => {
            trace!("Faulty ACTION {:#?}", action);
            return Err(CompileError::new(format!(
                "Invalid action {}",
                action.name
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
    trace!("TERM SECTION {}", term_section.name);

    // Push the code block that retrieves the argument for this section to
    // the cache.
    if let Some(s) = term_section.args.first() {
        state.used_arguments.push((
            s.token.clone(),
            s,
            argument_code_block.to_vec(),
        ));
    }

    state.push_command(
        OpCode::Label,
        vec![CommandArg::Label(
            format!("TERM SECTION {}", term_section.name)
                .as_str()
                .into(),
        )],
    );

    let terms = &term_section.args;
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
                .push_back(Command::new(OpCode::CondFalseSkipToEOB, vec![]));
        }
    }

    Ok(state)
}

pub(crate) fn compile_term<'a>(
    term: Term<'a>,
    mut state: CompilerState<'a>,
) -> Result<CompilerState<'a>, CompileError> {
    let saved_mem_pos = state.cur_mem_pos;

    match term.kind {
        SymbolKind::CompareExpr(op) => {
            let args = term.get_args_checked(2)?;
            state = recurse_compile(&args[0], state, None, false)?;
            state.cur_mem_pos += 1;
            state = recurse_compile(&args[1], state, None, false)?;

            state
                .cur_mir_block
                .command_stack
                .push_back(Command::new(OpCode::Cmp, vec![op.into()]));
        }
        SymbolKind::OrExpr => {
            let args = term.get_args_checked(2)?;
            state = compile_term(&args[0], state)?;
            state.cur_mem_pos += 1;
            state = compile_term(&args[1], state)?;

            state.cur_mir_block.command_stack.push_back(Command::new(
                OpCode::Cmp,
                vec![CommandArg::CompareOp(ast::CompareOp::Or)],
            ));
        }
        SymbolKind::AndExpr => {
            let args = term.get_args_checked(2)?;
            state = compile_term(&args[0], state)?;
            state.cur_mem_pos += 1;
            state = compile_term(&args[1], state)?;

            state.push_command(
                OpCode::Cmp,
                vec![CommandArg::CompareOp(ast::CompareOp::And)],
            );
        }
        SymbolKind::NotExpr => {
            todo!();
        }
        SymbolKind::ListCompareExpr(op) => {
            trace!("list compare");
            // Retrieve all the arguments and check that the length is at
            // least 2, so that we do not run afoul with indexing later on.
            let args = term.get_args_with_checked_min_len(2)?;
            state.cur_mem_pos += 1;
            let orig_mem_pos = state.cur_mem_pos;

            for arg in &args[1..] {
                // retrieve the left hand assignment and put it on the stack
                state = recurse_compile(&args[0], state, None, false)?;
                state.cur_mem_pos += 1;

                // retrieve the next value from the right hand list
                state = recurse_compile(arg, state, None, false)?;

                state.cur_mir_block.command_stack.push_back(Command::new(
                    OpCode::Cmp,
                    vec![CommandArg::CompareOp(op)],
                ));

                state.cur_mir_block.command_stack.push_back(Command::new(
                    OpCode::CondTrueSkipToEOB,
                    vec![],
                ));

                // restore old mem_pos, let's not waste memory
                state.cur_mem_pos = orig_mem_pos;
            }
        }
        SymbolKind::GlobalEnum => {
            trace!(
                "ENUM TERM EXPRESSION {} {:?}",
                term.name,
                term.get_kind_type_and_token()
            );

            let variants = &term.args;
            state.cur_mem_pos += 1;
            let orig_mem_pos = state.cur_mem_pos;

            state.cur_mir_block.command_stack.push_back(Command::new(
                OpCode::Label,
                vec![CommandArg::Label(
                    format!("ENUM {}", term.name).as_str().into(),
                )],
            ));

            trace!("enum token {:?}", term.token);

            let mut variants = variants.iter().peekable();
            while let Some(variant) = &mut variants.next() {
                state.cur_mir_block.command_stack.push_back(Command::new(
                    OpCode::Label,
                    vec![CommandArg::Label(
                        format!("VARIANT_{}", variant.name)
                            .as_str()
                            .into(),
                    )],
                ));

                // push the enum instance to the stack.
                state.cur_mir_block.extend(generate_code_for_token_value(
                    &state,
                    &term.token,
                ));

                if let Token::Variant(variant_index) = variant.token {
                    state.cur_mir_block.command_stack.push_back(
                        Command::new(
                            OpCode::StackIsVariant,
                            vec![CommandArg::Variant(variant_index)],
                        ),
                    );

                    // The next label should be next variant, so if this is
                    // not the right variant, jump there.
                    state.cur_mir_block.command_stack.push_back(
                        Command::new(
                            OpCode::CondFalseSkipToLabel,
                            vec![CommandArg::Variant(variant_index)],
                        ),
                    );

                    // Ok, we're continuing, so pop the StackIsVariant value
                    // from the stack.
                    state
                        .cur_mir_block
                        .command_stack
                        .push_back(Command::new(OpCode::PopStack, vec![]));
                }

                // compile term blocks for each variant.
                state = recurse_compile(variant, state, None, false)?;

                if variants.peek().is_some() {
                    // Do not spill over into the next variant, go to the end
                    // of the block.
                    state
                        .cur_mir_block
                        .command_stack
                        .push_back(Command::new(OpCode::SkipToEOB, vec![]));
                }
            }

            // restore old mem_pos, let's not waste memory
            state.cur_mem_pos = orig_mem_pos;
        }
        _ => {
            trace!(
                "RESIDUAL TERM EXPRESSION {} {:?}",
                term.name,
                term.get_kind_type_and_token()
            );
            state = recurse_compile(term, state, None, false)?;
        }
    };

    // restore old mem_pos, let's not waste memory
    state.cur_mem_pos = saved_mem_pos;

    Ok(state)
}
