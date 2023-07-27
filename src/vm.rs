use std::{
    cell::RefCell,
    fmt::{Display, Formatter},
    ops::{Index, IndexMut},
    sync::Arc,
};

use crate::{
    ast::{self, AcceptReject, CompareOp, ShortString},
    compile::{CompileError, MirBlock},
    traits::{RotoType, Token},
    types::{
        builtin::{Boolean, BuiltinTypeValue},
        collections::{
            BytesRecord, ElementTypeValue, LazyRecord, List, Record,
        },
        datasources::{DataSource, DataSourceMethodValue},
        outputs::OutputStreamMessage,
        typedef::TypeDef,
        typevalue::TypeValue,
    },
};

use arc_swap::ArcSwapOption;
use log::{log_enabled, trace, Level};
use smallvec::SmallVec;

//------------ Stack --------------------------------------------------------

#[derive(Debug, Clone)]
pub enum StackRefPos {
    // index into LinearMemory
    MemPos(u32),
    // index into a Table (which is a vec of shared Records)
    TablePos(Token, usize),
    // CompareResult, which is not a Ref at all, but hey,
    // it's smaller that a ref, so who cares
    CompareResult(bool),
}

impl From<u32> for StackRefPos {
    fn from(mem_pos: u32) -> Self {
        StackRefPos::MemPos(mem_pos)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct StackRef {
    pub(crate) pos: StackRefPos,
    // nested field -> record.field sequences are expressed as a Vec of field
    // indexes, e.g. [1,2] on
    // Record { a: U8, b: Record { a: U8, b: U8, c: U8 }} would point to a.b
    field_index: SmallVec<[usize; 8]>,
}

#[derive(Debug)]
struct Stack(Vec<StackRef>);

impl<'a> Stack {
    fn new() -> Self {
        Stack(Vec::new())
    }

    fn push(&'a mut self, pos: StackRefPos) -> Result<(), VmError> {
        self.0.push(StackRef {
            pos,
            field_index: smallvec::smallvec![],
        });
        Ok(())
    }

    fn pop(&'a mut self) -> Result<StackRef, VmError> {
        self.0.pop().ok_or(VmError::StackUnderflow)
    }

    fn get_top_value(&'a self) -> Result<&StackRef, VmError> {
        self.0.last().ok_or(VmError::StackUnderflow)
    }

    fn add_field_index(&mut self, index: usize) -> Result<(), VmError> {
        self.0
            .last_mut()
            .ok_or(VmError::StackUnderflow)?
            .field_index
            .push(index);
        Ok(())
    }

    fn unwind(&mut self) -> Vec<StackRef> {
        std::mem::take(&mut self.0)
    }

    fn clear(&mut self) {
        self.0.clear();
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum StackValue<'a> {
    Ref(&'a TypeValue),
    Arc(Arc<TypeValue>),
    Owned(TypeValue),
}

impl<'a> AsRef<TypeValue> for StackValue<'a> {
    fn as_ref(&self) -> &TypeValue {
        match self {
            StackValue::Ref(r) => r,
            StackValue::Arc(r) => r,
            StackValue::Owned(r) => r,
        }
    }
}

// #[derive(Debug, Eq, PartialEq, PartialOrd, Ord)]
// pub struct StackValueRef<SV>(SV);

//------------ LinearMemory -------------------------------------------------

#[derive(Debug)]
pub struct LinearMemory([TypeValue; 512]);

impl LinearMemory {
    pub fn uninit() -> Self {
        // LinearMemory(std::array::from_fn(|_| TypeValue::UnInit))
        const V: TypeValue = TypeValue::UnInit;
        LinearMemory([V; 512])
    }

    pub fn get_mem_pos(&self, index: usize) -> Option<&TypeValue> {
        self.0.get(index)
    }

    pub(crate) fn get_mp_field_as_bool(&self, stack_ref: &StackRef) -> bool {
        trace!("mp field: {:?}", stack_ref.pos);
        match stack_ref.pos {
            StackRefPos::MemPos(pos) => {
                if let StackValue::Owned(TypeValue::Builtin(
                    BuiltinTypeValue::Boolean(Boolean(b)),
                )) = self
                    .get_mp_field_by_index_as_stack_value(
                        pos as usize,
                        stack_ref.field_index.clone(),
                    )
                    .unwrap()
                {
                    b
                } else {
                    false
                }
            }
            StackRefPos::CompareResult(res) => res,
            _ => false,
        }
    }

    pub(crate) fn get_mp_field_by_stack_ref_owned(
        &mut self,
        stack_ref: &StackRef,
    ) -> Result<TypeValue, VmError> {
        let StackRef {
            pos: stack_ref_pos,
            field_index,
        } = stack_ref;

        match stack_ref_pos {
            StackRefPos::MemPos(pos) => match field_index {
                v if v.is_empty() => self
                    .get_mem_pos_as_owned(*pos as usize)
                    .ok_or(VmError::MemOutOfBounds),
                field_index => match self.get_mem_pos_as_owned(*pos as usize)
                {
                    Some(TypeValue::Record(mut r)) => {
                        let field =
                            r.get_field_by_index_owned(field_index.clone());
                        match field {
                            ElementTypeValue::Nested(nested) => Ok(*nested),
                            ElementTypeValue::Primitive(b) => Ok(b),
                        }
                    }
                    Some(TypeValue::List(mut l)) => {
                        let field =
                            l.get_field_by_index_owned(field_index.clone());
                        match field {
                            Some(ElementTypeValue::Nested(nested)) => {
                                Ok(*nested)
                            }
                            Some(ElementTypeValue::Primitive(b)) => Ok(b),
                            _ => Err(VmError::MemOutOfBounds),
                        }
                    }
                    _ => Err(VmError::MemOutOfBounds),
                },
            },
            StackRefPos::CompareResult(res) => Ok((*res).into()),
            _ => Err(VmError::MemOutOfBounds),
        }
    }

    pub fn get_mp_field_by_index_as_stack_value(
        &self,
        index: usize,
        field_index: SmallVec<[usize; 8]>,
    ) -> Option<StackValue> {
        match field_index {
            fi if fi.is_empty() => {
                self.get_mem_pos(index).map(StackValue::Ref)
            }
            field_index => {
                match self.get_mem_pos(index) {
                    Some(TypeValue::Record(rec)) => {
                        match rec.get_field_by_index(field_index) {
                            Some(ElementTypeValue::Nested(nested)) => {
                                Some(StackValue::Ref(nested))
                            }
                            Some(ElementTypeValue::Primitive(b)) => {
                                Some(StackValue::Ref(b))
                            }
                            _ => None,
                        }
                    }
                    Some(TypeValue::List(l)) => {
                        let field = l.get_field_by_index(field_index);
                        match field {
                            Some(ElementTypeValue::Nested(nested)) => {
                                Some(StackValue::Ref(nested))
                            }
                            Some(ElementTypeValue::Primitive(b)) => {
                                Some(StackValue::Ref(b))
                            }
                            _ => None,
                        }
                    }
                    Some(TypeValue::Builtin(BuiltinTypeValue::Route(
                        route,
                    ))) => {
                        if let Some(v) =
                            route.get_value_ref_for_field(field_index[0])
                        {
                            Some(StackValue::Ref(v))
                        } else if let Some(v) =
                            route.get_field_by_index(field_index[0])
                        {
                            Some(StackValue::Owned(v))
                        } else {
                            Some(StackValue::Owned(TypeValue::Unknown))
                        }
                    }
                    Some(TypeValue::Builtin(
                        BuiltinTypeValue::BgpUpdateMessage(bgp_msg),
                    )) => {
                        trace!("get bgp_update_message get_value_owned_for_field {:?} {:?}", bgp_msg, field_index);
                        if let Some(v) = (*bgp_msg.as_ref())
                            .get_value_owned_for_field(field_index[0])
                        {
                            trace!("v {:?}", v);
                            Some(StackValue::Owned(v))
                        } else {
                            Some(StackValue::Owned(TypeValue::Unknown))
                        }
                    }
                    Some(TypeValue::Builtin(
                        BuiltinTypeValue::BmpRouteMonitoringMessage(bmp_msg),
                    )) => {
                        trace!("get bmp_route_monitoring_message get_value_owned_for_field {:?} {:?}", bmp_msg, field_index);

                        LazyRecord::from_type_def(BytesRecord::<
                            routecore::bmp::message::RouteMonitoring<
                                bytes::Bytes,
                            >
                        >::lazy_type_def(
                        ))
                        .get_field_by_index(&field_index, bmp_msg.as_ref())
                        .map(|elm| StackValue::Owned(elm.into()))
                    }
                    Some(tv) => match tv {
                        // Do not own AsPath and Communities, cloning is expensive!
                        TypeValue::Builtin(BuiltinTypeValue::AsPath(_)) => {
                            Some(StackValue::Ref(tv))
                        }
                        TypeValue::Builtin(
                            BuiltinTypeValue::Communities(_),
                        ) => Some(StackValue::Ref(tv)),
                        // Clone all other builtins, they're cheap to clone (all copy) and the
                        // result is smaller than a pointer
                        TypeValue::Builtin(_) => {
                            Some(StackValue::Owned(tv.clone()))
                        }
                        _ => Some(StackValue::Ref(tv)),
                    },
                    // This is apparently a type that does not have fields
                    None => None,
                }
            }
        }
    }

    fn get_mem_pos_as_owned(&mut self, index: usize) -> Option<TypeValue> {
        self.0.get_mut(index).map(std::mem::take)
    }

    fn set_mem_pos(&mut self, index: usize, value: TypeValue) {
        *self.0.get_mut(index).unwrap() = value;
    }
}

impl Index<usize> for LinearMemory {
    type Output = TypeValue;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl IndexMut<usize> for LinearMemory {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}

impl Display for LinearMemory {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:?}", self.0)
    }
}

//------------ Command Arguments Stack --------------------------------------

// A stack especially for the aguments to a Command. The Vec that forms the
// stack is immutable, so that the VM doesn't need to clone the MIR code for
// each run. A counter keeps track of the current position in the stack.

#[derive(Debug)]
pub(crate) struct CommandArgsStack<'a> {
    args: &'a Vec<CommandArg>,
    args_counter: usize,
}

impl<'a> CommandArgsStack<'a> {
    // The counter counts down from the last element + 1.
    fn new(args: &'a Vec<CommandArg>) -> Self {
        Self {
            args,
            args_counter: args.len(),
        }
    }

    // Return the last item and decrement the counter.
    fn pop(&mut self) -> Option<&'_ CommandArg> {
        self.args_counter -= 1;
        self.args.get(self.args_counter)
    }

    fn is_empty(&self) -> bool {
        self.args.is_empty()
    }

    // Interpret the last stack entry as a constant value.
    pub(crate) fn take_arg_as_constant(
        &mut self,
    ) -> Result<TypeValue, VmError> {
        // the first arg is the memory position index,
        // the second arg is the value to set on the
        // memory position.
        match self.args.get(1) {
            Some(CommandArg::Constant(c)) => Ok(c.clone()),
            Some(CommandArg::List(l)) => Ok(TypeValue::List(l.clone())),
            Some(CommandArg::Record(r)) => Ok(TypeValue::Record(r.clone())),
            _ => Err(VmError::InvalidValueType),
        }
    }

    // Pop two arguments, the stack is gone after this.
    pub(crate) fn pop_2(mut self) -> (&'a CommandArg, &'a CommandArg) {
        self.args_counter -= 2;
        (
            self.args.get(self.args_counter + 1).unwrap(),
            self.args.get(self.args_counter).unwrap(),
        )
    }

    // Pop three arguments, the stack is gone after this.
    pub(crate) fn pop_3(
        mut self,
    ) -> (&'a CommandArg, &'a CommandArg, &'a CommandArg) {
        self.args_counter -= 3;
        (
            self.args.get(self.args_counter + 2).unwrap(),
            self.args.get(self.args_counter + 1).unwrap(),
            self.args.get(self.args_counter).unwrap(),
        )
    }
}

impl<'a> Index<usize> for CommandArgsStack<'a> {
    type Output = CommandArg;

    fn index(&self, index: usize) -> &Self::Output {
        self.args.index(index)
    }
}

impl<'a> From<&'a Vec<CommandArg>> for CommandArgsStack<'a> {
    fn from(value: &'a Vec<CommandArg>) -> Self {
        Self {
            args: value,
            args_counter: 0,
        }
    }
}

//------------ Argument -----------------------------------------------------

// These are the filter-map-level arguments, they can be compiled in when passed
// in before compiling (`with_arguments()`), or they can be provided at run-
// time.

#[derive(Debug)]
pub struct FilterMapArg {
    pub(crate) name: ShortString,
    index: usize,
    ty: TypeDef,
    value: TypeValue,
}

impl FilterMapArg {
    pub fn get_value(&self) -> &TypeValue {
        &self.value
    }

    pub fn take_value(&mut self) -> TypeValue {
        std::mem::take(&mut self.value)
    }

    pub fn get_name(&self) -> &str {
        self.name.as_str()
    }

    pub fn get_type(&self) -> TypeDef {
        self.ty.clone()
    }

    pub fn get_index(&self) -> usize {
        self.index
    }

    pub(crate) fn new(
        name: &str,
        index: usize,
        ty: TypeDef,
        value: TypeValue,
    ) -> Self {
        Self {
            name: name.into(),
            index,
            ty,
            value,
        }
    }
}

#[derive(Default, Debug)]
pub struct FilterMapArgs(Vec<FilterMapArg>);

impl FilterMapArgs {
    pub fn compile_arguments(
        &self,
        args: Vec<(&str, TypeValue)>,
    ) -> Result<FilterMapArgs, CompileError> {
        // Walk over all the filter_map arguments that were supplied and see if
        // they match up with the ones in the source code.
        let mut arguments_map = FilterMapArgs::new();
        let len = args.len();
        for supplied_arg in args {
            match self.iter().find(|a| supplied_arg.0 == a.get_name()) {
                // The argument is in the source code
                Some(found_arg) => {
                    // nice, but do the types match?
                    if found_arg.get_type() == supplied_arg.1 {
                        // yes, they match
                        arguments_map.insert(
                            found_arg.get_name(),
                            found_arg.get_index(),
                            found_arg.get_type(),
                            supplied_arg.1,
                        )
                    } else {
                        // Ok, but maybe we can convert into the type we
                        // need? Note that we can only try to convert if
                        // it's a builtin type.
                        match supplied_arg
                            .1
                            .into_builtin()
                            .and_then(|t| t.into_type(&found_arg.get_type()))
                        {
                            Ok(arg) => arguments_map.insert(
                                found_arg.get_name(),
                                found_arg.get_index(),
                                found_arg.get_type(),
                                arg,
                            ),
                            Err(_) => {
                                return Err(format!("An invalid type was specified for argument: {}", supplied_arg.0).into());
                            }
                        };
                    }
                }
                // The supplied argument is not in the source code.
                None => {
                    return Err(format!(
                        "Can't find argument in source: {}",
                        supplied_arg.0
                    )
                    .into())
                }
            }
        }

        // See if we got all the required arguments in the source code
        // covered.
        if arguments_map.len() != len {
            let missing_args = self
                .iter()
                .filter(|a| {
                    arguments_map.get_by_token_value(a.get_index()).is_none()
                })
                .map(|a| a.get_name())
                .collect::<Vec<&str>>();

            return Err(format!(
                "Some arguments are missing: {:?}",
                missing_args
            )
            .into());
        }

        Ok(arguments_map)
    }

    pub fn inspect_arguments(&self) -> Vec<(&str, TypeDef)> {
        self.iter()
            .map(|a| (a.get_name(), a.get_type()))
            .collect::<Vec<_>>()
    }

    pub fn take_value_by_token(
        &mut self,
        index: usize,
    ) -> Result<TypeValue, VmError> {
        self.0
            .iter_mut()
            .find(|a| a.get_index() == index)
            .map(|a| a.take_value())
            .ok_or(VmError::AnonymousArgumentNotFound)
    }

    pub fn get_by_token_value(&self, index: usize) -> Option<&TypeValue> {
        self.0
            .iter()
            .find(|a| a.get_index() == index)
            .map(|a| a.get_value())
    }

    fn _take_by_index(mut self, index: usize) -> Option<TypeValue> {
        self.0.get_mut(index).map(|a| a.take_value())
    }

    pub fn new() -> Self {
        FilterMapArgs(Vec::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn insert(
        &mut self,
        name: &str,
        index: usize,
        ty: TypeDef,
        value: TypeValue,
    ) {
        self.0.push(FilterMapArg {
            name: name.into(),
            index,
            ty,
            value,
        });
    }

    pub fn last_index(&self) -> Option<usize> {
        if self.0.is_empty() {
            Some(self.0.len())
        } else {
            None
        }
    }

    pub fn iter(&self) -> std::slice::Iter<'_, FilterMapArg> {
        self.0.iter()
    }
}

impl IntoIterator for FilterMapArgs {
    type IntoIter = std::vec::IntoIter<Self::Item>;
    type Item = FilterMapArg;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl From<Vec<FilterMapArg>> for FilterMapArgs {
    fn from(value: Vec<FilterMapArg>) -> Self {
        Self(value)
    }
}

//------------ Variables ----------------------------------------------------
#[derive(Debug)]
pub struct VariableRef {
    pub var_token_value: usize,
    pub mem_pos: u32,
    pub field_index: SmallVec<[usize; 8]>,
}

#[derive(Debug, Default)]
pub struct VariablesMap(Vec<VariableRef>);

impl VariablesMap {
    pub fn get_by_token_value(&self, index: usize) -> Option<&VariableRef> {
        self.0.iter().find(
            |VariableRef {
                 var_token_value: t, ..
             }| { t == &index },
        )
    }

    pub fn set(
        &mut self,
        var_token_value: usize,
        mem_pos: u32,
        field_index: SmallVec<[usize; 8]>,
    ) -> Result<(), CompileError> {
        self.0.push(VariableRef {
            var_token_value,
            mem_pos,
            field_index,
        });
        Ok(())
    }

    pub fn new() -> Self {
        VariablesMap(Vec::new())
    }
}

//------------ Virtual Machine ----------------------------------------------

pub struct VirtualMachine<MB: AsRef<[MirBlock]>, EDS: AsRef<[ExtDataSource]>>
{
    // _rx_type: TypeDef,
    // _tx_type: Option<TypeDef>,
    mir_code: MB,
    data_sources: EDS,
    arguments: FilterMapArgs,
    stack: RefCell<Stack>,
}

impl<'a, MB: AsRef<[MirBlock]>, EDS: AsRef<[ExtDataSource]>>
    VirtualMachine<MB, EDS>
{
    fn _move_rx_tx_to_mem(
        &'a mut self,
        rx: impl RotoType,
        tx: Option<impl RotoType>,
        mem: &mut LinearMemory,
    ) {
        let rx = rx.take_value();
        mem.set_mem_pos(0, rx.into());

        if let Some(tx) = tx {
            let tx = tx.take_value();
            mem.set_mem_pos(1, tx.into());
        }
    }

    fn _unwind_resolved_stack_into_vec(
        &'a self,
        mem: &'a LinearMemory,
    ) -> Vec<StackValue> {
        let stack = self.stack.borrow_mut().unwind();
        let mut unwind_stack: Vec<StackValue> =
            Vec::with_capacity(stack.len());
        for sr in stack.into_iter() {
            match sr.pos {
                StackRefPos::MemPos(pos) => {
                    let v = mem
                        .get_mp_field_by_index_as_stack_value(
                            pos as usize,
                            sr.field_index,
                        )
                        .unwrap_or_else(|| {
                            panic!(
                                "Uninitialized memory in position {}",
                                pos
                            );
                        });
                    unwind_stack.push(v);
                }
                StackRefPos::TablePos(token, pos) => {
                    let ds = &self.data_sources.as_ref()[token];
                    match ds.get_at_field_index(pos, sr.field_index) {
                        Some(v) => {
                            unwind_stack.push(StackValue::Arc(v.into()))
                        }
                        None => unwind_stack
                            .push(StackValue::Owned(TypeValue::Unknown)),
                    }
                }
                StackRefPos::CompareResult(res) => {
                    unwind_stack.push(StackValue::Owned(res.into()))
                }
            }
        }
        unwind_stack
    }

    // Take a `elem_num` elements on the stack and flush the rest, so we'll
    // end up with an empty stack.
    fn _take_resolved_and_flush(
        &'a self,
        elem_num: u32, // number of elements to take
        mem: &'a LinearMemory,
    ) -> Vec<StackValue> {
        let mut stack = self.stack.borrow_mut();

        let len = stack.0.len();
        let stack_part = stack.0.split_off(len - elem_num as usize);

        let take_vec = stack_part
            .iter()
            .map(|sr| match sr.pos.clone() {
                StackRefPos::MemPos(pos) => mem
                    .get_mp_field_by_index_as_stack_value(
                        pos as usize,
                        sr.field_index.clone(),
                    )
                    .unwrap_or_else(|| {
                        panic!("Uninitialized memory in position {}", pos);
                    }),
                StackRefPos::TablePos(token, pos) => {
                    let ds = &self.data_sources.as_ref()[token];
                    let v =
                        ds.get_at_field_index(pos, sr.field_index.clone());
                    if let Some(v) = v {
                        StackValue::Arc(v.into())
                    } else {
                        StackValue::Owned(TypeValue::Unknown)
                    }
                }
                StackRefPos::CompareResult(res) => {
                    StackValue::Owned(res.into())
                }
            })
            .collect();

        stack.clear();
        take_vec
    }

    // Take a `elem_num` elements on the stack, leaving the rest of the stack in place.
    fn _take_resolved(
        &'a self,
        elem_num: u32, // number of elements to take
        mem: &'a LinearMemory,
    ) -> Vec<StackValue> {
        let mut stack = self.stack.borrow_mut();

        let len = stack.0.len();
        let stack_part = stack.0.split_off(len - elem_num as usize);

        let take_vec = stack_part
            .iter()
            .map(|sr| match sr.pos.clone() {
                StackRefPos::MemPos(pos) => {
                    mem.get_mp_field_by_index_as_stack_value(
                        pos as usize,
                        sr.field_index.clone(),
                    )
                    .unwrap_or_else(|| {
                        panic!("Uninitialized memory in position {}", pos);
                    })
                    // StackValue::Ref(v)
                }
                StackRefPos::TablePos(token, pos) => {
                    let ds = &self.data_sources.as_ref()[token];
                    let v =
                        ds.get_at_field_index(pos, sr.field_index.clone());
                    if let Some(v) = v {
                        StackValue::Arc(v.into())
                    } else {
                        StackValue::Owned(TypeValue::Unknown)
                    }
                }
                StackRefPos::CompareResult(res) => {
                    StackValue::Owned(res.into())
                }
            })
            .collect();

        take_vec
    }

    // Take a `elem_num` elements on the stack, leaving the rest of the stack in place.
    fn _take_resolved_as_owned(
        &'a self,
        elem_num: u32, // number of elements to take
        mem: &'a mut LinearMemory,
    ) -> Vec<TypeValue> {
        let mut stack = self.stack.borrow_mut();

        let len = stack.0.len();
        let stack_part = stack.0.split_off(len - elem_num as usize);

        let take_vec = stack_part
            .iter()
            .map(|sr| match sr.pos.clone() {
                StackRefPos::MemPos(pos) => {
                    mem.get_mp_field_by_stack_ref_owned(
                        sr,
                        // sr.field_index.clone(),
                    )
                    .unwrap_or_else(|_| {
                        panic!("Uninitialized memory in position {}", pos);
                    })
                    // StackValue::Ref(v)
                }
                StackRefPos::TablePos(_token, _pos) => {
                    panic!("Attempt to move a value from a data-source");
                }
                StackRefPos::CompareResult(res) => res.into(),
            })
            .collect();

        take_vec
    }

    fn as_vec(&'a self) -> Vec<StackRef> {
        let mut stack = self.stack.borrow_mut();
        stack.unwind()
    }

    pub fn reset(&self) {
        self.stack.borrow_mut().clear();
    }

    fn get_data_source(
        &self,
        token: usize,
    ) -> Result<Arc<DataSource>, VmError> {
        self.data_sources
            .as_ref()
            .iter()
            .find(|ds| ds.token == token)
            .and_then(|ds| ds.source.load_full().as_ref().map(Arc::clone))
            .ok_or(VmError::DataSourceTokenNotFound(token))
    }

    pub fn exec(
        &'a mut self,
        rx: impl RotoType,
        tx: Option<impl RotoType>,
        // define filter-map-level arguments, not used yet! Todo
        mut _arguments: Option<FilterMapArgs>,
        mem: &mut LinearMemory,
    ) -> Result<VmResult, VmError> {
        trace!("\nstart executing vm...");

        let mut commands_num: usize = 0;

        self._move_rx_tx_to_mem(rx, tx, mem);
        let mut output_stream_queue: OutputStreamQueue =
            OutputStreamQueue::new();

        for MirBlock {
            command_stack,
            ty: _,
        } in self.mir_code.as_ref()
        {
            trace!("\n\n--mirblock------------------");
            trace!("stack: {:?}", self.stack);

            for (pc, Command { op, args }) in command_stack.iter().enumerate()
            {
                commands_num += 1;
                let mut args = CommandArgsStack::new(args);
                trace!("\n{:3} -> {:?} {:?} ", pc, op, args);
                match op {
                    // args: [CompareOperator]
                    // stack args: [cmp1, cmp2]
                    OpCode::Cmp => {
                        let stack_args = self._take_resolved(2, mem);

                        trace!("raw stack args {:#?}", stack_args);
                        let left = stack_args[0].as_ref();
                        let right = stack_args[1].as_ref();

                        if log_enabled!(Level::Trace) {
                            trace!(" {:?} <-> {:?}", left, right);
                        }

                        match args[0] {
                            CommandArg::CompareOp(CompareOp::Eq) => {
                                let res = left == right;
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::CompareResult(res))?;
                            }
                            CommandArg::CompareOp(CompareOp::Ne) => {
                                let res = left != right;
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::CompareResult(res))?;
                            }
                            CommandArg::CompareOp(CompareOp::Lt) => {
                                let res = left < right;
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::CompareResult(res))?;
                            }
                            CommandArg::CompareOp(CompareOp::Le) => {
                                let res = left <= right;
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::CompareResult(res))?;
                            }
                            CommandArg::CompareOp(CompareOp::Gt) => {
                                let res = left > right;
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::CompareResult(res))?;
                            }
                            CommandArg::CompareOp(CompareOp::Ge) => {
                                let res = left >= right;
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::CompareResult(res))?;
                            }
                            CommandArg::CompareOp(CompareOp::Or) => {
                                let l = left.try_into()?;
                                let r = right.try_into()?;
                                let res = l || r;
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::CompareResult(res))?;
                            }
                            CommandArg::CompareOp(CompareOp::And) => {
                                let res =
                                    left.try_into()? && right.try_into()?;
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::CompareResult(res))?;
                            }
                            // Two possibilities here:
                            // - the right hand side is an actual TypeValue::List,
                            //   we can compare the left hand to all the values in
                            //   the right hand side list.
                            // - the right hand side list consists of all the
                            //   elements in stack_args, but for the first one
                            //   (that's still the left hand side)
                            CommandArg::CompareOp(CompareOp::In) => {
                                let res = if let TypeValue::List(list) = right
                                {
                                    list.iter().any(|v| {
                                        assert_ne!(v, &TypeValue::UnInit);
                                        v == left
                                    })
                                } else {
                                    stack_args[1..].iter().any(|v| {
                                        assert_ne!(
                                            v.as_ref(),
                                            &TypeValue::UnInit
                                        );
                                        v.as_ref() == left
                                    })
                                };
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::CompareResult(res))?;
                            }
                            CommandArg::CompareOp(CompareOp::NotIn) => {
                                let res = if let TypeValue::List(list) = right
                                {
                                    !list.iter().any(|v| {
                                        assert_ne!(v, &TypeValue::UnInit);
                                        v == left
                                    })
                                } else {
                                    !stack_args[1..].iter().any(|v| {
                                        assert_ne!(
                                            v.as_ref(),
                                            &TypeValue::UnInit
                                        );
                                        v.as_ref() == left
                                    })
                                };
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::CompareResult(res))?;
                            }
                            _ => panic!("{:3} -> invalid compare op", pc),
                        }
                    }
                    // stack args: [type, method_token, args, return memory position]
                    OpCode::ExecuteTypeMethod => {
                        if log_enabled!(Level::Trace) {
                            trace!("Stack {:?}", self.stack);
                            trace!("Args {:?}", args);
                        }
                        let mem_pos = if let CommandArg::MemPos(pos) =
                            args.pop().unwrap()
                        {
                            *pos as usize
                        } else {
                            return Err(VmError::InvalidValueType);
                        };
                        let (_args, method_t, return_type) = args.pop_3();

                        let stack_args = self
                            ._take_resolved(_args.get_args_len() as u32, mem);

                        // We are going to call a method on a type, so we
                        // extract the type from the first argument on the
                        // stack.
                        if let CommandArg::Type(t) = return_type {
                            let val = t.exec_type_method(
                                method_t.into(),
                                &stack_args,
                                return_type.into(),
                            );
                            mem.set_mem_pos(mem_pos, val);
                        }
                    }
                    // stack args: [method_token, return_type,
                    //      arguments, result memory position
                    // ]
                    OpCode::ExecuteValueMethod => {
                        trace!("execute value method");
                        let mem_pos = if let CommandArg::MemPos(pos) =
                            args.pop().unwrap()
                        {
                            *pos as usize
                        } else {
                            return Err(VmError::InvalidValueType);
                        };

                        let args_len: usize =
                            if let Some(CommandArg::Arguments(args)) =
                                args.pop()
                            {
                                args.len()
                            } else {
                                0
                            };

                        let (return_type, method_token) = args.pop_2();
                        trace!(
                            "return_type {:?}, method_token {:?}",
                            return_type,
                            method_token
                        );

                        // pop as many refs from the stack as we have
                        // arguments for this method and resolve them  to
                        // their values.
                        let mut stack = self.stack.borrow_mut();

                        let stack_args = [0..args_len].iter().map(|_i| {
                            let sr = stack.pop().unwrap();
                            match sr.pos {
                                StackRefPos::MemPos(pos) => {
                                    mem
                                        .get_mp_field_by_index_as_stack_value(pos as usize, sr.field_index)
                                        .unwrap_or_else(|| {
                                            trace!("\nstack: {:?}", stack);
                                            trace!("mem: {:#?}", mem.0);
                                            panic!("Uninitialized memory in position {}", pos);
                                        })
                                }
                                StackRefPos::TablePos(token, pos) => {
                                    let ds = &self.data_sources.as_ref()[token];
                                    let v = ds.get_at_field_index(pos, sr.field_index);
                                    if let Some(v) = v {
                                        StackValue::Arc(v.into())
                                    } else { StackValue::Owned(TypeValue::Unknown) }
                                }
                                StackRefPos::CompareResult(res) => StackValue::Owned(res.into())
                            }
                        }).collect::<Vec<_>>();
                        trace!("stack_args {:?}", stack_args);

                        // The first value on the stack is the value which we
                        // are going to call a method with.
                        let call_value = stack_args.get(0).unwrap().as_ref();

                        let v = call_value.exec_value_method(
                            method_token.into(),
                            &stack_args[1..],
                            return_type.into(),
                        )?;

                        mem.set_mem_pos(mem_pos, v);
                    }
                    // stack args: [
                    //      method_token, return_type,
                    //      arguments, result memory position
                    // ]
                    // pops arguments from the stack
                    OpCode::ExecuteConsumeValueMethod => {
                        let mem_pos = if let CommandArg::MemPos(pos) =
                            args.pop().unwrap()
                        {
                            *pos as usize
                        } else {
                            return Err(VmError::InvalidValueType);
                        };

                        let args_len: usize =
                            if let Some(CommandArg::Arguments(args)) =
                                args.pop()
                            {
                                args.len()
                            } else {
                                0
                            };

                        if log_enabled!(Level::Trace) {
                            trace!("Args {:?}", args);
                        }
                        let (return_type, method_token) = args.pop_2();

                        // pop as many refs from the stack as we have
                        // arguments for this method and resolve them to
                        // their values.
                        let mut stack = self.stack.borrow_mut();

                        // the field_index of the first argument on the
                        // stack, this is the field that will be consumed
                        // and returned by `exec_consume_value_method`
                        // later on.
                        let mut target_field_index = smallvec::smallvec![];

                        trace!("\nargs_len {}", args_len);
                        trace!("Stack {:?}", stack);

                        let mut stack_args = (0..args_len).map(|_i| {
                            let sr = stack.pop().unwrap();

                            target_field_index = if target_field_index.is_empty() {
                                sr.field_index
                            } else {
                                target_field_index.clone()
                            };

                            match sr.pos {
                                StackRefPos::MemPos(pos) => {
                                    mem
                                        .get_mem_pos_as_owned(pos as usize)
                                        .unwrap_or_else(|| {
                                            trace!("\nstack: {:?}", stack);
                                            trace!("mem: {:#?}", mem.0);
                                            panic!(r#"Uninitialized memory in 
                                                pos {}. That's fatal"#, pos);
                                        })
                                }
                                StackRefPos::TablePos(_token, _pos) => {
                                    panic!(r#"Can't mutate data in a data source. 
                                    That's fatal."#);
                                }
                                StackRefPos::CompareResult(_res) => {
                                    panic!("Fatal: Can't mutate a compare result.");
                                }
                            }
                        }).collect::<Vec<_>>();

                        // The first value on the stack is the value which we
                        // are going to call a method with.
                        let collection_value = stack_args.remove(0);

                        // We have the complete instance, now see if we can:
                        // - get hold of the right field in this instance
                        //   (it is taken out of the instance!)
                        // - invoke the method that was requested on the
                        //   field
                        // - put the result of the method back in the
                        //   instance
                        // - write the instance back in the right memory
                        //   position.
                        let result_value = match collection_value {
                            TypeValue::Record(mut rec) => {
                                let call_value = TypeValue::from(
                                    rec.get_field_by_index_owned(
                                        target_field_index.clone(),
                                    ),
                                )
                                .exec_consume_value_method(
                                    method_token.into(),
                                    stack_args,
                                    return_type.into(),
                                    // target_field_index.clone(),
                                )?;
                                rec.set_value_on_field_index(
                                    target_field_index.clone(),
                                    call_value,
                                )?;
                                TypeValue::Record(rec)
                            }
                            TypeValue::List(mut list) => {
                                let call_value = TypeValue::from(
                                    list.get_field_by_index_owned(
                                        target_field_index.clone(),
                                    )
                                    .unwrap(),
                                )
                                .exec_consume_value_method(
                                    method_token.into(),
                                    stack_args,
                                    return_type.into(),
                                    // target_field_index.clone(),
                                )?;
                                list.set_field_for_index(
                                    target_field_index,
                                    call_value,
                                )?;
                                TypeValue::List(list)
                            }
                            _ => collection_value,
                        };

                        mem.set_mem_pos(mem_pos, result_value);
                    }
                    // args: [data_source_token, method_token, arguments,
                    //       return memory position]
                    OpCode::ExecuteDataStoreMethod => {
                        let mem_pos = if let CommandArg::MemPos(pos) =
                            args.pop().unwrap()
                        {
                            *pos as usize
                        } else {
                            return Err(VmError::InvalidValueType);
                        };

                        let _args_len: usize =
                            if let Some(CommandArg::Arguments(args)) =
                                args.pop()
                            {
                                args.len()
                            } else {
                                0
                            };

                        trace!(
                            "execute data store method with args {:?}",
                            args
                        );
                        let (method_token, data_source_token) = args.pop_2();

                        match data_source_token {
                            CommandArg::DataSourceTable(ds_s)
                            | CommandArg::DataSourceRib(ds_s) => {
                                let ds = self.get_data_source(*ds_s).unwrap();
                                let stack_args =
                                    self._unwind_resolved_stack_into_vec(mem);

                                let v = ds.exec_method(
                                    method_token.into(),
                                    &stack_args[..],
                                    TypeDef::Unknown,
                                );
                                let mut s = self.stack.borrow_mut();
                                match v {
                                    DataSourceMethodValue::Ref(sr_pos) => {
                                        s.push(sr_pos)?;
                                    }
                                    DataSourceMethodValue::TypeValue(tv) => {
                                        mem.set_mem_pos(mem_pos, tv);
                                    }
                                    DataSourceMethodValue::Empty(_ty) => {
                                        mem.set_mem_pos(
                                            mem_pos,
                                            TypeValue::Unknown,
                                        );
                                    }
                                }
                            }
                            _ => return Err(VmError::InvalidDataSource),
                        }
                    }
                    // stack args: [mem_pos, constant_value]
                    OpCode::PushStack => match args[0] {
                        CommandArg::MemPos(pos) => {
                            if log_enabled!(Level::Trace) {
                                trace!(
                                    " content: {:?}",
                                    mem.get_mem_pos(pos as usize)
                                );
                            }

                            let mut s = self.stack.borrow_mut();
                            s.push(StackRefPos::MemPos(pos))?;
                            if log_enabled!(Level::Trace) {
                                trace!(" stack {:?}", s);
                            }
                        }
                        _ => return Err(VmError::InvalidValueType),
                    },
                    // no stack_args
                    OpCode::PopStack => {
                        if args.is_empty() {
                            let mut s = self.stack.borrow_mut();
                            s.pop()?;
                        } else {
                            return Err(VmError::InvalidValueType);
                        }
                    }
                    // no stack_args
                    OpCode::ClearStack => {
                        let mut s = self.stack.borrow_mut();
                        s.clear();
                    }
                    // stack args: [mem_pos, constant_value]
                    OpCode::MemPosSet => {
                        trace!("memposset value: {:?}", args);

                        if let CommandArg::MemPos(pos) = args[0] {
                            let v = args.take_arg_as_constant()?;
                            mem.set_mem_pos(pos as usize, v);
                        } else {
                            return Err(VmError::InvalidValueType);
                        }
                    }
                    // stack args: [field_index]
                    OpCode::StackOffset => {
                        for arg in args.args.iter() {
                            if let CommandArg::FieldAccess(field) = arg {
                                let mut s = self.stack.borrow_mut();
                                trace!(" -> stack {:?}", s);
                                trace!("mem_pos 0 {:?}", mem.0[0]);
                                s.add_field_index(*field)?;
                                if log_enabled!(Level::Trace) {
                                    trace!(" -> stack {:?}", s);
                                }
                            } else {
                                return Err(VmError::InvalidValueType);
                            }
                        }
                    }
                    // stack args: [arg_token_value, mem_pos]
                    OpCode::ArgToMemPos => {
                        if let CommandArg::MemPos(pos) = args[1] {
                            match args[0] {
                                CommandArg::Argument(token_value) => {
                                    let arg_value = self
                                        .arguments
                                        .take_value_by_token(token_value)?;
                                    mem.set_mem_pos(pos as usize, arg_value);
                                }
                                _ => {
                                    return Err(VmError::InvalidValueType);
                                }
                            }
                        } else {
                            return Err(VmError::InvalidValueType);
                        }
                    }
                    OpCode::Label => {
                        // NOOP
                    }
                    // Term procedures
                    // stack args ignored
                    OpCode::CondFalseSkipToEOB => {
                        let s = self.stack.borrow();
                        let stack_ref = s.get_top_value()?;
                        if !mem.get_mp_field_as_bool(stack_ref) {
                            if log_enabled!(Level::Trace) {
                                trace!(" skip to end of block");
                            }
                            break;
                        } else {
                            if log_enabled!(Level::Trace) {
                                trace!(" continue");
                            }
                            continue;
                        }
                    }
                    // stack args ignored
                    OpCode::CondTrueSkipToEOB => {
                        let s = self.stack.borrow();
                        let stack_ref = s.get_top_value()?;
                        if !mem.get_mp_field_as_bool(stack_ref) {
                            if log_enabled!(Level::Trace) {
                                trace!(" continue");
                            }
                            continue;
                        } else {
                            if log_enabled!(Level::Trace) {
                                trace!(" skip to end of block");
                            }
                            break;
                        }
                    }
                    // stack args: [exit value]
                    OpCode::Exit(accept_reject) => {
                        let rx = mem
                            .get_mem_pos_as_owned(0)
                            .ok_or(VmError::InvalidPayload)?;

                        let tx = match mem.get_mem_pos_as_owned(1) {
                            Some(TypeValue::Record(rec)) => Some(rec.into()),
                            _ => None,
                        };

                        if *accept_reject != AcceptReject::NoReturn {
                            if log_enabled!(Level::Trace) {
                                trace!("\n\nINITIALIZED MEMORY POSITIONS");
                            }
                            for (i, addr) in
                                mem.0.as_slice().iter().enumerate()
                            {
                                if log_enabled!(Level::Trace)
                                    && !addr.is_unitialized()
                                {
                                    trace!("{}: {}", i, addr);
                                }
                            }

                            trace!(
                                "\n🍺 Done! Successfully executed {} instructions.",
                                commands_num
                            );

                            return Ok(VmResult {
                                accept_reject: *accept_reject,
                                rx,
                                tx,
                                output_stream_queue,
                            });
                        }
                    }

                    // SetRxField will replace the rx instance with the
                    // latest the value that is ref'ed by the top of the
                    // stack.

                    // stack args: [new value]
                    OpCode::SetRxField => {
                        // pop all refs from the stack and resolve them to
                        // their values.
                        let stack_args = self.as_vec();

                        // swap out the new value from memory
                        let val = mem
                            .get_mp_field_by_stack_ref_owned(
                                stack_args.last().unwrap(),
                            )
                            .unwrap();

                        // save the value in memory position 0 (rx instance
                        // by definition).
                        mem.set_mem_pos(0, val);
                    }
                    // stack args: [tx type instance field, new value]
                    OpCode::SetTxField => {
                        todo!();
                    }
                    // stack args: [ordered_field_values..]
                    OpCode::PushOutputStreamQueue => {
                        trace!("Send: {:?}", args);

                        let elem_num =
                            args[2].get_args_len_for_outputstream_record();

                        trace!(
                            "no of fields {}",
                            args[3].get_args_len() as u32
                        );

                        let stack_args =
                            self._take_resolved_as_owned(elem_num, mem);
                        trace!("Stack args {:?}", stack_args);

                        if let CommandArg::MemPos(mem_pos) = args[3] {
                            trace!("from mem pos {}", mem_pos);
                        }

                        let mut rec_fields: Vec<TypeValue> = vec![];

                        for stack_ref in stack_args {
                            rec_fields.insert(0, stack_ref);
                        }

                        if let CommandArg::Arguments(type_def) = &args[2] {
                            trace!("type_def {:?}", type_def);
                            if let TypeDef::OutputStream(os_ty) = &type_def[0]
                            {
                                let rec = Record::create_instance_from_ordered_fields(os_ty, rec_fields).unwrap();
                                output_stream_queue.push(rec.into());
                            }
                        }
                    }
                }
            }

            if log_enabled!(Level::Trace) {
                trace!("\n\n(end) stack: {:?}", self.stack);
                trace!("\nINITIALIZED MEMORY POSITIONS");
            }

            for (i, addr) in mem.0.as_slice().iter().enumerate() {
                if log_enabled!(Level::Trace) && !addr.is_unitialized() {
                    trace!("{}: {}", i, addr);
                }
            }
        }

        Err(VmError::UnexpectedTermination)
    }
}

pub struct VmBuilder<MB: AsRef<[MirBlock]>, EDS: AsRef<[ExtDataSource]>> {
    rx_type: TypeDef,
    tx_type: Option<TypeDef>,
    mir_code: Option<MB>,
    arguments: FilterMapArgs,
    data_sources: Option<EDS>,
}

impl<MB: AsRef<[MirBlock]>, EDS: AsRef<[ExtDataSource]>> VmBuilder<MB, EDS> {
    pub fn new() -> Self {
        Self {
            rx_type: TypeDef::default(),
            tx_type: None,
            mir_code: None,
            arguments: FilterMapArgs::default(),
            data_sources: None,
        }
    }

    pub fn with_mir_code(mut self, mir_code: MB) -> Self {
        self.mir_code = Some(mir_code);
        self
    }

    pub fn with_arguments(mut self, args: FilterMapArgs) -> Self {
        self.arguments = args;
        self
    }

    pub fn with_data_sources(mut self, data_sources: EDS) -> Self {
        self.data_sources = Some(data_sources);
        self
    }

    pub fn with_rx(mut self, rx_type: TypeDef) -> Self {
        self.rx_type = rx_type;
        self
    }

    pub fn with_tx(mut self, tx_type: TypeDef) -> Self {
        self.tx_type = Some(tx_type);
        self
    }

    pub fn build(self) -> Result<VirtualMachine<MB, EDS>, VmError> {
        // data sources need to be complete. Check that.
        trace!("data sources in builder");
        let data_sources = if let Some(data_sources) = self.data_sources {
            for ds in data_sources.as_ref().iter() {
                trace!("{}", ds.exists_and_is_empty());
                match ds.exists_and_is_empty() {
                    ExistsAndEmpty(Some(_)) => {}
                    ExistsAndEmpty(None) => {
                        return Err(VmError::DataSourceNotInBuild(
                            ds.get_name(),
                        ))
                    }
                };
            }
            data_sources
        } else {
            return Err(VmError::DataSourcesNotReady);
        };

        if let Some(mir_code) = self.mir_code {
            Ok(VirtualMachine {
                mir_code,
                data_sources,
                arguments: self.arguments,
                stack: RefCell::new(Stack::new()),
            })
        } else {
            Err(VmError::NoMir)
        }
    }
}

impl<MB: AsRef<[MirBlock]>, EDS: AsRef<[ExtDataSource]>> Default
    for VmBuilder<MB, EDS>
{
    fn default() -> Self {
        Self::new()
    }
}

//------------ VmResult -----------------------------------------------------

#[derive(Debug, Clone)]
pub struct VmResult {
    pub accept_reject: AcceptReject,
    pub rx: TypeValue,
    pub tx: Option<TypeValue>,
    pub output_stream_queue: OutputStreamQueue,
}

//------------ StreamOutputQueue --------------------------------------------

#[derive(Debug, Copy, Clone)]
pub struct StreamId(usize);

#[derive(Debug, Clone)]
pub struct OutputStreamQueue(SmallVec<[OutputStreamMessage; 8]>);

impl OutputStreamQueue {
    pub fn new() -> Self {
        OutputStreamQueue(SmallVec::<[OutputStreamMessage; 8]>::new())
    }

    pub fn push(&mut self, msg: OutputStreamMessage) {
        self.0.push(msg)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn pop(&mut self) -> Option<OutputStreamMessage> {
        self.0.pop()
    }

    pub fn iter(&self) -> impl Iterator<Item=&OutputStreamMessage> + '_ {
        self.0.iter()
    }
}

impl Default for OutputStreamQueue {
    fn default() -> Self {
        Self::new()
    }
}

impl Index<usize> for OutputStreamQueue {
    type Output = OutputStreamMessage;

    fn index(&self, index: usize) -> &Self::Output {
        self.0.index(index)
    }
}

impl IntoIterator for OutputStreamQueue {
    type Item = OutputStreamMessage;
    type IntoIter = <smallvec::SmallVec<[OutputStreamMessage; 8]> 
        as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

//------------ VmError ------------------------------------------------------

#[derive(Debug)]
pub enum VmError {
    StackUnderflow,
    StackOverflow,
    MemOutOfBounds,
    InvalidMemoryAccess(usize, Option<usize>),
    AnonymousArgumentNotFound,
    ArgumentNotFound(ShortString),
    ArgumentsMissing(Vec<ShortString>),
    InvalidValueType,
    InvalidPayload,
    InvalidVariableAccess,
    InvalidFieldAccess(usize),
    InvalidMethodCall,
    DataSourceTokenNotFound(usize),
    DataSourceNotInBuild(ShortString),
    DataSourceEmpty(ShortString),
    DataSourcesNotReady,
    ImpossibleComparison,
    InvalidDataSource,
    InvalidCommand,
    InvalidWrite,
    InvalidConversion,
    InvalidMsgType,
    UnexpectedTermination,
    AsPathTooLong,
    DeltaLocked,
    NoMir,
}

impl Display for VmError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            VmError::StackUnderflow => f.write_str("StackUnderflow"),
            VmError::StackOverflow => f.write_str("StackOverflow"),
            VmError::MemOutOfBounds => f.write_str("MemOutOfBounds"),
            VmError::InvalidMemoryAccess(_, _) => {
                f.write_str("InvalidMemoryAccess")
            }
            VmError::AnonymousArgumentNotFound => {
                f.write_str("AnonymousArgumentNotFound")
            }
            VmError::ArgumentNotFound(_) => f.write_str("ArgumentNotFound"),
            VmError::ArgumentsMissing(_) => f.write_str("ArgumentsMissing"),
            VmError::InvalidValueType => f.write_str("InvalidValueType"),
            VmError::InvalidPayload => f.write_str("InvalidPayload"),
            VmError::InvalidVariableAccess => {
                f.write_str("InvalidVariableAccess")
            }
            VmError::InvalidFieldAccess(_) => {
                f.write_str("InvalidFieldAccess")
            }
            VmError::InvalidMethodCall => f.write_str("InvalidMethodCall"),
            VmError::DataSourceTokenNotFound(_) => {
                f.write_str("DataSourceTokenNotFound")
            }
            VmError::DataSourceNotInBuild(ds_name) => {
                write!(
                    f,
                    "Data source '{}' was not in the build arguments.",
                    ds_name
                )
            }
            VmError::DataSourceEmpty(name) => {
                write!(f, "DataSourceEmpty {}", name)
            }
            VmError::DataSourcesNotReady => {
                write!(f, "DataSourceNotReady")
            }
            VmError::ImpossibleComparison => {
                f.write_str("ImpossibleComparison")
            }
            VmError::InvalidWrite => f.write_str("InvalidWrite"),
            VmError::InvalidCommand => f.write_str("InvalidCommand"),
            VmError::InvalidDataSource => f.write_str("InvalidDataSource"),
            VmError::InvalidConversion => f.write_str("InvalidConversion"),
            VmError::UnexpectedTermination => {
                f.write_str("UnexpectedTermination")
            }
            VmError::InvalidMsgType => f.write_str("InvalidMessageType"),
            VmError::AsPathTooLong => f.write_str("AsPathTooLong"),
            VmError::DeltaLocked => f.write_str("DeltaLocked"),
            VmError::NoMir => f.write_str("NoMir"),
        }
    }
}

impl From<VmError> for Box<dyn std::error::Error> {
    fn from(value: VmError) -> Self {
        format!("A fatal VM Error occured: {}", value).into()
    }
}

#[derive(Debug, Clone)]
pub struct Command {
    pub(crate) op: OpCode,
    pub(crate) args: Vec<CommandArg>,
}

impl Command {
    pub fn new(op: OpCode, args: Vec<CommandArg>) -> Self {
        Command { op, args }
    }
}

impl Display for Command {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let arrow = match &self.op {
            OpCode::Cmp => "<->",
            OpCode::ExecuteTypeMethod => "->",
            OpCode::ExecuteDataStoreMethod => "->",
            OpCode::ExecuteValueMethod => "->",
            OpCode::ExecuteConsumeValueMethod => "=>",
            OpCode::PushStack => "<-",
            OpCode::PopStack => "->",
            OpCode::ClearStack => "::",
            OpCode::MemPosSet => "->",
            OpCode::ArgToMemPos => "->",
            OpCode::StackOffset => "",
            OpCode::CondFalseSkipToEOB => "-->",
            OpCode::CondTrueSkipToEOB => "-->",
            OpCode::Label => {
                return write!(f, "🏷  {}", self.args[0]);
            }
            OpCode::Exit(accept_reject) => {
                return write!(f, "Exit::{}", accept_reject)
            }
            OpCode::PushOutputStreamQueue => "(o)=>",
            OpCode::SetRxField => "->",
            OpCode::SetTxField => "->",
        };
        write!(f, "{:?}{}{:?}", self.op, arrow, self.args)
    }
}

#[derive(Debug)]
pub enum CommandArg {
    Constant(TypeValue),        // Constant value
    Variable(usize),            // Variable with token value
    Argument(usize), // extra runtime argument for filter_map & term
    List(List),      // a list that needs to be stored at a memory posision
    Record(Record),  // a record that needs to be stored at a mem posistion
    RxValue, // the placeholder for the value of the rx type at runtime
    TxValue, // the placeholder for the value of the tx type at runtime
    Method(usize), // method token value
    DataSourceTable(usize), // data source: table token value
    DataSourceRib(usize), // data source: rib token value
    OutputStream(usize), // ouput stream value
    FieldAccess(usize), // field access token value
    BuiltinMethod(usize), // builtin method token value
    MemPos(u32), // memory position
    Type(TypeDef), // type definition
    Arguments(Vec<TypeDef>), // argument types (for method calls)
    Boolean(bool), // boolean value (used in cmp opcode)
    Term(usize), // term token value
    CompareOp(ast::CompareOp), // compare operation
    Label(ShortString), // a label with its name (to jump to)
    AcceptReject(AcceptReject), // argument tell what should happen after
}

impl CommandArg {
    pub fn as_token_value(&self) -> usize {
        match self {
            CommandArg::Argument(v) => *v,
            CommandArg::Variable(v) => *v,
            _ => {
                panic!("Cannot get token value from this arg: {:?} and that's fatal", self);
            }
        }
    }

    pub fn get_args_len(&self) -> usize {
        if let CommandArg::Arguments(args) = self {
            args.len()
        } else {
            1
        }
    }

    pub fn get_args_len_for_outputstream_record(&self) -> u32 {
        if let CommandArg::Arguments(fields) = &self {
            if let TypeDef::OutputStream(record) = &fields[0] {
                if let TypeDef::Record(args) = &**record {
                    args.len() as u32
                } else {
                    0
                }
            } else {
                0
            }
        } else {
            0
        }
    }
}

impl Display for CommandArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CommandArg::Label(v) => write!(f, "{}", v),
            _ => write!(f, ""),
        }
    }
}

impl From<&CommandArg> for TypeDef {
    fn from(value: &CommandArg) -> Self {
        match value {
            CommandArg::Type(t) => t.clone(),
            _ => {
                panic!(
                    "Cannot convert to TypeDef: {:?} and that's fatal.",
                    value
                );
            }
        }
    }
}

// extract the token value from an argument
impl From<&CommandArg> for usize {
    fn from(value: &CommandArg) -> Self {
        match value {
            CommandArg::Method(m) => *m,
            CommandArg::DataSourceTable(d) => *d,
            CommandArg::DataSourceRib(d) => *d,
            CommandArg::FieldAccess(f) => *f,
            CommandArg::BuiltinMethod(b) => *b,
            CommandArg::Variable(v) => *v,
            CommandArg::MemPos(m) => *m as usize,
            _ => {
                panic!(
                    "Cannot convert to usize {:?} and that's fatal.",
                    value
                );
            }
        }
    }
}

impl From<ast::CompareOp> for CommandArg {
    fn from(op: ast::CompareOp) -> Self {
        CommandArg::CompareOp(op)
    }
}

impl From<crate::traits::Token> for Vec<CommandArg> {
    fn from(to: crate::traits::Token) -> Self {
        if let Token::FieldAccess(v) = to {
            v.iter()
                .map(|f| CommandArg::FieldAccess(*f as usize))
                .collect::<Vec<_>>()
        } else {
            panic!("PANIC")
        }
    }
}

// Cloning an Arg works only it Arg::Constants does NOT contain a
// (used-defined) Record or List
impl Clone for CommandArg {
    fn clone(&self) -> Self {
        match self {
            CommandArg::Constant(c) => CommandArg::Constant(
                c.builtin_as_cloned_type_value().unwrap(),
            ),
            CommandArg::Variable(v) => CommandArg::Variable(*v),
            CommandArg::Argument(a) => CommandArg::Argument(*a),
            CommandArg::RxValue => CommandArg::RxValue,
            CommandArg::TxValue => CommandArg::TxValue,
            CommandArg::List(l) => CommandArg::List(l.clone()),
            CommandArg::Record(r) => CommandArg::Record(r.clone()),
            CommandArg::Method(m) => CommandArg::Method(*m),
            CommandArg::DataSourceTable(ds) => {
                CommandArg::DataSourceTable(*ds)
            }
            CommandArg::DataSourceRib(ds) => CommandArg::DataSourceTable(*ds),
            CommandArg::OutputStream(os) => CommandArg::OutputStream(*os),
            CommandArg::FieldAccess(fa) => CommandArg::FieldAccess(*fa),
            CommandArg::BuiltinMethod(bim) => CommandArg::BuiltinMethod(*bim),
            CommandArg::MemPos(mp) => CommandArg::MemPos(*mp),
            CommandArg::Type(ty) => CommandArg::Type(ty.clone()),
            CommandArg::Arguments(args) => {
                CommandArg::Arguments(args.to_vec())
            }
            CommandArg::Boolean(b) => CommandArg::Boolean(*b),
            CommandArg::Term(t) => CommandArg::Term(*t),
            CommandArg::CompareOp(op) => CommandArg::CompareOp(*op),
            CommandArg::Label(l) => CommandArg::Label(l.clone()),
            CommandArg::AcceptReject(ar) => CommandArg::AcceptReject(*ar),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum OpCode {
    Cmp,
    ExecuteTypeMethod,
    ExecuteDataStoreMethod,
    ExecuteValueMethod,
    ExecuteConsumeValueMethod,
    PopStack,
    PushStack,
    ClearStack,
    StackOffset,
    MemPosSet,
    ArgToMemPos,
    // Skip to the end of the MIR block if the top of the stack
    // holds a reference to a boolean value true
    CondFalseSkipToEOB,
    // Skip to the end of the MIR block if the top of the stack
    // holds a redference to a boolean value false.
    CondTrueSkipToEOB,
    // Debug Label for terms
    Label,
    SetRxField,
    SetTxField,
    // The output stream stack holds indexes to memory positions that
    // contain messages to be send out.
    PushOutputStreamQueue,
    Exit(AcceptReject),
}

pub struct ExtDataSource {
    name: ShortString,
    token: usize,
    ty: TypeDef,
    source: ArcSwapOption<DataSource>,
}

impl std::fmt::Debug for ExtDataSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

pub struct ExistsAndEmpty(Option<bool>);

impl Display for ExistsAndEmpty {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            ExistsAndEmpty(Some(true)) => {
                write!(f, "source exists and has values")
            }
            ExistsAndEmpty(Some(false)) => {
                write!(f, "source exists, but is empty")
            }
            ExistsAndEmpty(None) => write!(f, "source does not exist"),
        }
    }
}

impl Clone for ExtDataSource {
    fn clone(&self) -> Self {
        Self {
            name: self.name.clone(),
            token: self.token,
            ty: self.ty.clone(),
            source: if let Some(s) = self.source.load().as_ref() {
                ArcSwapOption::from(Some(Arc::clone(s)))
            } else {
                ArcSwapOption::from(None)
            },
        }
    }
}

impl ExtDataSource {
    pub fn new(name: &str, token: Token, ty: TypeDef) -> ExtDataSource {
        ExtDataSource {
            name: name.into(),
            token: token.into(),
            source: ArcSwapOption::from(None),
            ty,
        }
    }

    pub fn get_name(&self) -> ShortString {
        self.name.clone()
    }

    pub(crate) fn get_source(&self) -> &ArcSwapOption<DataSource> {
        &self.source
    }

    pub fn get_type(&self) -> &TypeDef {
        &self.ty
    }

    pub fn get_value_type(&self) -> TypeDef {
        match &self.ty {
            TypeDef::Table(t) => *t.clone(),
            TypeDef::Rib((rec, _)) => *rec.clone(),
            TypeDef::OutputStream(s) => *s.clone(),
            _ => self.ty.clone(),
        }
    }

    pub fn exists_and_is_empty(&self) -> ExistsAndEmpty {
        if let Some(source) = self.source.load().as_ref() {
            ExistsAndEmpty(Some(source.as_ref().is_empty()))
        } else {
            ExistsAndEmpty(None)
        }
    }

    pub fn get_at_field_index(
        &self,
        pos: usize,
        field_index: SmallVec<[usize; 8]>,
    ) -> Option<TypeValue> {
        self.source.load().as_ref().map(|ds| {
            match ds.get_at_field_index(pos, field_index) {
                Some(TypeValue::SharedValue(sv)) => {
                    TypeValue::SharedValue(Arc::clone(sv))
                }
                Some(_) => panic!("Fatal: Table contains non-shared value."),
                None => TypeValue::Unknown,
            }
        })
    }
}

impl Index<Token> for [ExtDataSource] {
    type Output = ExtDataSource;

    fn index(&self, index: Token) -> &ExtDataSource {
        match index {
            Token::Table(token) => {
                if let Some(s) = self.get(token) {
                    s
                } else {
                    panic!("No source for {:?}", index);
                }
            }
            _ => {
                panic!("Cannot with {:?}", index);
            }
        }
    }
}

impl AsRef<ExtDataSource> for &ExtDataSource {
    fn as_ref(&self) -> &ExtDataSource {
        self
    }
}
