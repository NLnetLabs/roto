use std::{
    cell::{Ref, RefCell},
    fmt::{Display, Formatter},
    ops::{Index, IndexMut},
};

use crate::{
    ast::{self, CompareOp, ShortString},
    compile::MirBlock,
    traits::Token,
    types::{
        builtin::{Boolean, BuiltinTypeValue},
        collections::{ElementTypeValue, Record},
        datasources::{DataSourceMethodValue, Rib, Table},
        typedef::TypeDef,
        typevalue::TypeValue,
    },
};

#[derive(Debug)]
pub(crate) enum StackRefPos {
    // index into LinearMemory
    MemPos(u32),
    // index into a Table (which is a vec of Records)
    TablePos(Token, usize),
}

impl From<u32> for StackRefPos {
    fn from(mem_pos: u32) -> Self {
        StackRefPos::MemPos(mem_pos)
    }
}

#[derive(Debug)]
pub(crate) struct StackRef {
    pos: StackRefPos,
    field_index: Option<usize>,
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
            field_index: None,
        });
        Ok(())
    }

    fn pop(&'a mut self) -> Result<StackRef, VmError> {
        self.0.pop().ok_or(VmError::StackUnderflow)
    }

    fn get_top_value(&'a self) -> Result<&StackRef, VmError> {
        self.0.get(0).ok_or(VmError::StackUnderflow)
    }

    fn set_field_index(&mut self, index: usize) -> Result<(), VmError> {
        self.0
            .last_mut()
            .ok_or(VmError::StackUnderflow)?
            .field_index = Some(index);
        Ok(())
    }

    fn unwind(&mut self) -> Vec<StackRef> {
        self.0.drain(..).collect()
    }

    fn clear(&mut self) {
        self.0.clear();
    }
}

#[derive(Debug)]
pub struct LinearMemory([TypeValue; 512]);

impl LinearMemory {
    pub fn empty() -> Self {
        LinearMemory(std::array::from_fn(|_| TypeValue::None))
    }

    pub fn get(&self, index: usize) -> Option<&TypeValue> {
        self.0.get(index)
    }

    pub(crate) fn get_by_stack_ref(
        &self,
        stack_ref: &StackRef,
    ) -> Option<&TypeValue> {
        if let StackRefPos::MemPos(pos) = stack_ref.pos {
            self.get_at_field_index(pos as usize, stack_ref.field_index)
        } else {
            None
        }
    }

    pub fn get_at_field_index(
        &self,
        index: usize,
        field_index: Option<usize>,
    ) -> Option<&TypeValue> {
        match field_index {
            None => self.get(index),
            Some(field_index) => match self.get(index) {
                Some(TypeValue::Record(r)) => {
                    let field = r.get_field_by_index(field_index);
                    match field {
                        Some((_, ElementTypeValue::Nested(nested))) => {
                            Some(nested)
                        }
                        Some((_, ElementTypeValue::Primitive(b))) => Some(b),
                        _ => None,
                    }
                }
                Some(TypeValue::List(l)) => {
                    let field = l.get_field_by_index(field_index);
                    match field {
                        Some((_, ElementTypeValue::Nested(nested))) => {
                            Some(nested)
                        }
                        Some((_, ElementTypeValue::Primitive(b))) => Some(b),
                        _ => None,
                    }
                }
                _ => None,
            },
        }
    }

    fn _get_mut(&mut self, index: usize) -> Option<&mut TypeValue> {
        self.0.get_mut(index)
    }

    fn _take(&mut self, index: usize) -> Option<TypeValue> {
        self.0.get_mut(index).map(std::mem::take)
    }

    fn set(&mut self, index: usize, value: TypeValue) {
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

#[derive(Default, Debug)]
pub struct ArgumentsMap(Vec<(usize, TypeValue)>);

impl ArgumentsMap {
    fn take_by_token_value(
        &mut self,
        index: usize,
    ) -> Result<(usize, TypeValue), VmError> {
        self.0
            .iter_mut()
            .find(move |(t, _)| *t == index)
            .map(std::mem::take)
            .ok_or(VmError::ArgumentNotFound)
    }

    fn _get_by_token_value(&self, index: usize) -> Option<&TypeValue> {
        self.0.iter().find(|(t, _)| *t == index).map(|(_, v)| v)
    }

    fn _take_by_index(mut self, index: usize) -> Option<TypeValue> {
        self.0.get_mut(index).map(std::mem::take).map(|(_, v)| v)
    }

    pub fn new() -> Self {
        ArgumentsMap(Vec::new())
    }

    pub fn insert(&mut self, index: usize, value: TypeValue) {
        self.0.push((index, value));
    }

    pub fn last_index(&self) -> Option<usize> {
        if self.0.is_empty() {
            Some(self.0.len())
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct VariableRef {
    pub var_token_value: usize,
    pub mem_pos: u32,
    pub field_index: usize,
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
        field_index: usize,
    ) -> Result<(), VmError> {
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

pub struct VirtualMachine<'a> {
    rx_type: TypeDef,
    tx_type: Option<TypeDef>,
    variables: &'a RefCell<VariablesMap>,
    data_sources: &'a [&'a ExtDataSource],
    stack: RefCell<Stack>,
}

impl<'a> VirtualMachine<'a> {
    fn _move_rx_tx_to_mem(
        &mut self,
        rx: impl Payload,
        tx: Option<impl Payload>,
        mem: &RefCell<LinearMemory>,
    ) {
        let mut m = mem.borrow_mut();
        let rx = rx.take_value();
        m.set(0, rx);

        if let Some(tx) = tx {
            let tx = tx.take_value();
            println!("move tx to mem[1s]: {}", tx);
            m.set(1, tx);
        }
    }

    fn _unwind_resolved_stack_into_vec(
        &'a self,
        mem: &'a LinearMemory,
    ) -> Vec<&'a TypeValue> {
        let mut stack = self.stack.borrow_mut();
        let mut unwind_stack = vec![];
        for sr in stack.unwind().into_iter() {
            match sr.pos {
                StackRefPos::MemPos(pos) => {
                    let v = mem
                        .get_at_field_index(pos as usize, sr.field_index)
                        .unwrap();
                    unwind_stack.push(v);
                }
                StackRefPos::TablePos(token, pos) => {
                    let ds = &self.data_sources[token];
                    let v = ds.get_at_field_index(pos, sr.field_index);
                    if let Some(v) = v {
                        unwind_stack.push(v);
                    }
                }
            }
        }
        unwind_stack
    }

    fn get_data_source(
        &self,
        token: usize,
    ) -> Result<&ExtDataSource, VmError> {
        self.data_sources
            .iter()
            .find(|ds| ds.token == token)
            .copied()
            .ok_or(VmError::DataSourceNotFound(token))
    }

    pub fn exec(
        &'a mut self,
        rx: impl Payload,
        tx: Option<impl Payload>,
        mut arguments: Option<ArgumentsMap>,
        mem: RefCell<LinearMemory>,
        mir_code: Vec<MirBlock>,
    ) -> Result<(), VmError> {
        println!("\nstart executing vm...");
        let mut commands_num: usize = 0;

        self._move_rx_tx_to_mem(rx, tx, &mem);
        let mut arguments = arguments.take().unwrap_or_default();

        for MirBlock { command_stack } in mir_code {
            println!("\n\n--mirblock------------------");

            println!("stack: {:?}", self.stack);
            for Command { op, mut args } in command_stack {
                commands_num += 1;
                print!("\n-> {:3?} {:?} ", op, args);
                match op {
                    OpCode::Cmp => {
                        let mut m = mem.borrow_mut();
                        let stack_args =
                            self._unwind_resolved_stack_into_vec(&m);
                        let left = stack_args[0];
                        let right = stack_args[1];

                        match &args[0] {
                            Arg::CompareOp(CompareOp::Eq) => {
                                let res = left == right;
                                println!(
                                    "-> {:?} == {:?} = {}",
                                    left, right, res
                                );
                                m.set(
                                    2,
                                    TypeValue::Builtin(
                                        BuiltinTypeValue::Boolean(Boolean(
                                            Some(res),
                                        )),
                                    ),
                                );
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::MemPos(2))?;
                            }
                            Arg::CompareOp(CompareOp::Ne) => {
                                let res = left != right;
                                println!(
                                    "-> {:?} != {:?} = {}",
                                    left, right, res
                                );
                                m.set(
                                    2,
                                    TypeValue::Builtin(
                                        BuiltinTypeValue::Boolean(Boolean(
                                            Some(res),
                                        )),
                                    ),
                                );
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::MemPos(2))?;
                            }
                            Arg::CompareOp(CompareOp::Lt) => {
                                let res = left < right;
                                println!(
                                    "-> {:?} < {:?} = {}",
                                    left, right, res
                                );
                                m.set(
                                    2,
                                    TypeValue::Builtin(
                                        BuiltinTypeValue::Boolean(Boolean(
                                            Some(res),
                                        )),
                                    ),
                                );
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::MemPos(2))?;
                            }
                            Arg::CompareOp(CompareOp::Le) => {
                                let res = left <= right;
                                println!(
                                    "-> {:?} <= {:?} = {}",
                                    left, right, res
                                );
                                m.set(
                                    2,
                                    TypeValue::Builtin(
                                        BuiltinTypeValue::Boolean(Boolean(
                                            Some(res),
                                        )),
                                    ),
                                );
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::MemPos(2))?;
                            }
                            Arg::CompareOp(CompareOp::Gt) => {
                                let res = left > right;
                                println!(
                                    "-> {:?} > {:?} = {}",
                                    left, right, res
                                );
                                m.set(
                                    2,
                                    TypeValue::Builtin(
                                        BuiltinTypeValue::Boolean(Boolean(
                                            Some(res),
                                        )),
                                    ),
                                );
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::MemPos(2))?;
                            }
                            Arg::CompareOp(CompareOp::Ge) => {
                                let res = left >= right;
                                println!(
                                    "-> {:?} >= {:?} = {}",
                                    left, right, res
                                );
                                m.set(
                                    2,
                                    TypeValue::Builtin(
                                        BuiltinTypeValue::Boolean(Boolean(
                                            Some(res),
                                        )),
                                    ),
                                );
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::MemPos(2))?;
                            }
                            Arg::CompareOp(CompareOp::Or) => {
                                let l = left.try_into()?;
                                let r = right.try_into()?;
                                let res = l || r;
                                println!(
                                    "-> {:?} || {:?} = {}",
                                    left, right, res
                                );
                                m.set(
                                    2,
                                    TypeValue::Builtin(
                                        BuiltinTypeValue::Boolean(Boolean(
                                            Some(res),
                                        )),
                                    ),
                                );
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::MemPos(2))?;
                            }
                            Arg::CompareOp(CompareOp::And) => {
                                let res =
                                    left.try_into()? && right.try_into()?;
                                println!(
                                    "-> {:?} && {:?} = {}",
                                    left, right, res
                                );
                                m.set(
                                    2,
                                    TypeValue::Builtin(
                                        BuiltinTypeValue::Boolean(Boolean(
                                            Some(res),
                                        )),
                                    ),
                                );
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::MemPos(2))?;
                            }
                            _ => panic!("invalid compare op"),
                        }
                    }
                    // stack args: [type, method_token, return memory position]
                    OpCode::ExecuteTypeMethod => {
                        println!("Stack {:?}", self.stack);
                        let mut m = mem.borrow_mut();

                        let mem_pos =
                            if let Arg::MemPos(pos) = args.pop().unwrap() {
                                pos as usize
                            } else {
                                return Err(VmError::InvalidValueType);
                            };
                        let stack_args =
                            self._unwind_resolved_stack_into_vec(&m);
                        let return_type = args.remove(2).into();

                        // We are going to call a method on a type, so we
                        // extract the type from the first argument on the
                        // stack.
                        if let Arg::Type(t) = &args[0] {
                            println!("-> with ");
                            stack_args
                                .iter()
                                .for_each(|a| print!("{:?}, ", a));
                            println!("\nwith return type {:?}", return_type);

                            if let Arg::Method(method_token) = args[1] {
                                let val = t.exec_type_method(
                                    method_token,
                                    &stack_args,
                                    return_type,
                                );
                                m.set(mem_pos, val);
                            } else {
                                return Err(VmError::InvalidValueType);
                            }
                        }
                    }
                    // stack args: [method_token, return_type, return memory position]
                    OpCode::ExecuteValueMethod => {
                        let mut m = mem.borrow_mut();

                        let mem_pos =
                            if let Arg::MemPos(pos) = args.pop().unwrap() {
                                pos as usize
                            } else {
                                return Err(VmError::InvalidValueType);
                            };
                        let return_type = args.pop().unwrap().into();
                        let method_token: Arg = args.pop().unwrap();

                        // pop all refs from the stack and resolve them to
                        // their values.
                        let stack_args =
                            self._unwind_resolved_stack_into_vec(&m);

                        // The first value on the stack is the value which we
                        // are going to call a method with.
                        let call_value = *stack_args.get(0).unwrap();
                        let v = call_value.exec_value_method(
                            method_token.into(),
                            &stack_args[1..],
                            return_type,
                        );
                        m.set(mem_pos, v);
                    }
                    // args: [data_source_token, method_token, return memory position]
                    OpCode::ExecuteDataStoreMethod => {
                        let mut m = mem.borrow_mut();

                        let mem_pos = args.pop().unwrap().into();
                        let method_token: Arg = args.pop().unwrap();
                        let data_source_token = args.pop().unwrap();

                        if let Arg::DataSourceTable(ds_s)
                        | Arg::DataSourceRib(ds_s) = data_source_token
                        {
                            let ds = self.get_data_source(ds_s).unwrap();
                            let stack_args =
                                self._unwind_resolved_stack_into_vec(&m);

                            let v = ds.exec_method(
                                method_token.into(),
                                &stack_args[..],
                                TypeDef::None,
                            );
                            let mut s = self.stack.borrow_mut();
                            match v {
                                DataSourceMethodValue::Ref(sr_pos) => {
                                    s.push(sr_pos)?;
                                }
                                DataSourceMethodValue::TypeValue(tv) => {
                                    m.set(mem_pos, tv);
                                }
                            }
                            drop(s);
                        }

                        drop(m);
                    }
                    // stack args: [mem_pos, constant_value]
                    OpCode::PushStack => match args[0] {
                        Arg::MemPos(pos) => {
                            print!(
                                " content: {:?}",
                                mem.borrow().get(pos as usize)
                            );

                            let mut s = self.stack.borrow_mut();
                            s.push(StackRefPos::MemPos(pos))?;
                            drop(s);
                        }
                        _ => return Err(VmError::InvalidValueType),
                    },
                    // no stack_args
                    OpCode::PopStack => {
                        if args.is_empty() {
                            let mut s = self.stack.borrow_mut();
                            s.pop();
                            drop(s);
                        } else {
                            return Err(VmError::InvalidValueType);
                        }
                    }
                    // no stack_args
                    OpCode::ClearStack => {
                        let mut s = self.stack.borrow_mut();
                        s.clear();
                        drop(s);
                    }
                    // stack args: [mem_pos, constant_value]
                    OpCode::MemPosSet => {
                        if let Arg::MemPos(pos) = args[0] {
                            if let Some(Arg::Constant(v)) = args.get_mut(1) {
                                let v = std::mem::take(v);
                                let mut m = mem.borrow_mut();
                                m.set(pos as usize, v);
                                drop(m);
                            } else {
                                return Err(VmError::InvalidValueType);
                            }
                        } else {
                            return Err(VmError::InvalidValueType);
                        }
                    }
                    // stack args: [field_index]
                    OpCode::StackOffset => {
                        for arg in args {
                            if let Arg::FieldAccess(field) = arg {
                                let mut s = self.stack.borrow_mut();
                                s.set_field_index(field)?;
                                drop(s);
                            } else {
                                return Err(VmError::InvalidValueType);
                            }
                        }
                    }
                    // stack args: [mem_pos, variable_token]
                    OpCode::MemPosRef => {
                        if let Arg::MemPos(pos) = args[0] {
                            self.variables.borrow_mut().set(
                                args[1].as_token_value(),
                                pos,
                                0,
                            )?;
                        } else {
                            return Err(VmError::InvalidValueType);
                        }
                    }
                    // stack args: [arg_token_value, mem_pos]
                    OpCode::ArgToMemPos => {
                        if let Arg::MemPos(pos) = args[1] {
                            match args[0] {
                                Arg::Argument(token_value) => {
                                    let (arg_index, arg_value) = arguments
                                        .take_by_token_value(token_value)?;

                                    let mut m = mem.borrow_mut();
                                    m.set(pos as usize, arg_value);
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
                        let m = mem.borrow();
                        let s = self.stack.borrow();
                        let stack_ref = s.get_top_value()?;
                        let bool_val = m.get_by_stack_ref(stack_ref).unwrap();
                        if bool_val.is_false()? {
                            print!(" skip to end of block");
                            break;
                        } else {
                            print!(" continue");
                            continue;
                        }
                    }
                    // stack args ignored
                    OpCode::CondTrueSkipToEOB => {
                        let m = mem.borrow();
                        let s = self.stack.borrow();
                        let stack_ref = s.get_top_value()?;
                        let bool_val = m.get_by_stack_ref(stack_ref).unwrap();
                        if bool_val.is_false()? {
                            print!(" continue");
                            continue;
                        } else {
                            print!(" skip to end of block");
                            break;
                        }
                    }
                    // stack args: [exit value]
                    OpCode::Exit => {
                        todo!();
                    }
                    // stack args: [vec[rx type instance field], new value]
                    OpCode::SetRxField => {
                        todo!();
                    }
                    // stack args: [tx type instance field, new value]
                    OpCode::SetTxField => {
                        todo!();
                    }
                };
            }
            println!("\n\n(end) stack: {:?}", self.stack);
            println!("mem  : {:?}", &mem.borrow().0[..10]);
        }

        let m = mem.borrow();
        for (i, addr) in m.0.as_slice().iter().enumerate() {
            if !addr.is_empty() {
                println!("{}: {}", i, addr);
            }
        }

        println!("vars {:?}", self.variables);
        println!(
            "\n🍺 Done! Successfully executed {} instruections.",
            commands_num
        );
        Ok(())
    }
}

#[derive(Default)]
pub struct VmBuilder<'a> {
    rx_type: TypeDef,
    tx_type: Option<TypeDef>,
    arguments: ArgumentsMap,
    data_sources: &'a [&'a ExtDataSource],
}

impl<'a> VmBuilder<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_arguments(mut self, args: ArgumentsMap) -> Self {
        self.arguments = args;
        self
    }

    pub fn with_data_sources(
        mut self,
        data_sources: &'a [&ExtDataSource],
    ) -> Self {
        self.data_sources = data_sources;
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

    pub fn build(
        self,
        variables: &'a RefCell<VariablesMap>,
    ) -> VirtualMachine<'a> {
        VirtualMachine {
            rx_type: self.rx_type,
            tx_type: self.tx_type,
            // arguments: self.arguments,
            variables,
            data_sources: self.data_sources,
            stack: RefCell::new(Stack::new()),
            // memory,
        }
    }
}

#[derive(Debug)]
pub enum VmError {
    StackUnderflow,
    StackOverflow,
    MemOutOfBounds,
    InvalidMemoryAccess(usize, Option<usize>),
    ArgumentNotFound,
    InvalidValueType,
    InvalidVariableAccess,
    InvalidFieldAccess(usize),
    InvalidMethodCall,
    DataSourceNotFound(usize),
    ImpossibleComparison,
}

#[derive(Debug)]
pub struct Command {
    pub(crate) op: OpCode,
    pub(crate) args: Vec<Arg>,
}

impl Command {
    pub fn new(op: OpCode, args: Vec<Arg>) -> Self {
        Command { op, args }
    }
}

impl Display for Command {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let arrow = match self.op {
            OpCode::Cmp => "<->",
            OpCode::ExecuteTypeMethod => "=>",
            OpCode::ExecuteDataStoreMethod => "=>",
            OpCode::ExecuteValueMethod => "=>",
            OpCode::PushStack => "<-",
            OpCode::PopStack => "->",
            OpCode::ClearStack => "::",
            OpCode::MemPosSet => "->",
            OpCode::MemPosRef => "",
            OpCode::ArgToMemPos => "->",
            OpCode::StackOffset => "",
            OpCode::CondFalseSkipToEOB => "-->",
            OpCode::CondTrueSkipToEOB => "-->",
            OpCode::Label => {
                return write!(f, "🏷  {}", self.args[0]);
            }
            OpCode::Exit => ".",
            OpCode::SetRxField => "->",
            OpCode::SetTxField => "->",
        };
        write!(f, "{:?}{}{:?}", self.op, arrow, self.args)
    }
}

#[derive(Debug)]
pub enum Arg {
    Constant(TypeValue),       // Constant value
    Variable(usize),           // Variable with token value
    Argument(usize),           // extra runtime arguments
    RxValue, // the placeholder for the value of the rx type at runtime
    TxValue, // the placeholder for the value of the tx type at runtime
    Method(usize), // method token value
    DataSourceTable(usize), // data source: table token value
    DataSourceRib(usize), // data source: rib token value
    FieldAccess(usize), // field access token value
    BuiltinMethod(usize), // builtin method token value
    MemPos(u32), // memory position
    Type(TypeDef), // type definition
    Boolean(bool), // boolean value (used in cmp opcode)
    Term(usize), // term token value
    CompareOp(ast::CompareOp), // compare operation
    Label(ShortString), // a label with its name (to jump to)
    Exit,    // exit the vm
}

impl Arg {
    pub fn as_token_value(&self) -> usize {
        match self {
            Arg::Argument(v) => *v,
            Arg::Variable(v) => *v,
            _ => {
                panic!("Cannot get token value from this arg: {:?} and that's fatal", self);
            }
        }
    }
}

impl Display for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Arg::Label(v) => write!(f, "{}", v),
            _ => write!(f, ""),
        }
    }
}

impl From<Arg> for TypeDef {
    fn from(value: Arg) -> Self {
        match value {
            Arg::Type(t) => t,
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
impl From<Arg> for usize {
    fn from(value: Arg) -> Self {
        match value {
            Arg::Method(m) => m,
            Arg::DataSourceTable(d) => d,
            Arg::DataSourceRib(d) => d,
            Arg::FieldAccess(f) => f,
            Arg::BuiltinMethod(b) => b,
            Arg::Variable(v) => v,
            // Arg::DataStore(d) => d,
            Arg::MemPos(m) => m as usize,
            _ => {
                panic!(
                    "Cannot convert to usize {:?} and that's fatal.",
                    value
                );
            }
        }
    }
}

impl From<ast::CompareOp> for Arg {
    fn from(op: ast::CompareOp) -> Self {
        Arg::CompareOp(op)
    }
}

impl From<crate::traits::Token> for Vec<Arg> {
    fn from(to: crate::traits::Token) -> Self {
        if let Token::FieldAccess(v) = to {
            v.iter()
                .map(|f| Arg::FieldAccess(*f as usize))
                .collect::<Vec<_>>()
        } else {
            panic!("PANIC")
        }
    }
}

#[derive(Debug)]
pub enum OpCode {
    Cmp,
    ExecuteTypeMethod,
    ExecuteDataStoreMethod,
    ExecuteValueMethod,
    PopStack,
    PushStack,
    ClearStack,
    StackOffset,
    MemPosSet,
    MemPosRef,
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
    Exit,
}

// struct VecPayload(Vec<(ShortString, TypeValue)>);

// impl Payload for VecPayload {
//     fn set(&mut self, field: ShortString, data: TypeValue) {
//         let field = &mut self.0.iter().find(|k| k.0 == field);
//         let key = field.unwrap().0.clone();
//         let _old = std::mem::replace(field, Some(&(key, data)));
//     }

//     fn get(&self, field: ShortString) -> Option<&TypeValue> {
//         self.0.iter().find(|k| k.0 == field).map(|kv| &kv.1)
//     }

//     fn take_value(self) -> TypeValue {
//         self
//     }
// }

pub trait Payload {
    fn set(&mut self, field: ShortString, value: TypeValue);
    fn get(&self, field: ShortString) -> Option<&TypeValue>;
    fn take_value(self) -> TypeValue;
}

#[derive(Debug)]
pub enum DataSource {
    Table(Table),
    Rib(Rib),
}

trait Source {
    fn new(name: &str, token: usize, ty: TypeDef) -> Self;
    fn get_by_key<'a>(&'a self, key: &str) -> Option<&'a Record>;
    fn exec_ref_value_method<'a>(
        &self,
        method_token: usize,
        args: &[&'a TypeValue],
        res_type: TypeDef,
    ) -> DataSourceMethodValue;
}

#[derive(Debug)]
pub struct ExtDataSource {
    name: ShortString,
    token: usize,
    source: DataSource,
}

impl ExtDataSource {
    pub fn new(name: &str, token: Token, ty: TypeDef) -> Self {
        ExtDataSource {
            name: name.into(),
            source: match &token {
                Token::Table(_) => DataSource::Table(Table {
                    ty,
                    records: vec![],
                }),
                Token::Rib(_) => DataSource::Rib(Rib {
                    ty,
                    records: vec![],
                }),
                _ => {
                    panic!("Invalid data source type: {:?}", ty);
                }
            },
            token: token.into(),
        }
    }

    pub fn get_by_key<'a>(&'a self, key: &str) -> Option<&'a Record> {
        match self.source {
            DataSource::Table(ref t) => t.records.iter().find(|v| {
                v.get_field_by_index(0).map(|v| v.0.as_str()) == Some(key)
            }),
            DataSource::Rib(ref r) => {
                todo!()
            }
        }
    }

    pub fn get_at_field_index(
        &self,
        index: usize,
        field_index: Option<usize>,
    ) -> Option<&TypeValue> {
        match self.source {
            DataSource::Table(ref t) => {
                t.get_at_field_index(index, field_index)
            }
            DataSource::Rib(ref r) => {
                todo!()
            }
        }
    }

    // methods on a data source can indicate whether they are returning a
    // value created by the method or a reference to a value in the data
    // source itself, through the TableMethodValue enum.
    pub(crate) fn exec_method<'a>(
        &self,
        method_token: usize,
        args: &[&'a TypeValue],
        res_type: TypeDef,
    ) -> DataSourceMethodValue {
        println!(
            "exec_method: {:?} {:?} {:?} {:?}",
            self.source, method_token, args, res_type
        );

        match self.source {
            DataSource::Table(ref t) => {
                t.exec_ref_value_method(method_token, args, res_type)()
            }
            DataSource::Rib(ref r) => {
                r.exec_ref_value_method(method_token, args, res_type)()
            }
        }
    }
}

impl Index<Token> for [&'_ ExtDataSource] {
    type Output = ExtDataSource;

    fn index(&self, index: Token) -> &Self::Output {
        if let Token::Table(token) = index {
            self[token]
        } else {
            panic!("Cannot index with {:?}", index);
        }
    }
}

// pub fn compile_filter<'a, I: Payload, O: Payload>(
//     symbols: &mut SymbolTable,
//     available_sources: ExtSources,
// ) -> impl Fn(&mut I, &mut O) -> Result<AcceptReject, Box<dyn std::error::Error>> + 'a
// {
//     let _used_source = available_sources.0[0].clone();

//     move |input: &mut I,
//           output: &mut O|
//           -> Result<AcceptReject, Box<dyn std::error::Error>> {
//         input.set("bla".into(), TypeValue::try_from("blub").unwrap());
//         output.set("bla".into(), TypeValue::try_from("blub").unwrap());
//         Ok(AcceptReject::Accept)
//     }
// }
