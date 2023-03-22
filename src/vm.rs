use std::{
    cell::RefCell,
    fmt::{Display, Formatter},
    ops::{Index, IndexMut},
};

use crate::{
    ast::{self, AcceptReject, CompareOp, ShortString},
    compile::{CompileError, MirBlock},
    traits::{RotoType, Token},
    types::{
        builtin::{Boolean, BuiltinTypeValue},
        collections::{ElementTypeValue, Record},
        datasources::{DataSourceMethodValue, Rib, Table},
        typedef::TypeDef,
        typevalue::TypeValue,
    },
};

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub(crate) struct StackRef {
    pub(crate) pos: StackRefPos,
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
    pub fn uninit() -> Self {
        LinearMemory(std::array::from_fn(|_| TypeValue::UnInit))
    }

    pub fn get_mem_pos(&self, index: usize) -> Option<&TypeValue> {
        self.0.get(index)
    }

    pub(crate) fn get_mp_field_by_stack_ref(
        &self,
        stack_ref: &StackRef,
    ) -> Option<&TypeValue> {
        if let StackRefPos::MemPos(pos) = stack_ref.pos {
            self.get_mp_field_by_index(pos as usize, stack_ref.field_index)
        } else {
            None
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
                None => self
                    .get_mem_pos_as_owned(*pos as usize)
                    .ok_or(VmError::MemOutOfBounds),
                Some(field_index) => match self
                    .get_mem_pos_as_owned(*pos as usize)
                {
                    Some(TypeValue::Record(mut r)) => {
                        let field = r.get_field_by_index_owned(*field_index);
                        match field {
                            ElementTypeValue::Nested(nested) => Ok(*nested),
                            ElementTypeValue::Primitive(b) => Ok(b),
                        }
                    }
                    Some(TypeValue::List(mut l)) => {
                        let field = l.get_field_by_index_owned(*field_index);
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
            _ => Err(VmError::MemOutOfBounds),
        }
    }

    pub fn get_mp_field_by_index(
        &self,
        index: usize,
        field_index: Option<usize>,
    ) -> Option<&TypeValue> {
        match field_index {
            None => self.get_mem_pos(index),
            Some(field_index) => match self.get_mem_pos(index) {
                Some(TypeValue::Record(rec)) => {
                    let field = rec.get_field_by_index(field_index);
                    match field {
                        Some(ElementTypeValue::Nested(nested)) => {
                            Some(nested)
                        }
                        Some(ElementTypeValue::Primitive(b)) => Some(b),
                        _ => None,
                    }
                }
                Some(TypeValue::List(l)) => {
                    let field = l.get_field_by_index(field_index);
                    match field {
                        Some(ElementTypeValue::Nested(nested)) => {
                            Some(nested)
                        }
                        Some(ElementTypeValue::Primitive(b)) => Some(b),
                        _ => None,
                    }
                }
                Some(TypeValue::Builtin(BuiltinTypeValue::Route(route))) => {
                    println!("A ROUTE: RUN TO THE HILLS!");
                    if let Some(v) = route.get_value_ref_for_field(field_index) {
                        println!("field {} in route: {}", field_index, route);
                       Some(v)
                    } else { 
                        println!("no field {} for route {}", field_index, route);
                        Some(&TypeValue::Unknown)
                    }
                }
                // This may be a type with fields, but its value is Unknown.
                // Return that so the caller can short-cut its chain.
                Some(TypeValue::Unknown) => Some(&TypeValue::Unknown),
                // This is apparently a type that does not have fields
                _ => None,
            },
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
    args: &'a Vec<Arg>,
    args_counter: usize,
}

impl<'a> CommandArgsStack<'a> {
    // The counter counts down from the last element + 1.
    fn new(args: &'a Vec<Arg>) -> Self {
        Self {
            args,
            args_counter: args.len(),
        }
    }

    // Return the last item and decrement the counter.
    fn pop(&mut self) -> Option<&'_ Arg> {
        self.args_counter -= 1;
        self.args.get(self.args_counter)
    }

    fn is_empty(&self) -> bool {
        self.args.is_empty()
    }

    // Interpret the whole stack as a constant value.
    pub(crate) fn take_arg_as_constant(
        &mut self,
    ) -> Result<TypeValue, VmError> {
        if let Some(Arg::Constant(v)) = self.args.get(1).cloned() {
            Ok(v)
        } else {
            Err(VmError::InvalidValueType)
        }
    }

    // Pop two arguments, the stack is gone after this.
    pub(crate) fn pop_2(mut self) -> (&'a Arg, &'a Arg) {
        self.args_counter -= 2;
        (
            self.args.get(self.args_counter + 1).unwrap(),
            self.args.get(self.args_counter).unwrap(),
        )
    }

    // Pop three arguments, the stack is gone after this.
    pub(crate) fn pop_3(mut self) -> (&'a Arg, &'a Arg, &'a Arg) {
        self.args_counter -= 3;
        (
            self.args.get(self.args_counter + 2).unwrap(),
            self.args.get(self.args_counter + 1).unwrap(),
            self.args.get(self.args_counter).unwrap(),
        )
    }
}

impl<'a> Index<usize> for CommandArgsStack<'a> {
    type Output = Arg;

    fn index(&self, index: usize) -> &Self::Output {
        self.args.index(index)
    }
}

impl<'a> From<&'a Vec<Arg>> for CommandArgsStack<'a> {
    fn from(value: &'a Vec<Arg>) -> Self {
        Self {
            args: value,
            args_counter: 0,
        }
    }
}

#[derive(Debug)]
pub struct Argument {
    name: ShortString,
    index: usize,
    ty: TypeDef,
    value: TypeValue,
}

impl Argument {
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
        name: ShortString,
        index: usize,
        ty: TypeDef,
        value: TypeValue,
    ) -> Self {
        Self {
            name,
            index,
            ty,
            value,
        }
    }
}

#[derive(Default, Debug)]
pub struct ArgumentsMap(Vec<Argument>);

impl ArgumentsMap {
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
        ArgumentsMap(Vec::new())
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
        self.0.push(Argument {
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

    pub fn iter(&self) -> std::slice::Iter<'_, Argument> {
        self.0.iter()
    }
}

impl IntoIterator for ArgumentsMap {
    type IntoIter = std::vec::IntoIter<Self::Item>;
    type Item = Argument;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl From<Vec<Argument>> for ArgumentsMap {
    fn from(value: Vec<Argument>) -> Self {
        Self(value)
    }
}

#[derive(Debug)]
pub struct VariableRef {
    pub var_token_value: usize,
    pub mem_pos: u32,
    pub field_index: Vec<usize>,
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
        field_index: Vec<usize>,
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

pub struct VirtualMachine<'a> {
    // _rx_type: TypeDef,
    // _tx_type: Option<TypeDef>,
    mir_code: &'a [MirBlock],
    data_sources: &'a [&'a ExtDataSource],
    arguments: ArgumentsMap,
    stack: RefCell<Stack>,
}

impl<'a> VirtualMachine<'a> {
    fn _move_rx_tx_to_mem(
        &mut self,
        rx: impl RotoType,
        tx: Option<impl RotoType>,
        mem: &RefCell<LinearMemory>,
    ) {
        let mut m = mem.borrow_mut();
        let rx = rx.take_value();
        m.set_mem_pos(0, rx.into());

        if let Some(tx) = tx {
            let tx = tx.take_value();
            m.set_mem_pos(1, tx.into());
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
                        .get_mp_field_by_index(pos as usize, sr.field_index)
                        .unwrap_or_else(|| {
                            println!("\nstack: {:?}", stack);
                            println!("mem: {:#?}", mem.0);
                            panic!(
                                "Uninitialized memory in position {}",
                                pos
                            );
                        });
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

    fn as_vec(&'a self) -> Vec<StackRef> {
        let mut stack = self.stack.borrow_mut();
        stack.unwind().into_iter().collect::<Vec<_>>()
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
        rx: impl RotoType,
        tx: Option<impl RotoType>,
        // define level arguments, not used yet! Todo
        mut _arguments: Option<ArgumentsMap>,
        mem: RefCell<LinearMemory>,
    ) -> Result<(AcceptReject, TypeValue, Option<TypeValue>), VmError>
    {
        println!("\nstart executing vm...");
        let mut commands_num: usize = 0;

        self._move_rx_tx_to_mem(rx, tx, &mem);

        for MirBlock {
            command_stack,
            ty: _,
        } in self.mir_code
        {
            println!("\n\n--mirblock------------------");

            println!("stack: {:?}", self.stack);
            for (pc, Command { op, args }) in command_stack.iter().enumerate()
            {
                commands_num += 1;
                let mut args = CommandArgsStack::new(args);
                print!("\n{:3} -> {:?} {:?} ", pc, op, args);
                match op {
                    OpCode::Cmp => {
                        let mut m = mem.borrow_mut();
                        let stack_args =
                            self._unwind_resolved_stack_into_vec(&m);
                        let left = stack_args[0];
                        let right = stack_args[1];

                        print!(" {:?} <-> {:?}", left, right);

                        match args[0] {
                            Arg::CompareOp(CompareOp::Eq) => {
                                let res = left == right;
                                m.set_mem_pos(
                                    0xff,
                                    TypeValue::Builtin(
                                        BuiltinTypeValue::Boolean(Boolean(
                                            res,
                                        )),
                                    ),
                                );
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::MemPos(0xff))?;
                            }
                            Arg::CompareOp(CompareOp::Ne) => {
                                let res = left != right;
                                m.set_mem_pos(
                                    0xff,
                                    TypeValue::Builtin(
                                        BuiltinTypeValue::Boolean(Boolean(
                                            res,
                                        )),
                                    ),
                                );
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::MemPos(0xff))?;
                            }
                            Arg::CompareOp(CompareOp::Lt) => {
                                let res = left < right;
                                m.set_mem_pos(
                                    0xff,
                                    TypeValue::Builtin(
                                        BuiltinTypeValue::Boolean(Boolean(
                                            res,
                                        )),
                                    ),
                                );
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::MemPos(0xff))?;
                            }
                            Arg::CompareOp(CompareOp::Le) => {
                                let res = left <= right;
                                m.set_mem_pos(
                                    0xff,
                                    TypeValue::Builtin(
                                        BuiltinTypeValue::Boolean(Boolean(
                                            res,
                                        )),
                                    ),
                                );
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::MemPos(0xff))?;
                            }
                            Arg::CompareOp(CompareOp::Gt) => {
                                let res = left > right;
                                m.set_mem_pos(
                                    0xff,
                                    TypeValue::Builtin(
                                        BuiltinTypeValue::Boolean(Boolean(
                                            res,
                                        )),
                                    ),
                                );
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::MemPos(0xff))?;
                            }
                            Arg::CompareOp(CompareOp::Ge) => {
                                let res = left >= right;
                                m.set_mem_pos(
                                    0xff,
                                    TypeValue::Builtin(
                                        BuiltinTypeValue::Boolean(Boolean(
                                            res,
                                        )),
                                    ),
                                );
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::MemPos(0xff))?;
                            }
                            Arg::CompareOp(CompareOp::Or) => {
                                let l = left.try_into()?;
                                let r = right.try_into()?;
                                let res = l || r;
                                m.set_mem_pos(
                                    0xff,
                                    TypeValue::Builtin(
                                        BuiltinTypeValue::Boolean(Boolean(
                                            res,
                                        )),
                                    ),
                                );
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::MemPos(0xff))?;
                            }
                            Arg::CompareOp(CompareOp::And) => {
                                let res =
                                    left.try_into()? && right.try_into()?;
                                m.set_mem_pos(
                                    0xff,
                                    TypeValue::Builtin(
                                        BuiltinTypeValue::Boolean(Boolean(
                                            res,
                                        )),
                                    ),
                                );
                                self.stack
                                    .borrow_mut()
                                    .push(StackRefPos::MemPos(0xff))?;
                            }
                            _ => panic!("{:3} -> invalid compare op", pc),
                        }
                    }
                    // stack args: [type, method_token, args, return memory position]
                    OpCode::ExecuteTypeMethod => {
                        println!("Stack {:?}", self.stack);
                        println!("Args {:?}", args);
                        let mut m = mem.borrow_mut();

                        let mem_pos =
                            if let Arg::MemPos(pos) = args.pop().unwrap() {
                                *pos as usize
                            } else {
                                return Err(VmError::InvalidValueType);
                            };
                        let (_args, method_t, return_type) = args.pop_3();

                        let stack_args =
                            self._unwind_resolved_stack_into_vec(&m);

                        // We are going to call a method on a type, so we
                        // extract the type from the first argument on the
                        // stack.
                        if let Arg::Type(t) = return_type {
                            let val = t.exec_type_method(
                                method_t.into(),
                                &stack_args,
                                return_type.into(),
                            );
                            m.set_mem_pos(mem_pos, val);
                        }
                    }
                    // stack args: [method_token, return_type,
                    //      arguments, result memory position
                    // ]
                    OpCode::ExecuteValueMethod => {
                        let mut m = mem.borrow_mut();

                        let mem_pos =
                            if let Arg::MemPos(pos) = args.pop().unwrap() {
                                *pos as usize
                            } else {
                                return Err(VmError::InvalidValueType);
                            };

                        let args_len: usize =
                            if let Some(Arg::Arguments(args)) = args.pop() {
                                args.len()
                            } else {
                                0
                            };

                        let (return_type, method_token) = args.pop_2();

                        // pop as many refs from the stack as we have
                        // arguments for this method and resolve them  to
                        // their values.
                        let mut stack = self.stack.borrow_mut();

                        let stack_args = [0..args_len].iter().map(|_i| {
                            let sr = stack.pop().unwrap();
                            match sr.pos {
                                StackRefPos::MemPos(pos) => {
                                    let v = m
                                        .get_mp_field_by_index(pos as usize, sr.field_index)
                                        .unwrap_or_else(|| {
                                            println!("\nstack: {:?}", stack);
                                            println!("mem: {:#?}", m.0);
                                            panic!("Uninitialized memory in position {}", pos);
                                        });
                                    v
                                }
                                StackRefPos::TablePos(token, pos) => {
                                    let ds = &self.data_sources[token];
                                    let v = ds.get_at_field_index(pos, sr.field_index);
                                    if let Some(v) = v {
                                        v
                                    } else { &TypeValue::Unknown }
                                }
                            }
                        }).collect::<Vec<_>>();

                        // The first value on the stack is the value which we
                        // are going to call a method with.
                        let call_value = *stack_args.get(0).unwrap();

                        let v = call_value.exec_value_method(
                            method_token.into(),
                            &stack_args[1..],
                            return_type.into(),
                        )?();

                        m.set_mem_pos(mem_pos, v);
                    }
                    // stack args: [
                    //      method_token, return_type,
                    //      arguments, result memory position
                    // ]
                    // pops arguments from the stack
                    OpCode::ExecuteConsumeValueMethod => {
                        let mut m = mem.borrow_mut();

                        let mem_pos =
                            if let Arg::MemPos(pos) = args.pop().unwrap() {
                                *pos as usize
                            } else {
                                return Err(VmError::InvalidValueType);
                            };

                        let args_len: usize =
                            if let Some(Arg::Arguments(args)) = args.pop() {
                                args.len()
                            } else {
                                0
                            };

                        println!("Args {:?}", args);
                        let (return_type, method_token) = args.pop_2();

                        // pop as many refs from the stack as we have
                        // arguments for this method and resolve them to
                        // their values.
                        let mut stack = self.stack.borrow_mut();

                        // the field_index of the first argument on the
                        // stack, this is the field that will be consumed
                        // and returned by `exec_consume_value_method`
                        // later on.
                        let mut target_field_index = None;

                        println!("\nargs_len {}", args_len);
                        println!("Stack {:?}", stack);
                        let mut stack_args = (0..args_len).into_iter().map(|_i| {
                            let sr = stack.pop().unwrap();

                            target_field_index = if target_field_index.is_none() {
                                sr.field_index
                            } else {
                                target_field_index
                            };

                            match sr.pos {
                                StackRefPos::MemPos(pos) => {
                                    m
                                        .get_mem_pos_as_owned(pos as usize)
                                        .unwrap_or_else(|| {
                                            println!("\nstack: {:?}", stack);
                                            println!("mem: {:#?}", m.0);
                                            panic!(r#"Uninitialized memory in 
                                                pos {}. That's fatal"#, pos);
                                        })
                                }
                                StackRefPos::TablePos(_token, _pos) => {
                                    panic!(r#"Can't mutate data in a data source. 
                                    That's fatal."#);
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
                                        target_field_index.unwrap(),
                                    ),
                                )
                                .exec_consume_value_method(
                                    method_token.into(),
                                    stack_args,
                                    return_type.into(),
                                    target_field_index,
                                )?(
                                );
                                rec.set_field_for_index(
                                    target_field_index.unwrap(),
                                    call_value,
                                )?;
                                TypeValue::Record(rec)
                            }
                            TypeValue::List(mut list) => {
                                let call_value = TypeValue::from(
                                    list.get_field_by_index_owned(
                                        target_field_index.unwrap(),
                                    )
                                    .unwrap(),
                                )
                                .exec_consume_value_method(
                                    method_token.into(),
                                    stack_args,
                                    return_type.into(),
                                    target_field_index,
                                )?(
                                );
                                list.set_field_for_index(
                                    target_field_index.unwrap(),
                                    call_value,
                                )?;
                                TypeValue::List(list)
                            }
                            _ => collection_value,
                        };

                        m.set_mem_pos(mem_pos, result_value);
                    }
                    // args: [data_source_token, method_token, arguments,
                    //       return memory position]
                    OpCode::ExecuteDataStoreMethod => {
                        let mut m = mem.borrow_mut();

                        let mem_pos =
                            if let Arg::MemPos(pos) = args.pop().unwrap() {
                                *pos as usize
                            } else {
                                return Err(VmError::InvalidValueType);
                            };

                        let _args_len: usize =
                            if let Some(Arg::Arguments(args)) = args.pop() {
                                args.len()
                            } else {
                                0
                            };

                        let (method_token, data_source_token) = args.pop_2();

                        if let Arg::DataSourceTable(ds_s)
                        | Arg::DataSourceRib(ds_s) = data_source_token
                        {
                            let ds = self.get_data_source(*ds_s).unwrap();
                            let stack_args =
                                self._unwind_resolved_stack_into_vec(&m);

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
                                    m.set_mem_pos(mem_pos, tv);
                                }
                                DataSourceMethodValue::Empty(_ty) => {
                                    m.set_mem_pos(
                                        mem_pos,
                                        TypeValue::Unknown,
                                    );
                                }
                            }
                        }
                    }
                    // stack args: [mem_pos, constant_value]
                    OpCode::PushStack => match args[0] {
                        Arg::MemPos(pos) => {
                            print!(
                                " content: {:?}",
                                mem.borrow().get_mem_pos(pos as usize)
                            );

                            let mut s = self.stack.borrow_mut();
                            s.push(StackRefPos::MemPos(pos))?;
                            println!(" stack {:?}", s);
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
                        if let Arg::MemPos(pos) = args[0] {
                            let v = args.take_arg_as_constant()?;
                            let mut m = mem.borrow_mut();
                            m.set_mem_pos(pos as usize, v);
                        } else {
                            return Err(VmError::InvalidValueType);
                        }
                    }
                    // stack args: [field_index]
                    OpCode::StackOffset => {
                        for arg in args.args.iter() {
                            if let Arg::FieldAccess(field) = arg {
                                let mut s = self.stack.borrow_mut();
                                s.set_field_index(*field)?;
                                print!(" -> stack {:?}", s);
                            } else {
                                return Err(VmError::InvalidValueType);
                            }
                        }
                    }
                    // stack args: [arg_token_value, mem_pos]
                    OpCode::ArgToMemPos => {
                        if let Arg::MemPos(pos) = args[1] {
                            match args[0] {
                                Arg::Argument(token_value) => {
                                    let arg_value = self
                                        .arguments
                                        .take_value_by_token(token_value)?;

                                    let mut m = mem.borrow_mut();
                                    m.set_mem_pos(pos as usize, arg_value);
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
                        let bool_val =
                            m.get_mp_field_by_stack_ref(stack_ref).unwrap();
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
                        let bool_val =
                            m.get_mp_field_by_stack_ref(stack_ref).unwrap();
                        if bool_val.is_false()? {
                            print!(" continue");
                            continue;
                        } else {
                            print!(" skip to end of block");
                            break;
                        }
                    }
                    // stack args: [exit value]
                    OpCode::Exit(accept_reject) => {
                        let mut m = mem.borrow_mut();

                        let rx: TypeValue = match m.get_mem_pos_as_owned(0) {
                            Some(TypeValue::Record(rec)) => rec.into(),
                            Some(TypeValue::Builtin(BuiltinTypeValue::Route(route))) => route.into(),
                            _ => return Err(VmError::InvalidPayload),
                        };

                        let tx = match m.get_mem_pos_as_owned(1) {
                            Some(TypeValue::Record(rec)) => Some(rec.into()),
                            _ => None,
                        };

                        if *accept_reject != AcceptReject::NoReturn {
                            println!("\n\nINITIALIZED MEMORY POSITIONS");
                            for (i, addr) in m.0.as_slice().iter().enumerate()
                            {
                                if !addr.is_unitialized() {
                                    println!("{}: {}", i, addr);
                                }
                            }

                            println!(
                                "\n🍺 Done! Successfully executed {} instructions.",
                                commands_num
                            );

                            return Ok((*accept_reject, rx, tx));
                        }
                    }

                    // SetRxField will replace the rx instance with the
                    // latest the value that is ref'ed by the top of the
                    // stack.

                    // stack args: [new value]
                    OpCode::SetRxField => {
                        let mut m = mem.borrow_mut();
                        // pop all refs from the stack and resolve them to
                        // their values.
                        let stack_args = self.as_vec();

                        // swap out the new value from memory
                        let val = m
                            .get_mp_field_by_stack_ref_owned(
                                stack_args.last().unwrap(),
                            )
                            .unwrap();

                        // save the value in memory position 0 (rx instance
                        // by definition).
                        m.set_mem_pos(0, val);
                    }
                    // stack args: [tx type instance field, new value]
                    OpCode::SetTxField => {
                        todo!();
                    }
                };
            }
            println!("\n\n(end) stack: {:?}", self.stack);
            println!("\nINITIALIZED MEMORY POSITIONS");
            for (i, addr) in mem.borrow().0.as_slice().iter().enumerate() {
                if !addr.is_unitialized() {
                    println!("{}: {}", i, addr);
                }
            }
        }

        Err(VmError::UnexpectedTermination)
    }
}

pub struct VmBuilder<'a> {
    rx_type: TypeDef,
    tx_type: Option<TypeDef>,
    mir_code: &'a [MirBlock],
    arguments: ArgumentsMap,
    data_sources: &'a [&'a ExtDataSource],
}

impl<'a> VmBuilder<'a> {
    pub fn new() -> Self {
        Self {
            rx_type: TypeDef::default(),
            tx_type: None,
            mir_code: &[],
            arguments: ArgumentsMap::default(),
            data_sources: &[],
        }
    }

    pub fn with_mir_code(mut self, mir_code: &'a [MirBlock]) -> Self {
        self.mir_code = mir_code;
        self
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

    pub fn build(self) -> VirtualMachine<'a> {
        VirtualMachine {
            // rx_type: self.rx_type,
            // tx_type: self.tx_type,
            mir_code: self.mir_code,
            data_sources: self.data_sources,
            arguments: self.arguments,
            stack: RefCell::new(Stack::new()),
        }
    }
}

impl<'a> Default for VmBuilder<'a> {
    fn default() -> Self {
        Self::new()
    }
}

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
    DataSourceNotFound(usize),
    ImpossibleComparison,
    InvalidWrite,
    InvalidConversion,
    UnexpectedTermination,
    AsPathTooLong,
    DeltaLocked,
}

impl From<VmError> for Box<dyn std::error::Error> {
    fn from(value: VmError) -> Self {
        format!("A VM Error occured: {:?}", value).into()
    }
}

#[derive(Debug, Clone)]
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
            OpCode::SetRxField => "->",
            OpCode::SetTxField => "->",
        };
        write!(f, "{:?}{}{:?}", self.op, arrow, self.args)
    }
}

#[derive(Debug)]
pub enum Arg {
    Constant(TypeValue),        // Constant value
    Variable(usize),            // Variable with token value
    Argument(usize),            // extra runtime argument for module & term
    RxValue, // the placeholder for the value of the rx type at runtime
    TxValue, // the placeholder for the value of the tx type at runtime
    Method(usize), // method token value
    DataSourceTable(usize), // data source: table token value
    DataSourceRib(usize), // data source: rib token value
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

impl From<&Arg> for TypeDef {
    fn from(value: &Arg) -> Self {
        match value {
            Arg::Type(t) => t.clone(),
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
impl From<&Arg> for usize {
    fn from(value: &Arg) -> Self {
        match value {
            Arg::Method(m) => *m,
            Arg::DataSourceTable(d) => *d,
            Arg::DataSourceRib(d) => *d,
            Arg::FieldAccess(f) => *f,
            Arg::BuiltinMethod(b) => *b,
            Arg::Variable(v) => *v,
            Arg::MemPos(m) => *m as usize,
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

// Cloning an Arg works only it Arg::Constants does NOT contain a
// (used-defined) Record or List
impl Clone for Arg {
    fn clone(&self) -> Self {
        match self {
            Arg::Constant(c) => {
                Arg::Constant(c.builtin_as_cloned_type_value().unwrap())
            }
            Arg::Variable(v) => Arg::Variable(*v),
            Arg::Argument(a) => Arg::Argument(*a),
            Arg::RxValue => Arg::RxValue,
            Arg::TxValue => Arg::TxValue,
            Arg::Method(m) => Arg::Method(*m),
            Arg::DataSourceTable(ds) => Arg::DataSourceTable(*ds),
            Arg::DataSourceRib(ds) => Arg::DataSourceTable(*ds),
            Arg::FieldAccess(fa) => Arg::FieldAccess(*fa),
            Arg::BuiltinMethod(bim) => Arg::BuiltinMethod(*bim),
            Arg::MemPos(mp) => Arg::MemPos(*mp),
            Arg::Type(ty) => Arg::Type(ty.clone()),
            Arg::Arguments(args) => Arg::Arguments(args.to_vec()),
            Arg::Boolean(b) => Arg::Boolean(*b),
            Arg::Term(t) => Arg::Term(*t),
            Arg::CompareOp(op) => Arg::CompareOp(*op),
            Arg::Label(l) => Arg::Label(l.clone()),
            Arg::AcceptReject(ar) => Arg::AcceptReject(*ar),
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
    Exit(AcceptReject),
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

// pub trait Payload
// where
//     Self: std::fmt::Debug + std::fmt::Display,
// {
//     fn set_field(&mut self, field: ShortString, value: TypeValue);
//     fn get(&self, field: ShortString) -> Option<&TypeValue>;
//     fn take_value(self) -> TypeValue;
// }

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
    _name: ShortString,
    token: usize,
    source: DataSource,
}

impl ExtDataSource {
    pub fn new(name: &str, token: Token, ty: TypeDef) -> Self {
        ExtDataSource {
            _name: name.into(),
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

    // pub fn get_by_key<'a>(&'a self, key: &str) -> Option<&'a Record> {
    //     match self.source {
    //         DataSource::Table(ref t) => t.records.iter().find(|v| {
    //             v.get_field_by_index(0).map(|v| v.0.as_str()) == Some(key)
    //         }),
    //         DataSource::Rib(ref r) => {
    //             todo!()
    //         }
    //     }
    // }

    pub fn get_at_field_index(
        &self,
        index: usize,
        field_index: Option<usize>,
    ) -> Option<&TypeValue> {
        match self.source {
            DataSource::Table(ref t) => {
                t.get_at_field_index(index, field_index)
            }
            DataSource::Rib(ref _r) => {
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
