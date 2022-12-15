use std::{
    cell::{Ref, RefCell},
    fmt::{Display, Formatter},
    ops::{Index, IndexMut},
    rc::Rc,
    sync::Arc,
};

use crate::{
    ast::ShortString,
    compile::MirBlock,
    types::{
        collections::ElementTypeValue, typedef::TypeDef, typevalue::TypeValue,
    },
};

#[derive(Debug)]
struct StackRef {
    mem_pos: usize,
    field_index: Option<usize>,
}

#[derive(Debug)]
struct Stack(Vec<StackRef>);

impl<'a> Stack {
    fn new() -> Self {
        Stack(Vec::new())
    }

    fn push(&'a mut self, pos: usize) -> Result<(), VmError> {
        self.0.push(StackRef {
            mem_pos: pos,
            field_index: None,
        });
        Ok(())
    }

    fn pop(&'a mut self, mem: Ref<LinearMemory>) -> Result<(), VmError> {
        let pos = self.0.pop().ok_or(VmError::StackUnderflow)?.mem_pos;
        mem.get(pos).ok_or(VmError::StackUnderflow)?;
        Ok(())
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

    fn get(&self, index: usize) -> Option<&TypeValue> {
        self.0.get(index)
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
    data_sources: DataSources,
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

    fn _unwind_resolved_stack_into_vec(&'a self, mem: &'a LinearMemory) -> Vec<&'a TypeValue> {
        let mut stack = self.stack.borrow_mut();
        stack
        .unwind()
        .into_iter()
        .map(|sr| {
            mem.get_at_field_index(
                sr.mem_pos,
                sr.field_index,
            )
            .ok_or(VmError::InvalidMemoryAccess(
                sr.mem_pos,
                sr.field_index,
            ))
            .unwrap()
        })
        .collect::<Vec<_>>()
    }

    pub fn exec(
        &'a mut self,
        rx: impl Payload,
        tx: Option<impl Payload>,
        mut arguments: Rc<ArgumentsMap>,
        mem: RefCell<LinearMemory>,
        // data_sources: DataSources,
        mir_code: Vec<MirBlock>,
    ) -> Result<(), VmError> {
        println!("\nstart executing vm...");
        let mut commands_num: usize = 0;

        self._move_rx_tx_to_mem(rx, tx, &mem);

        for MirBlock { command_stack } in mir_code {
            println!("\n\n--mirblock------------------");

            let mut s = self.stack.borrow_mut();
            s.clear();
            drop(s);

            println!("stack: {:?}", self.stack);
            for Command { op, mut args } in command_stack {
                commands_num += 1;
                print!("\n-> {:3?} {:?} ", op, args);
                match op {
                    OpCode::Cmp => todo!(),
                    // args: [type, method_token, return memory position]
                    OpCode::ExecuteTypeMethod => {
                        let m = mem.borrow();
                        let method_args = self._unwind_resolved_stack_into_vec(&m);
                        let return_type = args.remove(2).into();
                        if let Arg::Type(t) = &args[0] {
                            println!("-> with ");
                            method_args
                                .iter()
                                .for_each(|a| print!("{:?}, ", a));
                            println!("\nwith return type {:?}", return_type);
                            if let Arg::Method(method_token) = args[1] {
                                t.exec_type_method(
                                    method_token,
                                    method_args,
                                    return_type,
                                );
                            } else {
                                return Err(VmError::InvalidValueType);
                            }
                        }
                    }
                    // args: [method_token, return_type, return memory position]
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
                        let method_args = self._unwind_resolved_stack_into_vec(&m);

                        // The first value on the stack is the value which we
                        // are going to call a method with.
                        let call_value = *method_args.get(0).unwrap();
                        let v = call_value.exec_value_method(
                            method_token.into(),
                            &method_args[1..],
                            return_type,
                        );
                        m.set(mem_pos, v);
                    }
                    OpCode::ExecuteDataStoreMethod => {}
                    OpCode::PushStack => {
                        if let Arg::MemPos(pos) = args[0] {
                            let mut s = self.stack.borrow_mut();
                            s.push(pos as usize)?;
                            drop(s);
                        } else {
                            return Err(VmError::InvalidValueType);
                        }
                    }
                    OpCode::PopStack => {
                        if args.is_empty() {
                            let mut s = self.stack.borrow_mut();
                            let m = mem.borrow();
                            s.pop(m)?;
                            drop(s);
                        } else {
                            return Err(VmError::InvalidValueType);
                        }
                    }
                    // args: [mem_pos, constant_value]
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
                    // args: [field_index]
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
                    OpCode::MemPosOffset => {
                        unimplemented!()
                    }
                    // args: mem_pos, variable_token
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
                    // 2 arguments: [arg_token_value, mem_pos]
                    OpCode::ArgToMemPos => {
                        if let Arg::MemPos(pos) = args[1] {
                            match args[0] {
                                Arg::Argument(arg) => {
                                    let a =
                                        Rc::get_mut(&mut arguments).unwrap();
                                    let v = a
                                        .take_by_token_value(arg as usize)
                                        .unwrap();
                                    let mut m = mem.borrow_mut();
                                    println!("v: {:?}", v);
                                    m.set(pos as usize, v.1);
                                }
                                _ => {
                                    return Err(VmError::InvalidValueType);
                                }
                            }
                        } else {
                            return Err(VmError::InvalidValueType);
                        }
                    }
                };
            }
            println!("\n\n(end) stack: {:?}", self.stack);
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
pub struct VmBuilder {
    rx_type: TypeDef,
    tx_type: Option<TypeDef>,
    arguments: ArgumentsMap,
    data_sources: DataSources,
}

impl VmBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_arguments(mut self, args: ArgumentsMap) -> Self {
        self.arguments = args;
        self
    }

    pub fn with_data_sources(mut self, data_sources: DataSources) -> Self {
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
        variables: &'_ RefCell<VariablesMap>,
    ) -> VirtualMachine<'_> {
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
            OpCode::ExecuteDataStoreMethod => "",
            OpCode::ExecuteValueMethod => "=>",
            OpCode::PushStack => "<-",
            OpCode::PopStack => "->",
            OpCode::MemPosSet => "->",
            OpCode::MemPosOffset => "",
            OpCode::MemPosRef => "",
            // OpCode::PushArgStack => "->",
            OpCode::ArgToMemPos => "->",
            OpCode::StackOffset => "",
        };
        write!(f, "{:?}{}{:?}", self.op, arrow, self.args)
    }
}

#[derive(Debug)]
pub enum Arg {
    Constant(TypeValue),
    Variable(usize),
    Argument(usize),
    RxValue, // the placeholder for the value of the rx type at runtime
    TxValue, // the placeholder for the value of the tx type at runtime
    Method(usize),
    DataSource(usize),
    FieldAccess(usize),
    BuiltinMethod(usize),
    DataStore(usize),
    MemPos(u32),
    Type(TypeDef),
}

impl Arg {
    pub fn as_token_value(&self) -> usize {
        match self {
            Arg::Argument(v) => *v as usize,
            Arg::Variable(v) => *v as usize,
            _ => {
                println!("Cannot get token value from this arg: {:?}", self);
                panic!("..and that's fatal.");
            }
        }
    }
}

impl From<Arg> for TypeDef {
    fn from(value: Arg) -> Self {
        match value {
            Arg::Type(t) => t,
            _ => {
                println!("Cannot convert to TypeDef: {:?}", value);
                panic!("..and that's fatal.");
            }
        }
    }
}

// extract the token value from an argument
impl From<Arg> for usize {
    fn from(value: Arg) -> Self {
        match value {
            Arg::Method(m) => m,
            Arg::DataSource(d) => d,
            Arg::FieldAccess(f) => f,
            Arg::BuiltinMethod(b) => b,
            Arg::DataStore(d) => d,
            Arg::MemPos(m) => m as usize,
            _ => {
                println!("Cannot convert to usize: {:?}", value);
                panic!("..and that's fatal.");
            }
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
    StackOffset,
    MemPosSet,
    MemPosOffset,
    MemPosRef,
    ArgToMemPos,
    // PushArgStack,
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

trait ExtSource {
    fn get(&self, key: &str) -> Option<TypeValue>;
}

#[derive(Clone, Default)]
pub struct DataSources(Vec<Arc<dyn ExtSource>>);

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
