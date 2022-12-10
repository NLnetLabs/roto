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
    types::{typedef::TypeDef, typevalue::TypeValue},
};

struct StackRef {
    mem_pos: usize,
    field_index: usize,
}

struct Stack(Vec<StackRef>);

impl<'a> Stack {
    fn new() -> Self {
        Stack(Vec::new())
    }

    fn push(&'a mut self, pos: usize) -> Result<(), VmError> {
        self.0.push(StackRef {
            mem_pos: pos,
            field_index: 0,
        });
        Ok(())
    }

    fn pop(&'a mut self, mem: Ref<LinearMemory>) -> Result<(), VmError> {
        let pos = self.0.pop().ok_or(VmError::StackUnderflow)?.mem_pos;
        let v = mem.get(pos).ok_or(VmError::StackUnderflow)?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct LinearMemory([TypeValue; 512]);

impl LinearMemory {
    pub fn new() -> Self {
        LinearMemory(std::array::from_fn(|_| TypeValue::None))
    }

    fn get(&self, index: usize) -> Option<&TypeValue> {
        self.0.get(index)
    }

    fn get_mut(&mut self, index: usize) -> Option<&mut TypeValue> {
        self.0.get_mut(index)
    }

    fn take(&mut self, index: usize) -> Option<TypeValue> {
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

    fn get_by_token_value(&self, index: usize) -> Option<&TypeValue> {
        self.0
            .iter()
            .find(|(t, _)| *t == index.into())
            .map(|(_, v)| v)
    }

    fn take_by_index(mut self, index: usize) -> Option<TypeValue> {
        self.0.get_mut(index).map(std::mem::take).map(|(_, v)| v)
    }

    pub fn new() -> Self {
        ArgumentsMap(Vec::new())
    }

    pub fn insert(&mut self, index: usize, value: TypeValue) {
        self.0.push((index, value));
    }

    pub fn last_index(&self) -> Option<usize> {
        if self.0.len() > 0 {
            Some(self.0.len())
        } else {
            None
        }
    }
}

pub struct VariableRef {
    var_token_value: usize,
    mem_pos: usize,
    field_index: usize,
}

pub struct VariablesMap(Vec<VariableRef>);

impl VariablesMap {
    fn get_by_token_value(&self, index: usize) -> Option<&VariableRef> {
        self.0.iter().find(
            |VariableRef {
                 var_token_value: t, ..
             }| { t == &index },
        )
    }

    pub fn set(
        &mut self,
        var_token_value: usize,
        mem_pos: usize,
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
    // arguments: ArgumentsMap,
    variables: &'a RefCell<VariablesMap>,
    data_sources: DataSources,
    stack: RefCell<Stack>,
    // memory: &'a Rc<RefCell<LinearMemory>>,
}

impl<'a> VirtualMachine<'a> {
    // fn push_ref_to_stack(
    //     &'a self,
    //     slot: usize,
    //     // mem: &'a Rc<RefCell<LinearMemory>>,
    // ) -> Result<(), VmError> {
    //     let m = Rc::clone(&self.memory);
    //     let m = m.borrow();
    //     self.stack
    //         .push(slot)?;
    //     Ok(())
    // }

    // fn pop_from_stack(&'a mut self, mem: &'a LinearMemory) -> Result<&'a TypeValue, VmError> {
    //     self.stack.pop(mem)
    // }

    // fn get_mem(
    //     &'a self,
    //     slot: usize,
    //     mem: &'a Ref<'a, LinearMemory>,
    // ) -> Result<&'a TypeValue, VmError> {
    //     Ok(mem.get(slot).ok_or(VmError::InvalidMemoryAccess(slot))?)
    // }

    // fn set_mem(
    //     &'a self,
    //     slot: usize,
    //     value: TypeValue,
    //     mem: RefMut<'a, LinearMemory>,
    // ) -> Result<(), VmError> {
    //     mem.set(slot, value);
    //     Ok(())
    // }

    // fn take_mem(
    //     &'a self,
    //     slot: usize,
    //     mut mem: RefMut<LinearMemory>,
    // ) -> Result<TypeValue, VmError> {
    //     let v = std::mem::take(
    //         mem.get_mut(slot)
    //             .ok_or(VmError::InvalidMemoryAccess(slot))?,
    //     );
    //     Ok(v)
    // }

    // fn create_var_ref(
    //     &'a self,
    //     var_token_value: usize,
    //     mem_pos: usize,
    //     field_index: usize,
    //     // value: &'a TypeValue,
    //     // mut vars: Rc<RefCell<VariablesMap<'a>>>,
    // ) -> Result<(), VmError> {
    //     // let v = Rc::get_mut(&mut vars).unwrap();
    //     // let v = v.borrow_mut();
    //     // let v = Rc::clone(self.variables);
    //     let mut v = self.variables.borrow_mut();
    //     // let value_ref = mem.get(mem_pos).ok_or(VmError::InvalidVariableAccess)?;
    //     v.set(var_token_value, mem_pos, field_index)?;
    //     Ok(())
    // }

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

        // put rx and tx into memory in the first two slots
        let rx = rx.take_value();
        println!("move rx to mem[0]: {}", rx);

        // let v = std::mem::take(v);
        let mut m = mem.borrow_mut();
        m.set(0, rx);

        if let Some(tx) = tx {
            let tx = tx.take_value();
            // let v = std::mem::take(v);
            // let mut m = mem.borrow_mut();
            println!("move tx to mem[1s]: {}", tx);
            m.set(1, tx);
        }

        drop(m);

        for MirBlock { command_stack } in mir_code {
            for Command { op, mut args } in command_stack {
                println!("-> {:3?} {:?}", op, args);
                match op {
                    OpCode::Cmp => todo!(),
                    OpCode::ExecuteTypeMethod => todo!(),
                    OpCode::ExecuteDataStoreMethod => todo!(),
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
                    // args: mem_pos, constant_value
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
                    // args: mem_pos, field_access_token
                    OpCode::MemPosOffset => {
                        if let Arg::FieldAccess(field) = args[1] {
                            if let Arg::MemPos(pos) = args[0] {
                                let mut m = mem.borrow_mut();
                                let mem_loc = m.take(pos as usize).ok_or(
                                    VmError::InvalidMemoryAccess(
                                        pos as usize,
                                    ),
                                )?;
                                println!("mem_loc: {:?}", mem_loc);
                                let v = mem_loc
                                    .get_field_by_index(field)
                                    .map_err(|e| {
                                        VmError::InvalidFieldAccess(field)
                                    })?;
                                m.set(pos as usize, v);
                            } else {
                                return Err(VmError::InvalidValueType);
                            }
                        } else {
                            return Err(VmError::InvalidValueType);
                        }
                    }
                    OpCode::MemPosRef => {
                        if let Arg::MemPos(pos) = args[0] {
                            self.variables.borrow_mut().set(
                                args[1].as_token_value(),
                                pos as usize,
                                0,
                            )?;
                        } else {
                            return Err(VmError::InvalidValueType);
                        }
                    }
                    // OpCode::PushArgStack => {
                    //     if let Arg::Argument(arg) = args[0] {
                    //         let v = self
                    //             .arguments
                    //             .get_by_token_value(arg as usize)
                    //             .unwrap();
                    //         self.stack.push(v)?;
                    //     }
                    // }
                    // 2 arguments: arg_token_value, mem_pos
                    OpCode::ArgToMemPos => {
                        // println!("args: {:?}", args);
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
                                // Arg::RxValue => {
                                //     let v = rx.take_value();
                                //     let v = std::mem::take(v);
                                //     let mut m = mem.borrow_mut();
                                //     m.set(pos as usize, v);
                                // }
                                // Arg::TxValue => {
                                //     if let Some(tx) = tx {
                                //         let v = tx.take_value();
                                //         let v = std::mem::take(v);
                                //         let mut m = mem.borrow_mut();
                                //         m.set(pos as usize, v);
                                //     } else {
                                //         return Err(VmError::InvalidValueType);
                                //     }
                                // }
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
        }
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
    InvalidMemoryAccess(usize),
    ArgumentNotFound,
    InvalidValueType,
    InvalidVariableAccess,
    InvalidFieldAccess(usize),
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
            OpCode::ExecuteTypeMethod => "->",
            OpCode::ExecuteDataStoreMethod => "",
            OpCode::PushStack => "<-",
            OpCode::PopStack => "->",
            OpCode::MemPosSet => "->",
            OpCode::MemPosOffset => "",
            OpCode::MemPosRef => "",
            // OpCode::PushArgStack => "->",
            OpCode::ArgToMemPos => "->",
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
            _ => panic!("Cannot get index from this token"),
        }
    }
}

#[derive(Debug)]
pub enum OpCode {
    Cmp,
    ExecuteTypeMethod,
    ExecuteDataStoreMethod,
    PopStack,
    PushStack,
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
