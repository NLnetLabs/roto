use std::fmt::{Formatter, Display};

use crate::types::{typevalue::TypeValue, typedef::TypeDef};

pub struct VirtualMachine {
    registers: Vec<TypeValue>,
    commands: Vec<Command>,
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
        write!(f, "{:?} {:?}", self.op, self.args)
    }
}

#[derive(Debug)]
pub enum Arg {
    Constant(TypeValue),
    Variable(usize),
    Argument(usize),
    Method(usize),
    DataSource(usize),
    FieldAccess(usize),
    BuiltinMethod(usize),
    DataStore(usize),
    MemPos(u32),
    Type(TypeDef)
}

#[derive(Debug)]
pub enum OpCode {
    Cmp,
    ExecuteTypeMethod,
    ExecuteDataStoreMethod,
    PopStack,
    PushStack,
    MemPosOffset,
    MemPosRef
}
