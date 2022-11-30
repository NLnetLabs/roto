use crate::types::typevalue::TypeValue;

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

#[derive(Debug)]
pub enum Arg {
    Register(usize),
    Constant(TypeValue),
}

#[derive(Debug)]
pub enum OpCode {
    Cmp,
    LoadConstant,
    DataSourceCommand,
    LoadArgument,
    ExecuteMethod,
    LoadField,
    LoadVar,
    StoreVar
}
