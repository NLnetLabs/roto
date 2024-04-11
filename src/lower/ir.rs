use std::fmt::Display;

use crate::ast::BinOp;

pub trait Value {
    fn as_unit(&self) {}
    fn as_bool(&self) -> bool;
    fn as_u8(&self) -> u8;
    fn as_u16(&self) -> u16;
    fn as_u32(&self) -> u32;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SafeValue {
    Unit,
    Bool(bool),
    U8(u8),
    U16(u16),
    U32(u32),
}

macro_rules! as_type {
    ($method:ident, $t:ident, $variant:ident) => {
        fn $method(&self) -> $t {
            let Self::$variant(x) = self else {
                panic!("Invalid value!");
            };
            *x
        }
    };
}

impl Value for SafeValue {
    as_type!(as_bool, bool, Bool);
    as_type!(as_u8, u8, U8);
    as_type!(as_u16, u16, U16);

    fn as_u32(&self) -> u32 {
        match self {
            SafeValue::U8(x) => *x as u32,
            SafeValue::U16(x) => *x as u32,
            SafeValue::U32(x) => *x as u32,
            _ => panic!("Invalid value!"),
        }
    }
}

impl<P> From<SafeValue> for Operand<P, SafeValue> {
    fn from(value: SafeValue) -> Self {
        Operand::Value(value)
    }
}

impl Display for SafeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SafeValue::Unit => write!(f, "Unit"),
            SafeValue::Bool(x) => write!(f, "Bool({x})"),
            SafeValue::U8(x) => write!(f, "U8({x})"),
            SafeValue::U16(x) => write!(f, "U16({x})"),
            SafeValue::U32(x) => write!(f, "U32({x})"),
        }
    }
}

/// Humand readable place
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Var {
    pub var: String,
}

impl<V> From<Var> for Operand<Var, V> {
    fn from(value: Var) -> Self {
        Operand::Place(value)
    }
}

impl Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.var)
    }
}

pub enum Operand<P, V> {
    Place(P),
    Value(V),
}

impl<P, V> Display for Operand<P, V>
where
    P: Display,
    V: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Place(x) => write!(f, "{x}"),
            Operand::Value(x) => write!(f, "{x}"),
        }
    }
}

pub enum Instruction<P, V> {
    Jump(String),
    Switch {
        examinee: Operand<P, V>,
        branches: Vec<(usize, String)>,
        default: String,
    },
    Assign {
        to: P,
        val: Operand<P, V>,
    },
    /// Call a function.
    ///
    /// Takes a block to jump to. Differs from just because it will store
    /// the current location to return to.
    Call(String),
    /// Return from the current "function" (filter-map, term or action)
    Return,

    /// Exit the program entirely
    Exit,

    /// Perform a binary operation and store the result in `to`
    BinOp {
        to: P,
        op: BinOp,
        left: Operand<P, V>,
        right: Operand<P, V>,
    },
}

impl<P, V> Display for Instruction<P, V>
where
    P: Display,
    Operand<P, V>: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assign { to, val } => write!(f, "{to} = {val}"),
            Self::Exit => write!(f, "exit"),
            Self::Call(name) => write!(f, "call {name}"),
            Self::Return => write!(f, "return"),
            Self::BinOp {
                to,
                op,
                left,
                right,
            } => {
                write!(f, "{to} = {left} {op} {right}")
            }
            Self::Jump(to) => {
                write!(f, "jump {to}")
            }
            Self::Switch {
                examinee,
                default,
                branches,
            } => {
                write!(
                    f,
                    "switch {examinee} [{}] else {default}",
                    branches
                        .iter()
                        .map(|(i, b)| format!("{i} => {b}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

pub struct Program<P, V> {
    pub blocks: Vec<Block<P, V>>,
}

impl<P, V> Display for Program<P, V>
where
    Block<P, V>: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut blocks = self.blocks.iter();

        let Some(b) = blocks.next() else {
            write!(f, "<empty program>")?;
            return Ok(());
        };

        write!(f, "{b}")?;

        for b in blocks {
            writeln!(f)?;
            write!(f, "{b}")?;
        }

        Ok(())
    }
}

pub struct Block<P, V> {
    pub label: String,
    pub instructions: Vec<Instruction<P, V>>,
}

impl<P, V> Display for Block<P, V>
where
    Instruction<P, V>: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, ".{}", self.label)?;
        for i in &self.instructions {
            writeln!(f, "  {i}")?
        }
        Ok(())
    }
}
