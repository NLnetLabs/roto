//! High-level intermediate representation (HIR)
//!
//! The high-level intermediate IR is the first IR after type checking.
//! Its purpose is to be simple yet human-readable. Evaluating it does not
//! need to be particularly fast yet, but the evaluation is safe in the
//! sense that it in the case anything unexpected happens (e.g the wrong
//! type being given) it will panic instead of performing undefined
//! behaviour. By evaluating the HIR, we can run tests to test this
//! compilation step.
//!
//! The HIR has the following characteristics:
//!
//!  - Human-readable names for all variables and fields.
//!  - The names of all variables are global.
//!  - Blocks are also identified by readable labels.
//!  - Values are a tagged enum and types are checked at runtime.
//!  - Records and lists are heap allocated, hence the size of values does
//!    need to be known to construct the HIR.
//!  - Expressions are simple (as opposed to complex).
//!  - Control flow is represented with basic blocks.
//!
//! The instructions in the HIR are inspired by the instructions defined by
//! [cranelift].
//!
//! [cranelift]: https://docs.rs/cranelift-frontend/latest/cranelift_frontend/

use std::{fmt::Display, rc::Rc};

use crate::ast::BinOp;

pub trait Value {
    fn as_unit(&self) {}
    fn as_bool(&self) -> bool;
    fn as_u8(&self) -> u8;
    fn as_u16(&self) -> u16;
    fn as_u32(&self) -> u32;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SafeValue {
    Unit,
    Bool(bool),
    U8(u8),
    U16(u16),
    U32(u32),
    Record(Rc<Vec<(String, SafeValue)>>),
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
            SafeValue::Record(fields) => write!(
                f,
                "{{{}}}",
                fields
                    .iter()
                    .map(|(f, v)| format!("{f}: {v}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

/// Humand readable place
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone)]
pub enum Instruction<P, V> {
    /// Jump to a block
    Jump(String),

    /// Switch on the integer value of the `examinee`
    Switch {
        examinee: Operand<P, V>,
        branches: Vec<(usize, String)>,
        default: String,
    },

    /// Assign the value `val` to `P`.
    Assign { to: P, val: Operand<P, V> },

    /// Call a function.
    Call(P, String, Vec<(String, Operand<P, V>)>),

    /// Return from the current "function" (filter-map, term or action)
    Return(Operand<P, V>),

    /// Perform a binary operation and store the result in `to`
    BinOp {
        to: P,
        op: BinOp,
        left: Operand<P, V>,
        right: Operand<P, V>,
    },

    /// Access a record field
    Access { to: P, record: P, field: String },
}

impl<P, V> Display for Instruction<P, V>
where
    P: Display,
    Operand<P, V>: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assign { to, val } => write!(f, "{to} = {val}"),
            Self::Call(to, name, args) => write!(
                f,
                "{to} = {name}({})",
                args.iter()
                    .map(|a| format!("{} = {}", a.0, a.1))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Return(val) => write!(f, "return {val}"),
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
            Self::Access { to, record, field } => {
                write!(f, "{to} = {record}.{field}")
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
