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

use std::fmt::Display;

use crate::ast::BinOp;

use crate::runtime::wrap::WrappedFunction;

/// Human-readable place
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

    /// Call an external function (i.e. a Rust function)
    CallExternal(P, WrappedFunction, Vec<Operand<P, V>>),

    /// Return from the current "function" (filter-map, term or action)
    Return(Operand<P, V>),

    /// Exit the filter-map with an accept or reject
    Exit(bool, Operand<P, V>),

    /// Perform a binary operation and store the result in `to`
    BinOp {
        to: P,
        op: BinOp,
        left: Operand<P, V>,
        right: Operand<P, V>,
    },

    /// Access a record field
    AccessRecord {
        to: P,
        record: Operand<P, V>,
        field: String,
    },

    /// Create record
    CreateRecord {
        to: P,
        fields: Vec<(String, Operand<P, V>)>,
    },

    /// Create enum variant
    CreateEnum {
        to: P,
        variant: u32,
        data: Operand<P, V>,
    },

    /// Get enum data
    AccessEnum { to: P, from: Operand<P, V> },
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
            Self::CallExternal(to, func, args) => write!(
                f,
                "{to} = <rust function {:?}>({})",
                func.pointer,
                args.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Return(val) => write!(f, "return {val}"),
            Self::Exit(b, val) => {
                let s = if *b {
                    "accept"
                } else {
                    "reject"
                };
                write!(f, "{s} {val}")
            }
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
            Self::AccessRecord { to, record, field } => {
                write!(f, "{to} = {record}.{field}")
            }
            Self::CreateRecord { to, fields } => {
                write!(
                    f,
                    "{to} = {{ {} }}",
                    fields
                        .iter()
                        .map(|(i, v)| format!("{i}: {v}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Self::CreateEnum { to, variant, data } => {
                write!(f, "{to} = Enum({variant}, {data})")
            }
            Self::AccessEnum { to, from } => {
                write!(f, "{to} = get data of {from}")
            }
        }
    }
}

pub struct Function<P, V> {
    pub blocks: Vec<Block<P, V>>,
}

impl<P, V> Display for Function<P, V>
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
