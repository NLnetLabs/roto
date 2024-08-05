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

use crate::runtime;

use super::value::{IrType, IrValue};

/// Human-readable place
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Var {
    pub function: String,
    pub kind: VarKind,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum VarKind {
    Explicit(String),
    Tmp(usize),
    NamedTmp(&'static str, usize),
    Return,
}

impl From<Var> for Operand {
    fn from(value: Var) -> Self {
        Operand::Place(value)
    }
}

impl Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", &self.function, match &self.kind {
            VarKind::Explicit(name) => name.clone(),
            VarKind::Tmp(idx) => format!("$tmp-{idx}"),
            VarKind::NamedTmp(name, idx) => format!("${name}-{idx}"),
            VarKind::Return => "$return".to_string(),
        })
    }
}

#[derive(Clone, Debug)]
pub enum Operand {
    Place(Var),
    Value(IrValue),
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Place(x) => write!(f, "{x}"),
            Operand::Value(x) => write!(f, "{x}"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Instruction {
    /// Jump to a block
    Jump(String),

    /// Switch on the integer value of the `examinee`
    Switch {
        examinee: Operand,
        branches: Vec<(usize, String)>,
        default: String,
    },

    /// Assign the value `val` to `to`.
    Assign {
        to: Var,
        val: Operand,
        ty: IrType,
    },

    /// Call a function.
    Call {
        to: Option<(Var, IrType)>,
        func: String,
        args: Vec<(String, Operand)>,
    },

    /// Call a runtime function (i.e. a Rust function)
    CallRuntime {
        to: Option<(Var, IrType)>,
        func: runtime::RuntimeFunction,
        args: Vec<Operand>,
    },

    /// Return from the current function (or filter-map)
    Return(Option<Operand>),

    /// Perform a comparison and store the result in `to`
    Cmp {
        to: Var,
        cmp: IntCmp,
        left: Operand,
        right: Operand,
    },

    Add {
        to: Var,
        left: Operand,
        right: Operand,
    },
    
    Sub {
        to: Var,
        left: Operand,
        right: Operand,
    },
    
    Mul {
        to: Var,
        left: Operand,
        right: Operand,
    },

    Div {
        to: Var,
        ty: IrType,
        left: Operand,
        right: Operand,
    },

    /// More of a placeholder instruction until we got proper equality on
    /// more complicated types.
    Eq {
        to: Var,
        left: Operand,
        right: Operand,
    },

    Not {
        to: Var,
        val: Operand,
    },

    And {
        to: Var,
        left: Operand,
        right: Operand,
    },

    Or {
        to: Var,
        left: Operand,
        right: Operand,
    },

    // Add offset to a pointer
    Offset {
        to: Var,
        from: Operand,
        offset: u32,
    },

    // Allocate a stack slot
    Alloc {
        to: Var,
        size: u32,
    },

    /// Write to a stack slot
    Write {
        to: Operand,
        val: Operand,
    },

    /// Read from a stack slot
    Read {
        to: Var,
        from: Operand,
        ty: IrType,
    },

    /// Copy a stack slot
    Copy {
        to: Operand,
        from: Operand,
        size: u32,
    },

    /// Compare chunks of memory
    MemCmp {
        to: Var,
        size: u32,
        left: Operand,
        right: Operand,
    },
}

#[derive(Clone, Debug)]
pub enum IntCmp {
    Eq,
    Ne,
    ULt,
    ULe,
    UGt,
    UGe,
    SLt,
    SLe,
    SGt,
    SGe,
}

impl Display for IntCmp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            IntCmp::Eq => "eq",
            IntCmp::Ne => "ne",
            IntCmp::ULt => "ult",
            IntCmp::ULe => "ule",
            IntCmp::UGt => "ugt",
            IntCmp::UGe => "uge",
            IntCmp::SLt => "slt",
            IntCmp::SLe => "sle",
            IntCmp::SGt => "sgt",
            IntCmp::SGe => "sge",
        };
        write!(f, "{s}")
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assign { to, val, ty } => write!(f, "{to}: {ty} = {val}"),
            Self::Call {
                to: Some((to, ty)),
                func,
                args,
            } => write!(
                f,
                "{to}: {ty} = {func}({})",
                args.iter()
                    .map(|a| format!("{} = {}", a.0, a.1))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Call {
                to: None,
                func,
                args,
            } => write!(
                f,
                "{func}({})",
                args.iter()
                    .map(|a| format!("{} = {}", a.0, a.1))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::CallRuntime { to: Some((to, ty)), func, args } => write!(
                f,
                "{to}: {ty} = <runtime function {:?}>({})",
                func.name,
                args.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::CallRuntime { to: None, func, args } => write!(
                f,
                "<rust function {:?}>({})",
                func.name,
                args.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Return(None) => write!(f, "return"),
            Self::Return(Some(v)) => write!(f, "return {v}"),
            Self::Cmp {
                to,
                cmp,
                left,
                right,
            } => {
                write!(f, "{to} = {cmp}({left}, {right})")
            }
            Self::Eq { to, left, right } => {
                write!(f, "{to} = {left} == {right}")
            }
            Self::Not { to, val } => {
                write!(f, "{to} = not({val})")
            }
            Self::And { to, left, right } => {
                write!(f, "{to} = {left} & {right}")
            }
            Self::Or { to, left, right } => {
                write!(f, "{to} = {left} | {right}")
            }
            Self::Add { to, left, right } => {
                write!(f, "{to} = {left} + {right}")
            }
            Self::Sub { to, left, right } => {
                write!(f, "{to} = {left} - {right}")
            }
            Self::Mul { to, left, right } => {
                write!(f, "{to} = {left} * {right}")
            }
            Self::Div { to, ty, left, right } => {
                write!(f, "{to}: {ty} = {left} / {right}")
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
            Self::Alloc { to, size } => {
                write!(f, "{to} = mem::alloc({size})")
            }
            Self::Offset { to, from, offset } => {
                write!(f, "{to} = ptr::offset({from}, {offset})")
            }
            Self::Read { to, from, ty } => {
                write!(f, "{to}: {ty} = mem::read({from})")
            }
            Self::Write { to, val } => {
                write!(f, "mem::write({to}, {val})")
            }
            Self::Copy { to, from, size } => {
                write!(f, "mem::copy({to}, {from}, {size})")
            }
            Self::MemCmp {
                to,
                size,
                left,
                right,
            } => {
                write!(f, "{to} = mem::cmp({left}, {right}, {size})")
            }
        }
    }
}

#[derive(Debug)]
pub struct Function {
    /// Identifier of the function
    pub name: String,

    /// Signature of the function
    pub signature: Signature,

    /// Blocks belonging to this function
    pub blocks: Vec<Block>,

    /// Whether the function is public i.e. can be accessed from Rust
    pub public: bool,
}

#[derive(Clone, Debug)]
pub struct Signature {
    pub parameters: Vec<(String, IrType)>,

    /// Whether this function takes a pointer for for the return value
    /// passed as an argument
    pub return_ptr: bool,

    pub return_type: Option<IrType>,
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut blocks = self.blocks.iter();

        let Some(b) = blocks.next() else {
            write!(f, "<empty function>")?;
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

#[derive(Debug)]
pub struct Block {
    pub label: String,
    pub instructions: Vec<Instruction>,
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, ".{}", self.label)?;
        for i in &self.instructions {
            writeln!(f, "  {i}")?
        }
        Ok(())
    }
}
