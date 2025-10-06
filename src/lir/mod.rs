//! Low-level Intermediate representation (LIR)
//!
//! The IR is the representation between the AST and cranelift. Evaluating
//! it does not need to be particularly fast yet, but the evaluation is safe
//! in the sense that it in the case anything unexpected happens (e.g the
//! wrong type being given) it will panic instead of performing undefined
//! behavior. By evaluating the IR, we can run tests to test this
//! compilation step.
//!
//! The IR has the following characteristics:
//!
//!  - The names of all variables are global.
//!  - Blocks are also identified by readable labels.
//!  - Values are a tagged enum and types are checked at runtime.
//!  - Expressions are simple (as opposed to complex).
//!  - Control flow is represented with basic blocks.
//!
//! The instructions in the IR are inspired by the instructions defined by
//! [cranelift].
//!
//! [cranelift]: https://docs.rs/cranelift-frontend/latest/cranelift_frontend/

pub mod eval;
pub mod lower;
mod print;
pub mod value;

#[cfg(test)]
mod test_eval;

use crate::{
    ast::Identifier,
    label::LabelRef,
    runtime::{self, layout::Layout},
    typechecker::{
        self,
        scope::{ResolvedName, ScopeRef},
    },
};
pub use eval::Memory;
pub use lower::lower_to_lir;
use std::{fmt::Display, sync::Arc};
pub use value::{IrType, IrValue};

/// Human-readable place
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Var {
    pub scope: ScopeRef,
    pub kind: VarKind,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VarKind {
    Explicit(Identifier),
    Tmp(usize),
    Return,
    Context,
}

impl From<Var> for Operand {
    fn from(value: Var) -> Self {
        Operand::Place(value)
    }
}

#[derive(Clone, Debug)]
pub enum Operand {
    Place(Var),
    Value(IrValue),
}

#[derive(Clone, Debug)]
pub enum Instruction {
    /// Jump to a block
    Jump(LabelRef),

    /// Switch on the integer value of the `examinee`
    Switch {
        examinee: Operand,
        branches: Vec<(usize, LabelRef)>,
        default: LabelRef,
    },

    /// Assign the value `val` to `to`.
    Assign { to: Var, val: Operand, ty: IrType },

    /// Get the address of a constant
    ConstantAddress { to: Var, name: ResolvedName },

    /// Create string
    InitString {
        to: Var,
        string: String,
        init_func: unsafe extern "C" fn(*mut Arc<str>, *mut u8, u32),
    },

    /// Call a function.
    Call {
        to: Option<(Var, IrType)>,
        ctx: Operand,
        func: Identifier,
        args: Vec<Operand>,
        return_ptr: Option<Var>,
    },

    /// Call a runtime function (i.e. a Rust function)
    CallRuntime {
        func: runtime::RuntimeFunctionRef,
        args: Vec<Operand>,
    },

    /// Return from the current function (or filtermap)
    Return(Option<Operand>),

    /// Perform an integer comparison and store the result in `to`
    IntCmp {
        to: Var,
        cmp: IntCmp,
        left: Operand,
        right: Operand,
    },

    /// Perform a floating point comparison and store the result in `to`
    FloatCmp {
        to: Var,
        cmp: FloatCmp,
        left: Operand,
        right: Operand,
    },

    /// Integer addition
    Add {
        to: Var,
        left: Operand,
        right: Operand,
    },

    /// Integer subtraction
    Sub {
        to: Var,
        left: Operand,
        right: Operand,
    },

    /// Integer multiplication
    Mul {
        to: Var,
        left: Operand,
        right: Operand,
    },

    /// Integer division
    Div {
        to: Var,
        signed: bool,
        left: Operand,
        right: Operand,
    },

    /// Floating point division
    FDiv {
        to: Var,
        left: Operand,
        right: Operand,
    },

    /// Boolean not
    Not { to: Var, val: Operand },

    /// Numeric negation
    Negate { to: Var, val: Operand },

    /// Add offset to a pointer
    Offset { to: Var, from: Operand, offset: u32 },

    /// Write literal bytes to a variable
    Initialize {
        to: Var,
        bytes: Vec<u8>,
        layout: Layout,
    },

    /// Write to a stack slot
    Write { to: Operand, val: Operand },

    /// Read from a stack slot
    Read { to: Var, from: Operand, ty: IrType },

    /// Copy a stack slot
    Copy {
        to: Operand,
        from: Operand,
        size: u32,
    },

    /// Clone a value with a Rust clone function
    Clone {
        to: Operand,
        from: Operand,
        /// Pointer to the clone implementation of the type
        clone_fn: unsafe extern "C" fn(*const (), *mut ()),
    },

    /// Drop a value
    ///
    /// For primitives and copy types, this is a noop. For more complex types
    /// it matches Rust's Drop.
    Drop {
        var: Operand,
        /// Pointer to the drop implementation of the type
        drop: Option<unsafe extern "C" fn(*mut ())>,
    },

    /// Compare chunks of memory
    MemCmp {
        to: Var,
        size: Operand,
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

#[derive(Clone, Debug)]
pub enum FloatCmp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl Display for FloatCmp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            FloatCmp::Eq => "eq",
            FloatCmp::Ne => "ne",
            FloatCmp::Lt => "lt",
            FloatCmp::Le => "le",
            FloatCmp::Gt => "gt",
            FloatCmp::Ge => "ge",
        };
        write!(f, "{s}")
    }
}

#[derive(Debug)]
pub enum ValueOrSlot {
    Val(IrType),
    StackSlot(Layout),
}

#[derive(Debug)]
pub struct Function {
    /// Identifier of the function
    pub name: Identifier,

    /// Scope of the function
    pub scope: ScopeRef,

    pub signature: typechecker::types::Signature,

    /// Signature of the function
    pub ir_signature: Signature,

    /// Entry block of the function
    pub entry_block: LabelRef,

    /// Variables used by this function
    pub variables: Vec<(Var, ValueOrSlot)>,

    /// Blocks belonging to this function
    pub blocks: Vec<Block>,

    /// Whether the function is public i.e. can be accessed from Rust
    pub public: bool,
}

#[derive(Clone, Debug)]
pub struct Signature {
    pub parameters: Vec<(Identifier, IrType)>,

    /// Whether this function takes a pointer for the return value
    /// passed as an argument
    pub return_ptr: bool,

    pub return_type: Option<IrType>,
}

#[derive(Debug)]
pub struct Block {
    pub label: LabelRef,
    pub instructions: Vec<Instruction>,
}

pub struct Lir {
    pub functions: Vec<Function>,
}
