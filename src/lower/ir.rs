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

use string_interner::{backend::StringBackend, StringInterner};

use crate::{
    ast::Identifier,
    runtime,
    typechecker::scope::{ScopeGraph, ScopeRef},
};

use super::{
    label::{LabelRef, LabelStore},
    value::{IrType, IrValue},
};

/// Human-readable place
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Var {
    pub scope: ScopeRef,
    pub kind: VarKind,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum VarKind {
    Explicit(Identifier),
    Tmp(usize),
    NamedTmp(Identifier, usize),
    Return,
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
    Assign {
        to: Var,
        val: Operand,
        ty: IrType,
    },

    /// Call a function.
    Call {
        to: Option<(Var, IrType)>,
        func: Identifier,
        args: Vec<(Identifier, Operand)>,
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

#[derive(Debug)]
pub struct Function {
    /// Identifier of the function
    pub name: Identifier,

    /// Scope of the function
    pub scope: ScopeRef,

    /// Signature of the function
    pub signature: Signature,

    /// Entry block of the function
    pub entry_block: LabelRef,

    /// Blocks belonging to this function
    pub blocks: Vec<Block>,

    /// Whether the function is public i.e. can be accessed from Rust
    pub public: bool,
}

#[derive(Clone, Debug)]
pub struct Signature {
    pub parameters: Vec<(Identifier, IrType)>,

    /// Whether this function takes a pointer for for the return value
    /// passed as an argument
    pub return_ptr: bool,

    pub return_type: Option<IrType>,
}

#[derive(Debug)]
pub struct Block {
    pub label: LabelRef,
    pub instructions: Vec<Instruction>,
}

pub struct IrPrinter<'a> {
    pub scope_graph: &'a ScopeGraph,
    pub identifiers: &'a StringInterner<StringBackend>,
    pub label_store: &'a LabelStore,
}

impl<'a> IrPrinter<'a> {
    pub fn ident(&self, ident: &Identifier) -> &'a str {
        self.identifiers.resolve(ident.0).unwrap()
    }

    pub fn scope(&self, scope: ScopeRef) -> String {
        self.scope_graph.print_scope(scope, self.identifiers)
    }

    pub fn var(&self, var: &Var) -> String {
        let f = self.scope(var.scope);
        format!(
            "{}::{}",
            f,
            match &var.kind {
                VarKind::Explicit(name) => self.ident(name).to_string(),
                VarKind::Tmp(idx) => format!("$tmp-{idx}"),
                VarKind::NamedTmp(name, idx) =>
                    format!("${}-{idx}", self.ident(name)),
                VarKind::Return => "$return".to_string(),
            }
        )
    }

    pub fn operand(&self, operand: &Operand) -> String {
        match operand {
            Operand::Place(x) => self.var(x),
            Operand::Value(x) => x.to_string(),
        }
    }

    pub fn instruction(&self, instruction: &Instruction) -> String {
        use Instruction::*;
        match instruction {
            Assign { to, val, ty } => {
                format!("{}: {ty} = {}", self.var(to), self.operand(val),)
            }
            Call {
                to: Some((to, ty)),
                func,
                args,
            } => format!(
                "{}: {ty} = {}({})",
                self.var(to),
                self.ident(func),
                args.iter()
                    .map(|a| format!(
                        "{} = {}",
                        self.ident(&a.0),
                        self.operand(&a.1),
                    ))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Call {
                to: None,
                func,
                args,
            } => format!(
                "{}({})",
                self.ident(func),
                args.iter()
                    .map(|a| format!(
                        "{} = {}",
                        self.ident(&a.0),
                        self.operand(&a.1),
                    ))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            CallRuntime {
                to: Some((to, ty)),
                func,
                args,
            } => format!(
                "{}: {ty} = <runtime function {:?}>({})",
                self.var(to),
                func.name,
                args.iter()
                    .map(|a| self.operand(a))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            CallRuntime {
                to: None,
                func,
                args,
            } => format!(
                "<rust function {:?}>({})",
                func.name,
                args.iter()
                    .map(|a| self.operand(a))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Return(None) => "return".to_string(),
            Return(Some(v)) => {
                format!("return {}", self.operand(v))
            }
            Cmp {
                to,
                cmp,
                left,
                right,
            } => {
                format!(
                    "{} = {cmp}({}, {})",
                    self.var(to),
                    self.operand(left),
                    self.operand(right),
                )
            }
            Eq { to, left, right } => {
                format!(
                    "{} = {} == {}",
                    self.var(to),
                    self.operand(left),
                    self.operand(right),
                )
            }
            Not { to, val } => {
                format!("{} = not({})", self.var(to), self.operand(val),)
            }
            And { to, left, right } => {
                format!(
                    "{} = {} & {}",
                    self.var(to),
                    self.operand(left),
                    self.operand(right),
                )
            }
            Or { to, left, right } => {
                format!(
                    "{} = {} | {}",
                    self.var(to),
                    self.operand(left),
                    self.operand(right),
                )
            }
            Add { to, left, right } => {
                format!(
                    "{} = {} + {}",
                    self.var(to),
                    self.operand(left),
                    self.operand(right),
                )
            }
            Sub { to, left, right } => {
                format!(
                    "{} = {} - {}",
                    self.var(to),
                    self.operand(left),
                    self.operand(right),
                )
            }
            Mul { to, left, right } => {
                format!(
                    "{} = {} * {}",
                    self.var(to),
                    self.operand(left),
                    self.operand(right),
                )
            }
            Div {
                to,
                ty,
                left,
                right,
            } => {
                format!(
                    "{}: {ty} = {} / {}",
                    self.var(to),
                    self.operand(left),
                    self.operand(right),
                )
            }
            Jump(to) => {
                format!("jump {}", self.label(to))
            }
            Switch {
                examinee,
                default,
                branches,
            } => {
                format!(
                    "switch {} [{}] else {}",
                    self.operand(examinee),
                    branches
                        .iter()
                        .map(|(i, b)| format!("{i} => {}", self.label(b)))
                        .collect::<Vec<_>>()
                        .join(", "),
                    self.label(default)
                )
            }
            Alloc { to, size } => {
                format!("{} = mem::alloc({size})", self.var(to))
            }
            Offset { to, from, offset } => {
                format!(
                    "{} = ptr::offset({}, {offset})",
                    self.var(to),
                    self.operand(from),
                )
            }
            Read { to, from, ty } => {
                format!(
                    "{}: {ty} = mem::read({})",
                    self.var(to),
                    self.operand(from),
                )
            }
            Write { to, val } => {
                format!(
                    "mem::write({}, {})",
                    self.operand(to),
                    self.operand(val)
                )
            }
            Copy { to, from, size } => {
                format!(
                    "mem::copy({}, {}, {size})",
                    self.operand(to),
                    self.operand(from)
                )
            }
            MemCmp {
                to,
                size,
                left,
                right,
            } => {
                format!(
                    "{} = mem::cmp({}, {}, {size})",
                    self.var(to),
                    self.operand(left),
                    self.operand(right),
                )
            }
        }
    }

    pub fn label(&self, label: &LabelRef) -> String {
        let mut label = self.label_store.get(*label);
        let mut strings = Vec::new();
        loop {
            let dollar = if label.internal { "$" } else { "" };
            let ident = self.ident(&label.identifier);
            let counter = if label.counter > 0 {
                format!("({})", label.counter)
            } else {
                "".into()
            };
            strings.push(format!("{dollar}{ident}{counter}"));
            let Some(parent) = label.parent else {
                break;
            };
            label = self.label_store.get(parent);
        }

        strings.reverse();
        strings.join("::")
    }

    pub fn block(&self, block: &Block) -> String {
        use std::fmt::Write;
        let mut s = String::new();
        writeln!(s, ".{}", self.label(&block.label)).unwrap();
        for i in &block.instructions {
            writeln!(s, "  {}", self.instruction(i)).unwrap();
        }
        s
    }

    pub fn function(&self, function: &Function) -> String {
        use std::fmt::Write;
        let mut blocks = function.blocks.iter();

        let mut s = String::new();

        let Some(b) = blocks.next() else {
            write!(s, "<empty function>").unwrap();
            return s;
        };

        s.push_str(&self.block(b));

        for b in blocks {
            writeln!(s).unwrap();
            s.push_str(&self.block(b));
        }

        s
    }

    pub fn program(&self, program: &[Function]) -> String {
        let strings: Vec<_> =
            program.iter().map(|f| self.function(f)).collect();
        strings.join("\n")
    }
}
