//! Intermediate representation (IR)
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

use std::{fmt::Display, sync::Arc};

use crate::{
    ast::Identifier,
    label::{LabelRef, LabelStore},
    runtime::{self, layout::Layout},
    typechecker::{
        self,
        scope::{ScopeGraph, ScopeRef},
    },
};

use super::value::{IrType, IrValue};

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
    NamedTmp(Identifier, usize),
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
    Assign {
        to: Var,
        val: Operand,
        ty: IrType,
    },

    /// Load a constant
    LoadConstant {
        to: Var,
        name: Identifier,
        ty: IrType,
    },

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

    Extend {
        to: Var,
        ty: IrType,
        from: Operand,
    },

    /// Add offset to a pointer
    Offset {
        to: Var,
        from: Operand,
        offset: u32,
    },

    /// Allocate a stack slot
    Alloc {
        to: Var,
        layout: Layout,
    },

    /// Write literal bytes to a variable
    Initialize {
        to: Var,
        bytes: Vec<u8>,
        layout: Layout,
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
        /// Pointer to the clone implementation of the type
        clone: Option<unsafe extern "C" fn(*const (), *mut ())>,
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

pub struct Lir {
    pub functions: Vec<Function>,
}

pub struct IrPrinter<'a> {
    pub scope_graph: &'a ScopeGraph,
    pub label_store: &'a LabelStore,
}

pub trait Printable {
    fn print(&self, printer: &IrPrinter) -> String;
}

impl Printable for Identifier {
    fn print(&self, _printer: &IrPrinter) -> String {
        self.as_str().into()
    }
}

impl Printable for ScopeRef {
    fn print(&self, printer: &IrPrinter) -> String {
        printer.scope_graph.print_scope(*self)
    }
}

impl Printable for Var {
    fn print(&self, printer: &IrPrinter) -> String {
        let f = self.scope.print(printer);
        format!(
            "{}.{}",
            f,
            match &self.kind {
                VarKind::Explicit(name) => name.print(printer).to_string(),
                VarKind::Tmp(idx) => format!("$tmp-{idx}"),
                VarKind::NamedTmp(name, idx) =>
                    format!("${}-{idx}", name.print(printer)),
                VarKind::Return => "$return".to_string(),
                VarKind::Context => "$context".to_string(),
            }
        )
    }
}

impl Printable for Operand {
    fn print(&self, printer: &IrPrinter) -> String {
        match self {
            Operand::Place(x) => x.print(printer),
            Operand::Value(x) => x.to_string(),
        }
    }
}

impl Printable for Instruction {
    fn print(&self, printer: &IrPrinter) -> String {
        use Instruction::*;
        match self {
            Assign { to, val, ty } => {
                format!(
                    "{}: {ty} = {}",
                    to.print(printer),
                    val.print(printer),
                )
            }
            LoadConstant { to, name, ty } => {
                format!(
                    "{}: {ty} = LoadConstant(\"{}\")",
                    to.print(printer),
                    name
                )
            }
            Call {
                to: Some((to, ty)),
                ctx,
                func,
                args,
                return_ptr: _,
            } => format!(
                "{}: {ty} = {}({}, {})",
                to.print(printer),
                func.print(printer),
                ctx.print(printer),
                args.iter()
                    .map(|a| a.print(printer))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Call {
                to: None,
                ctx,
                func,
                args,
                return_ptr: Some(ret),
            } => format!(
                "{}({}, {}, {})",
                func.print(printer),
                ret.print(printer),
                ctx.print(printer),
                args.iter()
                    .map(|a| a.print(printer).to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Call {
                to: None,
                ctx,
                func,
                args,
                return_ptr: None,
            } => format!(
                "{}({}, {})",
                func.print(printer),
                ctx.print(printer),
                args.iter()
                    .map(|a| a.print(printer))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            CallRuntime { func, args } => format!(
                "<rust function {:?}>({})",
                func,
                args.iter()
                    .map(|a| a.print(printer))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            InitString {
                to,
                string,
                init_func: _,
            } => {
                format!("{}: String = \"{string}\"", to.print(printer),)
            }
            Return(None) => "return".to_string(),
            Return(Some(v)) => {
                format!("return {}", v.print(printer))
            }
            IntCmp {
                to,
                cmp,
                left,
                right,
            } => {
                format!(
                    "{} = {cmp}({}, {})",
                    to.print(printer),
                    left.print(printer),
                    right.print(printer),
                )
            }
            FloatCmp {
                to,
                cmp,
                left,
                right,
            } => {
                format!(
                    "{} = {cmp}({}, {})",
                    to.print(printer),
                    left.print(printer),
                    right.print(printer),
                )
            }
            Eq { to, left, right } => {
                format!(
                    "{} = {} == {}",
                    to.print(printer),
                    left.print(printer),
                    right.print(printer),
                )
            }
            Not { to, val } => {
                format!("{} = not({})", to.print(printer), val.print(printer))
            }
            And { to, left, right } => {
                format!(
                    "{} = {} & {}",
                    to.print(printer),
                    left.print(printer),
                    right.print(printer),
                )
            }
            Or { to, left, right } => {
                format!(
                    "{} = {} | {}",
                    to.print(printer),
                    left.print(printer),
                    right.print(printer),
                )
            }
            Add { to, left, right } => {
                format!(
                    "{} = {} + {}",
                    to.print(printer),
                    left.print(printer),
                    right.print(printer),
                )
            }
            Sub { to, left, right } => {
                format!(
                    "{} = {} - {}",
                    to.print(printer),
                    left.print(printer),
                    right.print(printer),
                )
            }
            Mul { to, left, right } => {
                format!(
                    "{} = {} * {}",
                    to.print(printer),
                    left.print(printer),
                    right.print(printer),
                )
            }
            Div {
                to,
                signed,
                left,
                right,
            } => {
                format!(
                    "{}: = {} / {} ({})",
                    to.print(printer),
                    left.print(printer),
                    right.print(printer),
                    if *signed { "signed" } else { "unsigned" },
                )
            }
            FDiv { to, left, right } => {
                format!(
                    "{}: = {} / {} (float)",
                    to.print(printer),
                    left.print(printer),
                    right.print(printer),
                )
            }
            Extend { to, ty, from } => {
                format!(
                    "{}: extend({ty}, {})",
                    to.print(printer),
                    from.print(printer),
                )
            }
            Jump(to) => {
                format!("jump {}", to.print(printer))
            }
            Switch {
                examinee,
                default,
                branches,
            } => {
                format!(
                    "switch {} [{}] else {}",
                    examinee.print(printer),
                    branches
                        .iter()
                        .map(|(i, b)| format!("{i} => {}", b.print(printer)))
                        .collect::<Vec<_>>()
                        .join(", "),
                    default.print(printer),
                )
            }
            Alloc { to, layout } => {
                format!(
                    "{} = mem::alloc(size={}, align={})",
                    to.print(printer),
                    layout.size(),
                    layout.align(),
                )
            }
            Initialize { to, bytes, layout } => {
                format!(
                    "{} = mem::initialize([{}], size={}, align={})",
                    to.print(printer),
                    bytes
                        .iter()
                        .map(|b| b.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                    layout.size(),
                    layout.align(),
                )
            }
            Offset { to, from, offset } => {
                format!(
                    "{} = ptr::offset({}, {offset})",
                    to.print(printer),
                    from.print(printer),
                )
            }
            Read { to, from, ty } => {
                format!(
                    "{}: {ty} = mem::read({})",
                    to.print(printer),
                    from.print(printer),
                )
            }
            Write { to, val } => {
                format!(
                    "mem::write({}, {})",
                    to.print(printer),
                    val.print(printer),
                )
            }
            Copy {
                to,
                from,
                size,
                clone: None,
            } => {
                format!(
                    "mem::copy({}, {}, {size})",
                    to.print(printer),
                    from.print(printer),
                )
            }
            Copy {
                to,
                from,
                size,
                clone: Some(clone),
            } => {
                format!(
                    "mem::copy({}, {}, {size}, with={clone:?})",
                    to.print(printer),
                    from.print(printer),
                )
            }
            MemCmp {
                to,
                size,
                left,
                right,
            } => {
                format!(
                    "{} = mem::cmp({}, {}, {})",
                    to.print(printer),
                    left.print(printer),
                    right.print(printer),
                    size.print(printer),
                )
            }
            Drop {
                var,
                drop: Some(drop),
            } => {
                format!("mem::drop({}, with={drop:?})", var.print(printer))
            }
            Drop { var, drop: None } => {
                format!("mem::drop({}, noop)", var.print(printer))
            }
        }
    }
}

impl Printable for LabelRef {
    fn print(&self, printer: &IrPrinter) -> String {
        let mut label = printer.label_store.get(*self);
        let mut strings = Vec::new();
        loop {
            let dollar = if label.internal { "$" } else { "" };
            let ident = label.identifier.print(printer);
            let counter = if label.counter > 0 {
                format!("({})", label.counter)
            } else {
                "".into()
            };
            strings.push(format!("{dollar}{ident}{counter}"));
            let Some(parent) = label.parent else {
                break;
            };
            label = printer.label_store.get(parent);
        }

        strings.reverse();
        strings.join("::")
    }
}

impl Printable for Block {
    fn print(&self, printer: &IrPrinter) -> String {
        use std::fmt::Write;
        let mut s = String::new();
        writeln!(s, ".{}", &self.label.print(printer)).unwrap();
        for i in &self.instructions {
            writeln!(s, "  {}", i.print(printer)).unwrap();
        }
        s
    }
}

impl Printable for Function {
    fn print(&self, printer: &IrPrinter) -> String {
        use std::fmt::Write;
        let mut blocks = self.blocks.iter();

        let mut s = String::new();

        let Some(b) = blocks.next() else {
            write!(s, "<empty function>").unwrap();
            return s;
        };

        s.push_str(&b.print(printer));

        for b in blocks {
            writeln!(s).unwrap();
            s.push_str(&b.print(printer));
        }

        s
    }
}

impl Printable for Lir {
    fn print(&self, printer: &IrPrinter) -> String {
        let strings: Vec<_> =
            self.functions.iter().map(|f| f.print(printer)).collect();
        strings.join("\n")
    }
}
