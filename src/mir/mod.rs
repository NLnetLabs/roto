//! Mid-level intermediate representation (MIR)
//!
//! This is the first intermediate representation. It is based on basic blocks
//! and simple expressions. We do not distinguish between types that are stored
//! in stack slots and types that are cranelift values; all types are treated
//! equally in this representation.

mod dead_code;
mod lower;
mod print;
mod ty;

use crate::{
    ast::{BinOp, Identifier, Literal},
    label::LabelRef,
    runtime,
    typechecker::{
        self,
        scope::{ResolvedName, ScopeRef},
    },
};
pub use lower::lower_to_mir;
pub use ty::{Pool, Signature, Ty, TyRef};

/// Human-readable place
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[must_use]
pub struct Var {
    pub scope: ScopeRef,
    pub kind: VarKind,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VarKind {
    Explicit(Identifier),
    Tmp(usize),
}

#[derive(Clone, Debug)]
pub struct Mir {
    pub items: Vec<Item>,
}

#[derive(Clone, Debug)]
pub enum ItemKind {
    Constant {
        ty: typechecker::types::Type,
        mir_ty: TyRef,
        name: ResolvedName,
    },
    Function {
        parameters: Vec<Var>,
        signature: typechecker::types::Signature,
        mir_signature: Signature,
    },
}

#[derive(Clone, Debug)]
pub struct Item {
    pub ty: ItemKind,

    /// Name of this function
    ///
    /// This is the fully resolved name.
    pub name: Identifier,

    /// Scope that belongs to this function
    pub scope: ScopeRef,

    /// Variables used in this function
    pub variables: Vec<(Var, TyRef)>,

    /// Basic blocks of this function
    ///
    /// The first block is the entrypoint.
    pub blocks: Vec<Block>,

    /// Index of the largest temporary variable in this function
    pub tmp_idx: usize,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub label: LabelRef,
    pub instructions: Vec<Instruction>,
}

#[derive(Clone, Debug)]
pub struct Place {
    pub var: Var,
    pub root_ty: TyRef,
    pub projection: Vec<Projection>,
}

impl Place {
    pub fn new(value: Var, ty: TyRef) -> Self {
        Self {
            var: value,
            root_ty: ty,
            projection: Vec::new(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Projection {
    VariantField(Identifier, usize),
    Field(Identifier),
}

#[derive(Clone, Debug)]
#[must_use]
pub enum Value {
    Const(Literal, TyRef),
    Constant(ResolvedName, TyRef),
    Context(usize),
    Clone(Place),
    Discriminant(Var),
    Not(Var),
    Negate(Var, TyRef),
    Move(Var),
    BinOp {
        left: Var,
        binop: BinOp,
        ty: TyRef,
        right: Var,
    },
    Call {
        func: ResolvedName,
        args: Vec<Var>,
        mir_signature: ty::Signature,
    },
    CallRuntime {
        func_ref: runtime::RuntimeFunctionRef,
        args: Vec<Var>,
        vtables: Vec<TyRef>,
        mir_signature: ty::Signature,
    },
}

#[derive(Clone, Debug)]
pub enum Instruction {
    /// Jump to a block
    Jump(LabelRef),

    /// Switch on the integer value of the `examinee`
    Switch {
        examinee: Var,
        branches: Vec<(usize, LabelRef)>,
        default: Option<LabelRef>,
    },

    Assign {
        to: Place,
        ty: TyRef,
        value: Value,
    },

    SetDiscriminant {
        to: Var,
        ty: TyRef,
        variant: Identifier,
    },

    /// Return from the current function (or filtermap)
    Return {
        var: Var,
    },

    Drop {
        val: Place,
        ty: TyRef,
    },
}
