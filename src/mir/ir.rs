use crate::{
    ast::{BinOp, Identifier, Literal},
    label::LabelRef,
    runtime,
    typechecker::{
        scope::{ResolvedName, ScopeRef},
        types::{EnumVariant, Signature, Type},
    },
};

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
    NamedTmp(Identifier, usize),
}

#[derive(Clone, Debug)]
pub struct Mir {
    pub functions: Vec<Function>,
}

#[derive(Clone, Debug)]
pub struct Function {
    /// Name of this function
    pub name: Identifier,

    /// Scope that belongs to this function
    pub scope: ScopeRef,

    pub variables: Vec<(Var, Type)>,

    /// Parameters to this function
    pub parameters: Vec<Var>,

    /// The function signature
    pub signature: Signature,

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
    pub root_ty: Type,
    pub projection: Vec<Projection>,
}

impl Place {
    pub fn new(value: Var, ty: Type) -> Self {
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
    Const(Literal, Type),
    Constant(Identifier, Type),
    Context(usize),
    Clone(Place),
    Discriminant(Var),
    Not(Var),
    Move(Var),
    BinOp {
        left: Var,
        binop: BinOp,
        ty: Type,
        right: Var,
    },
    Call {
        func: ResolvedName,
        args: Vec<Var>,
    },
    CallRuntime {
        func_ref: runtime::RuntimeFunctionRef,
        args: Vec<Var>,
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
        ty: Type,
        value: Value,
    },

    SetDiscriminant {
        to: Var,
        ty: Type,
        variant: EnumVariant,
    },

    /// Return from the current function (or filtermap)
    Return {
        var: Var,
    },

    Drop {
        var: Var,
        ty: Type,
    },
}
