use crate::{
    ast::{BinOp, Identifier, Literal},
    label::LabelRef,
    typechecker::{
        scope::{ResolvedName, ScopeRef},
        types::{Signature, Type},
    },
};

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
}

#[derive(Clone, Debug)]
pub enum Operand {
    Place(Var),
    Value(Literal),
}

impl From<Var> for Operand {
    fn from(value: Var) -> Self {
        Operand::Place(value)
    }
}

#[derive(Clone, Debug)]
pub struct Hir {
    pub functions: Vec<Function>,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Identifier,
    pub parameters: Vec<Var>,
    pub signature: Signature,
    pub blocks: Vec<Block>,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub label: LabelRef,
    pub instructions: Vec<Instruction>,
}

#[derive(Clone, Debug)]
pub enum Field {
    Numbered(usize),
    Named(Identifier),
    Discriminant,
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

    MakeRecord {
        to: Var,
        ty: Type,
        fields: Vec<(Identifier, Operand)>,
    },

    MakeEnum {
        to: Var,
        ty: Type,
        variant: Identifier,
        fields: Vec<Operand>,
    },

    ReadField {
        to: Var,
        from: Operand,
        field: Field,
    },

    /// Assign the value `val` to `to`.
    Assign {
        to: Var,
        root_ty: Type,
        fields: Vec<(Identifier, Type)>,
        val: Operand,
    },

    /// Call a function.
    Call {
        to: Var,
        func: ResolvedName,
        args: Vec<Operand>,
    },

    /// Return from the current function (or filtermap)
    Return(Operand),

    Not {
        to: Var,
        op: Operand,
    },

    BinOp {
        to: Var,
        left: Operand,
        op: BinOp,
        right: Operand,
    },

    Drop {
        value: Operand,
    },
}
