//! Precedence and associativity of operators in Roto
//!
//! This is based on work by Jana Dönszelmann, with explicit permission to use,
//! modify and distribute it under the BSD-3-Clause license.

use crate::ast::BinOp;
use std::cmp::Ordering;

/// Precedence of binary operators
///
/// The order of the variants of this enum is significant, because it defines
/// derived implementation of `PartialOrd`/`Ord`.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Precedence {
    /// The precedence of logical disjunction and conjunction.
    Logical,

    /// The precedence of comparison operators.
    Comparison,

    /// The precedence of addition and subtraction.
    AddSub,

    /// The precedence of multiplication and division.
    MulDiv,
}

/// Associativity of binary operator.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Associativity {
    /// Left associativity.
    ///
    /// `<x> o <y> o <z>` is parsed as `(<x> o <y>) o <z>`.
    Left,

    /// Right associativity.
    ///
    /// `<x> o <y> o <z>` is parsed as `<x> o (<y> o <z>)`.
    Right,

    /// Incompatible operators
    Not,
}

impl BinOp {
    fn precedence(&self) -> Precedence {
        match self {
            Self::Or | Self::And => Precedence::Logical,
            Self::Eq
            | Self::Ne
            | Self::Lt
            | Self::Le
            | Self::Gt
            | Self::Ge => Precedence::Comparison,
            Self::Mul | Self::Div | Self::Mod => Precedence::MulDiv,
            Self::Add | Self::Sub => Precedence::AddSub,
        }
    }

    fn associativity(&self) -> Associativity {
        match self.precedence() {
            Precedence::AddSub | Precedence::MulDiv | Precedence::Logical => {
                Associativity::Left
            }
            Precedence::Comparison => Associativity::Not,
        }
    }

    /// Get the relative associativity between a pair of operators.
    ///
    /// If we have two operators in sequence, we need to know which one of the
    /// two binds stronger. That's what this function returns.
    ///
    /// For example:
    ///
    ///  - `a + b + c` is left associative: `(a + b) + c`
    ///  - `a * b + c` is left associative: `(a * b) + c`
    ///  - `a + b * c` is right associative: `a + (b * c)`
    ///
    pub fn relative_associativity(&self, other: &Self) -> Associativity {
        // Special case: we don't allow mixing `||` and `&&`.
        if let (BinOp::Or, BinOp::And) | (BinOp::And, BinOp::Or) =
            (self, other)
        {
            return Associativity::Not;
        }

        match self.precedence().cmp(&other.precedence()) {
            Ordering::Less => Associativity::Right,
            Ordering::Greater => Associativity::Left,
            Ordering::Equal => self.associativity(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::BinOp, parser::precedence::Associativity};

    fn f(a: BinOp, b: BinOp) -> Associativity {
        a.relative_associativity(&b)
    }

    #[test]
    fn assoc() {
        use Associativity::*;
        use BinOp::*;

        assert_eq!(f(Add, Add), Left);
        assert_eq!(f(Mul, Add), Left);
        assert_eq!(f(Add, Mul), Right);
        assert_eq!(f(And, Or), Not);
        assert_eq!(f(And, And), Left);
        assert_eq!(f(Or, Or), Left);
        assert_eq!(f(Eq, Eq), Not);
    }
}
