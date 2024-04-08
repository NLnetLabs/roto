//! So, we want to lower that AST into simple expressions and stuff.
//!
//! This means: basic blocks and simple instructions. It also means that we
//! have to think about the values that we are using. And places.
//!
//! Let's start simple:
//!  - Memory is linear and contains `Value` (though we might want to be more general about the layout)

use std::{collections::HashMap, fmt::Display};

use crate::{
    ast::{self, BinOp, Identifier, Literal},
    parser::meta::{Meta, MetaId}, typechecker::types::{Primitive, Type},
};

#[derive(Clone, Copy)]
struct Ref(usize);

trait Value {
    fn as_unit(&self) -> () {}
    fn as_bool(&self) -> bool;
    fn as_u8(&self) -> u8;
    fn as_u16(&self) -> u16;
    fn as_u32(&self) -> u32;
    fn as_ref(&self) -> Ref;
}

pub enum SafeValue {
    Unit,
    Bool(bool),
    U8(u8),
    U16(u16),
    U32(u32),
    Ref(Ref),
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
    as_type!(as_u32, u32, U32);
    as_type!(as_ref, Ref, Ref);
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
            SafeValue::Ref(_) => todo!(),
        }
    }
}

/// Humand readable place
#[derive(Clone)]
pub struct Var {
    var: String,
    offset: usize,
}

impl<V> From<Var> for Operand<Var, V> {
    fn from(value: Var) -> Self {
        Operand::Place(value)
    }
}

impl Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.offset == 0 {
            write!(f, "{}", self.var)
        } else {
            write!(f, "{} + {}", self.var, self.offset)
        }
    }
}

struct BlockId(usize);

enum Operand<P, V> {
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

enum Instruction<P, V> {
    // Jump(BlockId),
    // JumpTrue {
    //     condition: Operand<P, V>,
    //     block: BlockId,
    // },
    Assign {
        to: P,
        val: Operand<P, V>,
    },
    // LogicalOr(Operand<P, V>, Operand<P, V>),
    Return,
    Exit,
    BinOp {
        to: P,
        op: BinOp,
        left: Operand<P, V>,
        right: Operand<P, V>,
    },
}

impl<P, V> Display for Instruction<P, V>
where
    P: Display,
    Operand<P, V>: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Assign { to, val } => write!(f, "{to} = {val}"),
            Instruction::Exit => write!(f, "exit"),
            Instruction::Return => write!(f, "return"),
            Instruction::BinOp { to, op, left, right } => {
                write!(f, "{to} = {left} {op} {right}")
            }
        }
    }
}

pub struct Program<P, V> {
    blocks: Vec<Block<P, V>>,
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

struct Block<P, V> {
    label: String,
    instructions: Vec<Instruction<P, V>>,
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

struct Lowerer {
    tmp_idx: usize,
    blocks: Vec<Block<Var, SafeValue>>,
    expr_types: HashMap<MetaId, Type>,
}

#[derive(Clone)]
struct Ctx {
    return_var: Var,
    var_prefix: String,
    block_idx: usize,
}

pub fn lower(tree: &ast::SyntaxTree, expr_types: HashMap<MetaId, Type>) -> Program<Var, SafeValue> {
    let lowerer = Lowerer {
        tmp_idx: 0,
        expr_types,
        blocks: Vec::new(),
    };

    lowerer.tree(tree)
}

impl Lowerer {
    fn new_tmp(&mut self) -> Var {
        let var = Var {
            var: format!("$tmp-{}", self.tmp_idx),
            offset: 0,
        };
        self.tmp_idx += 1;
        var
    }

    fn tree(mut self, tree: &ast::SyntaxTree) -> Program<Var, SafeValue> {
        let ast::SyntaxTree { expressions } = tree;

        for expr in expressions {
            match expr {
                ast::Declaration::FilterMap(fm) => {
                    self.filter_map(fm);
                }
                _ => {} // ignore declarations
            }
        }

        Program {
            blocks: self.blocks,
        }
    }

    fn filter_map(&mut self, fm: &ast::FilterMap) {
        let ast::FilterMap { ident, body, .. } = fm;
        let ast::FilterMapBody {
            define,
            expressions,
            apply,
        } = body;

        let return_var = Var {
            var: format!("{}-return", ident.0),
            offset: 0,
        };
        let ctx = Ctx {
            return_var: return_var.clone(),
            block_idx: 0,
            var_prefix: ident.0.clone(),
        };

        for decl in expressions {
            match decl {
                ast::FilterMapExpr::Term(ast::TermDeclaration {
                    ident,
                    params,
                    body,
                }) => self.function(ctx.clone(), &ident.0, params, body),
                ast::FilterMapExpr::Action(ast::ActionDeclaration {
                    ident,
                    params,
                    body,
                }) => self.function(ctx.clone(), &ident.0, params, body),
            }
        }

        let (mut instructions, last) = self.block(ctx, apply);

        if let Some(last) = last {
            instructions.push(Instruction::Assign {
                to: return_var,
                val: last,
            });
        }
        instructions.push(Instruction::Exit);

        self.blocks.push(Block {
            label: ident.0.clone(),
            instructions,
        });
    }

    fn function(
        &mut self,
        ctx: Ctx,
        ident: &str,
        _params: &Meta<Vec<(Meta<Identifier>, Meta<Identifier>)>>,
        body: &ast::Block,
    ) {
        let return_var = Var {
            var: format!("{}-return", ident),
            offset: 0,
        };
        let ctx = Ctx {
            return_var: return_var.clone(),
            block_idx: 0,
            var_prefix: format!("{}::{}", ctx.var_prefix, ident),
        };

        let (mut instructions, last) = self.block(ctx, body);

        if let Some(last) = last {
            instructions.push(Instruction::Assign {
                to: return_var,
                val: last,
            });
        }
        instructions.push(Instruction::Return);

        self.blocks.push(Block {
            label: ident.into(),
            instructions,
        });
    }

    fn block(
        &mut self,
        ctx: Ctx,
        block: &ast::Block,
    ) -> (
        Vec<Instruction<Var, SafeValue>>,
        Option<Operand<Var, SafeValue>>,
    ) {
        let mut instructions = Vec::new();

        // Result is ignored
        for expr in &block.exprs {
            self.expr(ctx.clone(), &mut instructions, &expr);
        }

        let last = if let Some(last) = &block.last {
            Some(self.expr(ctx, &mut instructions, &last))
        } else {
            None
        };

        (instructions, last)
    }

    fn expr(
        &mut self,
        ctx: Ctx,
        instructions: &mut Vec<Instruction<Var, SafeValue>>,
        expr: &Meta<ast::Expr>,
    ) -> Operand<Var, SafeValue> {
        match &expr.node {
            ast::Expr::Accept => self.return_with(
                ctx,
                instructions,
                SafeValue::Bool(true).into(),
            ),
            ast::Expr::Reject => self.return_with(
                ctx,
                instructions,
                SafeValue::Bool(false).into(),
            ),
            ast::Expr::Return(e) => {
                let val = self.expr(ctx.clone(), instructions, e);
                self.return_with(ctx, instructions, val)
            }
            ast::Expr::Literal(l) => self.literal(l),
            ast::Expr::Match(_) => todo!(),
            ast::Expr::PrefixMatch(_) => todo!(),
            ast::Expr::FunctionCall(_, _) => todo!(),
            ast::Expr::MethodCall(_, _, _) => todo!(),
            ast::Expr::Access(_, _) => todo!(),
            ast::Expr::Var(_) => todo!(),
            ast::Expr::Record(_) => todo!(),
            ast::Expr::TypedRecord(_, _) => todo!(),
            ast::Expr::List(_) => todo!(),
            ast::Expr::Not(e) => {
                let val = self.expr(ctx.clone(), instructions, e);
                let place = self.new_tmp();
                instructions.push(Instruction::Assign {
                    to: place.clone(),
                    val,
                });
                place.into()
            }
            ast::Expr::BinOp(left, op, right) => {
                let left = self.expr(ctx.clone(), instructions, left);
                let right = self.expr(ctx.clone(), instructions, right);

                let place = self.new_tmp();
                instructions.push(Instruction::BinOp {
                    to: place.clone(),
                    op: op.clone(),
                    left,
                    right,
                });
                place.into()
            }
            ast::Expr::IfElse(_, _, _) => todo!(),
        }
    }

    fn literal(&mut self, lit: &Meta<Literal>) -> Operand<Var, SafeValue> {
        match &lit.node {
            Literal::String(_) => todo!(),
            Literal::Prefix(_) => todo!(),
            Literal::PrefixLength(_) => todo!(),
            Literal::Asn(_) => todo!(),
            Literal::IpAddress(_) => todo!(),
            Literal::ExtendedCommunity(_) => todo!(),
            Literal::StandardCommunity(_) => todo!(),
            Literal::LargeCommunity(_) => todo!(),
            Literal::Integer(x) => {
                let ty = &self.expr_types[&lit.id];
                match ty {
                    Type::Primitive(Primitive::U8) => SafeValue::U8(*x as u8),
                    Type::Primitive(Primitive::U16) => SafeValue::U16(*x as u16),
                    Type::Primitive(Primitive::U32) => SafeValue::U32(*x as u32),
                    Type::IntVar(_) => SafeValue::U32(*x as u32),
                    _ => unreachable!("should be a type error: {ty}")
                }.into()
            },
            Literal::Bool(x) => SafeValue::Bool(*x).into(),
        }
    }

    fn return_with(
        &self,
        ctx: Ctx,
        instructions: &mut Vec<Instruction<Var, SafeValue>>,
        val: Operand<Var, SafeValue>,
    ) -> Operand<Var, SafeValue> {
        instructions.push(Instruction::Assign {
            to: ctx.return_var,
            val,
        });
        instructions.push(Instruction::Return);
        SafeValue::Unit.into()
    }
}
