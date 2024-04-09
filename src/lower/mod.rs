//! So, we want to lower that AST into simple expressions and stuff.
//!
//! This means: basic blocks and simple instructions. It also means that we
//! have to think about the values that we are using. And places.
//!
//! Let's start simple:
//!  - Memory is linear and contains `Value` (though we might want to be more general about the layout)
//!  - Operands to instructions are places (variables) or values.
//!  - Branching and jumping is done via basic blocks.

pub mod eval;
pub mod ir;

use ir::{Block, Instruction, Operand, Program, SafeValue, Var};
use std::collections::HashMap;

use crate::{
    ast::{self, Identifier, Literal},
    parser::meta::{Meta, MetaId},
    typechecker::types::{Primitive, Type},
};

struct Lowerer {
    tmp_idx: usize,
    blocks: Vec<Block<Var, SafeValue>>,
    expr_types: HashMap<MetaId, Type>,
    fully_qualified_names: HashMap<MetaId, String>,
    block_names: HashMap<String, usize>,
}

#[derive(Clone)]
struct Ctx {
    return_var: Var,
}

pub fn lower(
    tree: &ast::SyntaxTree,
    expr_types: HashMap<MetaId, Type>,
    fully_qualified_names: HashMap<MetaId, String>,
) -> Program<Var, SafeValue> {
    let lowerer = Lowerer {
        tmp_idx: 0,
        expr_types,
        fully_qualified_names,
        blocks: Vec::new(),
        block_names: HashMap::new(),
    };

    lowerer.tree(tree)
}

impl Lowerer {
    fn new_unique_block_name(&mut self, s: &str) -> String {
        let v = self.block_names.entry(s.to_string()).or_default();
        let name = if *v == 0 {
            s.to_string()
        } else {
            format!("{s}.{v}")
        };
        *v += 1;
        name
    }

    fn new_block(&mut self, s: &str) {
        self.blocks.push(Block {
            label: s.into(),
            instructions: Vec::new(),
        })
    }

    fn add(&mut self, instruction: Instruction<Var, SafeValue>) {
        self.blocks
            .last_mut()
            .unwrap()
            .instructions
            .push(instruction);
    }

    fn new_tmp(&mut self) -> Var {
        let var = Var {
            var: format!("$tmp-{}", self.tmp_idx),
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

        let ident = self.fully_qualified_names[&ident.id].clone();

        let return_var = Var {
            var: format!("{}-return", ident),
        };
        let ctx = Ctx {
            return_var: return_var.clone(),
        };

        for decl in expressions {
            match decl {
                ast::FilterMapExpr::Term(ast::TermDeclaration {
                    ident,
                    params,
                    body,
                }) => self.function(ident, params, body),
                ast::FilterMapExpr::Action(ast::ActionDeclaration {
                    ident,
                    params,
                    body,
                }) => self.function(ident, params, body),
            }
        }

        self.new_block(&ident);

        match &define.body.rx_tx_type {
            ast::RxTxType::RxOnly(x, _) => {
                let var = self.fully_qualified_names[&x.id].clone();
                self.add(Instruction::Assign {
                    to: Var { var },
                    val: Var {
                        var: format!("{ident}-rx"),
                    }
                    .into(),
                })
            }
            ast::RxTxType::Split { .. } => todo!(),
        }

        for (ident, expr) in &define.body.assignments {
            let val = self.expr(&ctx, expr);
            let name = self.fully_qualified_names[&ident.id].clone();
            self.add(Instruction::Assign {
                to: Var { var: name },
                val,
            })
        }

        let last = self.block(&ctx, apply);

        if let Some(last) = last {
            self.add(Instruction::Assign {
                to: return_var,
                val: last,
            });
        }
        self.add(Instruction::Exit);
    }

    fn function(
        &mut self,
        ident: &Meta<Identifier>,
        _params: &Meta<Vec<(Meta<Identifier>, Meta<Identifier>)>>,
        body: &ast::Block,
    ) {
        let ident = self.fully_qualified_names[&ident.id].clone();
        let return_var = Var {
            var: format!("{}-return", ident),
        };
        let ctx = Ctx {
            return_var: return_var.clone(),
        };

        self.new_block(&ident);

        let last = self.block(&ctx, body);

        if let Some(last) = last {
            self.add(Instruction::Assign {
                to: return_var,
                val: last,
            });
        }
        self.add(Instruction::Return);
    }

    fn block(
        &mut self,
        ctx: &Ctx,
        block: &ast::Block,
    ) -> Option<Operand<Var, SafeValue>> {
        // Result is ignored
        for expr in &block.exprs {
            self.expr(ctx, &expr);
        }

        let last = if let Some(last) = &block.last {
            Some(self.expr(ctx, &last))
        } else {
            None
        };

        last
    }

    fn expr(
        &mut self,
        ctx: &Ctx,
        expr: &Meta<ast::Expr>,
    ) -> Operand<Var, SafeValue> {
        match &expr.node {
            ast::Expr::Accept => {
                self.return_with(ctx, SafeValue::Bool(true).into())
            }
            ast::Expr::Reject => {
                self.return_with(ctx, SafeValue::Bool(false).into())
            }
            ast::Expr::Return(e) => {
                let val = self.expr(ctx, e);
                self.return_with(ctx, val)
            }
            ast::Expr::Literal(l) => self.literal(l),
            ast::Expr::Match(_) => todo!(),
            ast::Expr::PrefixMatch(_) => todo!(),
            ast::Expr::FunctionCall(_, _) => todo!(),
            ast::Expr::MethodCall(_, _, _) => todo!(),
            ast::Expr::Access(_, _) => todo!(),
            ast::Expr::Var(x) => {
                let var = self.fully_qualified_names[&x.id].clone();
                Var { var }.into()
            }
            ast::Expr::Record(_) => todo!(),
            ast::Expr::TypedRecord(_, _) => todo!(),
            ast::Expr::List(_) => todo!(),
            ast::Expr::Not(e) => {
                let val = self.expr(ctx, e);
                let place = self.new_tmp();
                self.add(Instruction::Assign {
                    to: place.clone(),
                    val,
                });
                place.into()
            }
            ast::Expr::BinOp(left, op, right) => {
                let left = self.expr(ctx, left);
                let right = self.expr(ctx, right);

                let place = self.new_tmp();
                self.add(Instruction::BinOp {
                    to: place.clone(),
                    op: op.clone(),
                    left,
                    right,
                });
                place.into()
            }
            ast::Expr::IfElse(condition, if_true, if_false) => {
                let place = self.expr(ctx, condition);
                let res = self.new_tmp();

                let lbl_cont = self.new_unique_block_name("if-continue");
                let lbl_then = self.new_unique_block_name("if-then");
                let lbl_else = self.new_unique_block_name("if-else");

                let branches = if if_false.is_some() {
                    vec![(1, lbl_then.clone())]
                } else {
                    vec![(1, lbl_then.clone())]
                };

                self.add(Instruction::Switch {
                    examinee: place,
                    branches,
                    default: if if_false.is_some() {
                        lbl_else.clone()
                    } else {
                        lbl_cont.clone()
                    },
                });

                self.new_block(&lbl_then);
                if let Some(op) = self.block(ctx, if_true) {
                    self.add(Instruction::Assign {
                        to: res.clone(),
                        val: op,
                    });
                }
                self.add(Instruction::Jump(lbl_cont.clone()));

                if let Some(if_false) = if_false {
                    self.new_block(&lbl_else);
                    if let Some(op) = self.block(ctx, if_false) {
                        self.add(Instruction::Assign {
                            to: res.clone(),
                            val: op,
                        });
                    }
                    self.add(Instruction::Jump(lbl_cont.clone()));
                }

                self.new_block(&lbl_cont);

                res.into()
            }
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
                    Type::Primitive(Primitive::U16) => {
                        SafeValue::U16(*x as u16)
                    }
                    Type::Primitive(Primitive::U32) => {
                        SafeValue::U32(*x as u32)
                    }
                    Type::IntVar(_) => SafeValue::U32(*x as u32),
                    _ => unreachable!("should be a type error: {ty}"),
                }
                .into()
            }
            Literal::Bool(x) => SafeValue::Bool(*x).into(),
        }
    }

    fn return_with(
        &mut self,
        ctx: &Ctx,
        val: Operand<Var, SafeValue>,
    ) -> Operand<Var, SafeValue> {
        self.add(Instruction::Assign {
            to: ctx.return_var.clone(),
            val,
        });
        self.add(Instruction::Return);
        SafeValue::Unit.into()
    }
}
