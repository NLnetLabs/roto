//! Compiler stage to transform the AST into HIR
//!
//! For more information on the HIR, see the [ir] module.

pub mod eval;
pub mod ir;
mod match_expr;
pub mod value;

#[cfg(test)]
mod test_eval;

use ir::{Block, Instruction, Operand, Program, Var};
use std::{collections::HashMap, net::IpAddr};

use crate::{
    ast::{self, Identifier, Literal},
    parser::meta::Meta,
    runtime::Runtime,
    typechecker::{
        types::{Primitive, Type},
        TypeInfo,
    },
};

use self::value::SafeValue;

struct Lowerer<'r> {
    runtime: &'r Runtime,
    tmp_idx: usize,
    blocks: Vec<Block<Var, SafeValue>>,
    type_info: TypeInfo,
    block_names: HashMap<String, usize>,
}

pub fn lower(
    runtime: &Runtime,
    tree: &ast::SyntaxTree,
    type_info: TypeInfo,
) -> Program<Var, SafeValue> {
    let lowerer = Lowerer {
        runtime,
        tmp_idx: 0,
        type_info,
        blocks: Vec::new(),
        block_names: HashMap::new(),
    };

    lowerer.tree(tree)
}

impl Lowerer<'_> {
    /// Create a block with a unique name starting with a given prefix
    ///
    /// For example, the prefix `if-then` will give `if-then` for the first
    /// occurrence, `if-then.1` for the second, `if-then.2` for the third, etc.
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

    /// Add a new block to the blocks in the program.
    fn new_block(&mut self, s: &str) {
        self.blocks.push(Block {
            label: s.into(),
            instructions: Vec::new(),
        })
    }

    /// Add an instruction to the last block
    fn add(&mut self, instruction: Instruction<Var, SafeValue>) {
        let instructions = &mut self.blocks.last_mut().unwrap().instructions;

        // If the last instruction is a return or an exit, we know that
        // the instruction we push will never be executed, so we just
        // omit it. This should be done by an optimizer as well but this
        // makes our initial generated code just a bit nicer.
        if !matches!(instructions.last(), Some(Instruction::Return(_))) {
            instructions.push(instruction);
        }
    }

    /// Create a new unique temporary variable
    fn new_tmp(&mut self) -> Var {
        let var = Var {
            var: format!("$tmp-{}", self.tmp_idx),
        };
        self.tmp_idx += 1;
        var
    }

    /// Lower a syntax tree
    fn tree(mut self, tree: &ast::SyntaxTree) -> Program<Var, SafeValue> {
        let ast::SyntaxTree { expressions } = tree;

        for expr in expressions {
            if let ast::Declaration::FilterMap(fm) = expr {
                self.filter_map(fm);
            }
        }

        Program {
            blocks: self.blocks,
        }
    }

    /// Lower a filter-map
    fn filter_map(&mut self, fm: &ast::FilterMap) {
        let ast::FilterMap {
            ident,
            body,
            params,
            ..
        } = fm;
        let ast::FilterMapBody {
            define,
            expressions,
            apply,
        } = body;

        let ident = self.type_info.full_name(ident);

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

        for (i, (p, _)) in params.iter().enumerate() {
            let p = self.type_info.full_name(p);
            self.add(Instruction::Assign {
                to: Var { var: p.clone() },
                val: Var {
                    var: format!("$arg_{i}"),
                }
                .into(),
            })
        }

        for (ident, expr) in define {
            let val = self.expr(expr);
            let name = self.type_info.full_name(ident);
            self.add(Instruction::Assign {
                to: Var { var: name },
                val,
            })
        }

        let last = self.block(apply);

        self.add(Instruction::Return(last.unwrap_or(SafeValue::Unit.into())));
    }

    /// Lower a function
    ///
    /// We compile functions differently than turing complete languages,
    /// because we don't have a full stack. Instead, the arguments just
    /// have fixed locations. The interpreter does have to keep track
    /// where the function is called and hence where it has to return to
    ///
    /// Ultimately, it should be possible to inline all functions, so
    /// that we don't even need a return instruction. However, this could
    /// also be done by an optimizing step.
    fn function(
        &mut self,
        ident: &Meta<Identifier>,
        _params: &Meta<Vec<(Meta<Identifier>, Meta<Identifier>)>>,
        body: &ast::Block,
    ) {
        let ident = self.type_info.full_name(ident);
        self.new_block(&ident);

        let last = self.block(body);

        self.add(Instruction::Return(last.unwrap_or(SafeValue::Unit.into())))
    }

    /// Lower a block
    ///
    /// Returns either the value of the expression or the place where the
    /// value can be retrieved.
    fn block(
        &mut self,
        block: &ast::Block,
    ) -> Option<Operand<Var, SafeValue>> {
        // Result is ignored
        for expr in &block.exprs {
            self.expr(expr);
        }

        block.last.as_ref().map(|last| self.expr(last))
    }

    /// Lower an expression
    ///
    /// Returns either the value of the expression or the place where the
    /// value can be retrieved. The nested expressions will be lowered
    /// recursively.
    fn expr(&mut self, expr: &Meta<ast::Expr>) -> Operand<Var, SafeValue> {
        let id = expr.id;
        match &expr.node {
            ast::Expr::Return(kind, e) => {
                let op = if let Some(e) = e {
                    self.expr(e)
                } else {
                    SafeValue::Unit.into()
                };

                self.add(match kind {
                    ast::ReturnKind::Return => Instruction::Return(op),
                    ast::ReturnKind::Accept => Instruction::Exit(true, op),
                    ast::ReturnKind::Reject => Instruction::Exit(false, op),
                });

                SafeValue::Unit.into()
            }
            ast::Expr::Literal(l) => self.literal(l),
            ast::Expr::Match(m) => self.match_expr(m),
            ast::Expr::PrefixMatch(_) => todo!(),
            ast::Expr::FunctionCall(ident, args) => {
                let ty = self.type_info.type_of(ident);
                let ident = self.type_info.full_name(ident);

                let (Type::Term(params) | Type::Action(params)) = ty else {
                    panic!("This shouldn't have passed typechecking");
                };

                let args = params
                    .iter()
                    .zip(&args.node)
                    .map(|(p, a)| ((&p.0).into(), self.expr(a)))
                    .collect();

                let to = self.new_tmp();
                self.add(Instruction::Call(to.clone(), ident.clone(), args));
                to.into()
            }
            ast::Expr::MethodCall(receiver, m, args) => {
                if let Some(t) = self.type_info.enum_variant_constructor(id) {
                    let t = t.clone();
                    let args: Vec<_> =
                        args.iter().map(|a| self.expr(a)).collect();

                    let [arg] = &args[..] else {
                        panic!("Should have been caught in typechecking");
                    };
                    let Type::Enum(_, fields) = t else {
                        panic!("Should have been caught in typechecking");
                    };
                    for (i, (f, _)) in fields.iter().enumerate() {
                        if m.node == f {
                            let to = self.new_tmp();
                            self.add(Instruction::CreateEnum {
                                to: to.clone(),
                                variant: i as u32,
                                data: arg.clone(),
                            });
                            return to.into();
                        }
                    }
                    panic!("Should have been caught in typechecking")
                }

                // It's not a constructor, so it's a method call!
                if let Some(f) = self.type_info.method(id) {
                    let f = f.clone();
                    let receiver = self.expr(receiver);
                    let mut all_args = vec![receiver];
                    all_args.extend(args.iter().map(|a| self.expr(a)));

                    let to = self.new_tmp();
                    self.add(Instruction::CallExternal(
                        to.clone(),
                        f,
                        all_args,
                    ));
                    return to.into();
                }

                todo!("method was declared but missing definition")
            }
            ast::Expr::Access(e, field) => {
                if let Some(t) = self.type_info.enum_variant_constructor(id) {
                    let Type::Enum(_, fields) = t else {
                        panic!("Should have been caught in typechecking");
                    };
                    for (i, (f, _)) in fields.iter().enumerate() {
                        if field.node == f {
                            let to = self.new_tmp();
                            self.add(Instruction::CreateEnum {
                                to: to.clone(),
                                variant: i as u32,
                                data: SafeValue::Unit.into(),
                            });
                            return to.into();
                        }
                    }
                    panic!("Should have been caught in typechecking")
                } else {
                    let val = self.expr(e);
                    let to = self.new_tmp();
                    self.add(Instruction::AccessRecord {
                        to: to.clone(),
                        record: val,
                        field: field.0.to_string(),
                    });
                    to.into()
                }
            }
            ast::Expr::Var(x) => {
                let var = self.type_info.full_name(x);
                Var { var }.into()
            }
            ast::Expr::TypedRecord(_, record) | ast::Expr::Record(record) => {
                let fields = record
                    .fields
                    .iter()
                    .map(|(s, expr)| ((&s.0).into(), self.expr(expr)))
                    .collect();
                let to = self.new_tmp();
                self.add(Instruction::CreateRecord {
                    to: to.clone(),
                    fields,
                });
                to.into()
            }
            ast::Expr::List(_) => todo!(),
            ast::Expr::Not(e) => {
                let val = self.expr(e);
                let place = self.new_tmp();
                self.add(Instruction::Assign {
                    to: place.clone(),
                    val,
                });
                place.into()
            }
            ast::Expr::BinOp(left, op, right) => {
                let ty = self.type_info.type_of(left.id);
                let left = self.expr(left);
                let right = self.expr(right);

                let place = self.new_tmp();
                match (op, ty) {
                    (ast::BinOp::Eq, Type::BuiltIn(_, i)) => {
                        let eq =
                            self.runtime.get_type(i).eq.as_ref().unwrap();
                        self.add(Instruction::CallExternal(
                            place.clone(),
                            eq.clone(),
                            vec![left, right],
                        ))
                    }
                    (_, _) => {
                        self.add(Instruction::BinOp {
                            to: place.clone(),
                            op: op.clone(),
                            left,
                            right,
                        });
                    }
                }

                place.into()
            }
            ast::Expr::IfElse(condition, if_true, if_false) => {
                let place = self.expr(condition);
                let res = self.new_tmp();

                let lbl_cont = self.new_unique_block_name("if-continue");
                let lbl_then = self.new_unique_block_name("if-then");
                let lbl_else = self.new_unique_block_name("if-else");

                let branches = vec![(1, lbl_then.clone())];

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
                if let Some(op) = self.block(if_true) {
                    self.add(Instruction::Assign {
                        to: res.clone(),
                        val: op,
                    });
                }
                self.add(Instruction::Jump(lbl_cont.clone()));

                if let Some(if_false) = if_false {
                    self.new_block(&lbl_else);
                    if let Some(op) = self.block(if_false) {
                        self.add(Instruction::Assign {
                            to: res.clone(),
                            val: op,
                        });
                    }
                    self.add(Instruction::Jump(lbl_cont.clone()));
                    self.new_block(&lbl_cont);
                    res.into()
                } else {
                    self.new_block(&lbl_cont);
                    SafeValue::Unit.into()
                }
            }
        }
    }

    /// Lower a literal
    fn literal(&mut self, lit: &Meta<Literal>) -> Operand<Var, SafeValue> {
        match &lit.node {
            Literal::String(_) => todo!(),
            Literal::Prefix(ast::Prefix { addr, len }) => {
                let addr = match addr.node {
                    ast::IpAddress::Ipv4(x) => IpAddr::V4(x),
                    ast::IpAddress::Ipv6(x) => IpAddr::V6(x),
                };
                SafeValue::from_any(Box::new(
                    routecore::addr::Prefix::new(addr, len.node).unwrap(),
                ))
                .into()
            }
            Literal::PrefixLength(n) => SafeValue::U8(*n).into(),
            Literal::Asn(n) => SafeValue::U32(*n).into(),
            Literal::IpAddress(addr) => {
                SafeValue::from_any(Box::new(match addr {
                    ast::IpAddress::Ipv4(x) => IpAddr::V4(*x),
                    ast::IpAddress::Ipv6(x) => IpAddr::V6(*x),
                }))
                .into()
            }
            Literal::ExtendedCommunity(_) => todo!(),
            Literal::StandardCommunity(_) => todo!(),
            Literal::LargeCommunity(_) => todo!(),
            Literal::Integer(x) => {
                let ty = self.type_info.type_of(lit);
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
}
