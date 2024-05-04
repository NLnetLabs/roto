//! Compiler stage to transform the AST into HIR
//!
//! For more information on the HIR, see the [ir] module.

pub mod eval;
pub mod ir;
mod match_expr;
pub mod value;

#[cfg(test)]
mod test_eval;

use ir::{Block, Function, Instruction, Operand, Var};
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
    function_name: &'r str,
    runtime: &'r Runtime,
    tmp_idx: usize,
    blocks: Vec<Block>,
    type_info: &'r mut TypeInfo,
    block_names: HashMap<String, usize>,
}

pub fn lower(
    runtime: &Runtime,
    tree: &ast::SyntaxTree,
    type_info: &mut TypeInfo,
) -> Vec<Function> {
    Lowerer::tree(runtime, type_info, tree)
}

impl<'r> Lowerer<'r> {
    fn new(
        runtime: &'r Runtime,
        type_info: &'r mut TypeInfo,
        function_name: &'r str,
    ) -> Self {
        Self {
            runtime,
            tmp_idx: 0,
            type_info,
            function_name,
            blocks: Vec::new(),
            block_names: HashMap::new(),
        }
    }
    /// Create a block with a unique name starting with a given prefix
    ///
    /// For example, the prefix `if-then` will give `if-then` for the first
    /// occurrence, `if-then.1` for the second, `if-then.2` for the third, etc.
    fn new_unique_block_name(&mut self, s: &str) -> String {
        let v = self.block_names.entry(s.to_string()).or_default();
        let name = if *v == 0 {
            s.to_string()
        } else {
            format!("{}::{s}.{v}", self.function_name)
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
    fn add(&mut self, instruction: Instruction) {
        let instructions = &mut self.blocks.last_mut().unwrap().instructions;

        // If the last instruction is a return or an exit, we know that
        // the instruction we push will never be executed, so we just
        // omit it. This should be done by an optimizer as well but this
        // makes our initial generated code just a bit nicer.
        if !matches!(
            instructions.last(),
            Some(Instruction::Return(_) | Instruction::Exit(..))
        ) {
            instructions.push(instruction);
        }
    }

    /// Create a new unique temporary variable
    fn new_tmp(&mut self) -> Var {
        let var = Var {
            var: format!("{}::$tmp-{}", self.function_name, self.tmp_idx),
        };
        self.tmp_idx += 1;
        var
    }

    /// Lower a syntax tree
    fn tree(
        runtime: &Runtime,
        type_info: &mut TypeInfo,
        tree: &ast::SyntaxTree,
    ) -> Vec<Function> {
        let ast::SyntaxTree { expressions } = tree;

        let mut functions = Vec::new();

        for expr in expressions {
            match expr {
                ast::Declaration::FilterMap(x) => {
                    functions.push(
                        Lowerer::new(runtime, type_info, x.ident.as_ref())
                            .filter_map(x),
                    );
                }
                ast::Declaration::Term(ast::TermDeclaration {
                    ident,
                    params,
                    body,
                }) => {
                    functions.push(
                        Lowerer::new(runtime, type_info, ident.as_ref())
                            .function(
                                ident,
                                params,
                                Type::Primitive(Primitive::Bool),
                                body,
                            ),
                    );
                }
                ast::Declaration::Action(ast::ActionDeclaration {
                    ident,
                    params,
                    body,
                }) => {
                    functions.push(
                        Lowerer::new(runtime, type_info, ident.as_ref())
                            .function(
                                ident,
                                params,
                                Type::Primitive(Primitive::Unit),
                                body,
                            ),
                    );
                }
                // Ignore the rest
                _ => {}
            }
        }

        functions
    }

    /// Lower a filter-map
    fn filter_map(mut self, fm: &ast::FilterMap) -> Function {
        let ast::FilterMap {
            ident,
            body,
            params,
            ..
        } = fm;
        let ast::FilterMapBody { define, apply } = body;

        let ident = self.type_info.full_name(ident);

        self.new_block(&ident);

        let parameters = params
            .0
            .iter()
            .map(|(x, _)| {
                (self.type_info.full_name(x), self.type_info.type_of(x))
            })
            .collect();

        for (ident, expr) in define {
            let val = self.expr(expr);
            let name = self.type_info.full_name(ident);
            let ty = self.type_info.type_of(ident);
            self.add(Instruction::Assign {
                to: Var { var: name },
                val,
                ty,
            })
        }

        let return_type = self.type_info.type_of(apply);
        let last = self.block(apply);

        self.add(Instruction::Return(last.unwrap_or(SafeValue::Unit.into())));

        Function {
            name: ident,
            parameters,
            return_type,
            blocks: self.blocks,
            public: true,
        }
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
        mut self,
        ident: &Meta<Identifier>,
        params: &Meta<ast::Params>,
        return_type: Type,
        body: &ast::Block,
    ) -> Function {
        let ident = self.type_info.full_name(ident);
        self.new_block(&ident);

        let parameters = params
            .0
            .iter()
            .map(|(x, _)| {
                (self.type_info.full_name(x), self.type_info.type_of(x))
            })
            .collect();

        let last = self.block(body);

        self.add(Instruction::Return(last.unwrap_or(SafeValue::Unit.into())));

        Function {
            name: ident,
            parameters,
            return_type,
            blocks: self.blocks,
            public: false,
        }
    }

    /// Lower a block
    ///
    /// Returns either the value of the expression or the place where the
    /// value can be retrieved.
    fn block(&mut self, block: &ast::Block) -> Option<Operand> {
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
    fn expr(&mut self, expr: &Meta<ast::Expr>) -> Operand {
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
                    .map(|(p, a)| (p.0.to_string(), self.expr(a)))
                    .collect();

                let to = self.new_tmp();
                let ty = self.type_info.type_of(id);
                self.add(Instruction::Call {
                    to: to.clone(),
                    ty,
                    func: ident.clone(),
                    args,
                });
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
                    let ty = self.type_info.type_of(id);
                    let receiver = self.expr(receiver);
                    let mut all_args = vec![receiver];
                    all_args.extend(args.iter().map(|a| self.expr(a)));

                    let to = self.new_tmp();
                    self.add(Instruction::CallExternal {
                        to: to.clone(),
                        ty,
                        func: f,
                        args: all_args,
                    });
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
                    let record_ty = self.type_info.type_of(&**e);
                    let val = self.expr(e);
                    let to = self.new_tmp();
                    self.add(Instruction::AccessRecord {
                        to: to.clone(),
                        record: val,
                        field: field.0.to_string(),
                        record_ty,
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
                let ty = self.type_info.type_of(id);
                self.add(Instruction::CreateRecord {
                    to: to.clone(),
                    fields,
                    ty,
                });
                to.into()
            }
            ast::Expr::List(_) => todo!(),
            ast::Expr::Not(e) => {
                let val = self.expr(e);
                let place = self.new_tmp();
                self.add(Instruction::Not {
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
                match (op, binop_to_cmp(op, &ty), ty) {
                    (ast::BinOp::Eq, _, Type::BuiltIn(_, i)) => {
                        let eq =
                            self.runtime.get_type(i).eq.as_ref().unwrap();
                        self.add(Instruction::CallExternal {
                            to: place.clone(),
                            ty: Type::Primitive(Primitive::Bool),
                            func: eq.clone(),
                            args: vec![left, right],
                        })
                    }
                    (_, Some(cmp), _) => {
                        self.add(Instruction::Cmp {
                            to: place.clone(),
                            cmp,
                            left,
                            right,
                        });
                    }
                    (ast::BinOp::And, _, _) => self.add(Instruction::And {
                        to: place.clone(),
                        left,
                        right,
                    }),
                    (ast::BinOp::Or, _, _) => self.add(Instruction::Or {
                        to: place.clone(),
                        left,
                        right,
                    }),
                    (ast::BinOp::Eq, _, _) => self.add(Instruction::Eq {
                        to: place.clone(),
                        left,
                        right,
                    }),
                    _ => todo!(),
                }

                place.into()
            }
            ast::Expr::IfElse(condition, if_true, if_false) => {
                let place = self.expr(condition);
                let res = self.new_tmp();

                let diverges = self.type_info.diverges(id);

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
                    let ty = self.type_info.type_of(if_true);
                    self.add(Instruction::Assign {
                        to: res.clone(),
                        val: op,
                        ty,
                    });
                }

                if !diverges {
                    self.add(Instruction::Jump(lbl_cont.clone()));
                }

                if let Some(if_false) = if_false {
                    self.new_block(&lbl_else);
                    if let Some(op) = self.block(if_false) {
                        let ty = self.type_info.type_of(if_false);
                        self.add(Instruction::Assign {
                            to: res.clone(),
                            val: op,
                            ty,
                        });
                    }
                    if !diverges {
                        self.add(Instruction::Jump(lbl_cont.clone()));
                        self.new_block(&lbl_cont);
                    }
                    res.into()
                } else {
                    if !diverges {
                        self.new_block(&lbl_cont);
                    }
                    SafeValue::Unit.into()
                }
            }
        }
    }

    /// Lower a literal
    fn literal(&mut self, lit: &Meta<Literal>) -> Operand {
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
                    Type::Primitive(Primitive::I8) => SafeValue::I8(*x as i8),
                    Type::Primitive(Primitive::I16) => {
                        SafeValue::I16(*x as i16)
                    }
                    Type::Primitive(Primitive::I32) => {
                        SafeValue::I32(*x as i32)
                    }
                    Type::IntVar(_) => SafeValue::I32(*x as i32),
                    _ => unreachable!("should be a type error: {ty}"),
                }
                .into()
            }
            Literal::Bool(x) => SafeValue::Bool(*x).into(),
        }
    }
}

fn binop_to_cmp(op: &ast::BinOp, ty: &Type) -> Option<ir::IntCmp> {
    let signed = match ty {
        Type::Primitive(p) => match p {
            Primitive::U32
            | Primitive::U16
            | Primitive::U8
            | Primitive::Bool => false,
            Primitive::I32 | Primitive::I16 | Primitive::I8 => true,
            Primitive::Unit => return None,
            Primitive::String => return None,
        },
        Type::IntVar(_) => true,
        _ => return None,
    };

    Some(match op {
        ast::BinOp::Eq => ir::IntCmp::Eq,
        ast::BinOp::Ne => ir::IntCmp::Ne,
        ast::BinOp::Lt if signed => ir::IntCmp::SLt,
        ast::BinOp::Le if signed => ir::IntCmp::SLe,
        ast::BinOp::Gt if signed => ir::IntCmp::SGt,
        ast::BinOp::Ge if signed => ir::IntCmp::SGe,
        ast::BinOp::Lt => ir::IntCmp::ULt,
        ast::BinOp::Le => ir::IntCmp::ULe,
        ast::BinOp::Gt => ir::IntCmp::UGt,
        ast::BinOp::Ge => ir::IntCmp::UGe,
        _ => return None,
    })
}
