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
use value::IrType;

use crate::{
    ast::{self, Identifier, Literal},
    parser::meta::Meta,
    runtime::Runtime,
    typechecker::{
        types::{Primitive, Type},
        TypeInfo,
    },
};

use self::value::IrValue;

/// Internal compiler error
macro_rules! ice {
    () => {
        panic!("ICE")
    };
}

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
        if !matches!(instructions.last(), Some(Instruction::Return(_))) {
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
                ast::Declaration::Function(ast::FunctionDeclaration {
                    ident,
                    params,
                    body,
                    ret,
                }) => {
                    functions.push(
                        Lowerer::new(runtime, type_info, ident.as_ref())
                            .function(ident, params, ret, body),
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

        let mut parameters: Vec<_> = params
            .0
            .iter()
            .map(|(x, _)| {
                (
                    self.type_info.full_name(x),
                    lower_type(&self.type_info.type_of(x)),
                )
            })
            .collect();

        for (ident, expr) in define {
            let val = self.expr(expr);
            let name = self.type_info.full_name(ident);
            let ty = self.type_info.type_of(ident);
            if ty != Type::Primitive(Primitive::Unit) {
                let val = val.unwrap();
                self.add(Instruction::Assign {
                    to: Var { var: name },
                    val,
                    ty: lower_type(&ty),
                })
            }
        }

        let return_type = self.type_info.type_of(apply);
        let last = self.block(apply);

        self.add(Instruction::Return(last));

        let return_type = match return_type {
            Type::Primitive(Primitive::Unit) => None,
            x if is_reference_type(&x) => {
                parameters.insert(
                    0,
                    (format!("{ident}::$return"), IrType::Pointer),
                );
                None
            }
            x => Some(lower_type(&x)),
        };

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
        return_type: &Option<Meta<Identifier>>,
        body: &ast::Block,
    ) -> Function {
        let ident = self.type_info.full_name(ident);
        self.new_block(&ident);

        let parameters = params
            .0
            .iter()
            .map(|(x, _)| {
                (
                    self.type_info.full_name(x),
                    lower_type(&self.type_info.type_of(x)),
                )
            })
            .collect();

        let return_type = return_type
            .as_ref()
            .map(|t| self.type_info.resolve(&Type::Name(t.to_string())))
            .map(|t| lower_type(&t));

        let last = self.block(body);

        self.add(Instruction::Return(last));

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

        block.last.as_ref().and_then(|last| self.expr(last))
    }

    /// Lower an expression
    ///
    /// Returns either the value of the expression or the place where the
    /// value can be retrieved. The nested expressions will be lowered
    /// recursively.
    fn expr(&mut self, expr: &Meta<ast::Expr>) -> Option<Operand> {
        let id = expr.id;
        match &expr.node {
            ast::Expr::Return(kind, e) => {
                let op = e.as_ref().and_then(|e| self.expr(e.as_ref()));
                let ty = self.type_info.return_type_of(id);

                match kind {
                    ast::ReturnKind::Return => {
                        // TODO: Return reference types
                        self.add(Instruction::Return(op))
                    }
                    ast::ReturnKind::Accept => {
                        let Type::Verdict(accept_ty, _) = ty else {
                            ice!()
                        };

                        let var = Var {
                            var: format!("{}::$return", self.function_name),
                        };

                        self.write_field(
                            var.clone().into(),
                            0,
                            IrValue::U8(1).into(),
                            &Type::Primitive(Primitive::U8),
                        );

                        if let Some(op) = op {
                            let offset =
                                self.type_info.padding_of(&accept_ty, 1);

                            self.write_field(
                                var.into(),
                                offset,
                                op,
                                &accept_ty,
                            )
                        }

                        self.add(Instruction::Return(None));
                    }
                    ast::ReturnKind::Reject => {
                        let Type::Verdict(_, reject_ty) = ty else {
                            ice!()
                        };

                        let var = Var {
                            var: format!("{}::$return", self.function_name),
                        };

                        self.write_field(
                            var.clone().into(),
                            0,
                            IrValue::U8(0).into(),
                            &Type::Primitive(Primitive::U8),
                        );

                        if let Some(op) = op {
                            let offset =
                                self.type_info.padding_of(&reject_ty, 1);

                            self.write_field(
                                var.into(),
                                offset,
                                op,
                                &reject_ty,
                            )
                        }

                        self.add(Instruction::Return(None));
                    }
                };

                None
            }
            ast::Expr::Literal(l) => Some(self.literal(l)),
            ast::Expr::Match(m) => self.match_expr(m),
            ast::Expr::PrefixMatch(_) => todo!(),
            ast::Expr::FunctionCall(ident, args) => {
                let ty = self.type_info.type_of(ident);
                let ident = self.type_info.full_name(ident);

                let Type::Function(params, ret) = ty else {
                    ice!();
                };

                let args = params
                    .iter()
                    .zip(&args.node)
                    .flat_map(|(p, a)| Some((p.0.to_string(), self.expr(a)?)))
                    .collect();

                let to = if *ret == Type::Primitive(Primitive::Unit) {
                    None
                } else {
                    Some(self.new_tmp())
                };
                let ty = self.type_info.type_of(id);
                self.add(Instruction::Call {
                    to: to.clone(),
                    ty: lower_type(&ty),
                    func: ident.clone(),
                    args,
                });

                to.map(Into::into)
            }
            ast::Expr::MethodCall(receiver, m, args) => {
                if let Some(ty) = self.type_info.enum_variant_constructor(id)
                {
                    let ty = ty.clone();
                    let args: Vec<_> =
                        args.iter().map(|a| self.expr(a)).collect();

                    let [arg] = &args[..] else {
                        ice!();
                    };
                    let Type::Enum(_, variants) = ty.clone() else {
                        ice!();
                    };

                    let idx = variants
                        .iter()
                        .position(|(f, _)| m.node == f)
                        .unwrap();

                    let to = self.new_tmp();
                    let size = self.type_info.size_of(&ty);
                    self.add(Instruction::Alloc {
                        to: to.clone(),
                        size,
                    });
                    self.add(Instruction::Write {
                        to: to.clone().into(),
                        val: IrValue::U8(idx as u8).into(),
                    });

                    if let Some(arg) = arg {
                        let offset = 1 + self.type_info.padding_of(&ty, 1);
                        let tmp = self.new_tmp();
                        self.add(Instruction::Offset {
                            to: tmp.clone(),
                            from: to.clone().into(),
                            offset,
                        });
                        self.add(Instruction::Write {
                            to: to.clone().into(),
                            val: arg.clone(),
                        })
                    }

                    return Some(to.into());
                }

                // It's not a constructor, so it's a method call!
                if let Some(f) = self.type_info.method(id) {
                    let f = f.clone();
                    let ty = self.type_info.type_of(id);
                    let mut all_args = Vec::new();
                    if let Some(receiver) = self.expr(receiver) {
                        all_args.push(receiver);
                    }
                    all_args.extend(args.iter().flat_map(|a| self.expr(a)));

                    let to = self.new_tmp();
                    self.add(Instruction::CallExternal {
                        to: to.clone(),
                        ty: lower_type(&ty),
                        func: f,
                        args: all_args,
                    });
                    return Some(to.into());
                }

                todo!("method was declared but missing definition")
            }
            ast::Expr::Access(e, field) => {
                if let Some(ty) = self.type_info.enum_variant_constructor(id)
                {
                    let ty = ty.clone();
                    let Type::Enum(_, variants) = ty.clone() else {
                        ice!()
                    };

                    let size = self.type_info.size_of(&ty);

                    let idx = variants
                        .iter()
                        .position(|(f, _)| field.node == f)
                        .unwrap();

                    let to = self.new_tmp();
                    self.add(Instruction::Alloc {
                        to: to.clone(),
                        size,
                    });

                    self.add(Instruction::Write {
                        to: to.clone().into(),
                        val: IrValue::U8(idx as u8).into(),
                    });

                    Some(to.into())
                } else {
                    let record_ty = self.type_info.type_of(&**e);
                    let op = self.expr(e).unwrap();
                    let (ty, offset) =
                        self.type_info.offset_of(&record_ty, field.as_ref());
                    Some(self.read_field(op, offset, &ty))
                }
            }
            ast::Expr::Var(x) => {
                let var = self.type_info.full_name(x);
                Some(Var { var }.into())
            }
            ast::Expr::TypedRecord(_, record) | ast::Expr::Record(record) => {
                let ty = self.type_info.type_of(id);
                let size = self.type_info.size_of(&ty);

                let fields: Vec<(&str, _)> = record
                    .fields
                    .iter()
                    .flat_map(|(s, expr)| {
                        Some((s.0.as_ref(), self.expr(expr)?))
                    })
                    .collect();

                let to = self.new_tmp();
                self.add(Instruction::Alloc {
                    to: to.clone(),
                    size,
                });

                for (field_name, field_operand) in fields {
                    let (ty, offset) =
                        self.type_info.offset_of(&ty, field_name);
                    self.write_field(
                        to.clone().into(),
                        offset,
                        field_operand,
                        &ty,
                    );
                }

                Some(to.into())
            }
            ast::Expr::List(_) => todo!(),
            ast::Expr::Not(e) => {
                let val = self.expr(e).unwrap();
                let place = self.new_tmp();
                self.add(Instruction::Not {
                    to: place.clone(),
                    val,
                });
                Some(place.into())
            }
            ast::Expr::BinOp(left, op, right) => {
                let ty = self.type_info.type_of(left.id);
                let left = self.expr(left).unwrap();
                let right = self.expr(right).unwrap();

                let place = self.new_tmp();
                match (op, binop_to_cmp(op, &ty), ty) {
                    (ast::BinOp::Eq, _, Type::BuiltIn(_, i)) => {
                        let eq =
                            self.runtime.get_type(i).eq.as_ref().unwrap();
                        self.add(Instruction::CallExternal {
                            to: place.clone(),
                            ty: IrType::Bool,
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

                Some(place.into())
            }
            ast::Expr::IfElse(condition, if_true, if_false) => {
                let place = self.expr(condition).unwrap();
                let res = self.new_tmp();
                let mut any_assigned = false;

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
                        ty: lower_type(&ty),
                    });
                    any_assigned = true;
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
                            ty: lower_type(&ty),
                        });
                        any_assigned = true;
                    }
                    if !diverges {
                        self.add(Instruction::Jump(lbl_cont.clone()));
                        self.new_block(&lbl_cont);
                    }
                    any_assigned.then(|| res.into())
                } else {
                    if !diverges {
                        self.new_block(&lbl_cont);
                    }
                    None
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
                IrValue::from_any(Box::new(
                    routecore::addr::Prefix::new(addr, len.node).unwrap(),
                ))
                .into()
            }
            Literal::PrefixLength(n) => IrValue::U8(*n).into(),
            Literal::Asn(n) => IrValue::U32(*n).into(),
            Literal::IpAddress(addr) => {
                IrValue::from_any(Box::new(match addr {
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
                    Type::Primitive(Primitive::U8) => IrValue::U8(*x as u8),
                    Type::Primitive(Primitive::U16) => {
                        IrValue::U16(*x as u16)
                    }
                    Type::Primitive(Primitive::U32) => {
                        IrValue::U32(*x as u32)
                    }
                    Type::Primitive(Primitive::I8) => IrValue::I8(*x as i8),
                    Type::Primitive(Primitive::I16) => {
                        IrValue::I16(*x as i16)
                    }
                    Type::Primitive(Primitive::I32) => {
                        IrValue::I32(*x as i32)
                    }
                    Type::IntVar(_) => IrValue::I32(*x as i32),
                    _ => unreachable!("should be a type error: {ty}"),
                }
                .into()
            }
            Literal::Bool(x) => IrValue::Bool(*x).into(),
        }
    }

    fn write_field(
        &mut self,
        to: Operand,
        offset: u32,
        val: Operand,
        ty: &Type,
    ) {
        let tmp = self.new_tmp();
        self.add(Instruction::Offset {
            to: tmp.clone(),
            from: to,
            offset,
        });

        if is_reference_type(ty) {
            let size = self.type_info.size_of(ty);
            self.add(Instruction::Copy {
                to: tmp.into(),
                from: val,
                size,
            });
        } else {
            self.add(Instruction::Write {
                to: tmp.into(),
                val,
            })
        }
    }

    fn read_field(
        &mut self,
        from: Operand,
        offset: u32,
        ty: &Type,
    ) -> Operand {
        let ty = self.type_info.resolve(ty);
        let to = self.new_tmp();

        if is_reference_type(&ty) {
            self.add(Instruction::Offset {
                to: to.clone(),
                from,
                offset,
            })
        } else {
            let tmp = self.new_tmp();

            self.add(Instruction::Offset {
                to: tmp.clone(),
                from,
                offset,
            });

            self.add(Instruction::Read {
                to: to.clone(),
                from: tmp.into(),
                ty: lower_type(&ty),
            });
        }

        to.into()
    }
}

fn lower_type(ty: &Type) -> IrType {
    match ty {
        Type::Primitive(Primitive::Bool) => IrType::Bool,
        Type::Primitive(Primitive::U8) => IrType::U8,
        Type::Primitive(Primitive::U16) => IrType::U16,
        Type::Primitive(Primitive::U32) => IrType::U32,
        Type::Primitive(Primitive::U64) => IrType::U64,
        Type::Primitive(Primitive::I8) => IrType::I8,
        Type::Primitive(Primitive::I16) => IrType::I16,
        Type::Primitive(Primitive::I64) => IrType::I64,
        Type::IntVar(_) => IrType::I32,
        x if is_reference_type(x) => IrType::Pointer,
        _ => panic!("could not lower: {ty}"),
    }
}

fn is_reference_type(t: &Type) -> bool {
    matches!(
        t,
        Type::Record(..)
            | Type::RecordVar(..)
            | Type::NamedRecord(..)
            | Type::Enum(..)
            | Type::Verdict(..)
    )
}

fn binop_to_cmp(op: &ast::BinOp, ty: &Type) -> Option<ir::IntCmp> {
    let signed = match ty {
        Type::Primitive(p) => match p {
            Primitive::U64
            | Primitive::U32
            | Primitive::U16
            | Primitive::U8
            | Primitive::Bool => false,
            Primitive::I64
            | Primitive::I32
            | Primitive::I16
            | Primitive::I8 => true,
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
