//! Compiler stage to transform the AST into HIR
//!
//! For more information on the HIR, see the [ir] module.

pub mod eval;
pub mod ir;
pub mod label;
mod match_expr;
pub mod value;

#[cfg(test)]
mod test_eval;

use ir::{Block, Function, Instruction, Operand, Var, VarKind};
use label::{LabelRef, LabelStore};
use std::{any::TypeId, collections::HashMap, net::IpAddr};
use value::IrType;

use crate::{
    ast::{self, Identifier, Literal},
    parser::meta::Meta,
    runtime::{self, Movability, RuntimeFunction},
    typechecker::{
        info::TypeInfo,
        scope::{DefinitionRef, ScopeRef},
        types::{
            FunctionDefinition, FunctionKind, Primitive, Signature, Type,
        },
    },
    Runtime,
};

use self::value::IrValue;

/// Internal compiler error
macro_rules! ice {
    () => {
        panic!("ICE")
    };
    ($s:literal) => {
        panic!("ICE: {}", format!($s))
    };
    ($s:literal, $e:expr,*) => {
        panic!("ICE: {}", format!($s, $e,*))
    }
}

pub struct IrFunction {
    #[allow(dead_code)]
    pub name: Identifier,
    pub ptr: *const u8,
    pub params: Vec<IrType>,
    // We keep this for completeness.
    #[allow(dead_code)]
    pub ret: Option<IrType>,
}

struct Lowerer<'r> {
    function_name: Identifier,
    function_scope: ScopeRef,
    tmp_idx: usize,
    blocks: Vec<Block>,
    type_info: &'r mut TypeInfo,
    runtime_functions: &'r mut HashMap<usize, IrFunction>,
    label_store: &'r mut LabelStore,
    runtime: &'r Runtime,
    stack_slots: Vec<(Var, Type)>,
}

pub fn lower(
    tree: &ast::SyntaxTree,
    type_info: &mut TypeInfo,
    runtime_functions: &mut HashMap<usize, IrFunction>,
    label_store: &mut LabelStore,
    runtime: &Runtime,
) -> Vec<Function> {
    Lowerer::tree(type_info, runtime_functions, tree, label_store, runtime)
}

impl<'r> Lowerer<'r> {
    fn new(
        type_info: &'r mut TypeInfo,
        runtime_functions: &'r mut HashMap<usize, IrFunction>,
        function_name: &Meta<Identifier>,
        label_store: &'r mut LabelStore,
        runtime: &'r Runtime,
    ) -> Self {
        let function_scope = type_info.function_scope(function_name);
        Self {
            tmp_idx: 0,
            type_info,
            function_name: function_name.node,
            function_scope,
            runtime_functions,
            blocks: Vec::new(),
            stack_slots: Vec::new(),
            label_store,
            runtime,
        }
    }

    /// Add a new block to the blocks in the program.
    fn new_block(&mut self, label: LabelRef) {
        self.blocks.push(Block {
            label,
            instructions: Vec::new(),
        })
    }

    fn current_label(&self) -> LabelRef {
        self.blocks.last().unwrap().label
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
            scope: self.function_scope,
            kind: VarKind::Tmp(self.tmp_idx),
        };
        self.tmp_idx += 1;
        var
    }

    /// Lower a syntax tree
    fn tree(
        type_info: &mut TypeInfo,
        runtime_functions: &mut HashMap<usize, IrFunction>,
        tree: &ast::SyntaxTree,
        label_store: &mut LabelStore,
        runtime: &'r Runtime,
    ) -> Vec<Function> {
        let ast::SyntaxTree {
            declarations: expressions,
        } = tree;

        let mut functions = Vec::new();

        for expr in expressions {
            match expr {
                ast::Declaration::FilterMap(x) => {
                    functions.push(
                        Lowerer::new(
                            type_info,
                            runtime_functions,
                            &x.ident,
                            label_store,
                            runtime,
                        )
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
                        Lowerer::new(
                            type_info,
                            runtime_functions,
                            ident,
                            label_store,
                            runtime,
                        )
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

        let label = self.label_store.new_label(self.function_name);
        self.new_block(label);

        let parameter_types: Vec<_> = params
            .0
            .iter()
            .map(|(x, _)| {
                let ty = self.type_info.type_of(x);
                (self.type_info.resolved_name(x), ty)
            })
            .collect();

        for (def, ty) in &parameter_types {
            let var = Var {
                scope: def.0,
                kind: VarKind::Explicit(def.1),
            };
            self.stack_slots.push((var, ty.clone()))
        }

        for (ident, expr) in define {
            let val = self.expr(expr);
            let name = self.type_info.resolved_name(ident);
            let ty = self.type_info.type_of(ident);
            if self.type_info.size_of(&ty, self.runtime) > 0 {
                let val = val.unwrap();
                let ty = self.lower_type(&ty);
                self.add(Instruction::Assign {
                    to: Var {
                        scope: name.0,
                        kind: VarKind::Explicit(name.1),
                    },
                    val,
                    ty,
                })
            }
        }

        let return_type = self.type_info.type_of(apply);
        let last = self.block(apply);

        self.add(Instruction::Return(last));

        let signature = Signature {
            kind: FunctionKind::Free,
            parameter_types: parameter_types
                .iter()
                .cloned()
                .map(|x| x.1)
                .collect(),
            return_type: return_type.clone(),
        };

        let (return_type, return_ptr) = match return_type {
            x if self.type_info.size_of(&x, self.runtime) == 0 => {
                (None, false)
            }
            x if self.is_reference_type(&x) => (None, true),
            x => (Some(self.lower_type(&x)), false),
        };

        let ir_signature = ir::Signature {
            parameters: parameter_types
                .iter()
                .map(|(def, ty)| (def.1, self.lower_type(ty)))
                .collect(),
            return_ptr,
            return_type,
        };

        Function {
            name: ident.node,
            scope: self.function_scope,
            entry_block: label,
            signature,
            ir_signature,
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
        let label = self.label_store.new_label(self.function_name);
        self.new_block(label);

        let parameter_types: Vec<_> = params
            .0
            .iter()
            .map(|(x, _)| {
                let ty = self.type_info.type_of(x);
                (self.type_info.resolved_name(x), ty)
            })
            .collect();

        let return_type = return_type
            .as_ref()
            .map(|t| self.type_info.resolve(&Type::Name(**t)));

        let signature = Signature {
            kind: FunctionKind::Free,
            parameter_types: parameter_types
                .iter()
                .cloned()
                .map(|x| x.1)
                .collect(),
            return_type: return_type
                .clone()
                .unwrap_or(Type::Primitive(Primitive::Unit)),
        };

        let return_type = return_type.map(|t| self.lower_type(&t));

        let ir_signature = ir::Signature {
            parameters: parameter_types
                .iter()
                .map(|(def, ty)| (def.1, self.lower_type(ty)))
                .collect(),
            return_ptr: false, // TODO: check this
            return_type,
        };

        let last = self.block(body);

        self.add(Instruction::Return(last));

        Function {
            name: ident.node,
            scope: self.function_scope,
            entry_block: label,
            blocks: self.blocks,
            public: false,
            signature,
            ir_signature,
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
                        self.drop_locals();
                        self.add(Instruction::Return(op))
                    }
                    ast::ReturnKind::Accept => {
                        let Type::Verdict(accept_ty, _) = ty else {
                            ice!("accept must have type of verdict")
                        };

                        let var = Var {
                            scope: self.function_scope,
                            kind: VarKind::Return,
                        };

                        self.write_field(
                            var.clone().into(),
                            0,
                            IrValue::U8(0).into(),
                            &Type::Primitive(Primitive::U8),
                        );

                        if let Some(op) = op {
                            let offset = 1 + self.type_info.padding_of(
                                &accept_ty,
                                1,
                                self.runtime,
                            );

                            self.write_field(
                                var.into(),
                                offset,
                                op,
                                &accept_ty,
                            )
                        }
                        self.drop_locals();
                        self.add(Instruction::Return(None));
                    }
                    ast::ReturnKind::Reject => {
                        let Type::Verdict(_, reject_ty) = ty else {
                            ice!("reject must have a type of verdict")
                        };

                        let var = Var {
                            scope: self.function_scope,
                            kind: VarKind::Return,
                        };

                        self.write_field(
                            var.clone().into(),
                            0,
                            IrValue::U8(1).into(),
                            &Type::Primitive(Primitive::U8),
                        );

                        if let Some(op) = op {
                            let offset = 1 + self.type_info.padding_of(
                                &reject_ty,
                                1,
                                self.runtime,
                            );

                            self.write_field(
                                var.into(),
                                offset,
                                op,
                                &reject_ty,
                            )
                        }

                        self.drop_locals();
                        self.add(Instruction::Return(None));
                    }
                };

                None
            }
            ast::Expr::Literal(l) => Some(self.literal(l)),
            ast::Expr::Match(m) => self.match_expr(m),
            ast::Expr::FunctionCall(ident, args) => {
                let func = self.type_info.function(ident).clone();

                match &func.definition {
                    FunctionDefinition::Runtime(runtime_func) => {
                        let args: Vec<_> = args
                            .iter()
                            .map(|e| self.expr(e).unwrap())
                            .collect();

                        self.call_runtime_function(
                            **ident,
                            runtime_func,
                            args,
                            &func.signature.parameter_types,
                            &func.signature.return_type,
                        )
                    }
                    FunctionDefinition::Roto => {
                        let ty = self.type_info.type_of(ident);
                        let DefinitionRef(_, ident) =
                            self.type_info.resolved_name(ident);

                        let Type::Function(params, ret) = ty else {
                            ice!("can only call functions");
                        };

                        let args = params
                            .iter()
                            .zip(&args.node)
                            .flat_map(|(p, a)| {
                                Some((p.0.node, self.expr(a)?))
                            })
                            .collect();

                        let to = if self.type_info.size_of(&ret, self.runtime)
                            > 0
                        {
                            let ty = self.type_info.type_of(id);
                            let ty = self.lower_type(&ty);
                            Some((self.new_tmp(), ty))
                        } else {
                            None
                        };

                        self.add(Instruction::Call {
                            to: to.clone(),
                            func: ident,
                            args,
                        });

                        to.map(|(to, _ty)| to.into())
                    }
                }
            }
            ast::Expr::MethodCall(receiver, ident, args) => {
                // Check whether it is in fact not a method call, but an
                // enum constructor
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
                        .position(|(f, _)| ident.node == *f)
                        .unwrap();

                    let to = self.new_tmp();
                    let size = self.type_info.size_of(&ty, self.runtime);
                    let alignment =
                        self.type_info.alignment_of(&ty, self.runtime);
                    let align_shift = alignment.ilog2() as u8;

                    self.stack_slots.push((to.clone(), ty.clone()));

                    self.add(Instruction::Alloc {
                        to: to.clone(),
                        size,
                        align_shift,
                    });
                    self.add(Instruction::Write {
                        to: to.clone().into(),
                        val: IrValue::U8(idx as u8).into(),
                    });

                    if let Some(arg) = arg {
                        let offset = 1 + self.type_info.padding_of(
                            &ty,
                            1,
                            self.runtime,
                        );
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

                // It's a runtime-defined method, which we compile just like
                // a function
                let func = self.type_info.function(ident).clone();

                let runtime_func = match &func.definition {
                    FunctionDefinition::Roto => panic!("Not yet supported"),
                    FunctionDefinition::Runtime(rt) => rt,
                };

                let receiver_iter =
                    if let FunctionKind::Method(_) = func.signature.kind {
                        Some(&**receiver)
                    } else {
                        None
                    };

                let args: Vec<_> = receiver_iter
                    .into_iter()
                    .chain(&args.node)
                    .map(|e| self.expr(e).unwrap())
                    .collect();

                self.call_runtime_function(
                    **ident,
                    runtime_func,
                    args,
                    &func.signature.parameter_types,
                    &func.signature.return_type,
                )
            }
            ast::Expr::Access(e, field) => {
                if let Some(ty) = self.type_info.enum_variant_constructor(id)
                {
                    let ty = ty.clone();
                    let Type::Enum(_, variants) = ty.clone() else {
                        ice!("it's an enum variant constructor, so the type must be an enum")
                    };

                    let size = self.type_info.size_of(&ty, self.runtime);
                    let alignment =
                        self.type_info.alignment_of(&ty, self.runtime);
                    let align_shift = alignment.ilog2() as u8;

                    let Some(idx) =
                        variants.iter().position(|(f, _)| field.node == *f)
                    else {
                        ice!("expected field to be present")
                    };

                    let to = self.new_tmp();
                    self.add(Instruction::Alloc {
                        to: to.clone(),
                        size,
                        align_shift,
                    });

                    self.add(Instruction::Write {
                        to: to.clone().into(),
                        val: IrValue::U8(idx as u8).into(),
                    });

                    Some(to.into())
                } else {
                    let record_ty = self.type_info.type_of(&**e);
                    let op = self.expr(e)?;
                    let (ty, offset) = self.type_info.offset_of(
                        &record_ty,
                        **field,
                        self.runtime,
                    );
                    if self.type_info.size_of(&ty, self.runtime) > 0 {
                        Some(self.read_field(op, offset, &ty))
                    } else {
                        None
                    }
                }
            }
            ast::Expr::Var(x) => {
                let DefinitionRef(scope, ident) =
                    self.type_info.resolved_name(x);
                Some(
                    Var {
                        scope,
                        kind: VarKind::Explicit(ident),
                    }
                    .into(),
                )
            }
            ast::Expr::TypedRecord(_, record) | ast::Expr::Record(record) => {
                let ty = self.type_info.type_of(id);
                let size = self.type_info.size_of(&ty, self.runtime);
                let alignment =
                    self.type_info.alignment_of(&ty, self.runtime);
                let align_shift = alignment.ilog2() as u8;

                let fields: Vec<_> = record
                    .fields
                    .iter()
                    .flat_map(|(s, expr)| Some((s, self.expr(expr)?)))
                    .collect();

                if size == 0 {
                    return None;
                }

                let to = self.new_tmp();
                self.add(Instruction::Alloc {
                    to: to.clone(),
                    size,
                    align_shift,
                });

                for (field_name, field_operand) in fields {
                    let (ty, offset) = self.type_info.offset_of(
                        &ty,
                        field_name.node,
                        self.runtime,
                    );
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

                let left = self.expr(left);
                let right = self.expr(right);

                if self.type_info.size_of(&ty, self.runtime) == 0 {
                    return Some(
                        IrValue::Bool(match op {
                            ast::BinOp::Eq => true,
                            ast::BinOp::Ne => false,
                            _ => panic!(),
                        })
                        .into(),
                    );
                }

                let left = left.unwrap();
                let right = right.unwrap();

                let place = self.new_tmp();
                match (op, binop_to_cmp(op, &ty), ty) {
                    (
                        ast::BinOp::Div,
                        _,
                        Type::Primitive(Primitive::IpAddr),
                    ) => {
                        let function = self.type_info.function(id);
                        let FunctionDefinition::Runtime(runtime_func) =
                            function.definition.clone()
                        else {
                            panic!()
                        };

                        let size = self.type_info.size_of(
                            &Type::Primitive(Primitive::Prefix),
                            self.runtime,
                        );
                        let alignment = self.type_info.alignment_of(
                            &Type::Primitive(Primitive::Prefix),
                            self.runtime,
                        );
                        let align_shift = alignment.ilog2() as u8;
                        self.add(Instruction::Alloc {
                            to: place.clone(),
                            size,
                            align_shift,
                        });

                        let ident = Identifier::from("new");
                        let ir_func = IrFunction {
                            name: ident,
                            ptr: runtime_func.description.pointer(),
                            params: vec![
                                IrType::Pointer,
                                IrType::Pointer,
                                IrType::U8,
                            ],
                            ret: None,
                        };

                        self.runtime_functions
                            .insert(runtime_func.id, ir_func);

                        self.add(Instruction::CallRuntime {
                            to: None,
                            func: runtime_func,
                            args: vec![place.clone().into(), left, right],
                        });
                    }
                    (
                        ast::BinOp::Eq,
                        _,
                        Type::Primitive(Primitive::IpAddr),
                    ) => {
                        let ip_addr_type_id = TypeId::of::<IpAddr>();
                        let runtime_func = self
                            .find_runtime_function(
                                runtime::FunctionKind::Method(
                                    ip_addr_type_id,
                                ),
                                "eq",
                            )
                            .clone();

                        let out = self
                            .call_runtime_function(
                                Identifier::from("eq"),
                                &runtime_func,
                                [left, right],
                                &[
                                    Type::Primitive(Primitive::IpAddr),
                                    Type::Primitive(Primitive::IpAddr),
                                ],
                                &Type::Primitive(Primitive::Bool),
                            )
                            .unwrap();
                        self.add(Instruction::Assign {
                            to: place.clone(),
                            val: out,
                            ty: IrType::Bool,
                        })
                    }
                    (
                        ast::BinOp::Ne,
                        _,
                        Type::Primitive(Primitive::IpAddr),
                    ) => {
                        let ip_addr_type_id = TypeId::of::<IpAddr>();
                        let runtime_func = self
                            .find_runtime_function(
                                runtime::FunctionKind::Method(
                                    ip_addr_type_id,
                                ),
                                "eq",
                            )
                            .clone();

                        let out = self
                            .call_runtime_function(
                                Identifier::from("eq"),
                                &runtime_func,
                                [left, right],
                                &[
                                    Type::Primitive(Primitive::IpAddr),
                                    Type::Primitive(Primitive::IpAddr),
                                ],
                                &Type::Primitive(Primitive::Bool),
                            )
                            .unwrap();

                        self.add(Instruction::Not {
                            to: place.clone(),
                            val: out,
                        })
                    }
                    (ast::BinOp::Eq, _, ty)
                        if self.is_reference_type(&ty) =>
                    {
                        let size = self.type_info.size_of(&ty, self.runtime);
                        let tmp = self.new_tmp();
                        self.add(Instruction::MemCmp {
                            to: tmp.clone(),
                            size: IrValue::Pointer(size as usize).into(),
                            left: left.clone(),
                            right: right.clone(),
                        });
                        self.add(Instruction::Cmp {
                            to: place.clone(),
                            cmp: ir::IntCmp::Eq,
                            left: tmp.into(),
                            right: IrValue::Pointer(0).into(),
                        })
                    }
                    (ast::BinOp::Ne, _, ty)
                        if self.is_reference_type(&ty) =>
                    {
                        let size = self.type_info.size_of(&ty, self.runtime);
                        let tmp = self.new_tmp();
                        self.add(Instruction::MemCmp {
                            to: tmp.clone(),
                            size: IrValue::Pointer(size as usize).into(),
                            left: left.clone(),
                            right: right.clone(),
                        });
                        self.add(Instruction::Cmp {
                            to: place.clone(),
                            cmp: ir::IntCmp::Ne,
                            left: tmp.into(),
                            right: IrValue::Pointer(0).into(),
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
                    (ast::BinOp::Add, _, _) => self.add(Instruction::Add {
                        to: place.clone(),
                        left,
                        right,
                    }),
                    (ast::BinOp::Sub, _, _) => self.add(Instruction::Sub {
                        to: place.clone(),
                        left,
                        right,
                    }),
                    (ast::BinOp::Mul, _, _) => self.add(Instruction::Mul {
                        to: place.clone(),
                        left,
                        right,
                    }),
                    (ast::BinOp::Div, _, ty) => {
                        let ty = self.lower_type(&ty);
                        self.add(Instruction::Div {
                            to: place.clone(),
                            ty,
                            left,
                            right,
                        })
                    }
                    _ => todo!(),
                }

                Some(place.into())
            }
            ast::Expr::IfElse(condition, if_true, if_false) => {
                let place = self.expr(condition).unwrap();
                let res = self.new_tmp();
                let mut any_assigned = false;

                let diverges = self.type_info.diverges(id);

                let current_label = self.current_label();
                let lbl_cont = self.label_store.next(current_label);
                let lbl_then = self.label_store.wrap_internal(
                    current_label,
                    Identifier::from("if-then"),
                );
                let lbl_else = self.label_store.wrap_internal(
                    current_label,
                    Identifier::from("if-else"),
                );

                let branches = vec![(1, lbl_then)];

                self.add(Instruction::Switch {
                    examinee: place,
                    branches,
                    default: if if_false.is_some() {
                        lbl_else
                    } else {
                        lbl_cont
                    },
                });

                self.new_block(lbl_then);
                if let Some(op) = self.block(if_true) {
                    let ty = self.type_info.type_of(if_true);
                    let ty = self.lower_type(&ty);
                    self.add(Instruction::Assign {
                        to: res.clone(),
                        val: op,
                        ty,
                    });
                    any_assigned = true;
                }

                if !diverges {
                    self.add(Instruction::Jump(lbl_cont));
                }

                if let Some(if_false) = if_false {
                    self.new_block(lbl_else);
                    if let Some(op) = self.block(if_false) {
                        let ty = self.type_info.type_of(if_false);
                        let ty = self.lower_type(&ty);
                        self.add(Instruction::Assign {
                            to: res.clone(),
                            val: op,
                            ty,
                        });
                        any_assigned = true;
                    }
                    if !diverges {
                        self.add(Instruction::Jump(lbl_cont));
                        self.new_block(lbl_cont);
                    }
                    any_assigned.then(|| res.into())
                } else {
                    if !diverges {
                        self.new_block(lbl_cont);
                    }
                    None
                }
            }
        }
    }

    fn find_runtime_function(
        &self,
        kind: runtime::FunctionKind,
        name: &str,
    ) -> &RuntimeFunction {
        self.runtime
            .functions
            .iter()
            .find(|f| f.kind == kind && f.name == name)
            .unwrap()
    }

    /// Lower a literal
    fn literal(&mut self, lit: &Meta<Literal>) -> Operand {
        match &lit.node {
            Literal::String(_) => todo!(),
            Literal::Asn(n) => IrValue::Asn(*n).into(),
            Literal::IpAddress(addr) => {
                let to = self.new_tmp();
                const SIZE: usize = std::mem::size_of::<IpAddr>();
                const ALIGN: usize = std::mem::align_of::<IpAddr>();
                let align_shift = ALIGN.ilog2() as u8;

                let x: [u8; SIZE] = unsafe { std::mem::transmute_copy(addr) };
                self.add(Instruction::Initialize {
                    to: to.clone(),
                    bytes: x.into(),
                    align_shift,
                });
                to.into()
            }
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
                    _ => ice!("should be a type error"),
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

        let ty = self.type_info.resolve(ty);
        if self.is_reference_type(&ty) {
            let size = self.type_info.size_of(&ty, self.runtime);
            let clone = self.get_clone_function(&ty);
            self.add(Instruction::Copy {
                to: tmp.into(),
                from: val,
                size,
                clone,
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

        if self.is_reference_type(&ty) {
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

            let ty = self.lower_type(&ty);
            self.add(Instruction::Read {
                to: to.clone(),
                from: tmp.into(),
                ty,
            });
        }

        to.into()
    }

    fn is_reference_type(&mut self, ty: &Type) -> bool {
        let ty = self.type_info.resolve(ty);
        matches!(
            ty,
            Type::Record(..)
                | Type::RecordVar(..)
                | Type::NamedRecord(..)
                | Type::Enum(..)
                | Type::Verdict(..)
                | Type::Primitive(Primitive::IpAddr | Primitive::Prefix)
                | Type::BuiltIn(..)
        )
    }

    fn lower_type(&mut self, ty: &Type) -> IrType {
        let ty = self.type_info.resolve(ty);
        match ty {
            Type::Primitive(Primitive::Bool) => IrType::Bool,
            Type::Primitive(Primitive::U8) => IrType::U8,
            Type::Primitive(Primitive::U16) => IrType::U16,
            Type::Primitive(Primitive::U32) => IrType::U32,
            Type::Primitive(Primitive::U64) => IrType::U64,
            Type::Primitive(Primitive::I8) => IrType::I8,
            Type::Primitive(Primitive::I16) => IrType::I16,
            Type::Primitive(Primitive::I32) => IrType::I32,
            Type::Primitive(Primitive::I64) => IrType::I64,
            Type::Primitive(Primitive::Asn) => IrType::Asn,
            Type::Primitive(Primitive::IpAddr) => IrType::Pointer,
            Type::IntVar(_) => IrType::I32,
            Type::BuiltIn(_, _) => IrType::ExtPointer,
            x if self.is_reference_type(&x) => IrType::Pointer,
            _ => panic!("could not lower: {ty:?}"),
        }
    }

    fn call_runtime_function(
        &mut self,
        ident: Identifier,
        runtime_func: &RuntimeFunction,
        args: impl IntoIterator<Item = Operand>,
        parameter_types: &[Type],
        return_type: &Type,
    ) -> Option<Operand> {
        let out_ptr = self.new_tmp();
        let size = self.type_info.size_of(return_type, self.runtime);
        self.add(Instruction::Alloc {
            to: out_ptr.clone(),
            size,
            align_shift: 0,
        });

        let mut params = Vec::new();
        params.push(IrType::Pointer);

        for ty in parameter_types {
            params.push(self.lower_type(ty))
        }

        let args = std::iter::once(Operand::Place(out_ptr.clone()))
            .chain(args)
            .collect();

        let ir_func = IrFunction {
            name: ident,
            ptr: runtime_func.description.pointer(),
            params,
            ret: None,
        };

        self.runtime_functions.insert(runtime_func.id, ir_func);

        self.add(Instruction::CallRuntime {
            to: None,
            func: runtime_func.clone(),
            args,
        });

        if self.is_reference_type(return_type) {
            Some(out_ptr.into())
        } else if size > 0 {
            Some(self.read_field(out_ptr.into(), 0, return_type))
        } else {
            None
        }
    }

    fn get_clone_function(
        &self,
        ty: &Type,
    ) -> Option<unsafe extern "C" fn(*const (), *mut ())> {
        let Type::BuiltIn(_, id) = ty else {
            return None;
        };

        let ty = self.runtime.get_runtime_type(*id)?;

        let &Movability::CloneDrop { clone, .. } = ty.movability() else {
            return None;
        };

        Some(clone)
    }

    fn get_drop_function(
        &self,
        ty: &Type,
    ) -> Option<unsafe extern "C" fn(*mut ())> {
        let Type::BuiltIn(_, id) = ty else {
            return None;
        };

        let ty = self.runtime.get_runtime_type(*id)?;

        let &Movability::CloneDrop { drop, .. } = ty.movability() else {
            return None;
        };

        Some(drop)
    }

    fn drop_locals(&mut self) {
        let mut instructions = Vec::new();
        for (var, ty) in &self.stack_slots {
            let drop = self.get_drop_function(ty);
            instructions.push(Instruction::Drop {
                var: var.clone(),
                drop,
            })
        }
        for inst in instructions {
            self.add(inst)
        }
    }
}

fn binop_to_cmp(op: &ast::BinOp, ty: &Type) -> Option<ir::IntCmp> {
    let signed = match ty {
        Type::Primitive(p) => match p {
            Primitive::U64
            | Primitive::U32
            | Primitive::U16
            | Primitive::U8
            | Primitive::Asn
            | Primitive::IpAddr
            | Primitive::Prefix
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
