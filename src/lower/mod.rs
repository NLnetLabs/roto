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
    module::ModuleTree,
    parser::meta::Meta,
    runtime::{self, Movability, RuntimeFunction, RuntimeFunctionRef},
    typechecker::{
        info::TypeInfo,
        scope::{DeclarationKind, ScopeRef, ValueKind},
        types::{
            FunctionDefinition, FunctionKind, Primitive, Signature, Type,
        },
        PathValue, ResolvedPath,
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
    runtime_functions: &'r mut HashMap<RuntimeFunctionRef, IrFunction>,
    label_store: &'r mut LabelStore,
    runtime: &'r Runtime,
    stack_slots: Vec<(Var, Type)>,
}

pub fn lower(
    tree: &ModuleTree,
    type_info: &mut TypeInfo,
    runtime_functions: &mut HashMap<RuntimeFunctionRef, IrFunction>,
    label_store: &mut LabelStore,
    runtime: &Runtime,
) -> Vec<Function> {
    Lowerer::tree(type_info, runtime_functions, tree, label_store, runtime)
}

impl<'r> Lowerer<'r> {
    fn new(
        type_info: &'r mut TypeInfo,
        runtime_functions: &'r mut HashMap<RuntimeFunctionRef, IrFunction>,
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
        runtime_functions: &mut HashMap<RuntimeFunctionRef, IrFunction>,
        tree: &ModuleTree,
        label_store: &mut LabelStore,
        runtime: &'r Runtime,
    ) -> Vec<Function> {
        let mut functions = Vec::new();

        for m in &tree.modules {
            for d in &m.ast.declarations {
                match d {
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
                    ast::Declaration::Function(x) => {
                        functions.push(
                            Lowerer::new(
                                type_info,
                                runtime_functions,
                                &x.ident,
                                label_store,
                                runtime,
                            )
                            .function(x),
                        );
                    }
                    ast::Declaration::Test(x) => {
                        functions.push(
                            Lowerer::new(
                                type_info,
                                runtime_functions,
                                &Meta {
                                    node: format!("test#{}", x.ident).into(),
                                    id: x.ident.id,
                                },
                                label_store,
                                runtime,
                            )
                            .test(x),
                        );
                    }
                    // Ignore the rest
                    _ => {}
                }
            }
        }
        functions
    }

    /// Lower a filtermap
    fn filter_map(self, fm: &ast::FilterMap) -> Function {
        let ast::FilterMap {
            ident,
            body,
            params,
            ..
        } = fm;

        let return_type = self.type_info.type_of(body);
        self.function_like(ident, params, &return_type, body)
    }

    fn function(self, function: &ast::FunctionDeclaration) -> Function {
        let name = self.type_info.resolved_name(&function.ident);
        let dec = self.type_info.scope_graph.get_declaration(name);

        let DeclarationKind::Function(_, ty) = dec.kind else {
            unreachable!();
        };

        let Type::Function(_, ret) = self.type_info.resolve(&ty) else {
            unreachable!();
        };

        self.function_like(
            &function.ident,
            &function.params,
            &ret,
            &function.body,
        )
    }

    fn test(self, test: &ast::Test) -> Function {
        let ident = Meta {
            node: format!("test#{}", *test.ident).into(),
            id: test.ident.id,
        };
        let unit = Box::new(Type::Primitive(Primitive::Unit));
        let return_type = Type::Verdict(unit.clone(), unit);
        let params = ast::Params(Vec::new());
        self.function_like(&ident, &params, &return_type, &test.body)
    }

    /// Lower a function-like construct (i.e. a function, filtermap or test)
    fn function_like(
        mut self,
        ident: &Meta<Identifier>,
        params: &ast::Params,
        return_type: &Type,
        body: &ast::Block,
    ) -> Function {
        let label = self.label_store.new_label(self.function_name);
        self.new_block(label);

        let mut parameter_types = Vec::new();

        for (x, _) in &params.0 {
            let ty = self.type_info.type_of(x);
            parameter_types.push((self.type_info.resolved_name(x), ty));
        }

        for (name, ty) in &parameter_types {
            let var = Var {
                scope: name.scope,
                kind: VarKind::Explicit(name.ident),
            };
            self.stack_slots.push((var, ty.clone()))
        }

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
            x if self.is_reference_type(x) => (None, true),
            x => (self.lower_type(x), false),
        };

        let ir_signature = ir::Signature {
            parameters: parameter_types
                .iter()
                .filter_map(|(def, ty)| {
                    let ty = self.lower_type(ty)?;
                    Some((def.ident, ty))
                })
                .collect(),
            return_ptr,
            return_type,
        };

        let last = self.block(body);

        self.add(Instruction::Return(last));

        let name = self.type_info.resolved_name(ident);
        let name = self.type_info.full_name(&name);

        Function {
            name,
            scope: self.function_scope,
            entry_block: label,
            blocks: self.blocks,
            public: true,
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
        for stmt in &block.stmts {
            self.stmt(stmt);
        }

        block.last.as_ref().and_then(|last| self.expr(last))
    }

    fn stmt(&mut self, stmt: &ast::Stmt) {
        match stmt {
            ast::Stmt::Let(ident, expr) => {
                let val = self.expr(expr);
                let name = self.type_info.resolved_name(ident);
                let ty = self.type_info.type_of(ident);
                if let Some(ty) = self.lower_type(&ty) {
                    let val = val.unwrap();
                    self.add(Instruction::Assign {
                        to: Var {
                            scope: name.scope,
                            kind: VarKind::Explicit(**ident),
                        },
                        val,
                        ty,
                    })
                }
            }
            ast::Stmt::Expr(expr) => {
                self.expr(expr);
            }
        }
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
            ast::Expr::FunctionCall(e, arg_exprs) => {
                let operand = match &e.node {
                    ast::Expr::Path(p) => {
                        let resolved_path = self.type_info.path_kind(p);
                        match resolved_path {
                            ResolvedPath::Method { value, .. } => {
                                self.path_value(&value.clone())
                            }
                            ResolvedPath::Function { .. }
                            | ResolvedPath::StaticMethod { .. }
                            | ResolvedPath::EnumConstructor { .. } => None,
                            ResolvedPath::Value { .. } => unreachable!(),
                        }
                    }
                    ast::Expr::Access(e, _) => self.expr(e),
                    _ => unreachable!(),
                };

                let mut args = Vec::new();
                if let Some(operand) = operand {
                    args.push(operand);
                }

                for e in &arg_exprs.node {
                    if let Some(e) = self.expr(e) {
                        args.push(e);
                    }
                }

                let func = self.type_info.function(id).clone();
                let name = func.name;

                match &func.definition {
                    FunctionDefinition::Runtime(runtime_func) => self
                        .call_runtime_function(
                            *runtime_func,
                            args,
                            &func.signature.parameter_types,
                            &func.signature.return_type,
                        ),
                    FunctionDefinition::Roto => {
                        let Signature {
                            kind: _,
                            parameter_types: _,
                            return_type,
                        } = func.signature;

                        let to = self
                            .lower_type(&return_type)
                            .map(|ty| (self.new_tmp(), ty));

                        let ctx = Var {
                            scope: self.function_scope,
                            kind: VarKind::Context,
                        };

                        let name = self.type_info.full_name(&name);

                        self.add(Instruction::Call {
                            to: to.clone(),
                            ctx: ctx.into(),
                            func: name,
                            args,
                        });

                        to.map(|(to, _ty)| to.into())
                    }
                }
            }
            ast::Expr::Access(e, field) => {
                let record_ty = self.type_info.type_of(&**e);
                let op = self.expr(e)?;
                let (ty, offset) = self.type_info.offset_of(
                    &record_ty,
                    **field,
                    self.runtime,
                );
                self.read_field(op, offset, &ty)
            }
            ast::Expr::Path(p) => {
                let path_kind = self.type_info.path_kind(p);

                let value = match path_kind.clone() {
                    ResolvedPath::Value(value) => value,
                    ResolvedPath::EnumConstructor { .. } => todo!(),
                    _ => unreachable!("should be rejected by type checker"),
                };

                self.path_value(&value)
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
                        ast::BinOp::Add,
                        _,
                        Type::Primitive(Primitive::String),
                    ) => {
                        let function = self.type_info.function(id);
                        let FunctionDefinition::Runtime(runtime_func_ref) =
                            function.definition.clone()
                        else {
                            panic!()
                        };

                        let size = self.type_info.size_of(
                            &Type::Primitive(Primitive::String),
                            self.runtime,
                        );
                        let alignment = self.type_info.alignment_of(
                            &Type::Primitive(Primitive::String),
                            self.runtime,
                        );
                        let align_shift = alignment.ilog2() as u8;
                        self.add(Instruction::Alloc {
                            to: place.clone(),
                            size,
                            align_shift,
                        });

                        let runtime_func =
                            self.runtime.get_function(runtime_func_ref);

                        let ident = Identifier::from("append");
                        let ir_func = IrFunction {
                            name: ident,
                            ptr: runtime_func.description.pointer(),
                            params: vec![
                                IrType::Pointer,
                                IrType::Pointer,
                                IrType::Pointer,
                            ],
                            ret: None,
                        };

                        self.runtime_functions
                            .insert(runtime_func_ref, ir_func);

                        self.add(Instruction::CallRuntime {
                            to: None,
                            func: runtime_func_ref,
                            args: vec![place.clone().into(), left, right],
                        });
                    }
                    (
                        ast::BinOp::Div,
                        _,
                        Type::Primitive(Primitive::IpAddr),
                    ) => {
                        let function = self.type_info.function(id);
                        let FunctionDefinition::Runtime(runtime_func_ref) =
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

                        let runtime_func =
                            self.runtime.get_function(runtime_func_ref);

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
                            .insert(runtime_func_ref, ir_func);

                        self.add(Instruction::CallRuntime {
                            to: None,
                            func: runtime_func_ref,
                            args: vec![place.clone().into(), left, right],
                        });
                    }
                    (
                        ast::BinOp::Eq,
                        _,
                        Type::Primitive(Primitive::IpAddr),
                    ) => {
                        let ip_addr_type_id = TypeId::of::<IpAddr>();
                        let runtime_func = self.find_runtime_function(
                            runtime::FunctionKind::Method(ip_addr_type_id),
                            "eq",
                        );

                        let out = self
                            .call_runtime_function(
                                runtime_func.get_ref(),
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
                        let runtime_func = self.find_runtime_function(
                            runtime::FunctionKind::Method(ip_addr_type_id),
                            "eq",
                        );

                        let out = self
                            .call_runtime_function(
                                runtime_func.get_ref(),
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
                        let ty = self.lower_type(&ty)?;
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
                    let ty = self.lower_type(&ty)?;
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
                        let ty = self.lower_type(&ty)?;
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
            Literal::String(s) => {
                let size = std::mem::size_of::<IpAddr>() as u32;
                let align = std::mem::align_of::<IpAddr>();
                let align_shift = align.ilog2() as u8;

                let to = self.new_tmp();

                self.add(Instruction::Alloc {
                    to: to.clone(),
                    size,
                    align_shift,
                });

                self.add(Instruction::InitString {
                    to: to.clone(),
                    string: s.clone(),
                    init_func: self.runtime.string_init_function,
                });

                to.into()
            }
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
                    Type::Primitive(Primitive::U64) => {
                        IrValue::U64(*x as u64)
                    }
                    Type::Primitive(Primitive::I8) => IrValue::I8(*x as i8),
                    Type::Primitive(Primitive::I16) => {
                        IrValue::I16(*x as i16)
                    }
                    Type::Primitive(Primitive::I32) => {
                        IrValue::I32(*x as i32)
                    }
                    Type::Primitive(Primitive::I64) => IrValue::I64(*x),
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
    ) -> Option<Operand> {
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

            let ty = self.lower_type(&ty)?;

            self.add(Instruction::Offset {
                to: tmp.clone(),
                from,
                offset,
            });

            self.add(Instruction::Read {
                to: to.clone(),
                from: tmp.into(),
                ty,
            });
        }

        Some(to.into())
    }

    fn path_value(
        &mut self,
        PathValue {
            kind,
            ty,
            name,
            fields,
        }: &PathValue,
    ) -> Option<Operand> {
        let var = match kind {
            ValueKind::Context(offset) => {
                let var = Var {
                    scope: self.function_scope,
                    kind: VarKind::Context,
                };
                self.read_field(var.into(), *offset as u32, ty)?
            }
            ValueKind::Constant => {
                let name = name.ident;
                let var = self.new_tmp();
                let ir_ty = self.lower_type(ty)?;
                self.add(Instruction::LoadConstant {
                    to: var.clone(),
                    name,
                    ty: ir_ty,
                });
                var.into()
            }
            ValueKind::Local => Var {
                scope: name.scope,
                kind: VarKind::Explicit(name.ident),
            }
            .into(),
        };

        let mut var = var;
        let mut record_ty = ty.clone();
        for (field, _) in fields {
            let (ty, offset) =
                self.type_info.offset_of(&record_ty, *field, self.runtime);
            var = self.read_field(var, offset, &ty)?;
            record_ty = ty;
        }
        Some(var)
    }

    fn is_reference_type(&mut self, ty: &Type) -> bool {
        self.type_info.is_reference_type(ty, self.runtime)
    }

    fn lower_type(&mut self, ty: &Type) -> Option<IrType> {
        let ty = self.type_info.resolve(ty);
        if self.type_info.size_of(&ty, self.runtime) == 0 {
            return None;
        }
        Some(match ty {
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
            Type::IntVar(_) => IrType::I32,
            Type::BuiltIn(_, _) => IrType::ExtPointer,
            x if self.is_reference_type(&x) => IrType::Pointer,
            _ => panic!("could not lower: {ty:?}"),
        })
    }

    fn call_runtime_function(
        &mut self,
        runtime_func_ref: RuntimeFunctionRef,
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

        params.extend(
            parameter_types.iter().filter_map(|ty| self.lower_type(ty)),
        );

        let args = std::iter::once(Operand::Place(out_ptr.clone()))
            .chain(args)
            .collect();

        let runtime_func = self.runtime.get_function(runtime_func_ref);

        let ir_func = IrFunction {
            name: (&runtime_func.name).into(),
            ptr: runtime_func.description.pointer(),
            params,
            ret: None,
        };

        self.runtime_functions.insert(runtime_func_ref, ir_func);

        self.add(Instruction::CallRuntime {
            to: None,
            func: runtime_func_ref,
            args,
        });

        if self.is_reference_type(return_type) {
            Some(out_ptr.into())
        } else {
            self.read_field(out_ptr.into(), 0, return_type)
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
