mod match_expr;

use super::ir::{
    Block, Field, Function, Hir, Instruction, Operand, Var, VarKind,
};

use crate::{
    ast::{self, BinOp, Identifier},
    ice,
    label::{LabelRef, LabelStore},
    module::ModuleTree,
    parser::meta::{Meta, MetaId},
    typechecker::{
        info::TypeInfo,
        scope::{DeclarationKind, ResolvedName, ScopeRef},
        types::{FunctionKind, Signature, Type},
        PathValue, ResolvedPath,
    },
};

pub struct Lowerer<'r> {
    function_scope: ScopeRef,
    tmp_idx: usize,
    blocks: Vec<Block>,
    type_info: &'r mut TypeInfo,
    label_store: &'r mut LabelStore,

    /// All the stack slots that are allocated in each scope
    ///
    /// The first element contains the variables allocated in the function body,
    /// the last element is our current scope and between are the parent
    /// scopes. At the end of a block we drop all variables in the last element
    /// and then pop that element.
    stack_slots: Vec<Vec<Var>>,
}

pub fn lower_to_hir(
    tree: &ModuleTree,
    type_info: &mut TypeInfo,
    label_store: &mut LabelStore,
) -> Hir {
    Lowerer::tree(type_info, tree, label_store)
}

impl<'r> Lowerer<'r> {
    fn new(
        type_info: &'r mut TypeInfo,
        function_name: &Meta<Identifier>,
        label_store: &'r mut LabelStore,
    ) -> Self {
        let function_scope = type_info.function_scope(function_name);
        Self {
            tmp_idx: 0,
            type_info,
            function_scope,
            blocks: Vec::new(),
            stack_slots: Vec::new(),
            label_store,
        }
    }

    /// Add a new block to the blocks in the program.
    fn new_block(&mut self, label: LabelRef) {
        self.blocks.push(Block {
            label,
            instructions: Vec::new(),
        })
    }

    /// Get the label of the block we are currently building
    fn current_label(&self) -> LabelRef {
        self.blocks.last().unwrap().label
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
        tree: &ModuleTree,
        label_store: &mut LabelStore,
    ) -> Hir {
        let mut functions = Vec::new();

        for m in &tree.modules {
            for d in &m.ast.declarations {
                match d {
                    ast::Declaration::FilterMap(x) => {
                        functions.push(
                            Lowerer::new(type_info, &x.ident, label_store)
                                .filter_map(x),
                        );
                    }
                    ast::Declaration::Function(x) => {
                        functions.push(
                            Lowerer::new(type_info, &x.ident, label_store)
                                .function(x),
                        );
                    }
                    // We give tests special names, so that they can't be referenced from Roto.
                    // It's a bit of a hack, but works well enough.
                    ast::Declaration::Test(x) => {
                        functions.push(
                            Lowerer::new(
                                type_info,
                                &Meta {
                                    node: format!("test#{}", x.ident).into(),
                                    id: x.ident.id,
                                },
                                label_store,
                            )
                            .test(x),
                        );
                    }
                    // Ignore the rest
                    _ => {}
                }
            }
        }
        Hir { functions }
    }

    /// Lower a filtermap
    fn filter_map(self, fm: &ast::FilterMap) -> Function {
        let ast::FilterMap {
            ident,
            body,
            params,
            ..
        } = fm;

        let Type::Function(_, ret) = self.type_info.type_of(ident) else {
            ice!("The type of a filter(map) must be a function");
        };
        self.function_like(ident, params, &ret, body)
    }

    fn function(self, function: &ast::FunctionDeclaration) -> Function {
        let name = self.type_info.resolved_name(&function.ident);
        let dec = self.type_info.scope_graph.get_declaration(name);

        let DeclarationKind::Function(_, ty) = dec.kind else {
            ice!();
        };

        let Type::Function(_, ret) = self.type_info.resolve(&ty) else {
            ice!("A function must have a function type");
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
        let return_type = Type::verdict(Type::unit(), Type::unit());
        let params = ast::Params(Vec::new());
        self.function_like(&ident, &params, &return_type, &test.body)
    }

    /// Lower a function-like construct (i.e. a function, filtermap or test)
    fn function_like(
        mut self,
        ident: &Meta<Identifier>,
        params: &ast::Params,
        return_type: &Type,
        body: &Meta<ast::Block>,
    ) -> Function {
        let label = self.label_store.new_label(**ident);
        self.new_block(label);

        let mut parameter_types = Vec::new();

        for (x, _) in &params.0 {
            let ty = self.type_info.type_of(x);
            parameter_types.push((self.type_info.resolved_name(x), ty));
        }

        self.stack_slots.push(Vec::new());

        let mut parameters = Vec::new();
        for (name, _ty) in &parameter_types {
            let var = Var {
                scope: name.scope,
                kind: VarKind::Explicit(name.ident),
            };
            parameters.push(var.clone());
            self.stack_slots.last_mut().unwrap().push(var);
        }

        let signature = Signature {
            kind: FunctionKind::Free,
            parameter_types: parameter_types
                .iter()
                .map(|x| &x.1)
                .cloned()
                .collect(),
            return_type: return_type.clone(),
        };

        let last = self.block(body);

        if !self.type_info.diverges(body) {
            self.emit_return(last);
        }

        let name = self.type_info.resolved_name(ident);
        let name = self.type_info.full_name(&name);

        Function {
            name,
            parameters,
            blocks: self.blocks,
            signature,
        }
    }

    fn block(&mut self, block: &Meta<ast::Block>) -> Operand {
        self.stack_slots.push(Vec::new());

        // Resulting operand is ignored
        for stmt in &block.stmts {
            self.stmt(stmt);
        }

        let op = match &block.last {
            Some(expr) => self.expr(expr),
            None => Operand::Value(ast::Literal::Unit),
        };

        // If the block diverges, which happens for instance with an explicit
        // return, then we don't need to drop anything. That will only generate
        // noise in the HIR.
        if self.type_info.diverges(block) {
            return op;
        }

        // Drop order is reversed
        let to_drop = self.stack_slots.pop().unwrap();
        for var in to_drop.into_iter().rev() {
            // Don't drop the final value of this block
            if let Operand::Place(final_var) = &op {
                if final_var == &var {
                    continue;
                }
            }
            self.emit_drop(var.into());
        }

        op
    }

    fn stmt(&mut self, stmt: &Meta<ast::Stmt>) {
        match &**stmt {
            ast::Stmt::Let(ident, expr) => {
                let val = self.expr(expr);
                let name = self.type_info.resolved_name(ident);
                let ty = self.type_info.type_of(ident);

                let to = Var {
                    scope: name.scope,
                    kind: VarKind::Explicit(**ident),
                };

                self.emit_assign(to, ty, Vec::new(), val);
            }
            ast::Stmt::Expr(expr) => {
                self.expr(expr);
            }
        }
    }

    fn expr(&mut self, expr: &Meta<ast::Expr>) -> Operand {
        let id = expr.id;
        match &**expr {
            ast::Expr::Return(return_kind, expr) => {
                self.r#return(id, return_kind, expr)
            }
            ast::Expr::Literal(literal) => self.literal(literal),
            ast::Expr::Match(r#match) => self.r#match(r#match),
            ast::Expr::FunctionCall(function, arguments) => {
                self.function_call(id, function, arguments)
            }
            ast::Expr::Access(expr, field) => self.access(expr, field),
            ast::Expr::Path(path) => self.path(path),
            ast::Expr::Record(record) | ast::Expr::TypedRecord(_, record) => {
                self.record(id, record)
            }
            ast::Expr::List(_list) => todo!(),
            ast::Expr::Not(expr) => self.not(expr),
            ast::Expr::Assign(expr, field) => self.assign(expr, field),
            ast::Expr::BinOp(left, op, right) => self.binop(left, op, right),
            ast::Expr::IfElse(condition, then, r#else) => {
                self.if_else(id, condition, then, r#else)
            }
        }
    }

    fn r#return(
        &mut self,
        id: MetaId,
        return_kind: &ast::ReturnKind,
        expr: &Option<Box<Meta<ast::Expr>>>,
    ) -> Operand {
        let e = match expr {
            Some(expr) => self.expr(expr),
            None => Operand::Value(ast::Literal::Unit),
        };

        let ty = self.type_info.return_type_of(id);

        match return_kind {
            ast::ReturnKind::Return => self.emit_return(e),
            ast::ReturnKind::Accept => {
                let to = self.new_tmp();
                self.emit_make_enum(to.clone(), ty, "Accept".into(), vec![e]);
                self.emit_return(to.into());
            }
            ast::ReturnKind::Reject => {
                let to = self.new_tmp();
                self.emit_make_enum(to.clone(), ty, "Reject".into(), vec![e]);
                self.emit_return(to.into());
            }
        }

        Operand::Value(ast::Literal::Unit)
    }

    fn literal(&mut self, literal: &Meta<ast::Literal>) -> Operand {
        Operand::Value((&**literal).clone())
    }

    fn function_call(
        &mut self,
        id: MetaId,
        function: &Meta<ast::Expr>,
        arguments: &Meta<Vec<Meta<ast::Expr>>>,
    ) -> Operand {
        match &function.node {
            ast::Expr::Path(p) => {
                let resolved_path = self.type_info.path_kind(p);
                match resolved_path {
                    ResolvedPath::Method { value, .. } => {
                        let op = self.path_value(&value.clone());
                        self.normalized_function_call(id, Some(op), arguments)
                    }
                    ResolvedPath::Function { .. }
                    | ResolvedPath::StaticMethod { .. } => {
                        self.normalized_function_call(id, None, arguments)
                    }
                    ResolvedPath::EnumConstructor { ty, variant } => {
                        let ty = ty.clone().type_name();
                        let variant = variant.clone();
                        let to = self.new_tmp();
                        let arguments =
                            arguments.iter().map(|a| self.expr(a)).collect();
                        self.emit_make_enum(
                            to.clone(),
                            Type::Name(ty),
                            variant.name,
                            arguments,
                        );
                        to.into()
                    }
                    ResolvedPath::Value { .. } => ice!(),
                }
            }
            ast::Expr::Access(e, _) => {
                let expr = self.expr(e);
                self.normalized_function_call(id, Some(expr), arguments)
            }
            _ => ice!(),
        }
    }

    fn normalized_function_call(
        &mut self,
        id: MetaId,
        receiver: Option<Operand>,
        arguments: &[Meta<ast::Expr>],
    ) -> Operand {
        let func = self.type_info.function(id).clone();
        let name = func.name;

        let mut args = Vec::new();
        if let Some(receiver) = receiver {
            args.push(receiver);
        }
        args.extend(arguments.iter().map(|a| self.expr(a)));

        let to = self.new_tmp();
        self.emit_function_call(to.clone(), name, args);
        to.into()
    }

    fn access(
        &mut self,
        expr: &Meta<ast::Expr>,
        field: &Meta<Identifier>,
    ) -> Operand {
        let op = self.expr(expr);
        let to = self.new_tmp();
        self.emit_read_field(
            to.clone(),
            op,
            Field::Named((&**field).clone()),
        );
        to.into()
    }

    fn path(&mut self, path: &Meta<ast::Path>) -> Operand {
        let path_kind = self.type_info.path_kind(path).clone();

        match path_kind {
            ResolvedPath::Value(value) => self.path_value(&value),
            ResolvedPath::EnumConstructor { ty, variant } => {
                let to = self.new_tmp();
                self.emit_make_enum(
                    to.clone(),
                    Type::Name(ty.type_name()),
                    variant.name,
                    Vec::new(),
                );
                to.into()
            }
            _ => ice!("should be rejected by the type checker"),
        }
    }

    fn record(&mut self, id: MetaId, record: &Meta<ast::Record>) -> Operand {
        let ty = self.type_info.type_of(id);

        let fields = record
            .fields
            .iter()
            .map(|(s, expr)| (**s, self.expr(expr)))
            .collect();

        let to = self.new_tmp();
        self.emit_make_record(to.clone(), ty, fields);
        to.into()
    }

    fn not(&mut self, expr: &Meta<ast::Expr>) -> Operand {
        let op = self.expr(expr);
        let to = self.new_tmp();
        self.emit_not(to.clone(), op);
        to.into()
    }

    fn assign(
        &mut self,
        path: &Meta<ast::Path>,
        expr: &Meta<ast::Expr>,
    ) -> Operand {
        let resolved_path = self.type_info.path_kind(path);
        let ResolvedPath::Value(PathValue {
            name,
            kind: _,
            root_ty,
            fields,
        }) = resolved_path.clone()
        else {
            ice!("should be rejected by type checker");
        };

        let to = Var {
            scope: name.scope,
            kind: VarKind::Explicit(name.ident),
        };
        let op = self.expr(expr);
        self.emit_assign(to, root_ty.clone(), fields.clone(), op);

        Operand::Value(ast::Literal::Unit)
    }

    fn binop(
        &mut self,
        left: &Meta<ast::Expr>,
        op: &ast::BinOp,
        right: &Meta<ast::Expr>,
    ) -> Operand {
        let left = self.expr(left);
        let right = self.expr(right);
        let to = self.new_tmp();
        self.emit_binop(to.clone(), left, op.clone(), right);
        to.into()
    }

    fn if_else(
        &mut self,
        id: MetaId,
        condition: &Meta<ast::Expr>,
        then: &Meta<ast::Block>,
        r#else: &Option<Meta<ast::Block>>,
    ) -> Operand {
        let place = self.expr(condition);
        let res = self.new_tmp();

        let diverges = self.type_info.diverges(id);

        let current_label = self.current_label();
        let lbl_cont = self.label_store.next(current_label);
        let lbl_then = self
            .label_store
            .wrap_internal(current_label, Identifier::from("if-then"));
        let lbl_else = self
            .label_store
            .wrap_internal(current_label, Identifier::from("if-else"));

        let branches = vec![(1, lbl_then)];

        self.emit_switch(
            place,
            branches,
            if r#else.is_some() { lbl_else } else { lbl_cont },
        );

        self.new_block(lbl_then);
        let op = self.block(then);
        let ty = self.type_info.type_of(then);
        self.emit_assign(res.clone(), ty, Vec::new(), op);

        if !diverges {
            self.emit_jump(lbl_cont);
        }

        if let Some(r#else) = r#else {
            self.new_block(lbl_else);
            let op = self.block(r#else);
            let ty = self.type_info.type_of(r#else);
            self.emit_assign(res.clone(), ty, Vec::new(), op);

            if !diverges {
                self.emit_jump(lbl_cont);
                self.new_block(lbl_cont);
            }
            res.into()
        } else {
            if !diverges {
                self.new_block(lbl_cont);
            }
            Operand::Value(ast::Literal::Unit)
        }
    }

    fn path_value(&mut self, path_value: &PathValue) -> Operand {
        let name = path_value.name;

        let var = Var {
            scope: name.scope,
            kind: VarKind::Explicit(name.ident),
        };

        Operand::Place(var)
    }
}

impl Lowerer<'_> {
    fn emit(&mut self, instruction: Instruction) {
        self.blocks
            .last_mut()
            .unwrap()
            .instructions
            .push(instruction)
    }

    fn emit_assign(
        &mut self,
        to: Var,
        root_ty: Type,
        fields: Vec<(Identifier, Type)>,
        val: Operand,
    ) {
        self.emit(Instruction::Assign {
            to,
            root_ty,
            fields,
            val,
        });
    }

    fn emit_return(&mut self, operand: Operand) {
        self.emit(Instruction::Return(operand));
    }

    fn emit_make_enum(
        &mut self,
        to: Var,
        ty: Type,
        variant: Identifier,
        fields: Vec<Operand>,
    ) {
        self.emit(Instruction::MakeEnum {
            to,
            ty,
            variant,
            fields,
        })
    }

    fn emit_make_record(
        &mut self,
        to: Var,
        ty: Type,
        fields: Vec<(Identifier, Operand)>,
    ) {
        self.emit(Instruction::MakeRecord { to, ty, fields })
    }

    fn emit_function_call(
        &mut self,
        to: Var,
        name: ResolvedName,
        arguments: Vec<Operand>,
    ) {
        self.emit(Instruction::Call {
            to,
            func: name,
            args: arguments,
        })
    }

    fn emit_binop(
        &mut self,
        to: Var,
        left: Operand,
        op: BinOp,
        right: Operand,
    ) {
        self.emit(Instruction::BinOp {
            to,
            left,
            op,
            right,
        })
    }

    fn emit_not(&mut self, to: Var, op: Operand) {
        self.emit(Instruction::Not { to, op })
    }

    fn emit_jump(&mut self, label_ref: LabelRef) {
        self.emit(Instruction::Jump(label_ref))
    }

    fn emit_switch(
        &mut self,
        examinee: Operand,
        branches: Vec<(usize, LabelRef)>,
        default: LabelRef,
    ) {
        self.emit(Instruction::Switch {
            examinee,
            branches,
            default,
        })
    }

    fn emit_read_field(&mut self, to: Var, from: Operand, field: Field) {
        self.emit(Instruction::ReadField { to, from, field })
    }

    fn emit_drop(&mut self, var: Operand) {
        self.emit(Instruction::Drop { value: var })
    }
}
