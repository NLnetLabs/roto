mod match_expr;

use std::{any::TypeId, net::IpAddr, sync::Arc};

use inetnum::addr::Prefix;

use super::{
    Block, Function, Instruction, Mir, Place, Projection, Value, Var, VarKind,
};

use crate::{
    ast::{self, Identifier, Literal},
    ice,
    ir_printer::{IrPrinter, Printable},
    label::{LabelRef, LabelStore},
    module::ModuleTree,
    parser::meta::{Meta, MetaId},
    runtime::{self, RuntimeFunction, RuntimeFunctionRef},
    typechecker::{
        self,
        info::TypeInfo,
        scope::{DeclarationKind, ScopeRef, ValueKind},
        scoped_display::TypeDisplay,
        types::{
            EnumVariant, FunctionDefinition, FunctionKind, Signature, Type,
            TypeDefinition,
        },
        PathValue, ResolvedPath,
    },
    Runtime,
};

pub struct Lowerer<'r> {
    function_scope: ScopeRef,
    tmp_idx: usize,
    blocks: Vec<Block>,
    runtime: &'r Runtime,
    type_info: &'r mut TypeInfo,
    label_store: &'r mut LabelStore,
    return_type: Type,

    /// All the stack slots that are allocated in each scope
    ///
    /// The first element contains the variables allocated in the function body,
    /// the last element is our current scope and between are the parent
    /// scopes. At the end of a block we drop all variables in the last element
    /// and then pop that element.
    stack_slots: Vec<Vec<(Var, Type)>>,
    vars: Vec<(Var, Type)>,
}

pub fn lower_to_mir(
    tree: &ModuleTree,
    runtime: &Runtime,
    type_info: &mut TypeInfo,
    label_store: &mut LabelStore,
) -> Mir {
    let mut mir = Lowerer::tree(runtime, type_info, tree, label_store);
    mir.eliminate_dead_code();
    mir
}

impl<'r> Lowerer<'r> {
    fn new(
        runtime: &'r Runtime,
        type_info: &'r mut TypeInfo,
        function_name: &Meta<Identifier>,
        label_store: &'r mut LabelStore,
    ) -> Self {
        let function_scope = type_info.function_scope(function_name);
        let ty = type_info.type_of(function_name);
        let Type::Function(_, return_type) = ty else {
            ice!()
        };
        Self {
            tmp_idx: 0,
            type_info,
            runtime,
            return_type: (*return_type).clone(),
            function_scope,
            blocks: Vec::new(),
            stack_slots: Vec::new(),
            vars: Vec::new(),
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

    /// Create a new unique temporary variable that won't be dropped
    fn undropped_tmp(&mut self) -> Var {
        let var = Var {
            scope: self.function_scope,
            kind: VarKind::Tmp(self.tmp_idx),
        };
        self.tmp_idx += 1;
        var
    }

    /// Create a new temporary variable that will be dropped
    fn tmp(&mut self, ty: Type) -> Var {
        let var = Var {
            scope: self.function_scope,
            kind: VarKind::Tmp(self.tmp_idx),
        };
        self.tmp_idx += 1;
        self.add_live_variable(var.clone(), ty);
        var
    }

    fn assign_to_var(&mut self, value: Value, ty: Type) -> Var {
        if let Value::Move(x) = value {
            return x;
        }
        let to = self.tmp(ty.clone());
        self.do_assign(Place::new(to.clone(), ty.clone()), ty, value);
        to
    }

    /// Lower a syntax tree
    fn tree(
        runtime: &Runtime,
        type_info: &mut TypeInfo,
        tree: &ModuleTree,
        label_store: &mut LabelStore,
    ) -> Mir {
        let mut functions = Vec::new();

        for m in &tree.modules {
            for d in &m.ast.declarations {
                match d {
                    ast::Declaration::FilterMap(x) => {
                        functions.push(
                            Lowerer::new(
                                runtime,
                                type_info,
                                &x.ident,
                                label_store,
                            )
                            .filter_map(x),
                        );
                    }
                    ast::Declaration::Function(x) => {
                        functions.push(
                            Lowerer::new(
                                runtime,
                                type_info,
                                &x.ident,
                                label_store,
                            )
                            .function(x),
                        );
                    }
                    // We give tests special names, so that they can't be referenced from Roto.
                    // It's a bit of a hack, but works well enough.
                    ast::Declaration::Test(x) => {
                        functions.push(
                            Lowerer::new(
                                runtime,
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
        Mir { functions }
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
        let scope = self.type_info.function_scope(ident);
        let label = self.label_store.new_label("$entry".into());
        self.new_block(label);

        let mut parameter_types = Vec::new();

        for (x, _) in &params.0 {
            let ty = self.type_info.type_of(x);
            parameter_types.push((self.type_info.resolved_name(x), ty));
        }

        self.stack_slots.push(Vec::new());

        let mut parameters = Vec::new();
        for (name, ty) in &parameter_types {
            let var = Var {
                scope: name.scope,
                kind: VarKind::Explicit(name.ident),
            };
            parameters.push(var.clone());
            self.add_live_variable(var, ty.clone());
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

        let to_drop = self.stack_slots.pop().unwrap();
        for (var, ty) in to_drop.into_iter().rev() {
            self.emit_drop(Place::new(var, ty.clone()), ty);
        }

        let tmp = self.assign_to_var(last, return_type.clone());
        self.emit_return(tmp);

        let name = self.type_info.resolved_name(ident);
        let name = self.type_info.full_name(&name);

        Function {
            name,
            scope,
            variables: self.vars,
            parameters,
            tmp_idx: self.tmp_idx,
            blocks: self.blocks,
            signature,
        }
    }

    fn block(&mut self, block: &Meta<ast::Block>) -> Value {
        self.stack_slots.push(Vec::new());

        // Resulting operand is ignored
        for stmt in &block.stmts {
            self.stmt(stmt);
        }

        let op = match &block.last {
            Some(expr) => self.expr(expr),
            None => Value::Const(ast::Literal::Unit, Type::unit()),
        };

        let ty = self.type_info.type_of(block);
        let final_var = self.assign_to_var(op.clone(), ty);
        self.remove_live_variable(&final_var);

        let to_drop = self.stack_slots.pop().unwrap();

        // If the block diverges, which happens for instance with an explicit
        // return, then we don't need to drop anything. That will only generate
        // noise in the MIR.
        if !self.type_info.diverges(block) {
            // Drop order is reversed
            for (var, ty) in to_drop.into_iter().rev() {
                self.emit_drop(Place::new(var, ty.clone()), ty);
            }
        }

        Value::Move(final_var)
    }

    fn drop_var(&mut self, var: Var) {
        let ty = self.remove_live_variable(&var);
        self.emit_drop(Place::new(var, ty.clone()), ty);
    }

    fn stmt(&mut self, stmt: &Meta<ast::Stmt>) {
        match &**stmt {
            ast::Stmt::Let(ident, _, expr) => {
                let val = self.expr(expr);
                let name = self.type_info.resolved_name(ident);
                let ty = self.type_info.type_of(ident);

                let to = Var {
                    scope: name.scope,
                    kind: VarKind::Explicit(**ident),
                };

                self.add_live_variable(to.clone(), ty.clone());
                self.do_assign(Place::new(to, ty.clone()), ty, val);
            }
            ast::Stmt::Expr(expr) => {
                let value = self.expr(expr);
                let ty = self.type_info.type_of(expr);
                let value = self.assign_to_var(value, ty);
                self.drop_var(value);
            }
        }
    }

    fn expr(&mut self, expr: &Meta<ast::Expr>) -> Value {
        let id = expr.id;
        match &**expr {
            ast::Expr::Return(return_kind, expr) => {
                self.r#return(return_kind, expr)
            }
            ast::Expr::Literal(literal) => self.literal(literal),
            ast::Expr::Match(r#match) => self.r#match(id, r#match),
            ast::Expr::FunctionCall(function, arguments) => {
                self.function_call(id, function, arguments)
            }
            ast::Expr::Access(expr, field) => self.access(expr, field),
            ast::Expr::Path(path) => self.path(id, path),
            ast::Expr::Record(record) | ast::Expr::TypedRecord(_, record) => {
                self.record(id, record)
            }
            ast::Expr::List(_list) => todo!(),
            ast::Expr::Not(expr) => self.not(expr),
            ast::Expr::Negate(expr) => self.negate(expr),
            ast::Expr::Assign(expr, field) => self.assign(expr, field),
            ast::Expr::BinOp(left, op, right) => self.binop(left, op, right),
            ast::Expr::IfElse(condition, then, r#else) => {
                self.if_else(id, condition, then, r#else)
            }
            ast::Expr::While(condition, block) => {
                self.r#while(condition, block)
            }
        }
    }

    fn r#return(
        &mut self,
        return_kind: &ast::ReturnKind,
        expr: &Option<Box<Meta<ast::Expr>>>,
    ) -> Value {
        let val = match expr {
            Some(expr) => (self.expr(expr), self.type_info.type_of(&**expr)),
            None => {
                (Value::Const(ast::Literal::Unit, Type::unit()), Type::unit())
            }
        };

        match return_kind {
            ast::ReturnKind::Return => self.return_value(val.0),
            ast::ReturnKind::Accept => {
                let ty = self.return_type.clone();
                let val = self.make_enum(ty, "Accept".into(), &[val]);
                self.return_value(val)
            }
            ast::ReturnKind::Reject => {
                let ty = self.return_type.clone();
                let val = self.make_enum(ty, "Reject".into(), &[val]);
                self.return_value(val)
            }
        }
    }

    fn literal(&mut self, literal: &Meta<ast::Literal>) -> Value {
        let ty = self.type_info.type_of(literal);
        Value::Const((**literal).clone(), ty)
    }

    fn return_value(&mut self, val: Value) -> Value {
        let var = self.assign_to_var(val, self.return_type.clone());
        let _ty = self.remove_live_variable(&var);
        for frame in self.stack_slots.clone().iter().rev() {
            for (var, ty) in frame.iter().rev() {
                self.emit_drop(
                    Place::new(var.clone(), ty.clone()),
                    ty.clone(),
                );
            }
        }
        self.emit_return(var);
        Value::Const(ast::Literal::Unit, Type::unit())
    }

    fn function_call(
        &mut self,
        id: MetaId,
        function: &Meta<ast::Expr>,
        arguments: &Meta<Vec<Meta<ast::Expr>>>,
    ) -> Value {
        match &function.node {
            ast::Expr::Path(p) => {
                let resolved_path = self.type_info.path_kind(p).clone();
                match resolved_path {
                    ResolvedPath::Method {
                        value, signature, ..
                    } => {
                        let op = self.path_value(&value.clone());
                        let ty = signature.parameter_types[0].clone();
                        let func = self.type_info.function(id).clone();
                        self.normalized_function_call(
                            &func,
                            Some((op, ty)),
                            arguments,
                        )
                    }
                    ResolvedPath::Function { .. }
                    | ResolvedPath::StaticMethod { .. } => {
                        let func = self.type_info.function(id).clone();
                        self.normalized_function_call(&func, None, arguments)
                    }
                    ResolvedPath::EnumConstructor { ty: _, variant } => {
                        let ty = self.type_info.type_of(id);
                        self.enum_constructor(&ty, variant.name, arguments)
                    }
                    ResolvedPath::Value { .. } => ice!(),
                }
            }
            ast::Expr::Access(e, _) => {
                let expr = self.expr(e);
                let ty = self.type_info.type_of(&**e);
                let func = self.type_info.function(id).clone();
                self.normalized_function_call(
                    &func,
                    Some((expr, ty)),
                    arguments,
                )
            }
            _ => ice!(),
        }
    }

    fn normalized_function_call(
        &mut self,
        func: &typechecker::types::Function,
        receiver: Option<(Value, Type)>,
        arguments: &[Meta<ast::Expr>],
    ) -> Value {
        let name = func.name;

        let mut args = Vec::new();
        if let Some((receiver, ty)) = receiver {
            // This values will be dropped by the callee
            let tmp = self.undropped_tmp();
            self.vars.push((tmp.clone(), ty.clone()));

            self.do_assign(Place::new(tmp.clone(), ty.clone()), ty, receiver);
            args.push(tmp);
        }
        args.extend(arguments.iter().map(|a| {
            let ty = self.type_info.type_of(a);
            let op = self.expr(a);

            // These values will be dropped by the callee
            let tmp = self.undropped_tmp();
            self.vars.push((tmp.clone(), ty.clone()));

            self.do_assign(Place::new(tmp.clone(), ty.clone()), ty, op);
            tmp
        }));

        match func.definition {
            FunctionDefinition::Runtime(func_ref) => {
                Value::CallRuntime { func_ref, args }
            }
            FunctionDefinition::Roto => Value::Call { func: name, args },
        }
    }

    fn enum_constructor(
        &mut self,
        ty: &Type,
        variant: Identifier,
        arguments: &[Meta<ast::Expr>],
    ) -> Value {
        let ty = ty.clone();
        let arguments: Vec<_> = arguments
            .iter()
            .map(|a| (self.expr(a), self.type_info.type_of(a)))
            .collect();
        self.make_enum(ty, variant, &arguments)
    }

    fn make_enum(
        &mut self,
        ty: Type,
        variant: Identifier,
        arguments: &[(Value, Type)],
    ) -> Value {
        let to = self.tmp(ty.clone());

        let Type::Name(name) = self.type_info.resolve(&ty) else {
            ice!()
        };

        let TypeDefinition::Enum(_, variants) =
            self.type_info.resolve_type_name(&name)
        else {
            ice!("Not an enum: {}", ty.display(self.type_info))
        };

        let Some(variant) = variants.iter().find(|v| v.name == variant)
        else {
            ice!()
        };

        self.emit_set_discriminant(to.clone(), ty.clone(), variant.clone());
        for (i, (value, field_ty)) in arguments.iter().enumerate() {
            self.do_assign(
                Place {
                    var: to.clone(),
                    root_ty: ty.clone(),
                    projection: vec![Projection::VariantField(
                        variant.name,
                        i,
                    )],
                },
                field_ty.clone(),
                value.clone(),
            )
        }
        Value::Move(to)
    }

    fn access(
        &mut self,
        expr: &Meta<ast::Expr>,
        field: &Meta<Identifier>,
    ) -> Value {
        let op = self.expr(expr);
        let ty = self.type_info.type_of(expr);
        let var = self.assign_to_var(op, ty.clone());
        Value::Clone(Place {
            var,
            root_ty: ty,
            projection: vec![Projection::Field(**field)],
        })
    }

    fn path(&mut self, id: MetaId, path: &Meta<ast::Path>) -> Value {
        let path_kind = self.type_info.path_kind(path).clone();

        match path_kind {
            ResolvedPath::Value(value) => self.path_value(&value),
            ResolvedPath::EnumConstructor { ty: _, variant } => {
                let ty = self.type_info.type_of(id);
                let to = self.tmp(ty.clone());
                self.emit_set_discriminant(to.clone(), ty, variant);
                Value::Move(to)
            }
            _ => ice!("should be rejected by the type checker"),
        }
    }

    fn record(&mut self, id: MetaId, record: &Meta<ast::Record>) -> Value {
        let ty = self.type_info.type_of(id);

        let to = self.tmp(ty.clone());

        for (s, expr) in &record.fields {
            let op = self.expr(expr);
            let field_ty = self.type_info.type_of(expr);
            self.do_assign(
                Place {
                    var: to.clone(),
                    root_ty: ty.clone(),
                    projection: vec![Projection::Field(**s)],
                },
                field_ty,
                op,
            );
        }

        Value::Move(to)
    }

    fn not(&mut self, expr: &Meta<ast::Expr>) -> Value {
        let val = self.expr(expr);
        let var = self.assign_to_var(val, Type::bool());
        Value::Not(var)
    }

    fn negate(&mut self, expr: &Meta<ast::Expr>) -> Value {
        let ty = self.type_info.type_of(expr);
        let val = self.expr(expr);
        let var = self.assign_to_var(val, ty.clone());
        Value::Negate(var, ty)
    }

    fn assign(
        &mut self,
        path: &Meta<ast::Path>,
        expr: &Meta<ast::Expr>,
    ) -> Value {
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

        let ty = self.type_info.type_of(expr);
        let tmp = self.tmp(ty.clone());

        let to = Var {
            scope: name.scope,
            kind: VarKind::Explicit(name.ident),
        };

        let place = Place {
            var: to,
            root_ty,
            projection: fields
                .iter()
                .map(|(s, _)| Projection::Field(*s))
                .collect(),
        };

        let val = self.expr(expr);
        self.do_assign(Place::new(tmp.clone(), ty.clone()), ty.clone(), val);

        self.emit_drop(place.clone(), ty.clone());

        self.do_assign(place, ty, Value::Move(tmp));

        Value::Const(ast::Literal::Unit, Type::unit())
    }

    fn binop(
        &mut self,
        l: &Meta<ast::Expr>,
        binop: &ast::BinOp,
        r: &Meta<ast::Expr>,
    ) -> Value {
        let l_ty = self.type_info.type_of(l);
        let r_ty = self.type_info.type_of(r);

        if l_ty == Type::string() {
            return self.binop_str(l, binop, r);
        }

        if l_ty == Type::ip_addr() {
            return self.binop_ip_addr(l, binop, r);
        }

        if *binop == ast::BinOp::And {
            return self.binop_and(l, r);
        }

        if *binop == ast::BinOp::Or {
            return self.binop_or(l, r);
        }

        let l = self.expr(l);
        let l = self.assign_to_var(l, l_ty.clone());

        let r = self.expr(r);
        let r = self.assign_to_var(r, r_ty);

        Value::BinOp {
            left: l,
            binop: *binop,
            ty: l_ty.clone(),
            right: r,
        }
    }

    fn binop_str(
        &mut self,
        l: &Meta<ast::Expr>,
        binop: &ast::BinOp,
        r: &Meta<ast::Expr>,
    ) -> Value {
        let type_id = TypeId::of::<Arc<str>>();
        match binop {
            ast::BinOp::Eq => self.desugared_binop(
                runtime::FunctionKind::Method(type_id),
                "eq",
                Type::bool(),
                (l, Type::string()),
                (r, Type::string()),
            ),
            ast::BinOp::Ne => {
                let val = self.desugared_binop(
                    runtime::FunctionKind::Method(type_id),
                    "eq",
                    Type::bool(),
                    (l, Type::string()),
                    (r, Type::string()),
                );
                let var = self.assign_to_var(val, Type::bool());
                Value::Not(var)
            }
            ast::BinOp::Add => self.desugared_binop(
                runtime::FunctionKind::Method(type_id),
                "append",
                Type::string(),
                (l, Type::string()),
                (r, Type::string()),
            ),
            _ => {
                ice!("Operator {binop} is not implemented for String")
            }
        }
    }

    fn binop_ip_addr(
        &mut self,
        l: &Meta<ast::Expr>,
        binop: &ast::BinOp,
        r: &Meta<ast::Expr>,
    ) -> Value {
        let type_id = TypeId::of::<IpAddr>();
        match binop {
            ast::BinOp::Eq => self.desugared_binop(
                runtime::FunctionKind::Method(type_id),
                "eq",
                Type::bool(),
                (l, Type::ip_addr()),
                (r, Type::ip_addr()),
            ),
            ast::BinOp::Ne => {
                let val = self.desugared_binop(
                    runtime::FunctionKind::Method(type_id),
                    "eq",
                    Type::bool(),
                    (l, Type::ip_addr()),
                    (r, Type::ip_addr()),
                );
                let var = self.assign_to_var(val, Type::bool());
                Value::Not(var)
            }
            ast::BinOp::Div => {
                let type_id = TypeId::of::<Prefix>();
                self.desugared_binop(
                    runtime::FunctionKind::StaticMethod(type_id),
                    "new",
                    Type::prefix(),
                    (l, Type::ip_addr()),
                    (r, Type::u8()),
                )
            }
            _ => {
                ice!("Operator {binop} is not implemented for IpAddr")
            }
        }
    }

    fn binop_and(
        &mut self,
        l: &Meta<ast::Expr>,
        r: &Meta<ast::Expr>,
    ) -> Value {
        let current_label = self.current_label();
        let lbl_cont = self.label_store.next(current_label);
        let lbl_other = self
            .label_store
            .wrap_internal(current_label, Identifier::from("and_other"));

        let val = self.expr(l);
        let tmp = self.assign_to_var(val, Type::bool());
        self.emit_switch(tmp.clone(), vec![(1, lbl_other)], Some(lbl_cont));

        self.new_block(lbl_other);
        let val = self.expr(r);
        self.do_assign(
            Place::new(tmp.clone(), Type::bool()),
            Type::bool(),
            val,
        );
        self.emit_jump(lbl_cont);

        self.new_block(lbl_cont);
        Value::Move(tmp)
    }

    fn binop_or(
        &mut self,
        l: &Meta<ast::Expr>,
        r: &Meta<ast::Expr>,
    ) -> Value {
        let current_label = self.current_label();
        let lbl_cont = self.label_store.next(current_label);
        let lbl_other = self
            .label_store
            .wrap_internal(current_label, Identifier::from("and_other"));

        let val = self.expr(l);
        let tmp = self.assign_to_var(val, Type::bool());
        self.emit_switch(tmp.clone(), vec![(0, lbl_other)], Some(lbl_cont));

        self.new_block(lbl_other);
        let val = self.expr(r);
        self.do_assign(
            Place::new(tmp.clone(), Type::bool()),
            Type::bool(),
            val,
        );
        self.emit_jump(lbl_cont);

        self.new_block(lbl_cont);
        Value::Move(tmp)
    }

    fn desugared_binop(
        &mut self,
        kind: runtime::FunctionKind,
        name: &str,
        return_type: Type,
        (l, l_ty): (&Meta<ast::Expr>, Type),
        (r, r_ty): (&Meta<ast::Expr>, Type),
    ) -> Value {
        let func = self.find_runtime_function(kind, name);
        let func_ref = func.get_ref();

        let l = self.expr(l);
        let l = self.assign_to_var(l, l_ty);
        let r = self.expr(r);
        let r = self.assign_to_var(r, r_ty);

        let tmp = self.tmp(return_type.clone());
        let val = self.call_runtime(func_ref, vec![l, r]);
        self.do_assign(
            Place::new(tmp.clone(), return_type.clone()),
            return_type,
            val,
        );

        Value::Move(tmp)
    }

    fn call_runtime(
        &mut self,
        func_ref: RuntimeFunctionRef,
        args: Vec<Var>,
    ) -> Value {
        for var in &args {
            self.remove_live_variable(var);
        }
        Value::CallRuntime { func_ref, args }
    }

    fn if_else(
        &mut self,
        id: MetaId,
        condition: &Meta<ast::Expr>,
        then: &Meta<ast::Block>,
        r#else: &Option<Meta<ast::Block>>,
    ) -> Value {
        let examinee = self.expr(condition);
        let examinee = self.assign_to_var(examinee, Type::bool());

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
            examinee,
            branches,
            Some(if r#else.is_some() { lbl_else } else { lbl_cont }),
        );

        self.new_block(lbl_then);
        let op = self.block(then);

        let ty = self.type_info.type_of(id);

        let res = self.undropped_tmp();
        self.emit_assign(Place::new(res.clone(), ty.clone()), ty.clone(), op);

        self.emit_jump(lbl_cont);

        if let Some(r#else) = r#else {
            self.new_block(lbl_else);
            let op = self.block(r#else);
            self.emit_assign(
                Place::new(res.clone(), ty.clone()),
                ty.clone(),
                op,
            );
            self.emit_jump(lbl_cont);
        }
        self.new_block(lbl_cont);
        self.add_live_variable(res.clone(), ty);
        Value::Move(res)
    }

    fn r#while(
        &mut self,
        condition: &Meta<ast::Expr>,
        block: &Meta<ast::Block>,
    ) -> Value {
        let current_label = self.current_label();
        let lbl_cont = self.label_store.next(current_label);
        let lbl_condition = self.label_store.wrap_internal(
            current_label,
            Identifier::from("while-condition"),
        );
        let lbl_body = self
            .label_store
            .wrap_internal(current_label, Identifier::from("while-body"));

        self.emit_jump(lbl_condition);

        self.new_block(lbl_condition);

        let examinee = self.expr(condition);
        let examinee = self.assign_to_var(examinee, Type::bool());

        self.emit_switch(examinee, vec![(1, lbl_body)], Some(lbl_cont));

        self.new_block(lbl_body);
        let val = self.block(block);
        let _ = self.assign_to_var(val, Type::unit());
        self.emit_jump(lbl_condition);

        self.new_block(lbl_cont);
        Value::Const(Literal::Unit, Type::unit())
    }

    fn path_value(&mut self, path_value: &PathValue) -> Value {
        let PathValue {
            name,
            kind,
            root_ty,
            fields,
        } = path_value;

        match kind {
            ValueKind::Local => {
                let var = Var {
                    scope: name.scope,
                    kind: VarKind::Explicit(name.ident),
                };

                let projection =
                    fields.iter().map(|f| Projection::Field(f.0)).collect();

                Value::Clone(Place {
                    var,
                    root_ty: root_ty.clone(),
                    projection,
                })
            }
            ValueKind::Constant => {
                if !fields.is_empty() {
                    panic!("Getting fields of constants not supported yet")
                }
                Value::Constant(name.ident, root_ty.clone())
            }
            ValueKind::Context(x) => Value::Context(*x),
        }
    }

    fn add_live_variable(&mut self, var: Var, ty: Type) {
        self.vars.push((var.clone(), ty.clone()));
        self.stack_slots.last_mut().unwrap().push((var, ty));
    }

    fn remove_live_variable(&mut self, var: &Var) -> Type {
        let slot = self.stack_slots.last_mut().unwrap();
        if let Some(i) = slot.iter().position(|v| &v.0 == var) {
            slot.remove(i).1
        } else {
            let printer = IrPrinter {
                type_info: self.type_info,
                label_store: self.label_store,
                scope: None,
            };
            let var = var.print(&printer);
            ice!("Variable wasn't live: {var:?}")
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

    fn do_assign(&mut self, to: Place, ty: Type, val: Value) {
        if let Value::Move(var) = &val {
            self.remove_live_variable(var);
        }
        self.emit_assign(to, ty, val);
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

    fn emit_assign(&mut self, to: Place, ty: Type, value: Value) {
        self.emit(Instruction::Assign { to, ty, value });
    }

    fn emit_set_discriminant(
        &mut self,
        to: Var,
        ty: Type,
        variant: EnumVariant,
    ) {
        self.emit(Instruction::SetDiscriminant { to, ty, variant })
    }

    fn emit_return(&mut self, value: Var) {
        self.emit(Instruction::Return { var: value });
    }

    fn emit_jump(&mut self, label_ref: LabelRef) {
        self.emit(Instruction::Jump(label_ref))
    }

    fn emit_switch(
        &mut self,
        examinee: Var,
        branches: Vec<(usize, LabelRef)>,
        default: Option<LabelRef>,
    ) {
        self.emit(Instruction::Switch {
            examinee,
            branches,
            default,
        })
    }

    fn emit_drop(&mut self, val: Place, ty: Type) {
        self.emit(Instruction::Drop { val, ty })
    }
}
