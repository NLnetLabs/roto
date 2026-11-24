//! Wrapping of REPL input
//!
//! Each input is parsed on its own and wrapped in a function built from
//! AST nodes. Names of the wrapper and its helpers start with `$`,
//! so they can't be typed by the user.

use std::cell::Cell;

use crate::{
    ast::{
        Block, ConstantDeclaration, Declaration, Expr, FStringPart,
        FunctionDeclaration, Identifier, Params, Path, ReplStmt, Stmt,
        TypeExpr,
    },
    codegen::RotoConstant,
    ice, library,
    load::SourceFile,
    location,
    module::Parsed,
    parser::{
        Parser,
        meta::{Meta, MetaId, Span, Spans},
    },
    runtime::{func::OutPtr, items::Module},
    value::{DynVal, VTable},
};

/// The module of the session's code
pub(super) const SESSION_MODULE: &str = "pkg";

/// Runtime module that holds the REPL's helper functions. `$` can't
/// start an identifier, so users can't name it or reach anything in it.
const HELPERS: &str = "$repl";

/// Helper that moves a value into a new cell: `store` in [`helpers`]
const STORE: &str = "store";

/// Helper that a cell's constant is initialized with: `cell` in
/// [`helpers`]
const CELL: &str = "cell";

/// The wrapper of an expression
pub(super) const MAIN: &str = "$repl_main";

/// The wrapper of a `let`
pub(super) const INIT: &str = "$repl_init";

/// The generated session text: imports, declarations, one constant per
/// value
pub(super) fn session_file(contents: String) -> SourceFile {
    SourceFile {
        name: "<session>".into(),
        module_name: SESSION_MODULE.into(),
        contents,
        location_offset: 0,
        children: Vec::new(),
    }
}

/// The user's line, exactly as typed, so that errors in it point at it
///
/// Its name must differ from the session file's: reports look files up
/// by name.
pub(super) fn input_file(line: &str) -> SourceFile {
    SourceFile {
        name: "<input>".into(),
        module_name: SESSION_MODULE.into(),
        contents: line.into(),
        location_offset: 0,
        children: Vec::new(),
    }
}

thread_local! {
    /// The cell that the store helper filled, until `add_let` takes it
    static STORED: Cell<Option<RotoConstant>> = const { Cell::new(None) };
}

/// Take the cell that the store helper filled, if any
pub(super) fn take_stored() -> Option<RotoConstant> {
    STORED.take()
}

unsafe extern "C" fn no_drop(_: *mut ()) {}

/// The runtime module with the helpers that the wrappers call
///
/// Built as a struct, not with `Module::new`, which would reject
/// the name. The name not being an identifier is what keeps the helpers
/// out of reach. This relies on `Runtime::add` not checking module names
/// again. The function names must match [`STORE`] and [`CELL`].
pub(super) fn helpers() -> Module {
    let lib = library! {
        /// Move a value into a new cell, see `Session::add_let`
        #[sig = "fn[T](T)"]
        #[vtables(T)]
        fn store(vtable: VTable, v: DynVal) {
            let cell = RotoConstant::new(
                vtable.size(),
                vtable.align(),
                vtable.drop_fn.unwrap_or(no_drop),
            );
            // SAFETY: the cell is allocated with the layout of `T`, and
            // `v` points to a `T`. A callee owns its parameters, so
            // copying the bytes moves the value, and `v` must not be
            // dropped.
            unsafe {
                std::ptr::copy_nonoverlapping(
                    v.0 as *const u8,
                    cell.ptr() as *mut u8,
                    vtable.size(),
                );
            }
            STORED.set(Some(cell));
        }

        /// The initializer of the constant of a cell. Never called,
        /// because codegen takes the address of the cell instead.
        #[sig = "fn[T]() -> T"]
        fn cell(_out: OutPtr<DynVal>) {
            ice!("the cell helper is never called")
        }
    };
    Module {
        ident: HELPERS.into(),
        doc: String::new(),
        children: lib.items,
        location: location!(),
    }
}

/// Builds wrapper nodes. Every node gets an ID from the session's
/// `Spans` and the same `span`, following the parser's rules for shared
/// IDs: `Expr::Path` shares the ID of its `Path` and `Stmt::Expr` the ID
/// of its expression.
struct NodeBuilder<'s> {
    spans: &'s mut Spans,
    span: Span,
}

impl<'s> NodeBuilder<'s> {
    fn new(spans: &'s mut Spans, span: Span) -> Self {
        Self { spans, span }
    }

    /// A node with a fresh ID
    fn node<T>(&mut self, node: T) -> Meta<T> {
        self.spans.add(self.span, node)
    }

    /// `a.b.c`: a fresh ID for each identifier and one for the path
    fn path_node(
        &mut self,
        names: impl IntoIterator<Item = impl Into<Identifier>>,
    ) -> Meta<Path> {
        let idents = names.into_iter().map(|n| self.node(n.into())).collect();
        self.node(Path { idents })
    }

    /// A path as an expression; shares the ID of the path
    fn path(
        &mut self,
        names: impl IntoIterator<Item = impl Into<Identifier>>,
    ) -> Meta<Expr> {
        let path = self.path_node(names);
        Meta {
            id: path.id,
            node: Expr::Path(path),
        }
    }

    /// `a.b(args)`: fresh IDs for the argument list and the call
    fn call(
        &mut self,
        callee: impl IntoIterator<Item = impl Into<Identifier>>,
        args: Vec<Meta<Expr>>,
    ) -> Meta<Expr> {
        let callee = Box::new(self.path(callee));
        let args = self.node(args);
        self.node(Expr::FunctionCall(callee, args))
    }

    /// `f"{expr}"`: the part and the f-string each get a fresh ID
    fn fstring(&mut self, expr: Meta<Expr>) -> Meta<Expr> {
        let part = self.node(FStringPart::Expr(expr));
        self.node(Expr::FString(vec![part]))
    }

    /// A type written as a path, such as `std.String`; does not share
    /// the ID of its path
    fn type_path(
        &mut self,
        names: impl IntoIterator<Item = impl Into<Identifier>>,
    ) -> Meta<TypeExpr> {
        let path = self.path_node(names);
        self.node(TypeExpr::Path(path, None))
    }

    /// `expr;`: shares the ID of `expr`
    fn expr_stmt(&mut self, expr: Meta<Expr>) -> Meta<Stmt> {
        Meta {
            id: expr.id,
            node: Stmt::Expr(expr),
        }
    }

    /// `const x: T = e`, typed at the prompt, as `let x: T = e`
    fn let_from_const(&mut self, c: ConstantDeclaration) -> Meta<Stmt> {
        self.node(Stmt::Let(c.ident, Some(c.ty), c.expr))
    }

    /// `fn name() [-> ret] { stmts [last] }`
    fn function(
        &mut self,
        name: impl Into<Identifier>,
        ret: Option<Meta<TypeExpr>>,
        stmts: Vec<Meta<Stmt>>,
        last: Option<Meta<Expr>>,
    ) -> FunctionDeclaration {
        FunctionDeclaration {
            ident: self.node(name.into()),
            params: self.node(Params(Vec::new())),
            ret,
            body: self.node(Block {
                stmts,
                last: last.map(Box::new),
            }),
        }
    }
}

/// Make every session constant read its cell: replace the placeholder
/// initializer `()` with a call to the cell helper
pub(super) fn patch_cells(parsed: &mut Parsed) {
    for decl in &mut parsed.module_tree.modules[0].ast.declarations {
        let Declaration::Const(c) = decl else {
            continue;
        };
        let span = parsed.spans.get(&c.expr);
        c.expr = NodeBuilder::new(&mut parsed.spans, span)
            .call([HELPERS, CELL], Vec::new());
    }
}

/// What to build around the user's input
#[derive(Clone, Copy, Debug)]
pub(super) enum Wrapper {
    /// `fn $repl_main() { <expr>; }`
    Main,
    /// `fn $repl_main() -> std.String { f"{<expr>}" }`
    Print,
    /// `fn $repl_init() { <let>; $repl.store(<name>); }`
    Init,
}

/// IDs inside the wrapper that callers read types from
#[derive(Clone, Copy, Debug)]
pub(super) struct Wrapped {
    /// The input expression, or the initializer of the `let`
    pub(super) expr: MetaId,
    /// The body of the wrapper function
    pub(super) body: MetaId,
}

/// Parse `line` into `parsed` as the file `<input>`, wrap it as
/// `wrapper` and make the wrapper the first declaration of the session
pub(super) fn wrap_input(
    parsed: &mut Parsed,
    line: &str,
    wrapper: Wrapper,
) -> Wrapped {
    let file = parsed.files.len();
    parsed.files.push(input_file(line));
    let stmt = Parser::parse_repl_stmt(file, &mut parsed.spans, line)
        .unwrap_or_else(|e| ice!("the input parsed before: {e}"));
    let span = Span::new(file, 0..line.len());

    let mut b = NodeBuilder::new(&mut parsed.spans, span);
    let (f, expr) = match (wrapper, stmt.node) {
        (Wrapper::Main, ReplStmt::Expr(e)) => {
            let expr = e.id;
            (main_function(&mut b, e), expr)
        }
        (Wrapper::Print, ReplStmt::Expr(e)) => {
            let expr = e.id;
            (print_function(&mut b, e), expr)
        }
        (Wrapper::Init, ReplStmt::Let(let_stmt)) => {
            let Stmt::Let(ident, _, init) = &let_stmt.node else {
                ice!("broken let statement parsing")
            };
            let (name, expr) = (ident.node, init.id);
            (init_function(&mut b, let_stmt, name), expr)
        }
        (Wrapper::Init, ReplStmt::Declaration(Declaration::Const(c))) => {
            let (name, expr) = (c.ident.node, c.expr.id);
            let let_stmt = b.let_from_const(c);
            (init_function(&mut b, let_stmt, name), expr)
        }
        (wrapper, _) => {
            ice!("the {wrapper:?} wrapper does not fit the input")
        }
    };

    let body = f.body.id;
    parsed.module_tree.modules[0]
        .ast
        .declarations
        .insert(0, Declaration::Function(f));
    Wrapped { expr, body }
}

/// `fn $repl_main() { <expr>; }`
fn main_function(
    b: &mut NodeBuilder<'_>,
    expr: Meta<Expr>,
) -> FunctionDeclaration {
    let stmt = b.expr_stmt(expr);
    b.function(MAIN, None, vec![stmt], None)
}

/// `fn $repl_main() -> std.String { f"{<expr>}" }`
///
/// `std.String`, not `String`, which a session type can shadow. `std` is
/// a keyword, so nothing can shadow it.
fn print_function(
    b: &mut NodeBuilder<'_>,
    expr: Meta<Expr>,
) -> FunctionDeclaration {
    let ret = b.type_path(["std", "String"]);
    let text = b.fstring(expr);
    b.function(MAIN, Some(ret), Vec::new(), Some(text))
}

/// `fn $repl_init() { <let>; $repl.store(<name>); }`
fn init_function(
    b: &mut NodeBuilder<'_>,
    let_stmt: Meta<Stmt>,
    name: Identifier,
) -> FunctionDeclaration {
    let arg = b.path([name]);
    let store = b.call([HELPERS, STORE], vec![arg]);
    let store = b.expr_stmt(store);
    b.function(INIT, None, vec![let_stmt, store], None)
}
