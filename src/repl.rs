#[cfg(feature = "repl")]
use rustyline::error::ReadlineError;
use rustyline::validate::{ValidationContext, ValidationResult, Validator};
use rustyline::{
    Cmd, Completer, Editor, EventHandler, Helper, Highlighter, Hinter,
    KeyCode, KeyEvent, Modifiers, history::DefaultHistory,
};

use std::cell::Cell;

use indexmap::{IndexMap, IndexSet};

use crate::{
    RotoError, RotoReport, Runtime,
    ast::{Declaration, Identifier, Stmt},
    codegen::{ConstantImports, RotoConstant, SharedModuleData},
    ice, library,
    load::{FileTree, Load, SourceFile},
    mir::{Pool, Ty, TyRef},
    parser::Parser,
    parser::lexer::Lexer,
    parser::meta::{Span, Spans},
    parser::token::Token,
    parser::{ParseError, ParseErrorKind, ParsedStmt},
    pipeline::{Blob, TypeChecked},
    runtime::context::NoCtx,
    runtime::func::OutPtr,
    typechecker::TypeChecker,
    typechecker::info::TypeInfo,
    typechecker::scope::{DeclarationKind, ResolvedName},
    typechecker::scoped_display::TypeDisplay,
    typechecker::types::{Type, TypeName},
    value::{DynVal, RotoString, VTable},
};

#[derive(Completer, Helper, Highlighter, Hinter)]
struct ReplValidator;

impl Validator for ReplValidator {
    fn validate(
        &self,
        ctx: &mut ValidationContext,
    ) -> rustyline::Result<ValidationResult> {
        if is_blank(ctx.input()) {
            return Ok(ValidationResult::Valid(None));
        }

        let mut spans = Spans::default();
        match Parser::parse_stmt(0, &mut spans, ctx.input()) {
            Err(e) if matches!(e.kind, ParseErrorKind::EndOfInput) => {
                Ok(ValidationResult::Incomplete)
            }
            _ => Ok(ValidationResult::Valid(None)),
        }
    }
}

const SESSION_MODULE: &str = "pkg";
const RESERVED_PREFIX: &str = "__repl_";

fn repl_files(contents: String) -> Vec<SourceFile> {
    vec![SourceFile {
        name: "<repl>".into(),
        module_name: SESSION_MODULE.into(),
        contents,
        location_offset: 0,
        children: Vec::new(),
    }]
}

// REPL input classification
#[derive(Debug)]
enum ReplInput {
    /// `fn`, `filtermap`, `record` and `enum`
    Decl(ItemKind, Identifier, String),
    /// `import <path>`
    Import(Vec<String>),
    /// `let`, `const`
    Let(Identifier, String),
    Expr(String),
}

fn classify_input(line: &str) -> Result<ReplInput, RotoReport> {
    let input = parse_input(line)?;

    // Do not allow user to call REPL's own runtime functions to
    // prevent unexpected consequences
    let tokens = lex(line);
    if tokens.iter().any(
        |t| matches!(t, Token::Ident(s) if s.starts_with(RESERVED_PREFIX)),
    ) {
        return Err(custom_report(&format!(
            "names starting with `{RESERVED_PREFIX}` are reserved for the REPL"
        )));
    }

    Ok(input)
}

fn lex(src: &str) -> Vec<Token<'_>> {
    let mut lexer = Lexer::new(0, src);
    let mut tokens = Vec::new();
    while let Some((Ok(token), _)) = lexer.next() {
        tokens.push(token);
    }
    tokens
}

fn is_blank(src: &str) -> bool {
    Lexer::new(0, src).next().is_none()
}

fn parse_input(line: &str) -> Result<ReplInput, RotoReport> {
    let mut spans = Spans::default();
    let stmt = Parser::parse_stmt(0, &mut spans, line)
        .map_err(|e| parse_report(line, *e))?;
    // The text is taken by the span of the statement,
    // which skips the optional trailing semicolon or comment
    let text = span_text(line, spans.get(&stmt));
    match stmt.node {
        ParsedStmt::Declaration(d) => {
            classify_declaration(&d, &spans, line, text)
        }
        ParsedStmt::Let(stmt) => {
            let Stmt::Let(ident, _, _) = stmt.node else {
                ice!("broken let statement parsing")
            };
            Ok(ReplInput::Let(ident.node, text.into()))
        }
        ParsedStmt::Expr(_) => Ok(ReplInput::Expr(text.into())),
    }
}

fn span_text(line: &str, span: Span) -> &str {
    &line[span.start..span.end]
}

fn classify_declaration(
    d: &Declaration,
    spans: &Spans,
    line: &str,
    text: &str,
) -> Result<ReplInput, RotoReport> {
    let src = text.to_owned();
    Ok(match d {
        Declaration::Function(f) => {
            ReplInput::Decl(ItemKind::Fn, f.ident.node, src)
        }
        Declaration::FilterMap(f) => {
            ReplInput::Decl(ItemKind::Fn, f.ident.node, src)
        }
        Declaration::Record(r) => {
            ReplInput::Decl(ItemKind::Type, r.ident.node, src)
        }
        Declaration::Enum(e) => {
            ReplInput::Decl(ItemKind::Type, e.ident.node, src)
        }
        // Group imports are flattened the way the type checker does it.
        // The semicolon, optional at the prompt, is required in the
        // session text.
        Declaration::Import(path) => {
            let mut paths = Vec::new();
            TypeChecker::flatten_import_paths(&mut paths, path);
            ReplInput::Import(
                paths
                    .iter()
                    .map(|p| {
                        let idents: Vec<_> = p
                            .idents
                            .iter()
                            .map(|i| i.node.as_str())
                            .collect();
                        format!("import {};", idents.join("."))
                    })
                    .collect(),
            )
        }
        // A constant handled like a 'let' as value
        Declaration::Const(c) => {
            let span = spans.get(&c.ident).merge(spans.get(&c.expr));
            let text = format!("let {}", span_text(line, span));
            ReplInput::Let(c.ident.node, text)
        }
        Declaration::Test(_) => {
            return Err(custom_report(
                "tests cannot be declared in the REPL",
            ));
        }
    })
}

fn custom_report(msg: &str) -> RotoReport {
    RotoReport {
        errors: vec![RotoError::Custom(msg.into())],
        ..Default::default()
    }
}

fn parse_report(line: &str, e: ParseError) -> RotoReport {
    RotoReport {
        files: repl_files(line.into()),
        errors: vec![RotoError::Parse(e)],
        ..Default::default()
    }
}

struct Probe<'r> {
    checked: TypeChecked<'r, NoCtx>,
    ty: Type,
    diverges: bool,
}

/// REPL state between lines
///
/// Each line is compiled by the regular pipeline as a single file: the wrapped
/// input first, followed by the session text. The type checker graph is rebuilt
/// from scratch every time.
struct Session<'r> {
    /// The host runtime
    rt: &'r Runtime<NoCtx>,
    /// User imports, normalized, in input order
    imports: IndexSet<String>,
    /// Declarations and values by name, in input order
    items: IndexMap<Identifier, Item>,
    /// The types that the values were stored with, see
    /// [`Session::changed_value`]
    shapes: Pool,
    /// The values of the `Value` items, by full name (`pkg.x`)
    cells: ConstantImports,
    /// Every module that ran. A value can refer to code in any of them: a
    /// list keeps the clone, drop and eq functions of its elements
    modules: Vec<SharedModuleData>,
}

struct Item {
    src: String,
    kind: ItemKind,
}

#[derive(Debug)]
enum ItemKind {
    /// `fn` and `filtermap`
    Fn,
    /// `record` and `enum`
    Type,
    /// A `let`: `src` is `const x: T = __repl_cell();` and the value is in a
    /// cell. `shape` is the type the value was stored with, in `shapes`.
    Value { shape: TyRef },
}

impl<'r> Session<'r> {
    fn new(rt: &'r Runtime<NoCtx>) -> Self {
        Self {
            rt,
            imports: IndexSet::new(),
            items: IndexMap::new(),
            shapes: Pool::new(),
            cells: ConstantImports::new(),
            modules: Vec::new(),
        }
    }

    /// Generate text to compile. `entry` goes first.
    /// `skip` is the name that `entry` redeclares.
    fn source(&self, entry: &str, skip: Option<Identifier>) -> String {
        let mut s = format!("{entry}\n");
        for import in &self.imports {
            s.push_str(import);
            s.push('\n');
        }
        for (name, item) in &self.items {
            if Some(*name) != skip {
                s.push_str(&item.src);
                s.push('\n');
            }
        }
        s
    }

    /// Add a declaration, or redefine one.
    /// Check if the new declaration breaks something
    /// defined in the session earlier.
    fn add_decl(
        &mut self,
        kind: ItemKind,
        name: Identifier,
        src: &str,
    ) -> Result<(), RotoReport> {
        let mut checked = self.check(self.source(src, Some(name)))?;
        // The type of a value is resolved from its text in every compilation,
        // so redefining a type it depends on would make its cell read as
        // another type
        if let Some(value) = self.changed_value(&mut checked, None) {
            return Err(custom_report(&format!(
                "cannot declare `{name}`: it changes the type of the value \
                 `{value}`; redefine `{value}` first"
            )));
        }

        if matches!(kind, ItemKind::Fn) {
            let ident =
                match checked.module_tree.modules[0].ast.declarations.first()
                {
                    Some(Declaration::Function(f)) => f.ident.id,
                    Some(Declaration::FilterMap(f)) => f.ident.id,
                    _ => ice!("the new function is the first declaration"),
                };
            let sig = checked.type_info.function_signature(ident);
            let ty = Type::Function(
                sig.parameter_types,
                Box::new(sig.return_type),
            );
            println!(
                "{name} : {}",
                ty.display(checked.main_scope, &checked.type_info)
            );
        }

        self.cells.remove(&cell_key(name));
        self.items.insert(
            name,
            Item {
                src: src.into(),
                kind,
            },
        );
        Ok(())
    }

    /// Evaluate `let name = ...` once and keep the value in a cell.
    /// `const name: T = __repl_cell();` goes into the session text, and
    /// codegen resolves that to the cell instead of running an initializer,
    /// so the initializer and its side effects run exactly once.
    fn add_let(
        &mut self,
        name: Identifier,
        stmt: &str,
    ) -> Result<(), RotoReport> {
        // a) Type check the initializer, which is run in step (c). The old
        // `name` stays in the session, so that `let x = x + 1` sees it.
        let init = format!(
            "fn __repl_init() {{\n{stmt}\n;\n__repl_store({name});\n}}"
        );
        let mut checked = self.check(self.source(&init, None))?;

        let (expr_id, body_id) = {
            let Some(Declaration::Function(f)) =
                checked.module_tree.modules[0].ast.declarations.first()
            else {
                ice!("__repl_init is the first declaration")
            };
            let Some(Stmt::Let(_, _, expr)) =
                f.body.stmts.first().map(|s| &s.node)
            else {
                ice!("__repl_init starts with the let")
            };
            (expr.id, f.body.id)
        };
        if checked.type_info.diverges(body_id) {
            return Err(custom_report(&format!(
                "the initializer of `{name}` never produces a value"
            )));
        }
        let ty = checked.type_info.type_of(expr_id);
        let ty = default_deep(&mut checked.type_info, &ty);
        let ty_text = ty
            .display(checked.main_scope, &checked.type_info)
            .to_string();
        match unstorable(&ty) {
            None => {}
            Some(Unstorable::Function) => {
                return Err(custom_report(&format!(
                    "cannot store `{name}` of type {ty_text}: functions \
                     cannot be stored as values"
                )));
            }
            Some(Unstorable::Unknown) => {
                return Err(custom_report(&format!(
                    "cannot store `{name}` of type {ty_text}: its type is not \
                     fully known; add a type annotation: \
                     `let {name}: <type> = ...`"
                )));
            }
        }
        let shape = {
            let ti = &mut checked.type_info;
            let ty = ti.convert(&ty);
            reintern(&ti.ty_pool, ty, &mut self.shapes)
        };

        // b) The session with the new value must type check, before the
        // initializer runs: functions that read the old `name` may not
        // accept the new type. The type text must also mean the type of the
        // value, which it does not if a session type shadows a built-in one.
        let src = format!("const {name}: {ty_text} = __repl_cell();");
        let mut session = self.check(self.source(&src, Some(name)))?;
        match self.changed_value(&mut session, Some((name, shape))) {
            None => {}
            Some(value) if value == name => {
                return Err(custom_report(&format!(
                    "cannot store `{name}`: its type, written as {ty_text}, \
                     means another type in the session"
                )));
            }
            Some(value) => {
                return Err(custom_report(&format!(
                    "cannot define `{name}`: it changes the type of the \
                     value `{value}`; redefine `{value}` first"
                )));
            }
        }

        // c) Run the initializer, which moves the value into a new cell
        let mut blob = checked
            .lower_to_mir()
            .lower_to_lir()
            .codegen_with_constants(&self.cells);
        run_function(&mut blob, "__repl_init");
        self.modules.push(blob.shared_data());
        let Some(cell) = STORED.take() else {
            ice!("__repl_init stores a value")
        };

        // d) Commit. The old cell, if any, is dropped here: nothing refers to
        // it any more, because every compilation starts afresh.
        println!("{name} : {ty_text}");
        self.cells.insert(cell_key(name), cell);
        self.items.insert(
            name,
            Item {
                src,
                kind: ItemKind::Value { shape },
            },
        );
        Ok(())
    }

    /// Find a session value whose type in `checked` no longer matches the
    /// type it was stored with
    ///
    /// Types are compared by structure (fields and variants), not by name.
    /// `new` is the name and stored type of a value not yet in the session.
    fn changed_value(
        &mut self,
        checked: &mut TypeChecked<'r, NoCtx>,
        new: Option<(Identifier, TyRef)>,
    ) -> Option<Identifier> {
        let ti = &mut checked.type_info;
        for d in &checked.module_tree.modules[0].ast.declarations {
            let Declaration::Const(c) = d else { continue };
            let name = c.ident.node;
            let stored = match new {
                Some((n, shape)) if n == name => shape,
                _ => self.value_shape(name),
            };
            let ty = ti.type_of(&c.ident);
            let ty = ti.convert(&ty);
            if reintern(&ti.ty_pool, ty, &mut self.shapes) != stored {
                return Some(name);
            }
        }
        None
    }

    /// The type that the value `name` was stored with, in `shapes`
    fn value_shape(&self, name: Identifier) -> TyRef {
        match self.items.get(&name) {
            Some(Item {
                kind: ItemKind::Value { shape },
                ..
            }) => *shape,
            _ => ice!("a constant in the session is a value"),
        }
    }

    /// Add the imports that the session does not have yet, all or none
    ///
    /// The type checker reports a clash between imports. A session item with
    /// the same name as an import takes precedence, as in a file.
    fn add_import(&mut self, imports: Vec<String>) -> Result<(), RotoReport> {
        let new: Vec<String> = imports
            .into_iter()
            .filter(|i| !self.imports.contains(i))
            .collect();
        if new.is_empty() {
            return Ok(());
        }
        let mut checked = self.check(self.source(&new.join("\n"), None))?;
        // An import can shadow a type that the type text of a value refers to,
        // which would make its cell read as another type
        if let Some(value) = self.changed_value(&mut checked, None) {
            return Err(custom_report(&format!(
                "cannot import this: it changes the type of the value \
                 `{value}`; redefine `{value}` first"
            )));
        }
        self.imports.extend(new);
        Ok(())
    }

    fn check(
        &self,
        text: String,
    ) -> Result<TypeChecked<'r, NoCtx>, RotoReport> {
        let rt = self.rt;
        let tree = FileTree {
            files: repl_files(text),
        };
        tree.load()?.resolve(rt)?.parse()?.typecheck(rt)
    }

    /// Type check an expression and return its type
    fn probe(&self, expr: &str) -> Result<Probe<'r>, RotoReport> {
        let text =
            self.source(&format!("fn __repl_main() {{\n{expr}\n;\n}}"), None);
        let mut checked = self.check(text)?;

        let (expr_id, body_id) = {
            let Some(Declaration::Function(f)) =
                checked.module_tree.modules[0].ast.declarations.first()
            else {
                ice!("__repl_main is the first declaration")
            };
            let [stmt] = &f.body.stmts[..] else {
                ice!("the wrapper contains exactly one statement")
            };
            let Stmt::Expr(e) = &stmt.node else {
                ice!("the wrapper statement is an expression")
            };
            (e.id, f.body.id)
        };

        let diverges = checked.type_info.diverges(body_id);
        let ty = default_numeric(checked.type_info.type_of(expr_id));

        Ok(Probe {
            checked,
            ty,
            diverges,
        })
    }

    fn eval(&mut self, line: &str) -> Result<(), RotoReport> {
        let mut p = self.probe(line)?;
        if p.diverges {
            eprintln!("Expression diverges: do nothing");
            return Ok(());
        }

        let checked = if has_to_string(&mut p.checked.type_info, &p.ty) {
            self.check(self.source(
                &format!(
                    "fn __repl_main() {{\n__repl_display(f\"{{\n{line}\n}}\");\n}}"
                ),
                None,
            ))?
        } else {
            if !matches!(p.ty, Type::Unit) {
                eprintln!(
                    "Warning: Expression result has type {} and can't be converted to string",
                    p.ty.display(p.checked.main_scope, &p.checked.type_info)
                );
            }
            p.checked
        };

        let mut pkg = checked
            .lower_to_mir()
            .lower_to_lir()
            .codegen_with_constants(&self.cells);
        run_function(&mut pkg, "__repl_main");
        self.modules.push(pkg.shared_data());

        Ok(())
    }
}

impl Drop for Session<'_> {
    fn drop(&mut self) {
        // The values refer to code in `modules`, so they go first, whatever
        // the order of the fields. So does a value that `__repl_store` left
        // behind, which would otherwise outlive the session in `STORED`.
        self.cells.clear();
        drop(STORED.take());
    }
}

fn default_numeric(ty: Type) -> Type {
    match ty {
        Type::IntVar(..) => Type::i32(),
        Type::FloatVar(_) => Type::f64(),
        ty => ty,
    }
}

/// Check whether `ty` is a named type with a `to_string` method, which an
/// f-string needs to show a value of it
fn has_to_string(type_info: &mut TypeInfo, ty: &Type) -> bool {
    let Type::Name(type_name) = type_info.resolve(ty) else {
        return false;
    };
    let type_dec = type_info.scope_graph.get_declaration(type_name.name);
    let Some(type_scope) = type_dec.scope else {
        return false;
    };
    let name = ResolvedName {
        scope: type_scope,
        ident: "to_string".into(),
    };
    matches!(
        type_info
            .scope_graph
            .declarations
            .get(&name)
            .map(|d| &d.kind),
        Some(DeclarationKind::Method(Some(_)))
    )
}

/// Run the generated function `name`
fn run_function(prog: &mut Blob<NoCtx>, name: &str) {
    let f = prog
        .get_function::<fn()>(name)
        .unwrap_or_else(|e| ice!("cannot get `{name}`: {e}"));
    f.call();
}

/// The key of the cell of `name`: the full name of its constant
fn cell_key(name: Identifier) -> String {
    format!("{SESSION_MODULE}.{name}")
}

/// Replace the numeric type variables in `ty`, at any depth, with the
/// defaults that lowering picks for them
///
/// The result can then be written as text in the session.
fn default_deep(ti: &mut TypeInfo, ty: &Type) -> Type {
    match ti.resolve(ty) {
        Type::IntVar(..) => Type::i32(),
        Type::FloatVar(_) => Type::f64(),
        Type::Record(fields) | Type::RecordVar(_, fields) => Type::Record(
            fields
                .iter()
                .map(|(n, t)| (n.clone(), default_deep(ti, t)))
                .collect(),
        ),
        Type::Name(TypeName { name, arguments }) => Type::Name(TypeName {
            name,
            arguments: arguments
                .iter()
                .map(|t| default_deep(ti, t))
                .collect(),
        }),
        t => t,
    }
}

/// Why a value of a (defaulted) type cannot be stored in a cell
enum Unstorable {
    /// Lowering has no layout for functions
    Function,
    /// A type variable is left, so the type can't be written back as text
    Unknown,
}

fn unstorable(ty: &Type) -> Option<Unstorable> {
    match ty {
        Type::Function(..) => Some(Unstorable::Function),
        Type::Var(_) | Type::ExplicitVar(_) => Some(Unstorable::Unknown),
        Type::Record(fields) | Type::RecordVar(_, fields) => {
            fields.iter().find_map(|(_, t)| unstorable(t))
        }
        Type::Name(TypeName { arguments, .. }) => {
            arguments.iter().find_map(unstorable)
        }
        _ => None,
    }
}

/// Copy `ty`, with the types it contains, from the pool `from` into `to`
///
/// A pool interns types by structure, so types from two compilations are
/// equal exactly when they get the same `TyRef` in one pool. A type that
/// type checked is not recursive, so the recursion ends.
fn reintern(from: &Pool, ty: TyRef, to: &mut Pool) -> TyRef {
    let ty = match from.get(ty) {
        Ty::Record(fields) => Ty::Record(
            fields
                .iter()
                .map(|&(n, t)| (n, reintern(from, t, to)))
                .collect(),
        ),
        Ty::Enum(variants) => Ty::Enum(
            variants
                .iter()
                .map(|(n, ts)| {
                    (*n, ts.iter().map(|&t| reintern(from, t, to)).collect())
                })
                .collect(),
        ),
        Ty::List(t) => Ty::List(reintern(from, *t, to)),
        ty @ (Ty::Unit | Ty::Never | Ty::Primitive(_) | Ty::Runtime(_)) => {
            ty.clone()
        }
    };
    to.intern(ty)
}

thread_local! {
    static STORED: Cell<Option<RotoConstant>> = const { Cell::new(None) };
}

unsafe extern "C" fn no_drop(_: *mut ()) {}

/// The host runtime plus the functions that the REPL wraps input in
fn repl_runtime(rt: &Runtime<NoCtx>) -> Runtime<NoCtx> {
    let mut rt = rt.clone();
    rt.add(library! {
        /// Print the value of an expression. The REPL has its own, rather
        /// than calling `print`, which the host may not have registered and
        /// the session may shadow.
        fn __repl_display(s: RotoString) {
            println!("{s}");
        }

        /// Move a value into a new cell, see `Session::add_let`
        #[sig = "fn[T](T)"]
        #[vtables(T)]
        fn __repl_store(vtable: VTable, v: DynVal) {
            let cell = RotoConstant::new(
                vtable.size(),
                vtable.align(),
                vtable.drop_fn.unwrap_or(no_drop),
            );
            // SAFETY: the cell is allocated with the layout of `T`, and `v`
            // points to a `T`. A callee owns its parameters, so copying the
            // bytes moves the value, and `v` must not be dropped.
            unsafe {
                std::ptr::copy_nonoverlapping(
                    v.0 as *const u8,
                    cell.ptr() as *mut u8,
                    vtable.size(),
                );
            }
            STORED.set(Some(cell));
        }

        /// The initializer of the constant of a cell. Never called, because
        /// codegen takes the address of the cell instead.
        #[sig = "fn[T]() -> T"]
        fn __repl_cell(_out: OutPtr<DynVal>) {
            ice!("__repl_cell is never called")
        }
    })
    .unwrap();
    rt
}

fn handle_line(
    session: &mut Session<'_>,
    line: &str,
) -> Result<(), RotoReport> {
    match classify_input(line)? {
        ReplInput::Decl(kind, name, src) => {
            session.add_decl(kind, name, &src)?
        }
        ReplInput::Import(imports) => session.add_import(imports)?,
        ReplInput::Let(name, stmt) => session.add_let(name, &stmt)?,
        ReplInput::Expr(src) => session.eval(&src)?,
    }
    Ok(())
}

pub(crate) fn repl(rt: &Runtime<NoCtx>) -> Result<(), RotoReport> {
    let rt = repl_runtime(rt);
    let mut session = Session::new(&rt);
    let mut rl = Editor::<ReplValidator, DefaultHistory>::new().unwrap();
    rl.set_helper(Some(ReplValidator));
    rl.bind_sequence(
        KeyEvent(KeyCode::Char('s'), Modifiers::CTRL),
        EventHandler::Simple(Cmd::Newline),
    );

    loop {
        let readline = rl.readline("roto> ");
        match readline {
            Ok(line) => {
                if is_blank(&line) {
                    continue;
                }
                let _ = rl.add_history_entry(line.as_str());
                if let Err(e) = handle_line(&mut session, &line) {
                    eprintln!("{e}");
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("Keyboard Interrupt");
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    Ok(())
}
