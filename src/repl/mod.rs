use indexmap::{IndexMap, IndexSet};
use rotoline::{
    Output, Signal,
    highlight::{Highlighter, ansi},
    prompt::{BufferInfo, LineInfo, Prompt},
    validate::Validator,
};

use crate::{
    RotoError, RotoReport, Runtime,
    ast::{Declaration, Identifier, ReplStmt, Stmt},
    codegen::{ConstantImports, SharedModuleData},
    ice,
    load::{FileTree, Load},
    mir::{Pool, Ty, TyRef},
    module::Parsed,
    parser::Parser,
    parser::lexer::Lexer,
    parser::meta::{Span, Spans},
    parser::{ParseError, ParseErrorKind},
    pipeline::{Blob, TypeChecked},
    runtime::context::NoCtx,
    typechecker::TypeChecker,
    typechecker::info::TypeInfo,
    typechecker::scope::{DeclarationKind, ResolvedName},
    typechecker::scoped_display::TypeDisplay,
    typechecker::types::{Type, TypeName},
    value::{RotoString, Value},
};

mod wrap;

/// REPL input classification
#[derive(Debug)]
enum ReplInput {
    /// `fn`, `filtermap`, `record` and `enum`
    Decl(ItemKind, Identifier, String),
    /// `import <path>`
    Import(Vec<String>),
    /// `let`, `const`. The wrapper is built from the parsed line.
    Let(Identifier),
    /// An expression. The wrapper is built from the parsed line.
    Expr,
}

fn classify_input(line: &str) -> Result<ReplInput, RotoReport> {
    let mut spans = Spans::default();
    let stmt = Parser::parse_repl_stmt(0, &mut spans, line)
        .map_err(|e| parse_report(line, *e))?;
    let span = spans.get(&stmt);
    match stmt.node {
        // A declaration goes into the session text. It is taken by the span
        // of the statement, which skips the optional trailing semicolon or
        // comment.
        ReplStmt::Declaration(d) => {
            classify_declaration(&d, span_text(line, span))
        }
        ReplStmt::Let(stmt) => {
            let Stmt::Let(ident, _, _) = stmt.node else {
                ice!("broken let statement parsing")
            };
            Ok(ReplInput::Let(ident.node))
        }
        ReplStmt::Expr(_) => Ok(ReplInput::Expr),
    }
}

fn is_blank(src: &str) -> bool {
    Lexer::new(0, src).next().is_none()
}

fn span_text(line: &str, span: Span) -> &str {
    &line[span.start..span.end]
}

fn classify_declaration(
    d: &Declaration,
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
        // A constant is stored like a `let`; the wrapper turns it into one
        Declaration::Const(c) => ReplInput::Let(c.ident.node),
        Declaration::Test(_) => {
            return Err(custom_report(
                "tests cannot be declared in the REPL",
            ));
        }
    })
}

fn custom_report(msg: impl Into<String>) -> RotoReport {
    RotoReport {
        errors: vec![RotoError::Custom(msg.into())],
        ..Default::default()
    }
}

fn parse_report(line: &str, e: ParseError) -> RotoReport {
    RotoReport {
        files: vec![wrap::input_file(line)],
        errors: vec![RotoError::Parse(e)],
        ..Default::default()
    }
}

/// The error for a line that would change the type of the stored value
/// `value`; `what` is what the line tried to do, such as "declare `f`"
fn changed_value_report(what: &str, value: Identifier) -> RotoReport {
    custom_report(format!(
        "cannot {what}: it changes the type of the value `{value}`; \
         redefine `{value}` first"
    ))
}

struct Probe<'r> {
    checked: TypeChecked<'r, NoCtx>,
    ty: Type,
    diverges: bool,
}

/// REPL state between lines
///
/// Each line is compiled by the regular pipeline from the session text, in
/// the file `<session>`. An expression or a `let` is parsed on its own, from
/// the file `<input>`, and wrapped in a function built as syntax tree nodes
/// (see [`wrap`]), which is added to the parsed session before type checking.
/// The type checker graph is rebuilt from scratch every time.
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
    /// A `let`: `src` is `const x: T = ();` and the value is in a cell. The
    /// placeholder initializer is replaced by a call to the cell helper
    /// after parsing (see [`wrap::patch_cells`]). `shape` is the type the
    /// value was stored with, in `shapes`.
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

    /// Generate the session text. `entry`, if any, goes first.
    /// `skip` is the name that `entry` redeclares.
    fn source(
        &self,
        entry: Option<&str>,
        skip: Option<Identifier>,
    ) -> String {
        let mut s = String::new();
        if let Some(entry) = entry {
            s.push_str(entry);
            s.push('\n');
        }
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
    ) -> Result<Option<String>, RotoReport> {
        let mut checked = self.check(self.source(Some(src), Some(name)))?;
        // The type of a value is resolved from its text in every compilation,
        // so redefining a type it depends on would make its cell read as
        // another type
        if let Some(value) = self.changed_value(&mut checked, None) {
            return Err(changed_value_report(
                &format!("declare `{name}`"),
                value,
            ));
        }

        let output = if matches!(kind, ItemKind::Fn) {
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
            Some(format!(
                "{name} : {}",
                ty.display(checked.main_scope, &checked.type_info)
            ))
        } else {
            None
        };

        self.cells.remove(&cell_key(name));
        self.items.insert(
            name,
            Item {
                src: src.into(),
                kind,
            },
        );
        Ok(output)
    }

    /// Evaluate the `let` (or `const`) in `line` once and keep the value in
    /// a cell. `const name: T = ();` goes into the session text, its
    /// initializer becomes a call to the cell helper, and codegen resolves
    /// that to the cell instead of running an initializer, so the
    /// initializer and its side effects run exactly once.
    fn add_let(
        &mut self,
        name: Identifier,
        line: &str,
    ) -> Result<Option<String>, RotoReport> {
        // a) Type check the initializer, which is run in step (c). The old
        // `name` stays in the session, so that `let x = x + 1` sees it.
        let (mut checked, wrapped) =
            self.check_input(line, wrap::Wrapper::Init)?;
        if checked.type_info.diverges(wrapped.body) {
            return Err(custom_report(format!(
                "the initializer of `{name}` never produces a value"
            )));
        }
        let ty = checked.type_info.type_of(wrapped.expr);
        let ty = default_deep(&mut checked.type_info, &ty);
        let ty_text = ty
            .display(checked.main_scope, &checked.type_info)
            .to_string();
        match unstorable(&ty) {
            None => {}
            Some(Unstorable::Function) => {
                return Err(custom_report(format!(
                    "cannot store `{name}` of type {ty_text}: functions \
                     cannot be stored as values"
                )));
            }
            Some(Unstorable::Unknown) => {
                return Err(custom_report(format!(
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
        let src = format!("const {name}: {ty_text} = ();");
        let mut session = self.check(self.source(Some(&src), Some(name)))?;
        match self.changed_value(&mut session, Some((name, shape))) {
            None => {}
            Some(value) if value == name => {
                return Err(custom_report(format!(
                    "cannot store `{name}`: its type, written as {ty_text}, \
                     means another type in the session"
                )));
            }
            Some(value) => {
                return Err(changed_value_report(
                    &format!("define `{name}`"),
                    value,
                ));
            }
        }

        // c) Run the initializer, which moves the value into a new cell
        let mut blob = checked
            .lower_to_mir()
            .lower_to_lir()
            .codegen_with_constants(&self.cells);
        run_function::<()>(&mut blob, wrap::INIT);
        self.modules.push(blob.shared_data());
        // A `return` in the initializer can skip the store. Nothing is
        // committed yet, so the session stays as it was.
        let Some(cell) = wrap::take_stored() else {
            return Err(custom_report(format!(
                "the initializer of `{name}` returned before producing a value"
            )));
        };

        // d) Commit. The old cell, if any, is dropped here: nothing refers to
        // it any more, because every compilation starts afresh.
        self.cells.insert(cell_key(name), cell);
        self.items.insert(
            name,
            Item {
                src,
                kind: ItemKind::Value { shape },
            },
        );
        Ok(Some(format!("{name} : {ty_text}")))
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
    fn add_import(
        &mut self,
        imports: Vec<String>,
    ) -> Result<Option<String>, RotoReport> {
        let new: Vec<String> = imports
            .into_iter()
            .filter(|i| !self.imports.contains(i))
            .collect();
        if new.is_empty() {
            return Ok(None);
        }
        let mut checked =
            self.check(self.source(Some(&new.join("\n")), None))?;
        // An import can shadow a type that the type text of a value refers to,
        // which would make its cell read as another type
        if let Some(value) = self.changed_value(&mut checked, None) {
            return Err(changed_value_report("import this", value));
        }
        self.imports.extend(new);
        Ok(None)
    }

    /// Load, resolve and parse the session text, and point the cells at
    /// their storage (see [`wrap::patch_cells`])
    fn parse_session(&self, text: String) -> Result<Parsed, RotoReport> {
        let tree = FileTree {
            files: vec![wrap::session_file(text)],
        };
        let mut parsed = tree.load()?.resolve(self.rt)?.parse()?;
        wrap::patch_cells(&mut parsed);
        Ok(parsed)
    }

    /// Type check the session text as it is
    fn check(
        &self,
        text: String,
    ) -> Result<TypeChecked<'r, NoCtx>, RotoReport> {
        self.parse_session(text)?.typecheck(self.rt)
    }

    /// Type check the session with `line` wrapped as `wrapper`
    fn check_input(
        &self,
        line: &str,
        wrapper: wrap::Wrapper,
    ) -> Result<(TypeChecked<'r, NoCtx>, wrap::Wrapped), RotoReport> {
        let mut parsed = self.parse_session(self.source(None, None))?;
        let wrapped = wrap::wrap_input(&mut parsed, line, wrapper);
        Ok((parsed.typecheck(self.rt)?, wrapped))
    }

    /// Type check the expression in `line` and return its type
    fn probe(&self, line: &str) -> Result<Probe<'r>, RotoReport> {
        let (mut checked, wrapped) =
            self.check_input(line, wrap::Wrapper::Main)?;
        let diverges = checked.type_info.diverges(wrapped.body);
        let ty = checked.type_info.type_of(wrapped.expr);
        let ty = default_deep(&mut checked.type_info, &ty);

        Ok(Probe {
            checked,
            ty,
            diverges,
        })
    }

    /// Evaluate the expression in `line`. Returns its value as text, for the
    /// caller to print, or `None` if it has none to show.
    ///
    /// A value that can't be converted to a string is reported as an error
    /// after the expression has run, so its side effects have happened.
    fn eval(&mut self, line: &str) -> Result<Option<String>, RotoReport> {
        let mut p = self.probe(line)?;
        if p.diverges {
            return Err(custom_report("Expression diverges: do nothing"));
        }

        let printable = has_to_string(&mut p.checked.type_info, &p.ty);
        let warning = (!printable && !matches!(p.ty, Type::Unit)).then(|| {
            custom_report(format!(
                "Warning: Expression result has type '{}' and can't be converted to string",
                p.ty.display(p.checked.main_scope, &p.checked.type_info)
            ))
        });

        // The print wrapper can reject input that `probe` accepted: a
        // `return` in it returns `()` where the wrapper returns a `String`.
        // Such input runs as it is, without printing.
        let print = if printable {
            self.check_input(line, wrap::Wrapper::Print).ok()
        } else {
            None
        };
        let (checked, printed) = match print {
            Some((checked, _)) => (checked, true),
            None => (p.checked, false),
        };

        let mut pkg = checked
            .lower_to_mir()
            .lower_to_lir()
            .codegen_with_constants(&self.cells);
        let text = if printed {
            let text = run_function::<RotoString>(&mut pkg, wrap::MAIN);
            Some(text.to_string())
        } else {
            run_function::<()>(&mut pkg, wrap::MAIN);
            None
        };
        self.modules.push(pkg.shared_data());

        match warning {
            Some(warning) => Err(warning),
            None => Ok(text),
        }
    }
}

impl Drop for Session<'_> {
    fn drop(&mut self) {
        // The values refer to code in `modules`, so they go first, whatever
        // the order of the fields. So does a value that the store helper left
        // behind, which would otherwise outlive the session in `STORED`.
        self.cells.clear();
        drop(wrap::take_stored());
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
fn run_function<R: Value>(prog: &mut Blob<NoCtx>, name: &str) -> R {
    prog.get_function::<fn() -> R>(name)
        .unwrap_or_else(|e| ice!("cannot get `{name}`: {e}"))
        .call()
}

/// The key of the cell of `name`: the full name of its constant
fn cell_key(name: Identifier) -> String {
    format!("{}.{name}", wrap::SESSION_MODULE)
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

/// The host runtime plus the helpers that the wrappers call (see
/// [`wrap::helpers`])
fn repl_runtime(rt: &Runtime<NoCtx>) -> Runtime<NoCtx> {
    let mut rt = rt.clone();
    rt.add(wrap::helpers()).expect("the REPL helpers register");
    rt
}

/// Handle one line. Returns what it has to show, for the caller to print.
fn handle_line(
    session: &mut Session<'_>,
    line: &str,
) -> Result<Option<String>, RotoReport> {
    match classify_input(line)? {
        ReplInput::Decl(kind, name, src) => {
            session.add_decl(kind, name, &src)
        }
        ReplInput::Import(imports) => session.add_import(imports),
        ReplInput::Let(name) => session.add_let(name, line),
        ReplInput::Expr => session.eval(line),
    }
}

struct RotoValidator;

impl Validator for RotoValidator {
    fn is_valid(&mut self, buffer: &str) -> bool {
        if is_blank(buffer) {
            return true;
        }

        let mut spans = Spans::default();
        match Parser::parse_repl_stmt(0, &mut spans, buffer) {
            Err(e) if matches!(e.kind, ParseErrorKind::EndOfInput) => false,
            _ => true,
        }
    }
}

struct RotoPrompt;

impl RotoPrompt {
    fn align(info: &BufferInfo) -> usize {
        let computed =
            1 + ((info.total_lines.0 + 1) as f64).log10().ceil() as usize;
        computed.max(2)
    }
}

impl Prompt for RotoPrompt {
    fn line_prefix(&mut self, info: &LineInfo) -> Option<String> {
        let align = Self::align(&info.buffer);

        if info.continuation {
            Some(format!("{}{:>align$}│ {}", ansi::DIM, " ", ansi::RESET))
        } else {
            Some(format!(
                "{}{:>align$}│ {}",
                ansi::DIM,
                info.line,
                ansi::RESET
            ))
        }
    }
}

struct RotoHighlighter;

impl Highlighter for RotoHighlighter {
    fn highlight(&mut self, buffer: &str) -> rotoline::highlight::StyledStr {
        crate::highlight::highlight(buffer)
    }
}

pub(crate) fn repl(rt: &Runtime<NoCtx>) -> Result<(), RotoReport> {
    let rt = repl_runtime(rt);
    let mut session = Session::new(&rt);
    let mut rl = rotoline::Reader::new()
        .with_validator(RotoValidator)
        .with_prompt(RotoPrompt)
        .with_highlighter(RotoHighlighter);

    loop {
        let readline = rl.read();
        match readline {
            Ok(Output::Text(text)) => {
                if is_blank(&text) {
                    println!();
                    continue;
                }
                println!();
                match handle_line(&mut session, &text) {
                    Ok(Some(text)) => {
                        println!("{text}");
                        println!();
                    }
                    Ok(None) => {
                        println!();
                    }
                    Err(e) => eprintln!("{e}"),
                }
            }
            Ok(Output::Signal(Signal::CtrlC)) => {
                println!("Keyboard Interrupt");
                break;
            }
            Ok(Output::Signal(Signal::CtrlD)) => {
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
