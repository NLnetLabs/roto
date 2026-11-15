//! Compiler pipeline that executes multiple compiler stages in sequence

use std::{
    collections::{HashMap, VecDeque},
    fmt,
    ops::Range,
};

use crate::{
    ast::Identifier,
    codegen::{
        self, Module, TypedFunc,
        check::{FunctionRetrievalError, RotoFunc},
    },
    label::LabelStore,
    lir,
    load::{LoadError, SourceFile},
    mir,
    module::{ModuleTree, Parsed},
    parser::{
        ParseError,
        meta::{Span, Spans},
    },
    runtime::{
        Runtime, RuntimeFunctionRef,
        context::Context,
        context::{Ctx, NoCtx, OptCtx},
    },
    typechecker::{
        error::{Level, TypeError},
        info::TypeInfo,
        scope::{ResolvedName, ScopeRef},
    },
};

#[cfg(test)]
use crate::lir::{
    IrValue,
    eval::{self, Memory},
};

#[cfg(feature = "logger")]
use crate::ir_printer::{IrPrinter, Printable};

#[cfg(feature = "logger")]
use log::info;

/// An error from a compilation of a Roto script.
#[derive(Debug)]
pub(crate) enum RotoError {
    DepsNotFound(Vec<String>),
    Read(LoadError),
    Parse(ParseError),
    Type(TypeError),
    TestsFailed(),
    CouldNotRetrieveFunction(FunctionRetrievalError),
    Custom(String),
}

/// An error report containing a set of Roto errors.
///
/// The report can be printed with the regular [`std::fmt::Display`].
#[derive(Default)]
pub struct RotoReport {
    /// Files that were used during compilation.
    ///
    /// These are used to print diagnostics.
    pub files: Vec<SourceFile>,
    pub(crate) errors: Vec<RotoError>,

    /// Spans of AST nodes.
    pub spans: Spans,
}

impl From<LoadError> for RotoReport {
    fn from(value: LoadError) -> Self {
        RotoReport {
            files: Vec::new(),
            errors: vec![RotoError::Read(value)],
            spans: Spans::default(),
        }
    }
}

/// Compiler stage: loaded, parsed and type checked
pub struct TypeChecked<'r, Ctx: OptCtx> {
    pub(crate) module_tree: ModuleTree,
    pub(crate) type_info: TypeInfo,
    pub(crate) main_scope: ScopeRef,
    order: Vec<ResolvedName>,
    runtime: &'r Runtime<Ctx>,
}

/// Compiler stage: MIR
pub struct LoweredToMir<'r, Ctx: OptCtx> {
    runtime: &'r Runtime<Ctx>,
    main: Identifier,
    main_scope: ScopeRef,
    ir: mir::Mir,
    label_store: LabelStore,
    type_info: TypeInfo,
}

/// Compiler stage: LIR
pub struct LoweredToLir<'r, Ctx: OptCtx> {
    runtime: &'r Runtime<Ctx>,
    main: Identifier,
    main_scope: ScopeRef,
    ir: lir::Lir,
    runtime_functions: HashMap<RuntimeFunctionRef, lir::Signature>,
    label_store: LabelStore,
    type_info: TypeInfo,
}

/// The final compiled package of a script.
///
/// Functions can be extracted from this package using [`Blob::get_function`].
pub struct Blob<Ctx: OptCtx> {
    module: Module<Ctx>,
}

impl RotoReport {
    /// Write this report to a type implementing [`fmt::Write`].
    pub fn write(&self, mut f: impl fmt::Write, color: bool) -> fmt::Result {
        use ariadne::{Color, IndexType, Label, Report, ReportKind};

        let sources = self
            .files
            .iter()
            .map(|s| {
                (
                    s.name(),
                    ariadne::Source::from(s.contents.clone())
                        .with_display_line_offset(s.location_offset),
                )
            })
            .collect();

        let mut file_cache = ariadne::FnCache::new(
            (move |id| {
                Err(Box::new(format!("Failed to fetch source '{}'", id)))
            })
                as fn(&_) -> Result<_, Box<dyn std::fmt::Debug + 'static>>,
        )
        .with_sources(sources);

        // Our spans are byte offsets
        let config = ariadne::Config::new()
            .with_color(color)
            .with_index_type(IndexType::Byte);

        for error in &self.errors {
            match error {
                RotoError::DepsNotFound(deps) => {
                    for dep in deps {
                        write!(f, "Could not find dependency: `{dep}`")?;
                    }
                }
                RotoError::Read(LoadError { path, err }) => {
                    write!(f, "Could not read file `{path}`: {err}")?;
                }
                RotoError::Parse(error) => {
                    let label_message = error.kind.label();
                    let label = Label::new(self.location(error.location))
                        .with_message(label_message)
                        .with_color(Color::Red);

                    let mut report = Report::build(
                        ReportKind::Error,
                        self.location(error.location),
                    )
                    .with_config(config)
                    .with_message(format!("Parse error: {}", error))
                    .with_label(label);

                    if let Some(hint) = error.kind.hint() {
                        report = report.with_help(hint);
                    }

                    for hint in &error.hints {
                        let label = Label::new(self.location(hint.location))
                            .with_message(&hint.text)
                            .with_color(Color::Yellow);

                        report = report.with_label(label)
                    }

                    if let Some(note) = &error.note {
                        report = report.with_note(note);
                    }

                    let report = report.finish();

                    let mut v = Vec::new();
                    report.write(&mut file_cache, &mut v).unwrap();
                    let s = String::from_utf8_lossy(&v);
                    write!(f, "{s}")?;
                }
                RotoError::Type(error) => {
                    let labels = error.labels.iter().map(|l| {
                        Label::new(self.location(self.spans.get(l.id)))
                            .with_message(&l.message)
                            .with_color(match l.level {
                                Level::Error => Color::Red,
                                Level::Info => Color::Blue,
                            })
                    });

                    let mut report = Report::build(
                        ReportKind::Error,
                        self.location(self.spans.get(error.location)),
                    )
                    .with_config(config)
                    .with_message(format!(
                        "Type error: {}",
                        error.description
                    ))
                    .with_labels(labels);

                    report.with_notes(&error.notes);
                    let report = report.finish();

                    let mut v = Vec::new();
                    report.write(&mut file_cache, &mut v).unwrap();
                    let s = String::from_utf8_lossy(&v);
                    write!(f, "{s}")?;
                }
                RotoError::TestsFailed() => {
                    write!(f, "Tests failed")?;
                }
                RotoError::CouldNotRetrieveFunction(e) => {
                    write!(f, "Could not retrieve function: {e}")?;
                }
                RotoError::Custom(s) => {
                    write!(f, "{s}")?;
                }
            }
        }

        Ok(())
    }
}

impl std::fmt::Display for RotoReport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.write(f, true)
    }
}

impl std::fmt::Debug for RotoReport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.write(f, true)
    }
}

impl RotoReport {
    /// The file name and byte range of a span
    fn location(&self, s: Span) -> (String, Range<usize>) {
        (self.files[s.file].name(), s.start..s.end)
    }
}

impl std::error::Error for RotoReport {}

#[cfg(all(test, not(miri)))]
macro_rules! src {
    ($code:expr) => {
        $crate::load::FileTree::test_file(
            file!(),
            $code,
            line!() as usize - 1,
        )
    };
}

#[cfg(all(test, not(miri)))]
pub(crate) use src;

#[cfg(all(test, not(miri)))]
macro_rules! source_file {
    ($module_name:literal, $code:literal) => {
        $crate::load::SourceFile {
            name: file!().into(),
            module_name: $module_name.into(),
            contents: $code.into(),
            location_offset: line!() as usize - 1,
            children: Vec::new(),
        }
    };
}

#[cfg(all(test, not(miri)))]
pub(crate) use source_file;

impl Parsed {
    pub fn typecheck<'r, Ctx: OptCtx>(
        self,
        runtime: &'r Runtime<Ctx>,
    ) -> Result<TypeChecked<'r, Ctx>, RotoReport> {
        let Parsed {
            files,
            module_tree,
            spans,
        } = self;

        let result = crate::typechecker::typecheck(&runtime.rt, &module_tree);

        let (main_scope, type_info, order) = match result {
            Ok(type_info) => type_info,
            Err(error) => {
                return Err(RotoReport {
                    files,
                    errors: vec![RotoError::Type(error)],
                    spans,
                });
            }
        };

        Ok(TypeChecked {
            module_tree,
            main_scope,
            type_info,
            order,
            runtime,
        })
    }
}

impl<'r, Ctx: OptCtx> TypeChecked<'r, Ctx> {
    pub fn lower_to_mir(self) -> LoweredToMir<'r, Ctx> {
        let TypeChecked {
            module_tree,
            main_scope,
            type_info,
            order,
            runtime,
        } = self;

        let mut type_info = type_info.clone();
        let mut label_store = LabelStore::default();
        let ir = mir::lower_to_mir(
            &module_tree,
            main_scope,
            &runtime.rt,
            &mut type_info,
            &mut label_store,
            &order,
        );

        #[cfg(feature = "logger")]
        {
            if log::log_enabled!(log::Level::Info) {
                let printer = IrPrinter {
                    type_info: &type_info,
                    label_store: &label_store,
                    scope: ScopeRef::GLOBAL,
                };
                let s = ir.print(&printer);
                info!("\n{s}");
            }
        }

        LoweredToMir {
            main: module_tree.main,
            main_scope,
            ir,
            runtime,
            label_store,
            type_info,
        }
    }
}

impl<'r, Ctx: OptCtx> LoweredToMir<'r, Ctx> {
    pub fn lower_to_lir(self) -> LoweredToLir<'r, Ctx> {
        let LoweredToMir {
            runtime,
            main,
            main_scope,
            ir,
            mut label_store,
            mut type_info,
        } = self;

        let mut runtime_functions = HashMap::new();
        let mut ctx = lir::lower::LowerCtx {
            main_scope,
            runtime: &runtime.rt,
            type_info: &mut type_info,
            label_store: &mut label_store,
            runtime_functions: &mut runtime_functions,
            drops_to_generate: VecDeque::new(),
            clones_to_generate: VecDeque::new(),
            eq_to_generate: VecDeque::new(),
        };
        let ir = lir::lower_to_lir(&mut ctx, ir);

        #[cfg(feature = "logger")]
        {
            if log::log_enabled!(log::Level::Info) {
                let printer = IrPrinter {
                    type_info: &type_info,
                    label_store: &label_store,
                    scope: ScopeRef::GLOBAL,
                };
                let s = ir.print(&printer);
                info!("\n{s}");
            }
        }

        LoweredToLir {
            main,
            main_scope,
            runtime,
            ir,
            label_store,
            type_info,
            runtime_functions,
        }
    }
}

impl<Ctx: OptCtx> LoweredToLir<'_, Ctx> {
    #[cfg(test)]
    pub fn eval(
        &self,
        mem: &mut Memory,
        ctx: IrValue,
        args: Vec<IrValue>,
    ) -> Option<IrValue> {
        eval::eval(
            &self.runtime.rt,
            &self.ir.functions,
            "main",
            mem,
            ctx,
            args,
        )
    }

    pub fn codegen(self) -> Blob<Ctx> {
        self.codegen_with_constants(&Default::default())
    }

    pub(crate) fn codegen_with_constants(
        self,
        constants: &codegen::ConstantImports,
    ) -> Blob<Ctx> {
        let module = codegen::codegen(
            self.runtime,
            self.main,
            self.main_scope,
            &self.ir.functions,
            &self.runtime_functions,
            self.label_store,
            self.type_info,
            constants,
        );
        Blob { module }
    }

    #[cfg(test)]
    pub(crate) fn codegen_with_memory_provider(
        self,
        memory_provider: Box<dyn cranelift_jit::JITMemoryProvider + Send>,
    ) -> Blob<Ctx> {
        let module = codegen::codegen_with_memory_provider(
            self.runtime,
            self.main,
            self.main_scope,
            &self.ir.functions,
            &self.runtime_functions,
            self.label_store,
            self.type_info,
            memory_provider,
        );
        Blob { module }
    }
}

impl<Ctx: OptCtx> Blob<Ctx> {
    #[cfg(feature = "repl")]
    pub(crate) fn shared_data(&self) -> codegen::SharedModuleData {
        self.module.shared_data()
    }

    /// Return an iterator with all the tests in this [`Blob`].
    pub fn get_tests(
        &mut self,
    ) -> impl Iterator<Item = codegen::testing::TestCase<Ctx>> + use<'_, Ctx>
    {
        codegen::testing::get_tests(&mut self.module)
    }

    /// Get a function from this [`Blob`].
    pub fn get_function<F: RotoFunc>(
        &mut self,
        name: &str,
    ) -> Result<TypedFunc<Ctx, F>, FunctionRetrievalError> {
        self.module.get_function(name)
    }

    /// Return an iterator with all matching functions in this [`Blob`].
    ///
    /// Returns an iterator of `(name, function)` tuples of all functions within
    /// the `Blob` that match the `F` [`RotoFunc`] signature.
    pub fn get_functions_by_signature<F: RotoFunc>(
        &mut self,
    ) -> impl Iterator<Item = (&str, TypedFunc<Ctx, F>)> {
        self.module.get_functions_by_signature()
    }
}

impl Blob<NoCtx> {
    /// Run all tests in this [`Blob`] without context.
    #[allow(clippy::result_unit_err)]
    pub fn run_tests(&mut self) -> Result<(), ()> {
        codegen::testing::run_tests(&mut self.module, NoCtx)
    }
}

impl<C: Context> Blob<Ctx<C>> {
    /// Run all tests in this [`Blob`] with context.
    #[allow(clippy::result_unit_err)]
    pub fn run_tests(&mut self, ctx: C) -> Result<(), ()> {
        codegen::testing::run_tests(&mut self.module, Ctx(ctx))
    }
}

#[cfg(all(test, not(miri)))]
mod report_tests {
    use crate::{
        Runtime,
        load::{FileSpec, FileTree, Load, SourceFile},
    };

    fn file(name: &str, module_name: &str, contents: &str) -> SourceFile {
        SourceFile {
            name: name.into(),
            module_name: module_name.into(),
            contents: contents.into(),
            location_offset: 0,
            children: Vec::new(),
        }
    }

    /// A label in another file than the error is placed using that file's
    /// text
    #[test]
    fn label_in_other_file() {
        let pkg = file(
            "pkg.roto",
            "pkg",
            "fn main() -> i32 {\n    foo.Point + 1\n}\n",
        );
        let foo = file(
            "foo.roto",
            "foo",
            "// A comment that pushes the declaration\n\
             // further into the file than pkg.roto is long.\n\
             record Point { x: i32, y: i32 }\n",
        );
        let tree = FileTree::file_spec(FileSpec::Directory(
            pkg,
            vec![FileSpec::File(foo)],
        ));

        let rt = Runtime::new();
        let parsed =
            tree.load().unwrap().resolve(&rt).unwrap().parse().unwrap();
        let Err(report) = parsed.typecheck(&rt) else {
            panic!("expected a type error");
        };

        let mut out = String::new();
        report.write(&mut out, false).unwrap();
        assert!(out.contains("foo.roto:3:8"), "{out}");
    }
}
