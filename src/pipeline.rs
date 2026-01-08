//! Compiler pipeline that executes multiple compiler stages in sequence

use std::{
    collections::{HashMap, VecDeque},
    fmt,
};

use crate::{
    codegen::{
        self, Module, TypedFunc,
        check::{FunctionRetrievalError, RotoFunc},
    },
    file_tree::SourceFile,
    label::LabelStore,
    lir::{
        self,
        eval::{self, Memory},
        value::IrValue,
    },
    mir,
    module::{ModuleTree, Parsed},
    parser::{
        ParseError,
        meta::{Span, Spans},
    },
    runtime::{
        Ctx, NoCtx, OptCtx, Runtime, RuntimeFunctionRef, context::Context,
    },
    typechecker::{
        error::{Level, TypeError},
        info::TypeInfo,
    },
};

use ariadne::Cache;

#[cfg(feature = "logger")]
use crate::ir_printer::{IrPrinter, Printable};

#[cfg(feature = "logger")]
use log::info;

/// An error from a compilation of a Roto script.
#[derive(Debug)]
pub(crate) enum RotoError {
    Read(String, std::io::Error),
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
    pub files: Vec<SourceFile>,
    pub(crate) errors: Vec<RotoError>,
    pub spans: Spans,
}

/// Compiler stage: loaded, parsed and type checked
pub struct TypeChecked<'r, Ctx: OptCtx> {
    module_tree: ModuleTree,
    type_info: TypeInfo,
    runtime: &'r Runtime<Ctx>,
}

/// Compiler stage: MIR
pub struct LoweredToMir<'r, Ctx: OptCtx> {
    runtime: &'r Runtime<Ctx>,
    ir: mir::Mir,
    label_store: LabelStore,
    type_info: TypeInfo,
}

/// Compiler stage: LIR
pub struct LoweredToLir<'r, Ctx: OptCtx> {
    runtime: &'r Runtime<Ctx>,
    ir: lir::Lir,
    runtime_functions: HashMap<RuntimeFunctionRef, lir::Signature>,
    label_store: LabelStore,
    type_info: TypeInfo,
}

/// The final compiled package of script.
///
/// Functions can be extracted from this package using [`Package::get_function`].
pub struct Package<Ctx: OptCtx> {
    module: Module<Ctx>,
}

impl RotoReport {
    pub fn write(&self, mut f: impl fmt::Write, color: bool) -> fmt::Result {
        use ariadne::{Color, Label, Report, ReportKind};

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

        let config = ariadne::Config::new().with_color(color);

        for error in &self.errors {
            match error {
                RotoError::Read(name, io) => {
                    write!(f, "Could not read file `{name}`: {io}")?;
                }
                RotoError::Parse(error) => {
                    let file = self.filename(error.location);
                    let file_text = file_cache.fetch(&file).unwrap().text();

                    let label_message = error.kind.label();
                    let label = Label::new((
                        self.filename(error.location),
                        error.location.character_range(file_text),
                    ))
                    .with_message(label_message)
                    .with_color(Color::Red);

                    let mut report = Report::build(
                        ReportKind::Error,
                        (file, error.location.character_range(file_text)),
                    )
                    .with_config(config)
                    .with_message(format!("Parse error: {}", error))
                    .with_label(label);

                    if let Some(hint) = error.kind.hint() {
                        report = report.with_help(hint);
                    }

                    for hint in &error.hints {
                        let label = Label::new((
                            self.filename(hint.location),
                            hint.location.start..hint.location.end,
                        ))
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
                    let file = self.filename(self.spans.get(error.location));
                    let file_text = file_cache.fetch(&file).unwrap().text();

                    let labels = error.labels.iter().map(|l| {
                        let s = self.spans.get(l.id);
                        Label::new((
                            self.filename(s),
                            s.character_range(file_text),
                        ))
                        .with_message(&l.message)
                        .with_color(match l.level {
                            Level::Error => Color::Red,
                            Level::Info => Color::Blue,
                        })
                    });

                    let span = self.spans.get(error.location);
                    let report = Report::build(
                        ReportKind::Error,
                        (file, span.character_range(file_text)),
                    )
                    .with_config(config)
                    .with_message(format!(
                        "Type error: {}",
                        &error.description
                    ))
                    .with_labels(labels)
                    .finish();

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
    fn filename(&self, s: Span) -> String {
        self.files[s.file].name()
    }
}

impl std::error::Error for RotoReport {}

#[cfg(test)]
macro_rules! src {
    ($code:literal) => {
        $crate::FileTree::test_file(file!(), $code, line!() as usize - 1)
    };
}

#[cfg(test)]
pub(crate) use src;

#[cfg(test)]
macro_rules! source_file {
    ($module_name:literal, $code:literal) => {
        $crate::SourceFile {
            name: file!().into(),
            module_name: $module_name.into(),
            contents: $code.into(),
            location_offset: line!() as usize - 1,
            children: Vec::new(),
        }
    };
}

#[cfg(test)]
pub(crate) use source_file;

impl Parsed {
    pub fn typecheck<'r, Ctx: OptCtx>(
        self,
        runtime: &'r Runtime<Ctx>,
    ) -> Result<TypeChecked<'r, Ctx>, RotoReport> {
        let Parsed {
            file_tree,
            module_tree,
            spans,
        } = self;

        let result = crate::typechecker::typecheck(&runtime.rt, &module_tree);

        let type_info = match result {
            Ok(type_info) => type_info,
            Err(error) => {
                return Err(RotoReport {
                    files: file_tree.files,
                    errors: vec![RotoError::Type(error)],
                    spans,
                });
            }
        };

        Ok(TypeChecked {
            module_tree,
            type_info,
            runtime,
        })
    }
}

impl<'r, Ctx: OptCtx> TypeChecked<'r, Ctx> {
    pub fn lower_to_mir(&self) -> LoweredToMir<'r, Ctx> {
        let TypeChecked {
            module_tree,
            type_info,
            runtime,
        } = self;

        let mut type_info = type_info.clone();
        let mut label_store = LabelStore::default();
        let ir = mir::lower_to_mir(
            module_tree,
            &runtime.rt,
            &mut type_info,
            &mut label_store,
        );

        #[cfg(feature = "logger")]
        {
            if log::log_enabled!(log::Level::Info) {
                let printer = IrPrinter {
                    type_info: &type_info,
                    label_store: &label_store,
                    scope: None,
                };
                let s = ir.print(&printer);
                info!("\n{s}");
            }
        }

        LoweredToMir {
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
            ir,
            mut label_store,
            mut type_info,
        } = self;

        let mut runtime_functions = HashMap::new();
        let mut ctx = lir::lower::LowerCtx {
            runtime: &runtime.rt,
            type_info: &mut type_info,
            label_store: &mut label_store,
            runtime_functions: &mut runtime_functions,
            drops_to_generate: VecDeque::new(),
            clones_to_generate: VecDeque::new(),
        };
        let ir = lir::lower_to_lir(&mut ctx, ir);

        #[cfg(feature = "logger")]
        {
            if log::log_enabled!(log::Level::Info) {
                let printer = IrPrinter {
                    type_info: &type_info,
                    label_store: &label_store,
                    scope: None,
                };
                let s = ir.print(&printer);
                info!("\n{s}");
            }
        }

        LoweredToLir {
            runtime,
            ir,
            label_store,
            type_info,
            runtime_functions,
        }
    }
}

impl<Ctx: OptCtx> LoweredToLir<'_, Ctx> {
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

    pub fn codegen(self) -> Package<Ctx> {
        let module = codegen::codegen(
            self.runtime,
            &self.ir.functions,
            &self.runtime_functions,
            self.label_store,
            self.type_info,
        );
        Package { module }
    }
}

impl<Ctx: OptCtx> Package<Ctx> {
    pub fn get_tests(
        &mut self,
    ) -> impl Iterator<Item = codegen::testing::TestCase<Ctx>> + use<'_, Ctx>
    {
        codegen::testing::get_tests(&mut self.module)
    }

    pub fn get_function<F: RotoFunc>(
        &mut self,
        name: &str,
    ) -> Result<TypedFunc<Ctx, F>, FunctionRetrievalError> {
        self.module.get_function(name)
    }
}

impl Package<NoCtx> {
    #[allow(clippy::result_unit_err)]
    pub fn run_tests(&mut self) -> Result<(), ()> {
        codegen::testing::run_tests(&mut self.module, NoCtx)
    }
}

impl<C: Context> Package<Ctx<C>> {
    #[allow(clippy::result_unit_err)]
    pub fn run_tests(&mut self, ctx: C) -> Result<(), ()> {
        codegen::testing::run_tests(&mut self.module, Ctx(ctx))
    }
}
