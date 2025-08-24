//! Compiler pipeline that executes multiple compiler stages in sequence

use std::{collections::HashMap, fmt};

use crate::{
    codegen::{
        self,
        check::{FunctionRetrievalError, RotoFunc},
        Module, TypedFunc,
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
        meta::{Span, Spans},
        ParseError,
    },
    runtime::{
        context::{Context, ContextDescription},
        Runtime, RuntimeFunctionRef,
    },
    typechecker::{
        error::{Level, TypeError},
        info::TypeInfo,
    },
};

#[cfg(feature = "logger")]
use crate::ir_printer::{IrPrinter, Printable};

#[cfg(feature = "logger")]
use log::info;

/// An error from a compilation of a Roto script.
#[derive(Debug)]
pub enum RotoError {
    Read(String, std::io::Error),
    Parse(ParseError),
    Type(TypeError),
}

/// An error report containing a set of Roto errors.
///
/// The errors can be printed with the regular [`std::fmt::Display`].
pub struct RotoReport {
    pub files: Vec<SourceFile>,
    pub errors: Vec<RotoError>,
    pub spans: Spans,
}

/// Compiler stage: loaded, parsed and type checked
pub struct TypeChecked<'r> {
    module_tree: ModuleTree,
    type_info: TypeInfo,
    runtime: &'r Runtime,
    context_type: ContextDescription,
}

/// Compiler stage: MIR
pub struct LoweredToMir<'r> {
    runtime: &'r Runtime,
    ir: mir::Mir,
    label_store: LabelStore,
    type_info: TypeInfo,
    context_type: ContextDescription,
}

/// Compiler stage: LIR
pub struct LoweredToLir<'r> {
    runtime: &'r Runtime,
    ir: lir::Lir,
    runtime_functions: HashMap<RuntimeFunctionRef, lir::Signature>,
    label_store: LabelStore,
    type_info: TypeInfo,
    context_type: ContextDescription,
}

/// The final compiled package of script.
///
/// Functions can be extracted from this package using [`Package::get_function`].
pub struct Package {
    module: Module,
}

impl RotoReport {
    pub fn write(&self, mut f: impl fmt::Write, color: bool) -> fmt::Result {
        use ariadne::{Color, Label, Report, ReportKind};

        let sources = self
            .files
            .iter()
            .map(|s| {
                (
                    s.name.clone(),
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
                    write!(f, "could not open `{name}`: {io}")?;
                }
                RotoError::Parse(error) => {
                    let label_message = error.kind.label();
                    let label = Label::new((
                        self.filename(error.location),
                        error.location.start..error.location.end,
                    ))
                    .with_message(label_message)
                    .with_color(Color::Red);

                    let file = self.filename(error.location);

                    let mut report = Report::build(
                        ReportKind::Error,
                        (file, error.location.start..error.location.end),
                    )
                    .with_config(config)
                    .with_message(format!("Parse error: {}", error))
                    .with_label(label);

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
                        let s = self.spans.get(l.id);
                        Label::new((self.filename(s), s.start..s.end))
                            .with_message(&l.message)
                            .with_color(match l.level {
                                Level::Error => Color::Red,
                                Level::Info => Color::Blue,
                            })
                    });

                    let file = self.filename(self.spans.get(error.location));

                    let span = self.spans.get(error.location);
                    let report = Report::build(
                        ReportKind::Error,
                        (file, span.start..span.end),
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
        self.files[s.file].name.clone()
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
    pub fn typecheck<'r>(
        self,
        runtime: &'r Runtime,
    ) -> Result<TypeChecked<'r>, RotoReport> {
        let Parsed {
            file_tree,
            module_tree,
            spans,
        } = self;

        let context_type =
            runtime.context().clone().unwrap_or_else(<()>::description);

        let result = crate::typechecker::typecheck(runtime, &module_tree);

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
            context_type,
        })
    }
}

impl<'r> TypeChecked<'r> {
    pub fn lower_to_mir(&self) -> LoweredToMir<'r> {
        let TypeChecked {
            module_tree,
            type_info,
            runtime,
            context_type,
        } = self;

        let mut type_info = type_info.clone();
        let mut label_store = LabelStore::default();
        let ir = mir::lower_to_mir(
            module_tree,
            runtime,
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
            context_type: context_type.clone(),
            type_info,
        }
    }
}

impl<'r> LoweredToMir<'r> {
    pub fn lower_to_lir(self) -> LoweredToLir<'r> {
        let LoweredToMir {
            runtime,
            ir,
            mut label_store,
            mut type_info,
            context_type,
        } = self;

        let mut runtime_functions = HashMap::new();
        let mut ctx = lir::lower::LowerCtx {
            runtime,
            type_info: &mut type_info,
            label_store: &mut label_store,
            runtime_functions: &mut runtime_functions,
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
            context_type,
            runtime_functions,
        }
    }
}

impl LoweredToLir<'_> {
    pub fn eval(
        &self,
        mem: &mut Memory,
        ctx: IrValue,
        args: Vec<IrValue>,
    ) -> Option<IrValue> {
        eval::eval(self.runtime, &self.ir.functions, "main", mem, ctx, args)
    }

    pub fn codegen(self) -> Package {
        let module = codegen::codegen(
            self.runtime,
            &self.ir.functions,
            &self.runtime_functions,
            self.label_store,
            self.type_info,
            self.context_type,
        );
        Package { module }
    }
}

impl Package {
    pub fn get_tests<Ctx: 'static>(
        &mut self,
    ) -> impl Iterator<Item = codegen::testing::TestCase<Ctx>> + use<'_, Ctx>
    {
        codegen::testing::get_tests(&mut self.module)
    }

    #[allow(clippy::result_unit_err)]
    pub fn run_tests<Ctx: 'static>(&mut self, ctx: Ctx) -> Result<(), ()> {
        codegen::testing::run_tests(&mut self.module, ctx)
    }

    pub fn get_function<Ctx: 'static, F: RotoFunc>(
        &mut self,
        name: &str,
    ) -> Result<TypedFunc<Ctx, F>, FunctionRetrievalError> {
        self.module.get_function(name)
    }
}
