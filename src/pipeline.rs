//! Compiler pipeline that executes multiple compiler stages in sequence

use std::{collections::HashMap, fmt, path::Path};

use log::info;

use crate::{
    codegen::{
        self,
        check::{FunctionRetrievalError, RotoFunc},
        Module, TypedFunc,
    },
    file_tree::SourceFile,
    ir_printer::{IrPrinter, Printable},
    label::LabelStore,
    lir::{
        self,
        eval::{self, Memory},
        value::IrValue,
        IrFunction,
    },
    mir,
    module::{ModuleTree, Parsed},
    parser::{
        meta::{Span, Spans},
        ParseError,
    },
    runtime::{
        context::{Context, ContextDescription},
        Runtime, RuntimeConstant, RuntimeFunctionRef,
    },
    typechecker::{
        error::{Level, TypeError},
        info::TypeInfo,
    },
    FileTree,
};

#[derive(Debug)]
pub enum RotoError {
    Read(String, std::io::Error),
    Parse(ParseError),
    Type(TypeError),
}

/// An error report containing a set of Roto errors
///
/// The errors can be printed with the regular [`std::fmt::Display`].
#[derive(Debug)]
pub struct RotoReport {
    pub files: Vec<SourceFile>,
    pub errors: Vec<RotoError>,
    pub spans: Spans,
}

/// Compiler stage: loaded, parsed and type checked
pub struct TypeChecked {
    module_tree: ModuleTree,
    type_info: TypeInfo,
    runtime: Runtime,
    context_type: ContextDescription,
}

/// Compiler stage: MIR
#[allow(dead_code)]
pub struct LoweredToMir {
    runtime: Runtime,
    pub ir: mir::ir::Mir,
    label_store: LabelStore,
    type_info: TypeInfo,
    context_type: ContextDescription,
}

/// Compiler stage: LIR
pub struct LoweredToLir {
    runtime: Runtime,
    pub ir: lir::ir::Lir,
    runtime_functions: HashMap<RuntimeFunctionRef, IrFunction>,
    runtime_constants: Vec<RuntimeConstant>,
    label_store: LabelStore,
    type_info: TypeInfo,
    context_type: ContextDescription,
}

pub struct Compiled {
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

impl RotoReport {
    fn filename(&self, s: Span) -> String {
        self.files[s.file].name.clone()
    }
}

impl std::error::Error for RotoReport {}

#[macro_export]
macro_rules! src {
    ($code:literal) => {
        $crate::FileTree::test_file(file!(), $code, line!() as usize - 1)
    };
}

#[macro_export]
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

/// Compile and run a Roto script from a file
pub fn interpret(
    runtime: Runtime,
    path: &Path,
    mem: &mut Memory,
    ctx: IrValue,
    args: Vec<IrValue>,
) -> Result<Option<IrValue>, RotoReport> {
    let lowered = FileTree::read(path)
        .parse()?
        .typecheck(runtime)?
        .lower_to_lir();

    let res = lowered.eval(mem, ctx, args);
    Ok(res)
}

impl Parsed {
    pub fn typecheck(
        self,
        runtime: Runtime,
    ) -> Result<TypeChecked, RotoReport> {
        let Parsed {
            file_tree,
            module_tree,
            spans,
        } = self;

        let context_type =
            runtime.context.clone().unwrap_or_else(<()>::description);

        let result = crate::typechecker::typecheck(&runtime, &module_tree);

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

impl TypeChecked {
    pub fn lower_to_mir(&self) -> LoweredToMir {
        let TypeChecked {
            module_tree,
            type_info,
            runtime,
            context_type,
        } = self;

        let mut type_info = type_info.clone();
        let mut label_store = LabelStore::default();
        let ir =
            mir::lower_to_mir(&module_tree, &mut type_info, &mut label_store);

        if log::log_enabled!(log::Level::Info) {
            let printer = IrPrinter {
                type_info: &type_info,
                label_store: &label_store,
                scope: None,
            };
            let s = ir.print(&printer);
            info!("\n{s}");
        }

        LoweredToMir {
            ir,
            runtime: runtime.clone(),
            label_store,
            context_type: context_type.clone(),
            type_info,
        }
    }

    pub fn lower_to_lir(self) -> LoweredToLir {
        let TypeChecked {
            module_tree,
            mut type_info,
            runtime,
            context_type,
        } = self;
        let mut runtime_functions = HashMap::new();
        let mut label_store = LabelStore::default();
        let ir = lir::lower(
            &module_tree,
            &mut type_info,
            &mut runtime_functions,
            &mut label_store,
            &runtime,
        );

        let _ = env_logger::try_init();
        if log::log_enabled!(log::Level::Info) {
            let printer = IrPrinter {
                type_info: &type_info,
                label_store: &label_store,
                scope: None,
            };
            let s = ir.print(&printer);
            info!("{s}");
        }

        let runtime_constants = runtime.constants.values().cloned().collect();

        LoweredToLir {
            ir,
            runtime,
            runtime_functions,
            runtime_constants,
            label_store,
            context_type,
            type_info,
        }
    }
}

impl LoweredToLir {
    pub fn eval(
        &self,
        mem: &mut Memory,
        ctx: IrValue,
        args: Vec<IrValue>,
    ) -> Option<IrValue> {
        eval::eval(
            &self.runtime,
            &self.ir.functions,
            "main",
            mem,
            &self.runtime_constants,
            ctx,
            args,
        )
    }

    pub fn codegen(self) -> Compiled {
        let module = codegen::codegen(
            &self.runtime,
            &self.ir.functions,
            &self.runtime_functions,
            &self.runtime_constants,
            self.label_store,
            self.type_info,
            self.context_type,
        );
        Compiled { module }
    }
}

impl Compiled {
    #[allow(clippy::result_unit_err)]
    pub fn run_tests<Ctx: 'static>(&mut self, ctx: Ctx) -> Result<(), ()> {
        self.module.run_tests(ctx)
    }

    pub fn get_function<Ctx: 'static, F: RotoFunc>(
        &mut self,
        name: &str,
    ) -> Result<TypedFunc<Ctx, F>, FunctionRetrievalError> {
        self.module.get_function(name)
    }
}
