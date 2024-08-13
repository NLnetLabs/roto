//! Compiler pipeline that executes multiple compiler stages in sequence

use std::collections::HashMap;

use string_interner::{backend::StringBackend, StringInterner};

use crate::{
    ast,
    codegen::{self, Module},
    lower::{
        self,
        eval::{self, Memory},
        ir::{self, IrPrinter},
        label::LabelStore,
        value::IrValue,
        IrFunction,
    },
    parser::{
        meta::{Span, Spans},
        ParseError, Parser,
    },
    runtime::Runtime,
    typechecker::{
        error::{Level, TypeError},
        info::TypeInfo,
        scope::ScopeGraph,
    },
};

/// A filename with its contents
#[derive(Clone, Debug)]
pub struct SourceFile {
    name: String,
    contents: String,
    /// The line offset that should be added to the location in error
    /// messages.
    ///
    /// This is used to add the offset of a string of source text in a test,
    /// so that Roto errors can refer to locations in Rust files accurately.
    location_offset: usize,
}

#[derive(Debug)]
enum RotoError {
    Read(String, std::io::Error),
    Parse(ParseError),
    Type(TypeError),
}

/// An error report containing a set of Roto errors
///
/// The errors can be printed with the regular [`std::fmt::Display`].
#[derive(Debug)]
pub struct RotoReport {
    files: Vec<SourceFile>,
    errors: Vec<RotoError>,
    spans: Spans,
}

/// Compiler stage: Files loaded and ready to be parsed
pub struct Files {
    files: Vec<SourceFile>,
}

/// Compiler stage: Files loaded and parsed
pub struct Parsed {
    /// Interned strings for identifiers and such
    identifiers: StringInterner<StringBackend>,
    files: Vec<SourceFile>,
    trees: Vec<ast::SyntaxTree>,
    spans: Spans,
}

/// Compiler stage: loaded, parsed and type checked
pub struct TypeChecked {
    runtime: Runtime,
    trees: Vec<ast::SyntaxTree>,
    type_infos: Vec<TypeInfo>,
    identifiers: StringInterner<StringBackend>,
    scope_graph: ScopeGraph,
}

/// Compiler stage: HIR
pub struct Lowered {
    runtime: Runtime,
    pub ir: Vec<ir::Function>,
    runtime_functions: HashMap<String, IrFunction>,
    identifiers: StringInterner<StringBackend>,
    label_store: LabelStore,
}

pub struct Compiled {
    pub runtime: Runtime,
    pub module: Module,
}

impl std::fmt::Display for RotoReport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
                Err(Box::new(format!("Failed to fetch source '{}'", id)) as _)
            }) as fn(&_) -> _,
        )
        .with_sources(sources);

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

                    let report = Report::build(
                        ReportKind::Error,
                        file,
                        error.location.start,
                    )
                    .with_message(format!("Parse error: {}", error))
                    .with_label(label)
                    .finish();

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

                    let report = Report::build(
                        ReportKind::Error,
                        file,
                        self.spans.get(error.location).start,
                    )
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

impl RotoReport {
    fn filename(&self, s: Span) -> String {
        self.files[s.file].name.clone()
    }
}

impl std::error::Error for RotoReport {}

#[macro_export]
macro_rules! src {
    ($code:literal) => {
        $crate::pipeline::test_file(file!(), $code, line!() as usize - 1)
    };
}

/// Compile and run a Roto script from a file
pub fn run(
    runtime: Runtime,
    files: impl IntoIterator<Item = String>,
    mem: &mut Memory,
    rx: Vec<IrValue>,
) -> Result<Option<IrValue>, RotoReport> {
    let pointer_bytes = usize::BITS / 8;

    let lowered = read_files(files)?
        .parse()?
        .typecheck(runtime, pointer_bytes)?
        .lower();

    let res = lowered.eval(mem, rx);
    Ok(res)
}

/// Create a test file to compile and run
pub fn test_file(file: &str, source: &str, location_offset: usize) -> Files {
    Files {
        files: vec![SourceFile {
            location_offset,
            name: file.into(),
            contents: source.into(),
        }],
    }
}

pub fn read_files<S>(
    files: impl IntoIterator<Item = S>,
) -> Result<Files, RotoReport>
where
    S: AsRef<str>,
{
    let results: Vec<_> = files
        .into_iter()
        .map(|f| {
            (f.as_ref().to_string(), std::fs::read_to_string(f.as_ref()))
        })
        .collect();

    let mut files = Vec::new();
    let mut errors = Vec::new();
    for (name, result) in results {
        match result {
            Ok(contents) => files.push(SourceFile {
                location_offset: 0,
                name,
                contents,
            }),
            Err(err) => {
                errors.push(RotoError::Read(name.clone(), err));
                files.push(SourceFile {
                    name,
                    location_offset: 0,
                    contents: String::new(),
                });
            }
        };
    }

    if errors.is_empty() {
        Ok(Files { files })
    } else {
        Err(RotoReport {
            files,
            errors,
            spans: Spans::default(),
        })
    }
}

impl Files {
    pub fn compile(self, runtime: Runtime, pointer_bytes: u32) -> Result<Compiled, RotoReport> {
        let x = self.parse()?;
        let x = x.typecheck(runtime, pointer_bytes)?;
        Ok(x.lower().codegen())
    }

    pub fn parse(self) -> Result<Parsed, RotoReport> {
        let mut spans = Spans::default();

        let mut identifiers = StringInterner::new();

        let results: Vec<_> = self
            .files
            .iter()
            .enumerate()
            .map(|(i, f)| {
                Parser::parse(i, &mut identifiers, &mut spans, &f.contents)
            })
            .collect();

        let mut trees = Vec::new();
        let mut errors = Vec::new();
        for result in results {
            match result {
                Ok(tree) => trees.push(tree),
                Err(err) => errors.push(RotoError::Parse(err)),
            };
        }

        if errors.is_empty() {
            Ok(Parsed {
                identifiers,
                trees,
                spans,
                files: self.files,
            })
        } else {
            Err(RotoReport {
                files: self.files.to_vec(),
                errors,
                spans,
            })
        }
    }
}

impl Parsed {
    pub fn typecheck(
        self,
        runtime: Runtime,
        pointer_bytes: u32,
    ) -> Result<TypeChecked, RotoReport> {
        let Parsed {
            mut identifiers,
            files,
            trees,
            spans,
        } = self;

        let mut scope_graph = ScopeGraph::new();

        let results: Vec<_> = trees
            .iter()
            .map(|f| {
                crate::typechecker::typecheck(
                    &runtime,
                    &mut identifiers,
                    &mut scope_graph,
                    f,
                    pointer_bytes,
                )
            })
            .collect();

        let mut type_infos = Vec::new();
        let mut errors = Vec::new();
        for result in results {
            match result {
                Ok(type_info) => {
                    type_infos.push(type_info);
                }
                Err(err) => errors.push(RotoError::Type(err)),
            }
        }

        if errors.is_empty() {
            Ok(TypeChecked {
                runtime,
                trees,
                type_infos,
                identifiers,
                scope_graph,
            })
        } else {
            Err(RotoReport {
                files: files.to_vec(),
                errors,
                spans,
            })
        }
    }
}

impl TypeChecked {
    pub fn lower(self) -> Lowered {
        let TypeChecked {
            runtime,
            trees,
            mut type_infos,
            mut identifiers,
            scope_graph,
        } = self;
        let mut runtime_functions = HashMap::new();
        let mut label_store = LabelStore::default();
        let ir = lower::lower(
            &trees[0],
            &mut type_infos[0],
            &mut runtime_functions,
            &mut identifiers,
            &mut label_store,
        );

        if log::log_enabled!(log::Level::Info) {
            let s = IrPrinter {
                scope_graph: &scope_graph,
                identifiers: &identifiers,
                label_store: &label_store,
            }
            .program(&ir);
            println!("{s}");
        }

        Lowered {
            ir,
            runtime,
            runtime_functions,
            identifiers,
            label_store,
        }
    }
}

impl Lowered {
    pub fn eval(
        &self,
        mem: &mut Memory,
        rx: Vec<IrValue>,
    ) -> Option<IrValue> {
        eval::eval(&self.ir, "main", mem, rx, &self.identifiers)
    }

    pub fn codegen(self) -> Compiled {
        let module = codegen::codegen(
            &self.ir,
            &self.runtime_functions,
            self.identifiers,
            self.label_store,
        );
        Compiled {
            runtime: self.runtime,
            module,
        }
    }
}
