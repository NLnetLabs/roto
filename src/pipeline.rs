//! Compiler pipeline that executes multiple compiler stages in sequence

use crate::{
    ast,
    codegen::{self, Module},
    lower::{self, eval, ir, value::SafeValue},
    parser::{
        meta::{Span, Spans},
        ParseError, Parser,
    },
    runtime::Runtime,
    typechecker::{
        error::{Level, TypeError},
        TypeInfo,
    },
};

/// A filename with its contents
#[derive(Clone, Debug)]
pub struct SourceFile {
    name: String,
    contents: String,
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
pub struct LoadedFiles {
    files: Vec<SourceFile>,
}

/// Compiler stage: Files loaded and parsed
pub struct Parsed {
    files: Vec<SourceFile>,
    trees: Vec<ast::SyntaxTree>,
    spans: Spans,
}

/// Compiler stage: loaded, parsed and type checked
pub struct TypeChecked {
    runtime: Runtime,
    trees: Vec<ast::SyntaxTree>,
    type_infos: Vec<TypeInfo>,
}

/// Compiler stage: HIR
pub struct Lowered {
    runtime: Runtime,
    type_info: TypeInfo,
    pub ir: Vec<ir::Function>,
}

pub struct Compiled {
    pub runtime: Runtime,
    pub module: Module,
}

impl std::fmt::Display for RotoReport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ariadne::{Color, Label, Report, ReportKind};

        let mut file_cache = ariadne::sources(
            self.files
                .iter()
                .map(|s| (s.name.clone(), s.contents.clone())),
        );

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

/// Compile and run a Roto script from a file
pub fn run(
    files: impl IntoIterator<Item = String>,
    rx: SafeValue,
) -> Result<SafeValue, RotoReport> {
    let lowered = read_files(files)?.parse()?.typecheck()?.lower();
    for f in &lowered.ir {
        println!("{}", f);
    }

    let res = lowered.eval(rx);
    lowered.codegen();

    Ok(res)
}

/// Create a test file to compile and run
pub fn test_file(source: &str) -> LoadedFiles {
    LoadedFiles {
        files: vec![SourceFile {
            name: "test".into(),
            contents: source.into(),
        }],
    }
}

fn read_files(
    files: impl IntoIterator<Item = String>,
) -> Result<LoadedFiles, RotoReport> {
    let results: Vec<_> = files
        .into_iter()
        .map(|f| (f.to_string(), std::fs::read_to_string(f)))
        .collect();

    let mut files = Vec::new();
    let mut errors = Vec::new();
    for (name, result) in results {
        match result {
            Ok(contents) => files.push(SourceFile { name, contents }),
            Err(err) => {
                errors.push(RotoError::Read(name.clone(), err));
                files.push(SourceFile {
                    name,
                    contents: String::new(),
                });
            }
        };
    }

    if errors.is_empty() {
        Ok(LoadedFiles { files })
    } else {
        Err(RotoReport {
            files,
            errors,
            spans: Spans::default(),
        })
    }
}

impl LoadedFiles {
    pub fn parse(self) -> Result<Parsed, RotoReport> {
        let mut spans = Spans::default();

        let results: Vec<_> = self
            .files
            .iter()
            .enumerate()
            .map(|(i, f)| Parser::parse(i, &mut spans, &f.contents))
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
    pub fn typecheck(self) -> Result<TypeChecked, RotoReport> {
        let Parsed {
            files,
            trees,
            spans,
        } = self;

        let runtime = Runtime::default();

        let results: Vec<_> = trees
            .iter()
            .map(|f| crate::typechecker::typecheck(&runtime, f))
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
        } = self;
        let ir = lower::lower(&runtime, &trees[0], &mut type_infos[0]);
        Lowered {
            ir,
            runtime,
            type_info: type_infos.remove(0),
        }
    }
}

impl Lowered {
    pub fn eval(&self, rx: SafeValue) -> SafeValue {
        eval::eval(&self.ir, "main", vec![rx])
    }

    pub fn codegen(mut self) -> Compiled {
        let module = codegen::codegen(&self.ir, &mut self.type_info, &self.runtime);
        Compiled {
            runtime: self.runtime,
            module,
        }
    }
}
