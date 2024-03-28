use crate::{
    ast::SyntaxTree,
    parser::{ParseError, Parser},
    // typechecker::error::{Level, TypeError},
};

#[derive(Clone, Debug)]
pub struct SourceFile {
    name: String,
    contents: String,
}

#[derive(Debug)]
enum RotoError {
    Read(String, std::io::Error),
    Parse(ParseError),
    // Type(TypeError),
}

#[derive(Debug)]
pub struct RotoReport {
    files: Vec<SourceFile>,
    errors: Vec<RotoError>,
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
                        self.filename(error.location.file),
                        error.location.start..error.location.end,
                    ))
                    .with_message(label_message)
                    .with_color(Color::Red);

                    let file = self.filename(error.location.file);

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
                } // RotoError::Type(error) => {
                  //     let labels = error.labels.iter().map(|l| {
                  //         Label::new((
                  //             self.filename(l.span.file),
                  //             l.span.start..l.span.end,
                  //         ))
                  //         .with_message(&l.message)
                  //         .with_color(match l.level {
                  //             Level::Error => Color::Red,
                  //             Level::Info => Color::Blue,
                  //         })
                  //     });

                  //     let file = self.filename(error.location.file);

                  //     let report = Report::build(
                  //         ReportKind::Error,
                  //         file,
                  //         error.location.start,
                  //     )
                  //     .with_message(format!(
                  //         "Type error: {}",
                  //         &error.description
                  //     ))
                  //     .with_labels(labels)
                  //     .finish();

                  //     let mut v = Vec::new();
                  //     report.write(&mut file_cache, &mut v).unwrap();
                  //     let s = String::from_utf8_lossy(&v);
                  //     write!(f, "{s}")?;
                  // }
            }
        }

        Ok(())
    }
}

impl RotoReport {
    fn filename(&self, i: usize) -> String {
        self.files[i].name.clone()
    }
}

impl std::error::Error for RotoReport {}

pub fn run(
    files: impl IntoIterator<Item = String>,
) -> Result<(), RotoReport> {
    let files = read_files(files)?;
    let _trees = parse(&files)?;
    // typecheck(&files, &trees)?;
    Ok(())
}

pub fn test_file(source: &str) -> Vec<SourceFile> {
    vec![SourceFile {
        name: "test".into(),
        contents: source.into(),
    }]
}

// pub fn run_test(
//     source: &str,
//     arguments: Option<(&Scope, Vec<(&str, TypeValue)>)>,
// ) -> Result<Rotolo, RotoReport> {
//     let files = test_file(source);
//     let trees = parse(&files)?;
//     typecheck(&files, &trees)?;
//     let symbols = evaluate(&files, &trees)?;
//     Ok(compile(&files, &symbols, arguments)?.remove(0))
// }

fn read_files(
    files: impl IntoIterator<Item = String>,
) -> Result<Vec<SourceFile>, RotoReport> {
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
        Ok(files)
    } else {
        Err(RotoReport { files, errors })
    }
}

pub fn parse(files: &[SourceFile]) -> Result<Vec<SyntaxTree>, RotoReport> {
    let results: Vec<_> = files
        .iter()
        .enumerate()
        .map(|(i, f)| Parser::parse(i, &f.contents))
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
        Ok(trees)
    } else {
        Err(RotoReport {
            files: files.to_vec(),
            errors,
        })
    }
}

// pub fn typecheck(
//     files: &[SourceFile],
//     trees: &[SyntaxTree],
// ) -> Result<(), RotoReport> {
//     let results: Vec<_> = trees
//         .into_iter()
//         .map(|f| crate::typechecker::typecheck(&f))
//         .collect();

//     let mut errors = Vec::new();
//     for result in results {
//         if let Err(err) = result {
//             errors.push(RotoError::Type(err));
//         }
//     }

//     if errors.is_empty() {
//         Ok(())
//     } else {
//         Err(RotoReport {
//             files: files.to_vec(),
//             errors,
//         })
//     }
// }
