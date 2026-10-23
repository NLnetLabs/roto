use std::{fs::File, path::PathBuf, process::ExitCode};

use clap::{Parser, Subcommand};

use crate::{
    RotoError, RotoReport, Runtime, fmt::fmt_path, load::LoadError,
    runtime::context::OptCtx,
};

#[derive(Parser)]
#[command(version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Generate documentation for the runtime
    Doc {
        #[arg()]
        path: PathBuf,
    },
    /// Type check a script
    Check {
        #[arg()]
        file: PathBuf,
    },
    /// Test a script
    Test {
        #[arg()]
        file: PathBuf,
    },
    /// Run a script's function
    Run {
        #[arg()]
        file: PathBuf,
        #[arg(default_value = "main")]
        function: String,
    },
    /// Format a Roto file
    Fmt {
        #[arg(long = "check")]
        check: bool,
        #[arg()]
        file: PathBuf,
    },
}

/// Run a basic CLI for a given runtime
///
/// This is useful for providing to users to check their scripts or run their tests
/// with the runtime that the host application provides.
///
/// This CLI provides the following subcommands:
///
///  - `doc`: generate documentation
///  - `check`: type check a script
///  - `test`: run tests for a script
///  - `run`: run a function of a script
pub fn cli(rt: &Runtime<impl OptCtx>) -> ExitCode {
    match cli_inner(rt) {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            eprintln!("{err}");
            ExitCode::FAILURE
        }
    }
}

fn cli_inner(rt: &Runtime<impl OptCtx>) -> Result<(), RotoReport> {
    let cli = Cli::parse();

    match &cli.command {
        Command::Doc { path } => {
            rt.rt.print_documentation(path).unwrap();
        }
        Command::Check { file } => {
            rt.check(&**file)?;
            println!("All ok!")
        }
        Command::Test { file } => {
            let Some(rt) = rt.clone().try_without_ctx() else {
                eprintln!("Can only run tests on a Runtime without Context");
                return Err(RotoReport {
                    errors: vec![RotoError::TestsFailed()],
                    ..Default::default()
                });
            };

            let mut p = rt.compile(&**file)?;

            if let Err(()) = p.run_tests() {
                return Err(RotoReport {
                    errors: vec![RotoError::TestsFailed()],
                    ..Default::default()
                });
            }
        }
        Command::Run { file, function } => {
            let Some(rt) = rt.clone().try_without_ctx() else {
                return Err(RotoReport {
                    errors: vec![RotoError::Custom("Can only run a script with a Runtime without Context".into())],
                    ..Default::default()
                });
            };

            let mut p = rt.compile(&**file)?;
            let f =
                p.get_function::<fn()>(function).map_err(|e| RotoReport {
                    errors: vec![RotoError::CouldNotRetrieveFunction(e)],
                    ..Default::default()
                })?;

            f.call()
        }
        Command::Fmt { file, check } => {
            let formatted = fmt_path(file)?;

            if *check {
                let original = match std::fs::read_to_string(file) {
                    Ok(original) => original,
                    Err(err) => {
                        return Err(RotoReport {
                            files: Vec::new(),
                            errors: vec![RotoError::Read(LoadError {
                                path: file.to_string_lossy().to_string(),
                                err,
                            })],
                            spans: Default::default(),
                        });
                    }
                };
                let patch = diffy::create_patch(&original, &formatted);
                let f = diffy::PatchFormatter::new().with_color();
                print!("{}", f.fmt_patch(&patch));
            } else {
                use std::io::Write;
                let mut f = match File::create(file) {
                    Ok(f) => f,
                    Err(err) => {
                        return Err(RotoReport {
                            files: Vec::new(),
                            errors: vec![RotoError::Read(LoadError {
                                path: file.to_string_lossy().to_string(),
                                err,
                            })],
                            spans: Default::default(),
                        });
                    }
                };
                writeln!(f, "{}", formatted).unwrap();
            }
        }
    }
    Ok(())
}
