use std::path::PathBuf;

use clap::{Parser, Subcommand};

use crate::{lsp, tools::print::print_highlighted, FileTree, Runtime};

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
    Doc,
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
    /// Run a script
    Run {
        #[arg()]
        file: PathBuf,
    },
    /// Print a Roto file with syntax highlighting
    Print {
        #[arg()]
        file: PathBuf,
    },
    /// Run an language server for Roto
    Lsp,
}

/// Run a basic CLI for a given runtime
///
/// This is useful for providing users to check their scripts or run their tests
/// with the runtime the host application provides.
///
/// This CLI provides the following subcommands:
///  - `doc`: generate documentation
///  - `check`: type check a script
///  - `test`: run tests for a script
///  - `run`: run the main function of a script
pub fn cli(rt: &Runtime) {
    match cli_inner(rt) {
        Ok(()) => std::process::exit(0),
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    }
}

fn cli_inner(rt: &Runtime) -> Result<(), String> {
    let cli = Cli::parse();

    match &cli.command {
        Command::Doc => {
            rt.print_documentation();
        }
        Command::Check { file } => {
            FileTree::read(file)
                .parse()
                .map_err(|r| r.to_string())?
                .typecheck(rt)
                .map_err(|r| r.to_string())?;
            println!("All ok!")
        }
        Command::Test { file } => {
            let mut p = FileTree::read(file)
                .parse()
                .map_err(|r| r.to_string())?
                .typecheck(rt)
                .map_err(|r| r.to_string())?
                .lower_to_mir()
                .lower_to_lir()
                .codegen();

            if let Err(()) = p.run_tests(()) {
                return Err("tests failed".into());
            }
        }
        Command::Run { file } => {
            let mut p = FileTree::read(file)
                .parse()
                .map_err(|r| r.to_string())?
                .typecheck(rt)
                .map_err(|r| r.to_string())?
                .lower_to_mir()
                .lower_to_lir()
                .codegen();

            let f = p
                .get_function::<(), fn()>("main")
                .map_err(|e| e.to_string())?;

            f.call(&mut ())
        }
        Command::Print { file } => {
            let s = std::fs::read_to_string(file).unwrap();
            print_highlighted(&s);
        }
        Command::Lsp => lsp::run(),
    }
    Ok(())
}
