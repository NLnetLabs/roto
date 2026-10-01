use std::{path::PathBuf, process::ExitCode};

use clap::{Parser, Subcommand};
use roto::{RotoReport, Runtime, module::Parsed};

const ENTRY_POINT: &str = "./yang-models";

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
        #[arg(default_value = "rotonda-conf.yang")]
        entry_point_path: PathBuf,
        #[arg(default_value = "./yang-models")]
        lib_path: PathBuf,
    },
    /// Print a Roto file with syntax highlighting
    Print {
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
pub fn main() -> ExitCode {
    match cli_inner() {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            eprintln!("{err}");
            ExitCode::FAILURE
        }
    }
}

fn cli_inner() -> Result<(), RotoReport> {
    let cli = Cli::parse();

    match &cli.command {
        Command::Doc { path } => {
            // rt.rt.print_documentation(path).unwrap();
            todo!()
        }
        Command::Check { file } => {
            let rt = Runtime::new();
            // YangFiles::read(file)?.parse()?.typecheck(&rt)?;
            println!("All ok!")
        }
        Command::Test { file } => {
            // let Some(rt) = rt.clone().try_without_ctx() else {
            //     eprintln!("Can only run tests on a Runtime without Context");
            //     return Err(RotoReport {
            //         errors: vec![RotoError::TestsFailed()],
            //         ..Default::default()
            //     });
            // };

            // let mut pi = FileTree::read(file)?.parse()?;
            // println!("syntax tree {:#?}", pi.module_tree.modules[0].ast);
            // let mut p =
            //     pi.typecheck(&rt)?.lower_to_mir().lower_to_lir().codegen();

            // if let Err(()) = p.run_tests() {
            //     return Err(RotoReport {
            //         errors: vec![RotoError::TestsFailed()],
            //         ..Default::default()
            //     });
            // }
            todo!()
        }
        Command::Run {
            entry_point_path,
            lib_path,
        } => {
            // let source_file = SourceFile::read(entry_point_path.as_ref())?;
            // let lib = YangFiles::yang_module_files(lib_path, "yang")?;
            let parsed =
                Parsed::from_entry_point(entry_point_path, lib_path)?;
            let rt = Runtime::new();
            let t_checked = parsed.typecheck(&rt).unwrap();
            println!(
                "[declarations] {:#?}",
                t_checked
                    .type_info
                    .scope_graph
                    .declarations
                    .iter()
                    .map(|dec| (dec.0.ident, &dec.1.kind, &dec.1.doc))
                    .collect::<Vec<_>>()
            );
            // t_checked.lower_to_mir();

            let modules = &t_checked.module_tree.modules;
            println!(
                "module tree {:#?}",
                modules.iter().map(|m| m.ident.clone()).collect::<Vec<_>>()
            );

            println!(
                "resolved name {:#?}",
                t_checked.type_info.resolved_names
            );
        }
        Command::Print { file } => {
            // let s = std::fs::read_to_string(file).unwrap();
            // print_highlighted(&s);
            todo!()
        }
    }
    Ok(())
}

// pub fn main() {
//     // let tree = FileTree::read_yang(DIR_PATH).unwrap();
//     // println!(
//     //     "file tree {:#?}",
//     //     tree.files.iter().map(|t| &t.name).collect::<Vec<_>>()
//     // );
//     let source_file = SourceFile::read(entry_point.as_ref())?;
//     let parsed = Parsed::from_entry_point();
//     // println!("modules {:#?}", parsed.module_tree.modules);

//     // let modules = &parsed.module_tree.modules;
//     let rt = Runtime::new();
//     let t_checked = parsed.typecheck(&rt).unwrap();
//     println!(
//         "[declarations] {:#?}",
//         t_checked
//             .type_info
//             .scope_graph
//             .declarations
//             .iter()
//             .map(|dec| (dec.0.ident, &dec.1.kind, &dec.1.doc))
//             .collect::<Vec<_>>()
//     );
//     // t_checked.lower_to_mir();

//     let modules = &t_checked.module_tree.modules;
//     println!(
//         "module tree {:#?}",
//         modules.iter().map(|m| m.ident.clone()).collect::<Vec<_>>()
//     );

//     println!("resolved name {:#?}", t_checked.type_info.resolved_names);
// }
