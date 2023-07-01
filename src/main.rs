use clap::{Parser, Subcommand};
use liblouis::{
    check_yaml::{check_yaml, TestResult},
    debug,
    parser::Line,
    translate,
};
use std::path::PathBuf;

#[derive(Debug, Subcommand)]
enum Commands {
    /// translate <INPUT> to or from braille using <TABLE>
    #[command(arg_required_else_help = true)]
    Translate {
        /// Braille table to use for the translation
        table: PathBuf,
        /// String to translate
        input: String,
    },
    /// Run the tests defined in the <YAML_TEST_FILE>. Return 0 if all
    /// tests pass or 1 if any of the tests fail.
    CheckYaml {
        /// YAML file listing all the tests
        yaml: PathBuf,
    },
    /// print debug information about the given table <TABLE>
    Debug {
        /// Braille table to debug
        table: PathBuf,
    },
}

#[derive(Debug, Parser)] // requires `derive` feature
#[command(name = "louis")]
#[command(about = "A command line tool to translate to and from Braille")]
#[command(author, version, long_about = None)] // Read from `Cargo.toml`
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

fn main() {
    let args = Cli::parse();

    match args.command {
        Commands::Translate { table, input } => {
            println!("translating {} using table {:?}", input, table);
            println!(
                "Braille: {}",
                translate(table, &input).expect("Translation failed")
            );
        }
        Commands::CheckYaml { yaml } => {
            println!("Testing with {:?}", yaml);
            let results = check_yaml(yaml);
            println!(
                "Pass: {}",
                results
                    .iter()
                    .filter(|r| **r == TestResult::Success)
                    .count()
            );
            println!(
                "Fail: {}",
                results.iter().filter(|r| r.is_failure()).count()
            );
        }
        Commands::Debug { table } => {
            println!("debugging table {:?}", table);
            let lines = debug(table).unwrap();
            for line in lines {
                match line {
                    Line::Empty => (),
                    Line::Comment { comment: _ } => (),
                    _ => println!("{:?}", line),
                }
            }
        }
    }
}
