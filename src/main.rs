use clap::{Parser, Subcommand};
use liblouis::{
    check::{TestResult, TestSuite},
    compile, debug, translate,
    translator::Direction,
};
use std::{fs::File, io::BufReader, path::PathBuf};

#[derive(Debug, Subcommand)]
enum Commands {
    /// translate <INPUT> to or from braille using <TABLE>
    #[command(arg_required_else_help = true)]
    Translate {
        /// Braille table to use for the translation
        table: PathBuf,
        /// String to translate
        input: String,
        #[arg(long,short,value_enum,default_value_t=Direction::Forward)]
        direction: Direction,
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
        Commands::Translate {
            table,
            input,
            direction,
        } => {
            println!(
                "{:?} translating {} using table {:?}",
                direction, input, table
            );
            let table = compile(&table).expect("Cannot compile table");
            println!("Braille: {}", translate(&table, &input));
        }
        Commands::CheckYaml { yaml } => {
            println!("Testing with {:?}", yaml);
            let input = File::open(yaml).expect("could not read to file");
            let buffered = BufReader::new(input);
            let test_suite: TestSuite =
                serde_yaml::from_reader(buffered).expect("Cannot parse yaml");

            let results = test_suite.check();
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
            for r in results {
                match r {
                    TestResult::Success => (),
                    _ => println!("{:?}", r),
                }
            }
        }
        Commands::Debug { table } => {
            println!("debugging table {:?}", table);
            match debug(table) {
                Ok(rules) => {
                    for rule in rules {
                        println!("{:?}", rule);
                    }
                }
                Err(error) => println!("{:?}", error),
            }
        }
    }
}
