use std::fs::File;
use std::io;
use std::io::BufRead;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process::exit;

use clap::{Parser, Subcommand};

mod parser;
use parser::RuleParser;
use parser::TableError;

use crate::parser::Direction;
use crate::translator::TranslationTable;

mod test;
mod translator;
mod yaml;

use yaml::YAMLParser;

#[derive(Debug, Subcommand)]
enum Commands {
    /// Parse and print debug information about the given table <TABLE>
    Parse {
        /// Braille table to parse. If no table is specified, a REPL
        /// is opened and each line you enter is parsed.
        table: Option<PathBuf>,
    },
    /// translate <INPUT> to braille using <TABLE>
    Translate {
        /// Braille table to use for the translation
        table: PathBuf,
        /// String to translate. If no input is specified, a REPL is
        /// opened and each line you enter is translated.
        input: Option<String>,
        #[arg(value_enum, short, long, default_value_t=Direction::Forward)]
        direction: Direction,
    },
    /// Test braille translations from given <YAML> file(s)
    Check {
        /// Only show a summary of the test results
        #[arg(short, long)]
        brief: bool,
        /// YAML document(s) that specify the tests
        #[arg(required = true)]
        yaml_files: Vec<PathBuf>,
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

fn print_errors(errors: Vec<TableError>) {
    for error in errors {
        match error {
            TableError::ParseError { path, line, error } => match path {
                Some(path) => eprintln!("{}:{}: {}", path.display(), line, error),
                None => eprintln!("{}: {}", line, error),
            },
            _ => eprintln!("{}", error),
        }
    }
}

fn parse(file: &Path) {
    match parser::table_expanded(file) {
        Ok(rules) => {
            for rule in rules {
                println!("{:?}", rule);
            }
        }
        Err(errors) => {
            print_errors(errors);
        }
    }
}

fn translate(table: &Path, direction: Direction, input: &str) {
    let rules = parser::table_file(table);
    match rules {
        Ok(rules) => {
            let table = TranslationTable::compile(rules, direction);
            println!("{}", table.translate(input));
        }
        Err(errors) => {
            print_errors(errors);
        }
    }
}

// we pass a closure to the repl function so that we can use it for
// both the parsing repl and the translation repl. In the case of the
// parsing repl it is an empty closure but the translation repl closes
// over the TranslationTable.
fn repl(handler: Box<dyn Fn(String)>) {
    print!("> ");
    io::stdout().flush().unwrap();

    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        match line {
            Ok(line) => handler(line),
            _ => exit(0),
        }
        print!("> ");
        io::stdout().flush().unwrap();
    }
}

fn parse_line(line: String) {
    if !line.starts_with('#') && !line.is_empty() {
        let rule = RuleParser::new(&line).rule();
        match rule {
            Ok(rule) => println!("{:?}", rule),
            Err(e) => eprintln!("{:?}", e),
        }
    }
}

fn percentage(n: usize, total: usize) -> String {
    let n = n as f32;
    let total = total as f32;
    let result = (n * 100.0) / total;
    format!("{:.0}%", result)
}

fn print_check_row(label: &str, occurences: usize, total: usize) {
    println!(
        "{} {} [{}]",
        occurences,
        label,
        percentage(occurences, total)
    );
}

fn check_yaml(files: Vec<PathBuf>, brief: bool) {
    let mut total = 0;
    let mut successes = 0;
    let mut failures = 0;
    let mut expected_failures = 0;
    let mut unexpected_successes = 0;
    for file in files {
        match File::open(file) {
            Ok(file) => match YAMLParser::new(file) {
                Ok(mut parser) => match parser.yaml() {
                    Ok(test_results) => {
                        total += test_results.len();
                        successes += test_results.iter().filter(|r| r.is_success()).count();
                        failures += test_results.iter().filter(|r| r.is_failure()).count();
                        expected_failures += test_results
                            .iter()
                            .filter(|r| r.is_expected_failure())
                            .count();
                        unexpected_successes += test_results
                            .iter()
                            .filter(|r| r.is_unexpected_success())
                            .count();
                        if !brief {
                            for res in test_results.iter().filter(|r| !r.is_success()) {
                                println!("{:?}", res);
                            }
                        }
                    }
                    Err(e) => {
                        eprintln!("Could not parse yaml or errors while testing: {:?}", e);
                    }
                },
                Err(e) => {
                    eprintln!("Could not create parser {:?}", e)
                }
            },
            Err(e) => {
                eprintln!("Could not open yaml {:?}", e)
            }
        }
    }
    println!("================================================================================");
    println!("{} tests run:", total);
    print_check_row("successes", successes, total);
    print_check_row("failures", failures, total);
    print_check_row("expected failures", expected_failures, total);
    print_check_row("unexpected successes", unexpected_successes, total);
}

fn main() {
    let args = Cli::parse();

    match args.command {
        Commands::Parse { table } => match table {
            Some(table) => parse(table.as_path()),
            None => repl(Box::new(parse_line)),
        },
        Commands::Translate {
            table,
            input,
            direction,
        } => match input {
            Some(input) => translate(&table, direction, &input),
            None => {
                let rules = parser::table_file(&table);
                match rules {
                    Ok(rules) => {
                        let table = TranslationTable::compile(rules, direction);
                        repl(Box::new(move |input| {
                            println!("{}", table.translate(&input))
                        }));
                    }
                    Err(errors) => {
                        print_errors(errors);
                    }
                }
            }
        },
        Commands::Check { yaml_files, brief } => check_yaml(yaml_files, brief),
    }
}
