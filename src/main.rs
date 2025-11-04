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

mod metadata;
mod test;
mod translator;
mod yaml;

use tabled::{
    Table, Tabled,
    settings::{Border, Style, object::Rows},
};
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
        /// Direction of translation
        #[arg(value_enum, short, long, default_value_t=Direction::Forward)]
        direction: Direction,
    },
    /// Test braille translations from given <YAML> file(s)
    Check {
        /// Only show a summary of the test results
        #[arg(short, long)]
        summary: bool,
        /// YAML document(s) that specify the tests
        #[arg(required = true)]
        yaml_files: Vec<PathBuf>,
    },
    /// Find braille tables based on a metadata query. The tables are
    /// searched in the the search path `LOUIS_TABLE_PATH`
    Query {
        /// Metadata search query <key=value,...>
        query: String,
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
            match TranslationTable::compile(rules, direction) {
                Ok(table) => println!("{}", table.translate(input)),
                Err(e) => eprintln!("Could not compile table: {:?}", e),
            };
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

#[derive(Tabled, Default)]
#[tabled(rename_all = "PascalCase")]
struct YAMLTestResult {
    #[tabled(rename = "YAML File")]
    yaml_file: String,
    tests: usize,
    #[tabled(display_with("Self::display_successes", self))]
    successes: usize,
    #[tabled(display_with("Self::display_failures", self))]
    failures: usize,
    #[tabled(
        rename = "Expected\nFailures",
        display_with("Self::display_expected", self)
    )]
    expected_failures: usize,
    #[tabled(
        rename = "Unexpected\nSuccesses",
        display_with("Self::display_unexpected", self)
    )]
    unexpected_successes: usize,
}

fn percent(n: usize, total: usize) -> f64 {
    (n as f64 * 100.0) / total as f64
}

impl YAMLTestResult {
    fn display_successes(&self) -> String {
        format!("{:.1}%", percent(self.successes, self.tests))
    }
    fn display_failures(&self) -> String {
        format!("{:.1}%", percent(self.failures, self.tests))
    }
    fn display_expected(&self) -> String {
        format!("{:.1}%", percent(self.expected_failures, self.tests))
    }
    fn display_unexpected(&self) -> String {
        format!("{:.1}%", percent(self.unexpected_successes, self.tests))
    }
    fn update(
        &mut self,
        tests: usize,
        successes: usize,
        failures: usize,
        expected_failures: usize,
        unexpected_successes: usize,
    ) {
        self.tests += tests;
        self.successes += successes;
        self.failures += failures;
        self.expected_failures += expected_failures;
        self.unexpected_successes += unexpected_successes;
    }
}

fn check_yaml(paths: Vec<PathBuf>, summary: bool) {
    let mut total = YAMLTestResult::default();
    let mut yaml_results: Vec<YAMLTestResult> = Vec::new();
    for path in paths {
        match File::open(&path) {
            Ok(file) => match YAMLParser::new(file) {
                Ok(mut parser) => match parser.yaml() {
                    Ok(test_results) => {
                        let tests = test_results.len();
                        let successes = test_results.iter().filter(|r| r.is_success()).count();
                        let failures = test_results.iter().filter(|r| r.is_failure()).count();
                        let expected_failures = test_results
                            .iter()
                            .filter(|r| r.is_expected_failure())
                            .count();
                        let unexpected_successes = test_results
                            .iter()
                            .filter(|r| r.is_unexpected_success())
                            .count();
                        total.update(
                            tests,
                            successes,
                            failures,
                            expected_failures,
                            unexpected_successes,
                        );
                        yaml_results.push(YAMLTestResult {
                            yaml_file: path
                                .file_name()
                                .map_or("".to_string(), |f| f.to_string_lossy().into_owned()),
                            tests,
                            successes,
                            failures,
                            expected_failures,
                            unexpected_successes,
                        });
                        if !summary {
                            for res in test_results.iter().filter(|r| !r.is_success()) {
                                println!("{:?}", res);
                            }
                        }
                    }
                    Err(e) => {
                        eprintln!("{}: {}", path.display(), e);
                    }
                },
                Err(e) => {
                    eprintln!("Could not create parser {:?} ({:?})", path, e)
                }
            },
            Err(e) => {
                eprintln!("Could not open yaml file {:?} ({})", path, e)
            }
        }
    }
    if summary {
        yaml_results.sort_by_key(|r| r.tests);
        total.yaml_file = "Total".to_string();
        yaml_results.push(total);
        let mut table = Table::new(yaml_results);
        // add a special separator above the Total row
        table.modify(Rows::last(), Border::inherit(Style::ascii()).top('='));
        println!("{}", table);
    }
}

fn main() {
    env_logger::init();

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
                    Ok(rules) => match TranslationTable::compile(rules, direction) {
                        Ok(table) => repl(Box::new(move |input| {
                            println!("{}", table.translate(&input))
                        })),
                        Err(e) => eprintln!("Could not compile table: {:?}", e),
                    },
                    Err(errors) => {
                        print_errors(errors);
                    }
                }
            }
        },
        Commands::Check {
            yaml_files,
            summary,
        } => check_yaml(yaml_files, summary),
        Commands::Query { query } => match metadata::index() {
            Ok(index) => {
                let query = query
                    .split(',')
                    .map(|s| {
                        let parts: Vec<_> = s.split('=').collect();
                        (parts[0].to_string(), parts[1].to_string())
                    })
                    .collect();
                println!("{:?}", metadata::find(&index, query));
            }
            Err(e) => {
                eprint!("Could not index all tables: {:?}", e)
            }
        },
    }
}
