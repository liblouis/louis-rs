use std::io;
use std::io::BufRead;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process::exit;

use clap::{Parser, Subcommand};

mod parser;
use parser::expand_includes;
use parser::Rule;
use parser::RuleParser;
use parser::TableError;
use search_path::SearchPath;

use crate::translator::TranslationTable;

mod translator;

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
        /// String to translate
        input: Option<String>,
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
            TableError::ParseError { file, line, error } => {
                eprintln!("{}:{}: {}", file, line, error)
            }
            _ => eprintln!("{}", error),
        }
    }
}

fn parse_file(file: &Path) -> Result<Vec<Rule>, Vec<TableError>> {
    let search_path = &SearchPath::new_or("LOUIS_TABLE_PATH", ".");
    let rules = parser::table(file)?;
    let rules = expand_includes(search_path, rules)?;
    Ok(rules)
}

fn parse(file: &Path) {
    match parse_file(file) {
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

fn translate(table: &Path, input: &str) {
    let rules = parser::table(table);
    match rules {
        Ok(rules) => {
            let table = TranslationTable::compile(&rules);
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

fn main() {
    let args = Cli::parse();

    match args.command {
        Commands::Parse { table } => match table {
            Some(table) => parse(table.as_path()),
            None => repl(Box::new(parse_line)),
        },
        Commands::Translate { table, input } => match input {
            Some(input) => translate(&table, &input),
            None => {
                let rules = parser::table(&table);
                match rules {
                    Ok(rules) => {
                        let table = TranslationTable::compile(&rules);
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
    }
}
