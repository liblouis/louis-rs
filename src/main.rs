use std::io::BufRead;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process::exit;
use std::io;

use clap::{Parser, Subcommand};

mod parser;
use parser::expand_includes;
use parser::table;
use parser::RuleParser;
use parser::TableError;
use search_path::SearchPath;

#[derive(Debug, Subcommand)]
enum Commands {
    /// Parse and print debug information about the given table <TABLE>
    Parse {
        /// Braille table to parse. If no table is specified, a REPL
        /// is opened and each line you enter is parsed.
        table: Option<PathBuf>,
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

fn read_file(file: &Path) {
    let search_path = &SearchPath::new_or("LOUIS_TABLE_PATH", ".");
    match table(file) {
        Ok(rules) => match expand_includes(search_path, rules) {
            Ok(rules) => {
                for rule in rules {
                    println!("{:?}", rule);
                }
            }
            Err(errors) => {
		print_errors(errors);
            }
        },
        Err(errors) => {
	    print_errors(errors);
        }
    }
}

fn repl() {
    print!("> ");
    io::stdout().flush().unwrap();

    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        match line {
            Ok(line) => {
                if !line.starts_with('#') && !line.is_empty() {
                    let rule = RuleParser::new(&line).rule();
                    match rule {
                        Ok(rule) => {
                            println!("{:?}", rule)
                        }
                        Err(e) => {
                            eprintln!("{:?}", e);
                        }
                    }
                    print!("> ");
                    io::stdout().flush().unwrap();
                }
            }
            _ => exit(0),
        }
    }
}

fn main() {
    let args = Cli::parse();

    match args.command {
	Commands::Parse { table } => match table {
	    Some(table) => read_file(table.as_path()),
	    None => repl(),
	},
    }
}
