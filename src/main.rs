use std::io::BufRead;
use std::io::Write;
use std::path::Path;
use std::process::exit;
use std::{env, io};

mod parser;
use parser::expand_includes;
use parser::table;
use parser::RuleParser;
use parser::TableError;
use search_path::SearchPath;

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

fn read_file(filename: &str) {
    let search_path = &SearchPath::new_or("LOUIS_TABLE_PATH", ".");
    match table(Path::new(filename)) {
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
    let args: Vec<String> = env::args().collect();

    let filename = &args.get(1);
    match filename {
        Some(filename) => read_file(filename),
        None => repl(),
    }
}
