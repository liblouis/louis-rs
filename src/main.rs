use std::io::BufRead;
use std::io::Write;
use std::process::exit;
use std::{env, fs::read_to_string, io};

mod parser;
use parser::RuleParser;

fn read_file(filename: &str) {
    for (line_no, line) in read_to_string(filename).unwrap().lines().enumerate() {
        // println!("{}", line);
        if !line.trim().starts_with('#') && !line.trim().is_empty() {
            let rule = RuleParser::new(line).rule();
            match rule {
                Ok(rule) => {
                    println!("{:?}", rule)
                }
                Err(e) => {
                    eprintln!("{}:{}: {:?}", filename, line_no + 1, e);
                }
            }
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
