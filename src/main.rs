use clap::{Parser, Subcommand};
use liblouis_nom::{translate, debug, parser::Line};
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
    /// print debug information about the given table <TABLE>
    Debug {
	/// Braille table to debug
	table: PathBuf,
    }
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
	    println!("Braille: {}", translate(table, &input).expect("Translation failed"));
        }
	Commands::Debug { table } => {
	    println!("debugging table {:?}", table);
	    let lines = debug(table).unwrap();
	    for line in lines {
		match line {
		    Line::Empty => (),
		    Line::Comment { comment: _ } => (),
		    _ => println!("{:?}", line)
		}
	    }
	}
    }
}
