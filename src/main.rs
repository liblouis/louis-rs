use clap::{Parser, Subcommand};
use liblouis_nom::translate;
use std::path::PathBuf;

#[derive(Debug, Subcommand)]
enum Commands {
    #[command(arg_required_else_help = true)]
    Translate {
	table: PathBuf,
	input: String,
    }
}

#[derive(Debug, Parser)] // requires `derive` feature
#[command(name = "louis")]
#[command(about = "A command line to translate to and from Braille", long_about = None)]
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
    }
}
