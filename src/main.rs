use std::fs::File;
use std::io;
use std::io::BufRead;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process::exit;

use clap::{Parser, Subcommand};
use rayon::prelude::*;

mod parser;
use parser::RuleParser;
use parser::TableError;

use crate::parser::AnchoredRule;
use crate::parser::Direction;
use crate::translator::ResolvedTranslation;
use crate::translator::TranslationPipeline;
use crate::translator::TranslationStage;

mod bundle;
pub mod emphasis;
mod hyphenation;
mod metadata;
mod test;
mod translator;
mod yaml;

use tabled::{
    Table, Tabled,
    settings::{Border, Style, object::Rows},
};
use yaml::YAMLParser;

#[derive(Debug, Clone, clap::ValueEnum)]
enum TraceStyle {
    Blank,
    Table,
}

/// The commands available in the command line tool.
#[derive(Debug, Subcommand)]
enum Commands {
    /// Parse and print debug information about the given `table`.
    Parse {
        /// Braille table to parse. If no table is specified, a REPL
        /// is opened and each line you enter is parsed.
        table: Option<PathBuf>,
    },
    /// Translate `input` using the specified braille `table`.
    ///
    /// If `direction` is not specified the `input` is translated to braille, otherwise the `input`
    /// is translated from braille to text.
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
    /// List all the rules that were applied for the translation of `input` using the specified
    /// braille `table`.
    ///
    /// If `direction` is not specified the `input` is translated to braille, otherwise the `input`
    /// is translated from braille to text.
    Trace {
        /// Braille table to use for the translation
        table: PathBuf,
        /// String to translate. If no input is specified, a REPL is
        /// opened and each line you enter is translated.
        input: Option<String>,
        /// Direction of translation
        #[arg(value_enum, short, long, default_value_t=Direction::Forward)]
        direction: Direction,
        #[arg(value_enum, short, long, default_value_t=TraceStyle::Table)]
        style: TraceStyle,
    },
    /// Test braille translations from given YAML file(s).
    Check {
        /// Only show a summary of the test results
        #[arg(short, long)]
        summary: bool,
        /// YAML document(s) that specify the tests
        #[arg(required = true)]
        yaml_files: Vec<PathBuf>,
    },
    /// Find braille tables based on a metadata query. The tables are
    /// searched in the the search path `LOUIS_TABLE_PATH`.
    Query {
        /// Metadata search query <key=value,...>
        query: String,
    },
    /// Parse and fully expand `table` (following all `include` directives), then serialize the
    /// resulting rules to a compact, gzip-compressed bincode bundle for distribution.
    Bundle {
        /// Braille table to bundle
        table: PathBuf,
        /// Where to write the bundle. Defaults to `table` with its extension replaced by
        /// `.bincode.gz`.
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
    /// Bundle every table discoverable via the metadata index (optionally filtered by a
    /// metadata query), writing one self-contained `.bincode.gz` file per table into
    /// `output_dir`. Tables are searched in `LOUIS_TABLE_PATH`, same as `query`.
    BundleAll {
        /// Directory to write bundles into (created if it doesn't exist)
        output_dir: PathBuf,
        /// Metadata query to filter which tables get bundled <key=value,...>. Omit to
        /// bundle every table found in the metadata index.
        #[arg(short, long)]
        query: Option<String>,
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

fn format_table_error(error: &TableError) -> String {
    match error {
        TableError::ParseError { path, line, error } => match path {
            Some(path) => format!("{}:{}: {}", path.display(), line, error),
            None => format!("{}: {}", line, error),
        },
        error => error.to_string(),
    }
}

fn print_errors(errors: Vec<TableError>) {
    for error in &errors {
        eprintln!("{}", format_table_error(error));
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

fn default_bundle_output(table: &Path) -> PathBuf {
    let mut path = table.to_path_buf();
    path.set_extension("bincode.gz");
    path
}

fn parse_metadata_query(query: &str) -> Vec<(String, String)> {
    query
        .split(',')
        .map(|s| {
            let parts: Vec<_> = s.split('=').collect();
            (parts[0].to_string(), parts[1].to_string())
        })
        .collect()
}

/// Parse, fully expand, and serialize `table` to `output`. Returns the rule and byte counts on
/// success, or a human-readable error message on failure -- never prints or exits, so it can be
/// used both for a single interactive bundle and as a rayon worker across many tables.
fn bundle_one(table: &Path, output: &Path) -> Result<(usize, usize), String> {
    let rules = parser::table_expanded(table).map_err(|errors| {
        errors
            .iter()
            .map(format_table_error)
            .collect::<Vec<_>>()
            .join("\n")
    })?;
    let bytes = bundle::serialize_rules(&rules)
        .map_err(|e| format!("Could not serialize rules for {}: {}", table.display(), e))?;
    std::fs::write(output, &bytes)
        .map_err(|e| format!("Could not write bundle to {}: {}", output.display(), e))?;
    Ok((rules.len(), bytes.len()))
}

fn bundle_table(table: &Path, output: Option<PathBuf>) {
    let output = output.unwrap_or_else(|| default_bundle_output(table));
    match bundle_one(table, &output) {
        Ok((rule_count, byte_count)) => println!(
            "Wrote {} rules ({} bytes) to {}",
            rule_count,
            byte_count,
            output.display()
        ),
        Err(message) => {
            eprintln!("{}", message);
            exit(1);
        }
    }
}

fn bundle_all(output_dir: &Path, query: Option<String>) {
    let index = match metadata::index() {
        Ok(index) => index,
        Err(e) => {
            eprintln!("Could not index tables: {:?}", e);
            exit(1);
        }
    };

    let mut tables: Vec<PathBuf> = match query {
        Some(query) => metadata::find(&index, parse_metadata_query(&query))
            .into_iter()
            .cloned()
            .collect(),
        None => index
            .values()
            .flatten()
            .cloned()
            .collect::<std::collections::HashSet<_>>()
            .into_iter()
            .collect(),
    };
    tables.sort();

    if tables.is_empty() {
        eprintln!("No tables found to bundle");
        exit(1);
    }

    if let Err(e) = std::fs::create_dir_all(output_dir) {
        eprintln!(
            "Could not create output directory {}: {}",
            output_dir.display(),
            e
        );
        exit(1);
    }

    type BundleResult<'a> = (&'a PathBuf, Result<(usize, usize), String>);
    let results: Vec<BundleResult> = tables
        .par_iter()
        .map(|table| {
            let file_name = table
                .file_name()
                .map(PathBuf::from)
                .unwrap_or_else(|| table.clone());
            let output = output_dir.join(default_bundle_output(&file_name));
            (table, bundle_one(table, &output))
        })
        .collect();

    let mut failures = 0;
    for (table, result) in &results {
        match result {
            Ok((rule_count, byte_count)) => {
                println!(
                    "{}: {} rules, {} bytes",
                    table.display(),
                    rule_count,
                    byte_count
                )
            }
            Err(message) => {
                eprintln!("{}: {}", table.display(), message);
                failures += 1;
            }
        }
    }

    println!(
        "Bundled {} of {} tables",
        results.len() - failures,
        results.len()
    );
    if failures > 0 {
        exit(1);
    }
}

#[derive(Tabled, Default)]
#[tabled(rename_all = "PascalCase")]
struct Trace {
    #[tabled(rename = "")]
    sequence_id: usize,
    from: String,
    to: String,
    #[tabled(display_with("Self::display_origin", self))]
    rule: Option<AnchoredRule>,
    stage: TranslationStage,
}

impl Trace {
    fn display_origin(&self) -> String {
        match &self.rule {
            Some(AnchoredRule { rule, .. }) => {
                format!("{}", rule)
            }
            None => String::new(),
        }
    }
}

fn print_trace(all_translations: &Vec<Vec<ResolvedTranslation>>, style: &TraceStyle) {
    let mut traces: Vec<Trace> = Vec::new();
    for (id, translation) in all_translations
        .iter()
        .flatten()
        // Only show translations witout an originating rule for the main translation stage
        .filter(|t| t.stage() == TranslationStage::Main || t.origin().is_some())
        .enumerate()
    {
        traces.push(Trace {
            sequence_id: id + 1,
            rule: translation.origin(),
            from: translation.input(),
            to: translation.output(),
            stage: translation.stage(),
        });
    }
    match style {
        TraceStyle::Blank => {
            let mut table = Table::new(traces);
            table.with(Style::blank());
            println!("{}", table);
        }
        TraceStyle::Table => {
            let mut table = Table::new(traces);
            table.with(Style::sharp());
            println!("{}", table);
        }
    }
}

fn trace(table: &Path, direction: Direction, input: &str, style: &TraceStyle) {
    let rules = parser::table_expanded(table);
    match rules {
        Ok(rules) => {
            match TranslationPipeline::compile(&rules, direction) {
                Ok(table) => {
                    println!("{}", table.translate(input));
                    print_trace(&table.trace(input), style);
                }
                Err(e) => eprintln!("Could not compile table: {:?}", e),
            };
        }
        Err(errors) => {
            print_errors(errors);
        }
    }
}

fn translate(table: &Path, direction: Direction, input: &str) {
    let rules = parser::table_expanded(table);
    match rules {
        Ok(rules) => {
            match TranslationPipeline::compile(&rules, direction) {
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
// over the TranslationPipeline.
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

/// Parses and runs a single YAML test file, returning either the test
/// results or a ready-to-print error message.
fn check_yaml_file(path: &Path) -> Result<Vec<test::TestResult>, String> {
    let file =
        File::open(path).map_err(|e| format!("Could not open yaml file {:?} ({})", path, e))?;
    let mut parser = YAMLParser::new(file)
        .map_err(|e| format!("Could not create parser {:?} ({:?})", path, e))?;
    parser
        .yaml()
        .map_err(|e| format!("{}: {}", path.display(), e))
}

fn check_yaml(paths: Vec<PathBuf>, summary: bool) {
    // Each YAML file is independent (its own tables, its own tests), so we
    // run them concurrently and only merge results back together afterwards
    // in the original order, keeping output deterministic.
    let outcomes: Vec<_> = paths.par_iter().map(|path| check_yaml_file(path)).collect();

    let mut total = YAMLTestResult::default();
    let mut yaml_results: Vec<YAMLTestResult> = Vec::new();
    for (path, outcome) in paths.iter().zip(outcomes) {
        match outcome {
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
            Err(message) => {
                eprintln!("{}", message);
            }
        }
    }
    if summary {
        yaml_results.sort_by_key(|r| r.tests);
        total.yaml_file = "Total".to_string();
        yaml_results.push(total);
        let mut table = Table::new(yaml_results);
        table.with(Style::sharp());
        // add a special separator above the Total row
        table.modify(Rows::last(), Border::inherit(Style::sharp()));
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
            Some(input) => {
                translate(&table, direction, &input);
            }
            None => {
                let rules = parser::table_expanded(&table);
                match rules {
                    Ok(rules) => match TranslationPipeline::compile(&rules, direction) {
                        Ok(table) => repl(Box::new(move |input| {
                            println!("{}", table.translate(&input));
                        })),
                        Err(e) => eprintln!("Could not compile table: {:?}", e),
                    },
                    Err(errors) => {
                        print_errors(errors);
                    }
                }
            }
        },
        Commands::Trace {
            table,
            input,
            direction,
            style,
        } => match input {
            Some(input) => {
                trace(&table, direction, &input, &style);
            }
            None => {
                let rules = parser::table_expanded(&table);
                match rules {
                    Ok(rules) => match TranslationPipeline::compile(&rules, direction) {
                        Ok(table) => repl(Box::new(move |input| {
                            println!("{}", table.translate(&input));
                            print_trace(&table.trace(&input), &style);
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
                println!("{:?}", metadata::find(&index, parse_metadata_query(&query)));
            }
            Err(e) => {
                eprint!("Could not index all tables: {:?}", e)
            }
        },
        Commands::Bundle { table, output } => bundle_table(&table, output),
        Commands::BundleAll { output_dir, query } => bundle_all(&output_dir, query),
    }
}
