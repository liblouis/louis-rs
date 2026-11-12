use std::fs::File;
use std::io;
use std::io::BufRead;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::process::{ExitCode, exit};
use std::sync::LazyLock;

use clap::{Parser, Subcommand};
use rayon::prelude::*;
use search_path::SearchPath;

use louis::Error;
use louis::internals::parser::{self, AnchoredRule, Direction, TableError};
use louis::internals::translator::{
    DisplayTable, ResolvedTranslation, TranslationPipeline, TranslationStage,
};
use louis::metadata;

use crate::test::TestResult;

mod test;
mod yaml;

use tabled::{
    Table, Tabled,
    settings::{Border, Style, object::Rows},
};
use yaml::YAMLParser;

/// The library's [`Direction`], which deliberately knows nothing about clap.
#[derive(Debug, Clone, Copy, clap::ValueEnum)]
enum DirectionArg {
    Forward,
    Backward,
}

impl From<DirectionArg> for Direction {
    fn from(direction: DirectionArg) -> Self {
        match direction {
            DirectionArg::Forward => Direction::Forward,
            DirectionArg::Backward => Direction::Backward,
        }
    }
}

#[derive(Debug, Clone, clap::ValueEnum)]
enum TraceStyle {
    Blank,
    Table,
}

/// The commands available in the command line tool.
#[derive(Debug, Subcommand)]
enum Commands {
    /// Check that `table` compiles, like liblouis's `lou_checktable`.
    ///
    /// Errors go to stderr and make the exit status non-zero.
    CheckTable {
        /// Braille table to check
        table: PathBuf,
        /// Don't write "No errors found." when there are none
        #[arg(short, long)]
        quiet: bool,
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
        #[arg(value_enum, short, long, default_value_t=DirectionArg::Forward)]
        direction: DirectionArg,
        /// Braille encoding: a liblouis display table (`.dis`) saying which characters stand
        /// for the braille cells, e.g. en-us-brf.dis for ASCII braille. Without it braille is
        /// read and written as Unicode braille (U+2800).
        #[arg(long, visible_alias = "display")]
        braille_encoding: Option<PathBuf>,
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
        #[arg(value_enum, short, long, default_value_t=DirectionArg::Forward)]
        direction: DirectionArg,
        #[arg(value_enum, short, long, default_value_t=TraceStyle::Table)]
        style: TraceStyle,
        /// Braille encoding: a liblouis display table (`.dis`) saying which characters stand
        /// for the braille cells, e.g. en-us-brf.dis for ASCII braille. Without it braille is
        /// read and written as Unicode braille (U+2800).
        #[arg(long, visible_alias = "display")]
        braille_encoding: Option<PathBuf>,
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
        /// Metadata search query in liblouis's syntax, e.g. `language:de grade:2`, either as
        /// one argument or as one argument per pair
        #[arg(required = true)]
        query: Vec<String>,
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
            TableError::Syntax { path, line, error } => match path {
                Some(path) => eprintln!("{}:{}: {}", path.display(), line, error),
                None => eprintln!("{}: {}", line, error),
            },
            _ => eprintln!("{}", error),
        }
    }
}

/// The search path for a table named on the command line: its own directory first, so that
/// `louis check-table /some/where/top.utb` finds an `include base.utb` sitting next to it without that
/// directory being on `LOUIS_TABLE_PATH`. This is a convenience for a human typing a path, not a
/// lookup rule -- the library API resolves names against the search path it is handed and
/// nothing else. Unlike liblouis, which rebases on the including table at every level, the
/// directory stays on the search path for nested includes too: more permissive, never less.
fn cli_search_path(table: &Path) -> SearchPath {
    let mut search_path = SearchPath::new_or("LOUIS_TABLE_PATH", ".");
    if let Some(dir) = table.parent() {
        search_path.prepend(dir.to_path_buf());
    }
    search_path
}

/// The table is compiled in both directions, as a rule can be valid in one only.
fn check_table(table: &Path, quiet: bool) -> ExitCode {
    match pipeline(table, None, Direction::Forward)
        .and_then(|_| pipeline(table, None, Direction::Backward))
    {
        Ok(_) => {
            if !quiet {
                eprintln!("No errors found.");
            }
            ExitCode::SUCCESS
        }
        Err(e) => {
            report(e);
            ExitCode::FAILURE
        }
    }
}

#[derive(Tabled, Default)]
#[tabled(rename_all = "PascalCase")]
struct Trace {
    #[tabled(rename = "")]
    sequence_id: usize,
    from: String,
    to: String,
    #[tabled(display = "display_rule")]
    rule: Option<AnchoredRule>,
    stage: TranslationStage,
}

fn display_rule(rule: &Option<AnchoredRule>) -> String {
    match rule {
        Some(AnchoredRule { rule, .. }) => format!("{}", rule),
        None => String::new(),
    }
}

fn print_trace(all_translations: &[Vec<ResolvedTranslation>], style: &TraceStyle) {
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

/// Report why a table could not be turned into a [`TranslationPipeline`]: reading can fail
/// with any number of errors, each naming its own file and line, compiling with one.
fn report(error: Error) {
    match error {
        Error::Table(errors) => print_errors(errors),
        error => eprintln!("Could not compile table: {error}"),
    }
}

/// Compile `table`, and `display` if one was given, into a pipeline.
fn pipeline(
    table: &Path,
    display: Option<&Path>,
    direction: Direction,
) -> Result<TranslationPipeline, Error> {
    let rules = parser::table_expanded_in(table, &cli_search_path(table)).map_err(Error::Table)?;
    let pipeline = TranslationPipeline::compile(&rules, direction)?;
    match display {
        Some(path) => {
            let rules =
                parser::table_expanded_in(path, &cli_search_path(path)).map_err(Error::Table)?;
            Ok(pipeline.with_display(DisplayTable::compile(&rules, direction)))
        }
        None => Ok(pipeline),
    }
}

fn trace(
    table: &Path,
    display: Option<&Path>,
    direction: Direction,
    input: &str,
    style: &TraceStyle,
) -> ExitCode {
    match pipeline(table, display, direction) {
        Ok(table) => {
            println!("{}", table.translate(input));
            print_trace(&table.trace(input), style);
            ExitCode::SUCCESS
        }
        Err(e) => {
            report(e);
            ExitCode::FAILURE
        }
    }
}

fn translate(table: &Path, display: Option<&Path>, direction: Direction, input: &str) -> ExitCode {
    match pipeline(table, display, direction) {
        Ok(table) => {
            println!("{}", table.translate(input));
            ExitCode::SUCCESS
        }
        Err(e) => {
            report(e);
            ExitCode::FAILURE
        }
    }
}

// The translate and trace repls differ only in what they do with a line, so each passes a
// closure over its TranslationPipeline.
fn repl(handler: Box<dyn Fn(String)>) -> ExitCode {
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
    ExitCode::SUCCESS
}

#[derive(Tabled, Default)]
#[tabled(rename_all = "PascalCase")]
struct YAMLTestResult {
    #[tabled(rename = "YAML File")]
    yaml_file: String,
    tests: usize,
    #[tabled(display("Self::display_percent", self))]
    successes: usize,
    #[tabled(display("Self::display_percent", self))]
    failures: usize,
    #[tabled(rename = "Expected\nFailures", display("Self::display_percent", self))]
    expected_failures: usize,
    #[tabled(
        rename = "Unexpected\nSuccesses",
        display("Self::display_percent", self)
    )]
    unexpected_successes: usize,
    #[tabled(rename = "Position\nMismatches")]
    position_mismatches: usize,
}

fn percent(n: usize, total: usize) -> f64 {
    (n as f64 * 100.0) / total as f64
}

impl YAMLTestResult {
    /// A count as a percentage of the tests
    fn display_percent(n: &usize, result: &Self) -> String {
        format!("{:.1}%", percent(*n, result.tests))
    }
    fn update(
        &mut self,
        tests: usize,
        successes: usize,
        failures: usize,
        expected_failures: usize,
        unexpected_successes: usize,
        position_mismatches: usize,
    ) {
        self.tests += tests;
        self.successes += successes;
        self.failures += failures;
        self.expected_failures += expected_failures;
        self.unexpected_successes += unexpected_successes;
        self.position_mismatches += position_mismatches;
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

fn check_yaml(paths: Vec<PathBuf>, summary: bool) -> ExitCode {
    // Each YAML file is independent (its own tables, its own tests), so we
    // run them concurrently and only merge results back together afterwards
    // in the original order, keeping output deterministic.
    let outcomes: Vec<_> = paths.par_iter().map(|path| check_yaml_file(path)).collect();

    let mut unreadable = false;
    let mut total = YAMLTestResult::default();
    let mut yaml_results: Vec<YAMLTestResult> = Vec::new();
    for (path, outcome) in paths.iter().zip(outcomes) {
        match outcome {
            Ok(test_results) => {
                // a block that never ran is not a test, so it stays out of the
                // denominator of the success/failure percentages; it is reported
                // on stderr instead
                let tests = test_results.iter().filter(|r| !r.is_error()).count();
                let successes = test_results.iter().filter(|r| r.is_success()).count();
                let failures = test_results
                    .iter()
                    .filter(|r| r.is_translation_failure())
                    .count();
                let expected_failures = test_results
                    .iter()
                    .filter(|r| r.is_expected_failure())
                    .count();
                let unexpected_successes = test_results
                    .iter()
                    .filter(|r| r.is_unexpected_success())
                    .count();
                let position_mismatches = test_results
                    .iter()
                    .filter(|r| r.is_position_failure())
                    .count();
                total.update(
                    tests,
                    successes,
                    failures,
                    expected_failures,
                    unexpected_successes,
                    position_mismatches,
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
                    position_mismatches,
                });
                // A block that never ran is a problem with the table or the test
                // file, not a translation failure, so it goes to stderr with the
                // file that caused it -- and is counted in its own column, since
                // the suite is normally run with stderr discarded.
                for res in &test_results {
                    if let TestResult::Error(error) = res {
                        eprintln!("{}: {}", path.display(), error);
                    }
                }
                if !summary {
                    for res in &test_results {
                        if !res.is_success() && !res.is_error() {
                            println!("{:?}", res);
                        }
                    }
                }
            }
            Err(message) => {
                eprintln!("{}", message);
                unreadable = true;
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
    // Only a yaml file we could not read or parse is an error; failing tests are
    // a result, not a failure of the run.
    if unreadable {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

static INDEX: LazyLock<Result<metadata::Index, metadata::MetadataError>> =
    LazyLock::new(metadata::Index::new);

/// The index of the tables on `LOUIS_TABLE_PATH`, built once on first use: the YAML files
/// run in parallel and many query their tables, so they share one walk of the search path.
fn index() -> Result<&'static metadata::Index, &'static metadata::MetadataError> {
    INDEX.as_ref()
}

fn main() -> ExitCode {
    env_logger::init();

    let args = Cli::parse();

    match args.command {
        Commands::CheckTable { table, quiet } => check_table(&table, quiet),
        Commands::Translate {
            table,
            input,
            direction,
            braille_encoding: display,
        } => match input {
            Some(input) => translate(&table, display.as_deref(), direction.into(), &input),
            None => match pipeline(&table, display.as_deref(), direction.into()) {
                Ok(table) => repl(Box::new(move |input| {
                    println!("{}", table.translate(&input));
                })),
                Err(e) => {
                    report(e);
                    ExitCode::FAILURE
                }
            },
        },
        Commands::Trace {
            table,
            input,
            direction,
            style,
            braille_encoding: display,
        } => match input {
            Some(input) => trace(&table, display.as_deref(), direction.into(), &input, &style),
            None => match pipeline(&table, display.as_deref(), direction.into()) {
                Ok(table) => repl(Box::new(move |input| {
                    println!("{}", table.translate(&input));
                    print_trace(&table.trace(&input), &style);
                })),
                Err(e) => {
                    report(e);
                    ExitCode::FAILURE
                }
            },
        },
        Commands::Check {
            yaml_files,
            summary,
        } => check_yaml(yaml_files, summary),
        Commands::Query { query } => match (query.join(" ").parse::<metadata::Query>(), index()) {
            (Ok(query), Ok(index)) => {
                println!("{:?}", index.find(&query));
                ExitCode::SUCCESS
            }
            (Err(e), _) => {
                eprintln!("Invalid query: {e}");
                ExitCode::FAILURE
            }
            (_, Err(e)) => {
                eprintln!("Could not index all tables: {e}");
                ExitCode::FAILURE
            }
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cli_search_path_starts_at_the_tables_own_directory() {
        // Only the first entry is asserted, so the ambient LOUIS_TABLE_PATH does not matter.
        let search_path = cli_search_path(Path::new("/some/where/top.utb"));
        assert_eq!(
            search_path.iter().next(),
            Some(&PathBuf::from("/some/where"))
        );
    }
}
