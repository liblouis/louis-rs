//! CLI-facing coverage reporting: which rules across the given YAML test
//! files are never exercised by any test. The measurement itself (tracing a
//! test's translation to see which rules fire) lives in
//! [`crate::test::coverage`].

use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::path::{Path, PathBuf};

use rayon::prelude::*;
use tabled::{
    Table, Tabled,
    settings::{Border, Style, object::Rows},
};

use crate::parser::RuleKey;
use crate::yaml::YAMLParser;

/// Parses and runs coverage analysis on a single YAML test file, returning
/// either the rules it hit plus the rule universe it touched, or a
/// ready-to-print error message.
fn coverage_yaml_file(path: &Path) -> Result<(HashSet<RuleKey>, HashMap<RuleKey, String>), String> {
    let file =
        File::open(path).map_err(|e| format!("Could not open yaml file {:?} ({})", path, e))?;
    let mut parser = YAMLParser::new(file)
        .map_err(|e| format!("Could not create parser {:?} ({:?})", path, e))?;
    parser
        .coverage()
        .map_err(|e| format!("{}: {}", path.display(), e))
}

#[derive(Tabled, Default)]
#[tabled(rename_all = "PascalCase")]
struct CoverageSummary {
    #[tabled(rename = "YAML File")]
    yaml_file: String,
    rules: usize,
    #[tabled(rename = "Not Exercised")]
    not_exercised: usize,
}

impl CoverageSummary {
    fn update(&mut self, rules: usize, not_exercised: usize) {
        self.rules += rules;
        self.not_exercised += not_exercised;
    }
}

pub fn coverage_yaml(paths: Vec<PathBuf>, summary: bool) {
    eprintln!("warning: `coverage` is experimental; its output and flags may still change");

    let outcomes: Vec<_> = paths
        .par_iter()
        .map(|path| coverage_yaml_file(path))
        .collect();

    let mut total = CoverageSummary::default();
    let mut yaml_results: Vec<CoverageSummary> = Vec::new();
    for (path, outcome) in paths.iter().zip(outcomes) {
        match outcome {
            Ok((hit, universe)) => {
                let mut dead: Vec<&String> = universe
                    .iter()
                    .filter(|(key, _)| !hit.contains(key))
                    .map(|(_, text)| text)
                    .collect();
                dead.sort();

                let yaml_file = path
                    .file_name()
                    .map_or(path.display().to_string(), |f| {
                        f.to_string_lossy().into_owned()
                    });
                total.update(universe.len(), dead.len());
                yaml_results.push(CoverageSummary {
                    yaml_file: yaml_file.clone(),
                    rules: universe.len(),
                    not_exercised: dead.len(),
                });
                if !summary {
                    println!("{}:", yaml_file);
                    for text in dead {
                        println!("  {}", text);
                    }
                }
            }
            Err(message) => eprintln!("{}", message),
        }
    }

    if summary {
        yaml_results.sort_by_key(|r| r.rules);
        total.yaml_file = "Total".to_string();
        yaml_results.push(total);
        let mut table = Table::new(yaml_results);
        table.with(Style::sharp());
        table.modify(Rows::last(), Border::inherit(Style::sharp()));
        println!("{}", table);
    }
}
