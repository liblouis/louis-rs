//! Coverage analysis: which rules in a table are never exercised by any test.

use std::collections::{HashMap, HashSet};

use rayon::prelude::*;

use crate::{
    parser::{Direction, RuleKey},
    translator::{DisplayTable, TranslationOptions, TranslationPipeline},
};

use super::{Test, TestError, TestMatrix, TestMode};

impl Test {
    /// Like [`check`](Test::check), but traces the translation instead of
    /// comparing it against `expected`, returning which rules fired.
    fn hit_rules(
        &self,
        table: &TranslationPipeline,
        display_table: &DisplayTable,
        direction: Direction,
    ) -> HashSet<RuleKey> {
        let options = TranslationOptions::default()
            .with_mode(self.modes.clone())
            .with_emphasis(self.emphasis.clone());
        let traced = match direction {
            Direction::Forward => table.trace_with_options(&self.input, &options),
            Direction::Backward => {
                let displayed = display_table.translate(&self.input);
                table.trace_with_options(&displayed, &options)
            }
        };
        traced
            .into_iter()
            .flatten()
            .filter_map(|t| t.origin())
            .map(|r| r.key())
            .collect()
    }
}

impl TestMatrix<'_> {
    /// Which rules in the table(s) this matrix uses are ever exercised by its
    /// tests, alongside the full universe of rules the table(s) define
    /// (source location -> rule text). The difference between the two is the
    /// dead-rule list.
    pub fn coverage(&self) -> Result<(HashSet<RuleKey>, HashMap<RuleKey, String>), TestError> {
        let mut hit = HashSet::new();
        let mut universe = HashMap::new();
        let mut run = |tests: &[Test],
                        table: &TranslationPipeline,
                        display_table: &DisplayTable,
                        direction: Direction| {
            hit.extend(
                tests
                    .par_iter()
                    .flat_map(|test| test.hit_rules(table, display_table, direction))
                    .collect::<HashSet<_>>(),
            );
        };
        match self.mode {
            TestMode::Forward => {
                let display_table = self.display_table(Direction::Forward)?;
                for table in self.tables {
                    let (rules, table) = self.translation_table(table, Direction::Forward)?;
                    universe.extend(rules.iter().map(|r| (r.key(), r.to_string())));
                    run(self.tests, &table, &display_table, Direction::Forward);
                }
            }
            TestMode::Backward => {
                if option_env!("LOUIS_TEST_FORWARD_ONLY").is_none() {
                    let display_table = self.display_table(Direction::Backward)?;
                    for table in self.tables {
                        let (rules, table) = self.translation_table(table, Direction::Backward)?;
                        universe.extend(rules.iter().map(|r| (r.key(), r.to_string())));
                        run(self.tests, &table, &display_table, Direction::Backward);
                    }
                }
            }
            TestMode::BothDirections => {
                let display_table = self.display_table(Direction::Forward)?;
                for table in self.tables {
                    let (rules, table) = self.translation_table(table, Direction::Forward)?;
                    universe.extend(rules.iter().map(|r| (r.key(), r.to_string())));
                    run(self.tests, &table, &display_table, Direction::Forward);
                }
                if option_env!("LOUIS_TEST_FORWARD_ONLY").is_none() {
                    let display_table = self.display_table(Direction::Backward)?;
                    let reversed: Vec<Test> =
                        self.tests.iter().cloned().map(|t| t.reverse()).collect();
                    for table in self.tables {
                        let (rules, table) = self.translation_table(table, Direction::Backward)?;
                        universe.extend(rules.iter().map(|r| (r.key(), r.to_string())));
                        run(&reversed, &table, &display_table, Direction::Backward);
                    }
                }
            }
            _ => (), // FIXME: not yet implemented, same gap as check()
        }
        Ok((hit, universe))
    }
}
