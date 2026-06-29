//! Per-position translation constraints
//!
//! A [`Constrainer`] analyses the input text before translation begins and
//! produces a [`PositionConstraints`] value. The translation loop consults this
//! to decide which rules are eligible at each character position.

use enumset::{EnumSet, EnumSetType};
use std::collections::HashSet;

/// A rule-selection constraint that can be set at a character position.
#[derive(EnumSetType, Debug)]
pub enum Constraint {
    /// Suppress multi-character (contraction) rules at this position.
    ///
    /// Set for every character inside a numeric run so that contractions are
    /// not applied to digit sequences.
    DontContract,
}

pub type Constraints = EnumSet<Constraint>;

/// The result of pre-computing per-position constraints for a given input.
pub struct PositionConstraints(Vec<Constraints>);

impl PositionConstraints {
    pub fn empty() -> Self {
        Self(Vec::new())
    }

    pub fn dont_contract_at(&self, pos: usize) -> bool {
        self.0
            .get(pos)
            .is_some_and(|f| f.contains(Constraint::DontContract))
    }
}

/// Detects numeric runs in the input and sets [`Constraint::DontContract`] at
/// every character position that falls inside one.
#[derive(Debug, Clone)]
pub struct Constrainer {
    numeric_chars: HashSet<char>,
    extra_numeric_chars: HashSet<char>,
    mid_numeric_chars: HashSet<char>,
}

impl Constrainer {
    pub fn precompute(&self, input: &str) -> PositionConstraints {
        let chars: Vec<char> = input.chars().collect();
        let n = chars.len();
        let mut constraints: Vec<Constraints> = vec![Constraints::empty(); n];
        let mut in_numeric = false;

        for (pos, &c) in chars.iter().enumerate() {
            let is_numeric = self.numeric_chars.contains(&c);
            match (is_numeric, in_numeric) {
                (false, false) => {}
                (true, false) => {
                    in_numeric = true;
                    constraints[pos].insert(Constraint::DontContract);
                }
                (true, true) => {
                    constraints[pos].insert(Constraint::DontContract);
                }
                (false, true) if self.extra_numeric_chars.contains(&c) => {
                    constraints[pos].insert(Constraint::DontContract);
                }
                (false, true)
                    if self.extra_numeric_chars.is_empty()
                        && self.mid_numeric_chars.contains(&c)
                        && chars
                            .get(pos + 1)
                            .is_some_and(|nc| self.numeric_chars.contains(nc)) =>
                {
                    constraints[pos].insert(Constraint::DontContract);
                }
                (false, true) => {
                    in_numeric = false;
                }
            }
        }

        PositionConstraints(constraints)
    }
}

/// A builder for [`Constrainer`]
#[derive(Debug, Default)]
pub struct ConstrainerBuilder {
    numeric_chars: HashSet<char>,
    extra_numeric_chars: HashSet<char>,
    mid_numeric_chars: HashSet<char>,
}

impl ConstrainerBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn numeric_characters(&mut self, chars: HashSet<char>) {
        self.numeric_chars = chars;
    }

    pub fn numericmodechars(&mut self, s: &str) {
        self.extra_numeric_chars = HashSet::from_iter(s.chars());
    }

    pub fn midnum(&mut self, s: &str) {
        self.mid_numeric_chars.extend(s.chars());
    }

    pub fn build(self) -> Option<Constrainer> {
        if self.numeric_chars.is_empty() {
            None
        } else {
            Some(Constrainer {
                numeric_chars: self.numeric_chars,
                extra_numeric_chars: self.extra_numeric_chars,
                mid_numeric_chars: self.mid_numeric_chars,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dont_contract(c: &PositionConstraints) -> Vec<bool> {
        c.0.iter()
            .map(|f| f.contains(Constraint::DontContract))
            .collect()
    }

    #[test]
    fn numeric_run() {
        let mut builder = ConstrainerBuilder::new();
        builder.numeric_characters(HashSet::from(['1', '2', '3']));
        let constrainer = builder.build().unwrap();

        let c = constrainer.precompute("ab12 a");
        assert_eq!(
            dont_contract(&c),
            vec![false, false, true, true, false, false]
        );
    }

    #[test]
    fn midnum_extends_run() {
        let mut builder = ConstrainerBuilder::new();
        builder.numeric_characters(HashSet::from(['1', '2', '3']));
        builder.midnum(".");
        let constrainer = builder.build().unwrap();

        let c = constrainer.precompute("1.2");
        assert_eq!(dont_contract(&c), vec![true, true, true]);
    }

    #[test]
    fn empty_returns_false() {
        let c = PositionConstraints::empty();
        assert!(!c.dont_contract_at(0));
        assert!(!c.dont_contract_at(99));
    }
}
