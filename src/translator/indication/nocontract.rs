//! Nocontract braille indication
//!
//! Sometimes a sequence of letters happens to be the same as a braille contraction. In that case an
//! indication is needed to clarify the meaning. The nocontract indicator indicates that the
//! following braille cells are not a contraction.
//!
//! Other than the rest of the indicators this [`Indicator`] is not a state machine. It keeps a list
//! of contractions and matches the input against it. If a contraction appears in the input an
//! indication is emitted.
//!
//! # FIXME:
//!
//! This indicator is basically the same as the [lettersign
//! indicator](crate::translator::indication::lettersign::Indicator). In liblouis they do not seem
//! to have exactly the same behaviour. Find a way to merge them.

use crate::translator::TranslationStage;
use crate::translator::trie::Boundary;
use crate::{
    parser::{AnchoredRule, Direction, Precedence},
    translator::{Translation, trie::Trie},
};

use log::warn;

use std::collections::HashMap;

/// A builder for [`Indicator`]
#[derive(Debug)]
pub struct IndicatorBuilder {
    nocontractsign: Option<(String, AnchoredRule)>,
    contractions: HashMap<String, AnchoredRule>,
}

impl IndicatorBuilder {
    pub fn new() -> Self {
        Self {
            nocontractsign: None,
            contractions: HashMap::new(),
        }
    }

    /// Build an [`Indicator`]
    pub fn build(self) -> Indicator {
        let mut trie = Trie::new();
        if !self.contractions.is_empty() && self.nocontractsign.is_some() {
            warn!("Table contains contractions but no nocontractionsign");
        }
        if let Some((nocontractsign, origin)) = self.nocontractsign {
            for (contraction, _origin) in self.contractions {
                trie.insert(
                    &contraction,
                    &nocontractsign,
                    Boundary::Word,
                    Boundary::Word,
                    Direction::Forward,
                    Precedence::Default,
                    TranslationStage::Main,
                    &origin,
                );
            }
        }
        Indicator { contractions: trie }
    }

    pub fn nocontractsign(&mut self, s: &str, origin: &AnchoredRule) {
        self.nocontractsign = Some((s.to_string(), origin.clone()));
    }

    pub fn contraction(&mut self, s: &str, origin: &AnchoredRule) {
        self.contractions.insert(s.to_string(), origin.clone());
    }
}

#[derive(Debug)]
pub struct Indicator {
    contractions: Trie,
}

impl Indicator {
    pub fn next(&self, s: &str, prev: Option<char>) -> Option<Translation> {
        let translations = self.contractions.find_translations(s, prev);
        if !translations.is_empty() {
            let translation = translations.first().unwrap();
            // the translations contain the contraction as input, so set that one to the empty
            // string
            Some(translation.clone().with_input("").with_weight(1))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parser::RuleParser, translator::TranslationStage};

    fn rule(rule: &str) -> AnchoredRule {
        let rule = RuleParser::new(rule).rule().unwrap();
        AnchoredRule::new(rule, None, 0)
    }

    #[test]
    fn indicator() {
        let mut builder = IndicatorBuilder::new();
        builder.nocontractsign("⡀", &rule("nocontractsign 7"));
        builder.contraction("ab", &rule("contraction ab"));
        builder.contraction("cd", &rule("contraction cd"));
        let indicator = builder.build();
        assert_eq!(indicator.next("aa".into(), None), None);
        assert_eq!(
            indicator.next("ab".into(), None),
            Some(Translation::new(
                "".into(),
                "⡀".into(),
                1,
                TranslationStage::Main,
                rule("nocontractsign 7")
            ))
        );
        assert_eq!(
            indicator.next("cd".into(), None),
            Some(Translation::new(
                "".into(),
                "⡀".into(),
                1,
                TranslationStage::Main,
                rule("nocontractsign 7")
            ))
        );
    }
}
