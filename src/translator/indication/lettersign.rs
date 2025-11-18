//! Letter sign braille indication
//!
//! [`Indicator`] keeps a list of contractions in a prefix tree. If the input string contains any of
//! these contractions a [`Translation`] is emitted containing the associated indicator.

// FIXME: should the returned translation contain the letsign or the contraction rule?

use log::warn;

use crate::translator::trie::Boundary;
use crate::{
    parser::{AnchoredRule, Direction, Precedence},
    translator::{Translation, trie::Trie},
};

use std::collections::HashMap;

/// A builder for [`Indicator`]
#[derive(Debug)]
pub struct IndicatorBuilder {
    lettersign: Option<(String, AnchoredRule)>,
    contractions: HashMap<String, AnchoredRule>,
}

impl IndicatorBuilder {
    pub fn new() -> Self {
        Self {
            lettersign: None,
            contractions: HashMap::new(),
        }
    }

    pub fn build(self) -> Indicator {
        let mut trie = Trie::new();
        if self.lettersign.is_none() && !self.contractions.is_empty() {
            warn!("Contractions defined without lettersign rule");
        }
        if let Some((lettersign, origin)) = self.lettersign {
            for (contraction, _origin) in self.contractions {
                trie.insert(
                    contraction,
                    lettersign.to_string(),
                    Boundary::Word,
                    Boundary::Word,
                    Direction::Forward,
                    Precedence::Default,
                    &origin,
                );
            }
        }
        Indicator { contractions: trie }
    }

    pub fn letsign(mut self, s: &str, origin: &AnchoredRule) -> Self {
        self.lettersign = Some((s.to_string(), origin.clone()));
        self
    }

    pub fn contraction(mut self, s: &str, origin: &AnchoredRule) -> Self {
        self.contractions.insert(s.to_string(), origin.clone());
        self
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
            Some(Translation {
                length: 1,
                weight: 1,
                input: "".to_string(),
                ..translation.clone()
            })
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::RuleParser;

    fn rule(rule: &str) -> AnchoredRule {
        let rule = RuleParser::new(rule).rule().unwrap();
        AnchoredRule::new(rule, None, 0)
    }

    #[test]
    fn indicator() {
        let mut builder = IndicatorBuilder::new();
        builder = builder.letsign("⠠", &rule("letsign 6"));
        builder = builder.contraction("ab", &rule("contraction ab"));
        builder = builder.contraction("cd", &rule("contraction cd"));
        let indicator = builder.build();
        assert_eq!(indicator.next("aa".into(), None), None);
        assert_eq!(
            indicator.next("ab".into(), None),
            Some(Translation {
                input: "".into(),
                output: "⠠".into(),
                length: 1,
                weight: 1,
                offset: 0,
                precedence: Precedence::Default,
                origin: Some(rule("letsign 6")),
            })
        );
        assert_eq!(
            indicator.next("cd".into(), None),
            Some(Translation {
                input: "".into(),
                output: "⠠".into(),
                length: 1,
                weight: 1,
                offset: 0,
                precedence: Precedence::Default,
                origin: Some(rule("letsign 6")),
            })
        );
    }
}
