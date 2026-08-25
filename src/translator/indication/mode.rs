//! Mode attribute  braille indication
//!
//! [`Indicator`] analyses the full input text in a single pass before translation begins
//! and emits the appropriate mode indicator at each position. liblouis defines four
//! escalating tiers for this (`modeletter`/`begmodeword`/`begmode`/`begmodephrase)`.
//!
//! This is a simplified [`uppercase::Indicator`](super::uppercase::Indicator): uppercase
//! has two nested classes, whereas a `mode` attribute has only itself — there is no
//! generic counterpart to `capsmodechars` that would let some other class of characters
//! extend a mode run without being part of it. So a mode "word" is exactly its own
//! maximal run of class membership: never partial, and always ending at what liblouis's
//! own `resetsEmphMode`/`isEmphSpace` treats as a clean boundary.
//!
//! Passage-level grouping (`begmode`/`begmodephrase`) works the same way it does for
//! uppercase: consecutive whole-mode words are grouped, and hyphen-joined segments count
//! as one *linguistic* word toward the [`lenmodephrase`](IndicatorBuilder::lenmodephrase)
//! threshold rather than one each.

use std::collections::{HashMap, HashSet};

use crate::{
    parser::{AnchoredRule, CharacterClass, CharacterClasses, Position},
    translator::{ResolvedTranslation, TranslationStage, table::TableContext},
};

fn make_translation(dots: &str, origin: &AnchoredRule) -> ResolvedTranslation {
    ResolvedTranslation::new("", dots, 1, TranslationStage::Main, origin.clone())
}

/// Compiled indicator data for one mode class (e.g. "digit", or a custom attribute).
#[derive(Debug, Clone)]
struct ClassIndicator {
    /// Characters belonging to this mode's named class, resolved from the table's
    /// `CharacterClasses` at build time. The mode is active exactly where these appear, and
    /// (see module doc) that extent is also the word boundary — there is no `endmodeword`
    /// counterpart here the way there is `endcapsword` in `uppercase.rs`, since a mode word
    /// can never end mid-run.
    class_chars: HashSet<char>,
    character_classes: CharacterClasses,
    modeletter: Option<ResolvedTranslation>,
    begmodeword: Option<ResolvedTranslation>,
    begmode: Option<ResolvedTranslation>,
    endmode: Option<ResolvedTranslation>,
    begmodephrase: Option<ResolvedTranslation>,
    endmodephrase: Option<ResolvedTranslation>,
    endmodephrase_before: bool,
    len_phrase: usize,
}

impl ClassIndicator {
    fn new() -> Self {
        ClassIndicator {
            class_chars: HashSet::new(),
            character_classes: CharacterClasses::default(),
            modeletter: None,
            begmodeword: None,
            begmode: None,
            endmode: None,
            begmodephrase: None,
            endmodephrase: None,
            endmodephrase_before: true,
            len_phrase: 2,
        }
    }

    fn is_indicating(&self) -> bool {
        self.modeletter.is_some() || self.begmodeword.is_some() || self.begmode.is_some()
    }

    fn is_active(&self, c: char) -> bool {
        self.class_chars.contains(&c)
    }

    fn is_space(&self, c: char) -> bool {
        self.character_classes.is_whitespace(c)
    }

    /// Splits the input into maximal runs of class-membership ("mode words").
    fn find_words(&self, chars: &[char]) -> Vec<(usize, usize)> {
        super::find_spans(chars, 0, chars.len(), |c| self.is_active(c))
    }

    /// Emits indicators for a single mode word (used both standalone and, when a passage-level
    /// tier isn't available or reached, for each word of a group).
    fn emit_word(&self, open: usize, close: usize, result: &mut Vec<(usize, ResolvedTranslation)>) {
        if close - open == 1
            && let Some(t) = &self.modeletter
        {
            result.push((open, t.clone()));
            return;
        }
        if let Some(t) = &self.begmodeword {
            // A mode word always ends at a clean boundary (see module doc), so no
            // endmodeword is ever needed here.
            result.push((open, t.clone()));
            return;
        }
        if let Some(begt) = &self.begmode {
            result.push((open, begt.clone()));
            if let Some(endt) = &self.endmode {
                result.push((close, endt.clone()));
            }
            return;
        }
        if let Some(t) = &self.modeletter {
            // Fallback when neither begmodeword nor begmode is defined: mark every
            // character in the run individually.
            for pos in open..close {
                result.push((pos, t.clone()));
            }
        }
    }

    /// Emits indicators for a maximal group of consecutive mode words. `linguistic_words` is
    /// the number of real (whitespace-separated) words the group spans.
    fn emit_group(
        &self,
        group: &[(usize, usize)],
        linguistic_words: usize,
        result: &mut Vec<(usize, ResolvedTranslation)>,
    ) {
        if linguistic_words >= self.len_phrase
            && let Some(begphrase) = &self.begmodephrase
        {
            let start_pos = group[0].0;
            let &(last_open, last_close) = group.last().unwrap();
            result.push((start_pos, begphrase.clone()));
            match &self.endmodephrase {
                Some(_) if self.endmodephrase_before => {
                    self.emit_word(last_open, last_close, result);
                }
                Some(endt) => {
                    result.push((last_close, endt.clone()));
                }
                None => {
                    if let Some(endt) = &self.endmode {
                        result.push((last_close, endt.clone()));
                    }
                }
            }
            return;
        }
        for &(open, close) in group {
            self.emit_word(open, close, result);
        }
    }

    fn precompute(&self, chars: &[char]) -> Vec<(usize, ResolvedTranslation)> {
        let mut result = Vec::new();
        if !self.is_indicating() {
            return result;
        }

        let words = self.find_words(chars);
        let mut group: Vec<(usize, usize)> = Vec::new();
        let mut linguistic_words = 0usize;
        let mut prev_close: Option<usize> = None;

        for (open, close) in words {
            let starts_new_linguistic_word = match prev_close {
                Some(prev) => chars[prev..open].iter().any(|&c| self.is_space(c)),
                None => true,
            };
            if starts_new_linguistic_word {
                linguistic_words += 1;
            }
            group.push((open, close));
            prev_close = Some(close);
        }
        if !group.is_empty() {
            self.emit_group(&group, linguistic_words, &mut result);
        }

        result
    }
}

/// Builder for [`Indicator`].
#[derive(Debug)]
pub struct IndicatorBuilder {
    classes: HashMap<String, ClassIndicator>,
}

impl IndicatorBuilder {
    pub fn new() -> Self {
        Self {
            classes: HashMap::new(),
        }
    }

    fn class_mut(&mut self, name: &str) -> &mut ClassIndicator {
        if !self.classes.contains_key(name) {
            self.classes.insert(name.to_string(), ClassIndicator::new());
        }
        self.classes.get_mut(name).unwrap()
    }

    pub fn modeletter(&mut self, name: &str, dots: &str, origin: &AnchoredRule) {
        self.class_mut(name).modeletter = Some(make_translation(dots, origin));
    }

    pub fn begmodeword(&mut self, name: &str, dots: &str, origin: &AnchoredRule) {
        self.class_mut(name).begmodeword = Some(make_translation(dots, origin));
    }

    /// `endmodeword` is accepted (so a table declaring it still parses and compiles) but
    /// never emitted — see the module doc for why a mode word can never end mid-run.
    pub fn endmodeword(&mut self, name: &str, _dots: &str, _origin: &AnchoredRule) {
        self.class_mut(name);
    }

    pub fn begmode(&mut self, name: &str, dots: &str, origin: &AnchoredRule) {
        self.class_mut(name).begmode = Some(make_translation(dots, origin));
    }

    pub fn endmode(&mut self, name: &str, dots: &str, origin: &AnchoredRule) {
        self.class_mut(name).endmode = Some(make_translation(dots, origin));
    }

    pub fn begmodephrase(&mut self, name: &str, dots: &str, origin: &AnchoredRule) {
        self.class_mut(name).begmodephrase = Some(make_translation(dots, origin));
    }

    pub fn endmodephrase(
        &mut self,
        name: &str,
        dots: &str,
        position: &Position,
        origin: &AnchoredRule,
    ) {
        let class = self.class_mut(name);
        class.endmodephrase = Some(make_translation(dots, origin));
        class.endmodephrase_before = matches!(position, Position::Before);
    }

    pub fn lenmodephrase(&mut self, name: &str, len: usize) {
        self.class_mut(name).len_phrase = len;
    }

    pub fn build(self, ctx: &TableContext) -> Option<Indicator> {
        let classes: Vec<ClassIndicator> = self
            .classes
            .into_iter()
            .filter_map(|(name, indicator)| {
                let mut indicator = indicator;
                indicator.character_classes = ctx.character_classes().clone();
                indicator.class_chars = ctx
                    .character_classes()
                    .get(&CharacterClass::from(name.as_str()))
                    .unwrap_or_default();
                Some(indicator)
            })
            .filter(|indicator| indicator.is_indicating())
            .collect();
        if classes.is_empty() {
            None
        } else {
            Some(Indicator { classes })
        }
    }
}

/// Compiled mode indicator for one or more mode classes.
#[derive(Debug, Clone)]
pub struct Indicator {
    classes: Vec<ClassIndicator>,
}

impl Indicator {
    /// Returns sparse `(position, translation)` pairs for the given input.
    pub fn precompute(&self, input: &str) -> Vec<(usize, ResolvedTranslation)> {
        let chars: Vec<char> = input.chars().collect();
        let mut result: Vec<(usize, ResolvedTranslation)> = self
            .classes
            .iter()
            .flat_map(|class| class.precompute(&chars))
            .collect();
        result.sort_by_key(|(pos, _)| *pos);
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{CharacterClass, RuleParser};

    fn rule(s: &str) -> AnchoredRule {
        AnchoredRule::new(RuleParser::new(s).rule().unwrap(), None, 0)
    }

    fn outputs_at(result: &[(usize, ResolvedTranslation)], pos: usize) -> Vec<String> {
        result
            .iter()
            .filter(|(p, _)| *p == pos)
            .map(|(_, t)| t.output().to_string())
            .collect()
    }

    fn digit_ctx() -> TableContext {
        let cc = CharacterClasses::new(&[
            (CharacterClass::Digit, &['1', '2', '3']),
            (CharacterClass::Space, &[' ']),
        ]);
        TableContext::new(cc, CharacterClasses::default(), Default::default())
    }

    #[test]
    fn begmodeword_mirrors_numericmode_test() {
        // see liblouis/tests/yaml/numericmode.yaml
        let mut b = IndicatorBuilder::new();
        b.begmodeword("digit", "⠼", &rule("begmodeword digit 3456"));
        b.endmodeword("digit", "⠰", &rule("endmodeword digit 56"));
        let indicator = b.build(&digit_ctx()).unwrap();

        let result = indicator.precompute("123a");
        assert_eq!(outputs_at(&result, 0), vec!["⠼"]);
        assert!(outputs_at(&result, 3).is_empty());

        let result = indicator.precompute("123 ");
        assert_eq!(outputs_at(&result, 0), vec!["⠼"]);
        assert!(outputs_at(&result, 3).is_empty());
    }

    #[test]
    fn modeletter_for_single_char() {
        let mut b = IndicatorBuilder::new();
        b.modeletter("digit", "⠸", &rule("modeletter digit 6"));
        let indicator = b.build(&digit_ctx()).unwrap();

        let result = indicator.precompute("a1b");
        assert_eq!(outputs_at(&result, 1), vec!["⠸"]);
        assert!(outputs_at(&result, 0).is_empty());
    }

    #[test]
    fn modeletter_repeats_when_no_word_tier() {
        let mut b = IndicatorBuilder::new();
        b.modeletter("digit", "⠸", &rule("modeletter digit 6"));
        let indicator = b.build(&digit_ctx()).unwrap();

        let result = indicator.precompute("123");
        assert_eq!(
            outputs_at(&result, 0)
                .into_iter()
                .chain(outputs_at(&result, 1))
                .chain(outputs_at(&result, 2))
                .collect::<Vec<_>>(),
            vec!["⠸", "⠸", "⠸"]
        );
    }

    #[test]
    fn begmode_endmode_general_passage() {
        let mut b = IndicatorBuilder::new();
        b.begmode("digit", "⠼⠼", &rule("begmode digit 3456-3456"));
        b.endmode("digit", "⠰", &rule("endmode digit 56"));
        let indicator = b.build(&digit_ctx()).unwrap();

        let result = indicator.precompute("a123b");
        assert_eq!(outputs_at(&result, 1), vec!["⠼⠼"]);
        assert_eq!(outputs_at(&result, 4), vec!["⠰"]);
    }

    #[test]
    fn phrase_tier_used_once_length_threshold_reached() {
        let mut b = IndicatorBuilder::new();
        b.begmodeword("digit", "⠼", &rule("begmodeword digit 3456"));
        b.begmodephrase("digit", "⠼⠼⠼", &rule("begmodephrase digit 3456-3456-3456"));
        b.endmode("digit", "⠰", &rule("endmode digit 56"));
        b.lenmodephrase("digit", 2);
        let indicator = b.build(&digit_ctx()).unwrap();

        // Two separate digit words -> reaches the (default-lowered) threshold, uses
        // begmodephrase, falls back to the shared endmode closer (no endmodephrase set).
        let result = indicator.precompute("1 2");
        assert_eq!(outputs_at(&result, 0), vec!["⠼⠼⠼"]);
        assert_eq!(outputs_at(&result, 3), vec!["⠰"]);
    }
}
