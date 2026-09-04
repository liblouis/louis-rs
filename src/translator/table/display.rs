//! The display table: how braille cells are shown as characters.
//!
//! A stage of the translation pipeline like [`super::primary::PrimaryTable`] and
//! [`super::multipass::MultipassTable`], but on the braille side of it -- last when
//! translating to braille, first when translating from it. See
//! [`crate::translator::TranslationPipeline::with_display`].

use std::collections::HashMap;

use crate::{
    Direction,
    parser::{AnchoredRule, HasDirection, Rule},
    translator::{ResolvedTranslation, TranslationStage},
};

/// What one braille cell is shown as, and the `display` rule that says so.
///
#[derive(Debug)]
struct DisplayMapping {
    to: char,
    // FIXME: unboxed AnchoredRule inflates the HashMap value, measured slower than the
    // Box<AnchoredRule> version (cargo bench --bench translate). Display tables are small and
    // char-keyed, so a sorted Vec<(char, DisplayMapping)> + binary_search (or a fixed-size array
    // keyed by dot pattern) would likely beat re-boxing if this shows up as a hot path.
    origin: AnchoredRule,
}

#[derive(Debug)]
pub struct DisplayTable {
    mappings: HashMap<char, DisplayMapping>,
}

impl DisplayTable {
    pub fn compile(rules: &[AnchoredRule], direction: Direction) -> DisplayTable {
        let mut mappings = HashMap::new();
        let rules: Vec<_> = rules.iter().filter(|r| r.is_direction(direction)).collect();

        for rule in rules {
            match &rule.rule {
                Rule::Display {
                    character, dots, ..
                } => {
                    let braille = dots.to_unicode();
                    // swap `from` and `to` for backwards translation
                    let (from, to) = match direction {
                        Direction::Forward => (braille, *character),
                        Direction::Backward => (*character, braille),
                    };
                    let mapping = DisplayMapping {
                        to,
                        origin: rule.clone(),
                    };
                    if cfg!(feature = "backwards_compatibility") {
                        // first rule wins
                        mappings.entry(from).or_insert(mapping);
                    } else {
                        // last rule wins
                        mappings.insert(from, mapping);
                    }
                }
                _ => (), // ignore all other rules for display tables
            }
        }
        DisplayTable { mappings }
    }

    /// Map the `input` to the output using the display rules in the
    /// `DisplayTable`.
    ///
    /// If the `DisplayTable` does not contain a mapping for a
    /// specific char then the original character is returned
    pub fn translate(&self, input: &str) -> String {
        input.chars().map(|c| self.displayed(c)).collect()
    }

    /// The character `c` is shown as, or `c` itself when the table has no mapping for it.
    fn displayed(&self, c: char) -> char {
        self.mappings.get(&c).map_or(c, |mapping| mapping.to)
    }

    /// One [`ResolvedTranslation`] per character, so a display table can take part in the
    /// pipeline like any other stage.
    ///
    /// Every mapping is one character to one character, which is what lets
    /// [`PositionMap::from_trace`] compose this stage as the identity it is. A character the
    /// table has no rule for translates to itself and carries no origin.
    pub fn trace(&self, input: &str) -> Vec<ResolvedTranslation> {
        input
            .chars()
            .map(|c| {
                let (to, origin) = match self.mappings.get(&c) {
                    Some(mapping) => (mapping.to, Some(mapping.origin.clone())),
                    None => (c, None),
                };
                ResolvedTranslation::new(
                    &c.to_string(),
                    &to.to_string(),
                    1,
                    TranslationStage::Display,
                    origin,
                )
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{
        parser::RuleParser, translator::table::TableContext,
        translator::table::primary::PrimaryTable,
    };

    fn parse_rule(source: &str) -> AnchoredRule {
        RuleParser::new(source).rule().unwrap().into()
    }

    #[test]
    fn display_table() {
        let display_rules = [parse_rule("display a 1"), parse_rule("display \\s 0")];
        let display_table = DisplayTable::compile(&display_rules, Direction::Forward);
        assert_eq!(display_table.translate("⠁"), "a");
        assert_eq!(display_table.translate("⠀"), " ");
        assert_eq!(display_table.translate(""), "");
        assert_eq!(display_table.translate("x"), "x"); // unknown chars are translated to themselves
    }

    #[test]
    fn translate_with_display() {
        let display_rules = [parse_rule("display A 1"), parse_rule("display \\s 0")];
        let rules = [parse_rule("letter a 1"), parse_rule("space \\s 0")];
        let display_table = DisplayTable::compile(&display_rules, Direction::Forward);
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(display_table.translate(&table.translate("a")), "A");
        assert_eq!(display_table.translate(&table.translate(" ")), " ");
        assert_eq!(display_table.translate(&table.translate("a a")), "A A");
    }
}
