//! The display table: how braille cells are shown as characters.
//!
//! A stage of the translation pipeline like [`super::primary::PrimaryTable`] and
//! [`super::multipass::MultipassTable`], but on the braille side of it -- last when
//! translating to braille, first when translating from it. See
//! [`crate::translator::TranslationPipeline::with_display`].

use std::collections::HashMap;

use crate::{
    Direction,
    parser::{AnchoredRule, BrailleChar, HasDirection, Rule},
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
    /// How braille is shown without a braille encoding: as Unicode braille.
    ///
    /// Unicode braille has dots 1 to 8 only, so a cell with virtual dots (9 to F) is shown
    /// without them, as liblouis does when it gives Unicode braille (`ucBrl`). A table
    /// writes a virtual dot to tell cells apart for later rules or for its own display
    /// table, such as `ru-litbrl.ctb`'s ⠁ with dot 9 for Cyrillic а.
    pub fn unicode_braille() -> DisplayTable {
        DisplayTable {
            mappings: HashMap::new(),
        }
    }

    pub fn compile(rules: &[AnchoredRule], direction: Direction) -> DisplayTable {
        let mut mappings = HashMap::new();
        let rules: Vec<_> = rules.iter().filter(|r| r.is_direction(direction)).collect();

        // a display table only takes the `display` rules
        for rule in rules {
            if let Rule::Display {
                character, dots, ..
            } = &rule.rule
            {
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
                // first rule wins, as in liblouis
                mappings.entry(from).or_insert(mapping);
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

    /// The character `c` is shown as, by the table's `display` rule for it or else as
    /// [`Self::unmapped`] says
    fn displayed(&self, c: char) -> char {
        self.mappings
            .get(&c)
            .map_or_else(|| self.unmapped(c), |mapping| mapping.to)
    }

    /// The character `c` is shown as when the table has no `display` rule for it: a cell
    /// with virtual dots as the cell without them, by the table's rule for that or as
    /// Unicode braille. A braille encoding without a rule for `en-ueb-g2.ctb`'s no-break
    /// space, a blank cell with dot A, shows it as a blank cell, not as a private use
    /// character.
    fn unmapped(&self, c: char) -> char {
        match BrailleChar::from_unicode(c) {
            Some(cell) => {
                let plain = cell.without_virtual_dots().to_unicode();
                self.mappings
                    .get(&plain)
                    .map_or(plain, |mapping| mapping.to)
            }
            None => c,
        }
    }

    /// One [`ResolvedTranslation`] per character, so a display table can take part in the
    /// pipeline like any other stage.
    ///
    /// Every mapping is one character to one character, which is what lets
    /// [`PositionMap::from_trace`] compose this stage as the identity it is. A character the
    /// table has no rule for translates as [`Self::unmapped`] says and carries no origin.
    pub fn trace(&self, input: &str) -> Vec<ResolvedTranslation> {
        input
            .chars()
            .map(|c| {
                let (to, origin) = match self.mappings.get(&c) {
                    Some(mapping) => (mapping.to, Some(mapping.origin.clone())),
                    None => (self.unmapped(c), None),
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
    fn unicode_braille_drops_virtual_dots() {
        let unicode = DisplayTable::unicode_braille();
        // dots 1 and 9, dots 1234 and 9, dot 9 alone
        assert_eq!(unicode.translate("\u{f0101}\u{f010f}\u{f0100}"), "⠁⠏⠀");
        // cells without virtual dots, and what isn't braille, stay as they are
        assert_eq!(unicode.translate("⠁⣿ x"), "⠁⣿ x");
    }

    #[test]
    fn display_table() {
        let display_rules = [parse_rule("display a 1"), parse_rule("display \\s 0")];
        let display_table = DisplayTable::compile(&display_rules, Direction::Forward);
        assert_eq!(display_table.translate("⠁"), "a");
        assert_eq!(display_table.translate("⠀"), " ");
        assert_eq!(display_table.translate(""), "");
        // a cell with virtual dots it has no rule for, as the cell without them: dots 1
        // and 9, and en-ueb-g2's no-break space, a blank cell with dot A
        assert_eq!(display_table.translate("\u{f0101}\u{f0200}"), "a ");
        // a cell without any rule, as Unicode braille
        assert_eq!(display_table.translate("\u{f0102}"), "⠂");
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
