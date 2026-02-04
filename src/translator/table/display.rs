use std::collections::HashMap;

use crate::{
    Direction,
    parser::{AnchoredRule, HasDirection, Rule},
    translator::{ResolvedTranslation, TranslationStage},
};

#[derive(Debug)]
pub struct DisplayTable {
    dots_to_char: HashMap<char, char>,
}

impl DisplayTable {
    pub fn compile(rules: &[AnchoredRule], direction: Direction) -> DisplayTable {
        let mut mapping = HashMap::new();
        let rules: Vec<_> = rules.iter().filter(|r| r.is_direction(direction)).collect();

        for rule in rules {
            match &rule.rule {
                Rule::Display {
                    character, dots, ..
                } => {
                    if cfg!(feature = "backwards_compatibility") {
                        // first rule wins
                        let key = dots.to_string().chars().nth(0).unwrap();
                        mapping.entry(key).or_insert(*character);
                    } else {
                        // last rule wins
                        mapping.insert(dots.to_string().chars().nth(0).unwrap(), *character);
                    }
                }
                _ => (), // ignore all other rules for display tables
            }
        }
        DisplayTable {
            dots_to_char: mapping,
        }
    }

    pub fn trace(&self, input: &str) -> Vec<ResolvedTranslation> {
        input
            .chars()
            .map(|ref c| {
                ResolvedTranslation::new(
                    c.to_string().as_str(),
                    self.dots_to_char.get(c).unwrap_or(c).to_string().as_str(),
                    1,
                    TranslationStage::Display,
                    None,
                )
            })
            .collect()
    }

    pub fn translate(&self, input: &str) -> String {
        self.trace(input).iter().map(|t| t.output()).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parser::RuleParser;

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
        assert_eq!(display_table.translate("x"), "x"); // unknown chars translate to themselves
    }
}
