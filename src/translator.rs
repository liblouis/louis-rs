use std::collections::HashMap;

use trie::Trie;

use crate::parser::{dots_to_unicode, fallback, Braille, Direction, Rule};

use self::trie::Boundary;

mod boundaries;
mod trie;

#[derive(Debug, PartialEq)]
pub struct Translation {
    from: String,
    to: String,
    length: usize,
}

impl Translation {
    pub fn new(from: String, to: String, length: usize) -> Self {
        Self { from, to, length }
    }
}

/// Mapping of an input char to the translated output
#[derive(Debug, PartialEq)]
pub struct TranslationMapping<'a> {
    input: &'a str,
    output: String,
}

impl<'a> From<&'a Translation> for TranslationMapping<'a> {
    fn from(translation: &'a Translation) -> Self {
        Self {
            input: &translation.from,
            output: translation.to.clone(),
        }
    }
}

#[derive(Debug)]
pub struct TranslationTable {
    undefined: Option<Translation>,
    character_definitions: HashMap<char, String>,
    translations: Trie,
    direction: Direction,
}

impl TranslationTable {
    pub fn compile(rules: Vec<Rule>, direction: Direction) -> Self {
        let mut undefined = None;
        let mut character_definitions = HashMap::new();
        let mut translations = Trie::new();

        let rules: Vec<Rule> = rules
            .into_iter()
            .filter(|r| r.is_direction(direction))
            .collect();

        for rule in rules {
            match rule {
                Rule::Undefined { dots } => {
                    undefined = Some(Translation::new("".into(), dots_to_unicode(&dots), 0));
                }
                Rule::Space {
                    character, dots, ..
                }
                | Rule::Punctuation {
                    character, dots, ..
                }
                | Rule::Digit {
                    character, dots, ..
                }
                | Rule::Letter {
                    character,
                    dots: Braille::Explicit(dots),
                    ..
                }
                | Rule::Lowercase {
                    character, dots, ..
                }
                | Rule::Uppercase {
                    character, dots, ..
                }
                | Rule::Litdigit {
                    character, dots, ..
                }
                | Rule::Sign {
                    character, dots, ..
                }
                | Rule::Math {
                    character, dots, ..
                } => {
                    character_definitions.insert(character, dots_to_unicode(&dots));
                }
                Rule::Base { from, to, .. } => {
                    if let Some(dots) = character_definitions.get(&to) {
                        character_definitions.insert(from, dots.to_string());
                    } else {
                        // FIXME: return an error here instead of logging
                        eprintln!(
                            "Character in base rule not defined: from: {}, to: {}",
                            from, to
                        );
                    }
                }
                Rule::Comp6 {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                }
                | Rule::Always {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                } => translations.insert(
                    chars.to_string(),
                    dots_to_unicode(&dots),
                    Boundary::None,
                    Boundary::None,
                ),
                Rule::Word {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                }
                | Rule::Joinword { chars, dots, .. }
                | Rule::Lowword { chars, dots, .. } => translations.insert(
                    chars.to_string(),
                    dots_to_unicode(&dots),
                    Boundary::Word,
                    Boundary::Word,
                ),
                Rule::Begword {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                } => translations.insert(
                    chars.to_string(),
                    dots_to_unicode(&dots),
                    Boundary::Word,
                    Boundary::NotWord,
                ),
                Rule::Sufword {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                } => translations.insert(
                    chars.to_string(),
                    dots_to_unicode(&dots),
                    Boundary::Word,
                    Boundary::None,
                ),
                Rule::Midword {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                }
                | Rule::Partword {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                } => translations.insert(
                    chars.to_string(),
                    dots_to_unicode(&dots),
                    Boundary::NotWord,
                    Boundary::NotWord,
                ),
                Rule::Midendword {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                } => translations.insert(
                    chars.to_string(),
                    dots_to_unicode(&dots),
                    Boundary::NotWord,
                    Boundary::None,
                ),
                Rule::Endword {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                }
                | Rule::Prfword {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                } => translations.insert(
                    chars.to_string(),
                    dots_to_unicode(&dots),
                    Boundary::None,
                    Boundary::Word,
                ),
                Rule::Begmidword {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                } => translations.insert(
                    chars.to_string(),
                    dots_to_unicode(&dots),
                    Boundary::None,
                    Boundary::NotWord,
                ),
                _ => (),
            }
        }
        TranslationTable {
            undefined,
            direction,
            character_definitions,
            translations,
        }
    }

    pub fn translate(&self, input: &str) -> String {
        let mut translations: Vec<TranslationMapping> = Vec::new();
        let mut current = input;
        let mut prev: Option<char> = None;

        while !current.is_empty() {
            // is there a matching translation rule
            let candidates = self.translations.find_translations(current, prev);
            if let Some(t) = candidates.last() {
                let mapping = TranslationMapping::from(*t);
                let length = current
                    .chars()
                    .take(mapping.input.chars().count())
                    .map(|c| c.len_utf8())
                    .sum();
                current = &current[length..];
                prev = mapping.input.chars().last();
                translations.push(mapping);
            } else {
                prev = current.chars().next();
                let next_char = current.chars().next().unwrap();
                // or is there a character definition for the next character?
                if let Some(translation) = self.character_definitions.get(&next_char) {
                    let mapping = TranslationMapping {
                        input: &current[..next_char.len_utf8()],
                        output: translation.to_string(),
                    };
                    current = current.strip_prefix(mapping.input).unwrap();
                    translations.push(mapping);
                } else {
                    // or is there rule for undefined characters
                    let replacement = match self.undefined {
                        Some(ref r) => r.to.clone(),
                        // if all else fails
                        None => self.handle_undefined_char(next_char),
                    };
                    let mapping = TranslationMapping {
                        input: &current[..next_char.len_utf8()],
                        output: replacement,
                    };
                    current = current.strip_prefix(mapping.input).unwrap();
                    translations.push(mapping);
                }
            }
        }
        translations.into_iter().map(|t| t.output).collect()
    }

    fn handle_undefined_char(&self, ch: char) -> String {
        ch.escape_unicode()
            .to_string()
            .replace(r"\u", r"\x") // replace \u by \x
            .replace(['{', '}'], "") // drop the curly braces
            .chars()
            .map(|c| {
                if let Some(s) = self.character_definitions.get(&c) {
                    s.clone()
                } else {
                    fallback(c).to_string()
                }
            })
            .collect()
    }
}

#[derive(Debug)]
pub struct DisplayTable {
    dots_to_char: HashMap<char, char>,
}

impl DisplayTable {
    pub fn compile(rules: Vec<Rule>, direction: Direction) -> DisplayTable {
        let mut mapping = HashMap::new();
        let rules: Vec<Rule> = rules
            .into_iter()
            .filter(|r| r.is_direction(direction))
            .collect();

        for rule in rules {
            match rule {
                Rule::Display {
                    character, dots, ..
                } => {
                    if cfg!(feature = "backwards_compatibility") {
                        // first rule wins
                        let key = dots_to_unicode(&dots).chars().nth(0).unwrap();
                        mapping.entry(key).or_insert(character);
                    } else {
                        // last rule wins
                        mapping.insert(dots_to_unicode(&dots).chars().nth(0).unwrap(), character);
                    }
                }
                _ => (), // ignore all other rules for display tables
            }
        }
        DisplayTable {
            dots_to_char: mapping,
        }
    }

    /// Map the `input` to the output using the display rules in the
    /// `DisplayTable`.
    ///
    /// If the `DisplayTable` does not contain a mapping for a
    /// specific char then the original character is returned
    pub fn translate(&self, input: &str) -> String {
        input
            .chars()
            .map(|ref c| *self.dots_to_char.get(c).unwrap_or(c))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::RuleParser;

    #[test]
    fn translate_test() {
        let rules = vec![
            RuleParser::new("always foo 123").rule().unwrap(),
            RuleParser::new("always bar 456").rule().unwrap(),
            RuleParser::new("space \\s 0").rule().unwrap(),
        ];
        let table = TranslationTable::compile(rules, Direction::Forward);
        assert_eq!(table.translate("foobar"), "⠇⠸");
        assert_eq!(table.translate("  "), "⠀⠀");
        assert_eq!(table.translate("🐂"), "⠳⠭⠂⠋⠲⠴⠆");
    }

    #[test]
    fn midword_test() {
        let rules = vec![
            RuleParser::new("lowercase a 1").rule().unwrap(),
            RuleParser::new("lowercase b 2").rule().unwrap(),
            RuleParser::new("lowercase f 3").rule().unwrap(),
            RuleParser::new("lowercase o 4").rule().unwrap(),
            RuleParser::new("lowercase r 5").rule().unwrap(),
            RuleParser::new("always foo 14").rule().unwrap(),
            RuleParser::new("midword bar 15").rule().unwrap(),
            RuleParser::new("space \\s 0").rule().unwrap(),
        ];
        let table = TranslationTable::compile(rules, Direction::Forward);
        assert_eq!(table.translate("bar"), "⠂⠁⠐"); // should not contract
        assert_eq!(table.translate("foobar"), "⠉⠂⠁⠐"); // only foo should be contracted
        assert_eq!(table.translate("foobarfoo"), "⠉⠑⠉"); // foo and bar should be contracted
        assert_eq!(table.translate("foobar foo"), "⠉⠂⠁⠐⠀⠉"); // only foo should be contracted
        assert_eq!(table.translate("foo bar foo"), "⠉⠀⠂⠁⠐⠀⠉"); // only foo should be contracted
    }

    #[test]
    fn midword_with_precedence_test() {
        let rules = vec![
            RuleParser::new("lowercase a 1").rule().unwrap(),
            RuleParser::new("lowercase b 2").rule().unwrap(),
            RuleParser::new("lowercase f 3").rule().unwrap(),
            RuleParser::new("lowercase o 4").rule().unwrap(),
            RuleParser::new("lowercase r 5").rule().unwrap(),
            RuleParser::new("always foo 14").rule().unwrap(),
            RuleParser::new("always bar 24").rule().unwrap(),
            RuleParser::new("midword bar 26").rule().unwrap(),
            RuleParser::new("space \\s 0").rule().unwrap(),
        ];
        let table = TranslationTable::compile(rules, Direction::Forward);
        assert_eq!(table.translate("bar"), "⠊"); // bar should contract with 24
        assert_eq!(table.translate("foobar"), "⠉⠊"); // bar should contract with 24
        assert_eq!(table.translate("foobarfoo"), "⠉⠢⠉"); // bar should contract with 26
        assert_eq!(table.translate("foobar foo"), "⠉⠊⠀⠉"); // bar should contract with 24
        assert_eq!(table.translate("foo bar foo"), "⠉⠀⠊⠀⠉"); // bar should contract with 24
    }

    #[test]
    fn endword_test() {
        let rules = vec![
            RuleParser::new("lowercase a 1").rule().unwrap(),
            RuleParser::new("lowercase b 2").rule().unwrap(),
            RuleParser::new("lowercase f 3").rule().unwrap(),
            RuleParser::new("lowercase o 4").rule().unwrap(),
            RuleParser::new("lowercase r 5").rule().unwrap(),
            RuleParser::new("punctuation . 6").rule().unwrap(),
            RuleParser::new("always foo 14").rule().unwrap(),
            RuleParser::new("endword bar 15").rule().unwrap(),
            RuleParser::new("space \\s 0").rule().unwrap(),
        ];
        let table = TranslationTable::compile(rules, Direction::Forward);
        assert_eq!(table.translate("bar"), "⠑"); // should contract
        assert_eq!(table.translate("foobar"), "⠉⠑"); // both should be contracted
        assert_eq!(table.translate("foobar."), "⠉⠑⠠"); // both should be contracted
        assert_eq!(table.translate("foobarfoo"), "⠉⠂⠁⠐⠉"); // only foo should be contracted
        assert_eq!(table.translate("foobar foo"), "⠉⠑⠀⠉"); // both should be contracted
        assert_eq!(table.translate("foo bar foo"), "⠉⠀⠑⠀⠉"); // both should be contracted
    }

    #[test]
    fn partword_test() {
        let rules = vec![
            RuleParser::new("lowercase a 1").rule().unwrap(),
            RuleParser::new("lowercase b 2").rule().unwrap(),
            RuleParser::new("lowercase f 3").rule().unwrap(),
            RuleParser::new("lowercase o 4").rule().unwrap(),
            RuleParser::new("lowercase r 5").rule().unwrap(),
            RuleParser::new("punctuation . 6").rule().unwrap(),
            RuleParser::new("always foo 14").rule().unwrap(),
            RuleParser::new("partword bar 15").rule().unwrap(),
            RuleParser::new("space \\s 0").rule().unwrap(),
        ];
        let table = TranslationTable::compile(rules, Direction::Forward);
        assert_eq!(table.translate("bar"), "⠂⠁⠐"); // bar should not be contracted
        assert_eq!(table.translate("foobar"), "⠉⠂⠁⠐"); // bar should not be contracted
        assert_eq!(table.translate("foobar."), "⠉⠂⠁⠐⠠"); // bar should not be contracted
        assert_eq!(table.translate("foobarfoo"), "⠉⠑⠉"); // bar should be contracted
        assert_eq!(table.translate("foobar foo"), "⠉⠂⠁⠐⠀⠉"); // bar should not be contracted
        assert_eq!(table.translate("foobar. foo"), "⠉⠂⠁⠐⠠⠀⠉"); // bar should not be contracted
        assert_eq!(table.translate("foo bar foo"), "⠉⠀⠂⠁⠐⠀⠉"); // bar should not be contracted
    }

    #[test]
    fn base_test() {
        let rules = vec![
            RuleParser::new("lowercase a 1").rule().unwrap(),
            RuleParser::new("lowercase b 12").rule().unwrap(),
            RuleParser::new("base uppercase A a").rule().unwrap(),
        ];
        let table = TranslationTable::compile(rules, Direction::Forward);
        assert_eq!(table.translate("a"), "⠁");
        assert_eq!(table.translate("A"), "⠁");
        assert_eq!(table.translate("ab"), "⠁⠃");
        assert_eq!(table.translate("Ab"), "⠁⠃");
    }

    #[test]
    fn display_table_test() {
        let display_rules = vec![
            RuleParser::new("display a 1").rule().unwrap(),
            RuleParser::new("display \\s 0").rule().unwrap(),
        ];
        let display_table = DisplayTable::compile(display_rules, Direction::Forward);
        assert_eq!(display_table.translate("⠁"), "a");
        assert_eq!(display_table.translate("⠀"), " ");
        assert_eq!(display_table.translate(""), "");
        assert_eq!(display_table.translate("x"), "x"); // unknown chars are translated to themselves
    }

    #[test]
    fn translate_with_display_test() {
        let display_rules = vec![
            RuleParser::new("display A 1").rule().unwrap(),
            RuleParser::new("display \\s 0").rule().unwrap(),
        ];
        let rules = vec![
            RuleParser::new("letter a 1").rule().unwrap(),
            RuleParser::new("space \\s 0").rule().unwrap(),
        ];
        let display_table = DisplayTable::compile(display_rules, Direction::Forward);
        let table = TranslationTable::compile(rules, Direction::Forward);
        assert_eq!(display_table.translate(&table.translate("a")), "A");
        assert_eq!(display_table.translate(&table.translate(" ")), " ");
        assert_eq!(display_table.translate(&table.translate("a a")), "A A");
    }
}
