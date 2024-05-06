use log::warn;
use std::{borrow::Cow, collections::HashMap};

use trie::Trie;

use crate::parser::{dots_to_unicode, fallback, AnchoredRule, Braille, Direction, Rule};

use self::trie::Boundary;

mod boundaries;
mod trie;

#[derive(Debug, PartialEq, Clone)]
pub struct Translation {
    input: String,
    output: String,
    /// number of chars in `input`
    length: usize,
    /// the length of the match in chars including word boundaries
    weight: usize,
}

impl Translation {
    pub fn new(input: String, output: String, weight: usize) -> Self {
        let length = input.chars().count();
        Self {
            input,
            output,
            weight,
            length,
        }
    }
}

#[derive(Debug)]
struct CharacterDefinition(HashMap<char, Translation>);

impl CharacterDefinition {
    fn new() -> Self {
        Self(HashMap::new())
    }

    fn insert(&mut self, c: char, translation: Translation) {
        if cfg!(feature = "backwards_compatibility") {
            // first rule wins
            self.0.entry(c).or_insert(translation);
        } else {
            // last rule wins
            self.0.insert(c, translation);
        }
    }

    fn get(&self, c: &char) -> Option<&Translation> {
        self.0.get(c)
    }
}

#[derive(Debug)]
pub struct TranslationTable {
    undefined: Option<String>,
    character_definitions: CharacterDefinition,
    translations: Trie,
    direction: Direction,
}

impl TranslationTable {
    pub fn compile(rules: Vec<AnchoredRule>, direction: Direction) -> Self {
        let mut undefined = None;
        let mut character_definitions = CharacterDefinition::new();
        let mut translations = Trie::new();

        let rules: Vec<AnchoredRule> = rules
            .into_iter()
            .filter(|r| r.rule.is_direction(direction))
            .collect();

        for rule in &rules {
            match &rule.rule {
                Rule::Undefined { dots } => {
                    undefined = Some(dots_to_unicode(dots));
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
                    character_definitions.insert(
                        *character,
                        Translation::new(character.to_string(), dots_to_unicode(dots), 1),
                    );
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
                    dots_to_unicode(dots),
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
                    dots_to_unicode(dots),
                    Boundary::Word,
                    Boundary::Word,
                ),
                Rule::Begword {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                } => translations.insert(
                    chars.to_string(),
                    dots_to_unicode(dots),
                    Boundary::Word,
                    Boundary::NotWord,
                ),
                Rule::Sufword {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                } => translations.insert(
                    chars.to_string(),
                    dots_to_unicode(dots),
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
                    dots_to_unicode(dots),
                    Boundary::NotWord,
                    Boundary::NotWord,
                ),
                Rule::Midendword {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                } => translations.insert(
                    chars.to_string(),
                    dots_to_unicode(dots),
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
                    dots_to_unicode(dots),
                    Boundary::None,
                    Boundary::Word,
                ),
                Rule::Begmidword {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                } => translations.insert(
                    chars.to_string(),
                    dots_to_unicode(dots),
                    Boundary::None,
                    Boundary::NotWord,
                ),
                _ => (),
            }
        }

        // use a second pass through the rules to resolve the base opcodes and
        // `=` arguments in rules
        for rule in rules {
            match rule.rule {
                Rule::Base { derived, base, .. } => {
                    if let Some(translation) = character_definitions.get(&base) {
                        character_definitions.insert(
                            derived,
                            Translation {
                                input: derived.to_string(),
                                ..translation.clone()
                            },
                        );
                    } else {
                        warn!("Character in base rule not defined: derived: {}, base: {}, direction: {:?}",
                            derived, base, direction);
                    }
                }
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
        let mut translations: Vec<Cow<Translation>> = Vec::new();
        let mut chars = input.chars();
        let mut prev: Option<char> = None;

        loop {
            let candidates = self.translations.find_translations(chars.as_str(), prev);
            if let Some(t) = candidates.last() {
                // there is a matching translation rule
                let translation = Cow::Borrowed(*t);
                // move the iterator forward by the number of characters in the translation
                chars.nth(t.length - 1);
                prev = translation.input.chars().last();
                translations.push(translation);
            } else if let Some(next_char) = chars.next() {
                // no translation rule found; try character definition rules
                prev = Some(next_char);
                if let Some(translation) = self.character_definitions.get(&next_char) {
                    // there is a matching character definition for the next character
                    let translation = Cow::Borrowed(translation);
                    translations.push(translation);
                } else if let Some(ref replacement) = self.undefined {
                    // there is a rule for undefined characters
                    let translation =
                        Translation::new(next_char.to_string(), replacement.to_string(), 1);
                    let translation: Cow<'_, Translation> = Cow::Owned(translation);
                    translations.push(translation);
                } else {
                    // otherwise handle it as a undefined character
                    let translation = Translation::new(
                        next_char.to_string(),
                        self.handle_undefined_char(next_char),
                        1,
                    );
                    let translation: Cow<'_, Translation> = Cow::Owned(translation);
                    translations.push(translation);
                }
            } else {
                // the chars iterator is exhausted
                break;
            }
        }
        translations.iter().map(|t| t.output.as_str()).collect()
    }

    fn handle_undefined_char(&self, ch: char) -> String {
        ch.escape_unicode()
            .to_string()
            .replace(r"\u", r"\x") // replace \u by \x
            .replace(['{', '}'], "") // drop the curly braces
            .chars()
            .map(|c| {
                if let Some(t) = self.character_definitions.get(&c) {
                    t.output.clone()
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
    pub fn compile(rules: Vec<AnchoredRule>, direction: Direction) -> DisplayTable {
        let mut mapping = HashMap::new();
        let rules: Vec<_> = rules
            .into_iter()
            .filter(|r| r.rule.is_direction(direction))
            .collect();

        for rule in rules {
            match rule.rule {
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

    fn parse_rule(source: &str) -> AnchoredRule {
        RuleParser::new(source).rule().unwrap().into()
    }

    #[test]
    fn translate_test() {
        let rules = vec![
            parse_rule("always foo 123"),
            parse_rule("always bar 456"),
            parse_rule("space \\s 0"),
        ];
        let table = TranslationTable::compile(rules, Direction::Forward);
        assert_eq!(table.translate("foobar"), "⠇⠸");
        assert_eq!(table.translate("  "), "⠀⠀");
        assert_eq!(table.translate("🐂"), "⠳⠭⠂⠋⠲⠴⠆");
    }

    #[test]
    fn midword_test() {
        let rules = vec![
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 2"),
            parse_rule("lowercase f 3"),
            parse_rule("lowercase o 4"),
            parse_rule("lowercase r 5"),
            parse_rule("always foo 14"),
            parse_rule("midword bar 15"),
            parse_rule("space \\s 0"),
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
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 2"),
            parse_rule("lowercase f 3"),
            parse_rule("lowercase o 4"),
            parse_rule("lowercase r 5"),
            parse_rule("always foo 14"),
            parse_rule("always bar 24"),
            parse_rule("midword bar 26"),
            parse_rule("space \\s 0"),
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
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 2"),
            parse_rule("lowercase f 3"),
            parse_rule("lowercase o 4"),
            parse_rule("lowercase r 5"),
            parse_rule("punctuation . 6"),
            parse_rule("always foo 14"),
            parse_rule("endword bar 15"),
            parse_rule("space \\s 0"),
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
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 2"),
            parse_rule("lowercase f 3"),
            parse_rule("lowercase o 4"),
            parse_rule("lowercase r 5"),
            parse_rule("punctuation . 6"),
            parse_rule("always foo 14"),
            parse_rule("partword bar 15"),
            parse_rule("space \\s 0"),
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
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 12"),
            parse_rule("base uppercase A a"),
        ];
        let table = TranslationTable::compile(rules, Direction::Forward);
        assert_eq!(table.translate("a"), "⠁");
        assert_eq!(table.translate("A"), "⠁");
        assert_eq!(table.translate("ab"), "⠁⠃");
        assert_eq!(table.translate("Ab"), "⠁⠃");
    }

    #[test]
    fn display_table_test() {
        let display_rules = vec![parse_rule("display a 1"), parse_rule("display \\s 0")];
        let display_table = DisplayTable::compile(display_rules, Direction::Forward);
        assert_eq!(display_table.translate("⠁"), "a");
        assert_eq!(display_table.translate("⠀"), " ");
        assert_eq!(display_table.translate(""), "");
        assert_eq!(display_table.translate("x"), "x"); // unknown chars are translated to themselves
    }

    #[test]
    fn translate_with_display_test() {
        let display_rules = vec![parse_rule("display A 1"), parse_rule("display \\s 0")];
        let rules = vec![parse_rule("letter a 1"), parse_rule("space \\s 0")];
        let display_table = DisplayTable::compile(display_rules, Direction::Forward);
        let table = TranslationTable::compile(rules, Direction::Forward);
        assert_eq!(display_table.translate(&table.translate("a")), "A");
        assert_eq!(display_table.translate(&table.translate(" ")), " ");
        assert_eq!(display_table.translate(&table.translate("a a")), "A A");
    }
}
