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
    trie: Trie,
    direction: Direction,
}

impl TranslationTable {
    pub fn compile(rules: Vec<Rule>, direction: Direction) -> Self {
        let mut undefined = None;
        let mut trie = Trie::new();

        let rules: Vec<Rule> = rules
            .into_iter()
            .filter(|r| r.is_direction(direction))
            .collect();

        for rule in rules {
            match rule {
                Rule::Undefined { dots } => {
                    undefined = Some(Translation {
                        from: "".into(),
                        to: dots_to_unicode(&dots),
                        length: 0,
                    });
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
                } => trie.insert(
                    character.to_string(),
                    dots_to_unicode(&dots),
                    Boundary::None,
                    Boundary::None,
                ),
                Rule::Comp6 {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                }
                | Rule::Always {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                } => trie.insert(
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
                | Rule::Lowword { chars, dots, .. } => trie.insert(
                    chars.to_string(),
                    dots_to_unicode(&dots),
                    Boundary::Word,
                    Boundary::Word,
                ),
                Rule::Begword {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                } => trie.insert(
                    chars.to_string(),
                    dots_to_unicode(&dots),
                    Boundary::Word,
                    Boundary::NotWord,
                ),
                Rule::Sufword {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                } => trie.insert(
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
                } => trie.insert(
                    chars.to_string(),
                    dots_to_unicode(&dots),
                    Boundary::NotWord,
                    Boundary::NotWord,
                ),
                Rule::Midendword {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                } => trie.insert(
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
                } => trie.insert(
                    chars.to_string(),
                    dots_to_unicode(&dots),
                    Boundary::None,
                    Boundary::Word,
                ),
                Rule::Begmidword {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                } => trie.insert(
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
            trie,
        }
    }

    pub fn translate(&self, input: &str) -> String {
        let mut translations: Vec<TranslationMapping> = Vec::new();
        let mut current = input;
        let mut prev: Option<char> = None;

        while !current.is_empty() {
            let candidates = self.trie.find_translations(current, prev);
            if let Some(t) = candidates.last() {
                let mapping = TranslationMapping::from(*t);
                current = current.strip_prefix(mapping.input).unwrap();
                prev = mapping.input.chars().last();
                translations.push(mapping);
            } else {
                prev = current.chars().next();
                let next_char = current.chars().next().unwrap();
                let replacement = match self.undefined {
                    Some(ref r) => r.to.clone(),
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
        translations.into_iter().map(|t| t.output).collect()
    }

    /// Return a mapping for character `ch` if there is one in the table
    fn character_definition(&self, ch: char) -> Option<char> {
        let candidates = self.trie.find_translations(&ch.to_string(), None);
        match candidates.last() {
            Some(translation) => translation.to.chars().next(),
            None => None,
        }
    }

    fn handle_undefined_char(&self, ch: char) -> String {
        ch.escape_unicode()
            .to_string()
            .replace(r"\u", r"\x") // replace \u by \x
            .replace(['{', '}'], "") // drop the curly braces
            .chars()
            .map(|c| self.character_definition(c).unwrap_or_else(|| fallback(c)))
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
