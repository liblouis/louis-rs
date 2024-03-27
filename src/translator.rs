use std::collections::HashMap;

use trie::Trie;

use crate::parser::{dots_to_unicode, Braille, Direction, Rule};

use self::trie::Boundary;

mod boundaries;
mod trie;

#[derive(Debug, PartialEq, Clone)]
pub struct Translation {
    from: String,
    to: String,
}

/// Mapping of an input char to the translated output
#[derive(Debug, PartialEq)]
pub struct TranslationMapping<'a> {
    input: &'a str,
    output: &'a str,
}

impl<'a> From<&'a Translation> for TranslationMapping<'a> {
    fn from(translation: &'a Translation) -> Self {
        Self {
            input: &translation.from,
            output: &translation.to,
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
                    &character.to_string(),
                    Translation {
                        from: character.to_string(),
                        to: dots_to_unicode(&dots),
                    },
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
                    &chars,
                    Translation {
                        from: chars.to_string(),
                        to: dots_to_unicode(&dots),
                    },
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
                    &chars,
                    Translation {
                        from: chars.to_string(),
                        to: dots_to_unicode(&dots),
                    },
                    Boundary::Word,
                    Boundary::Word,
                ),
                Rule::Begword {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                } => trie.insert(
                    &chars,
                    Translation {
                        from: chars.to_string(),
                        to: dots_to_unicode(&dots),
                    },
                    Boundary::Word,
                    // Boundary::NotWord, FIXME:
                    Boundary::None,
                ),
                Rule::Sufword {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                } => trie.insert(
                    &chars,
                    Translation {
                        from: chars.to_string(),
                        to: dots_to_unicode(&dots),
                    },
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
                    &chars,
                    Translation {
                        from: chars.to_string(),
                        to: dots_to_unicode(&dots),
                    },
                    Boundary::NotWord, // FIXME:
                    Boundary::NotWord, // FIXME:
                                       // Boundary::None,
                                       // Boundary::None,
                ),
		Rule::Midendword {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                } => trie.insert(
                    &chars,
                    Translation {
                        from: chars.to_string(),
                        to: dots_to_unicode(&dots),
                    },
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
                    &chars,
                    Translation {
                        from: chars.to_string(),
                        to: dots_to_unicode(&dots),
                    },
                    Boundary::None,
                    Boundary::Word,
                ),
                Rule::Begmidword {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                } => trie.insert(
                    &chars,
                    Translation {
                        from: chars.to_string(),
                        to: dots_to_unicode(&dots),
                    },
                    Boundary::None,
		    // Boundary::NotWord, FIXME:
                    Boundary::None,
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
        let default_replacement = "?";
        let mut prev: Option<char> = None;

        while !current.is_empty() {
            let candidates = self.trie.find_translations(current, prev);
            if let Some(t) = candidates.last() {
                let mapping = TranslationMapping::from(*t);
                current = current.strip_prefix(mapping.input).unwrap();
                prev = mapping.input.chars().last();
                translations.push(mapping);
            } else {
                let replacement = match self.undefined {
                    Some(ref r) => &r.to,
                    // FIXME: convert the next char to escape chars
                    // instead of using a constant replacement
                    None => default_replacement,
                };
                prev = current.chars().next();
                let next_char = current.chars().next().unwrap();
                let mapping = TranslationMapping {
                    input: &current[..next_char.len_utf8()],
                    output: replacement,
                };
                current = current.strip_prefix(mapping.input).unwrap();
                translations.push(mapping);
            }
        }
        translations.iter().map(|t| t.output).collect()
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
            if let Rule::Display {
                character, dots, ..
            } = rule
            {
                mapping.insert(dots_to_unicode(&dots).chars().nth(0).unwrap(), character);
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
        assert_eq!(table.translate("🐂"), "?");
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
        assert_eq!(table.translate("foobarfoo"), "⠉⠑⠉"); // foo and bar should be  contracted
        assert_eq!(table.translate("foobar foo"), "⠉⠂⠁⠐⠀⠉"); // only foo should be  contracted
        assert_eq!(table.translate("foo bar foo"), "⠉⠀⠂⠁⠐⠀⠉"); // only foo should be  contracted
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
}
