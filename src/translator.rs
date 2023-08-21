//! Translate text to braille using liblouis translation tables.

use std::collections::HashMap;

use serde::Deserialize;

use crate::parser::dots_to_unicode;
use crate::parser::BrailleCharsOrImplicit;
use crate::parser::Rule;
use crate::translator::character::CharacterAttributes;

mod character;
mod indication;

type Corrections = HashMap<String, String>;
type CharacterDefinitions = HashMap<char, String>;
type DisplayDefinitions = HashMap<char, char>;
type Translations = HashMap<String, String>;

/// Mapping of an input char to the translated output
#[derive(Debug, PartialEq)]
pub struct TranslationMapping<'a> {
    input: &'a str,
    output: &'a str,
}

impl<'a> TranslationMapping<'a> {
    fn new(input: &'a str, output: &'a str) -> TranslationMapping<'a> {
        TranslationMapping { input, output }
    }
}

/// Mode of a translation
enum TranslationMode {
    Numeric,
    AllCaps,
    Caps,
}

/// Context of a translation
///
/// Contains information that is needed to do the translation, such as
/// whether the previous character is a number, etc
struct TranslationContext {
    prev_char: char,
    mode: TranslationMode,
}

#[derive(Default, Debug, PartialEq, Eq, Deserialize, Hash, Clone, clap::ValueEnum)]
pub enum Direction {
    #[default]
    Forward,
    Backward,
}

#[derive(Debug, PartialEq)]
pub struct DisplayTable {
    dots_to_char: DisplayDefinitions,
}

impl DisplayTable {
    pub fn compile(rules: Vec<Rule>) -> DisplayTable {
        let mut mapping = DisplayDefinitions::new();
        for rule in rules {
            if let Rule::Display { ch, dots, .. } = rule {
                mapping.insert(dots_to_unicode(dots).chars().nth(0).unwrap(), ch);
            }
        }
        DisplayTable {
            dots_to_char: mapping,
        }
    }
    /// Map the `input` to the output using the display rules in the
    /// `DisplayTable`.
    ///
    /// if the `DisplayTable` does not contain a mapping for a
    /// specific char then the original character is returned
    pub fn translate(&self, input: &str) -> String {
        input
            .chars()
            .map(|c| *self.dots_to_char.get(&c).unwrap_or(&c))
            .collect()
    }
}

/// A Translation table holds all the rules needed to do a braille translation
///
/// Contains static information that is needed to do the translation,
/// such as character definitions, translation rules, etc
#[derive(Debug, Default, PartialEq)]
pub struct TranslationTable {
    undefined: String,
    corrections: Corrections,
    character_definitions: CharacterDefinitions,
    character_attributes: CharacterAttributes,
    translations: Translations,
}

impl TranslationTable {
    pub fn compile(rules: Vec<Rule>, direction: Direction) -> Self {
        let mut character_definitions = HashMap::new();
        let mut translations = HashMap::new();
        let mut undefined: Option<String> = None;
        let rules = rules.into_iter().filter(|r| {
            if direction == Direction::Forward {
                r.is_forward()
            } else {
                r.is_backward()
            }
        });
        for rule in rules {
            match rule {
                Rule::Undefined { dots } => {
                    undefined = Some(dots_to_unicode(dots));
                }
                Rule::Space { ch, dots, .. }
                | Rule::Punctuation { ch, dots, .. }
                | Rule::Digit { ch, dots }
                | Rule::Lowercase { ch, dots, .. }
                | Rule::Uppercase { ch, dots, .. }
                | Rule::Litdigit { ch, dots }
                | Rule::Sign { ch, dots, .. }
                | Rule::Math { ch, dots, .. } => {
                    character_definitions.insert(ch, dots_to_unicode(dots));
                }
                Rule::Letter {
                    ch,
                    dots: BrailleCharsOrImplicit::Explicit(explicit_dots),
                    ..
                } => {
                    character_definitions.insert(ch, dots_to_unicode(explicit_dots));
                }
                Rule::Always { chars, dots, .. }
                | Rule::Word { chars, dots, .. }
                | Rule::Partword { chars, dots, .. } => {
                    if let BrailleCharsOrImplicit::Explicit(explicit_dots) = dots {
                        translations.insert(chars, dots_to_unicode(explicit_dots));
                    }
                }
                _ => (), // ignore all other rules for now
            }
        }
        if let Some(undefined) = undefined {
            TranslationTable {
                undefined,
                character_definitions,
                translations,
                ..Default::default()
            }
        } else {
            TranslationTable {
                character_definitions,
                translations,
                ..Default::default()
            }
        }
    }

    pub fn translate(&self, input: &str) -> String {
        // apply the corrections
        // apply the translations
        self.pass1(input)
            .into_iter()
            .map(|m| m.output)
            .collect::<Vec<&str>>()
            .concat()
    }

    fn pass1<'a>(&'a self, input: &'a str) -> Vec<TranslationMapping<'a>> {
        let mut current_pos = 0;
        let mut mappings = Vec::new();
        while current_pos < input.len() {
            // check if any translations apply
            if let Some((consumed, mapping)) = self.apply_translations(input, current_pos) {
                current_pos += consumed;
                mappings.push(mapping);
            // check if any character definitions apply
            } else if let Some((consumed, mapping)) =
                self.apply_character_definition(&input[current_pos..])
            {
                current_pos += consumed;
                mappings.push(mapping);
            // otherwise just use the undefined definition
            } else if let Some((consumed, mapping)) = self.apply_undefined(&input[current_pos..]) {
                current_pos += consumed;
                mappings.push(mapping);
            } else {
                // FIXME: what should pass1 do if it can't even apply
                // the undefined definition?
                panic!("Looks like the input is not valid UTF-8")
            }
        }
        mappings
    }

    fn char_to_braille(&self, c: char) -> Option<&String> {
        self.character_definitions.get(&c)
    }

    fn apply_undefined<'a>(&'a self, input: &'a str) -> Option<(usize, TranslationMapping<'a>)> {
        let c = input.chars().next()?;
        let char_len = c.to_string().len();
        Some((
            char_len,
            TranslationMapping::new(&input[..char_len], &self.undefined),
        ))
    }

    fn apply_character_definition<'a>(
        &'a self,
        input: &'a str,
    ) -> Option<(usize, TranslationMapping<'a>)> {
        let c = input.chars().next()?;
        let braille = self.char_to_braille(c)?;
        // the c.to_string().len() is fishy, but I need to know the
        // byte length of the char so I can split the slice at the
        // right point
        let char_len = c.to_string().len();
        Some((
            char_len,
            TranslationMapping::new(&input[..char_len], braille),
        ))
    }

    fn apply_translations(&self, input: &str, pos: usize) -> Option<(usize, TranslationMapping)> {
        self.longest_matching_translation(input, pos)
            .map(|(k, v)| (k.len(), TranslationMapping::new(k, v)))
    }

    /// Find the longest matching translation for given input

    // This is a very naive implementation. It basically goes through
    // all translation opcodes and checks if the match the given
    // input. This is O(m*n) with m translation opcodes and n the
    // average length of an opcode. A much better implementation will
    // be using a [trie](https://en.wikipedia.org/wiki/Trie), such as
    // the one provided in the [fst crate](https://crates.io/crates/fst)
    fn longest_matching_translation(&self, input: &str, pos: usize) -> Option<(&String, &String)> {
        self.translations
            .iter()
            .filter(|(k, _)| input[pos..].starts_with(&**k))
            .max_by(|a, b| a.0.chars().count().cmp(&b.0.chars().count()))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::parser::BrailleDot;
    use crate::parser::Prefixes;

    use enumset::enum_set;

    use super::*;

    #[test]
    fn compile_display_table_test() {
        let display_table = DisplayTable::compile(vec![Rule::Display {
            ch: 'a',
            dots: vec![enum_set!(BrailleDot::DOT1)],
            prefixes: Prefixes::empty(),
        }]);
        let display_table2 = DisplayTable {
            dots_to_char: HashMap::from([('⠁', 'a')]),
        };
        assert_eq!(display_table, display_table2);
    }

    #[test]
    fn translate_display_table_test() {
        let display_table = DisplayTable::compile(vec![Rule::Display {
            ch: 'a',
            dots: vec![enum_set!(BrailleDot::DOT1)],
            prefixes: Prefixes::empty(),
        }]);
        assert_eq!(display_table.translate("⠁"), "a");
    }

    #[test]
    fn compile_translation_table_test() {
        let table = TranslationTable::compile(
            vec![Rule::Letter {
                ch: 'a',
                dots: BrailleCharsOrImplicit::Explicit(vec![enum_set!(
                    BrailleDot::DOT1 | BrailleDot::DOT2 | BrailleDot::DOT3
                )]),
                prefixes: Prefixes::empty(),
            }],
            Direction::Forward,
        );
        let table2 = TranslationTable {
            character_definitions: HashMap::from([('a', "⠇".to_string())]),
            ..Default::default()
        };
        assert_eq!(table, table2);
    }

    #[test]
    fn apply_character_definition_test() {
        let char_defs = HashMap::from([('a', "A".to_string()), ('b', "B".to_string())]);
        let table = TranslationTable {
            character_definitions: char_defs,
            ..Default::default()
        };
        assert_eq!(
            table.apply_character_definition("a"),
            Some((1, TranslationMapping::new("a", "A")))
        );
        assert_eq!(
            table.apply_character_definition("b"),
            Some((1, TranslationMapping::new("b", "B")))
        );
        assert_eq!(table.apply_character_definition("x"), None);
        // only consume one char
        assert_eq!(
            table.apply_character_definition("aa"),
            Some((1, TranslationMapping::new("a", "A")))
        );
        // handle weird unicode correctly
        assert_eq!(table.apply_character_definition("🛖a"), None);
    }

    #[test]
    fn longest_matching_translation_test() {
        let translations = HashMap::from([
            ("haha".to_string(), "".to_string()),
            ("ha".to_string(), "".to_string()),
            ("hahaha".to_string(), "".to_string()),
            ("hahahi".to_string(), "".to_string()),
        ]);
        let table = TranslationTable {
            translations: translations,
            ..Default::default()
        };
        let ignore = String::new();
        assert_eq!(
            table.longest_matching_translation("haha", 0),
            Some((&"haha".to_string(), &ignore))
        );
        assert_eq!(
            table.longest_matching_translation("hahaha", 0),
            Some((&"hahaha".to_string(), &ignore))
        );
        assert_eq!(
            table.longest_matching_translation("hahaho", 0),
            Some((&"haha".to_string(), &ignore))
        );
    }

    #[test]
    fn apply_translation_test() {
        let translations = HashMap::from([
            ("haha".to_string(), "HA".to_string()),
            ("ha".to_string(), "H".to_string()),
            ("hahaha".to_string(), "HAA".to_string()),
            ("hahahi".to_string(), "HAI".to_string()),
        ]);
        let table = TranslationTable {
            translations: translations,
            ..Default::default()
        };
        assert_eq!(
            table.apply_translations("haha", 0),
            Some((4, TranslationMapping::new("haha", "HA")))
        );
        assert_eq!(
            table.apply_translations("hahaha", 0),
            Some((6, TranslationMapping::new("hahaha", "HAA")))
        );
        assert_eq!(
            table.apply_translations("hahaho", 0),
            Some((4, TranslationMapping::new("haha", "HA")))
        );
    }

    #[test]
    fn pass1_test() {
        let char_defs = HashMap::from([('a', "A".to_string()), ('b', "B".to_string())]);
        let table = TranslationTable {
            character_definitions: char_defs,
            ..Default::default()
        };
        assert_eq!(
            table.pass1("ab"),
            vec![
                TranslationMapping::new("a", "A"),
                TranslationMapping::new("b", "B")
            ]
        );
    }

    #[test]
    fn translate_test() {
        let char_defs = HashMap::from([('a', "⠁".to_string()), ('b', "⠂".to_string())]);
        let table = TranslationTable {
            character_definitions: char_defs,
            ..Default::default()
        };
        assert_eq!(table.translate("ab"), "⠁⠂");
    }

    #[test]
    fn find_translation_test() {
        let translations = HashMap::from([
            ("gegen".to_string(), "G".to_string()),
            ("immer".to_string(), "I".to_string()),
        ]);
        let table = TranslationTable {
            translations: translations,
            undefined: "X".to_string(),
            ..Default::default()
        };
        assert_eq!(table.translate("gegen"), "G");
        assert_eq!(table.translate("gegenw"), "GX");
        assert_eq!(table.translate("🛖gegen"), "XG");
    }

    #[test]
    fn undefined_test() {
        let table = TranslationTable {
            undefined: "X".to_string(),
            ..Default::default()
        };
        assert_eq!(table.translate("x"), "X");
        assert_eq!(table.translate("🛖h"), "XX");
    }
}
