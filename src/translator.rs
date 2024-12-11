use std::collections::{HashMap, HashSet};

use trie::Trie;

use crate::parser::{dots_to_unicode, fallback, AnchoredRule, Attribute, Braille, Direction, Rule};

use self::trie::Boundary;

mod boundaries;
mod trie;

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum TranslationError {
    #[error("Implicit character {0:?} not defined")]
    ImplicitCharacterNotDefined(char),
    #[error("Character in base rule not defined: derived: {derived:?}, base: {base:?}, direction: {direction:?}")]
    BaseCharacterNotDefined {
        base: char,
        derived: char,
        direction: Direction,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct Translation {
    /// Input string to be translated
    input: String,
    /// The translation of `input`, typically Unicode braille. In the case of back-translation the
    /// `input` contains Unicode braille and the output plain text.
    output: String,
    /// Number of chars in `input`
    length: usize,
    /// Weight of a translation. Typically this is the length of the input, but often it includes
    /// word boundaries as well. In some cases the weight has to be calculated dynamically, for
    /// instance for `match` opcodes where the length of the matched input often depends on regular
    /// expressions.
    weight: usize,
    /// The `match` opcode contains a pre-pattern which is essentially a look-behind regexp. The
    /// `offset` is the length of this pre-pattern (calculated at run-time) so that the translation
    /// can be applied later in the input string, when the pre-pattern has been consumed.
    offset: usize,
}

impl Translation {
    pub fn new(input: String, output: String, weight: usize) -> Self {
        let length = input.chars().count();
        Self {
            input,
            output,
            weight,
            length,
            offset: 0,
        }
    }

    /// Set the `offset` of a translation.
    fn with_offset(self, offset: usize) -> Self {
        Self { offset, ..self }
    }

    /// Set the `weight` of a translation if `offset` is greater than 0, otherwise return the
    /// translation unchanged.
    fn with_weight_if_offset(self, weight: usize, offset: usize) -> Self {
        if offset > 0 {
            Self { weight, ..self }
        } else {
            self
        }
    }

    /// Decrement the `offset` of a translation.
    fn decrement_offset(self, decrement: usize) -> Self {
        Self {
            offset: self.offset - decrement,
            ..self
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

fn resolve_implicit_dots(
    chars: &str,
    character_definitions: &CharacterDefinition,
) -> Result<String, TranslationError> {
    chars
        .chars()
        .map(|c| {
            character_definitions
                .get(&c)
                .ok_or(TranslationError::ImplicitCharacterNotDefined(c))
                .map(|t| t.output.to_string())
        })
        .collect()
}

#[derive(Debug)]
struct CharacterAttributes(HashSet<(char, Attribute)>);

impl CharacterAttributes {
    fn new() -> Self {
        Self(HashSet::new())
    }

    fn insert(&mut self, c: char, attribute: Attribute) {
        self.0.insert((c, attribute));
    }

    fn contains(&self, c: char, attribute: Attribute) -> bool {
        self.0.contains(&(c, attribute))
    }
}

#[derive(Debug)]
pub struct TranslationTable {
    undefined: Option<String>,
    character_definitions: CharacterDefinition,
    character_attributes: CharacterAttributes,
    translations: Trie,
    direction: Direction,
}

impl TranslationTable {
    pub fn compile(
        rules: Vec<AnchoredRule>,
        direction: Direction,
    ) -> Result<Self, TranslationError> {
        let mut undefined = None;
        let mut character_definitions = CharacterDefinition::new();
        let mut character_attributes = CharacterAttributes::new();
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
                } => {
                    let translation =
                        Translation::new(character.to_string(), dots_to_unicode(dots), 1);
                    character_definitions.insert(*character, translation);
                    character_attributes.insert(*character, Attribute::Space);
                }
                Rule::Punctuation {
                    character, dots, ..
                } => {
                    let translation =
                        Translation::new(character.to_string(), dots_to_unicode(dots), 1);
                    character_definitions.insert(*character, translation);
                    character_attributes.insert(*character, Attribute::Punctuation);
                }
                Rule::Digit {
                    character, dots, ..
                }
                | Rule::Litdigit {
                    character, dots, ..
                } => {
                    let translation =
                        Translation::new(character.to_string(), dots_to_unicode(dots), 1);
                    character_definitions.insert(*character, translation);
                    character_attributes.insert(*character, Attribute::Digit);
                }
                Rule::Letter {
                    character, dots, ..
                } => {
                    let translation =
                        Translation::new(character.to_string(), dots_to_unicode(dots), 1);
                    character_definitions.insert(*character, translation);
                    character_attributes.insert(*character, Attribute::Letter);
                }
                Rule::Lowercase {
                    character, dots, ..
                } => {
                    let translation =
                        Translation::new(character.to_string(), dots_to_unicode(dots), 1);
                    character_definitions.insert(*character, translation);
                    character_attributes.insert(*character, Attribute::Lowercase);
                }
                Rule::Uppercase {
                    character, dots, ..
                } => {
                    let translation =
                        Translation::new(character.to_string(), dots_to_unicode(dots), 1);
                    character_definitions.insert(*character, translation);
                    character_attributes.insert(*character, Attribute::Uppercase);
                }
                Rule::Sign {
                    character, dots, ..
                } => {
                    let translation =
                        Translation::new(character.to_string(), dots_to_unicode(dots), 1);
                    character_definitions.insert(*character, translation);
                    character_attributes.insert(*character, Attribute::Sign);
                }
                Rule::Math {
                    character, dots, ..
                } => {
                    character_definitions.insert(
                        *character,
                        Translation::new(character.to_string(), dots_to_unicode(dots), 1),
                    );
		    // TODO: should the math opcode not also define a CharacterAttribute?
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
                Rule::Begnum { chars, dots, .. } => translations.insert(
                    chars.to_string(),
                    dots_to_unicode(dots),
                    Boundary::Word,
                    Boundary::WordNumber,
                ),
                Rule::Midnum { chars, dots, .. } => translations.insert(
                    chars.to_string(),
                    dots_to_unicode(dots),
                    Boundary::Number,
                    Boundary::Number,
                ),
                Rule::Endnum {
                    chars,
                    dots: Braille::Explicit(dots),
                    ..
                } => translations.insert(
                    chars.to_string(),
                    dots_to_unicode(dots),
                    Boundary::NumberWord,
                    Boundary::Word,
                ),
                Rule::Match {
                    pre,
                    chars,
                    post,
                    dots: Braille::Explicit(dots),
                    ..
                } => translations.insert_match(chars.to_string(), dots_to_unicode(dots), pre, post),
                // the base rule is handled in the second pass
                Rule::Base { .. } => (),
                // display rules are ignored for translation tables
                Rule::Display { .. } => (),
                _ => (),
            }
        }

        // use a second pass through the rules to resolve the base opcodes and
        // implicit (`=`) arguments in rules
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
                        // hm, there is no character definition for the base character.
                        // If we are backwards compatible ignore the problem, otherwise
                        // throw an error
                        if !cfg!(feature = "backwards_compatibility") {
                            return Err(TranslationError::BaseCharacterNotDefined {
                                base,
                                derived,
                                direction,
                            });
                        }
                    }
                }
                Rule::Comp6 {
                    chars,
                    dots: Braille::Implicit,
                }
                | Rule::Always {
                    chars,
                    dots: Braille::Implicit,
                    ..
                } => {
                    translations.insert(
                        chars.to_string(),
                        resolve_implicit_dots(&chars, &character_definitions)?,
                        Boundary::None,
                        Boundary::None,
                    );
                }
                Rule::Word {
                    chars,
                    dots: Braille::Implicit,
                    ..
                } => {
                    translations.insert(
                        chars.to_string(),
                        resolve_implicit_dots(&chars, &character_definitions)?,
                        Boundary::Word,
                        Boundary::Word,
                    );
                }
                Rule::Begword {
                    chars,
                    dots: Braille::Implicit,
                    ..
                } => translations.insert(
                    chars.to_string(),
                    resolve_implicit_dots(&chars, &character_definitions)?,
                    Boundary::Word,
                    Boundary::NotWord,
                ),
                Rule::Sufword {
                    chars,
                    dots: Braille::Implicit,
                    ..
                } => {
                    translations.insert(
                        chars.to_string(),
                        resolve_implicit_dots(&chars, &character_definitions)?,
                        Boundary::Word,
                        Boundary::None,
                    );
                }
                Rule::Midword {
                    chars,
                    dots: Braille::Implicit,
                    ..
                }
                | Rule::Partword {
                    chars,
                    dots: Braille::Implicit,
                    ..
                } => translations.insert(
                    chars.to_string(),
                    resolve_implicit_dots(&chars, &character_definitions)?,
                    Boundary::NotWord,
                    Boundary::NotWord,
                ),
                Rule::Midendword {
                    chars,
                    dots: Braille::Implicit,
                    ..
                } => translations.insert(
                    chars.to_string(),
                    resolve_implicit_dots(&chars, &character_definitions)?,
                    Boundary::NotWord,
                    Boundary::None,
                ),
                Rule::Endword {
                    chars,
                    dots: Braille::Implicit,
                    ..
                }
                | Rule::Prfword {
                    chars,
                    dots: Braille::Implicit,
                    ..
                } => translations.insert(
                    chars.to_string(),
                    resolve_implicit_dots(&chars, &character_definitions)?,
                    Boundary::None,
                    Boundary::Word,
                ),
                Rule::Begmidword {
                    chars,
                    dots: Braille::Implicit,
                    ..
                } => translations.insert(
                    chars.to_string(),
                    resolve_implicit_dots(&chars, &character_definitions)?,
                    Boundary::None,
                    Boundary::NotWord,
                ),
                Rule::Endnum {
                    chars,
                    dots: Braille::Implicit,
                    ..
                } => translations.insert(
                    chars.to_string(),
                    resolve_implicit_dots(&chars, &character_definitions)?,
                    Boundary::NumberWord,
                    Boundary::Word,
                ),
                _ => (),
            }
        }

        Ok(TranslationTable {
            undefined,
            direction,
            character_definitions,
            character_attributes,
            translations,
        })
    }

    fn update_offsets(&self, translations: Vec<Translation>, decrement: usize) -> Vec<Translation> {
        translations
            .into_iter()
            .map(|t| t.decrement_offset(decrement))
            .collect()
    }

    pub fn translate(&self, input: &str) -> String {
        let mut translations: Vec<Translation> = Vec::new();
        let mut delayed_translations: Vec<Translation> = Vec::new();
        let mut chars = input.chars();
        let mut prev: Option<char> = None;

        loop {
            // given an input query the translation table for matching translations. Then split off
            // the translations that are delayed, i.e. have an offset because they have a  pre-pattern
            let (mut candidates, delayed): (Vec<Translation>, Vec<Translation>) = self
                .translations
                .find_translations(chars.as_str(), prev)
                .into_iter()
                // TODO: figure out what to do with delayed rules that have a negative offset, i.e.
                // if there was a matching rule that consumed so much input that the delayed rule is
                // no longer applicable
                .partition(|t| t.offset == 0);
            delayed_translations.extend(delayed);
            let (current, delayed): (Vec<Translation>, Vec<Translation>) = delayed_translations
                .into_iter()
                .partition(|t| t.offset == 0);
            delayed_translations = delayed;
            candidates.extend(current);
            candidates.sort_by_key(|translation| translation.weight);
            if let Some(t) = candidates.last() {
                // there is a matching translation rule
                let translation = t.clone();
                // move the iterator forward by the number of characters in the translation
                let length = t.length - 1;
                chars.nth(length);
                prev = translation.input.chars().last();
                translations.push(translation);
                delayed_translations = delayed_translations
                    .into_iter()
                    .map(|t| t.decrement_offset(length))
                    .collect();
            } else if let Some(next_char) = chars.next() {
                // no translation rule found; try character definition rules
                prev = Some(next_char);
                if let Some(translation) = self.character_definitions.get(&next_char) {
                    // there is a matching character definition for the next character
                    translations.push(translation.clone());
                    delayed_translations = delayed_translations
                        .into_iter()
                        .map(|t| t.decrement_offset(1))
                        .collect();
                } else if let Some(ref replacement) = self.undefined {
                    // there is a rule for undefined characters
                    let translation =
                        Translation::new(next_char.to_string(), replacement.to_string(), 1);
                    translations.push(translation);
                    delayed_translations = delayed_translations
                        .into_iter()
                        .map(|t| t.decrement_offset(1))
                        .collect();
                } else {
                    // otherwise handle it as a undefined character
                    let translation = Translation::new(
                        next_char.to_string(),
                        self.handle_undefined_char(next_char),
                        1,
                    );
                    translations.push(translation);
                    delayed_translations = delayed_translations
                        .into_iter()
                        .map(|t| t.decrement_offset(1))
                        .collect();
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
    fn resolve_implicit_dots_test() {
        let char_defs = CharacterDefinition::new();
        assert_eq!(
            resolve_implicit_dots("xs", &char_defs),
            Err(TranslationError::ImplicitCharacterNotDefined('x'))
        );
        let mut char_defs = CharacterDefinition::new();
        char_defs.insert('a', Translation::new("a".to_string(), "A".to_string(), 1));
        char_defs.insert('h', Translation::new("h".to_string(), "H".to_string(), 1));
        assert_eq!(resolve_implicit_dots("haha", &char_defs), Ok("HAHA".into()));
    }

    #[test]
    fn translate_test() {
        let rules = vec![
            parse_rule("always foo 123"),
            parse_rule("always bar 456"),
            parse_rule("space \\s 0"),
        ];
        let table = TranslationTable::compile(rules, Direction::Forward).unwrap();
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
        let table = TranslationTable::compile(rules, Direction::Forward).unwrap();
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
        let table = TranslationTable::compile(rules, Direction::Forward).unwrap();
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
        let table = TranslationTable::compile(rules, Direction::Forward).unwrap();
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
        let table = TranslationTable::compile(rules, Direction::Forward).unwrap();
        assert_eq!(table.translate("bar"), "⠂⠁⠐"); // bar should not be contracted
        assert_eq!(table.translate("foobar"), "⠉⠂⠁⠐"); // bar should not be contracted
        assert_eq!(table.translate("foobar."), "⠉⠂⠁⠐⠠"); // bar should not be contracted
        assert_eq!(table.translate("foobarfoo"), "⠉⠑⠉"); // bar should be contracted
        assert_eq!(table.translate("foobar foo"), "⠉⠂⠁⠐⠀⠉"); // bar should not be contracted
        assert_eq!(table.translate("foobar. foo"), "⠉⠂⠁⠐⠠⠀⠉"); // bar should not be contracted
        assert_eq!(table.translate("foo bar foo"), "⠉⠀⠂⠁⠐⠀⠉"); // bar should not be contracted
    }

    #[test]
    fn begnum_test() {
        let rules = vec![
            parse_rule("digit 1 1"),
            parse_rule("sign a 3456"),
            parse_rule("begnum a 4"),
        ];
        let table = TranslationTable::compile(rules, Direction::Forward).unwrap();
        assert_eq!(table.translate("1"), "⠁");
        assert_eq!(table.translate("a"), "⠼");
        assert_eq!(table.translate("a1"), "⠈⠁");
    }

    #[test]
    fn endnum_test() {
        let rules = vec![
            parse_rule("digit 1 1"),
            parse_rule("lowercase h 125"),
            parse_rule("lowercase t 2345"),
            parse_rule("punctuation . 6"),
            parse_rule("always th 14"),
            parse_rule("endnum th 15"),
        ];
        let table = TranslationTable::compile(rules, Direction::Forward).unwrap();
        assert_eq!(table.translate("th"), "⠉");
        assert_eq!(table.translate("1th"), "⠁⠑");
        assert_eq!(table.translate("1th."), "⠁⠑⠠");
    }

    #[test]
    fn base_test() {
        let rules = vec![
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 12"),
            parse_rule("base uppercase A a"),
        ];
        let table = TranslationTable::compile(rules, Direction::Forward).unwrap();
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
        let table = TranslationTable::compile(rules, Direction::Forward).unwrap();
        assert_eq!(display_table.translate(&table.translate("a")), "A");
        assert_eq!(display_table.translate(&table.translate(" ")), " ");
        assert_eq!(display_table.translate(&table.translate("a a")), "A A");
    }

    #[test]
    fn match_test() {
        let rules = vec![
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 2"),
            parse_rule("lowercase f 3"),
            parse_rule("lowercase o 4"),
            parse_rule("lowercase r 5"),
            parse_rule("match a foo b 14"),
        ];
        let table = TranslationTable::compile(rules, Direction::Forward).unwrap();
        assert_eq!(table.translate("foo"), "⠄⠈⠈");
        assert_eq!(table.translate("afoo"), "⠁⠄⠈⠈");
        assert_eq!(table.translate("afoob"), "⠁⠉⠂");
    }

    #[test]
    fn match_with_any_test() {
        let rules = vec![
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 2"),
            parse_rule("lowercase f 3"),
            parse_rule("lowercase o 4"),
            parse_rule("lowercase r 5"),
            parse_rule("match . foo b 14"),
        ];
        let table = TranslationTable::compile(rules, Direction::Forward).unwrap();
        assert_eq!(table.translate("foo"), "⠄⠈⠈");
        assert_eq!(table.translate("afoo"), "⠁⠄⠈⠈");
        assert_eq!(table.translate("afoob"), "⠁⠉⠂");
        assert_eq!(table.translate("ffoob"), "⠄⠉⠂");
    }
}
