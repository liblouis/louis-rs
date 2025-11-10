use std::collections::{HashMap, HashSet};

use match_pattern::MatchPatterns;
use trie::Trie;

use crate::parser::{AnchoredRule, Attribute, Braille, Direction, Rule, dots_to_unicode, fallback};

use self::trie::Boundary;
use indication::{Indication, numeric, uppercase};

mod boundaries;
mod indication;
mod match_pattern;
mod nfa;
mod trie;

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum TranslationError {
    #[error("Implicit character {0:?} not defined")]
    ImplicitCharacterNotDefined(char),
    #[error(
        "Character in base rule not defined: derived: {derived:?}, base: {base:?}, direction: {direction:?}"
    )]
    BaseCharacterNotDefined {
        base: char,
        derived: char,
        direction: Direction,
    },
    #[error("Attribute {0:?} has not been defined")]
    AttributeNotDefined(String),
}

#[derive(Debug, PartialEq, Clone, Default)]
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

    fn insert(&mut self, c: char, mapped: String) {
        let translation = Translation::new(c.to_string(), mapped.to_string(), 1);
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

    fn resolve_implicit_dots(&self, chars: &str) -> Result<String, TranslationError> {
        chars
            .chars()
            .map(|c| {
                self.get(&c)
                    .ok_or(TranslationError::ImplicitCharacterNotDefined(c))
                    .map(|t| t.output.to_string())
            })
            .collect()
    }

    /// Convert braille dots to Unicode characters.
    ///
    /// Convert given braille `dots` to Unicode characters. If the dots are
    /// [explicit](Braille::Explicit) then simply delegate to the [`dots_to_unicode`] function.
    /// Otherwise, if the dots are [implicit](Braille::Implicit) convert the given `chars` to
    /// braille with the given `character_definitions` and using the [`Self::resolve_implicit_dots`]
    /// function.
    ///
    /// Returns the braille Unicode characters or [`TranslationError`] if the implicit characters
    /// could not be converted.
    fn braille_to_unicode(&self, dots: &Braille, chars: &str) -> Result<String, TranslationError> {
        let dots = match dots {
            Braille::Implicit => self.resolve_implicit_dots(&chars)?,
            Braille::Explicit(dots) => dots_to_unicode(&dots),
        };
        Ok(dots)
    }
}

/// A mapping between a character [Attribute] and the associated set
/// of characters
#[derive(Debug, Default)]
struct CharacterAttributes(HashMap<Attribute, HashSet<char>>);

impl CharacterAttributes {
    fn new() -> Self {
        Self(HashMap::new())
    }

    fn insert(&mut self, attribute: Attribute, c: char) {
        self.0.entry(attribute).or_default().insert(c);
    }

    fn get(&self, attribute: Attribute) -> Option<HashSet<char>> {
        self.0.get(&attribute).cloned()
    }
}

/// A mapping between a name of an attribute and it's enum
#[derive(Debug, Default)]
struct AttributeMapping(HashMap<String, Attribute>);

impl AttributeMapping {
    fn new() -> Self {
        Self(HashMap::from([
            ("space".to_string(), Attribute::Space),
            ("digit".to_string(), Attribute::Digit),
            ("letter".to_string(), Attribute::Letter),
            ("lowercase".to_string(), Attribute::Lowercase),
            ("uppercase".to_string(), Attribute::Uppercase),
            ("punctuation".to_string(), Attribute::Punctuation),
            ("sign".to_string(), Attribute::Sign),
            //("math".to_string(), Attribute::Math),
            //("litdigit".to_string(), Attribute::LitDigit),
        ]))
    }

    fn insert(&mut self, name: &str, attribute: Attribute) {
        self.0.entry(name.to_string()).or_insert(attribute);
    }

    fn get(&self, name: &str) -> Option<Attribute> {
        self.0.get(name).cloned()
    }
}

#[derive(Debug)]
pub struct TranslationTable {
    undefined: Option<String>,
    character_definitions: CharacterDefinition,
    character_attributes: CharacterAttributes,
    attributes: AttributeMapping,
    trie: Trie,
    match_patterns: MatchPatterns,
    numeric_indicator: numeric::Indicator,
    uppercase_indicator: uppercase::Indicator,
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
        let mut attributes = AttributeMapping::new();
        let mut trie = Trie::new();
        let mut match_patterns = MatchPatterns::new();
        let mut numeric_indicator_builder = numeric::IndicatorBuilder::new();
        let mut uppercase_indicator_builder = uppercase::IndicatorBuilder::new();

        let rules: Vec<AnchoredRule> = rules
            .into_iter()
            .filter(|r| r.rule.is_direction(direction))
            .collect();

        // FIXME: For some unknown reason the litdigit rule seems to have precedence over the digit
        // rule. Since they both want to define digits in the same character_definitions slot we
        // need to make sure litdigits rules are handled before digit rules
        for rule in rules
            .iter()
            .filter(|r| matches!(r.rule, Rule::Litdigit { .. }))
        {
            match &rule.rule {
                Rule::Litdigit {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, dots_to_unicode(dots));
                    character_attributes.insert(Attribute::Digit, *character);
                }
                _ => (),
            }
        }

        // The compilation is done in two passes: The first pass simply collects all character
        // definitions and character attributes, so that they are then known in a second pass, e.g.
        // for implicit braille definitions or for the `base` opcode
        for rule in &rules {
            match &rule.rule {
                Rule::Undefined { dots } => {
                    undefined = Some(dots_to_unicode(dots));
                }
                Rule::Space {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, dots_to_unicode(dots));
                    character_attributes.insert(Attribute::Space, *character);
                }
                Rule::Punctuation {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, dots_to_unicode(dots));
                    character_attributes.insert(Attribute::Punctuation, *character);
                }
                Rule::Digit {
                    character, dots, ..
                }
                | Rule::Litdigit {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, dots_to_unicode(dots));
                    character_attributes.insert(Attribute::Digit, *character);
                }
                Rule::Letter {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, dots_to_unicode(dots));
                    character_attributes.insert(Attribute::Letter, *character);
                }
                Rule::Lowercase {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, dots_to_unicode(dots));
                    character_attributes.insert(Attribute::Lowercase, *character);
                    // a lowercase is also a letter
                    character_attributes.insert(Attribute::Letter, *character);
                }
                Rule::Uppercase {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, dots_to_unicode(dots));
                    character_attributes.insert(Attribute::Uppercase, *character);
                    // an uppercase is also a letter
                    character_attributes.insert(Attribute::Letter, *character);
                }
                Rule::Sign {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character, dots_to_unicode(dots));
                    character_attributes.insert(Attribute::Sign, *character);
                }
                Rule::Math {
                    character, dots, ..
                } => {
                    character_definitions.insert(*character,dots_to_unicode(dots));
                    // TODO: should the math opcode not also define a CharacterAttribute?
                }
                Rule::Numsign { dots } => {
                    numeric_indicator_builder =
                        numeric_indicator_builder.numsign(&dots_to_unicode(dots));
                }
                Rule::Nonumsign { dots } => {
                    numeric_indicator_builder =
                        numeric_indicator_builder.nonumsign(&dots_to_unicode(dots));
                }
                Rule::Numericnocontchars { chars } => {
                    numeric_indicator_builder =
                        numeric_indicator_builder.numericnocontchars(&chars);
                }
                Rule::Numericmodechars { chars } => {
                    numeric_indicator_builder = numeric_indicator_builder.numericmodechars(&chars);
                }
                Rule::Capsletter { dots, .. } => {
                    uppercase_indicator_builder =
                        uppercase_indicator_builder.capsletter(&dots_to_unicode(dots));
                }
                Rule::Begcapsword { dots, .. } => {
                    uppercase_indicator_builder =
                        uppercase_indicator_builder.begcapsword(&dots_to_unicode(dots));
                }
                Rule::Endcapsword { dots, .. } => {
                    uppercase_indicator_builder =
                        uppercase_indicator_builder.endcapsword(&dots_to_unicode(dots));
                }
                Rule::Begcaps { dots } => {
                    uppercase_indicator_builder =
                        uppercase_indicator_builder.begcaps(&dots_to_unicode(dots));
                }
                Rule::Endcaps { dots } => {
                    uppercase_indicator_builder =
                        uppercase_indicator_builder.endcaps(&dots_to_unicode(dots));
                }
                Rule::Capsmodechars { chars } => {
                    uppercase_indicator_builder = uppercase_indicator_builder.capsmodechars(&chars);
                }
                // display rules are ignored for translation tables
                Rule::Display { .. } => (),
                _ => (),
            }
        }

        // The second pass goes through the rules to resolve the base opcodes and implicit (`=`)
        // arguments in rules
        for rule in &rules {
            match &rule.rule {
                Rule::Base {
                    derived,
                    base,
                    name,
                } => {
                    if let Some(translation) = character_definitions.get(base) {
                        character_definitions.insert(*derived, translation.output.clone());
                        if let Some(attribute) = attributes.get(name) {
                            character_attributes.insert(attribute, *derived)
                        } else {
                            return Err(TranslationError::AttributeNotDefined(name.to_string()));
                        }
                    } else {
                        // hm, there is no character definition for the base character.
                        // If we are backwards compatible ignore the problem, otherwise
                        // throw an error
                        if !cfg!(feature = "backwards_compatibility") {
                            return Err(TranslationError::BaseCharacterNotDefined {
                                base: *base,
                                derived: *derived,
                                direction,
                            });
                        }
                    }
                }
                Rule::Comp6 { chars, dots } | Rule::Always { chars, dots, .. } => {
                    let dots = character_definitions.braille_to_unicode(dots, chars)?;
                    trie.insert(chars.to_string(), dots, Boundary::None, Boundary::None);
                }
                Rule::Word { chars, dots, .. } => {
                    let dots = character_definitions.braille_to_unicode(dots, chars)?;
                    trie.insert(chars.to_string(), dots, Boundary::Word, Boundary::Word);
                }
                Rule::Begword { chars, dots, .. } => {
                    let dots = character_definitions.braille_to_unicode(dots, chars)?;
                    trie.insert(chars.to_string(), dots, Boundary::Word, Boundary::NotWord)
                }
                Rule::Sufword { chars, dots, .. } => {
                    let dots = character_definitions.braille_to_unicode(dots, chars)?;
                    trie.insert(chars.to_string(), dots, Boundary::Word, Boundary::None);
                }
                Rule::Midword { chars, dots, .. } | Rule::Partword { chars, dots, .. } => {
                    let dots = character_definitions.braille_to_unicode(dots, chars)?;
                    trie.insert(
                        chars.to_string(),
                        dots,
                        Boundary::NotWord,
                        Boundary::NotWord,
                    )
                }
                Rule::Midendword { chars, dots, .. } => {
                    let dots = character_definitions.braille_to_unicode(dots, chars)?;
                    trie.insert(chars.to_string(), dots, Boundary::NotWord, Boundary::None);
                }
                Rule::Endword { chars, dots, .. } | Rule::Prfword { chars, dots, .. } => {
                    let dots = character_definitions.braille_to_unicode(dots, chars)?;
                    trie.insert(chars.to_string(), dots, Boundary::None, Boundary::Word);
                }
                Rule::Begmidword { chars, dots, .. } => {
                    let dots = character_definitions.braille_to_unicode(dots, chars)?;
                    trie.insert(chars.to_string(), dots, Boundary::None, Boundary::NotWord);
                }
                Rule::Joinword { chars, dots, .. } | Rule::Lowword { chars, dots, .. } => trie
                    .insert(
                        chars.to_string(),
                        dots_to_unicode(dots),
                        Boundary::Word,
                        Boundary::Word,
                    ),
                Rule::Begnum { chars, dots, .. } => trie.insert(
                    chars.to_string(),
                    dots_to_unicode(dots),
                    Boundary::Word,
                    Boundary::WordNumber,
                ),
                Rule::Midnum { chars, dots, .. } => trie.insert(
                    chars.to_string(),
                    dots_to_unicode(dots),
                    Boundary::Number,
                    Boundary::Number,
                ),
                Rule::Endnum { chars, dots, .. } => {
                    let dots = character_definitions.braille_to_unicode(dots, chars)?;
                    trie.insert(
                        chars.to_string(),
                        dots,
                        Boundary::NumberWord,
                        Boundary::Word,
                    );
                }
                Rule::Match {
                    pre,
                    chars,
                    post,
                    dots,
                    ..
                } => {
                    let dots = character_definitions.braille_to_unicode(dots, chars)?;
                    match_patterns.insert(pre, chars.to_string(), post, dots);
                }

                _ => (),
            }
        }

        numeric_indicator_builder = numeric_indicator_builder.numeric_characters(
            character_attributes
                .get(Attribute::Digit)
                .unwrap_or(HashSet::default()),
        );
        uppercase_indicator_builder = uppercase_indicator_builder
            .uppercase_characters(
                character_attributes
                    .get(Attribute::Uppercase)
                    .unwrap_or(HashSet::default()),
            )
            .letter_characters(
                character_attributes
                    .get(Attribute::Letter)
                    .unwrap_or(HashSet::default()),
            );
        Ok(TranslationTable {
            undefined,
            direction,
            character_definitions,
            character_attributes,
            attributes,
            trie,
            match_patterns,
            numeric_indicator: numeric_indicator_builder.build(),
            uppercase_indicator: uppercase_indicator_builder.build(),
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
        // FIXME: the following seems weird, but the indicator is a mutable state machine. Since
        // self (the translation table) is immutable we build a mutable copy of the indicator for
        // each translation
        let mut numeric_indicator = self.numeric_indicator.clone();
        let mut uppercase_indicator = self.uppercase_indicator.clone();

        loop {
            // Check if there is a need for an indication
            if let Some(indication) = numeric_indicator.next(chars.as_str()) {
                let indicator_sign = match indication {
                    Indication::NumericStart => numeric_indicator.start_indicator().unwrap(),
                    Indication::NumericEnd => numeric_indicator.end_indicator().unwrap(),
                    _ => unreachable!(),
                };
                translations.push(Translation::new("".to_string(), indicator_sign, 1));
            }
            if let Some(indication) = uppercase_indicator.next(chars.as_str()) {
                let indicator_sign = match indication {
                    Indication::UppercaseStart => uppercase_indicator.start_indicator().unwrap(),
                    Indication::UppercaseEnd => uppercase_indicator.end_indicator().unwrap(),
                    Indication::UppercaseStartLetter => {
                        uppercase_indicator.start_letter_indicator().unwrap()
                    }
                    Indication::UppercaseStartWord => {
                        uppercase_indicator.start_word_indicator().unwrap()
                    }
                    Indication::UppercaseEndWord => {
                        uppercase_indicator.end_word_indicator().unwrap()
                    }
                    _ => unreachable!(),
                };
                translations.push(Translation::new("".to_string(), indicator_sign, 1));
            }
            // given an input query the translation table for matching translations. Then split off
            // the translations that are delayed, i.e. have an offset because they have a  pre-pattern
            let (mut candidates, delayed): (Vec<Translation>, Vec<Translation>) = self
                .trie
                .find_translations(chars.as_str(), prev)
                .into_iter()
                // TODO: figure out what to do with delayed rules that have a negative offset, i.e.
                // if there was a matching rule that consumed so much input that the delayed rule is
                // no longer applicable
                .partition(|t| t.offset == 0);
            delayed_translations.extend(delayed);
            // then search for matching match patterns. Unless they have empty pre patterns they will all have
            // an offset. Split those off.
            let (match_candidates, match_delayed): (Vec<Translation>, Vec<Translation>) = self
                .match_patterns
                .find_translations(chars.as_str())
                .into_iter()
                .partition(|t| t.offset == 0);
            delayed_translations.extend(match_delayed);
            // merge the candidates from the match patters with the candidates from the plain translations
            candidates.extend(match_candidates);
            // move delayed_translations with zero offset into candidates
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
                delayed_translations = self.update_offsets(delayed_translations, length);
            } else if let Some(next_char) = chars.next() {
                // no translation rule found; try character definition rules
                prev = Some(next_char);
                if let Some(translation) = self.character_definitions.get(&next_char) {
                    // there is a matching character definition for the next character
                    translations.push(translation.clone());
                    delayed_translations = self.update_offsets(delayed_translations, 1);
                } else if let Some(ref replacement) = self.undefined {
                    // there is a rule for undefined characters
                    let translation =
                        Translation::new(next_char.to_string(), replacement.to_string(), 1);
                    translations.push(translation);
                    delayed_translations = self.update_offsets(delayed_translations, 1);
                } else {
                    // otherwise handle it as a undefined character
                    let translation = Translation::new(
                        next_char.to_string(),
                        self.handle_undefined_char(next_char),
                        1,
                    );
                    translations.push(translation);
                    delayed_translations = self.update_offsets(delayed_translations, 1);
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
            char_defs.resolve_implicit_dots("xs"),
            Err(TranslationError::ImplicitCharacterNotDefined('x'))
        );
        let mut char_defs = CharacterDefinition::new();
        char_defs.insert('a', "A".to_string());
        char_defs.insert('h', "H".to_string());
        assert_eq!(char_defs.resolve_implicit_dots("haha"), Ok("HAHA".into()));
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

    #[test]
    fn match_with_set_test() {
        let rules = vec![
            parse_rule("lowercase f 3"),
            parse_rule("lowercase o 4"),
            parse_rule("lowercase s 7"),
            parse_rule("lowercase z 5"),
            parse_rule("match [fz] oo [fz] 14"),
        ];
        let table = TranslationTable::compile(rules, Direction::Forward).unwrap();
        assert_eq!(table.translate("oo"), "⠈⠈");
        assert_eq!(table.translate("soo"), "⡀⠈⠈");
        assert_eq!(table.translate("foof"), "⠄⠉⠄");
        assert_eq!(table.translate("zoof"), "⠐⠉⠄");
        assert_eq!(table.translate("soof"), "⡀⠈⠈⠄");
    }

    #[test]
    fn numeric_indication_text() {
        let rules = vec![
            parse_rule("lowercase f 3"),
            parse_rule("lowercase o 4"),
            parse_rule("litdigit 1 1"),
            parse_rule("litdigit 2 12"),
            parse_rule("litdigit 3 14"),
            parse_rule("numsign 3456"),
            parse_rule("nonumsign 56"),
            parse_rule("numericnocontchars abcdefghij"),
        ];
        let table = TranslationTable::compile(rules, Direction::Forward).unwrap();
        assert_eq!(table.translate("123"), "⠼⠁⠃⠉");
        assert_eq!(table.translate("123foo"), "⠼⠁⠃⠉⠰⠄⠈⠈");
        assert_eq!(table.translate("foof"), "⠄⠈⠈⠄");
    }

    #[test]
    fn uppercase_indication_text() {
        let rules = vec![
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 12"),
            parse_rule("lowercase c 14"),
            parse_rule("base uppercase A a"),
            parse_rule("base uppercase B b"),
            parse_rule("base uppercase C c"),
            parse_rule("capsletter 46"),
            parse_rule("begcapsword 6-6"),
        ];
        let table = TranslationTable::compile(rules, Direction::Forward).unwrap();
        assert_eq!(table.translate("abc"), "⠁⠃⠉");
        assert_eq!(table.translate("Abc"), "⠨⠁⠃⠉");
        assert_eq!(table.translate("ABC"), "⠠⠠⠁⠃⠉");
    }
}
