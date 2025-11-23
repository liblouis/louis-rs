//! A braille translator that uses [liblouis](https://liblouis.io) braille tables

use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io;

use hyphenation::Standard;
use hyphenation::{Hyphenator, Load};
use match_pattern::MatchPatterns;
use trie::Trie;

use crate::parser::HasDirection;
use crate::parser::HasNocross;
use crate::parser::HasPrecedence;
use crate::parser::Precedence;
use crate::parser::{AnchoredRule, Attribute, Braille, Direction, Rule, dots_to_unicode, fallback};

use self::trie::Boundary;
use indication::{lettersign, nocontract, numeric, uppercase};

mod boundaries;
mod indication;
mod match_pattern;
mod nfa;
mod trie;

#[derive(thiserror::Error, Debug)]
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
    #[error(transparent)]
    HyphenationTableIoError(#[from] io::Error),
    #[error(transparent)]
    HyphenationTableLoadError(#[from] hyphenation::load::Error),
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
    precedence: Precedence,
    origin: Option<AnchoredRule>,
}

impl Translation {
    pub fn new(
        input: &str,
        output: &str,
        weight: usize,
        // FIXME: this is some weird thing recommended by Claude: apparently the `impl
        // Into<Option<T>>` trait bound automatically converts `T` to `Some(T)` and `None` to
        // `None`, giving you the overloaded behavior you want with a single function. This is more
        // idiomatic than having separate `new` and `new_with_origin` methods.
        origin: impl Into<Option<AnchoredRule>>,
    ) -> Self {
        let length = input.chars().count();
        Self {
            input: input.to_string(),
            output: output.to_string(),
            weight,
            length,
            offset: 0,
            precedence: Precedence::Default,
            origin: origin.into(),
        }
    }

    pub fn input(&self) -> String {
        self.input.clone()
    }

    pub fn output(&self) -> String {
        self.output.clone()
    }

    pub fn origin(&self) -> Option<AnchoredRule> {
        self.origin.clone()
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
struct CharacterDefinition(HashMap<char, String>);

impl CharacterDefinition {
    fn new() -> Self {
        Self(HashMap::new())
    }

    fn insert(&mut self, from: char, to: &str) {
        if cfg!(feature = "backwards_compatibility") {
            // first rule wins
            self.0.entry(from).or_insert(to.to_string());
        } else {
            // last rule wins
            self.0.insert(from, to.to_string());
        }
    }

    fn get(&self, from: &char) -> Option<&String> {
        self.0.get(from)
    }

    fn resolve_implicit_dots(&self, chars: &str) -> Result<String, TranslationError> {
        chars
            .chars()
            .map(|c| {
                self.get(&c)
                    .ok_or(TranslationError::ImplicitCharacterNotDefined(c))
                    .map(|t| t.to_string())
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
    /// Defines a mapping between characters and a string to translate to. Not used for translation,
    /// only to resolve implicit braille
    character_definitions: CharacterDefinition,
    character_attributes: CharacterAttributes,
    attributes: AttributeMapping,
    /// A prefix tree that contains all the translation rules and their [`Translations`](Translation)
    trie: Trie,
    match_patterns: MatchPatterns,
    /// All the nocross translation rules are stored in a separate trie
    nocross_trie: Trie,
    hyphenator: Option<Standard>,
    numeric_indicator: numeric::Indicator,
    uppercase_indicator: uppercase::Indicator,
    lettersign_indicator: lettersign::Indicator,
    nocontract_indicator: nocontract::Indicator,
    direction: Direction,
}

/// A builder for [`TranslationTable`]
#[derive(Debug)]
struct TranslationTableBuilder {
    undefined: Option<String>,
    character_definitions: CharacterDefinition,
    character_attributes: CharacterAttributes,
    attributes: AttributeMapping,
    trie: Trie,
    nocross_trie: Trie,
    hyphenator: Option<Standard>,
    match_patterns: MatchPatterns,
    numeric_indicator: numeric::IndicatorBuilder,
    uppercase_indicator: uppercase::IndicatorBuilder,
    lettersign_indicator: lettersign::IndicatorBuilder,
    nocontract_indicator: nocontract::IndicatorBuilder,
}

impl TranslationTableBuilder {
    fn new() -> Self {
        Self {
            undefined: None,
            character_definitions: CharacterDefinition::new(),
            character_attributes: CharacterAttributes::new(),
            attributes: AttributeMapping::new(),
            trie: Trie::new(),
            nocross_trie: Trie::new(),
            hyphenator: None,
            match_patterns: MatchPatterns::new(),
            numeric_indicator: numeric::IndicatorBuilder::new(),
            uppercase_indicator: uppercase::IndicatorBuilder::new(),
            lettersign_indicator: lettersign::IndicatorBuilder::new(),
            nocontract_indicator: nocontract::IndicatorBuilder::new(),
        }
    }

    fn get_trie_mut(&mut self, rule: &AnchoredRule) -> &mut Trie {
        if rule.is_nocross() {
            &mut self.nocross_trie
        } else {
            &mut self.trie
        }
    }

    fn insert_character_definition(
        &mut self,
        c: &char,
        dots: &str,
        attributes: Vec<Attribute>,
        direction: Direction,
        rule: &AnchoredRule,
    ) {
        self.character_definitions.insert(*c, dots);
        for attribute in attributes {
            self.character_attributes.insert(attribute, *c);
        }
        self.trie
            .insert_char(*c, dots, direction, rule.precedence(), rule);
        self.nocross_trie
            .insert_char(*c, dots, direction, rule.precedence(), rule);
    }

    fn build(self, direction: Direction) -> TranslationTable {
        TranslationTable {
            undefined: self.undefined,
            direction,
            character_definitions: self.character_definitions,
            character_attributes: self.character_attributes,
            attributes: self.attributes,
            trie: self.trie,
            nocross_trie: self.nocross_trie,
            hyphenator: self.hyphenator,
            match_patterns: self.match_patterns,
            numeric_indicator: self.numeric_indicator.build(),
            uppercase_indicator: self.uppercase_indicator.build(),
            lettersign_indicator: self.lettersign_indicator.build(),
            nocontract_indicator: self.nocontract_indicator.build(),
        }
    }
}

impl TranslationTable {
    pub fn compile(
        rules: Vec<AnchoredRule>,
        direction: Direction,
    ) -> Result<Self, TranslationError> {
        let mut builder = TranslationTableBuilder::new();

        let rules: Vec<AnchoredRule> = rules
            .into_iter()
            .filter(|r| r.is_direction(direction))
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
                    builder.insert_character_definition(
                        character,
                        &dots_to_unicode(dots),
                        vec![Attribute::Digit],
                        direction,
                        rule,
                    );
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
                    builder.undefined = Some(dots_to_unicode(dots));
                }
                Rule::Space {
                    character, dots, ..
                } => {
                    builder.insert_character_definition(
                        character,
                        &dots_to_unicode(dots),
                        vec![Attribute::Space],
                        direction,
                        rule,
                    );
                }
                Rule::Punctuation {
                    character, dots, ..
                } => {
                    builder.insert_character_definition(
                        character,
                        &dots_to_unicode(dots),
                        vec![Attribute::Punctuation],
                        direction,
                        rule,
                    );
                }
                Rule::Digit {
                    character, dots, ..
                }
                | Rule::Litdigit {
                    character, dots, ..
                } => {
                    builder.insert_character_definition(
                        character,
                        &dots_to_unicode(dots),
                        vec![Attribute::Digit],
                        direction,
                        rule,
                    );
                }
                Rule::Letter {
                    character, dots, ..
                } => {
                    builder.insert_character_definition(
                        character,
                        &dots_to_unicode(dots),
                        vec![Attribute::Letter],
                        direction,
                        rule,
                    );
                }
                Rule::Lowercase {
                    character, dots, ..
                } => {
                    builder.insert_character_definition(
                        character,
                        &dots_to_unicode(dots),
                        vec![Attribute::Lowercase, Attribute::Letter],
                        direction,
                        rule,
                    );
                }
                Rule::Uppercase {
                    character, dots, ..
                } => {
                    builder.insert_character_definition(
                        character,
                        &dots_to_unicode(dots),
                        vec![Attribute::Uppercase, Attribute::Letter],
                        direction,
                        rule,
                    );
                }
                Rule::Sign {
                    character, dots, ..
                } => {
                    builder.insert_character_definition(
                        character,
                        &dots_to_unicode(dots),
                        vec![Attribute::Sign],
                        direction,
                        rule,
                    );
                }
                Rule::Math {
                    character, dots, ..
                } => {
                    builder
                        .character_definitions
                        .insert(*character, &dots_to_unicode(dots));
                    // TODO: should the math opcode not also define a CharacterAttribute?
                    builder.trie.insert_char(
                        *character,
                        &dots_to_unicode(dots),
                        direction,
                        rule.precedence(),
                        rule,
                    );
                }
                Rule::Numsign { dots } => {
                    builder
                        .numeric_indicator
                        .numsign(&dots_to_unicode(dots), rule);
                }
                Rule::Nonumsign { dots } => {
                    builder
                        .numeric_indicator
                        .nonumsign(&dots_to_unicode(dots), rule);
                }
                Rule::Numericnocontchars { chars } => {
                    builder.numeric_indicator.numericnocontchars(&chars);
                }
                Rule::Numericmodechars { chars } => {
                    builder.numeric_indicator.numericmodechars(&chars);
                }
                Rule::Capsletter { dots, .. } => {
                    builder
                        .uppercase_indicator
                        .capsletter(&dots_to_unicode(dots), rule);
                }
                Rule::Begcapsword { dots, .. } => {
                    builder
                        .uppercase_indicator
                        .begcapsword(&dots_to_unicode(dots), rule);
                }
                Rule::Endcapsword { dots, .. } => {
                    builder
                        .uppercase_indicator
                        .endcapsword(&dots_to_unicode(dots), rule);
                }
                Rule::Begcaps { dots } => {
                    builder
                        .uppercase_indicator
                        .begcaps(&dots_to_unicode(dots), rule);
                }
                Rule::Endcaps { dots } => {
                    builder
                        .uppercase_indicator
                        .endcaps(&dots_to_unicode(dots), rule);
                }
                Rule::Capsmodechars { chars } => {
                    builder.uppercase_indicator.capsmodechars(&chars);
                }
                Rule::Letsign { dots } => {
                    builder
                        .lettersign_indicator
                        .letsign(&dots_to_unicode(dots), rule);
                }
                Rule::Contraction { chars } => {
                    builder.lettersign_indicator.contraction(&chars, rule);
                    builder.nocontract_indicator.contraction(&chars, rule);
                }
                Rule::Nocontractsign { dots } => {
                    builder
                        .nocontract_indicator
                        .nocontractsign(&dots_to_unicode(dots), rule);
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
                    if let Some(translation) = builder.character_definitions.get(base).cloned() {
                        builder.character_definitions.insert(*derived, &translation);
                        builder.trie.insert_char(
                            *derived,
                            &translation,
                            direction,
                            rule.precedence(),
                            rule,
                        );
                        if let Some(attribute) = builder.attributes.get(name) {
                            builder.character_attributes.insert(attribute, *derived)
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
                    let dots = builder
                        .character_definitions
                        .braille_to_unicode(dots, chars)?;
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Boundary::None,
                        Boundary::None,
                        direction,
                        rule.precedence(),
                        rule,
                    );
                }
                Rule::Word { chars, dots, .. } => {
                    let dots = builder
                        .character_definitions
                        .braille_to_unicode(dots, chars)?;
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Boundary::Word,
                        Boundary::Word,
                        direction,
                        rule.precedence(),
                        rule,
                    );
                }
                Rule::Begword { chars, dots, .. } => {
                    let dots = builder
                        .character_definitions
                        .braille_to_unicode(dots, chars)?;
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Boundary::Word,
                        Boundary::NotWord,
                        direction,
                        rule.precedence(),
                        rule,
                    )
                }
                Rule::Midword { chars, dots, .. } | Rule::Partword { chars, dots, .. } => {
                    let dots = builder
                        .character_definitions
                        .braille_to_unicode(dots, chars)?;
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Boundary::NotWord,
                        Boundary::NotWord,
                        direction,
                        rule.precedence(),
                        rule,
                    )
                }
                Rule::Midendword { chars, dots, .. } => {
                    let dots = builder
                        .character_definitions
                        .braille_to_unicode(dots, chars)?;
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Boundary::NotWord,
                        Boundary::None,
                        direction,
                        rule.precedence(),
                        rule,
                    );
                }
                Rule::Endword { chars, dots, .. } => {
                    let dots = builder
                        .character_definitions
                        .braille_to_unicode(dots, chars)?;
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Boundary::None,
                        Boundary::Word,
                        direction,
                        rule.precedence(),
                        rule,
                    );
                }
                Rule::Prfword { chars, dots, .. } => {
                    let dots = builder
                        .character_definitions
                        .braille_to_unicode(dots, chars)?;
                    // a prfword is basically syntactic sugar for a word rule combined with an
                    // endword rule. So just make the two appropriate insertions in the trie
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Boundary::Word,
                        Boundary::Word,
                        direction,
                        rule.precedence(),
                        rule,
                    );
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Boundary::None,
                        Boundary::Word,
                        direction,
                        rule.precedence(),
                        rule,
                    );
                }
                Rule::Sufword { chars, dots, .. } => {
                    let dots = builder
                        .character_definitions
                        .braille_to_unicode(dots, chars)?;
                    // a sufword is basically syntactic sugar for a word rule combined with an
                    // begword rule. So just make the two appropriate insertions in the trie
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Boundary::Word,
                        Boundary::Word,
                        direction,
                        rule.precedence(),
                        rule,
                    );
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Boundary::Word,
                        Boundary::None,
                        direction,
                        rule.precedence(),
                        rule,
                    );
                }
                Rule::Begmidword { chars, dots, .. } => {
                    let dots = builder
                        .character_definitions
                        .braille_to_unicode(dots, chars)?;
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Boundary::None,
                        Boundary::NotWord,
                        direction,
                        rule.precedence(),
                        rule,
                    );
                }
                Rule::Joinword { chars, dots, .. } | Rule::Lowword { chars, dots, .. } => {
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots_to_unicode(dots),
                        Boundary::Word,
                        Boundary::Word,
                        direction,
                        rule.precedence(),
                        rule,
                    )
                }
                Rule::Begnum { chars, dots, .. } => builder.get_trie_mut(rule).insert(
                    chars,
                    &dots_to_unicode(dots),
                    Boundary::Word,
                    Boundary::WordNumber,
                    direction,
                    rule.precedence(),
                    rule,
                ),
                Rule::Midnum { chars, dots, .. } => builder.get_trie_mut(rule).insert(
                    chars,
                    &dots_to_unicode(dots),
                    Boundary::Number,
                    Boundary::Number,
                    direction,
                    rule.precedence(),
                    rule,
                ),
                Rule::Endnum { chars, dots, .. } => {
                    let dots = builder
                        .character_definitions
                        .braille_to_unicode(dots, chars)?;
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Boundary::NumberWord,
                        Boundary::Word,
                        direction,
                        rule.precedence(),
                        rule,
                    );
                }
                Rule::Match {
                    pre,
                    chars,
                    post,
                    dots,
                    ..
                } => {
                    let dots = builder
                        .character_definitions
                        .braille_to_unicode(dots, chars)?;
                    builder.match_patterns.insert(pre, chars, post, &dots, rule);
                }
                Rule::IncludeHyphenation { path } => {
                    let file = File::open(path)?;
                    let mut reader = io::BufReader::new(file);
                    builder.hyphenator = Some(Standard::any_from_reader(&mut reader)?);
                }

                _ => (),
            }
        }

        builder.numeric_indicator.numeric_characters(
            builder
                .character_attributes
                .get(Attribute::Digit)
                .unwrap_or(HashSet::default()),
        );
        builder.uppercase_indicator.uppercase_characters(
            builder
                .character_attributes
                .get(Attribute::Uppercase)
                .unwrap_or(HashSet::default()),
        );
        builder.uppercase_indicator.letter_characters(
            builder
                .character_attributes
                .get(Attribute::Letter)
                .unwrap_or(HashSet::default()),
        );
        Ok(builder.build(direction))
    }

    fn update_offsets(&self, translations: Vec<Translation>, decrement: usize) -> Vec<Translation> {
        translations
            .into_iter()
            .map(|t| t.decrement_offset(decrement))
            .collect()
    }

    pub fn translate(&self, input: &str) -> String {
        self.trace(input)
            .iter()
            .map(|t| t.output.as_str())
            .collect()
    }

    fn translation_candidates(
        &self,
        input: &str,
        prev: Option<char>,
    ) -> (Vec<Translation>, Vec<Translation>) {
        self.trie
            .find_translations(input, prev)
            .into_iter()
            // TODO: figure out what to do with delayed rules that have a negative offset, i.e.
            // if there was a matching rule that consumed so much input that the delayed rule is
            // no longer applicable
            .partition(|t| t.offset == 0)
    }

    fn word_hyphenates(&self, input: &str) -> bool {
        match &self.hyphenator {
            Some(hyphenator) => !hyphenator.opportunities(&input.to_lowercase()).is_empty(),
            // if there is no hyphenator claim that the word hyphenates. Then it will not be used as
            // a nocross candidate
            _ => true,
        }
    }

    fn nocross_candidates(&self, input: &str, prev: Option<char>) -> Vec<Translation> {
        self.nocross_trie
            .find_translations(input, prev)
            .into_iter()
            .filter(|t| !self.word_hyphenates(&t.input))
            .collect()
    }

    fn match_candidates(&self, input: &str) -> (Vec<Translation>, Vec<Translation>) {
        self.match_patterns
            .find_translations(input)
            .into_iter()
            .partition(|t| t.offset == 0)
    }

    fn partition_delayed_translations(
        &self,
        delayed: Vec<Translation>,
    ) -> (Vec<Translation>, Vec<Translation>) {
        delayed.into_iter().partition(|t| t.offset == 0)
    }

    pub fn trace(&self, input: &str) -> Vec<Translation> {
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
            if let Some(translation) = self.lettersign_indicator.next(chars.as_str(), prev) {
                translations.push(translation);
            }
            if let Some(translation) = self.nocontract_indicator.next(chars.as_str(), prev) {
                translations.push(translation);
            }
            if let Some(translation) = numeric_indicator.next(chars.as_str()) {
                translations.push(translation);
            }
            if let Some(translation) = uppercase_indicator.next(chars.as_str()) {
                translations.push(translation);
            }
            // First check for nocross candidates
            let nocross_candidate = self
                .nocross_candidates(chars.as_str(), prev)
                .into_iter()
                .max_by_key(|t| t.weight);

            // given an input query the trie for matching translations. Then split off the
            // translations that are delayed, i.e. have an offset because they have a pre-pattern
            let (mut candidates, delayed) = self.translation_candidates(chars.as_str(), prev);
            delayed_translations.extend(delayed);
            // then search for matching match patterns. Unless they have empty pre patterns they will all have
            // an offset. Split those off.
            let (match_candidates, match_delayed) = self.match_candidates(chars.as_str());
            delayed_translations.extend(match_delayed);
            // merge the candidates from the match patters with the candidates from the plain translations
            candidates.extend(match_candidates);

            // move delayed_translations with zero offset into candidates
            let (current, delayed) = self.partition_delayed_translations(delayed_translations);
            delayed_translations = delayed;
            candidates.extend(current);

            // use the longest translation
            let candidate = candidates
                .iter()
                .max_by_key(|translation| translation.weight);
            if let Some(t) = candidate {
                if let Some(nocross) = nocross_candidate
                    && nocross.weight >= t.weight
                {
                    // Use the nocross translation if it is at least as long as the normal translation
                    let translation = nocross.clone();
                    // move the iterator forward by the number of characters in the translation
                    chars.nth(nocross.length - 1);
                    prev = translation.input.chars().last();
                    translations.push(translation);
                    delayed_translations = self.update_offsets(delayed_translations, t.length);
                } else {
                    // there is a matching translation rule
                    let translation = t.clone();
                    // move the iterator forward by the number of characters in the translation
                    chars.nth(t.length - 1);
                    prev = translation.input.chars().last();
                    translations.push(translation);
                    delayed_translations = self.update_offsets(delayed_translations, t.length);
                }
            } else if let Some(next_char) = chars.next() {
                prev = Some(next_char);
                // no translation rule found
                if let Some(ref replacement) = self.undefined {
                    // there is a rule for undefined characters
                    let translation =
                        Translation::new(&next_char.to_string(), replacement, 1, None); // FIXME: add the undefined rule here
                    translations.push(translation);
                    delayed_translations = self.update_offsets(delayed_translations, 1);
                } else {
                    // otherwise handle it as a undefined character
                    let translation = Translation::new(
                        &next_char.to_string(),
                        &self.handle_undefined_char(next_char),
                        1,
                        None,
                    );
                    translations.push(translation);
                    delayed_translations = self.update_offsets(delayed_translations, 1);
                }
            } else {
                // the chars iterator is exhausted
                break;
            }
        }
        translations
    }

    fn handle_undefined_char(&self, ch: char) -> String {
        ch.escape_unicode()
            .to_string()
            .replace(r"\u", r"\x") // replace \u by \x
            .replace(['{', '}'], "") // drop the curly braces
            .chars()
            .map(|c| {
                if let Some(t) = self.character_definitions.get(&c) {
                    t.clone()
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
            .filter(|r| r.is_direction(direction))
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

    use crate::{parser::expand_includes, RuleParser};

    fn parse_rule(source: &str) -> AnchoredRule {
        RuleParser::new(source).rule().unwrap().into()
    }

    #[test]
    fn resolve_implicit_dots() {
        let char_defs = CharacterDefinition::new();
        assert!(char_defs.resolve_implicit_dots("xs").is_err());
        let mut char_defs = CharacterDefinition::new();
        char_defs.insert('a', "A");
        char_defs.insert('h', "H");
        assert_eq!(char_defs.resolve_implicit_dots("haha").unwrap(), "HAHA");
    }

    #[test]
    fn translate() {
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
    fn midword() {
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
    fn midword_with_precedence() {
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
    fn endword() {
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
    fn partword() {
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
    fn sufword() {
        let rules = vec![
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 2"),
            parse_rule("lowercase f 3"),
            parse_rule("lowercase o 4"),
            parse_rule("lowercase r 5"),
            parse_rule("punctuation . 6"),
            parse_rule("always foo 14"),
            parse_rule("sufword bar 15"),
            parse_rule("space \\s 0"),
        ];
        let table = TranslationTable::compile(rules, Direction::Forward).unwrap();
        assert_eq!(table.translate("bar"), "⠑"); // bar should be contracted
        assert_eq!(table.translate("foobar"), "⠉⠂⠁⠐"); // bar should not be contracted
        assert_eq!(table.translate("barfoo"), "⠑⠉"); // bar should be contracted
        assert_eq!(table.translate("foobar."), "⠉⠂⠁⠐⠠"); // bar should not be contracted
        assert_eq!(table.translate("foobarfoo"), "⠉⠂⠁⠐⠉"); // bar should not be contracted
        assert_eq!(table.translate("foobar foo"), "⠉⠂⠁⠐⠀⠉"); // bar should not be contracted
        assert_eq!(table.translate("foobar. foo"), "⠉⠂⠁⠐⠠⠀⠉"); // bar should not be contracted
        assert_eq!(table.translate("foo bar foo"), "⠉⠀⠑⠀⠉"); // bar should be contracted
    }

    #[test]
    fn prfword() {
        let rules = vec![
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 2"),
            parse_rule("lowercase f 3"),
            parse_rule("lowercase o 4"),
            parse_rule("lowercase r 5"),
            parse_rule("punctuation . 6"),
            parse_rule("always foo 14"),
            parse_rule("prfword bar 15"),
            parse_rule("space \\s 0"),
        ];
        let table = TranslationTable::compile(rules, Direction::Forward).unwrap();
        assert_eq!(table.translate("bar"), "⠑"); // bar should be contracted
        assert_eq!(table.translate("foobar"), "⠉⠑"); // bar should be contracted
        assert_eq!(table.translate("barfoo"), "⠂⠁⠐⠉"); // bar should be not contracted
        assert_eq!(table.translate("foobar."), "⠉⠑⠠"); // bar should be contracted
        assert_eq!(table.translate("foobarfoo"), "⠉⠂⠁⠐⠉"); // bar should not be contracted
        assert_eq!(table.translate("foobar foo"), "⠉⠑⠀⠉"); // bar should be contracted
        assert_eq!(table.translate("foobar. foo"), "⠉⠑⠠⠀⠉"); // bar should be contracted
        assert_eq!(table.translate("foo bar foo"), "⠉⠀⠑⠀⠉"); // bar should be contracted
    }

    #[test]
    fn begnum() {
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
    fn endnum() {
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
    fn base() {
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
    fn display_table() {
        let display_rules = vec![parse_rule("display a 1"), parse_rule("display \\s 0")];
        let display_table = DisplayTable::compile(display_rules, Direction::Forward);
        assert_eq!(display_table.translate("⠁"), "a");
        assert_eq!(display_table.translate("⠀"), " ");
        assert_eq!(display_table.translate(""), "");
        assert_eq!(display_table.translate("x"), "x"); // unknown chars are translated to themselves
    }

    #[test]
    fn translate_with_display() {
        let display_rules = vec![parse_rule("display A 1"), parse_rule("display \\s 0")];
        let rules = vec![parse_rule("letter a 1"), parse_rule("space \\s 0")];
        let display_table = DisplayTable::compile(display_rules, Direction::Forward);
        let table = TranslationTable::compile(rules, Direction::Forward).unwrap();
        assert_eq!(display_table.translate(&table.translate("a")), "A");
        assert_eq!(display_table.translate(&table.translate(" ")), " ");
        assert_eq!(display_table.translate(&table.translate("a a")), "A A");
    }

    #[test]
    fn match_simple() {
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
    fn match_with_any() {
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
    fn match_with_set() {
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
    fn numeric_indication() {
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
    fn uppercase_indication() {
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

    #[test]
    fn lettersign_indication() {
        let rules = vec![
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 12"),
            parse_rule("lowercase c 14"),
            parse_rule("lowercase d 145"),
            parse_rule("lowercase e 15"),
            parse_rule("lowercase f 124"),
            parse_rule("lowercase o 135"),
            parse_rule("lowercase u 136"),
            parse_rule("lowercase t 2345"),
            parse_rule("letsign 6"),
            parse_rule("word about 1-12"),
            parse_rule("contraction ab"),
            parse_rule("contraction cd"),
        ];
        let table = TranslationTable::compile(rules, Direction::Forward).unwrap();
        assert_eq!(table.translate("about"), "⠁⠃");
        assert_eq!(table.translate("ab"), "⠠⠁⠃");
        assert_eq!(table.translate("cd"), "⠠⠉⠙");
        assert_eq!(table.translate("abcd"), "⠁⠃⠉⠙");
        assert_eq!(table.translate("ef"), "⠑⠋");
    }

    #[test]
    fn nocontractsign_indication() {
        let rules = vec![
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 12"),
            parse_rule("lowercase c 14"),
            parse_rule("lowercase d 145"),
            parse_rule("lowercase e 15"),
            parse_rule("lowercase f 124"),
            parse_rule("lowercase o 135"),
            parse_rule("lowercase u 136"),
            parse_rule("lowercase t 2345"),
            parse_rule("nocontractsign 6"),
            parse_rule("word about 1-12"),
            parse_rule("contraction ab"),
            parse_rule("contraction cd"),
        ];
        let table = TranslationTable::compile(rules, Direction::Forward).unwrap();
        assert_eq!(table.translate("about"), "⠁⠃");
        assert_eq!(table.translate("ab"), "⠠⠁⠃");
        assert_eq!(table.translate("cd"), "⠠⠉⠙");
        assert_eq!(table.translate("abcd"), "⠁⠃⠉⠙");
        assert_eq!(table.translate("ef"), "⠑⠋");
    }

    #[test]
    #[ignore = "Doesn't seem to work at the moment"]
    fn nocross() {
        let rules = vec![
            parse_rule("include dictionaries/de-g1-core-patterns.dic"),
	    parse_rule("lowercase a 1"),
	    parse_rule("lowercase b 12"),
	    parse_rule("lowercase h 125"),
	    parse_rule("lowercase o 135"),
	    parse_rule("lowercase s 234"),
	    parse_rule("lowercase t 2345"),
	    parse_rule("lowercase u 136"),
            parse_rule("always hausboot 123"),
            parse_rule("nocross always hausboot 456"),
            parse_rule("always fff 123"),
            parse_rule("nocross always fff 456"),
            parse_rule("space \\s 0"),
        ];
	let rules = expand_includes(rules).unwrap();
        let table = TranslationTable::compile(rules, Direction::Forward).unwrap();
        assert_eq!(table.translate("hausboot"), "⠇");
        assert_eq!(table.translate("fff"), "⠸");

    }

    #[test]
    fn backtranslation() {
        let rules = vec![
            parse_rule("lowercase f 3"),
            parse_rule("lowercase o 4"),
            parse_rule("always foo 123"),
            parse_rule("always bar 456"),
            parse_rule("space \\s 0"),
        ];
        let table = TranslationTable::compile(rules, Direction::Backward).unwrap();
        assert_eq!(table.translate("⠇⠸"), "foobar");
        assert_eq!(table.translate("⠀⠀"), "  ");
        assert_eq!(table.translate("⠄⠈⠈"), "foo");
    }
}
