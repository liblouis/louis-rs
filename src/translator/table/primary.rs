use std::{fs::File, io};

use hyphenation::{Hyphenator, Load, Standard};

use crate::{
    Direction,
    parser::{
        AnchoredRule, Braille, CharacterClass, CharacterClasses, HasNocross, HasPrecedence,
        fallback,
    },
    translator::{
        CharacterDefinition, ResolvedTranslation, Rule, TranslationError, TranslationStage,
        dots_to_unicode,
        indication::{lettersign, nocontract, numeric, uppercase},
        match_pattern::{MatchPatterns, MatchPatternsBuilder},
        table::TableContext,
        trie::{Boundary, Trie},
    },
};

#[derive(Debug)]
pub struct PrimaryTable {
    undefined: Option<String>,
    character_definitions: CharacterDefinition,
    character_classes: CharacterClasses,
    /// A prefix tree that contains all the translation rules and their [`ResolvedTranslations`](ResolvedTranslation)
    trie: Trie,
    match_patterns: MatchPatterns,
    /// All the nocross translation rules are stored in a separate trie
    nocross_trie: Trie,
    hyphenator: Option<Standard>,
    numeric_indicator: numeric::Indicator,
    uppercase_indicator: uppercase::Indicator,
    lettersign_indicator: lettersign::Indicator,
    nocontract_indicator: nocontract::Indicator,
    stage: TranslationStage,
    direction: Direction,
}

/// A builder for [`PrimaryTable`]
#[derive(Debug)]
struct PrimaryTableBuilder {
    undefined: Option<String>,
    trie: Trie,
    nocross_trie: Trie,
    hyphenator: Option<Standard>,
    match_patterns: MatchPatternsBuilder,
    numeric_indicator: numeric::IndicatorBuilder,
    uppercase_indicator: uppercase::IndicatorBuilder,
    lettersign_indicator: lettersign::IndicatorBuilder,
    nocontract_indicator: nocontract::IndicatorBuilder,
}

impl PrimaryTableBuilder {
    fn new() -> Self {
        Self {
            undefined: None,
            trie: Trie::new(),
            nocross_trie: Trie::new(),
            hyphenator: None,
            match_patterns: MatchPatternsBuilder::new(),
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

    fn insert_character(&mut self, c: char, dots: &str, direction: Direction, rule: &AnchoredRule) {
        self.trie.insert_char(
            c,
            dots,
            direction,
            rule.precedence(),
            TranslationStage::Main,
            rule,
        );
        self.nocross_trie.insert_char(
            c,
            dots,
            direction,
            rule.precedence(),
            TranslationStage::Main,
            rule,
        );
    }

    fn build(self, direction: Direction, ctx: &TableContext) -> PrimaryTable {
        PrimaryTable {
            undefined: self.undefined,
            direction,
            stage: TranslationStage::Main,
            character_definitions: ctx.character_definitions().clone(),
            character_classes: ctx.character_classes().clone(),
            trie: self.trie,
            nocross_trie: self.nocross_trie,
            hyphenator: self.hyphenator,
            match_patterns: self.match_patterns.build(),
            numeric_indicator: self.numeric_indicator.build(),
            uppercase_indicator: self.uppercase_indicator.build(),
            lettersign_indicator: self.lettersign_indicator.build(),
            nocontract_indicator: self.nocontract_indicator.build(),
        }
    }
}

impl PrimaryTable {
    pub fn compile(
        rules: &[AnchoredRule],
        direction: Direction,
        stage: TranslationStage,
        ctx: &TableContext,
    ) -> Result<Self, TranslationError> {
        let mut builder = PrimaryTableBuilder::new();

        for rule in rules {
            match &rule.rule {
                Rule::Undefined { dots } => {
                    builder.undefined = Some(dots_to_unicode(dots));
                }
                Rule::Space {
                    character, dots, ..
                } => {
                    builder.insert_character(*character, &dots_to_unicode(dots), direction, rule);
                }
                Rule::Punctuation {
                    character, dots, ..
                } => {
                    builder.insert_character(*character, &dots_to_unicode(dots), direction, rule);
                }
                Rule::Digit {
                    character, dots, ..
                } => {
                    builder.insert_character(*character, &dots_to_unicode(dots), direction, rule);
                }
                Rule::Litdigit {
                    character, dots, ..
                } => {
                    builder.insert_character(*character, &dots_to_unicode(dots), direction, rule);
                }
                Rule::Letter {
                    character, dots, ..
                } => {
                    builder.insert_character(*character, &dots_to_unicode(dots), direction, rule);
                }
                Rule::Lowercase {
                    character, dots, ..
                } => {
                    builder.insert_character(*character, &dots_to_unicode(dots), direction, rule);
                }
                Rule::Uppercase {
                    character, dots, ..
                } => {
                    builder.insert_character(*character, &dots_to_unicode(dots), direction, rule);
                }
                Rule::Sign {
                    character, dots, ..
                } => {
                    builder.insert_character(*character, &dots_to_unicode(dots), direction, rule);
                }
                Rule::Math {
                    character, dots, ..
                } => {
                    builder.insert_character(*character, &dots_to_unicode(dots), direction, rule);
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
                    builder.numeric_indicator.numericnocontchars(chars);
                }
                Rule::Numericmodechars { chars } => {
                    builder.numeric_indicator.numericmodechars(chars);
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
                    builder.uppercase_indicator.capsmodechars(chars);
                }
                Rule::Letsign { dots } => {
                    builder
                        .lettersign_indicator
                        .letsign(&dots_to_unicode(dots), rule);
                }
                // Treat a contraction rule similarly to a word rule. Pretend the dots have been
                // defined implicitely
                Rule::Contraction { chars } => {
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(&Braille::Implicit, chars)?;
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Boundary::Word,
                        Boundary::Word,
                        direction,
                        rule.precedence(),
                        TranslationStage::Main,
                        rule,
                    );
                    builder.lettersign_indicator.contraction(chars, rule);
                    builder.nocontract_indicator.contraction(chars, rule);
                }
                Rule::Nocontractsign { dots } => {
                    builder
                        .nocontract_indicator
                        .nocontractsign(&dots_to_unicode(dots), rule);
                }
                Rule::Base {
                    derived,
                    base,
                    name,
                } => {
                    if let Some(translation) = ctx.character_definitions().get(base).cloned() {
                        builder.trie.insert_char(
                            *derived,
                            &translation,
                            direction,
                            rule.precedence(),
                            TranslationStage::Main,
                            rule,
                        );
                    } else {
                        // hm, there is no character definition for the base character.
                        // If we are backwards compatible ignore the problem, otherwise
                        // throw an error
                        if !cfg!(feature = "backwards_compatibility") {
                            return Err(TranslationError::BaseCharacterNotDefined {
                                base: *base,
                                derived: *derived,
                            });
                        }
                    }
                }
                Rule::Comp6 { chars, dots } | Rule::Always { chars, dots, .. } => {
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Boundary::None,
                        Boundary::None,
                        direction,
                        rule.precedence(),
                        TranslationStage::Main,
                        rule,
                    );
                }
                Rule::Word { chars, dots, .. } => {
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Boundary::Word,
                        Boundary::Word,
                        direction,
                        rule.precedence(),
                        TranslationStage::Main,
                        rule,
                    );
                }
                Rule::Begword { chars, dots, .. } => {
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Boundary::Word,
                        Boundary::NotWord,
                        direction,
                        rule.precedence(),
                        TranslationStage::Main,
                        rule,
                    )
                }
                Rule::Midword { chars, dots, .. } | Rule::Partword { chars, dots, .. } => {
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Boundary::NotWord,
                        Boundary::NotWord,
                        direction,
                        rule.precedence(),
                        TranslationStage::Main,
                        rule,
                    )
                }
                Rule::Midendword { chars, dots, .. } => {
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Boundary::NotWord,
                        Boundary::None,
                        direction,
                        rule.precedence(),
                        TranslationStage::Main,
                        rule,
                    );
                }
                Rule::Endword { chars, dots, .. } => {
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Boundary::None,
                        Boundary::Word,
                        direction,
                        rule.precedence(),
                        TranslationStage::Main,
                        rule,
                    );
                }
                Rule::Prfword { chars, dots, .. } => {
                    let dots = ctx
                        .character_definitions()
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
                        TranslationStage::Main,
                        rule,
                    );
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Boundary::None,
                        Boundary::Word,
                        direction,
                        rule.precedence(),
                        TranslationStage::Main,
                        rule,
                    );
                }
                Rule::Sufword { chars, dots, .. } => {
                    let dots = ctx
                        .character_definitions()
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
                        TranslationStage::Main,
                        rule,
                    );
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Boundary::Word,
                        Boundary::None,
                        direction,
                        rule.precedence(),
                        TranslationStage::Main,
                        rule,
                    );
                }
                Rule::Begmidword { chars, dots, .. } => {
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Boundary::None,
                        Boundary::NotWord,
                        direction,
                        rule.precedence(),
                        TranslationStage::Main,
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
                        TranslationStage::Main,
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
                    TranslationStage::Main,
                    rule,
                ),
                Rule::Midnum { chars, dots, .. } => builder.get_trie_mut(rule).insert(
                    chars,
                    &dots_to_unicode(dots),
                    Boundary::Number,
                    Boundary::Number,
                    direction,
                    rule.precedence(),
                    TranslationStage::Main,
                    rule,
                ),
                Rule::Endnum { chars, dots, .. } => {
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Boundary::NumberWord,
                        Boundary::Word,
                        direction,
                        rule.precedence(),
                        TranslationStage::Main,
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
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    builder.match_patterns.insert(
                        pre,
                        chars,
                        post,
                        &dots,
                        rule,
                        ctx.character_classes(),
                    );
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
            ctx.character_classes()
                .get(&CharacterClass::Litdigit)
                .unwrap_or_default(),
        );
        builder.uppercase_indicator.uppercase_characters(
            ctx.character_classes()
                .get(&CharacterClass::Uppercase)
                .unwrap_or_default(),
        );
        builder.uppercase_indicator.letter_characters(
            ctx.character_classes()
                .get(&CharacterClass::Letter)
                .unwrap_or_default(),
        );
        Ok(builder.build(direction, ctx))
    }

    fn update_offsets(
        &self,
        translations: Vec<ResolvedTranslation>,
        decrement: usize,
    ) -> Vec<ResolvedTranslation> {
        translations
            .into_iter()
            .map(|t| t.decrement_offset(decrement))
            .collect()
    }

    pub fn translate(&self, input: &str) -> String {
        self.trace(input).iter().map(|t| t.output()).collect()
    }

    fn translation_candidates(
        &self,
        input: &str,
        prev: Option<char>,
    ) -> (Vec<ResolvedTranslation>, Vec<ResolvedTranslation>) {
        self.trie
            .find_translations(input, prev)
            .into_iter()
            // TODO: figure out what to do with delayed rules that have a negative offset, i.e.
            // if there was a matching rule that consumed so much input that the delayed rule is
            // no longer applicable
            .partition(|t| t.offset() == 0)
    }

    fn word_hyphenates(&self, input: &str) -> bool {
        match &self.hyphenator {
            Some(hyphenator) => !hyphenator.opportunities(&input.to_lowercase()).is_empty(),
            // if there is no hyphenator claim that the word hyphenates. Then it will not be used as
            // a nocross candidate
            _ => true,
        }
    }

    fn nocross_candidates(&self, input: &str, prev: Option<char>) -> Vec<ResolvedTranslation> {
        self.nocross_trie
            .find_translations(input, prev)
            .into_iter()
            .filter(|t| !self.word_hyphenates(&t.input()))
            .collect()
    }

    fn match_candidates(
        &self,
        input: &str,
    ) -> (Vec<ResolvedTranslation>, Vec<ResolvedTranslation>) {
        self.match_patterns
            .find(input)
            .into_iter()
            .partition(|t| t.offset() == 0)
    }

    fn partition_delayed_translations(
        &self,
        delayed: Vec<ResolvedTranslation>,
    ) -> (Vec<ResolvedTranslation>, Vec<ResolvedTranslation>) {
        delayed.into_iter().partition(|t| t.offset() == 0)
    }

    pub fn trace(&self, input: &str) -> Vec<ResolvedTranslation> {
        let mut translations: Vec<ResolvedTranslation> = Vec::new();
        let mut delayed_translations: Vec<ResolvedTranslation> = Vec::new();

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
                .max_by_key(|t| t.weight());

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
                .max_by_key(|translation| translation.weight());
            if let Some(t) = candidate {
                if let Some(nocross) = nocross_candidate
                    && nocross.weight() >= t.weight()
                {
                    // Use the nocross translation if it is at least as long as the normal translation
                    let translation = nocross.clone();
                    // move the iterator forward by the number of characters in the translation
                    chars.nth(nocross.length() - 1);
                    prev = translation.input().chars().last();
                    translations.push(translation);
                    delayed_translations = self.update_offsets(delayed_translations, t.length());
                } else {
                    // there is a matching translation rule
                    let translation = t.clone();
                    // move the iterator forward by the number of characters in the translation
                    chars.nth(t.length() - 1);
                    prev = translation.input().chars().last();
                    translations.push(translation);
                    delayed_translations = self.update_offsets(delayed_translations, t.length());
                }
            } else if let Some(next_char) = chars.next() {
                prev = Some(next_char);
                // no translation rule found
                if let Some(ref replacement) = self.undefined {
                    // there is a rule for undefined characters
                    let translation = ResolvedTranslation::new(
                        &next_char.to_string(),
                        replacement,
                        1,
                        TranslationStage::Main,
                        None, // FIXME: add the undefined rule here
                    );
                    translations.push(translation);
                    delayed_translations = self.update_offsets(delayed_translations, 1);
                } else {
                    // otherwise handle it as a undefined character
                    let translation = ResolvedTranslation::new(
                        &next_char.to_string(),
                        &self.handle_undefined_char(next_char),
                        1,
                        TranslationStage::Main,
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

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parser::{RuleParser, expand_includes};

    fn parse_rule(source: &str) -> AnchoredRule {
        RuleParser::new(source).rule().unwrap().into()
    }

    #[test]
    fn translate() {
        let rules = [
            parse_rule("always foo 123"),
            parse_rule("always bar 456"),
            parse_rule("space \\s 0"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("foobar"), "⠇⠸");
        assert_eq!(table.translate("  "), "⠀⠀");
        assert_eq!(table.translate("🐂"), "⠳⠭⠂⠋⠲⠴⠆");
    }

    #[test]
    fn midword() {
        let rules = [
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 2"),
            parse_rule("lowercase f 3"),
            parse_rule("lowercase o 4"),
            parse_rule("lowercase r 5"),
            parse_rule("always foo 14"),
            parse_rule("midword bar 15"),
            parse_rule("space \\s 0"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("bar"), "⠂⠁⠐"); // should not contract
        assert_eq!(table.translate("foobar"), "⠉⠂⠁⠐"); // only foo should be contracted
        assert_eq!(table.translate("foobarfoo"), "⠉⠑⠉"); // foo and bar should be contracted
        assert_eq!(table.translate("foobar foo"), "⠉⠂⠁⠐⠀⠉"); // only foo should be contracted
        assert_eq!(table.translate("foo bar foo"), "⠉⠀⠂⠁⠐⠀⠉"); // only foo should be contracted
    }

    #[test]
    fn midword_with_precedence() {
        let rules = [
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
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("bar"), "⠊"); // bar should contract with 24
        assert_eq!(table.translate("foobar"), "⠉⠊"); // bar should contract with 24
        assert_eq!(table.translate("foobarfoo"), "⠉⠢⠉"); // bar should contract with 26
        assert_eq!(table.translate("foobar foo"), "⠉⠊⠀⠉"); // bar should contract with 24
        assert_eq!(table.translate("foo bar foo"), "⠉⠀⠊⠀⠉"); // bar should contract with 24
    }

    #[test]
    fn endword() {
        let rules = [
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
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("bar"), "⠑"); // should contract
        assert_eq!(table.translate("foobar"), "⠉⠑"); // both should be contracted
        assert_eq!(table.translate("foobar."), "⠉⠑⠠"); // both should be contracted
        assert_eq!(table.translate("foobarfoo"), "⠉⠂⠁⠐⠉"); // only foo should be contracted
        assert_eq!(table.translate("foobar foo"), "⠉⠑⠀⠉"); // both should be contracted
        assert_eq!(table.translate("foo bar foo"), "⠉⠀⠑⠀⠉"); // both should be contracted
    }

    #[test]
    fn partword() {
        let rules = [
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
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
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
        let rules = [
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
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
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
        let rules = [
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
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
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
        let rules = [
            parse_rule("digit 1 1"),
            parse_rule("sign a 3456"),
            parse_rule("begnum a 4"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("1"), "⠁");
        assert_eq!(table.translate("a"), "⠼");
        assert_eq!(table.translate("a1"), "⠈⠁");
    }

    #[test]
    fn endnum() {
        let rules = [
            parse_rule("digit 1 1"),
            parse_rule("lowercase h 125"),
            parse_rule("lowercase t 2345"),
            parse_rule("punctuation . 6"),
            parse_rule("always th 14"),
            parse_rule("endnum th 15"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("th"), "⠉");
        assert_eq!(table.translate("1th"), "⠁⠑");
        assert_eq!(table.translate("1th."), "⠁⠑⠠");
    }

    #[test]
    fn base() {
        let rules = [
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 12"),
            parse_rule("base uppercase A a"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("a"), "⠁");
        assert_eq!(table.translate("A"), "⠁");
        assert_eq!(table.translate("ab"), "⠁⠃");
        assert_eq!(table.translate("Ab"), "⠁⠃");
    }

    #[test]
    fn match_simple() {
        let rules = [
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 2"),
            parse_rule("lowercase f 3"),
            parse_rule("lowercase o 4"),
            parse_rule("lowercase r 5"),
            parse_rule("match a foo b 14"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("foo"), "⠄⠈⠈");
        assert_eq!(table.translate("afoo"), "⠁⠄⠈⠈");
        assert_eq!(table.translate("afoob"), "⠁⠉⠂");
    }

    #[test]
    fn match_with_any() {
        let rules = [
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 2"),
            parse_rule("lowercase f 3"),
            parse_rule("lowercase o 4"),
            parse_rule("lowercase r 5"),
            parse_rule("match . foo b 14"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("foo"), "⠄⠈⠈");
        assert_eq!(table.translate("afoo"), "⠁⠄⠈⠈");
        assert_eq!(table.translate("afoob"), "⠁⠉⠂");
        assert_eq!(table.translate("ffoob"), "⠄⠉⠂");
    }

    #[test]
    fn match_with_set() {
        let rules = [
            parse_rule("lowercase f 3"),
            parse_rule("lowercase o 4"),
            parse_rule("lowercase s 7"),
            parse_rule("lowercase z 5"),
            parse_rule("match [fz] oo [fz] 14"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("oo"), "⠈⠈");
        assert_eq!(table.translate("soo"), "⡀⠈⠈");
        assert_eq!(table.translate("foof"), "⠄⠉⠄");
        assert_eq!(table.translate("zoof"), "⠐⠉⠄");
        assert_eq!(table.translate("soof"), "⡀⠈⠈⠄");
    }

    #[test]
    fn numeric_indication() {
        let rules = [
            parse_rule("lowercase f 3"),
            parse_rule("lowercase o 4"),
            parse_rule("litdigit 1 1"),
            parse_rule("litdigit 2 12"),
            parse_rule("litdigit 3 14"),
            parse_rule("numsign 3456"),
            parse_rule("nonumsign 56"),
            parse_rule("numericnocontchars abcdefghij"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("123"), "⠼⠁⠃⠉");
        assert_eq!(table.translate("123foo"), "⠼⠁⠃⠉⠰⠄⠈⠈");
        assert_eq!(table.translate("foof"), "⠄⠈⠈⠄");
    }

    #[test]
    fn uppercase_indication() {
        let rules = [
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 12"),
            parse_rule("lowercase c 14"),
            parse_rule("base uppercase A a"),
            parse_rule("base uppercase B b"),
            parse_rule("base uppercase C c"),
            parse_rule("capsletter 46"),
            parse_rule("begcapsword 6-6"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("abc"), "⠁⠃⠉");
        assert_eq!(table.translate("Abc"), "⠨⠁⠃⠉");
        assert_eq!(table.translate("ABC"), "⠠⠠⠁⠃⠉");
    }

    #[test]
    fn lettersign_indication() {
        let rules = [
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
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("about"), "⠁⠃");
        assert_eq!(table.translate("ab"), "⠠⠁⠃");
        assert_eq!(table.translate("cd"), "⠠⠉⠙");
        assert_eq!(table.translate("abcd"), "⠁⠃⠉⠙");
        assert_eq!(table.translate("ef"), "⠑⠋");
    }

    #[test]
    fn nocontractsign_indication() {
        let rules = [
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
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
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
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("hausboot"), "⠇");
        assert_eq!(table.translate("fff"), "⠸");
    }

    #[test]
    fn backtranslation() {
        let rules = [
            parse_rule("lowercase f 3"),
            parse_rule("lowercase o 4"),
            parse_rule("always foo 123"),
            parse_rule("always bar 456"),
            parse_rule("space \\s 0"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table = PrimaryTable::compile(
            &rules,
            Direction::Backward,
            TranslationStage::Main,
            &context,
        )
        .unwrap();
        assert_eq!(table.translate("⠇⠸"), "foobar");
        assert_eq!(table.translate("⠀⠀"), "  ");
        assert_eq!(table.translate("⠄⠈⠈"), "foo");
    }
}
