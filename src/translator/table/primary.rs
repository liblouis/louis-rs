use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io,
};

use hyphenation::{Hyphenator, Load, Standard};
use log::warn;

use crate::{
    Direction,
    emphasis::EmphasisSpan,
    parser::{
        AnchoredRule, Braille, BrailleChars, CharacterClass, CharacterClasses, HasNocross,
        HasPrecedence, WithClass as ParsedClass, fallback,
    },
    translator::{
        CharacterDefinition, ClassConstraint, ResolvedTranslation, Rule, TranslationError,
        TranslationOptions, TranslationStage,
        context_pattern::{ContextPatterns, ContextPatternsBuilder},
        effect::Environment,
        indication::{
            Indicator, Indicators, computer_braille, emphasis, lettersign, nocontract, numeric,
            uppercase,
        },
        match_pattern::{MatchPatterns, MatchPatternsBuilder},
        position_constraints::{
            BackwardCapsConstrainerBuilder, BackwardNumericConstrainerBuilder,
            ComputerBrailleConstrainer, Constrainer, Constrainers, NumericConstrainerBuilder,
        },
        table::TableContext,
        translation::TranslationSubset,
        trie::{Boundary, Transition, Trie},
    },
};

/// Detects compbrl-triggered computer braille regions and adds `computer_braille` spans.
///
/// When a table contains `compbrl` rules, any word in the input that contains one
/// of the trigger strings is treated as a computer braille span.
#[derive(Debug, Clone)]
struct CompbrlScanner {
    triggers: Vec<String>,
    character_classes: CharacterClasses,
}

impl CompbrlScanner {
    fn enrich(&self, input: &str, caller_spans: &[EmphasisSpan]) -> Vec<EmphasisSpan> {
        let chars: Vec<char> = input.chars().collect();
        let n = chars.len();
        let mut spans = caller_spans.to_vec();

        let mut pos = 0;
        while pos < n {
            // Check if any trigger matches at this character position.
            let remaining: String = chars[pos..].iter().collect();
            let matched = self
                .triggers
                .iter()
                .any(|t| remaining.starts_with(t.as_str()));
            if matched {
                // Expand to surrounding word: scan backward for whitespace.
                let mut word_start = pos;
                while word_start > 0 && !self.character_classes.is_whitespace(chars[word_start - 1])
                {
                    word_start -= 1;
                }
                // Scan forward for whitespace.
                let mut word_end = pos;
                while word_end < n && !self.character_classes.is_whitespace(chars[word_end]) {
                    word_end += 1;
                }
                let new_span = EmphasisSpan::new("computer_braille", word_start..word_end);
                // Avoid exact duplicates.
                if !spans.contains(&new_span) {
                    spans.push(new_span);
                }
                // Skip to end of word to avoid adding duplicate spans for the same word.
                pos = word_end;
            } else {
                pos += 1;
            }
        }

        spans
    }
}

#[derive(Debug)]
/// Character Translations map characters to a [`ResolvedTranslation`]. This cannot be done using
/// the [`Trie`] as the trie provides a case insensitive mapping only. The [`CharacterTranslation`]
/// on the other hand is case sensitive
struct CharacterTranslation(HashMap<char, ResolvedTranslation>);
impl CharacterTranslation {
    fn new() -> Self {
        Self(HashMap::new())
    }

    fn insert(&mut self, from: char, translation: ResolvedTranslation) {
        if cfg!(feature = "backwards_compatibility") {
            // first rule wins
            self.0.entry(from).or_insert(translation);
        } else {
            // last rule wins
            self.0.insert(from, translation);
        }
    }

    fn get(&self, from: &char) -> Option<&ResolvedTranslation> {
        self.0.get(from)
    }
}

#[derive(Debug)]
pub struct PrimaryTable {
    undefined: Option<String>,
    /// Fallback character definitions that are used as a last resort to translate unknown
    /// characters to their hex representation
    fallback_definitions: CharacterDefinition,
    /// A mapping of characters and their respective [`ResolvedTranslation`]
    character_translations: CharacterTranslation,
    /// A prefix tree that contains all the translation rules and their
    /// [`ResolvedTranslations`](ResolvedTranslation)
    trie: Trie,
    /// A prefix tree that contains comp6 (computer braille) translation rules.
    /// These are only consulted when `UseComp6` is set at a character position.
    comp6_trie: Trie,
    match_patterns: MatchPatterns,
    context_patterns: ContextPatterns,
    /// All the nocross translation rules are stored in a separate trie
    nocross_trie: Trie,
    hyphenator: Option<Standard>,
    indicators: Indicators,
    constrainers: Constrainers,
    /// Detects compbrl-triggered computer braille regions.
    compbrl_scanner: Option<CompbrlScanner>,
    direction: Direction,
    /// Backward-only: overrides the trie's default (letter) reading for a braille cell
    /// that is shared between a digit and a letter, keyed by the cell itself. Consulted
    /// only inside a `numsign` span (see [`Constraint::PreferDigit`]).
    ///
    /// [`Constraint::PreferDigit`]: crate::translator::position_constraints::Constraint::PreferDigit
    backward_digit_overrides: HashMap<char, ResolvedTranslation>,
}

/// A builder for [`PrimaryTable`]
#[derive(Debug)]
struct PrimaryTableBuilder {
    undefined: Option<String>,
    character_translations: CharacterTranslation,
    trie: Trie,
    comp6_trie: Trie,
    nocross_trie: Trie,
    hyphenator: Option<Standard>,
    match_patterns: MatchPatternsBuilder,
    context_patterns: ContextPatternsBuilder,
    numeric_indicator: numeric::IndicatorBuilder,
    uppercase_indicator: uppercase::IndicatorBuilder,
    lettersign_indicator: lettersign::IndicatorBuilder,
    nocontract_indicator: nocontract::IndicatorBuilder,
    emphasis_indicator: emphasis::IndicatorBuilder,
    computer_braille_indicator: computer_braille::IndicatorBuilder,
    numeric_constrainer: NumericConstrainerBuilder,
    backward_caps_constrainer: BackwardCapsConstrainerBuilder,
    backward_numeric_constrainer: BackwardNumericConstrainerBuilder,
    backward_digit_overrides: HashMap<char, ResolvedTranslation>,
    compbrl_triggers: Vec<String>,
}

impl PrimaryTableBuilder {
    fn new() -> Self {
        Self {
            undefined: None,
            character_translations: CharacterTranslation::new(),
            trie: Trie::new(),
            comp6_trie: Trie::new(),
            nocross_trie: Trie::new(),
            hyphenator: None,
            match_patterns: MatchPatternsBuilder::new(),
            context_patterns: ContextPatternsBuilder::new(),
            numeric_indicator: numeric::IndicatorBuilder::new(),
            uppercase_indicator: uppercase::IndicatorBuilder::new(),
            lettersign_indicator: lettersign::IndicatorBuilder::new(),
            nocontract_indicator: nocontract::IndicatorBuilder::new(),
            emphasis_indicator: emphasis::IndicatorBuilder::new(),
            computer_braille_indicator: computer_braille::IndicatorBuilder::new(),
            numeric_constrainer: NumericConstrainerBuilder::new(),
            backward_caps_constrainer: BackwardCapsConstrainerBuilder::new(),
            backward_numeric_constrainer: BackwardNumericConstrainerBuilder::new(),
            backward_digit_overrides: HashMap::new(),
            compbrl_triggers: Vec::new(),
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
        match direction {
            Direction::Forward => {
                // Forward translation is case sensitive, so we cannot solely rely on the trie which
                // is case insensitive. We also need to keep the rules in a case sensitive storage
                let translation = ResolvedTranslation::new(
                    c.to_string().as_str(),
                    dots,
                    1,
                    TranslationStage::Main,
                    rule.clone(),
                );
                self.character_translations.insert(c, translation);
            }
            Direction::Backward => {
                // Backward translation is not case sensitive, so we do not need a separate case
                // sensitive storage for translations. We can handle them like other rules via the
                // trie
                self.trie.insert_char(
                    c,
                    dots,
                    direction,
                    rule.precedence(),
                    TranslationStage::Main,
                    rule,
                )
            }
        }
    }

    /// Inserts a zero-output entry for a backward indicator opcode (`capsletter`,
    /// `begcapsword`, `numsign`, ...), so its dots are recognized in backward
    /// translation instead of falling through to a coincidentally-identical
    /// character definition. Only meaningful for `Direction::Backward`.
    fn insert_backward_indicator(&mut self, dots: &str, rule: &AnchoredRule) {
        self.trie.insert(
            "",
            dots,
            None,
            None,
            Direction::Backward,
            rule.precedence(),
            vec![],
            TranslationStage::Main,
            rule,
        );
    }

    /// Returns whether `opcode`'s `dots` are safe to register as a backward indicator:
    /// some tables include a shared subtable purely for other opcodes (e.g.
    /// Malayalam/Punjabi including `en-in-g1.ctb`), whose `capsletter`/`begcapsword`/
    /// `numsign` happen to coincide with one of the table's own script letters. Since
    /// the table itself doesn't actually intend a conflict — the collision is an
    /// accident of composing independently-authored subtables — this is reported as a
    /// warning (the opcode is ignored for backward translation, the letter wins) rather
    /// than a hard error, mirroring how a missing `letsign`/`nocontractionsign`
    /// counterpart is already handled.
    fn backward_indicator_dots_available(
        opcode: &str,
        dots: &str,
        letter_dots: &HashSet<String>,
    ) -> bool {
        if letter_dots.contains(dots) {
            warn!(
                "{opcode} dots {dots} collide with a letter definition in this table; \
                 ignoring {opcode} for backward translation"
            );
            false
        } else {
            true
        }
    }

    /// If `dots` is shared with a letter definition (common in six-dot literary codes,
    /// where digits 1-9,0 conventionally reuse letter cells a-j), records the digit
    /// reading as a backward override for that cell — consulted only inside a
    /// `numsign` span (see `Constraint::PreferDigit`). Only meaningful for
    /// `Direction::Backward`.
    ///
    /// A digit is always a single braille cell, so `dots` normally has length 1; this
    /// is skipped (rather than assumed) for any rule where that doesn't hold, mirroring
    /// `CharacterClasses::insert_dots`.
    fn register_backward_digit_override(
        &mut self,
        character: char,
        dots: &BrailleChars,
        letter_dots: &HashSet<String>,
        rule: &AnchoredRule,
    ) {
        let dots_str = dots.to_string();
        if dots.len() == 1 && letter_dots.contains(&dots_str) {
            let cell = dots.iter().next().unwrap().to_unicode();
            self.backward_digit_overrides.insert(
                cell,
                ResolvedTranslation::new(
                    &dots_str,
                    &character.to_string(),
                    1,
                    TranslationStage::Main,
                    rule.clone(),
                ),
            );
        }
    }

    fn build(self, direction: Direction, ctx: &TableContext) -> PrimaryTable {
        let triggers = self.compbrl_triggers;
        let indicators = [
            // Computer braille must be first so begcomp appears before emphasis.
            self.computer_braille_indicator
                .build()
                .map(Indicator::ComputerBraille),
            // Emphasis must be before capsletter so begemphword appears before capsletter.
            self.emphasis_indicator.build(ctx).map(Indicator::Emphasis),
            self.numeric_indicator.build().map(Indicator::Numeric),
            self.lettersign_indicator
                .build(ctx)
                .map(Indicator::LetterSign),
            self.uppercase_indicator
                .build(ctx)
                .map(Indicator::Uppercase),
            self.nocontract_indicator
                .build(ctx)
                .map(Indicator::NoContract),
        ]
        .into_iter()
        .flatten()
        .collect();
        let trie_ctx = match direction {
            Direction::Forward => ctx.character_classes.clone(),
            Direction::Backward => ctx.dots_classes().clone(),
        };
        PrimaryTable {
            undefined: self.undefined,
            character_translations: self.character_translations,
            direction,
            fallback_definitions: ctx.character_definitions().clone(),
            trie: self.trie.with_context(trie_ctx.clone()),
            comp6_trie: self.comp6_trie.with_context(trie_ctx.clone()),
            nocross_trie: self.nocross_trie.with_context(trie_ctx.clone()),
            hyphenator: self.hyphenator,
            match_patterns: self.match_patterns.build(),
            context_patterns: self.context_patterns.build(),
            indicators: Indicators::new(indicators),
            constrainers: Constrainers::new(
                [
                    Some(Constrainer::ComputerBraille(ComputerBrailleConstrainer {})),
                    self.numeric_constrainer.build().map(Constrainer::Numeric),
                    self.backward_caps_constrainer
                        .build()
                        .map(Constrainer::BackwardCaps),
                    self.backward_numeric_constrainer
                        .build()
                        .map(Constrainer::BackwardNumeric),
                ]
                .into_iter()
                .flatten()
                .collect(),
            ),
            compbrl_scanner: (!triggers.is_empty()).then(|| CompbrlScanner {
                triggers,
                character_classes: ctx.character_classes.clone(),
            }),
            backward_digit_overrides: self.backward_digit_overrides,
        }
    }
}

impl PrimaryTable {
    pub fn compile(
        rules: &[AnchoredRule],
        direction: Direction,
        _stage: TranslationStage,
        ctx: &TableContext,
    ) -> Result<Self, TranslationError> {
        let mut builder = PrimaryTableBuilder::new();

        // In literary braille tables, digit and litdigit serve distinct roles:
        //
        //   digit    — defines the "computer" dot pattern for a digit character, primarily
        //              used for *backward* translation (matching a dot pattern back to a digit).
        //              Some tables (e.g. digits6DotsPlusDot6.uti) add an extra dot so the
        //              pattern is unambiguous in backward translation.
        //
        //   litdigit — defines the *literary* dot pattern used in *forward* translation when
        //              the numeric indicator (numsign) is active.  Tables that target literary
        //              braille include a separate litdigit file alongside the digit file.
        //
        // When both are defined for the same character (a common pattern), litdigit must win
        // for forward translation.  We collect the set of characters that have a litdigit rule
        // so that we can skip the digit rule for those characters in the forward direction.
        let litdigit_chars: HashSet<char> = if direction == Direction::Forward {
            rules
                .iter()
                .filter_map(|r| {
                    if let Rule::Litdigit { character, .. } = &r.rule {
                        Some(*character)
                    } else {
                        None
                    }
                })
                .collect()
        } else {
            HashSet::new()
        };

        // Backward-only: digit dots can coincide with a letter's dots (six-dot literary
        // codes conventionally reuse letter cells a-j for digits 1-9,0). Collect the set
        // of dots used by letter definitions so digit/litdigit rules below can detect the
        // collision and register a numsign-scoped override (see
        // `register_backward_digit_override`), without disturbing the trie's normal
        // (letter) reading used outside a numeric run.
        let letter_dots: HashSet<String> = if direction == Direction::Backward {
            rules
                .iter()
                .filter_map(|r| match &r.rule {
                    Rule::Letter { dots, .. }
                    | Rule::Lowercase { dots, .. }
                    | Rule::Uppercase { dots, .. } => Some(dots.to_string()),
                    _ => None,
                })
                .collect()
        } else {
            HashSet::new()
        };

        // decpoint and hyphen take precedence over other character definition rules, so insert
        // them first
        for rule in rules {
            match &rule.rule {
                Rule::Decpoint {
                    character, dots, ..
                } => {
                    builder.insert_character(*character, &dots.to_string(), direction, rule);
                    // A decimal point extends the numeric run: digits that follow it
                    // must not get a second numsign.  Registering it as a midnum
                    // character achieves this via the numeric indicator and constrainer.
                    let s = character.to_string();
                    builder.numeric_indicator.midnum(&s);
                    builder.numeric_constrainer.midnum(&s);
                }
                Rule::Hyphen {
                    character, dots, ..
                } => {
                    builder.insert_character(*character, &dots.to_string(), direction, rule);
                }
                _ => (),
            }
        }
        for rule in rules {
            match &rule.rule {
                Rule::Undefined { dots } => {
                    builder.undefined = Some(dots.to_string());
                }
                Rule::Space {
                    character, dots, ..
                } => {
                    builder.insert_character(*character, &dots.to_string(), direction, rule);
                }
                Rule::Punctuation {
                    character, dots, ..
                } => {
                    builder.insert_character(*character, &dots.to_string(), direction, rule);
                }
                Rule::Digit {
                    character, dots, ..
                } => {
                    // Skip forward translation when a litdigit rule exists for this character;
                    // the litdigit rule is handled below and takes precedence.
                    if !litdigit_chars.contains(character) {
                        builder.insert_character(*character, &dots.to_string(), direction, rule);
                    }
                    if direction == Direction::Backward {
                        builder.register_backward_digit_override(
                            *character,
                            dots,
                            &letter_dots,
                            rule,
                        );
                    }
                }
                Rule::Litdigit {
                    character, dots, ..
                } => {
                    builder.insert_character(*character, &dots.to_string(), direction, rule);
                    if direction == Direction::Backward {
                        builder.register_backward_digit_override(
                            *character,
                            dots,
                            &letter_dots,
                            rule,
                        );
                    }
                }
                Rule::Letter {
                    character, dots, ..
                } => {
                    builder.insert_character(*character, &dots.to_string(), direction, rule);
                }
                Rule::Lowercase {
                    character, dots, ..
                } => {
                    builder.insert_character(*character, &dots.to_string(), direction, rule);
                }
                Rule::Uppercase {
                    character, dots, ..
                } => {
                    builder.insert_character(*character, &dots.to_string(), direction, rule);
                }
                Rule::Sign {
                    character, dots, ..
                } => {
                    builder.insert_character(*character, &dots.to_string(), direction, rule);
                }
                Rule::Math {
                    character, dots, ..
                } => {
                    builder.insert_character(*character, &dots.to_string(), direction, rule);
                }
                Rule::Numsign { dots } => {
                    builder.numeric_indicator.numsign(&dots.to_string(), rule);
                    // See `backward_indicator_dots_available` for why this can be skipped.
                    if direction == Direction::Backward
                        && PrimaryTableBuilder::backward_indicator_dots_available(
                            "numsign",
                            &dots.to_string(),
                            &letter_dots,
                        )
                    {
                        builder.insert_backward_indicator(&dots.to_string(), rule);
                        builder
                            .backward_numeric_constrainer
                            .numsign(&dots.to_string());
                    }
                }
                Rule::Nonumsign { dots } => {
                    builder.numeric_indicator.nonumsign(&dots.to_string(), rule);
                    if direction == Direction::Backward
                        && PrimaryTableBuilder::backward_indicator_dots_available(
                            "nonumsign",
                            &dots.to_string(),
                            &letter_dots,
                        )
                    {
                        builder.insert_backward_indicator(&dots.to_string(), rule);
                        builder
                            .backward_numeric_constrainer
                            .nonumsign(&dots.to_string());
                    }
                }
                Rule::Numericnocontchars { chars } => {
                    builder.numeric_indicator.numericnocontchars(chars);
                }
                Rule::Numericmodechars { chars } => {
                    builder.numeric_indicator.numericmodechars(chars);
                    builder.numeric_constrainer.numericmodechars(chars);
                }
                Rule::Capsletter { dots, .. } => {
                    builder
                        .uppercase_indicator
                        .capsletter(&dots.to_string(), rule);
                    // See `backward_indicator_dots_available` for why this can be skipped.
                    if direction == Direction::Backward
                        && PrimaryTableBuilder::backward_indicator_dots_available(
                            "capsletter",
                            &dots.to_string(),
                            &letter_dots,
                        )
                    {
                        builder.insert_backward_indicator(&dots.to_string(), rule);
                        builder
                            .backward_caps_constrainer
                            .capsletter(&dots.to_string());
                    }
                }
                Rule::Begcapsword { dots, .. } => {
                    builder
                        .uppercase_indicator
                        .begcapsword(&dots.to_string(), rule);
                    if direction == Direction::Backward
                        && PrimaryTableBuilder::backward_indicator_dots_available(
                            "begcapsword",
                            &dots.to_string(),
                            &letter_dots,
                        )
                    {
                        builder.insert_backward_indicator(&dots.to_string(), rule);
                        builder
                            .backward_caps_constrainer
                            .begcapsword(&dots.to_string());
                    }
                }
                Rule::Endcapsword { dots, .. } => {
                    builder
                        .uppercase_indicator
                        .endcapsword(&dots.to_string(), rule);
                    if direction == Direction::Backward
                        && PrimaryTableBuilder::backward_indicator_dots_available(
                            "endcapsword",
                            &dots.to_string(),
                            &letter_dots,
                        )
                    {
                        builder.insert_backward_indicator(&dots.to_string(), rule);
                        builder
                            .backward_caps_constrainer
                            .endcapsword(&dots.to_string());
                    }
                }
                Rule::Begcaps { dots } => {
                    builder.uppercase_indicator.begcaps(&dots.to_string(), rule);
                }
                Rule::Endcaps { dots } => {
                    builder.uppercase_indicator.endcaps(&dots.to_string(), rule);
                }
                Rule::Capsmodechars { chars } => {
                    builder.uppercase_indicator.capsmodechars(chars);
                }
                Rule::Begcapsphrase { dots } => {
                    builder
                        .uppercase_indicator
                        .begcapsphrase(&dots.to_string(), rule);
                }
                Rule::Endcapsphrase { dots, position } => {
                    builder
                        .uppercase_indicator
                        .endcapsphrase(&dots.to_string(), position, rule);
                }
                Rule::Lencapsphrase { number } => {
                    builder.uppercase_indicator.lencapsphrase(*number as usize);
                }
                Rule::Emphclass { name } => {
                    builder.emphasis_indicator.emphclass(name);
                }
                Rule::Emphletter { name, dots } => {
                    builder
                        .emphasis_indicator
                        .emphletter(name, &dots.to_string(), rule);
                }
                Rule::Begemphword { name, dots } => {
                    builder
                        .emphasis_indicator
                        .begemphword(name, &dots.to_string(), rule);
                }
                Rule::Endemphword { name, dots } => {
                    builder
                        .emphasis_indicator
                        .endemphword(name, &dots.to_string(), rule);
                }
                Rule::Begemphphrase { name, dots } => {
                    builder
                        .emphasis_indicator
                        .begemphphrase(name, &dots.to_string(), rule);
                }
                Rule::Endemphphrase {
                    name,
                    dots,
                    position,
                } => {
                    builder.emphasis_indicator.endemphphrase(
                        name,
                        &dots.to_string(),
                        position,
                        rule,
                    );
                }
                Rule::Lenemphphrase { name, number } => {
                    builder
                        .emphasis_indicator
                        .lenemphphrase(name, *number as usize);
                }
                Rule::Begemph { name, dots, .. } => {
                    builder
                        .emphasis_indicator
                        .begemph(name, &dots.to_string(), rule);
                }
                Rule::Endemph { name, dots, .. } => {
                    builder
                        .emphasis_indicator
                        .endemph(name, &dots.to_string(), rule);
                }
                Rule::Emphmodechars { name, chars } => {
                    builder.emphasis_indicator.emphmodechars(name, chars);
                }
                Rule::Noemphchars { name, chars } => {
                    builder.emphasis_indicator.noemphchars(name, chars);
                }
                Rule::Letsign { dots } => {
                    builder
                        .lettersign_indicator
                        .letsign(&dots.to_string(), rule);
                }
                Rule::Noletsign { chars } => {
                    builder.lettersign_indicator.noletsign(chars);
                }
                Rule::Noletsignafter { chars } => {
                    builder.lettersign_indicator.noletsignafter(chars);
                }
                Rule::Noletsignbefore { chars } => {
                    builder.lettersign_indicator.noletsignbefore(chars);
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
                        Some(Transition::Start(Boundary::Word)),
                        Some(Transition::End(Boundary::Word)),
                        direction,
                        rule.precedence(),
                        vec![],
                        TranslationStage::Main,
                        rule,
                    );
                    builder.lettersign_indicator.contraction(chars, rule);
                    builder.nocontract_indicator.contraction(chars, rule);
                }
                Rule::Nocontractsign { dots } => {
                    builder
                        .nocontract_indicator
                        .nocontractsign(&dots.to_string(), rule);
                }
                Rule::Base { derived, base, .. } => {
                    if let Some(translation) = ctx.character_definitions().get(base).cloned() {
                        builder.insert_character(*derived, &translation, direction, rule);
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
                Rule::Comp6 { chars, dots } => {
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    builder.comp6_trie.insert(
                        chars,
                        &dots,
                        None,
                        None,
                        direction,
                        rule.precedence(),
                        vec![],
                        TranslationStage::Main,
                        rule,
                    );
                }
                Rule::Always {
                    chars,
                    dots,
                    with_classes,
                    ..
                } => {
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    let class_constraints: Vec<ClassConstraint> = with_classes
                        .iter()
                        .filter_map(|wc| match wc {
                            // "before CLASS" keyword → lookahead: char after match must be in class
                            ParsedClass::Before { class } => {
                                let key = CharacterClass::from(class.as_str());
                                ctx.character_classes.get(&key)?;
                                Some(ClassConstraint::End(key))
                            }
                            // "after CLASS" keyword → lookbehind: char before match must be in class
                            ParsedClass::After { class } => {
                                let key = CharacterClass::from(class.as_str());
                                ctx.character_classes.get(&key)?;
                                Some(ClassConstraint::Start(key))
                            }
                        })
                        .collect();
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        None,
                        None,
                        direction,
                        rule.precedence(),
                        class_constraints,
                        TranslationStage::Main,
                        rule,
                    );
                }
                Rule::Begcomp { dots, .. } => {
                    builder
                        .computer_braille_indicator
                        .begcomp(&dots.to_string(), rule);
                }
                Rule::Endcomp { dots, .. } => {
                    builder
                        .computer_braille_indicator
                        .endcomp(&dots.to_string(), rule);
                }
                Rule::Compbrl { chars, .. } => {
                    builder.compbrl_triggers.push(chars.clone());
                }
                Rule::Largesign { chars, dots } => {
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots.to_string(),
                        None,
                        None,
                        direction,
                        rule.precedence(),
                        vec![],
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
                        Some(Transition::Start(Boundary::Word)),
                        Some(Transition::End(Boundary::Word)),
                        direction,
                        rule.precedence(),
                        vec![],
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
                        Some(Transition::Start(Boundary::Word)),
                        Some(Transition::End(Boundary::NotWord)),
                        direction,
                        rule.precedence(),
                        vec![],
                        TranslationStage::Main,
                        rule,
                    )
                }
                Rule::Midword { chars, dots, .. } => {
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Some(Transition::Start(Boundary::NotWord)),
                        Some(Transition::End(Boundary::NotWord)),
                        direction,
                        rule.precedence(),
                        vec![],
                        TranslationStage::Main,
                        rule,
                    )
                }
                Rule::Partword { chars, dots, .. } => {
                    // partword fires when preceded OR followed by a letter (i.e. not standalone).
                    // Two insertions cover all non-standalone positions:
                    // 1. preceded by word char (midword + endword)
                    // 2. at word-start followed by word char (begword)
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Some(Transition::Start(Boundary::NotWord)),
                        None,
                        direction,
                        rule.precedence(),
                        vec![],
                        TranslationStage::Main,
                        rule,
                    );
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Some(Transition::Start(Boundary::Word)),
                        Some(Transition::End(Boundary::NotWord)),
                        direction,
                        rule.precedence(),
                        vec![],
                        TranslationStage::Main,
                        rule,
                    );
                }
                Rule::Midendword { chars, dots, .. } => {
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Some(Transition::Start(Boundary::NotWord)),
                        None,
                        direction,
                        rule.precedence(),
                        vec![],
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
                        None,
                        Some(Transition::End(Boundary::Word)),
                        direction,
                        rule.precedence(),
                        vec![],
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
                        Some(Transition::Start(Boundary::Word)),
                        Some(Transition::End(Boundary::Word)),
                        direction,
                        rule.precedence(),
                        vec![],
                        TranslationStage::Main,
                        rule,
                    );
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        None,
                        Some(Transition::End(Boundary::Word)),
                        direction,
                        rule.precedence(),
                        vec![],
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
                        Some(Transition::Start(Boundary::Word)),
                        Some(Transition::End(Boundary::Word)),
                        direction,
                        rule.precedence(),
                        vec![],
                        TranslationStage::Main,
                        rule,
                    );
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Some(Transition::Start(Boundary::Word)),
                        None,
                        direction,
                        rule.precedence(),
                        vec![],
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
                        None,
                        Some(Transition::End(Boundary::NotWord)),
                        direction,
                        rule.precedence(),
                        vec![],
                        TranslationStage::Main,
                        rule,
                    );
                }
                Rule::Joinword { chars, dots, .. } | Rule::Lowword { chars, dots, .. } => {
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots.to_string(),
                        Some(Transition::Start(Boundary::Word)),
                        Some(Transition::End(Boundary::Word)),
                        direction,
                        rule.precedence(),
                        vec![],
                        TranslationStage::Main,
                        rule,
                    )
                }
                Rule::Begnum { chars, dots, .. } => builder.get_trie_mut(rule).insert(
                    chars,
                    &dots.to_string(),
                    Some(Transition::Start(Boundary::AfterSpaceOrPunct)),
                    Some(Transition::Start(Boundary::Number)),
                    direction,
                    rule.precedence(),
                    vec![],
                    TranslationStage::Main,
                    rule,
                ),
                Rule::Midnum { chars, dots, .. } => {
                    builder.numeric_indicator.midnum(chars);
                    builder.numeric_constrainer.midnum(chars);
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots.to_string(),
                        Some(Transition::End(Boundary::Number)),
                        Some(Transition::Start(Boundary::Number)),
                        direction,
                        rule.precedence(),
                        vec![],
                        TranslationStage::Main,
                        rule,
                    );
                }
                Rule::Endnum { chars, dots, .. } => {
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Some(Transition::End(Boundary::Number)),
                        None,
                        direction,
                        rule.precedence(),
                        vec![],
                        TranslationStage::Main,
                        rule,
                    );
                }
                Rule::Prepunc { chars, dots, .. } => {
                    // are all the characters punctuation?
                    if chars.chars().all(|c| {
                        ctx.character_classes
                            .get(&CharacterClass::Punctuation)
                            .is_some_and(|class| class.contains(&c))
                    }) {
                        builder.get_trie_mut(rule).insert(
                            chars,
                            &dots.to_string(),
                            Some(Transition::Start(Boundary::Punctuation)),
                            Some(Transition::End(Boundary::PunctuationWord)),
                            direction,
                            rule.precedence(),
                            vec![],
                            TranslationStage::Main,
                            rule,
                        );
                    }
                }
                Rule::Postpunc { chars, dots, .. } => {
                    // are all the characters punctuation?
                    if chars.chars().all(|c| {
                        ctx.character_classes
                            .get(&CharacterClass::Punctuation)
                            .is_some_and(|class| class.contains(&c))
                    }) {
                        builder.get_trie_mut(rule).insert(
                            chars,
                            &dots.to_string(),
                            Some(Transition::Start(Boundary::WordPunctuation)),
                            Some(Transition::End(Boundary::Punctuation)),
                            direction,
                            rule.precedence(),
                            vec![],
                            TranslationStage::Main,
                            rule,
                        );
                    }
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
                Rule::Context { test, action, .. } => {
                    builder.context_patterns.insert(
                        test,
                        action,
                        rule,
                        TranslationStage::Main,
                        ctx,
                    )?;
                }
                Rule::IncludeHyphenation { path } => {
                    let file = File::open(path)?;
                    let mut reader = io::BufReader::new(file);
                    builder.hyphenator = Some(Standard::any_from_reader(&mut reader)?);
                }
                _ => (),
            }
        }
        let numeric_chars = ctx
            .character_classes()
            .get(&CharacterClass::Litdigit)
            .unwrap_or_default();
        builder
            .numeric_indicator
            .numeric_characters(numeric_chars.clone());
        builder
            .numeric_constrainer
            .numeric_characters(numeric_chars);
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
        if direction == Direction::Backward {
            let mut letter_cells = ctx
                .dots_classes()
                .get(&CharacterClass::Letter)
                .unwrap_or_default();
            // Multi-letter `always` rules (e.g. Hungarian "sz"/"cs" digraphs) define
            // their own dots but aren't registered in the Letter dots class; a
            // begcapsword span must still continue across them.
            for rule in rules {
                if let Rule::Always { chars, dots, .. } = &rule.rule
                    && chars.chars().all(|c| c.is_alphabetic())
                {
                    letter_cells.extend(dots.to_string().chars());
                }
            }
            builder
                .backward_numeric_constrainer
                .letter_cells(letter_cells.clone());
            builder.backward_caps_constrainer.letter_cells(letter_cells);
            let mut digit_cells = ctx
                .dots_classes()
                .get(&CharacterClass::Digit)
                .unwrap_or_default();
            digit_cells.extend(
                ctx.dots_classes()
                    .get(&CharacterClass::Litdigit)
                    .unwrap_or_default(),
            );
            builder
                .backward_numeric_constrainer
                .digit_cells(digit_cells);
        }
        Ok(builder.build(direction, ctx))
    }

    fn update_offsets(
        &self,
        translations: Vec<ResolvedTranslation>,
        decrement: usize,
    ) -> Vec<ResolvedTranslation> {
        translations
            .into_iter()
            // drop translations where the offet is smaller than the decrement
            .filter(|t| t.offset() >= decrement)
            .map(|t| t.decrement_offset(decrement))
            .collect()
    }

    pub fn translate(&self, input: &str) -> String {
        self.trace(input, &TranslationOptions::default())
            .iter()
            .map(|t| t.output())
            .collect()
    }

    pub fn translate_with_options(&self, input: &str, options: &TranslationOptions) -> String {
        self.trace(input, options)
            .iter()
            .map(|t| t.output())
            .collect()
    }

    fn translation_candidates(
        &self,
        input: &str,
        prev: Option<char>,
    ) -> (Vec<ResolvedTranslation>, Vec<ResolvedTranslation>) {
        self.trie
            .find_translations(input, prev)
            .into_iter()
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
        at_start: bool,
    ) -> (Vec<ResolvedTranslation>, Vec<ResolvedTranslation>) {
        self.match_patterns
            .find(input, at_start)
            .into_iter()
            .partition(|t| t.offset() == 0)
    }

    fn context_candidates(
        &self,
        input: &str,
        env: &Environment,
        at_start: bool,
    ) -> (Vec<ResolvedTranslation>, Vec<ResolvedTranslation>) {
        self.context_patterns
            .find(input, env, at_start)
            .into_iter()
            .partition(|t| t.offset() == 0)
    }

    fn partition_delayed_translations(
        &self,
        delayed: Vec<ResolvedTranslation>,
    ) -> (Vec<ResolvedTranslation>, Vec<ResolvedTranslation>) {
        delayed.into_iter().partition(|t| t.offset() == 0)
    }

    pub fn trace(&self, input: &str, options: &TranslationOptions) -> Vec<ResolvedTranslation> {
        let mut translations: Vec<ResolvedTranslation> = Vec::new();
        let mut delayed_translations: Vec<ResolvedTranslation> = Vec::new();

        let mut env = Environment::new();
        let mut chars = input.chars();
        let mut prev: Option<char> = None;
        let mut seen: HashSet<TranslationSubset> = HashSet::default();
        let mut char_pos: usize = 0;

        // Enrich spans with compbrl-detected regions.
        let enriched_spans = match &self.compbrl_scanner {
            Some(scanner) => scanner.enrich(input, options.emphasis()),
            None => options.emphasis().to_vec(),
        };

        let indications = self.indicators.precompute(input, &enriched_spans);
        let constraints = self.constrainers.precompute(input, &enriched_spans);

        loop {
            translations.extend(indications.translations_at(char_pos));

            // whether we're about to match starting at the true beginning of the whole input,
            // needed by match/context rules anchored with liblouis' `^`/`` ` `` boundary markers
            let at_start = chars.as_str().len() == input.len();

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
            let (match_candidates, match_delayed) = self.match_candidates(chars.as_str(), at_start);
            delayed_translations.extend(match_delayed);
            // merge the candidates from the match patters with the candidates from the plain translations
            candidates.extend(match_candidates);

            // then search for context patterns. Unless they have empty pre patterns they will all
            // have an offset. Split those off.
            let (context_candidates, context_delayed) =
                self.context_candidates(chars.as_str(), &env, at_start);
            delayed_translations.extend(context_delayed);
            // merge the candidates from the context patters with the candidates from the plain translations
            candidates.extend(context_candidates);

            // move delayed_translations with zero offset into candidates
            let (current, delayed) = self.partition_delayed_translations(delayed_translations);
            delayed_translations = delayed;
            candidates.extend(current);

            // Backward numeric mode: prefer the digit reading of a cell shared with a
            // letter (common in six-dot literary codes). Rather than out-weighing the
            // trie's single-cell letter candidate, replace it outright — any genuinely
            // longer (multi-cell) candidate at this position is left alone to compete
            // on its own weight, same as usual.
            if self.direction == Direction::Backward
                && constraints.prefer_digit_at(char_pos)
                && let Some(next_char) = chars.clone().next()
                && let Some(digit_translation) = self.backward_digit_overrides.get(&next_char)
            {
                candidates.retain(|t| t.length() != 1);
                candidates.push(digit_translation.clone());
            }

            // inside a numeric run, suppress multi-character contractions
            if constraints.dont_contract_at(char_pos) {
                candidates.retain(|t| t.length() <= 1);
            }

            // In computer braille mode, discard normal candidates and use comp6 rules only.
            // comp6 rules have no pre-pattern so offset() is always 0 — no delayed handling needed.
            if constraints.use_comp6_at(char_pos) {
                candidates.clear();
                candidates.extend(self.comp6_trie.find_translations(chars.as_str(), prev));
            }

            // Backward caps mode: capitalize the output of whichever candidate wins below.
            // `Uppercase` (begcapsword span) capitalizes every character a cell resolves
            // to; `TitleCase` (capsletter) capitalizes only the first — a cell can
            // resolve to more than one character (e.g. a digraph rule), and capsletter
            // marks a single capitalized letter, not a whole capitalized word.
            let is_backward = self.direction == Direction::Backward;
            let uppercase_here = is_backward && constraints.uppercase_at(char_pos);
            let titlecase_here = is_backward && constraints.titlecase_at(char_pos);
            let uppercase_if_needed =
                move |translation: ResolvedTranslation| -> ResolvedTranslation {
                    if uppercase_here {
                        let upper = translation.output().to_uppercase();
                        translation.with_output(&upper)
                    } else if titlecase_here {
                        let output = translation.output();
                        let mut chars = output.chars();
                        let titled = match chars.next() {
                            Some(c) => c.to_uppercase().chain(chars).collect::<String>(),
                            None => String::new(),
                        };
                        translation.with_output(&titled)
                    } else {
                        translation
                    }
                };

            // use the longest translation
            let candidate = candidates
                .iter()
                // drop translation candidates that we have applied already at this position in the
                // input
                .filter(|t| !seen.contains(&TranslationSubset::from(*t)))
                .max_by_key(|translation| translation.weight());
            // A nocross rule wins if there is no plain candidate to beat (many, e.g.
            // liblouis' `nocross always en`, have no plain counterpart at all).
            if let Some(nocross) = &nocross_candidate
                && candidate.is_none_or(|t| nocross.weight() >= t.weight())
            {
                let translation = nocross.clone();
                let translation = uppercase_if_needed(translation);
                // move the iterator forward by the number of characters in the translation
                chars.nth(nocross.length() - 1);
                prev = translation.input().chars().last();
                char_pos += nocross.length();
                translations.push(translation);
                delayed_translations = self.update_offsets(delayed_translations, nocross.length());
            } else if let Some(t) = candidate {
                if t.length() == 0 {
                    // if there is a zero-length translation candiate we run the risk of an infinite
                    // loop, so remember the current translation so we only apply it once
                    seen.insert(TranslationSubset::from(t));
                } else {
                    seen.clear();
                    // move the iterator forward by the number of characters in the translation
                    chars.nth(t.length() - 1);
                    prev = t.input().chars().last();
                    char_pos += t.length();
                }
                // there is a matching translation rule
                let translation = t.clone();
                let translation = uppercase_if_needed(translation);
                translations.push(translation);
                delayed_translations = self.update_offsets(delayed_translations, t.length());

                // update the environment if needed
                if !t.effects().is_empty() {
                    for effect in t.effects() {
                        env.apply(effect);
                    }
                }
            } else if let Some(next_char) = chars.next() {
                prev = Some(next_char);
                char_pos += 1;
                // no translation rule found
                if let Some(translation) = self.character_translations.get(&next_char) {
                    translations.push(translation.clone());
                    delayed_translations = self.update_offsets(delayed_translations, 1);
                } else if let Some(ref replacement) = self.undefined {
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
                // the chars iterator is exhausted — slot `n` translations were
                // already emitted by the translations_at call at the top of this
                // iteration, so just break here.
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
                if let Some(t) = self.fallback_definitions.get(&c) {
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

    use search_path::SearchPath;

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
        // partword fires when either side is a letter (not standalone)
        assert_eq!(table.translate("bar"), "⠂⠁⠐"); // standalone: NOT contracted
        assert_eq!(table.translate("foobar"), "⠉⠑"); // word-end (prev=letter): contracted
        assert_eq!(table.translate("foobar."), "⠉⠑⠠"); // word-end (prev=letter): contracted
        assert_eq!(table.translate("foobarfoo"), "⠉⠑⠉"); // midword: contracted
        assert_eq!(table.translate("foobar foo"), "⠉⠑⠀⠉"); // word-end: contracted
        assert_eq!(table.translate("foobar. foo"), "⠉⠑⠠⠀⠉"); // word-end: contracted
        assert_eq!(table.translate("foo bar foo"), "⠉⠀⠂⠁⠐⠀⠉"); // standalone: NOT contracted
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
            parse_rule("lowercase a 3456"),
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
    fn begnum_punctuation_indicator() {
        // '#' is punctuation, not a word char. The old Start(Word) boundary would have
        // missed it; Start(AfterSpaceOrPunct) correctly fires when preceded by
        // punctuation or at start-of-string.
        let rules = [
            parse_rule("digit 1 1"),
            parse_rule("punctuation # 3456"),
            parse_rule("punctuation . 256"),
            parse_rule("lowercase a 14"),
            parse_rule("begnum # 4"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        // fires at start of string (prev = None)
        assert_eq!(table.translate("#1"), "⠈⠁");
        // fires when preceded by punctuation
        assert_eq!(table.translate(".#1"), "⠲⠈⠁");
        // does NOT fire when preceded by a letter
        assert_eq!(table.translate("a#1"), "⠉⠼⠁");
        // does NOT fire when not followed by a digit
        assert_eq!(table.translate("#a"), "⠼⠉");
    }

    #[test]
    fn endnum_punctuation_indicator() {
        // '#' is punctuation. End(Number) only checks that the preceding char is a
        // digit; there is no constraint on what follows (unlike the old End(Word)).
        let rules = [
            parse_rule("digit 1 1"),
            parse_rule("lowercase a 14"),
            parse_rule("punctuation # 3456"),
            parse_rule("endnum # 4"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        // fires when preceded by a digit
        assert_eq!(table.translate("1#"), "⠁⠈");
        // does NOT fire when preceded by a letter
        assert_eq!(table.translate("a#"), "⠉⠼");
        // does NOT fire at start of string
        assert_eq!(table.translate("#"), "⠼");
        // no constraint on what follows the indicator
        assert_eq!(table.translate("1#1"), "⠁⠈⠁");
        assert_eq!(table.translate("1#a"), "⠁⠈⠉");
    }

    #[test]
    fn prepunc_postpunc() {
        let rules = [
            parse_rule("lowercase f 3"),
            parse_rule("lowercase o 4"),
            parse_rule("punctuation ( 23678"),
            parse_rule("prepunc     ( 2356"),
            parse_rule("punctuation ) 35678"),
            parse_rule("postpunc    ) 2356"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate(")"), "⣴");
        assert_eq!(table.translate("("), "⣦");
        assert_eq!(table.translate("()"), "⣦⣴");
        assert_eq!(table.translate("foo)"), "⠄⠈⠈⠶");
        assert_eq!(table.translate("(foo"), "⠶⠄⠈⠈");
        assert_eq!(table.translate("(foo)"), "⠶⠄⠈⠈⠶");
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
    fn nocross() {
        let rules = vec![
            parse_rule("include dictionaries/de-g2-core-patterns.dic"),
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
        let rules = expand_includes(rules, &SearchPath::new_or("LOUIS_TABLE_PATH", ".")).unwrap();
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("hausboot"), "⠇");
        assert_eq!(table.translate("fff"), "⠸");
    }

    #[test]
    fn nocross_without_plain_counterpart() {
        // liblouis tables routinely define a `nocross` rule with no plain counterpart
        // (e.g. da-dk-g28.ctb's `nocross always en`) — it must still win when it's the
        // only candidate at a position, not just when it out-weighs a competing plain rule.
        let rules = vec![
            parse_rule("include dictionaries/de-g2-core-patterns.dic"),
            parse_rule("lowercase f 124"),
            parse_rule("nocross always fff 456"),
            parse_rule("space \\s 0"),
        ];
        let rules = expand_includes(rules, &SearchPath::new_or("LOUIS_TABLE_PATH", ".")).unwrap();
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
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

    #[test]
    fn context_with_variable() {
        let rules = [
            parse_rule("lowercase a 1"),
            parse_rule("sign : 123"),
            parse_rule("sign ; 456"),
            parse_rule("context \":\" @123#1=1"),
            parse_rule("context #1=1\"a\" @3#1=1"),
            parse_rule("context \";\" @456#1=0"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("aa"), "⠁⠁");
        assert_eq!(table.translate("aa;aa"), "⠁⠁⠸⠁⠁");
        assert_eq!(table.translate("aa:aa;aa"), "⠁⠁⠇⠄⠄⠸⠁⠁");
    }

    #[test]
    fn always_with_uppercase() {
        let rules = [
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 12"),
            parse_rule("lowercase c 14"),
            parse_rule("base uppercase A a"),
            parse_rule("base uppercase B b"),
            parse_rule("base uppercase C c"),
            parse_rule("capsletter 46"),
            parse_rule("begcapsword 6-6"),
            parse_rule("always abc 78"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("abc"), "⣀");
        assert_eq!(table.translate("Abc"), "⠨⣀");
        assert_eq!(table.translate("ABC"), "⠠⠠⣀");
    }

    #[test]
    fn backward_capsletter_beats_colliding_punctuation_and_titlecases_next_letter() {
        // `capsletter` and `punctuation $` share dots 46 here, mirroring hu-hu-g1.ctb's
        // real collision (GitHub issue #4). capsletter must win and title-case the
        // following letter, not fall through to the punctuation definition.
        let rules = [
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 12"),
            parse_rule("capsletter 46"),
            parse_rule("punctuation $ 46"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table = PrimaryTable::compile(
            &rules,
            Direction::Backward,
            TranslationStage::Main,
            &context,
        )
        .unwrap();
        assert_eq!(table.translate("⠨⠃"), "B");
    }

    #[test]
    fn backward_numsign_prefers_digit_reading_of_shared_dots() {
        // `litdigit 1` shares dots with `lowercase a` (as in six-dot literary codes,
        // where digits conventionally reuse letter cells). Inside a numsign span the
        // digit reading must win; outside it, the letter reading is unaffected.
        let rules = [
            parse_rule("lowercase a 1"),
            parse_rule("litdigit 1 1"),
            parse_rule("numsign 3456"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table = PrimaryTable::compile(
            &rules,
            Direction::Backward,
            TranslationStage::Main,
            &context,
        )
        .unwrap();
        assert_eq!(table.translate("⠼⠁"), "1");
        assert_eq!(table.translate("⠁"), "a");
    }

    #[test]
    fn backward_begcapsword_uppercases_whole_word() {
        let rules = [
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 12"),
            parse_rule("begcapsword 46-46"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table = PrimaryTable::compile(
            &rules,
            Direction::Backward,
            TranslationStage::Main,
            &context,
        )
        .unwrap();
        assert_eq!(table.translate("⠨⠨⠁⠃"), "AB");
    }

    #[test]
    fn backward_capsletter_does_not_shadow_a_real_letter_sharing_its_dots() {
        // Regression: some tables (e.g. Malayalam, Punjabi) include a shared subtable
        // (en-in-g1.ctb) purely for other opcodes, whose capsletter/numsign happen to
        // share dots with one of the table's own script letters (Malayalam "ഃ" and
        // capsletter both use dots 6). Unlike the `$`/capsletter collision, the letter
        // must win here — capsletter should not be recognized as an indicator at all
        // for dots already claimed by a real letter.
        let rules = [
            parse_rule("lowercase a 1"),
            parse_rule("letter b 6"),
            parse_rule("capsletter 6"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table = PrimaryTable::compile(
            &rules,
            Direction::Backward,
            TranslationStage::Main,
            &context,
        )
        .unwrap();
        assert_eq!(table.translate("⠠⠁"), "ba");
    }
}
