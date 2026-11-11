use std::collections::{HashMap, HashSet};

use log::warn;

use crate::{
    Direction,
    emphasis::EmphasisSpan,
    parser::{
        AnchoredRule, Braille, BrailleChar, BrailleChars, CharacterClass, CharacterClasses,
        HasDirection, HasNocross, WithClass, in_definition_order,
    },
    translator::{
        CharacterDefinition, ClassConstraints, ResolvedTranslation, Rule, TranslationError,
        TranslationMode, TranslationOptions, TranslationStage,
        beg_word::BegWordScanner,
        context_pattern::{ContextPatterns, ContextPatternsBuilder},
        effect::Environment,
        end_word::EndWordScanner,
        indication::{
            Indicator, Indicators, PrecomputedIndications, computer_braille, emphasis, lettersign,
            mode, nocontract, numeric,
            uppercase::{self, BackwardCapsWord, BackwardCapsWordBuilder},
        },
        match_pattern::{MatchPatterns, MatchPatternsBuilder},
        position_constraints::{
            BackwardCapsConstrainerBuilder, BackwardNoContractConstrainerBuilder,
            BackwardNumericConstrainerBuilder, CapsNoContConstrainerBuilder,
            ComputerBrailleConstrainer, Constrainer, Constrainers, Constraint,
            HyphenationConstrainerBuilder, JoinConstrainerBuilder, NoContConstrainerBuilder,
            NumericConstrainerBuilder, PositionConstraints, PunctuationConstrainerBuilder,
            RepwordConstrainerBuilder, SyllableConstrainerBuilder,
        },
        standing_alone::StandingAloneScanner,
        table::TableContext,
        translation::{TranslationSubset, TranslationTarget},
        trie::{Transition, Trie},
    },
};

/// Spell an undefined character as the hex escape liblouis prints for it
///
/// The counterpart of [`unknown_dots`], for the forward direction. Every character
/// has one, so this cannot fail. The result is still text: the caller renders it
/// into braille, since that is the direction's output.
fn unknown_hex(ch: char) -> String {
    ch.escape_unicode()
        .to_string()
        .replace(r"\u", r"\x") // replace \u by \x
        .replace(['{', '}'], "") // drop the curly braces
}

/// Spell an undefined braille cell as the dot numbers liblouis prints for it
///
/// The counterpart of [`unknown_hex`], for the backward direction. Back-translation
/// writes text, so an undefined cell cannot be escaped as braille. liblouis spells out
/// the dot numbers between a backslash and a slash, and uses `\\0/` for a cell with no
/// dots. Returns `None` for a character that is not a braille cell.
fn unknown_dots(ch: char) -> Option<String> {
    BrailleChar::from_unicode(ch).map(|cell| format!("\\{}/", cell.dot_numbers()))
}

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
    /// characters to their hex representation. Also lent to the `context` rules, whose `*`
    /// translates the characters between `[` and `]` by them
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
    /// Backward-only: capitalizes the words a `begcapsword` starts, once translated.
    backward_caps_word: Option<BackwardCapsWord>,
}

/// A builder for [`PrimaryTable`]
#[derive(Debug)]
struct PrimaryTableBuilder {
    undefined: Option<String>,
    character_translations: CharacterTranslation,
    trie: Trie,
    comp6_trie: Trie,
    nocross_trie: Trie,
    match_patterns: MatchPatternsBuilder,
    context_patterns: ContextPatternsBuilder,
    numeric_indicator: numeric::IndicatorBuilder,
    uppercase_indicator: uppercase::IndicatorBuilder,
    mode_indicator: mode::IndicatorBuilder,
    lettersign_indicator: lettersign::IndicatorBuilder,
    nocontract_indicator: nocontract::IndicatorBuilder,
    emphasis_indicator: emphasis::IndicatorBuilder,
    computer_braille_indicator: computer_braille::IndicatorBuilder,
    numeric_constrainer: NumericConstrainerBuilder,
    backward_caps_constrainer: BackwardCapsConstrainerBuilder,
    backward_caps_word: BackwardCapsWordBuilder,
    backward_numeric_constrainer: BackwardNumericConstrainerBuilder,
    backward_nocontract_constrainer: BackwardNoContractConstrainerBuilder,
    hyphenation_constrainer: HyphenationConstrainerBuilder,
    syllable_constrainer: SyllableConstrainerBuilder,
    punctuation_constrainer: PunctuationConstrainerBuilder,
    join_constrainer: JoinConstrainerBuilder,
    nocont_constrainer: NoContConstrainerBuilder,
    capsnocont_constrainer: CapsNoContConstrainerBuilder,
    repword_constrainer: RepwordConstrainerBuilder,
    backward_digit_overrides: HashMap<char, ResolvedTranslation>,
    compbrl_triggers: Vec<String>,
}

impl PrimaryTableBuilder {
    /// `lookahead_classes` must be the character classes to resolve the lookahead
    /// (`Transition::End`, `ClassConstraints::following`) boundary checks against: text classes for
    /// forward translation, dots classes for backward. `lookbehind_classes` is the same for the
    /// lookbehind checks, and is the text classes in both directions, since the lookbehind
    /// reads the output produced so far when translating backward (see
    /// [`PrimaryTable::trace`]). The tries need both from construction on, since
    /// `Trie::insert` resolves those checks against them immediately rather than deferring
    /// to match time. `beg_word` resolves `Transition::BegWord`, `end_word` resolves
    /// `Transition::EndWord` and `Transition::ContinuesWord`, `standing_alone` resolves
    /// `Transition::StandsAloneBefore` and `Transition::StandsAloneAfter`.
    fn new(
        lookahead_classes: CharacterClasses,
        lookbehind_classes: CharacterClasses,
        beg_word: BegWordScanner,
        end_word: EndWordScanner,
        standing_alone: StandingAloneScanner,
    ) -> Self {
        Self {
            undefined: None,
            character_translations: CharacterTranslation::new(),
            trie: Trie::new()
                .with_context(lookahead_classes.clone())
                .with_lookbehind_classes(lookbehind_classes.clone())
                .with_beg_word_scanner(beg_word.clone())
                .with_end_word_scanner(end_word.clone())
                .with_standing_alone_scanner(standing_alone.clone()),
            comp6_trie: Trie::new()
                .with_context(lookahead_classes.clone())
                .with_lookbehind_classes(lookbehind_classes.clone())
                .case_sensitive(),
            nocross_trie: Trie::new()
                .with_context(lookahead_classes)
                .with_lookbehind_classes(lookbehind_classes)
                .with_beg_word_scanner(beg_word)
                .with_end_word_scanner(end_word)
                .with_standing_alone_scanner(standing_alone),
            match_patterns: MatchPatternsBuilder::new(),
            context_patterns: ContextPatternsBuilder::new(),
            numeric_indicator: numeric::IndicatorBuilder::new(),
            uppercase_indicator: uppercase::IndicatorBuilder::new(),
            mode_indicator: mode::IndicatorBuilder::new(),
            lettersign_indicator: lettersign::IndicatorBuilder::new(),
            nocontract_indicator: nocontract::IndicatorBuilder::new(),
            emphasis_indicator: emphasis::IndicatorBuilder::new(),
            computer_braille_indicator: computer_braille::IndicatorBuilder::new(),
            numeric_constrainer: NumericConstrainerBuilder::new(),
            backward_caps_constrainer: BackwardCapsConstrainerBuilder::new(),
            backward_caps_word: BackwardCapsWordBuilder::new(),
            backward_numeric_constrainer: BackwardNumericConstrainerBuilder::new(),
            backward_nocontract_constrainer: BackwardNoContractConstrainerBuilder::new(),
            hyphenation_constrainer: HyphenationConstrainerBuilder::new(),
            syllable_constrainer: SyllableConstrainerBuilder::new(),
            punctuation_constrainer: PunctuationConstrainerBuilder::new(),
            join_constrainer: JoinConstrainerBuilder::new(),
            nocont_constrainer: NoContConstrainerBuilder::new(),
            capsnocont_constrainer: CapsNoContConstrainerBuilder::new(),
            repword_constrainer: RepwordConstrainerBuilder::new(),
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
                self.trie
                    .insert_char(c, dots, direction, TranslationStage::Main, rule)
            }
        }
    }

    /// Inserts a zero-output entry for a backward indicator opcode (`capsletter`,
    /// `begcapsword`, `numsign`, ...), so its dots are recognized in backward
    /// translation instead of falling through to a coincidentally-identical
    /// character definition. Only meaningful for `Direction::Backward`.
    fn insert_backward_indicator(&mut self, dots: &str, rule: &AnchoredRule) {
        self.insert_backward_indicator_between(dots, None, None, rule);
    }

    /// [`Self::insert_backward_indicator`], with checks on the text before the indicator
    /// and on the cells after it.
    fn insert_backward_indicator_between(
        &mut self,
        dots: &str,
        before: Option<Transition>,
        after: Option<Transition>,
        rule: &AnchoredRule,
    ) {
        self.trie.insert(
            "",
            dots,
            before,
            after,
            Direction::Backward,
            ClassConstraints::default(),
            TranslationStage::Main,
            rule,
        );
    }

    /// Backward, an emphasis indicator's cells say where emphasis starts or stops and
    /// stand for no text, so they are consumed rather than read as the characters that
    /// happen to share their dots. The emphasis itself would go into the typeform, which
    /// louis-rs does not report yet. Only meaningful for `Direction::Backward`.
    ///
    /// When the dots are also those of a character that isn't a letter, as with
    /// `begemphphrase italic 56` and "¨" in the Danish 1993 tables, the indicator is
    /// only consumed in front of a letter, like `letsign`.
    fn insert_backward_emphasis_indicator(
        &mut self,
        opcode: &str,
        dots: &str,
        rule: &AnchoredRule,
        ctx: &TableContext,
        letter_dots: &HashSet<String>,
    ) {
        if Self::backward_indicator_dots_available(opcode, dots, letter_dots) {
            if ctx.character_definitions().defines_dots(dots) {
                self.insert_backward_indicator_before_letter(dots, rule);
            } else {
                self.insert_backward_indicator(dots, rule);
            }
        }
    }

    /// Like [`Self::insert_backward_indicator`], but only consumes the cell when the
    /// *next* cell is a letter or sign cell.
    ///
    /// `letsign`'s dots are routinely shared with an ordinary punctuation character
    /// (English defines both `letsign 56` and `punctuation ; 56`), so an unguarded
    /// backward indicator would swallow every `;`.
    fn insert_backward_indicator_before_letter(&mut self, dots: &str, rule: &AnchoredRule) {
        self.insert_backward_indicator_between(dots, None, Some(Self::before_letter()), rule);
    }

    /// Like [`Self::insert_backward_indicator_before_letter`], but also not right after
    /// a letter, as liblouis takes a `letsign`. After one, the dots are something else:
    /// Danish ⠰ is also `endcapsword`.
    fn insert_backward_letsign(&mut self, dots: &str, rule: &AnchoredRule) {
        self.insert_backward_indicator_between(
            dots,
            Some(Self::not_after_letter()),
            Some(Self::before_letter()),
            rule,
        );
    }

    /// A letter or sign cell follows.
    fn before_letter() -> Transition {
        Transition::End(vec![CharacterClass::Letter, CharacterClass::Sign])
    }

    /// The text written so far doesn't end in a letter.
    fn not_after_letter() -> Transition {
        Transition::NotStart(vec![CharacterClass::Letter])
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
            // liblouis puts the nocontractsign before a contraction ahead of the letsign
            // and the capitals, so "B" gives ⠰⠠⠃ and not ⠠⠰⠃.
            self.nocontract_indicator
                .build(ctx)
                .map(Indicator::NoContract),
            self.lettersign_indicator
                .build(ctx)
                .map(Indicator::LetterSign),
            self.uppercase_indicator
                .build(ctx)
                .map(Indicator::Uppercase),
            self.mode_indicator.build(ctx).map(Indicator::Mode),
            // Last, as in liblouis: "1A" must give ⠠⠰ for en-ueb-g2's pass2 rule
            // `_$D[@6-56]$U` to drop the nonumsign in front of a capital.
            self.numeric_indicator.build().map(Indicator::Numeric),
        ]
        .into_iter()
        .flatten()
        .collect();
        PrimaryTable {
            backward_caps_word: (direction == Direction::Backward)
                .then(|| self.backward_caps_word.build(ctx)),
            undefined: self.undefined,
            character_translations: self.character_translations,
            direction,
            fallback_definitions: ctx.character_definitions().clone(),
            trie: self.trie,
            comp6_trie: self.comp6_trie,
            nocross_trie: self.nocross_trie,
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
                    self.backward_nocontract_constrainer
                        .build()
                        .map(Constrainer::BackwardNoContract),
                    // liblouis's `nocross`/hyphenation-based syllable-break check is a
                    // forward-translation-only mechanism (lou_backTranslateString.c never
                    // consults it); a nocross rule is always an unrestricted candidate
                    // during backward translation.
                    (direction == Direction::Forward)
                        .then(|| self.hyphenation_constrainer.build())
                        .flatten()
                        .map(Constrainer::Hyphenation),
                    (direction == Direction::Forward)
                        .then(|| self.syllable_constrainer.build())
                        .flatten()
                        .map(Constrainer::Syllable),
                    // backward, `prepunc` and `postpunc` are gated in the trie by where a
                    // word begins and ends instead (see the `Rule::Prepunc` arm)
                    (direction == Direction::Forward)
                        .then(|| self.punctuation_constrainer.build())
                        .flatten()
                        .map(Constrainer::Punctuation),
                    (direction == Direction::Forward)
                        .then(|| self.join_constrainer.build())
                        .flatten()
                        .map(Constrainer::Join),
                    (direction == Direction::Forward)
                        .then(|| self.nocont_constrainer.build())
                        .flatten()
                        .map(Constrainer::NoCont),
                    (direction == Direction::Forward)
                        .then(|| self.capsnocont_constrainer.build())
                        .flatten()
                        .map(Constrainer::CapsNoCont),
                    (direction == Direction::Forward)
                        .then(|| self.repword_constrainer.build())
                        .flatten()
                        .map(Constrainer::Repword),
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
    /// Resolves `before CLASS`/`after CLASS` rule modifiers into the trie's
    /// [`ClassConstraints`]. `before` is a lookahead (the character *after* the match must be
    /// in the class); `after` is a lookbehind (the character *before* the match must be in
    /// the class). A class the table doesn't define is left out.
    fn resolve_class_constraints(
        ctx: &TableContext,
        with_classes: &[WithClass],
    ) -> ClassConstraints {
        let defined = |class: &str| {
            let key = CharacterClass::from(class);
            ctx.character_classes.get(&key).map(|_| key)
        };
        let preceding = with_classes
            .iter()
            .filter_map(|wc| match wc {
                WithClass::After { class } => defined(class),
                WithClass::Before { .. } => None,
            })
            .collect();
        let following = with_classes
            .iter()
            .filter_map(|wc| match wc {
                WithClass::Before { class } => defined(class),
                WithClass::After { .. } => None,
            })
            .collect();
        ClassConstraints::new(preceding, following)
    }

    /// The after-side boundary check for a rule that must end a word: a class test on the
    /// next character forward, a scan of the braille that follows backward (see
    /// [`EndWordScanner`]).
    fn word_end(direction: Direction) -> Transition {
        match direction {
            Direction::Forward => {
                Transition::End(vec![CharacterClass::Space, CharacterClass::Punctuation])
            }
            Direction::Backward => Transition::EndWord,
        }
    }

    /// The after-side boundary check for a rule that must not end a word: a letter test on
    /// the next character forward, the same scan negated backward.
    fn word_continues(direction: Direction) -> Transition {
        match direction {
            Direction::Forward => Transition::End(vec![CharacterClass::Letter]),
            Direction::Backward => Transition::ContinuesWord,
        }
    }

    pub fn compile(
        rules: &[AnchoredRule],
        direction: Direction,
        _stage: TranslationStage,
        ctx: &TableContext,
    ) -> Result<Self, TranslationError> {
        // definition order is the order of `rules`, see `Rank::definition_order`
        let rules = &in_definition_order(rules);
        // Word/number boundary checks must consult the classes of the characters they are
        // checked against. The lookahead (`Transition::End`, `ClassConstraints::following`)
        // reads the input, which is text forward and braille backward. The lookbehind
        // (`Transition::Start`, `ClassConstraints::preceding`) reads text in both directions:
        // backward it reads the output produced so far, as liblouis's `back_setBefore` and
        // `isBegWord` do, because a braille cell does not say how it was translated.
        let lookahead_classes = match direction {
            Direction::Forward => ctx.character_classes.clone(),
            Direction::Backward => ctx.dots_classes().clone(),
        };
        // only backward tries insert `Transition::BegWord`, `Transition::EndWord` and
        // `Transition::ContinuesWord`
        let (beg_word, end_word) = match direction {
            Direction::Forward => (BegWordScanner::default(), EndWordScanner::default()),
            Direction::Backward => (
                BegWordScanner::new(&ctx.character_classes),
                EndWordScanner::compile(rules),
            ),
        };
        // backward, an `always` rule on a cell outranks a `prepunc` or `postpunc` rule on it.
        // liblouis decides by table order, and tables put the `always` rule first to override
        // the punctuation (`pu-in-g1.utb`: "needed to override ! in en-in-g1.ctb"); without
        // this the word-boundary transition would give the punctuation rule the higher weight
        let always_cells: HashSet<String> = rules
            .iter()
            .filter_map(|rule| match &rule.rule {
                Rule::Always {
                    dots: Braille::Explicit(dots),
                    ..
                } if dots.len() == 1 && rule.rule.is_direction(Direction::Backward) => {
                    Some(dots.to_string())
                }
                _ => None,
            })
            .collect();
        // forward, a table with a `seqdelimiter` checks whether a `word` or `contraction`
        // rule stands alone rather than for a space or punctuation on either side
        let standing_alone = match direction {
            Direction::Forward => StandingAloneScanner::new(
                &ctx.character_classes,
                rules
                    .iter()
                    .filter_map(|rule| match &rule.rule {
                        Rule::Seqafterpattern { chars } => Some(chars.clone()),
                        _ => None,
                    })
                    .collect(),
            ),
            Direction::Backward => None,
        };
        let (stands_alone_before, stands_alone_after) = match standing_alone {
            Some(_) => (Transition::StandsAloneBefore, Transition::StandsAloneAfter),
            None => (
                Transition::Start(vec![CharacterClass::Space, CharacterClass::Punctuation]),
                Self::word_end(direction),
            ),
        };
        let mut builder = PrimaryTableBuilder::new(
            lookahead_classes,
            ctx.character_classes.clone(),
            beg_word,
            end_word,
            standing_alone.clone().unwrap_or_default(),
        );
        if let Some(scanner) = standing_alone {
            builder.lettersign_indicator.standing_alone(scanner.clone());
            builder.nocontract_indicator.standing_alone(scanner);
        }

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

        for rule in rules {
            match &rule.rule {
                Rule::Undefined { dots } => {
                    builder.undefined = Some(dots.to_string());
                }
                Rule::Space {
                    character, dots, ..
                }
                | Rule::Hyphen {
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
                Rule::Nonumsign { dots, .. } => {
                    builder.numeric_indicator.nonumsign(&dots.to_string(), rule);
                    builder.numeric_constrainer.nonumsign(&dots.to_string());
                    if direction == Direction::Backward
                        && PrimaryTableBuilder::backward_indicator_dots_available(
                            "nonumsign",
                            &dots.to_string(),
                            &letter_dots,
                        )
                    {
                        // not right after a letter, as liblouis takes it
                        builder.insert_backward_indicator_between(
                            &dots.to_string(),
                            Some(PrimaryTableBuilder::not_after_letter()),
                            None,
                            rule,
                        );
                        builder
                            .backward_numeric_constrainer
                            .nonumsign(&dots.to_string());
                        builder
                            .backward_nocontract_constrainer
                            .indicator(&dots.to_string());
                    }
                }
                Rule::Numericnocontchars { chars } => {
                    builder.numeric_indicator.numericnocontchars(chars);
                    builder.numeric_constrainer.numeric_mode();
                }
                Rule::Numericmodechars { chars } => {
                    builder.numeric_indicator.numericmodechars(chars);
                    builder.numeric_constrainer.numeric_mode();
                }
                Rule::Midendnumericmodechars { chars } => {
                    builder.numeric_indicator.midendnumericmodechars(chars);
                    builder.numeric_constrainer.numeric_mode();
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
                        builder
                            .backward_nocontract_constrainer
                            .caps_indicator(&dots.to_string());
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
                        builder
                            .backward_nocontract_constrainer
                            .caps_indicator(&dots.to_string());
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
                            .backward_nocontract_constrainer
                            .caps_indicator(&dots.to_string());
                    }
                }
                Rule::Begcaps { dots } => {
                    builder.uppercase_indicator.begcaps(&dots.to_string(), rule);
                    builder
                        .backward_nocontract_constrainer
                        .caps_indicator(&dots.to_string());
                }
                Rule::Endcaps { dots } => {
                    builder.uppercase_indicator.endcaps(&dots.to_string(), rule);
                    builder
                        .backward_nocontract_constrainer
                        .caps_indicator(&dots.to_string());
                }
                Rule::Capsmodechars { chars } => {
                    builder.uppercase_indicator.capsmodechars(chars);
                    builder.backward_caps_word.capsmodechars(chars);
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
                Rule::Modeletter { name, dots, .. } => {
                    builder
                        .mode_indicator
                        .modeletter(name, &dots.to_string(), rule);
                }
                Rule::Begmodeword { name, dots, .. } => {
                    builder
                        .mode_indicator
                        .begmodeword(name, &dots.to_string(), rule);
                }
                Rule::Endmodeword { name, dots, .. } => {
                    builder
                        .mode_indicator
                        .endmodeword(name, &dots.to_string(), rule);
                }
                Rule::Begmode { name, dots } => {
                    builder
                        .mode_indicator
                        .begmode(name, &dots.to_string(), rule);
                }
                Rule::Endmode { name, dots } => {
                    builder
                        .mode_indicator
                        .endmode(name, &dots.to_string(), rule);
                }
                Rule::Begmodephrase { name, dots } => {
                    builder
                        .mode_indicator
                        .begmodephrase(name, &dots.to_string(), rule);
                }
                Rule::Endmodephrase {
                    name,
                    dots,
                    position,
                } => {
                    builder
                        .mode_indicator
                        .endmodephrase(name, &dots.to_string(), position, rule);
                }
                Rule::Lenmodephrase { name, number } => {
                    builder.mode_indicator.lenmodephrase(name, *number as usize);
                }
                Rule::Emphclass { name } => {
                    builder.emphasis_indicator.emphclass(name);
                }
                Rule::Emphletter { name, dots } => {
                    builder
                        .emphasis_indicator
                        .emphletter(name, &dots.to_string(), rule);
                    if direction == Direction::Backward {
                        builder.insert_backward_emphasis_indicator(
                            "emphletter",
                            &dots.to_string(),
                            rule,
                            ctx,
                            &letter_dots,
                        );
                    }
                }
                Rule::Begemphword { name, dots } => {
                    builder
                        .emphasis_indicator
                        .begemphword(name, &dots.to_string(), rule);
                    if direction == Direction::Backward {
                        builder.insert_backward_emphasis_indicator(
                            "begemphword",
                            &dots.to_string(),
                            rule,
                            ctx,
                            &letter_dots,
                        );
                    }
                }
                Rule::Endemphword { name, dots } => {
                    builder
                        .emphasis_indicator
                        .endemphword(name, &dots.to_string(), rule);
                    if direction == Direction::Backward {
                        builder.insert_backward_emphasis_indicator(
                            "endemphword",
                            &dots.to_string(),
                            rule,
                            ctx,
                            &letter_dots,
                        );
                    }
                }
                Rule::Begemphphrase { name, dots } => {
                    builder
                        .emphasis_indicator
                        .begemphphrase(name, &dots.to_string(), rule);
                    if direction == Direction::Backward {
                        builder.insert_backward_emphasis_indicator(
                            "begemphphrase",
                            &dots.to_string(),
                            rule,
                            ctx,
                            &letter_dots,
                        );
                    }
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
                    if direction == Direction::Backward {
                        builder.insert_backward_emphasis_indicator(
                            "endemphphrase",
                            &dots.to_string(),
                            rule,
                            ctx,
                            &letter_dots,
                        );
                    }
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
                    if direction == Direction::Backward {
                        builder.insert_backward_emphasis_indicator(
                            "begemph",
                            &dots.to_string(),
                            rule,
                            ctx,
                            &letter_dots,
                        );
                    }
                }
                Rule::Endemph { name, dots, .. } => {
                    builder
                        .emphasis_indicator
                        .endemph(name, &dots.to_string(), rule);
                    if direction == Direction::Backward {
                        builder.insert_backward_emphasis_indicator(
                            "endemph",
                            &dots.to_string(),
                            rule,
                            ctx,
                            &letter_dots,
                        );
                    }
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
                    if direction == Direction::Backward
                        && PrimaryTableBuilder::backward_indicator_dots_available(
                            "letsign",
                            &dots.to_string(),
                            &letter_dots,
                        )
                    {
                        builder.insert_backward_letsign(&dots.to_string(), rule);
                        builder
                            .backward_nocontract_constrainer
                            .indicator(&dots.to_string());
                    }
                }
                Rule::Multind { dots, names, .. } => {
                    // Back-translation only: `dots` is several indicators in a row
                    // (`multind 56-6 letsign capsletter`). Each indicator's own
                    // backward guard would reject it here -- letsign wants a letter
                    // next, not the capital sign -- so the whole sequence is consumed
                    // as one unit and each named indicator's effect is keyed on it.
                    let dots = dots.to_string();
                    if direction == Direction::Backward
                        && PrimaryTableBuilder::backward_indicator_dots_available(
                            "multind",
                            &dots,
                            &letter_dots,
                        )
                    {
                        builder.insert_backward_indicator(&dots, rule);
                        for name in names {
                            match name.as_str() {
                                "letsign" | "nocontractsign" | "nonumsign" => {
                                    builder.backward_nocontract_constrainer.indicator(&dots)
                                }
                                "capsletter" => {
                                    builder.backward_caps_constrainer.multind_capsletter(&dots)
                                }
                                "begcapsword" => {
                                    builder.backward_caps_constrainer.multind_begcapsword(&dots)
                                }
                                "numsign" => {
                                    builder.backward_numeric_constrainer.multind_numsign(&dots)
                                }
                                // endcapsword etc.: consuming the cells is the
                                // whole effect, the constrainers already end a
                                // caps word at the first non-letter cell.
                                _ => (),
                            }
                        }
                    }
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
                // The replacement is text, translated by the character definitions; without
                // one the characters are dropped. Forward only, like liblouis.
                Rule::Replace { chars, replacement } => {
                    if direction == Direction::Forward {
                        let dots = match replacement {
                            Some(replacement) => ctx
                                .character_definitions()
                                .braille_to_unicode(&Braille::Implicit, replacement)?,
                            None => String::new(),
                        };
                        builder.get_trie_mut(rule).insert(
                            chars,
                            &dots,
                            None,
                            None,
                            direction,
                            ClassConstraints::default(),
                            TranslationStage::Main,
                            rule,
                        );
                    }
                }
                // Treat a contraction rule similarly to a word rule, but forward only
                Rule::Contraction { chars } => {
                    if direction == Direction::Forward {
                        let dots = ctx
                            .character_definitions()
                            .braille_to_unicode(&Braille::Implicit, chars)?;
                        builder.get_trie_mut(rule).insert(
                            chars,
                            &dots,
                            Some(stands_alone_before.clone()),
                            Some(stands_alone_after.clone()),
                            direction,
                            ClassConstraints::default(),
                            TranslationStage::Main,
                            rule,
                        );
                    }
                    builder.lettersign_indicator.contraction(chars, rule);
                    builder.nocontract_indicator.contraction(chars, rule);
                }
                Rule::Nocontractsign { dots, .. } => {
                    builder
                        .nocontract_indicator
                        .nocontractsign(&dots.to_string(), rule);
                    builder
                        .numeric_constrainer
                        .nocontractsign(&dots.to_string());
                    if direction == Direction::Backward
                        && PrimaryTableBuilder::backward_indicator_dots_available(
                            "nocontractsign",
                            &dots.to_string(),
                            &letter_dots,
                        )
                    {
                        builder.insert_backward_indicator_before_letter(&dots.to_string(), rule);
                        builder
                            .backward_nocontract_constrainer
                            .indicator(&dots.to_string());
                    }
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
                Rule::Comp6 { character, dots } => {
                    let character = character.to_string();
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, &character)?;
                    builder.comp6_trie.insert(
                        &character,
                        &dots,
                        None,
                        None,
                        direction,
                        ClassConstraints::default(),
                        TranslationStage::Main,
                        rule,
                    );
                }
                Rule::Repeated { chars, dots, .. } => {
                    // Like `always`; `trace` then swallows the copies of `chars`
                    // that follow. liblouis skips the single-cell backward rule
                    // as too ambiguous (`repeated \s 0` would turn every blank
                    // cell into a space).
                    let dots = dots.to_string();
                    if direction == Direction::Forward || dots.chars().count() > 1 {
                        builder.get_trie_mut(rule).insert(
                            chars,
                            &dots,
                            None,
                            None,
                            direction,
                            ClassConstraints::default(),
                            TranslationStage::Main,
                            rule,
                        );
                    }
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
                    let class_constraints = Self::resolve_class_constraints(ctx, with_classes);
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        None,
                        None,
                        direction,
                        class_constraints,
                        TranslationStage::Main,
                        rule,
                    );
                }
                // A syllable is an unconditional substitution, like `always`, and other rules
                // can't run into it (see `PrimaryTable::crosses_syllable`).
                Rule::Syllable { chars, dots } => {
                    builder.syllable_constrainer.syllable(chars);
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        None,
                        None,
                        direction,
                        ClassConstraints::default(),
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
                Rule::Capsnocont {} => builder.capsnocont_constrainer.mark_present(),
                Rule::Nocont { chars } => {
                    // Forward only, like liblouis: no dots to back-translate from.
                    builder.nocont_constrainer.nocont(chars);
                }
                // literal is a deprecated synonym of compbrl
                Rule::Compbrl { chars, .. } | Rule::Literal { chars } => {
                    builder.compbrl_triggers.push(chars.clone());
                }
                Rule::Largesign { chars, dots } => {
                    builder.lettersign_indicator.single_letter_word(chars);
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots.to_string(),
                        None,
                        None,
                        direction,
                        ClassConstraints::default(),
                        TranslationStage::Main,
                        rule,
                    );
                }
                Rule::Word {
                    chars,
                    dots,
                    with_classes,
                    ..
                } => {
                    builder.lettersign_indicator.single_letter_word(chars);
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    let class_constraints = Self::resolve_class_constraints(ctx, with_classes);
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Some(stands_alone_before.clone()),
                        Some(stands_alone_after.clone()),
                        direction,
                        class_constraints,
                        TranslationStage::Main,
                        rule,
                    );
                }
                Rule::Begword {
                    chars,
                    dots,
                    with_classes,
                    ..
                } => {
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    let class_constraints = Self::resolve_class_constraints(ctx, with_classes);
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Some(Transition::Start(vec![
                            CharacterClass::Space,
                            CharacterClass::Punctuation,
                        ])),
                        Some(Self::word_continues(direction)),
                        direction,
                        class_constraints,
                        TranslationStage::Main,
                        rule,
                    )
                }
                Rule::Midword {
                    chars,
                    dots,
                    with_classes,
                    ..
                } => {
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    if direction == Direction::Backward {
                        builder.backward_numeric_constrainer.after_letter(&dots);
                    }
                    let class_constraints = Self::resolve_class_constraints(ctx, with_classes);
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Some(Transition::Start(vec![CharacterClass::Letter])),
                        Some(Self::word_continues(direction)),
                        direction,
                        class_constraints,
                        TranslationStage::Main,
                        rule,
                    )
                }
                Rule::Partword {
                    chars,
                    dots,
                    with_classes,
                    ..
                } => {
                    // partword fires when preceded OR followed by a letter (i.e. not standalone).
                    // Two insertions cover all non-standalone positions:
                    // 1. preceded by word char (midword + endword)
                    // 2. at word-start followed by word char (begword)
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    let class_constraints = Self::resolve_class_constraints(ctx, with_classes);
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Some(Transition::Start(vec![CharacterClass::Letter])),
                        None,
                        direction,
                        class_constraints.clone(),
                        TranslationStage::Main,
                        rule,
                    );
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        None,
                        Some(Self::word_continues(direction)),
                        direction,
                        class_constraints,
                        TranslationStage::Main,
                        rule,
                    );
                }
                Rule::Midendword {
                    chars,
                    dots,
                    with_classes,
                    ..
                } => {
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    if direction == Direction::Backward {
                        builder.backward_numeric_constrainer.after_letter(&dots);
                    }
                    let class_constraints = Self::resolve_class_constraints(ctx, with_classes);
                    // backward, liblouis checks nothing after a midendword
                    let after = match direction {
                        Direction::Forward => Some(Transition::End(vec![
                            CharacterClass::Letter,
                            CharacterClass::Space,
                            CharacterClass::Punctuation,
                        ])),
                        Direction::Backward => None,
                    };
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Some(Transition::Start(vec![CharacterClass::Letter])),
                        after,
                        direction,
                        class_constraints,
                        TranslationStage::Main,
                        rule,
                    );
                }
                Rule::Endword {
                    chars,
                    dots,
                    with_classes,
                    ..
                } => {
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    if direction == Direction::Backward {
                        builder.backward_numeric_constrainer.after_letter(&dots);
                    }
                    let class_constraints = Self::resolve_class_constraints(ctx, with_classes);
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Some(Transition::Start(vec![CharacterClass::Letter])),
                        Some(Self::word_end(direction)),
                        direction,
                        class_constraints,
                        TranslationStage::Main,
                        rule,
                    );
                }
                Rule::Prfword {
                    chars,
                    dots,
                    with_classes,
                    ..
                } => {
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    let class_constraints = Self::resolve_class_constraints(ctx, with_classes);
                    // a prfword is basically syntactic sugar for a word rule combined with an
                    // endword rule. So just make the two appropriate insertions in the trie
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Some(Transition::Start(vec![
                            CharacterClass::Space,
                            CharacterClass::Punctuation,
                        ])),
                        Some(Self::word_end(direction)),
                        direction,
                        class_constraints.clone(),
                        TranslationStage::Main,
                        rule,
                    );
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Some(Transition::Start(vec![CharacterClass::Letter])),
                        Some(Self::word_end(direction)),
                        direction,
                        class_constraints,
                        TranslationStage::Main,
                        rule,
                    );
                }
                Rule::Sufword {
                    chars,
                    dots,
                    with_classes,
                    ..
                } => {
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    let class_constraints = Self::resolve_class_constraints(ctx, with_classes);
                    // forward, a sufword is basically syntactic sugar for a word rule combined
                    // with a begword rule; backward, liblouis checks nothing after it
                    let afters = match direction {
                        Direction::Forward => vec![
                            Some(Transition::End(vec![
                                CharacterClass::Space,
                                CharacterClass::Punctuation,
                            ])),
                            Some(Transition::End(vec![CharacterClass::Letter])),
                        ],
                        Direction::Backward => vec![None],
                    };
                    for after in afters {
                        builder.get_trie_mut(rule).insert(
                            chars,
                            &dots,
                            Some(Transition::Start(vec![
                                CharacterClass::Space,
                                CharacterClass::Punctuation,
                            ])),
                            after,
                            direction,
                            class_constraints.clone(),
                            TranslationStage::Main,
                            rule,
                        );
                    }
                }
                Rule::Begmidword {
                    chars,
                    dots,
                    with_classes,
                    ..
                } => {
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    let class_constraints = Self::resolve_class_constraints(ctx, with_classes);
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Some(Transition::Start(vec![
                            CharacterClass::Letter,
                            CharacterClass::Space,
                            CharacterClass::Punctuation,
                        ])),
                        Some(Self::word_continues(direction)),
                        direction,
                        class_constraints,
                        TranslationStage::Main,
                        rule,
                    );
                }
                // whitespace on both sides, no punctuation, see the liblouis manual
                Rule::Lowword { chars, dots, .. } => builder.get_trie_mut(rule).insert(
                    chars,
                    &dots.to_string(),
                    Some(Transition::Start(vec![CharacterClass::Space])),
                    Some(Transition::End(vec![CharacterClass::Space])),
                    direction,
                    ClassConstraints::default(),
                    TranslationStage::Main,
                    rule,
                ),
                Rule::Begnum { chars, dots, .. } => builder.get_trie_mut(rule).insert(
                    chars,
                    &dots.to_string(),
                    Some(Transition::Start(vec![
                        CharacterClass::Space,
                        CharacterClass::Punctuation,
                    ])),
                    Some(Transition::End(vec![CharacterClass::Digit])),
                    direction,
                    ClassConstraints::default(),
                    TranslationStage::Main,
                    rule,
                ),
                // Not a character definition: like liblouis, only before a digit (forward)
                Rule::Decpoint {
                    character, dots, ..
                } => {
                    // A decimal point extends the numeric run: digits that follow it
                    // must not get a second numsign.
                    let s = character.to_string();
                    builder.numeric_indicator.midnum(&s);
                    match direction {
                        Direction::Forward => builder.get_trie_mut(rule).insert(
                            &s,
                            &dots.to_string(),
                            None,
                            Some(Transition::End(vec![CharacterClass::Digit])),
                            direction,
                            ClassConstraints::default(),
                            TranslationStage::Main,
                            rule,
                        ),
                        // FIXME: liblouis also requires a digit after it backward, but an
                        // End transition here would add weight and beat an equally long
                        // `always` the table defines earlier, see TODO.org
                        Direction::Backward => {
                            builder.insert_character(*character, &dots.to_string(), direction, rule)
                        }
                    }
                }
                Rule::Midnum { chars, dots, .. } => {
                    builder.numeric_indicator.midnum(chars);
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots.to_string(),
                        Some(Transition::Start(vec![CharacterClass::Digit])),
                        Some(Transition::End(vec![CharacterClass::Digit])),
                        direction,
                        ClassConstraints::default(),
                        TranslationStage::Main,
                        rule,
                    );
                }
                Rule::Endnum { chars, dots, .. } => {
                    let dots = ctx
                        .character_definitions()
                        .braille_to_unicode(dots, chars)?;
                    if direction == Direction::Backward {
                        builder.backward_numeric_constrainer.endnum(&dots);
                    }
                    builder.get_trie_mut(rule).insert(
                        chars,
                        &dots,
                        Some(Transition::Start(vec![CharacterClass::Digit])),
                        None,
                        direction,
                        ClassConstraints::default(),
                        TranslationStage::Main,
                        rule,
                    );
                }
                Rule::Prepunc { chars, dots, .. } | Rule::Postpunc { chars, dots, .. } => {
                    let outranked = direction == Direction::Backward
                        && always_cells.contains(&dots.to_string());
                    // are all the characters punctuation?
                    if !outranked
                        && chars.chars().all(|c| {
                            ctx.character_classes
                                .get(&CharacterClass::Punctuation)
                                .is_some_and(|class| class.contains(&c))
                        })
                    {
                        // Forward, eligibility isn't a fixed-position lookaround (see
                        // `PunctuationConstrainer`), so the trie itself imposes no boundary;
                        // `trace()` gates it. Backward, a word must begin before a `prepunc`
                        // and end after a `postpunc`.
                        builder.punctuation_constrainer.mark_present();
                        let (before, after) = match (direction, &rule.rule) {
                            (Direction::Forward, _) => (None, None),
                            (Direction::Backward, Rule::Prepunc { .. }) => {
                                (Some(Transition::BegWord), None)
                            }
                            (Direction::Backward, _) => (None, Some(Transition::EndWord)),
                        };
                        builder.get_trie_mut(rule).insert(
                            chars,
                            &dots.to_string(),
                            before,
                            after,
                            direction,
                            ClassConstraints::default(),
                            TranslationStage::Main,
                            rule,
                        );
                    }
                }
                // Forward only, like liblouis. Fires only after a repetition of the word that
                // follows, a comparison the trie can't express: `trace()` gates it and
                // swallows the repetitions. A letter on either side of the separator holds for
                // both opcodes and keeps them apart from each other and from a plain rule
                // on the separator.
                Rule::Repword { separator, dots }
                | Rule::Rependword {
                    separator,
                    other: dots,
                    ..
                } => {
                    if direction == Direction::Forward {
                        builder.repword_constrainer.separator(*separator);
                        let (before, after) = match rule.rule {
                            Rule::Repword { .. } => {
                                (None, Some(Transition::End(vec![CharacterClass::Letter])))
                            }
                            _ => (Some(Transition::Start(vec![CharacterClass::Letter])), None),
                        };
                        builder.get_trie_mut(rule).insert(
                            &separator.to_string(),
                            &dots.to_string(),
                            before,
                            after,
                            direction,
                            ClassConstraints::default(),
                            TranslationStage::Main,
                            rule,
                        );
                    }
                }
                Rule::Joinnum { chars, dots, .. } | Rule::Joinword { chars, dots, .. } => {
                    match direction {
                        Direction::Forward => {
                            // Fires only when whitespace and then a digit (`joinnum`) or a
                            // letter (`joinword`) follow, a scan the trie can't express:
                            // `trace()` gates it and swallows the whitespace. The word end
                            // that whitespace implies is in the trie all the same, so it
                            // weighs as much as a `word` rule for the same characters and
                            // the one defined first wins, as in liblouis.
                            builder.join_constrainer.mark_present();
                            builder.get_trie_mut(rule).insert(
                                chars,
                                &dots.to_string(),
                                Some(Transition::Start(vec![
                                    CharacterClass::Space,
                                    CharacterClass::Punctuation,
                                ])),
                                Some(Self::word_end(direction)),
                                direction,
                                ClassConstraints::default(),
                                TranslationStage::Main,
                                rule,
                            );
                        }
                        Direction::Backward => {
                            // The output puts back the space that forward translation
                            // swallowed, as liblouis's insertSpace does. Not when a space
                            // follows anyway.
                            builder.get_trie_mut(rule).insert(
                                &format!("{chars} "),
                                &dots.to_string(),
                                Some(Transition::BegWord),
                                Some(Transition::ContinuesWord),
                                direction,
                                ClassConstraints::default(),
                                TranslationStage::Main,
                                rule,
                            );
                        }
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
                    // Forward, `*` translates the characters between `[` and `]` by their
                    // character definitions
                    let capture_target = match direction {
                        Direction::Forward => TranslationTarget::TranslateCapture,
                        Direction::Backward => TranslationTarget::Capture,
                    };
                    builder.context_patterns.insert(
                        test,
                        action,
                        rule,
                        TranslationStage::Main,
                        ctx,
                        &capture_target,
                    )?;
                }
                Rule::IncludeHyphenation { table, .. } => {
                    builder.hyphenation_constrainer.hyphenator(table.clone());
                }
                // compiled into the multipass and display stages
                Rule::Correct { .. }
                | Rule::Pass2 { .. }
                | Rule::Pass3 { .. }
                | Rule::Pass4 { .. }
                | Rule::Display { .. } => (),
                // collected into the table context (see `TableContext::compile`)
                Rule::Attribute { .. }
                // read into the `StandingAloneScanner` before this loop
                | Rule::Seqafterpattern { .. }
                | Rule::Seqafterchars { .. }
                | Rule::Seqbeforechars { .. }
                | Rule::Seqdelimiter { .. }
                | Rule::Swapcc { .. }
                | Rule::Swapcd { .. }
                | Rule::Swapdd { .. } => (),
                // expanded by the parser
                Rule::Include { .. } => (),
                // a no-op in liblouis too: compiled, but its only use in pattern.c is
                // commented out
                Rule::Seqafterexpression { .. } => (),
                // FIXME: not implemented yet, see TODO.org
                Rule::Class { .. }
                | Rule::Exactdots { .. }
                | Rule::Grouping { .. } => (),
            }
        }
        let classes = ctx.character_classes();
        let digits = classes.get(&CharacterClass::Digit).unwrap_or_default();
        let litdigits = classes.get(&CharacterClass::Litdigit).unwrap_or_default();
        // like liblouis, both `digit` and `litdigit` characters start a number
        let numeric_chars = &digits | &litdigits;
        builder
            .numeric_indicator
            .numeric_characters(numeric_chars.clone());
        builder
            .numeric_constrainer
            .numeric_characters(numeric_chars);
        let classes = ctx.character_classes();
        builder.numeric_constrainer.word_ends(
            &classes.get(&CharacterClass::Space).unwrap_or_default()
                | &classes
                    .get(&CharacterClass::Seqdelimiter)
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
        builder.hyphenation_constrainer.letter_characters(
            ctx.character_classes()
                .get(&CharacterClass::Letter)
                .unwrap_or_default(),
        );
        let letter_chars = ctx
            .character_classes()
            .get(&CharacterClass::Letter)
            .unwrap_or_default();
        builder
            .punctuation_constrainer
            .letter_characters(letter_chars.clone());
        builder.punctuation_constrainer.letter_or_digit_characters(
            letter_chars
                .union(
                    &ctx.character_classes()
                        .get(&CharacterClass::Digit)
                        .unwrap_or_default(),
                )
                .copied()
                .collect(),
        );
        builder
            .repword_constrainer
            .letter_characters(letter_chars.clone());
        // like liblouis's onlyLettersAhead, letters and literary digits but no digits
        let litdigit_chars = ctx
            .character_classes()
            .get(&CharacterClass::Litdigit)
            .unwrap_or_default();
        builder
            .join_constrainer
            .letter_characters(&letter_chars | &litdigit_chars);
        builder.join_constrainer.digit_characters(
            ctx.character_classes()
                .get(&CharacterClass::Digit)
                .unwrap_or_default(),
        );
        builder.capsnocont_constrainer.uppercase_characters(
            ctx.character_classes()
                .get(&CharacterClass::Uppercase)
                .unwrap_or_default(),
        );
        builder.capsnocont_constrainer.space_characters(
            ctx.character_classes()
                .get(&CharacterClass::Space)
                .unwrap_or_default(),
        );
        builder.nocont_constrainer.space_characters(
            ctx.character_classes()
                .get(&CharacterClass::Space)
                .unwrap_or_default(),
        );
        builder.join_constrainer.space_characters(
            ctx.character_classes()
                .get(&CharacterClass::Space)
                .unwrap_or_default(),
        );
        builder.punctuation_constrainer.space_characters(
            ctx.character_classes()
                .get(&CharacterClass::Space)
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
            // a numsign can be the inner part of a longer rule, see
            // `BackwardNumericConstrainer::covered`
            for rule in rules {
                match &rule.rule {
                    Rule::Always { chars, dots, .. }
                    | Rule::Word { chars, dots, .. }
                    | Rule::Sufword { chars, dots, .. }
                    | Rule::Prfword { chars, dots, .. }
                    | Rule::Begword { chars, dots, .. }
                    | Rule::Begmidword { chars, dots, .. }
                    | Rule::Midword { chars, dots, .. }
                    | Rule::Midendword { chars, dots, .. }
                    | Rule::Endword { chars, dots, .. }
                    | Rule::Partword { chars, dots, .. } => {
                        let dots = ctx
                            .character_definitions()
                            .braille_to_unicode(dots, chars)?;
                        builder.backward_numeric_constrainer.rule_dots(&dots);
                        // an `always` rule takes an indicator's dots as its own, where the
                        // word-family rules would only be kept off the letters after it
                        if matches!(rule.rule, Rule::Always { .. }) {
                            builder.backward_nocontract_constrainer.rule_dots(&dots);
                        }
                        // and so does a rule inside a word after a letter, as UEB's ⠰⠎ for
                        // "ness" does
                        if matches!(
                            rule.rule,
                            Rule::Begmidword { .. }
                                | Rule::Midword { .. }
                                | Rule::Midendword { .. }
                                | Rule::Endword { .. }
                                | Rule::Partword { .. }
                        ) {
                            builder
                                .backward_nocontract_constrainer
                                .inword_rule_dots(&dots);
                        }
                    }
                    // so does punctuation, as Danish `prepunc ! 56-235` does
                    Rule::Prepunc { dots, .. } | Rule::Postpunc { dots, .. } => {
                        builder
                            .backward_nocontract_constrainer
                            .rule_dots(&dots.to_string());
                    }
                    Rule::Context { test, .. } => {
                        if let Some(dots) = test.literal_text() {
                            builder.backward_numeric_constrainer.rule_dots(&dots);
                        }
                    }
                    _ => (),
                }
            }
            builder
                .backward_numeric_constrainer
                .letter_cells(letter_cells.clone());
            builder
                .backward_nocontract_constrainer
                .letter_cells(letter_cells.clone());
            // a character of several cells can start with an indicator's dots, as Danish
            // "é" does with ⠰⠿, and takes them as its own
            for dots in ctx.character_definitions().dots() {
                builder.backward_nocontract_constrainer.rule_dots(dots);
            }
            builder.backward_nocontract_constrainer.space_cells(
                ctx.dots_classes()
                    .get(&CharacterClass::Space)
                    .unwrap_or_default(),
            );
            // A `digit` cell that is also a letter reads as the letter, which a table
            // defines first, so only a `litdigit` makes it a digit inside a number.
            let mut digit_cells: HashSet<char> = ctx
                .dots_classes()
                .get(&CharacterClass::Digit)
                .unwrap_or_default()
                .difference(&letter_cells)
                .copied()
                .collect();
            digit_cells.extend(
                ctx.dots_classes()
                    .get(&CharacterClass::Litdigit)
                    .unwrap_or_default(),
            );
            builder
                .backward_numeric_constrainer
                .digit_cells(digit_cells);
            let numeric_mode_chars: HashSet<char> = rules
                .iter()
                .filter_map(|rule| match &rule.rule {
                    Rule::Numericmodechars { chars } | Rule::Midendnumericmodechars { chars } => {
                        Some(chars)
                    }
                    _ => None,
                })
                .flat_map(|chars| chars.chars())
                .collect();
            // A `decpoint` or `midnum` can give its character a cell of its own, as
            // `decpoint . 3` does in it-it-comp6.utb, where "." is otherwise 256.
            let decpoint_dots = rules.iter().filter_map(|rule| match &rule.rule {
                Rule::Decpoint {
                    character, dots, ..
                } if numeric_mode_chars.contains(character) => Some(dots.to_string()),
                Rule::Midnum { chars, dots, .. }
                    if chars.chars().count() == 1
                        && chars.chars().all(|c| numeric_mode_chars.contains(&c)) =>
                {
                    Some(dots.to_string())
                }
                _ => None,
            });
            // only single cells, a number can't continue across part of a multi-cell sign
            let single_cell = |dots: &str| -> Option<char> {
                let mut cells = dots.chars();
                cells.next().filter(|_| cells.next().is_none())
            };
            let char_cells: HashSet<char> = numeric_mode_chars
                .iter()
                .filter_map(|c| ctx.character_definitions().get(c))
                .filter_map(|dots| single_cell(dots))
                .collect();
            let decpoint_cells: HashSet<char> = decpoint_dots
                .filter_map(|dots| single_cell(&dots))
                .collect();
            builder
                .backward_numeric_constrainer
                .numeric_mode_cells(&char_cells | &decpoint_cells);
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
        preceding: &str,
    ) -> (Vec<ResolvedTranslation>, Vec<ResolvedTranslation>) {
        self.trie
            .find_translations(input, preceding)
            .into_iter()
            .partition(|t| t.offset() == 0)
    }

    /// A nocross candidate is valid only if it doesn't cross an internal
    /// hyphenation break of the word it's part of, mirroring liblouis's
    /// `syllableBreak`. `char_pos` is the candidate's starting position in
    /// the whole input, needed to look up the precomputed breaks.
    fn nocross_candidates(
        &self,
        input: &str,
        preceding: &str,
        char_pos: usize,
        constraints: &PositionConstraints,
    ) -> Vec<ResolvedTranslation> {
        self.nocross_trie
            .find_translations(input, preceding)
            .into_iter()
            .filter(|t| {
                !(char_pos..char_pos + t.length().saturating_sub(1))
                    .any(|pos| constraints.hyphenation_break_at(pos))
                    && !Self::crosses_syllable(t, input, char_pos, constraints)
            })
            .collect()
    }

    /// Whether `t`, starting at `char_pos` of the `input` that's left, would take the start
    /// of a `syllable` rule's syllable after its own first character, which liblouis doesn't
    /// allow (`validMatch`), unless that character is a capital. `context` rules are tested
    /// their own way.
    fn crosses_syllable(
        t: &ResolvedTranslation,
        input: &str,
        char_pos: usize,
        constraints: &PositionConstraints,
    ) -> bool {
        !matches!(t.rule(), Some(Rule::Context { .. }))
            && !input.chars().next().is_some_and(char::is_uppercase)
            && (char_pos + 1..char_pos + t.length()).any(|pos| constraints.syllable_start_at(pos))
    }

    fn context_candidates(
        &self,
        input: &str,
        env: &Environment,
        at_start: bool,
    ) -> (Vec<ResolvedTranslation>, Vec<ResolvedTranslation>) {
        self.context_patterns
            .find(input, env, at_start, &self.fallback_definitions)
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
        // backward, the output produced so far, kept up to date with `translations`
        let mut output = String::new();
        let mut output_from = 0;
        let mut seen: HashSet<TranslationSubset> = HashSet::default();
        let mut char_pos: usize = 0;
        // A zero-length candidate (e.g. a zero-width `context`/`match` action) re-enters
        // the loop at the same char_pos without advancing, so indications must only be
        // emitted the first time a given position is visited, not on every re-entry, and
        // the zero-length rules applied (`seen`) are forgotten only on moving on.
        let mut last_pos: Option<usize> = None;
        // A `match` rule's `pre` is a look-behind, run over the input before the cursor
        // read backwards: the end of this, see `MatchPatterns::find`.
        let reversed: String = input.chars().rev().collect();

        // Enrich spans with compbrl-detected regions.
        let enriched_spans = match &self.compbrl_scanner {
            Some(scanner) => scanner.enrich(input, options.emphasis()),
            None => options.emphasis().to_vec(),
        };

        let indications = self.indicators.precompute(input, &enriched_spans);
        let constraints = self.constrainers.precompute(input, &enriched_spans);

        loop {
            if last_pos != Some(char_pos) {
                translations.extend(indications.translations_at(char_pos));
                seen.clear();
                last_pos = Some(char_pos);
            }

            // Backward, the lookbehind reads the output produced so far rather than the
            // preceding braille (liblouis's `back_setBefore`): a cell is ambiguous -- ⠂ is
            // both a comma and "aa" in Afrikaans -- but the output records which reading was
            // chosen.
            let preceding = match self.direction {
                Direction::Forward => &input[..input.len() - chars.as_str().len()],
                Direction::Backward => {
                    output.extend(translations[output_from..].iter().map(|t| t.output()));
                    output_from = translations.len();
                    output.as_str()
                }
            };

            // whether we're about to match starting at the true beginning of the whole input,
            // needed by match/context rules anchored with liblouis' `^`/`` ` `` boundary markers
            let at_start = chars.as_str().len() == input.len();

            // First check for nocross candidates
            let nocross_candidate = self
                .nocross_candidates(chars.as_str(), preceding, char_pos, &constraints)
                .into_iter()
                .filter(|t| !(constraints.uncontracted_at(char_pos) && Self::contracts(t)))
                .max_by_key(|t| t.rank(self.direction));

            // given an input query the trie for matching translations. Then split off the
            // translations that are delayed, i.e. have an offset because they have a pre-pattern
            let (mut candidates, delayed) = self.translation_candidates(chars.as_str(), preceding);
            delayed_translations.extend(delayed);

            // then the match patterns, which all match at the cursor: `pre` is checked
            // backwards from it
            let cursor_byte = input.len() - chars.as_str().len();
            let reversed_preceding = &reversed[input.len() - cursor_byte..];
            candidates.extend(self.match_patterns.find(chars.as_str(), reversed_preceding));

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

            // Forward, some rules have a condition the trie can't express, so they're
            // inserted without it and gated here, mostly against the constrainers. A
            // rejected candidate is simply dropped, and a plainer rule or the character
            // definition (`character_translations`) takes the position, matching liblouis.
            if self.direction == Direction::Forward {
                candidates.retain(|t| match t.rule() {
                    // a variable-length scan of the input (see `PunctuationConstrainer`).
                    // Backward, the trie gates them by where a word begins and ends.
                    Some(Rule::Prepunc { .. }) => constraints.prepunc_ok_at(char_pos),
                    Some(Rule::Postpunc { .. }) => constraints.postpunc_ok_at(char_pos),
                    // whitespace and then a digit or a letter follow
                    Some(Rule::Joinnum { .. }) => {
                        constraints.space_before_digit_at(char_pos + t.length())
                    }
                    Some(Rule::Joinword { .. }) => {
                        constraints.space_before_letter_at(char_pos + t.length())
                    }
                    // a repetition of the word that follows, see ADR-0020
                    Some(Rule::Repword { .. } | Rule::Rependword { .. }) => self
                        .repeated_word(t, char_pos, &translations, &constraints, &indications)
                        .is_some(),
                    _ => true,
                });
            }

            // Backward numeric mode: a cell shared with a letter (common in six-dot
            // literary codes) reads as its digit. liblouis tries no rule of more than
            // one cell at a digit cell inside a number, so a contraction spelled by the
            // digits' letters, such as the shortform ⠁⠃ "about" after ⠼, must not claim
            // them either. Candidates that consume nothing don't compete for the cell
            // and stay.
            if self.direction == Direction::Backward
                && constraints.prefer_digit_at(char_pos)
                && let Some(next_char) = chars.clone().next()
                && let Some(digit_translation) = self.backward_digit_overrides.get(&next_char)
            {
                candidates.retain(|t| t.length() == 0);
                candidates.push(digit_translation.clone());
            }

            if constraints.uncontracted_at(char_pos) {
                candidates.retain(|t| !Self::contracts(t));
            }

            // a rule can't run into a `syllable` rule's syllable
            candidates
                .retain(|t| !Self::crosses_syllable(t, chars.as_str(), char_pos, &constraints));

            // from a number to the next space, suppress multi-character contractions
            if constraints.dont_contract_at(char_pos) {
                candidates.retain(|t| t.length() <= 1);
            }

            // Backward, once a letsign/nocontractsign/nonumsign's letters have ended, the
            // whole-word rules stay off until the next space (`begword` no longer does).
            if self.direction == Direction::Backward && constraints.no_whole_word_at(char_pos) {
                candidates.retain(|t| {
                    !matches!(
                        t.rule(),
                        Some(Rule::Word { .. } | Rule::Sufword { .. } | Rule::Prfword { .. })
                    )
                });
            }

            // Backward no-contract mode: a letsign/nocontractsign/nonumsign says the
            // cells that follow spell out letters, so the word-family rules must not
            // claim them. Only these four opcodes are suppressed -- liblouis leaves
            // `always`, `partword` and the mid-word opcodes eligible.
            if self.direction == Direction::Backward && constraints.no_contract_at(char_pos) {
                candidates.retain(|t| {
                    !matches!(
                        t.rule(),
                        Some(
                            Rule::Word { .. }
                                | Rule::Sufword { .. }
                                | Rule::Prfword { .. }
                                | Rule::Begword { .. }
                        )
                    )
                });
            }

            // In computer braille mode, discard normal candidates and use comp6 rules only.
            // comp6 rules have no pre-pattern so offset() is always 0 — no delayed handling needed.
            if constraints.use_comp6_at(char_pos) {
                candidates.clear();
                candidates.extend(self.comp6_trie.find_translations(chars.as_str(), preceding));
            }

            // Backward `capsletter`: capitalize the first character of whichever candidate
            // wins below — a cell can resolve to more than one character (e.g. a digraph
            // rule), and capsletter marks a single capitalized letter. A capitalized word
            // is done once translated (`BackwardCapsWord`).
            let titlecase_here =
                self.direction == Direction::Backward && constraints.titlecase_at(char_pos);
            let uppercase_if_needed =
                move |translation: ResolvedTranslation| -> ResolvedTranslation {
                    if titlecase_here {
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
                .max_by_key(|translation| translation.rank(self.direction));
            // A nocross rule wins if there is no plain candidate to beat (many, e.g.
            // liblouis' `nocross always en`, have no plain counterpart at all).
            if let Some(nocross) = &nocross_candidate
                && candidate.is_none_or(|t| nocross.rank(self.direction) >= t.rank(self.direction))
            {
                let translation = nocross.clone();
                let translation = uppercase_if_needed(translation);
                // move the iterator forward by the number of characters in the translation
                chars.nth(nocross.length() - 1);
                char_pos += nocross.length();
                translations.push(translation);
                delayed_translations = self.update_offsets(delayed_translations, nocross.length());
            } else if let Some(t) = candidate
                && let Some((index, len)) =
                    self.repeated_word(t, char_pos, &translations, &constraints, &indications)
            {
                if let Some(dots) = Self::rependword_dots(t) {
                    translations.insert(index, dots);
                }
                let repetition = Self::repetition_translations(
                    t,
                    char_pos,
                    len,
                    input,
                    &constraints,
                    &indications,
                );
                let consumed = repetition.iter().map(|t| t.length()).sum();
                translations.extend(repetition);
                chars.nth(consumed - 1);
                delayed_translations = self.update_offsets(delayed_translations, consumed);
                char_pos += consumed;
            } else if let Some(t) = candidate {
                let t = &self.swallow_following(t.clone(), chars.as_str(), char_pos, &constraints);
                if t.length() == 0 {
                    // if there is a zero-length translation candiate we run the risk of an infinite
                    // loop, so remember the current translation so we only apply it once
                    seen.insert(TranslationSubset::from(t));
                } else {
                    // move the iterator forward by the number of characters in the translation
                    chars.nth(t.length() - 1);
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
                        &self.handle_undefined_char(next_char, options),
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
        match &self.backward_caps_word {
            Some(caps_word) => caps_word.apply(translations),
            None => translations,
        }
    }

    /// Whether `t` comes from an opcode that liblouis switches off in a word with a
    /// `nocont` string (its dontContract). Character definitions, `prepunc`/`postpunc`,
    /// `context` and the number opcodes still apply there.
    fn contracts(t: &ResolvedTranslation) -> bool {
        matches!(
            t.rule(),
            Some(
                Rule::Always { .. }
                    | Rule::Syllable { .. }
                    | Rule::Repeated { .. }
                    | Rule::Repword { .. }
                    | Rule::Rependword { .. }
                    | Rule::Largesign { .. }
                    | Rule::Word { .. }
                    | Rule::Partword { .. }
                    | Rule::Joinnum { .. }
                    | Rule::Lowword { .. }
                    | Rule::Joinword { .. }
                    | Rule::Sufword { .. }
                    | Rule::Prfword { .. }
                    | Rule::Begword { .. }
                    | Rule::Begmidword { .. }
                    | Rule::Midword { .. }
                    | Rule::Midendword { .. }
                    | Rule::Endword { .. }
            )
        )
    }

    /// Forward, some rules also consume input after their match: `repeated` the
    /// copies of its characters that immediately follow, so `repeated --- 36-36-36`
    /// turns "---------" into one 36-36-36, and `joinnum` the whitespace before the
    /// number, so `joinnum § 346` turns "§ 3" into 346 and the number.
    fn swallow_following(
        &self,
        t: ResolvedTranslation,
        rest: &str,
        char_pos: usize,
        constraints: &PositionConstraints,
    ) -> ResolvedTranslation {
        if self.direction != Direction::Forward {
            return t;
        }
        let space_at: Option<fn(&PositionConstraints, usize) -> bool> = match t.rule() {
            Some(Rule::Joinnum { .. }) => Some(PositionConstraints::space_before_digit_at),
            Some(Rule::Joinword { .. }) => Some(PositionConstraints::space_before_letter_at),
            _ => None,
        };
        if let Some(space_at) = space_at {
            let mut length = t.length();
            while space_at(constraints, char_pos + length) {
                length += 1;
            }
            let input: String = rest.chars().take(length).collect();
            return t.with_input(&input);
        }
        if !matches!(t.rule(), Some(Rule::Repeated { .. })) {
            return t;
        }
        // The matched text, not the rule's `chars`: the trie matches case-insensitively,
        // so the two can differ, even in byte length.
        let end = rest
            .char_indices()
            .nth(t.length())
            .map_or(rest.len(), |(i, _)| i);
        let unit = &rest[..end];
        let mut consumed = &rest[end..];
        while let Some(after) = consumed.strip_prefix(unit) {
            consumed = after;
        }
        let input = &rest[..rest.len() - consumed.len()];
        t.with_input(input)
    }

    /// The word before a `repword`/`rependword` separator at `char_pos` that the letters
    /// after it repeat: the index of its first translation and its length. None if the
    /// rule doesn't apply: there is no such word, it starts inside an earlier
    /// translation (a contraction), or capitals or emphasis change inside the word or
    /// its repetition.
    fn repeated_word(
        &self,
        t: &ResolvedTranslation,
        char_pos: usize,
        translations: &[ResolvedTranslation],
        constraints: &PositionConstraints,
        indications: &PrecomputedIndications,
    ) -> Option<(usize, usize)> {
        let constraint = match t.rule() {
            Some(Rule::Repword { .. }) => Constraint::RepeatedWord,
            Some(Rule::Rependword { .. }) => Constraint::RepeatedWordEnd,
            _ => return None,
        };
        let len = constraints.run_before(char_pos, constraint);
        if len == 0 {
            return None;
        }
        let separator_end = char_pos + t.length();
        // an indicator stands for a change of capitals or emphasis, allowed only at the
        // start of the word and of its repetition. First the word after its first
        // character, and the separator
        if indications.any_in(char_pos - len + 1..separator_end) {
            return None;
        }
        // the repetition after its first character
        if indications.any_in(separator_end + 1..separator_end + len) {
            return None;
        }
        // the word must start where a translation starts, not inside a contraction
        let mut index = translations.len();
        let mut covered = 0;
        while covered < len && index > 0 {
            index -= 1;
            covered += translations[index].length();
        }
        if covered != len {
            return None;
        }
        // step back over the indicators of the first letter (zero length), so that
        // `rependword` puts its dots before a capital sign, like liblouis
        while index > 0 && translations[index - 1].length() == 0 {
            index -= 1;
        }
        Some((index, len))
    }

    /// `rependword`'s first dots, which go before the repeated word
    fn rependword_dots(t: &ResolvedTranslation) -> Option<ResolvedTranslation> {
        match t.rule() {
            Some(Rule::Rependword { dots, .. }) => {
                Some(t.clone().with_input("").with_output(&dots.to_string()))
            }
            _ => None,
        }
    }

    /// What replaces a `repword`/`rependword` separator at `char_pos` and the repetitions
    /// of the word of `len` letters before it: the separator without output, the
    /// indicators at the start of the repetition, then the rule's dots for all the
    /// repetitions
    fn repetition_translations(
        t: &ResolvedTranslation,
        char_pos: usize,
        len: usize,
        input: &str,
        constraints: &PositionConstraints,
        indications: &PrecomputedIndications,
    ) -> Vec<ResolvedTranslation> {
        let separator_end = char_pos + t.length();
        let end = Self::repetitions_end(separator_end + len, len, constraints, indications);
        let repetitions: String = input
            .chars()
            .skip(separator_end)
            .take(end - separator_end)
            .collect();
        let mut translations = vec![t.clone().with_output("")];
        translations.extend(indications.translations_at(separator_end));
        translations.push(t.clone().with_input(&repetitions));
        translations
    }

    /// Where the repetitions of a word of `len` letters end, the first of which ends at
    /// `end`: further ones, the separator and the word again, count as long as no
    /// indicator falls in them
    fn repetitions_end(
        mut end: usize,
        len: usize,
        constraints: &PositionConstraints,
        indications: &PrecomputedIndications,
    ) -> usize {
        while (end..end + 1 + len).all(|p| constraints.repetition_at(p))
            && !indications.any_in(end + 1..end + 1 + len)
        {
            end += 1 + len;
        }
        end
    }

    fn handle_undefined_char(&self, ch: char, options: &TranslationOptions) -> String {
        if options.mode().contains(TranslationMode::NoUndefined) {
            // the caller asked for undefined input to be dropped
            return String::new();
        }
        match self.direction {
            // the output is braille, so the escape is spelled in braille
            Direction::Forward => self.escape_to_braille(&unknown_hex(ch)),
            // the output is text, so the escape is spelled in text, and a character
            // that is not a cell has no dots to spell out
            Direction::Backward => unknown_dots(ch).unwrap_or_else(|| unknown_hex(ch)),
        }
    }

    /// Render the characters of an escape as braille, preferring the table's own
    /// definitions and falling back to [`fallback`] for what it does not define
    fn escape_to_braille(&self, escape: &str) -> String {
        escape
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

/// Map a char to dots according to the North American Braille Computer Code (NABCC)
///
/// A fallback mapping for character to braille in case the table does
/// not provide a mapping. This is used as a last resort when printing
/// unicode escapes for undefined characters when the table does not
/// define the character mappings that are needed. Only the characters
/// that [`PrimaryTable::handle_undefined_char`] can produce are mapped.
fn fallback(ch: char) -> BrailleChars {
    let dots = match ch {
        '0' => "356",
        '1' => "2",
        '2' => "23",
        '3' => "25",
        '4' => "256",
        '5' => "26",
        '6' => "235",
        '7' => "2356",
        '8' => "236",
        '9' => "35",
        'a' => "1",
        'b' => "12",
        'c' => "14",
        'd' => "145",
        'e' => "15",
        'f' => "124",
        '\\' => "1256",
        'x' => "1346",
        _ => unreachable!("no NABCC fallback for {ch:?}"),
    };
    BrailleChars::try_from(dots).expect("NABCC dot pattern is valid")
}

#[cfg(test)]
mod tests {
    use super::*;

    use search_path::SearchPath;

    use crate::parser::{RuleParser, expand_includes};

    #[test]
    fn fallback_covers_every_escaped_char() {
        // handle_undefined_char emits `\x` plus lowercase hex digits, so
        // fallback must map all of them without hitting the unreachable arm
        for c in r"\x0123456789abcdef".chars() {
            assert_eq!(fallback(c).len(), 1);
        }
        assert_eq!(fallback('a').to_string(), "\u{2801}"); // dot 1
        assert_eq!(fallback('9').to_string(), "\u{2814}"); // dots 35, not 25
    }

    fn parse_rule(source: &str) -> AnchoredRule {
        RuleParser::new(source).rule().unwrap().into()
    }

    /// A [`SearchPath`] rooted at the crate directory, for tests that `include` one of the
    /// fixtures under `dictionaries/`.
    fn fixture_search_path() -> SearchPath {
        SearchPath::from(std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")))
    }

    #[test]
    fn test_unknown_hex() {
        assert_eq!(unknown_hex('\u{4e2d}'), r"\x4e2d");
        assert_eq!(unknown_hex('a'), r"\x61");
        // the escape is spelled with the same characters fallback maps
        assert!(
            unknown_hex('\u{10ffff}')
                .chars()
                .all(|c| c == '\\' || c == 'x' || c.is_ascii_hexdigit())
        );
    }

    #[test]
    fn test_unknown_dots() {
        assert_eq!(unknown_dots('\u{28ff}'), Some(r"\12345678/".into()));
        assert_eq!(unknown_dots('\u{2840}'), Some(r"\7/".into()));
        assert_eq!(unknown_dots('\u{2800}'), Some(r"\0/".into()));
        // virtual dots live in the private use area
        assert_eq!(unknown_dots('\u{f0101}'), Some(r"\19/".into()));
        assert_eq!(unknown_dots('a'), None);
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
    fn repeated() {
        let rules = [
            parse_rule("punctuation - 36"),
            parse_rule("space \\s 0"),
            parse_rule("repeated --- 25-25"),
            parse_rule("repeated \\s 0"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("-------- -"), "⠒⠒⠤⠤⠀⠤");
        assert_eq!(table.translate("-    -"), "⠤⠀⠤");
    }

    #[test]
    fn nocont() {
        let rules = [
            parse_rule("letter a 1"),
            parse_rule("letter b 12"),
            parse_rule("punctuation @ 4"),
            parse_rule("space \\s 0"),
            parse_rule("always ab 123456"),
            parse_rule("nocont @"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        // only the word with the @ is uncontracted, all of it
        assert_eq!(table.translate("ab ab@ab ab"), "⠿⠀⠁⠃⠈⠁⠃⠀⠿");
    }

    #[test]
    fn joinnum() {
        let rules = [
            parse_rule("letter a 1"),
            parse_rule("digit 1 1"),
            parse_rule("punctuation $ 4"),
            parse_rule("space \\s 0"),
            parse_rule("joinnum $ 456"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        // whitespace and then a number: the whitespace goes
        assert_eq!(table.translate("$  1"), "⠸⠁");
        // no number, or no whitespace, or a letter before: plain punctuation
        assert_eq!(table.translate("$ a"), "⠈⠀⠁");
        assert_eq!(table.translate("$1"), "⠈⠁");
        assert_eq!(table.translate("a$ 1"), "⠁⠈⠀⠁");

        // backward the space comes back; with a space after the sign the rule doesn't
        // apply, and nothing else defines ⠸ here
        let table = PrimaryTable::compile(
            &rules,
            Direction::Backward,
            TranslationStage::Main,
            &context,
        )
        .unwrap();
        assert_eq!(table.translate("⠸⠁"), "$ a");
        assert_eq!(table.translate("⠸⠀⠁"), "\\456/ a");
    }

    #[test]
    fn replace() {
        let rules = [
            parse_rule("letter a 1"),
            parse_rule("letter b 12"),
            parse_rule("space \\x2060 0"),
            parse_rule("replace \\x2060\t#WORD JOINER"),
            parse_rule("replace c ab"),
            parse_rule("punctuation # 3456"),
            parse_rule("replace d \\#a"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        // without a replacement the character is dropped, a `#` starting a comment
        assert_eq!(table.translate("a\u{2060}b"), "⠁⠃");
        // the replacement is text, translated by the character definitions
        assert_eq!(table.translate("c"), "⠁⠃");
        // `\#` starts it with a literal `#`
        assert_eq!(table.translate("d"), "⠼⠁");
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
        // liblouis's real endword requires a letter before the match (CTO_EndWord:
        // beforeAttributes & CTC_Letter), so "bar" alone (nothing precedes it) is
        // spelled out, not contracted.
        assert_eq!(table.translate("bar"), "⠂⠁⠐");
        assert_eq!(table.translate("foobar"), "⠉⠑"); // both should be contracted
        assert_eq!(table.translate("foobar."), "⠉⠑⠠"); // both should be contracted
        assert_eq!(table.translate("foobarfoo"), "⠉⠂⠁⠐⠉"); // only foo should be contracted
        assert_eq!(table.translate("foobar foo"), "⠉⠑⠀⠉"); // both should be contracted
        // "bar" is preceded by a space here, not a letter, so it's its own standalone
        // word rather than embedded in a larger one - endword requires a letter before
        // the match, so it's spelled out, not contracted.
        assert_eq!(table.translate("foo bar foo"), "⠉⠀⠂⠁⠐⠀⠉");
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
    fn postpunc_before_word_rule_defined_after_it() {
        // liblouis takes the first rule that applies, not the one checking more context:
        // the closing quote is the postpunc, as in en-g3.ctb
        let rules = [
            parse_rule("lowercase a 1"),
            parse_rule("punctuation \" 5"),
            parse_rule("prepunc \" 236"),
            parse_rule("postpunc \" 356"),
            parse_rule("word \" 2356"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("\"a\""), "⠦⠁⠴");
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
    fn match_pre_after_multi_character_translation() {
        // "ar" is consumed in one step, jumping over the "r" where `pre` starts
        let rules = [
            parse_rule("lowercase a 1"),
            parse_rule("lowercase e 15"),
            parse_rule("lowercase r 1235"),
            parse_rule("lowercase s 234"),
            parse_rule("always ar 345"),
            parse_rule("match %a ea %a 2"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("areas"), "⠜⠂⠎");
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
    fn number_sign_before_a_digit() {
        // a `digit` character starts a number, not only a `litdigit` one
        let rules = [parse_rule("digit 1 1"), parse_rule("numsign 3456")];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("1"), "⠼⠁");
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
    fn rependword_dots_go_before_the_capital_sign() {
        let rules = [
            parse_rule("lowercase x 1346"),
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 12"),
            parse_rule("lowercase c 14"),
            parse_rule("base uppercase A a"),
            parse_rule("capsletter 46"),
            parse_rule("sign - 25"),
            parse_rule("repword - 123456"),
            parse_rule("rependword - 456,123456"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("xAbc-Abc"), "⠭⠸⠨⠁⠃⠉⠨⠿");
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
    fn nonumsign_comes_after_capsletter() {
        // en-ueb-g2's pass2 rule `_$D[@6-56]$U @6` relies on this order to drop the
        // nonumsign in "1A"
        let rules = [
            parse_rule("lowercase a 1"),
            parse_rule("base uppercase A a"),
            parse_rule("litdigit 1 1"),
            parse_rule("numsign 3456"),
            parse_rule("nonumsign 56"),
            parse_rule("numericnocontchars aA"),
            parse_rule("capsletter 6"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("1a"), "⠼⠁⠰⠁");
        assert_eq!(table.translate("1A"), "⠼⠁⠠⠰⠁");
    }

    #[test]
    fn nocontractsign_comes_before_capsletter() {
        // UEB writes a capitalized contraction standing alone as ⠰⠠⠃, grade 1
        // indicator first
        let rules = [
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 12"),
            parse_rule("base uppercase A a"),
            parse_rule("base uppercase B b"),
            parse_rule("capsletter 6"),
            parse_rule("nocontractsign 56"),
            parse_rule("contraction b"),
            parse_rule("contraction ab"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("b"), "⠰⠃");
        assert_eq!(table.translate("B"), "⠰⠠⠃");
        assert_eq!(table.translate("Ab"), "⠰⠠⠁⠃");
    }

    #[test]
    fn nocross() {
        let rules = vec![
            parse_rule("include dictionaries/nocross-test.dic"),
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
        let rules = expand_includes(rules, &fixture_search_path(), &[]).unwrap();
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
            parse_rule("include dictionaries/nocross-test.dic"),
            parse_rule("lowercase f 124"),
            parse_rule("nocross always fff 456"),
            parse_rule("space \\s 0"),
        ];
        let rules = expand_includes(rules, &fixture_search_path(), &[]).unwrap();
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("fff"), "⠸");
    }

    #[test]
    fn joinword_joins_the_word_that_follows() {
        let rules = [
            parse_rule("lowercase t 2345"),
            parse_rule("lowercase o 135"),
            parse_rule("lowercase b 12"),
            parse_rule("digit 1 1"),
            parse_rule("punctuation . 256"),
            parse_rule("space \\s 0"),
            parse_rule("joinword to 235"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("to  b"), "⠖⠃");
        // no word follows
        assert_eq!(table.translate("to"), "⠞⠕");
        assert_eq!(table.translate("to. b"), "⠞⠕⠲⠀⠃");
        assert_eq!(table.translate("to 1"), "⠞⠕⠀⠁");
    }

    #[test]
    fn lowword_needs_whitespace_on_both_sides() {
        let rules = [
            parse_rule("lowercase b 12"),
            parse_rule("lowercase e 15"),
            parse_rule("punctuation . 256"),
            parse_rule("punctuation ( 2356"),
            parse_rule("space \\s 0"),
            parse_rule("lowword be 23"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("be"), "⠆");
        assert_eq!(table.translate("e be e"), "⠑⠀⠆⠀⠑");
        assert_eq!(table.translate("be."), "⠃⠑⠲");
        assert_eq!(table.translate("(be"), "⠶⠃⠑");
    }

    #[test]
    fn zero_length_rule_fires_again_after_moving_on() {
        // a zero-length rule must not stay blocked at the positions after a `nocross`
        // match or a character definition
        let rules = vec![
            parse_rule("include dictionaries/nocross-test.dic"),
            parse_rule("digit 0 245"),
            parse_rule("lowercase f 124"),
            parse_rule("nocross always fff 456"),
            parse_rule("context $d[]$l @6"),
        ];
        let rules = expand_includes(rules, &fixture_search_path(), &[]).unwrap();
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("0fff0fff"), "⠚⠠⠸⠚⠠⠸");
        assert_eq!(table.translate("0f0f"), "⠚⠠⠋⠚⠠⠋");
    }

    #[test]
    fn nocross_precedence_follows_word_hyphenation() {
        // da-dk-g28.ctb defines `nocross always er` and `nocross partword re` with no
        // plain counterparts. For the word "ere" the real hyphenation break falls
        // between 'e' and 'r' ("e-re"), which disqualifies `er` (it would cross that
        // break) but not `re` (fully inside the second syllable) -- so `re` must win,
        // even though `er` starts earlier in the word.
        let rules = vec![
            parse_rule("include dictionaries/nocross-precedence-test.dic"),
            parse_rule("lowercase e 1"),
            parse_rule("lowercase r 12"),
            parse_rule("nocross always er 6"),
            parse_rule("nocross partword re 56"),
        ];
        let rules = expand_includes(rules, &fixture_search_path(), &[]).unwrap();
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        // literal 'e' (⠁) + the `re` contraction (⠰), not the `er` contraction (⠠) + 'e'
        assert_eq!(table.translate("ere"), "⠁⠰");
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
    fn indication_not_reemitted_at_zero_length_candidate() {
        // A zero-length `context` action (e.g. liblouis' `$d[]$l`, used to insert a
        // letsign between a digit and a single trailing letter) legitimately overlaps
        // with the letsign indicator that also fires there, producing two signs — a
        // table author relies on a later pass rule to collapse that expected overlap
        // (see da-dk-g28.ctb's "Remove any superfluous letsigns" pass3 rule). What must
        // not happen is a *third* sign: the translation loop revisits this position
        // without advancing (the context action is zero-length), and the indicator
        // must not be re-emitted on that second visit.
        let rules = [
            parse_rule("digit 0 1"),
            parse_rule("lowercase i 24"),
            parse_rule("letsign 6"),
            parse_rule("context $d[]$l @6"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("0i"), "⠁⠠⠠⠊");
    }

    #[test]
    fn backward_drops_emphasis_indicators() {
        // the indicators share their dots with `,`, `;` and `'`, as in UEB
        let rules = [
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 12"),
            parse_rule("punctuation , 2"),
            parse_rule("punctuation ; 23"),
            parse_rule("punctuation ' 3"),
            parse_rule("emphclass italic"),
            parse_rule("emphletter italic 46-23"),
            parse_rule("begemphword italic 46-2"),
            parse_rule("endemphword italic 46-3"),
            // as in the Danish 1993 tables, where ⠰ is also "¨"
            parse_rule("punctuation ¨ 56"),
            parse_rule("begemphphrase italic 56"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table = PrimaryTable::compile(
            &rules,
            Direction::Backward,
            TranslationStage::Main,
            &context,
        )
        .unwrap();
        assert_eq!(table.translate("⠁⠨⠆⠃"), "ab");
        assert_eq!(table.translate("⠨⠂⠁⠃⠨⠄"), "ab");
        assert_eq!(table.translate("⠁⠂⠃⠆"), "a,b;");
        assert_eq!(table.translate("⠰"), "¨");
        assert_eq!(table.translate("⠰⠁⠃"), "ab");
    }

    #[test]
    fn context_translates_the_capture() {
        // `*` puts in the dots of the characters between `[` and `]`, not the characters
        let rules = [
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 12"),
            parse_rule("context [\"ab\"] *@3"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table =
            PrimaryTable::compile(&rules, Direction::Forward, TranslationStage::Main, &context)
                .unwrap();
        assert_eq!(table.translate("ab"), "⠁⠃⠄");
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
    fn after_math_checks_the_math_class() {
        // as in pt-pt-g1.utb, where ⠷ is "[" only after a math character
        let rules = [
            parse_rule("lowercase a 1"),
            parse_rule("math + 235"),
            parse_rule("lowercase á 12356"),
            parse_rule("after math always [ 12356"),
        ];
        let context = TableContext::compile(&rules).unwrap();
        let table = PrimaryTable::compile(
            &rules,
            Direction::Backward,
            TranslationStage::Main,
            &context,
        )
        .unwrap();
        assert_eq!(table.translate("⠁⠷"), "aá");
        assert_eq!(table.translate("⠖⠷"), "+[");
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
    fn backward_number_continues_across_numeric_mode_chars() {
        // as in it-it-comp6.utb: "." has a cell of its own as a decimal point
        let rules = [
            parse_rule("lowercase a 1"),
            parse_rule("lowercase e 15"),
            parse_rule("litdigit 1 1"),
            parse_rule("litdigit 5 15"),
            parse_rule("punctuation , 2"),
            parse_rule("punctuation . 256"),
            parse_rule("decpoint , 2"),
            parse_rule("decpoint . 3"),
            parse_rule("numericmodechars .,"),
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
        assert_eq!(table.translate("⠼⠁⠂⠑"), "1,5");
        assert_eq!(table.translate("⠼⠁⠄⠑"), "1.5");
        assert_eq!(table.translate("⠁⠂⠑"), "a,e");
    }

    #[test]
    fn backward_endnum_ends_a_number_unless_a_digit_follows() {
        // as in bg.utb, which has both rules for each operator
        let rules = [
            parse_rule("space \\s 0"),
            parse_rule("lowercase a 1"),
            parse_rule("lowercase b 12"),
            parse_rule("litdigit 1 1"),
            parse_rule("litdigit 2 12"),
            parse_rule("sign + 235"),
            parse_rule("midendnumericmodechars +"),
            parse_rule("midnum + 235"),
            parse_rule("endnum + 235"),
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
        assert_eq!(table.translate("⠼⠁⠖⠃"), "1+2");
        assert_eq!(table.translate("⠼⠁⠖⠀⠃"), "1+ b");
    }

    #[test]
    fn backward_digit_sharing_a_letter_cell_reads_as_the_letter() {
        // as in hu-hu-g1.ctb, where `digit 1 16` comes after `lowercase é 16`: liblouis
        // only prefers a `litdigit` inside a number
        let rules = [
            parse_rule("lowercase a 1"),
            parse_rule("lowercase é 16"),
            parse_rule("lowercase t 2345"),
            parse_rule("litdigit 1 1"),
            parse_rule("digit 1 16"),
            parse_rule("hyphen - 36"),
            parse_rule("midendnumericmodechars -"),
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
        assert_eq!(table.translate("⠼⠁⠤⠡⠞"), "1-ét");
        assert_eq!(table.translate("⠼⠁⠤⠁"), "1-1");
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
