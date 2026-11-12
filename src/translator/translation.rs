use std::cmp::Reverse;

use crate::{
    Direction,
    parser::{AnchoredRule, HasNocross, HasPrecedence, Precedence, Rule},
    translator::{CharacterDefinition, effect::Effect, regexp::CharacterRange, swap::Swapper},
};

/// A translation can have multiple stages.
///
/// The `Main` translation is always done. The others are only done if
/// specific rules are present in a translation table
#[derive(Debug, PartialEq, Clone, Default, Copy)]
pub enum TranslationStage {
    /// Pre-translation stage where the `correct` rules are applied
    Pre,
    /// Main translation stage where all the translation and the
    /// context rules are applied
    #[default]
    Main,
    /// The second post-translation stage where the `pass2` rules are applied
    Post1,
    /// The third post-translation stage where the `pass3` rules are applied
    Post2,
    /// The fourth post-translation stage where the `pass4` rules are applied
    Post3,
    /// Mapping between braille cells and the characters a display table shows them as.
    /// Applied to the braille side: after every other stage when translating to braille,
    /// before every other stage when translating from it.
    Display,
}

impl std::fmt::Display for TranslationStage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TranslationStage::Pre => write!(f, "Pre"),
            TranslationStage::Main => write!(f, "Main"),
            TranslationStage::Post1 => write!(f, "Pass2"),
            TranslationStage::Post2 => write!(f, "Pass3"),
            TranslationStage::Post3 => write!(f, "Pass4"),
            TranslationStage::Display => write!(f, "Display"),
        }
    }
}

/// Wrapper type to handle different kinds of translation targets.
///
/// In most case a translation just translates to a literal string. However in some cases the output
/// depends on a capture from the input or even some modified (_swapped_) version of the capture
#[derive(Debug, PartialEq, Clone)]
pub enum TranslationTarget {
    /// Translate to a literal string
    Literal(String),
    /// Translate to the result of a capture
    Capture,
    /// Translate to the result of a capture but also apply the mapping defined in the [`Swapper`]
    Swap(Swapper),
    /// Translate the captured characters one by one by their character definitions, without
    /// any other rule or indicator
    TranslateCapture,
}

impl std::fmt::Display for TranslationTarget {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TranslationTarget::Literal(s) => write!(f, "{}", s),
            t => unreachable!(
                "to_string() should never be invoked on an unresolved TranslationTarget {:?}",
                t
            ),
        }
    }
}

impl TranslationTarget {
    pub fn resolve(self, captured: &str, fallback_definitions: &CharacterDefinition) -> Self {
        match self {
            TranslationTarget::Literal(_) => self,
            TranslationTarget::Capture => TranslationTarget::Literal(captured.to_string()),
            TranslationTarget::Swap(swapper) => TranslationTarget::Literal(swapper.swap(captured)),
            // FIXME: a character without a definition stays as it is, where liblouis's
            // putCharacter uses the `undefined` rule or the `\x…/` escape
            TranslationTarget::TranslateCapture => TranslationTarget::Literal(
                captured
                    .chars()
                    .map(|c| {
                        fallback_definitions
                            .get(&c)
                            .cloned()
                            .unwrap_or_else(|| c.to_string())
                    })
                    .collect(),
            ),
        }
    }
}

/// A sequence of [`TranslationTarget`]
#[derive(Debug, Clone, PartialEq, Default)]
pub struct TranslationTargets(Vec<TranslationTarget>);

impl std::fmt::Display for TranslationTargets {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: String = self.0.iter().map(|t| t.to_string()).collect();
        write!(f, "{}", s)
    }
}

pub trait Resolve {
    /// `fallback_definitions` are the table's character definitions, for
    /// [`TranslationTarget::TranslateCapture`]
    fn resolve(
        self,
        captured: &str,
        consumed: CharacterRange,
        weight: usize,
        fallback_definitions: &CharacterDefinition,
    ) -> ResolvedTranslation;
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnresolvedTranslation {
    output: Vec<TranslationTarget>,
    stage: TranslationStage,
    effects: Vec<Effect>,
    origin: Option<AnchoredRule>,
}

impl UnresolvedTranslation {
    pub fn new(
        output: &[TranslationTarget],
        stage: TranslationStage,
        effects: &[Effect],
        origin: impl Into<Option<AnchoredRule>>,
    ) -> Self {
        Self {
            output: output.to_vec(),
            stage,
            effects: effects.to_vec(),
            origin: origin.into(),
        }
    }
}

impl Resolve for UnresolvedTranslation {
    fn resolve(
        self,
        captured: &str,
        consumed: CharacterRange,
        weight: usize,
        fallback_definitions: &CharacterDefinition,
    ) -> ResolvedTranslation {
        let resolved: String = self
            .output
            .iter()
            .cloned()
            .map(|t| t.resolve(captured, fallback_definitions))
            .map(|t| t.to_string())
            .collect();
        ResolvedTranslation {
            input: captured.to_string(),
            output: resolved,
            length: consumed.len(),
            weight,
            offset: consumed.start,
            stage: self.stage,
            effects: self.effects,
            origin: self.origin,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Translation {
    Resolved(ResolvedTranslation),
    Unresolved(UnresolvedTranslation),
}

impl Default for Translation {
    fn default() -> Self {
        Translation::Resolved(ResolvedTranslation::default())
    }
}

impl Resolve for Translation {
    fn resolve(
        self,
        captured: &str,
        consumed: CharacterRange,
        weight: usize,
        fallback_definitions: &CharacterDefinition,
    ) -> ResolvedTranslation {
        match self {
            Translation::Resolved(translation) => translation
                .with_weight(weight)
                .with_length(consumed.len())
                .with_offset(consumed.start),
            Translation::Unresolved(unresolved) => {
                unresolved.resolve(captured, consumed, weight, fallback_definitions)
            }
        }
    }
}

/// The key used to pick between competing translation candidates at the same position: the
/// highest wins. The fields are compared in the order they are declared, so that order is the
/// order of precedence; each one only matters when all the fields above it tie.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Rank {
    /// A forward main-stage `context` rule whose test starts with no literal characters wins
    /// over everything else. liblouis files rules under their leading characters; having
    /// none, these go on a separate list, which it checks after choosing a rule from its
    /// chains, and the first one that matches overrides that choice (`findForPassRule`).
    /// An artifact of liblouis's indexing rather than a priority anyone chose, but tables
    /// depend on it.
    context_no_leading_literal: bool,
    /// A zero-length candidate, e.g. a `context` or `match` rule that only emits an
    /// indicator, is not competing for the same input at all: it consumes nothing and lets
    /// the next iteration pick the rule that does. Ranking it by characters consumed would
    /// mean it could never win, so it goes ahead of every consuming candidate instead.
    zero_length: bool,
    /// Characters consumed: a longer match always beats a shorter one. For a forward
    /// `context` rule, the characters liblouis files it under (`Test::leading_literal_len`).
    length: usize,
    /// Pass and `correct` rules: how many characters of the test are spelled out literally
    /// rather than given by a class, so `@123` beats `$a` on the same cell. liblouis sorts
    /// pass rules by their leading literals alone and takes the first match, never comparing
    /// the input consumed; here [`length`](Rank::length) still comes first.
    literals: usize,
    /// The kind of rule: a translation rule beats `always`, which beats an indicator, which
    /// beats a character definition. On a one-character match `always` counts as a
    /// translation rule, as liblouis only puts it last in its multi-character chains.
    precedence: Precedence,
    /// Backward main stage only: the rule producing more text wins, as liblouis sorts its
    /// backward rule chains by output length. Forward and in the passes liblouis does not look
    /// at the output, and this is 0.
    output_length: usize,
    /// A `nocross` rule beats its plain twin: it only applies when no syllable break falls
    /// inside the match, so it is the more specific of the two.
    nocross: bool,
    /// Between two forward `context` rules only, how much of the input their tests checked,
    /// the longer test winning. liblouis has no such measure: it takes the first rule in its
    /// chain that applies, and tries a `context` rule's test where it starts. louis-rs gets
    /// to such a rule only once the cursor reaches its `[`, and this stands in for having
    /// tried it earlier. Every other rule has 0, so definition order decides between them.
    context_weight: usize,
    /// Definition order, the earliest-defined rule winning, as in liblouis. Only reached when
    /// nothing in the rules themselves separates the candidates, and it keeps the outcome
    /// deterministic and compatible with liblouis.
    definition_order: Reverse<usize>,
}

/// The basic unit of a translation.
///
/// Maps an `input` string to an `output` which is typically also a string.
#[derive(Debug, PartialEq, Clone, Default)]
pub struct ResolvedTranslation {
    /// Input string to be translated
    input: String,
    /// The translation of `input`, typically Unicode braille. In the case of back-translation the
    /// `input` contains Unicode braille and the output plain text.
    output: String,
    /// Number of matched chars in `input`
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
    /// The stage in which this translation is applied
    stage: TranslationStage,
    /// A possibly empty list of [`Effect`]s. These will be applied to the
    /// [`Environment`](crate::translator::effect::Environment) if this Translation is used
    effects: Vec<Effect>,
    /// Which translation rule was the cause for this translation
    origin: Option<AnchoredRule>,
}

/// A subset of translation for which [`Hash`] and [`Eq`] is implemented
#[derive(Debug, PartialEq, Eq, Clone, Default, Hash)]
pub struct TranslationSubset {
    input: String,
    output: String,
    length: usize,
    weight: usize,
}

impl From<&ResolvedTranslation> for TranslationSubset {
    fn from(translation: &ResolvedTranslation) -> Self {
        Self {
            input: translation.input(),
            output: translation.output(),
            length: translation.length(),
            weight: translation.weight(),
        }
    }
}

impl ResolvedTranslation {
    /// An indication: dots from an indicator `rule`, such as a `capsletter` or `numsign`,
    /// that consume no input.
    pub fn indication(dots: &str, rule: &AnchoredRule) -> Self {
        Self::new("", dots, 1, TranslationStage::Main, rule.clone())
    }

    pub fn new(
        input: &str,
        output: &str,
        weight: usize,
        stage: TranslationStage,
        // FIXME: this is some weird thing recommended by Claude: apparently the `impl
        // Into<Option<T>>` trait bound automatically converts `T` to `Some(T)` and `None` to
        // `None`, giving you the overloaded behavior you want with a single function. This is more
        // idiomatic than having separate `new` and `new_with_origin` methods.
        origin: impl Into<Option<AnchoredRule>>,
    ) -> Self {
        Self {
            input: input.to_string(),
            output: output.to_string(),
            length: input.chars().count(),
            weight,
            offset: 0,
            stage,
            effects: Vec::default(),
            origin: origin.into(),
        }
    }

    pub fn input(&self) -> String {
        self.input.clone()
    }

    pub fn output(&self) -> String {
        self.output.to_string()
    }

    pub fn length(&self) -> usize {
        self.length
    }

    pub fn weight(&self) -> usize {
        self.weight
    }

    /// This candidate's [`Rank`] when translating in `direction`.
    pub fn rank(&self, direction: Direction) -> Rank {
        // a translation without a rule, such as the fallback for an undefined character,
        // ranks lowest
        let (literals, precedence, definition_order) = match &self.origin {
            Some(o) => (o.rule.literal_count(), o.precedence(), o.definition_order()),
            None => (0, Precedence::CharacterDefinition, usize::MAX),
        };
        // A main-stage `context` rule with literal characters is in liblouis's rule chain
        // under those characters, and competes by their number, not by what it consumes.
        // Forward only: backward, liblouis chooses rules its own way (`back_selectRule`).
        let leading_literal_len = match (direction, self.stage, self.rule()) {
            (Direction::Forward, TranslationStage::Main, Some(Rule::Context { test, .. })) => {
                Some(test.leading_literal_len())
            }
            _ => None,
        };
        Rank {
            context_no_leading_literal: leading_literal_len == Some(0),
            zero_length: self.length == 0,
            length: leading_literal_len
                .filter(|&n| n > 0)
                .unwrap_or(self.length),
            literals,
            precedence: match precedence {
                Precedence::Always if self.length < 2 => Precedence::Translation,
                p => p,
            },
            output_length: match (direction, self.stage, self.rule()) {
                // the space a `joinword` or `joinnum` puts back is not part of its characters
                (
                    Direction::Backward,
                    TranslationStage::Main,
                    Some(Rule::Joinword { chars, .. } | Rule::Joinnum { chars, .. }),
                ) => chars.chars().count(),
                (Direction::Backward, TranslationStage::Main, _) => self.output.chars().count(),
                _ => 0,
            },
            nocross: self.origin.as_ref().is_some_and(|o| o.is_nocross()),
            context_weight: if leading_literal_len.is_some() {
                self.weight
            } else {
                0
            },
            definition_order: Reverse(definition_order),
        }
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn origin(&self) -> Option<AnchoredRule> {
        self.origin.clone()
    }

    /// The rule this translation came from, borrowed. Prefer this over
    /// [`origin`](Self::origin) where only the rule is needed.
    pub fn rule(&self) -> Option<&Rule> {
        self.origin.as_ref().map(|anchored| &anchored.rule)
    }

    pub fn stage(&self) -> TranslationStage {
        self.stage
    }

    pub fn effects(&self) -> &Vec<Effect> {
        &self.effects
    }

    /// Set the `input` of a translation.
    pub fn with_input(self, input: &str) -> Self {
        Self {
            input: input.to_string(),
            length: input.chars().count(),
            ..self
        }
    }

    /// Set the `weight` of a translation.
    pub fn with_weight(self, weight: usize) -> Self {
        Self { weight, ..self }
    }

    /// Set the `length` of a translation.
    pub fn with_length(self, length: usize) -> Self {
        Self { length, ..self }
    }

    /// Set the `output` of a translation.
    pub fn with_output(self, output: &str) -> Self {
        Self {
            output: output.to_string(),
            ..self
        }
    }

    /// Set the `offset` of a translation.
    pub fn with_offset(self, offset: usize) -> Self {
        Self { offset, ..self }
    }

    /// Decrement the `offset` of a translation.
    pub fn decrement_offset(self, decrement: usize) -> Self {
        Self {
            offset: self.offset - decrement,
            ..self
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::translator::swap::Swapper;

    #[test]
    fn rank_breaks_ties_by_precedence_then_definition_order() {
        use crate::parser::RuleParser;
        let best = |sources: &[&str], input: &str| {
            let rules: Vec<_> = sources
                .iter()
                .map(|s| AnchoredRule::from(RuleParser::new(s).rule().unwrap()))
                .collect();
            crate::parser::in_definition_order(&rules)
                .into_iter()
                .map(|r| ResolvedTranslation::new(input, "x", 1, TranslationStage::Main, r))
                .max_by_key(|t| t.rank(Direction::Forward))
                .unwrap()
                .origin()
                .unwrap()
                .definition_order()
        };
        // a character definition is only the fallback
        assert_eq!(best(&["letter a 1", "always a 2"], "a"), 1);
        // on one character `always` is just another rule: the earliest-defined wins
        assert_eq!(best(&["always a 2", "word a 4"], "a"), 0);
        // on several, any other rule beats `always`
        assert_eq!(best(&["always ab 2", "word ab 4"], "ab"), 1);
        // among equals, the earliest-defined rule wins
        assert_eq!(best(&["always ab 2", "always ab 3"], "ab"), 0);
    }

    #[test]
    fn resolve_literal() {
        assert_eq!(
            TranslationTarget::Literal("foo".to_string())
                .resolve("bar", &CharacterDefinition::default()),
            TranslationTarget::Literal("foo".to_string())
        );
    }

    #[test]
    fn resolve_capture() {
        let resolved =
            TranslationTarget::Capture.resolve("CAPTURED", &CharacterDefinition::default());
        assert_eq!(resolved, TranslationTarget::Literal("CAPTURED".to_string()));
    }

    #[test]
    fn resolve_swap() {
        let swapper = Swapper::new(&[('a', "A"), ('b', "B"), ('c', "C")]);

        let resolved =
            TranslationTarget::Swap(swapper).resolve("abc", &CharacterDefinition::default());
        assert_eq!(resolved, TranslationTarget::Literal("ABC".to_string()));
    }

    #[test]
    fn resolve_translate_capture() {
        let mut definitions = CharacterDefinition::new();
        definitions.insert('a', "⠁");
        definitions.insert('b', "⠃");

        let resolved = TranslationTarget::TranslateCapture.resolve("abc", &definitions);
        // `c` has no definition and stays as it is
        assert_eq!(resolved, TranslationTarget::Literal("⠁⠃c".to_string()));
    }

    #[test]
    fn translation_capture_empty() {
        let translation = UnresolvedTranslation::new(
            &[TranslationTarget::Capture],
            TranslationStage::Main,
            &[],
            None,
        );
        let result = translation.resolve("", 0..0, 0, &CharacterDefinition::default());

        assert_eq!(result.input(), "");
        assert_eq!(result.output(), "");
    }

    #[test]
    fn translation_capture_literal() {
        let translation = UnresolvedTranslation::new(
            &[TranslationTarget::Capture],
            TranslationStage::Main,
            &[],
            None,
        );
        let result = translation.resolve("captured", 0..8, 0, &CharacterDefinition::default());

        assert_eq!(result.input(), "captured");
        assert_eq!(result.output(), "captured");
    }

    #[test]
    fn translation_capture_capture() {
        let translation = UnresolvedTranslation::new(
            &[
                TranslationTarget::Literal("<".to_string()),
                TranslationTarget::Capture,
                TranslationTarget::Literal(">".to_string()),
            ],
            TranslationStage::Main,
            &[],
            None,
        );
        let result = translation.resolve("MIDDLE", 0..8, 0, &CharacterDefinition::default());

        assert_eq!(result.input(), "MIDDLE");
        assert_eq!(result.output(), "<MIDDLE>");
    }

    #[test]
    fn translation_capture_swap() {
        let swapper = Swapper::new(&[('x', "X"), ('y', "Y")]);

        let translation = UnresolvedTranslation::new(
            &[
                TranslationTarget::Literal("<".to_string()),
                TranslationTarget::Swap(swapper),
                TranslationTarget::Literal(">".to_string()),
            ],
            TranslationStage::Main,
            &[],
            None,
        );
        let result = translation.resolve("xyz", 0..8, 0, &CharacterDefinition::default());

        assert_eq!(result.input(), "xyz");
        assert_eq!(result.output(), "<XYz>");
    }

    #[test]
    fn translation_capture_unicode() {
        let translation = UnresolvedTranslation::new(
            &[
                TranslationTarget::Literal("<".to_string()),
                TranslationTarget::Capture,
                TranslationTarget::Literal(">".to_string()),
            ],
            TranslationStage::Main,
            &[],
            None,
        );
        let result = translation.resolve("café🚀🚀", 0..6, 0, &CharacterDefinition::default());

        assert_eq!(result.input(), "café🚀🚀");
        assert_eq!(result.length(), 6);
    }
}
