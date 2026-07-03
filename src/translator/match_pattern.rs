//! Store and find match rules using a simple regexp engine ([`Regexp`](crate::translator::regexp))

use std::collections::HashSet;

use crate::parser::{AnchoredRule, Attribute, CharacterClasses, Pattern, Patterns};

use crate::translator::effect::Environment;
use crate::translator::regexp::{CompiledRegexp, Regexp};
use crate::translator::translation::Translation;
use crate::translator::{ResolvedTranslation, TranslationStage};

/// How the `^`/`$` boundary marker (standalone [`Pattern::Boundary`] or the `^` attribute inside
/// a `%[...]` set) should be compiled.
///
/// liblouis' matcher is bidirectional: `pre` scans backward from the cursor over
/// already-translated text, `post` scans forward over the upcoming text. A boundary check
/// succeeds when there is no more input left *in whichever direction is currently scanning*.
/// Our regexp engine only scans forward, which is not a problem for `post` — the text handed to
/// it always already extends to the true end of the whole input, so "no more input" can be
/// checked from inside the regexp itself ([`Regexp::EndAnchor`]). For `pre`, "no more input"
/// means "the whole match begins at the true start of the original input", which the regexp
/// can't determine from its own position — the caller has to know this externally. Since it
/// can't vary per-position within one compiled regexp, we instead compile `pre` twice, once
/// assuming it does and once assuming it doesn't, and the caller picks the right one per call
/// (see [`MatchPatterns::find`]).
#[derive(Clone, Copy)]
enum BoundaryMode {
    /// Compiling `post`: checked dynamically via [`Regexp::EndAnchor`].
    Dynamic,
    /// Compiling `pre` for the variant queried when the caller confirms we are at the true
    /// start of the whole original input, so there is definitely no preceding character: a
    /// boundary check always succeeds (zero-width), and any check that requires a real
    /// preceding character always fails.
    AssumeAtStart,
    /// Compiling `pre` for the variant queried otherwise. Matches the previous behavior: the
    /// boundary marker contributes nothing on its own (we can't tell from inside the regexp
    /// whether we're at the true start), other attributes in the same set are unaffected.
    Otherwise,
}

impl Regexp {
    fn from_pattern(item: &Pattern, ctx: &CharacterClasses, boundary: BoundaryMode) -> Self {
        match item {
            Pattern::Empty => Regexp::Empty,
            // `Characters`/`Set`/`Any` (below) don't consult `boundary`, unlike `Boundary`/
            // `Attributes`: under `AssumeAtStart` they should, per liblouis, unconditionally fail
            // (there's provably no preceding character to check them against), but no shipped
            // liblouis table combines a literal/set/any `pre` element with a boundary check, so
            // this is left as a known, currently-dormant gap rather than generalized.
            Pattern::Characters(s) => Regexp::String(s.to_string()),
            Pattern::Boundary => match boundary {
                BoundaryMode::Dynamic => Regexp::EndAnchor,
                BoundaryMode::AssumeAtStart => Regexp::Empty,
                BoundaryMode::Otherwise => Regexp::Never,
            },
            Pattern::Any => Regexp::Any,
            Pattern::Set(hash_set) => Regexp::CharacterClass(hash_set.clone()),
            Pattern::Attributes(hash_set) => Regexp::from_attributes(hash_set, ctx, boundary),
            Pattern::Group(patterns) => {
                Regexp::Group(Box::new(Regexp::from_patterns(patterns, ctx, boundary)))
            }
            Pattern::Negate(pattern) => Regexp::from_pattern(pattern, ctx, boundary).negate(),
            Pattern::Optional(pattern) => {
                Regexp::Optional(Box::new(Regexp::from_pattern(pattern, ctx, boundary)))
            }
            Pattern::ZeroOrMore(pattern) => {
                Regexp::ZeroOrMore(Box::new(Regexp::from_pattern(pattern, ctx, boundary)))
            }
            Pattern::OneOrMore(pattern) => {
                Regexp::OneOrMore(Box::new(Regexp::from_pattern(pattern, ctx, boundary)))
            }
            Pattern::Either(left, right) => Regexp::Either(
                Box::new(Regexp::from_patterns(left, ctx, boundary)),
                Box::new(Regexp::from_patterns(right, ctx, boundary)),
            ),
        }
    }

    fn from_patterns(patterns: &Patterns, ctx: &CharacterClasses, boundary: BoundaryMode) -> Self {
        match patterns.len() {
            0 => todo!(),
            1 => Regexp::from_pattern(&patterns[0], ctx, boundary),
            _ => {
                let mut regexp = Regexp::from_pattern(&patterns[0], ctx, boundary);
                for pattern in patterns.iter().skip(1) {
                    let other = Regexp::from_pattern(pattern, ctx, boundary);
                    regexp = Regexp::Concat(Box::new(regexp), Box::new(other));
                }
                regexp
            }
        }
    }

    fn from_attributes(
        attributes: &HashSet<Attribute>,
        ctx: &CharacterClasses,
        boundary: BoundaryMode,
    ) -> Self {
        let mut characters = HashSet::new();
        let mut has_boundary = false;
        for attr in attributes {
            match attr {
                Attribute::Class(class) => {
                    if let Some(chars) = ctx.get(class) {
                        characters.extend(chars);
                    }
                }
                Attribute::Boundary => has_boundary = true,
                // attributes that match a character class by order of definition luckily do not
                // exist in match regular expressions
                Attribute::ByOrder(_) => unreachable!(),
                Attribute::Any => (), // TODO
            }
        }
        match (boundary, has_boundary) {
            (BoundaryMode::Dynamic, true) => Regexp::Either(
                Box::new(Regexp::CharacterClass(characters)),
                Box::new(Regexp::EndAnchor),
            ),
            (BoundaryMode::AssumeAtStart, true) => Regexp::Empty,
            (BoundaryMode::AssumeAtStart, false) => Regexp::Never,
            (BoundaryMode::Dynamic, false) | (BoundaryMode::Otherwise, _) => {
                Regexp::CharacterClass(characters)
            }
        }
    }

    /// Combine the pre and post patterns with the match characters into one big Regexp by joining
    /// them with concat. `pre_boundary` selects which of the two `pre` variants to compile (see
    /// [`BoundaryMode`]); `post` always uses [`BoundaryMode::Dynamic`] since its boundary checks
    /// don't need a variant.
    fn from_match_rule(
        pre: &Patterns,
        chars: String,
        post: &Patterns,
        ctx: &CharacterClasses,
        pre_boundary: BoundaryMode,
    ) -> Self {
        Regexp::Concat(
            Box::new(Regexp::Concat(
                Box::new(Regexp::from_patterns(pre, ctx, pre_boundary)),
                Box::new(Regexp::Capture(Box::new(Regexp::String(chars)))),
            )),
            Box::new(Regexp::from_patterns(post, ctx, BoundaryMode::Dynamic)),
        )
    }
}

/// A match rule compiled twice: once assuming `pre` is being matched at the true start of the
/// whole original input, once assuming it isn't. See [`BoundaryMode`] for why.
///
/// This looks like it duplicates `context_pattern.rs`'s `AnchoredContextRegexp` (also
/// start-anchor-dependent), but that one only needs a bool gate on a single compiled regexp,
/// not two full variants — see the comment on that struct for why the two can't be unified.
#[derive(Debug)]
struct AnchoredMatchRegexp {
    at_start: CompiledRegexp,
    otherwise: CompiledRegexp,
}

#[derive(Debug)]
pub struct MatchPatternsBuilder {
    regexps: Vec<AnchoredMatchRegexp>,
}

impl MatchPatternsBuilder {
    pub fn new() -> Self {
        Self {
            regexps: Vec::new(),
        }
    }

    pub fn insert(
        &mut self,
        pre: &Patterns,
        chars: &str,
        post: &Patterns,
        to: &str,
        origin: &AnchoredRule,
        ctx: &CharacterClasses,
    ) {
        let translation = Translation::Resolved(ResolvedTranslation::new(
            chars,
            to,
            0,
            TranslationStage::Main,
            origin.clone(),
        ));
        let at_start = Regexp::from_match_rule(
            pre,
            chars.to_string(),
            post,
            ctx,
            BoundaryMode::AssumeAtStart,
        )
        .compile_with_payload(translation.clone());
        let otherwise =
            Regexp::from_match_rule(pre, chars.to_string(), post, ctx, BoundaryMode::Otherwise)
                .compile_with_payload(translation);
        self.regexps.push(AnchoredMatchRegexp {
            at_start,
            otherwise,
        });
    }

    pub fn build(self) -> MatchPatterns {
        MatchPatterns {
            regexps: self.regexps,
        }
    }
}

#[derive(Debug)]
pub struct MatchPatterns {
    regexps: Vec<AnchoredMatchRegexp>,
}

impl MatchPatterns {
    /// `at_start` tells whether `input` begins at position 0 of the whole string being
    /// translated — needed to pick the right compiled variant for `pre`'s boundary checks (see
    /// [`BoundaryMode`]).
    pub fn find(&self, input: &str, at_start: bool) -> Vec<ResolvedTranslation> {
        self.regexps
            .iter()
            .flat_map(|r| {
                let re = if at_start { &r.at_start } else { &r.otherwise };
                re.find(input, &Environment::new())
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::RuleParser;
    use crate::parser::{CharacterClass, PatternParser};
    use crate::translator::TranslationStage;

    // just create some fake anchored rule for testing purposes
    fn fake_rule() -> AnchoredRule {
        let rule = RuleParser::new("always foo 1").rule().unwrap();
        AnchoredRule::new(rule, None, 0)
    }

    #[test]
    fn find_pattern() {
        let env = Environment::new();
        let patterns = PatternParser::new("abc").pattern().unwrap();
        let stage = TranslationStage::Main;
        let ctx = CharacterClasses::default();
        let re = Regexp::from_patterns(&patterns, &ctx, BoundaryMode::Dynamic).compile();
        assert_eq!(
            re.find("abc", &env).unwrap(),
            ResolvedTranslation::new("", "", 3, stage, None)
        );
        assert_eq!(re.find("def", &env), None);
    }

    #[test]
    fn find_either() {
        let env = Environment::new();
        let patterns = PatternParser::new("a|b").pattern().unwrap();
        let stage = TranslationStage::Main;
        let ctx = CharacterClasses::default();
        let re = Regexp::from_patterns(&patterns, &ctx, BoundaryMode::Dynamic).compile();
        assert_eq!(
            re.find("a", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None)
        );
        assert_eq!(
            re.find("b", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None)
        );
        assert_eq!(re.find("c", &env), None);
        assert_eq!(re.find("def", &env), None);
    }

    #[test]
    fn find_attribute_digit() {
        let env = Environment::new();
        let patterns = PatternParser::new("%[#]").pattern().unwrap();
        let stage = TranslationStage::Main;
        let mut ctx = CharacterClasses::default();
        for digit in ['1', '2', '3'] {
            ctx.insert(CharacterClass::Digit, digit);
        }
        let re = Regexp::from_patterns(&patterns, &ctx, BoundaryMode::Dynamic).compile();
        assert_eq!(
            re.find("1", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None)
        );
        assert_eq!(
            re.find("2", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None)
        );
        assert_eq!(
            re.find("3", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None)
        );
        assert_eq!(re.find("def", &env), None);
    }

    #[test]
    fn find_attribute_uppercase() {
        let env = Environment::new();
        let patterns = PatternParser::new("%[u]").pattern().unwrap();
        let stage = TranslationStage::Main;
        let mut ctx = CharacterClasses::default();
        for c in ['A', 'B', 'C'] {
            ctx.insert(CharacterClass::Uppercase, c);
        }
        let re = Regexp::from_patterns(&patterns, &ctx, BoundaryMode::Dynamic).compile();
        assert_eq!(
            re.find("A", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None)
        );
        assert_eq!(
            re.find("A", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None)
        );
        assert_eq!(
            re.find("C", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None)
        );
        assert_eq!(re.find("def", &env), None);
    }

    #[test]
    fn find_attribute_uppercase_punctuation_or_sign() {
        let env = Environment::new();
        let patterns = PatternParser::new("%[.$u]").pattern().unwrap();
        let stage = TranslationStage::Main;
        let mut ctx = CharacterClasses::default();
        for c in ['A', 'B', 'C'] {
            ctx.insert(CharacterClass::Uppercase, c);
        }
        for c in ['.', ',', '!'] {
            ctx.insert(CharacterClass::Punctuation, c);
        }
        for c in ['%', '&', '/'] {
            ctx.insert(CharacterClass::Sign, c);
        }
        let re = Regexp::from_patterns(&patterns, &ctx, BoundaryMode::Dynamic).compile();
        assert_eq!(
            re.find("%", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None)
        );
        assert_eq!(
            re.find(".", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None)
        );
        assert_eq!(
            re.find("A", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None)
        );
        assert_eq!(re.find("def", &env), None);
    }

    #[test]
    fn find_character_class() {
        let env = Environment::new();
        let patterns = PatternParser::new("[abc]").pattern().unwrap();
        let stage = TranslationStage::Main;
        let ctx = CharacterClasses::default();
        let re = Regexp::from_patterns(&patterns, &ctx, BoundaryMode::Dynamic).compile();
        assert_eq!(
            re.find("a", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None)
        );
        assert_eq!(
            re.find("b", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None)
        );
        assert_eq!(
            re.find("c", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None)
        );
        assert_eq!(re.find("def", &env), None);
    }

    #[test]
    fn find_character_class_one_or_more() {
        let env = Environment::new();
        let patterns = PatternParser::new("[abc]+").pattern().unwrap();
        let stage = TranslationStage::Main;
        let ctx = CharacterClasses::default();
        let re = Regexp::from_patterns(&patterns, &ctx, BoundaryMode::Dynamic).compile();
        assert_eq!(
            re.find("a", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None)
        );
        assert_eq!(
            re.find("b", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None)
        );
        assert_eq!(
            re.find("c", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None)
        );
        assert_eq!(re.find("def", &env), None);
    }

    #[test]
    fn find_match() {
        let pre = PatternParser::new("[abc]+").pattern().unwrap();
        let post = PatternParser::new("[123]+").pattern().unwrap();
        let ctx = CharacterClasses::default();
        let rule = fake_rule();
        let mut builder = MatchPatternsBuilder::new();
        builder.insert(&pre, "foo", &post, "", &rule, &ctx);
        let translation =
            ResolvedTranslation::new("foo", "", 5, TranslationStage::Main, rule.clone())
                .with_offset(1);
        let matcher = builder.build();
        assert_eq!(matcher.find("afoo1", true), vec![translation.clone()]);
        assert_eq!(matcher.find("bfoo2", true), vec![translation.clone()]);
        let translations = vec![
            ResolvedTranslation::new("foo", "", 9, TranslationStage::Main, rule.clone())
                .with_offset(3),
        ];
        assert_eq!(matcher.find("cccfoo333", true), translations);
        assert!(matcher.find("def", true).is_empty());
    }

    #[test]
    fn find_multiple_match() {
        let pre = PatternParser::new("[abc]+").pattern().unwrap();
        let post = PatternParser::new("[1234567890]").pattern().unwrap();
        let ctx = CharacterClasses::default();
        let rule = fake_rule();
        let mut builder = MatchPatternsBuilder::new();
        builder.insert(&pre, "foo", &post, "FOO", &rule, &ctx);
        builder.insert(&pre, "bar", &post, "BAR", &rule, &ctx);
        let translation = vec![
            ResolvedTranslation::new("foo", "FOO", 7, TranslationStage::Main, rule.clone())
                .with_offset(3),
        ];
        let matcher = builder.build();
        assert_eq!(matcher.find("aaafoo333", true), translation);
        let translation = vec![
            ResolvedTranslation::new("bar", "BAR", 7, TranslationStage::Main, rule.clone())
                .with_offset(3),
        ];
        assert_eq!(matcher.find("aaabar333", true), translation);
        assert_ne!(matcher.find("aaabaz333", true), translation);
    }
}
