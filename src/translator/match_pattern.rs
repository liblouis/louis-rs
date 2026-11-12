//! Store and find match rules using a simple regexp engine ([`Regexp`](crate::translator::regexp))

use std::collections::HashSet;

use crate::parser::{AnchoredRule, Attribute, CharacterClasses, Pattern, Patterns, Side};

use crate::translator::effect::Environment;
use crate::translator::regexp::{CompiledRegexp, Regexp};
use crate::translator::translation::Translation;
use crate::translator::{CharacterDefinition, ResolvedTranslation, TranslationStage};

/// The zero-width anchor liblouis' `^`/`$` boundary marker means on this side of a `match` rule
/// -- see [`Pattern::Boundary`].
fn boundary_anchor(side: Side) -> Regexp {
    match side {
        Side::Pre => Regexp::StartAnchor,
        Side::Post => Regexp::EndAnchor,
    }
}

impl Regexp {
    fn from_pattern(item: &Pattern, ctx: &CharacterClasses) -> Self {
        match item {
            Pattern::Empty => Regexp::Empty,
            Pattern::Characters(s) => Regexp::String(s.to_string()),
            Pattern::Boundary(side) => boundary_anchor(*side),
            Pattern::Any => Regexp::Any,
            Pattern::Set(hash_set) => Regexp::CharacterClass(hash_set.clone()),
            Pattern::Attributes(hash_set) => Regexp::from_attributes(hash_set, ctx),
            // `%[u^]`-style: one of the attributes, or the boundary -- see
            // `Pattern::AttributesOrBoundary` for why this decision belongs here, not in the
            // parser.
            Pattern::AttributesOrBoundary(hash_set, side) => Regexp::Either(
                Box::new(Regexp::from_attributes(hash_set, ctx)),
                Box::new(boundary_anchor(*side)),
            ),
            Pattern::Group(patterns) => {
                Regexp::Group(Box::new(Regexp::from_patterns(patterns, ctx)))
            }
            Pattern::Negate(pattern) => Regexp::from_pattern(pattern, ctx).negate(),
            Pattern::Optional(pattern) => {
                Regexp::Optional(Box::new(Regexp::from_pattern(pattern, ctx)))
            }
            Pattern::ZeroOrMore(pattern) => {
                Regexp::ZeroOrMore(Box::new(Regexp::from_pattern(pattern, ctx)))
            }
            Pattern::OneOrMore(pattern) => {
                Regexp::OneOrMore(Box::new(Regexp::from_pattern(pattern, ctx)))
            }
            Pattern::Either(left, right) => Regexp::Either(
                Box::new(Regexp::from_patterns(left, ctx)),
                Box::new(Regexp::from_patterns(right, ctx)),
            ),
        }
    }

    fn from_patterns(patterns: &Patterns, ctx: &CharacterClasses) -> Self {
        match patterns.len() {
            0 => todo!(),
            1 => Regexp::from_pattern(&patterns[0], ctx),
            _ => {
                let mut regexp = Regexp::from_pattern(&patterns[0], ctx);
                for pattern in patterns.iter().skip(1) {
                    let other = Regexp::from_pattern(pattern, ctx);
                    regexp = Regexp::Concat(Box::new(regexp), Box::new(other));
                }
                regexp
            }
        }
    }

    fn from_attributes(attributes: &HashSet<Attribute>, ctx: &CharacterClasses) -> Self {
        let mut characters = HashSet::new();
        for attr in attributes {
            match attr {
                Attribute::Class(class) => {
                    if let Some(chars) = ctx.get(class) {
                        characters.extend(chars);
                    }
                }
                // the parser (`match_rule.rs::attributes`) already pulls `Boundary` out of the
                // set and re-expresses it as a sibling `Pattern::Boundary`, so it never survives
                // into a `Pattern::Attributes` set
                Attribute::Boundary => unreachable!(),
                // attributes that match a character class by order of definition luckily do not
                // exist in match regular expressions
                Attribute::ByOrder(_) => unreachable!(),
                // attributes that match any character do not exist in match regular
                // expressions
                Attribute::Any => unreachable!(),
            }
        }
        Regexp::CharacterClass(characters)
    }

    /// Combine the match characters with the post pattern into one Regexp, to run forward
    /// from the cursor. `pre` is a separate, reversed program, see [`MatchPatterns::find`].
    ///
    /// `chars` is matched case-insensitively ([`Regexp::CaseInsensitiveString`]), unlike `pre`/
    /// `post`, which stay case-sensitive. This matches liblouis: a `match` rule's literal
    /// `chars` word is found the same case-insensitive way as every other opcode, so a rule
    /// written as `match ... little ...` also fires for "Little"/"LITTLE" — but `pre`/`post`
    /// context patterns are matched case-sensitively, which is why tables sometimes spell out
    /// both cases explicitly there (e.g. `[Aa][Tt]`) when they need to.
    fn from_match_rule(chars: String, post: &Patterns, ctx: &CharacterClasses) -> Self {
        Regexp::Concat(
            Box::new(Regexp::Consume(Box::new(Regexp::Capture(Box::new(
                Regexp::CaseInsensitiveString(chars),
            ))))),
            Box::new(Regexp::from_patterns(post, ctx)),
        )
    }
}

#[derive(Debug)]
pub struct MatchPatternsBuilder {
    regexps: Vec<MatchRegexp>,
}

/// A `match` rule as two programs: `pre` reversed, to run over the reversed text before the
/// cursor (`None` when there is no `pre`), and the characters followed by `post`, to run
/// from the cursor.
#[derive(Debug)]
struct MatchRegexp {
    pre: Option<CompiledRegexp>,
    main: CompiledRegexp,
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
        let main =
            Regexp::from_match_rule(chars.to_string(), post, ctx).compile_with_payload(translation);
        let pre = match Regexp::from_patterns(pre, ctx) {
            Regexp::Empty => None,
            pre => Some(pre.reverse().compile()),
        };
        self.regexps.push(MatchRegexp { pre, main });
    }

    pub fn build(self) -> MatchPatterns {
        MatchPatterns {
            regexps: self.regexps,
        }
    }
}

#[derive(Debug)]
pub struct MatchPatterns {
    regexps: Vec<MatchRegexp>,
}

impl MatchPatterns {
    /// The rules that match at the cursor: their characters and `post` match `input`, the
    /// text from the cursor on, and their `pre` matches `preceding`, the text before the
    /// cursor. `preceding` must be reversed, so that it reads backwards from the cursor.
    /// This is liblouis's look-behind, and as `pre`'s [`Pattern::Boundary`] is then the end
    /// of `preceding`, no flag for the start of the input is needed.
    pub fn find(&self, input: &str, preceding: &str) -> Vec<ResolvedTranslation> {
        let env = Environment::new();
        let definitions = CharacterDefinition::default();
        self.regexps
            .iter()
            .filter_map(|r| {
                let found = r.main.find_anchored(input, &env, false, &definitions)?;
                match &r.pre {
                    // the weight counts the whole match, `pre` included, as in a
                    // single forward run
                    Some(pre) => pre
                        .find_anchored(preceding, &env, false, &definitions)
                        .map(|p| {
                            let weight = found.weight() + p.weight();
                            found.with_weight(weight)
                        }),
                    None => Some(found),
                }
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::RuleParser;
    use crate::parser::{CharacterClass, match_rule::PatternParser};
    use crate::translator::TranslationStage;

    // just create some fake anchored rule for testing purposes
    fn fake_rule() -> AnchoredRule {
        let rule = RuleParser::new("always foo 1").rule().unwrap();
        AnchoredRule::new(rule, None, 0)
    }

    #[test]
    fn find_pattern() {
        let env = Environment::new();
        let patterns = PatternParser::new("abc", Side::Post).pattern().unwrap();
        let stage = TranslationStage::Main;
        let ctx = CharacterClasses::default();
        let re = Regexp::from_patterns(&patterns, &ctx).compile();
        assert_eq!(
            re.find("abc", &env).unwrap(),
            ResolvedTranslation::new("", "", 3, stage, None)
        );
        assert_eq!(re.find("def", &env), None);
    }

    #[test]
    fn find_either() {
        let env = Environment::new();
        let patterns = PatternParser::new("a|b", Side::Post).pattern().unwrap();
        let stage = TranslationStage::Main;
        let ctx = CharacterClasses::default();
        let re = Regexp::from_patterns(&patterns, &ctx).compile();
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
        let patterns = PatternParser::new("%[#]", Side::Post).pattern().unwrap();
        let stage = TranslationStage::Main;
        let mut ctx = CharacterClasses::default();
        for digit in ['1', '2', '3'] {
            ctx.insert(CharacterClass::Digit, digit);
        }
        let re = Regexp::from_patterns(&patterns, &ctx).compile();
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
        let patterns = PatternParser::new("%[u]", Side::Post).pattern().unwrap();
        let stage = TranslationStage::Main;
        let mut ctx = CharacterClasses::default();
        for c in ['A', 'B', 'C'] {
            ctx.insert(CharacterClass::Uppercase, c);
        }
        let re = Regexp::from_patterns(&patterns, &ctx).compile();
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
        let patterns = PatternParser::new("%[.$u]", Side::Post).pattern().unwrap();
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
        let re = Regexp::from_patterns(&patterns, &ctx).compile();
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
        let patterns = PatternParser::new("[abc]", Side::Post).pattern().unwrap();
        let stage = TranslationStage::Main;
        let ctx = CharacterClasses::default();
        let re = Regexp::from_patterns(&patterns, &ctx).compile();
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
        let patterns = PatternParser::new("[abc]+", Side::Post).pattern().unwrap();
        let stage = TranslationStage::Main;
        let ctx = CharacterClasses::default();
        let re = Regexp::from_patterns(&patterns, &ctx).compile();
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
        let pre = PatternParser::new("[abc]+", Side::Pre).pattern().unwrap();
        let post = PatternParser::new("[123]+", Side::Post).pattern().unwrap();
        let ctx = CharacterClasses::default();
        let rule = fake_rule();
        let mut builder = MatchPatternsBuilder::new();
        builder.insert(&pre, "foo", &post, "", &rule, &ctx);
        let translation =
            ResolvedTranslation::new("foo", "", 5, TranslationStage::Main, rule.clone());
        let matcher = builder.build();
        assert_eq!(matcher.find("foo1", "a"), vec![translation.clone()]);
        assert_eq!(matcher.find("foo2", "b"), vec![translation.clone()]);
        let translations = vec![ResolvedTranslation::new(
            "foo",
            "",
            9,
            TranslationStage::Main,
            rule.clone(),
        )];
        assert_eq!(matcher.find("foo333", "ccc"), translations);
        // `pre` must end right at the cursor
        assert!(matcher.find("foo1", "xa").is_empty());
        assert!(matcher.find("foo1", "").is_empty());
        assert!(matcher.find("def", "").is_empty());
    }

    #[test]
    fn find_multiple_match() {
        let pre = PatternParser::new("[abc]+", Side::Pre).pattern().unwrap();
        let post = PatternParser::new("[1234567890]", Side::Post)
            .pattern()
            .unwrap();
        let ctx = CharacterClasses::default();
        let rule = fake_rule();
        let mut builder = MatchPatternsBuilder::new();
        builder.insert(&pre, "foo", &post, "FOO", &rule, &ctx);
        builder.insert(&pre, "bar", &post, "BAR", &rule, &ctx);
        let translation = vec![ResolvedTranslation::new(
            "foo",
            "FOO",
            7,
            TranslationStage::Main,
            rule.clone(),
        )];
        let matcher = builder.build();
        assert_eq!(matcher.find("foo333", "aaa"), translation);
        let translation = vec![ResolvedTranslation::new(
            "bar",
            "BAR",
            7,
            TranslationStage::Main,
            rule.clone(),
        )];
        assert_eq!(matcher.find("bar333", "aaa"), translation);
        assert_ne!(matcher.find("baz333", "aaa"), translation);
    }

    #[test]
    fn find_match_with_pre_boundary() {
        let pre = PatternParser::new("^", Side::Pre).pattern().unwrap();
        let post = PatternParser::new(".", Side::Post).pattern().unwrap();
        let ctx = CharacterClasses::default();
        let rule = fake_rule();
        let mut builder = MatchPatternsBuilder::new();
        builder.insert(&pre, "a", &post, "X", &rule, &ctx);
        let matcher = builder.build();
        let translation = ResolvedTranslation::new("a", "X", 2, TranslationStage::Main, rule);
        // "a" is preceded by nothing only when it truly begins the whole input
        assert_eq!(matcher.find("ab", ""), vec![translation]);
        assert!(matcher.find("ab", "x").is_empty());
    }

    #[test]
    fn find_match_with_pre_attribute_boundary() {
        // `%[u^]` in `pre`: an uppercase letter immediately before `chars`, or (via `^`) nothing
        // at all if `chars` itself is at the true beginning of input.
        let pre = PatternParser::new("%[u^]", Side::Pre).pattern().unwrap();
        let post = PatternParser::new(".", Side::Post).pattern().unwrap();
        let mut ctx = CharacterClasses::default();
        for c in ['A', 'B', 'C'] {
            ctx.insert(CharacterClass::Uppercase, c);
        }
        let rule = fake_rule();
        let mut builder = MatchPatternsBuilder::new();
        builder.insert(&pre, "x", &post, "X", &rule, &ctx);
        let matcher = builder.build();
        // character-class branch: preceded by an uppercase letter, mid-string
        let translation =
            ResolvedTranslation::new("x", "X", 3, TranslationStage::Main, rule.clone());
        assert_eq!(matcher.find("xy", "A"), vec![translation]);
        // boundary branch: no preceding letter needed, at the true start of input
        let translation = ResolvedTranslation::new("x", "X", 2, TranslationStage::Main, rule);
        assert_eq!(matcher.find("xy_", ""), vec![translation]);
        // neither branch applies: not at the true start, and not preceded by an uppercase letter
        assert!(matcher.find("xy_", "b").is_empty());
    }
}
