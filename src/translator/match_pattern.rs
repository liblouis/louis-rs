//! Store and find match rules using a simple regexp engine ([`Regexp`](crate::translator::regexp))

use std::collections::HashSet;

use crate::parser::{AnchoredRule, Attribute, CharacterClasses, Pattern, Patterns};

use crate::translator::effect::Environment;
use crate::translator::regexp::{CompiledRegexp, Regexp};
use crate::translator::translation::Translation;
use crate::translator::{ResolvedTranslation, TranslationStage};

impl Regexp {
    fn from_pattern(item: &Pattern, ctx: &CharacterClasses) -> Self {
        match item {
            Pattern::Empty => Regexp::Empty,
            Pattern::Characters(s) => Regexp::String(s.to_string()),
            Pattern::Boundary => Regexp::NotImplemented,
            Pattern::Any => Regexp::Any,
            Pattern::Set(hash_set) => Regexp::CharacterClass(hash_set.clone()),
            Pattern::Attributes(hash_set) => Regexp::from_attributes(hash_set, ctx),
            Pattern::Group(_vec) => Regexp::NotImplemented,
            Pattern::Negate(_pattern) => Regexp::NotImplemented,
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
                Box::new(Regexp::from_pattern(left, ctx)),
                Box::new(Regexp::from_pattern(right, ctx)),
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
                    if let Some(chars) = ctx.get(&class) {
                        characters.extend(chars);
                    }
                }
                Attribute::Boundary => (),
                Attribute::ByOrder(_) => (),
                Attribute::Any => (), // TODO
            }
        }
        Regexp::CharacterClass(characters)
    }

    /// Combine the pre and post patterns with the match characters into one big Regexp by joining
    /// them with concat
    fn from_match_rule(
        pre: &Patterns,
        chars: String,
        post: &Patterns,
        ctx: &CharacterClasses,
    ) -> Self {
        Regexp::Concat(
            Box::new(Regexp::Concat(
                Box::new(Regexp::from_patterns(pre, ctx)),
                Box::new(Regexp::Capture(Box::new(Regexp::String(chars)))),
            )),
            Box::new(Regexp::from_patterns(post, ctx)),
        )
    }
}

#[derive(Debug)]
pub struct MatchPatternsBuilder {
    pairs: Vec<(Regexp, Translation)>,
}

impl MatchPatternsBuilder {
    pub fn new() -> Self {
        Self { pairs: Vec::new() }
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
        let re = Regexp::from_match_rule(pre, chars.to_string(), post, &ctx);
        self.pairs.push((re, translation));
    }

    pub fn build(self) -> MatchPatterns {
        MatchPatterns {
            re: Regexp::compile_many_accepting(&self.pairs),
        }
    }
}

#[derive(Debug)]
pub struct MatchPatterns {
    re: CompiledRegexp,
}

impl MatchPatterns {
    pub fn find(&self, input: &str) -> Vec<ResolvedTranslation> {
        self.re.find(input, &Environment::new())
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
        let re = Regexp::from_patterns(&patterns, &ctx).compile();
        assert_eq!(
            re.find("abc", &env),
            vec![ResolvedTranslation::new("", "", 3, stage, None)]
        );
        assert!(re.find("def", &env).is_empty());
    }

    #[test]
    fn find_either() {
        let env = Environment::new();
        let patterns = PatternParser::new("a|b").pattern().unwrap();
        let stage = TranslationStage::Main;
        let ctx = CharacterClasses::default();
        let re = Regexp::from_patterns(&patterns, &ctx).compile();
        assert_eq!(
            re.find("a", &env),
            vec![ResolvedTranslation::new("", "", 1, stage, None)]
        );
        assert_eq!(
            re.find("b", &env),
            vec![ResolvedTranslation::new("", "", 1, stage, None)]
        );
        assert_eq!(re.find("c", &env), vec![]);
        assert!(re.find("def", &env).is_empty());
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
        let re = Regexp::from_patterns(&patterns, &ctx).compile();
        assert_eq!(
            re.find("1", &env),
            vec![ResolvedTranslation::new("", "", 1, stage, None)]
        );
        assert_eq!(
            re.find("2", &env),
            vec![ResolvedTranslation::new("", "", 1, stage, None)]
        );
        assert_eq!(
            re.find("3", &env),
            vec![ResolvedTranslation::new("", "", 1, stage, None)]
        );
        assert!(re.find("def", &env).is_empty());
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
        let re = Regexp::from_patterns(&patterns, &ctx).compile();
        assert_eq!(
            re.find("A", &env),
            vec![ResolvedTranslation::new("", "", 1, stage, None)]
        );
        assert_eq!(
            re.find("A", &env),
            vec![ResolvedTranslation::new("", "", 1, stage, None)]
        );
        assert_eq!(
            re.find("C", &env),
            vec![ResolvedTranslation::new("", "", 1, stage, None)]
        );
        assert!(re.find("def", &env).is_empty());
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
        let re = Regexp::from_patterns(&patterns, &ctx).compile();
        assert_eq!(
            re.find("%", &env),
            vec![ResolvedTranslation::new("", "", 1, stage, None)]
        );
        assert_eq!(
            re.find(".", &env),
            vec![ResolvedTranslation::new("", "", 1, stage, None)]
        );
        assert_eq!(
            re.find("A", &env),
            vec![ResolvedTranslation::new("", "", 1, stage, None)]
        );
        assert!(re.find("def", &env).is_empty());
    }

    #[test]
    fn find_character_class() {
        let env = Environment::new();
        let patterns = PatternParser::new("[abc]").pattern().unwrap();
        let stage = TranslationStage::Main;
        let ctx = CharacterClasses::default();
        let re = Regexp::from_patterns(&patterns, &ctx).compile();
        assert_eq!(
            re.find("a", &env),
            vec![ResolvedTranslation::new("", "", 1, stage, None)]
        );
        assert_eq!(
            re.find("b", &env),
            vec![ResolvedTranslation::new("", "", 1, stage, None)]
        );
        assert_eq!(
            re.find("c", &env),
            vec![ResolvedTranslation::new("", "", 1, stage, None)]
        );
        assert!(re.find("def", &env).is_empty());
    }

    #[test]
    fn find_character_class_one_or_more() {
        let env = Environment::new();
        let patterns = PatternParser::new("[abc]+").pattern().unwrap();
        let stage = TranslationStage::Main;
        let ctx = CharacterClasses::default();
        let re = Regexp::from_patterns(&patterns, &ctx).compile();
        assert_eq!(
            re.find("a", &env),
            vec![ResolvedTranslation::new("", "", 1, stage, None)]
        );
        assert_eq!(
            re.find("b", &env),
            vec![ResolvedTranslation::new("", "", 1, stage, None)]
        );
        assert_eq!(
            re.find("c", &env),
            vec![ResolvedTranslation::new("", "", 1, stage, None)]
        );
        assert!(re.find("def", &env).is_empty());
    }

    #[test]
    fn find_match() {
        let pre = PatternParser::new("[abc]+").pattern().unwrap();
        let post = PatternParser::new("[123]+").pattern().unwrap();
        let ctx = CharacterClasses::default();
        let rule = fake_rule();
        let mut builder = MatchPatternsBuilder::new();
        builder.insert(&pre, "foo".into(), &post, "".into(), &rule, &ctx);
        let translation = ResolvedTranslation::new(
            "foo".into(),
            "".into(),
            5,
            TranslationStage::Main,
            rule.clone(),
        )
        .with_offset(1);
        let matcher = builder.build();
        assert_eq!(matcher.find("afoo1"), vec![translation.clone()]);
        assert_eq!(matcher.find("bfoo2"), vec![translation.clone()]);
        let translations = vec![
            ResolvedTranslation::new(
                "foo".into(),
                "".into(),
                9,
                TranslationStage::Main,
                rule.clone(),
            )
            .with_offset(3),
            ResolvedTranslation::new(
                "foo".into(),
                "".into(),
                8,
                TranslationStage::Main,
                rule.clone(),
            )
            .with_offset(3),
            ResolvedTranslation::new(
                "foo".into(),
                "".into(),
                7,
                TranslationStage::Main,
                rule.clone(),
            )
            .with_offset(3),
        ];
        assert_eq!(matcher.find("cccfoo333"), translations);
        assert!(matcher.find("def").is_empty());
    }

    #[test]
    fn find_multiple_match() {
        let env = Environment::new();
        let pre = PatternParser::new("[abc]+").pattern().unwrap();
        let post = PatternParser::new("[1234567890]").pattern().unwrap();
        let ctx = CharacterClasses::default();
        let rule = fake_rule();
        let mut builder = MatchPatternsBuilder::new();
        builder.insert(&pre, "foo".into(), &post, "FOO".into(), &rule, &ctx);
        builder.insert(&pre, "bar".into(), &post, "BAR".into(), &rule, &ctx);
        let translation = vec![
            ResolvedTranslation::new(
                "foo".into(),
                "FOO".into(),
                7,
                TranslationStage::Main,
                rule.clone(),
            )
            .with_offset(3),
        ];
        let matcher = builder.build();
        assert_eq!(matcher.find("aaafoo333"), translation);
        let translation = vec![
            ResolvedTranslation::new(
                "bar".into(),
                "BAR".into(),
                7,
                TranslationStage::Main,
                rule.clone(),
            )
            .with_offset(3),
        ];
        assert_eq!(matcher.find("aaabar333"), translation);
        assert_ne!(matcher.find("aaabaz333"), translation);
    }
}
