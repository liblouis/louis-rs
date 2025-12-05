//! Store and find match rules using a simple regexp engine ([`nfa`](crate::translator::nfa))

use std::collections::HashSet;

use crate::parser::{AnchoredRule, Attribute, Pattern, Patterns};

use crate::translator::character_classes::{CharacterClass, CharacterClasses};
use crate::translator::nfa::{AST, NFA};
use crate::translator::{Translation, TranslationStage};

impl AST {
    fn from_pattern(item: &Pattern, ctx: &CharacterClasses) -> Self {
        match item {
            Pattern::Empty => AST::NotImplemented,
            Pattern::Characters(s) => AST::String(s.to_string()),
            Pattern::Boundary => AST::NotImplemented,
            Pattern::Any => AST::Any,
            Pattern::Set(hash_set) => AST::Set(hash_set.clone()),
            Pattern::Attributes(hash_set) => AST::from_attributes(hash_set, ctx),
            Pattern::Group(_vec) => AST::NotImplemented,
            Pattern::Negate(_pattern) => AST::NotImplemented,
            Pattern::Optional(pattern) => AST::Optional(Box::new(AST::from_pattern(pattern, ctx))),
            Pattern::ZeroOrMore(pattern) => {
                AST::ZeroOrMore(Box::new(AST::from_pattern(pattern, ctx)))
            }
            Pattern::OneOrMore(pattern) => {
                AST::OneOrMore(Box::new(AST::from_pattern(pattern, ctx)))
            }
            Pattern::Either(left, right) => AST::Either(
                Box::new(AST::from_pattern(left, ctx)),
                Box::new(AST::from_pattern(right, ctx)),
            ),
        }
    }

    fn from_patterns(patterns: &Patterns, ctx: &CharacterClasses) -> Self {
        match patterns.len() {
            0 => todo!(),
            1 => AST::from_pattern(&patterns[0], ctx),
            _ => {
                let mut ast = AST::from_pattern(&patterns[0], ctx);
                for pattern in patterns.iter().skip(1) {
                    let other = AST::from_pattern(pattern, ctx);
                    ast = AST::Concat(Box::new(ast), Box::new(other));
                }
                ast
            }
        }
    }

    fn from_attributes(attributes: &HashSet<Attribute>, ctx: &CharacterClasses) -> Self {
        let mut characters = HashSet::new();
        for attr in attributes {
            match attr {
                Attribute::Space => {
                    if let Some(chars) = ctx.get(&CharacterClass::Space) {
                        characters.extend(chars);
                    }
                }
                Attribute::Digit => {
                    if let Some(chars) = ctx.get(&CharacterClass::Digit) {
                        characters.extend(chars);
                    }
                }
                Attribute::Letter => {
                    if let Some(chars) = ctx.get(&CharacterClass::Letter) {
                        characters.extend(chars);
                    }
                }
                Attribute::Uppercase => {
                    if let Some(chars) = ctx.get(&CharacterClass::Uppercase) {
                        characters.extend(chars);
                    }
                }
                Attribute::Lowercase => {
                    if let Some(chars) = ctx.get(&CharacterClass::Lowercase) {
                        characters.extend(chars);
                    }
                }
                Attribute::Punctuation => {
                    if let Some(chars) = ctx.get(&CharacterClass::Punctuation) {
                        characters.extend(chars);
                    }
                }
                Attribute::Sign => {
                    if let Some(chars) = ctx.get(&CharacterClass::Sign) {
                        characters.extend(chars);
                    }
                }
                Attribute::Seqdelimiter => {
                    if let Some(chars) = ctx.get(&CharacterClass::Seqdelimiter) {
                        characters.extend(chars);
                    }
                }
                Attribute::Seqbeforechars => {
                    if let Some(chars) = ctx.get(&CharacterClass::Seqbeforechars) {
                        characters.extend(chars);
                    }
                }
                Attribute::Seqafterchars => {
                    if let Some(chars) = ctx.get(&CharacterClass::Seqafterchars) {
                        characters.extend(chars);
                    }
                }
                Attribute::Boundary => (),       // TODO: implement
                Attribute::UserDefined(_) => (), // TODO: implement
            }
        }
        AST::Set(characters)
    }

    /// Combine the pre and post patterns with the match characters into one big regexp AST by joining them with concat
    fn from_match_rule(
        pre: &Patterns,
        chars: String,
        post: &Patterns,
        ctx: &CharacterClasses,
    ) -> Self {
        AST::Concat(
            Box::new(AST::Concat(
                Box::new(AST::Concat(
                    Box::new(AST::from_patterns(pre, ctx)),
                    Box::new(AST::Offset),
                )),
                Box::new(AST::String(chars)),
            )),
            Box::new(AST::from_patterns(post, ctx)),
        )
    }
}

#[derive(Debug)]
pub struct MatchPatterns {
    nfa: NFA,
}

impl MatchPatterns {
    pub fn new() -> Self {
        Self {
            nfa: NFA::default(),
        }
    }

    pub fn insert(
        &mut self,
        pre: &Patterns,
        chars: &str,
        post: &Patterns,
        to: &str,
        origin: &AnchoredRule,
    ) {
        let translation = Translation::new(chars, to, 0, TranslationStage::Main, origin.clone());
        let ctx = CharacterClasses::new();
        let ast = AST::from_match_rule(pre, chars.to_string(), post, &ctx);
        self.nfa.merge_accepting_fragment(&ast, translation);
    }

    pub fn find_translations(&self, input: &str) -> Vec<Translation> {
        self.nfa.find_translations(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::PatternParser;
    use crate::parser::RuleParser;
    use crate::translator::TranslationStage;

    // just create some fake anchored rule for testing purposes
    fn fake_rule() -> AnchoredRule {
        let rule = RuleParser::new("always foo 1").rule().unwrap();
        AnchoredRule::new(rule, None, 0)
    }

    #[test]
    fn find_pattern() {
        let patterns = PatternParser::new("abc").pattern().unwrap();
        let translation = Translation::default();
        let ctx = CharacterClasses::new();
        let ast = AST::from_patterns(&patterns, &ctx);
        let nfa = NFA::from(&ast);
        assert_eq!(nfa.find_translations("abc"), vec![translation]);
        assert!(nfa.find_translations("def").is_empty());
    }

    #[test]
    fn find_either() {
        let patterns = PatternParser::new("a|b").pattern().unwrap();
        let translation = Translation::default();
        let ctx = CharacterClasses::new();
        let ast = AST::from_patterns(&patterns, &ctx);
        let nfa = NFA::from(&ast);
        assert_eq!(nfa.find_translations("a"), vec![translation.clone()]);
        assert_eq!(nfa.find_translations("b"), vec![translation.clone()]);
        assert_eq!(nfa.find_translations("c"), vec![]);
        assert!(nfa.find_translations("def").is_empty());
    }

    #[test]
    fn find_character_class() {
        let patterns = PatternParser::new("[abc]").pattern().unwrap();
        let translation = Translation::default();
        let ctx = CharacterClasses::new();
        let ast = AST::from_patterns(&patterns, &ctx);
        let nfa = NFA::from(&ast);
        assert_eq!(nfa.find_translations("a"), vec![translation.clone()]);
        assert_eq!(nfa.find_translations("b"), vec![translation.clone()]);
        assert_eq!(nfa.find_translations("c"), vec![translation]);
        assert!(nfa.find_translations("def").is_empty());
    }

    #[test]
    fn find_character_class_one_or_more() {
        let patterns = PatternParser::new("[abc]+").pattern().unwrap();
        let translation = Translation::default();
        let ctx = CharacterClasses::new();
        let ast = AST::from_patterns(&patterns, &ctx);
        let nfa = NFA::from(&ast);
        assert_eq!(nfa.find_translations("a"), vec![translation.clone()]);
        assert_eq!(nfa.find_translations("b"), vec![translation.clone()]);
        assert_eq!(nfa.find_translations("c"), vec![translation]);
        assert!(nfa.find_translations("def").is_empty());
    }

    #[test]
    fn find_match() {
        let pre = PatternParser::new("[abc]+").pattern().unwrap();
        let post = PatternParser::new("[123]+").pattern().unwrap();
        let rule = fake_rule();
        let mut matcher = MatchPatterns::new();
        matcher.insert(&pre, "foo".into(), &post, "".into(), &rule);
        let translation = Translation::new(
            "foo".into(),
            "".into(),
            5,
            TranslationStage::Main,
            rule.clone(),
        )
        .with_offset(1);
        assert_eq!(
            matcher.find_translations("afoo1"),
            vec![translation.clone()]
        );
        assert_eq!(
            matcher.find_translations("bfoo2"),
            vec![translation.clone()]
        );
        let translations = vec![
            Translation::new(
                "foo".into(),
                "".into(),
                9,
                TranslationStage::Main,
                rule.clone(),
            )
            .with_offset(3),
            Translation::new(
                "foo".into(),
                "".into(),
                8,
                TranslationStage::Main,
                rule.clone(),
            )
            .with_offset(3),
            Translation::new(
                "foo".into(),
                "".into(),
                7,
                TranslationStage::Main,
                rule.clone(),
            )
            .with_offset(3),
        ];
        assert_eq!(matcher.find_translations("cccfoo333"), translations);
        assert!(matcher.find_translations("def").is_empty());
    }

    #[test]
    fn find_multiple_match() {
        let pre = PatternParser::new("[abc]+").pattern().unwrap();
        let post = PatternParser::new("[1234567890]").pattern().unwrap();
        let rule = fake_rule();
        let mut match_patterns = MatchPatterns::new();
        match_patterns.insert(&pre, "foo".into(), &post, "FOO".into(), &rule);
        match_patterns.insert(&pre, "bar".into(), &post, "BAR".into(), &rule);
        let translation = vec![
            Translation::new(
                "foo".into(),
                "FOO".into(),
                7,
                TranslationStage::Main,
                rule.clone(),
            )
            .with_offset(3),
        ];
        assert_eq!(match_patterns.find_translations("aaafoo333"), translation);
        let translation = vec![
            Translation::new(
                "bar".into(),
                "BAR".into(),
                7,
                TranslationStage::Main,
                rule.clone(),
            )
            .with_offset(3),
        ];
        assert_eq!(match_patterns.find_translations("aaabar333"), translation);
        assert_ne!(match_patterns.find_translations("aaabaz333"), translation);
    }
}
