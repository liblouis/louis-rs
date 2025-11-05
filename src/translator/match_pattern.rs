use std::collections::HashSet;

use crate::parser::{Attribute, Pattern, Patterns};

use crate::translator::Translation;
use crate::translator::nfa::{AST, NFA};

use super::CharacterAttributes;

impl AST {
    fn from_pattern(item: &Pattern, ctx: &CharacterAttributes) -> Self {
        match item {
            Pattern::Empty => AST::NotImplemented,
            Pattern::Characters(s) => AST::String(s.to_string()),
            Pattern::Boundary => AST::NotImplemented,
            Pattern::Any => AST::Any,
            Pattern::Set(hash_set) => AST::Set(hash_set.clone()),
            Pattern::Attributes(hash_set) => AST::from_attributes(hash_set, ctx),
            Pattern::Group(vec) => AST::NotImplemented,
            Pattern::Negate(pattern) => AST::NotImplemented,
            Pattern::Optional(pattern) => {
                AST::Optional(Box::new(AST::from_pattern(pattern, ctx)))
            }
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

    fn from_patterns(patterns: &Patterns, ctx: &CharacterAttributes) -> Self {
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

    fn from_attributes(attributes: &HashSet<Attribute>, ctx: &CharacterAttributes) -> Self {
        let mut characters = HashSet::new();
        for attr in attributes {
            match attr {
                Attribute::Space => {
                    if let Some(chars) = ctx.get(Attribute::Space) {
                        characters.extend(chars);
                    }
                }
                Attribute::Digit => {
                    if let Some(chars) = ctx.get(Attribute::Digit) {
                        characters.extend(chars);
                    }
                }
                Attribute::Letter => {
                    if let Some(chars) = ctx.get(Attribute::Letter) {
                        characters.extend(chars);
                    }
                }
                Attribute::Uppercase => (), // TODO: implement
                Attribute::Lowercase => {
                    if let Some(chars) = ctx.get(Attribute::Letter) {
                        characters.extend(chars);
                    }
                }
                Attribute::Punctuation => {
                    if let Some(chars) = ctx.get(Attribute::Punctuation) {
                        characters.extend(chars);
                    }
                }
                Attribute::Sign => {
                    if let Some(chars) = ctx.get(Attribute::Sign) {
                        characters.extend(chars);
                    }
                }
                Attribute::Seqdelimiter => {
                    if let Some(chars) = ctx.get(Attribute::Seqdelimiter) {
                        characters.extend(chars);
                    }
                }
                Attribute::Seqbeforechars => {
                    if let Some(chars) = ctx.get(Attribute::Seqbeforechars) {
                        characters.extend(chars);
                    }
                }
                Attribute::Seqafterchars => {
                    if let Some(chars) = ctx.get(Attribute::Seqafterchars) {
                        characters.extend(chars);
                    }
                }
                Attribute::Boundary => {
                    if let Some(chars) = ctx.get(Attribute::Boundary) {
                        characters.extend(chars);
                    }
                }
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
        ctx: &CharacterAttributes,
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

    pub fn insert(&mut self, pre: &Patterns, chars: String, post: &Patterns, to: String) {
        let translation = Translation::new(chars.clone(), to, 0);
        let ctx = CharacterAttributes::new();
        let ast = AST::from_match_rule(pre, chars, post, &ctx);
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

    #[test]
    fn find_pattern() {
        let patterns = PatternParser::new("abc").pattern().unwrap();
        let translation = Translation::default();
        let ctx = CharacterAttributes::new();
        let ast = AST::from_patterns(&patterns, &ctx);
        let nfa = NFA::from(&ast);
        assert_eq!(nfa.find_translations("abc"), vec![translation]);
        assert!(nfa.find_translations("def").is_empty());
    }

    #[test]
    fn find_either() {
        let patterns = PatternParser::new("a|b").pattern().unwrap();
        let translation = Translation::default();
        let ctx = CharacterAttributes::new();
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
        let ctx = CharacterAttributes::new();
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
        let ctx = CharacterAttributes::new();
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
        let ctx = CharacterAttributes::new();
        let ast = AST::from_match_rule(&pre, "foo".into(), &post, &ctx);
        let nfa = NFA::from(&ast);
        let translation = Translation::new("".into(), "".into(), 5).with_offset(1);
        assert_eq!(nfa.find_translations("afoo1"), vec![translation.clone()]);
        assert_eq!(nfa.find_translations("bfoo2"), vec![translation.clone()]);
        let translations = vec![
            Translation::new("".into(), "".into(), 9).with_offset(3),
            Translation::new("".into(), "".into(), 8).with_offset(3),
            Translation::new("".into(), "".into(), 7).with_offset(3),
        ];
        assert_eq!(nfa.find_translations("cccfoo333"), translations);
        assert!(nfa.find_translations("def").is_empty());
    }

    #[test]
    fn find_multiple_match() {
        let pre = PatternParser::new("[abc]+").pattern().unwrap();
        let post = PatternParser::new("[1234567890]").pattern().unwrap();
        let mut match_patterns = MatchPatterns::new();
        match_patterns.insert(&pre, "foo".into(), &post, "FOO".into());
        match_patterns.insert(&pre, "bar".into(), &post, "BAR".into());
        let translation = vec![Translation::new("foo".into(), "FOO".into(), 7).with_offset(3)];
        assert_eq!(match_patterns.find_translations("aaafoo333"), translation);
        let translation = vec![Translation::new("bar".into(), "BAR".into(), 7).with_offset(3)];
        assert_eq!(match_patterns.find_translations("aaabar333"), translation);
        assert_ne!(match_patterns.find_translations("aaabaz333"), translation);
    }
}
