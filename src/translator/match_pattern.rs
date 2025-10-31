use std::collections::HashSet;
use std::ops::Deref;

use crate::parser::{Attribute, Pattern, Patterns};

use crate::translator::Translation;
use crate::translator::nfa::{AST, NFA};

impl From<&Patterns> for AST {
    fn from(patterns: &Patterns) -> Self {
        match patterns.len() {
            0 => todo!(),
            1 => AST::from(&patterns[0]),
            _ => {
                let mut ast = AST::from(&patterns[0]);
                for pattern in patterns.iter().skip(1) {
                    let other = AST::from(pattern);
                    ast = AST::Concat(Box::new(ast), Box::new(other));
                }
                ast
            }
        }
    }
}

impl From<&Box<Pattern>> for AST {
    fn from(pattern: &Box<Pattern>) -> Self {
        AST::from(pattern.deref())
    }
}

impl From<&Pattern> for AST {
    fn from(item: &Pattern) -> Self {
        match item {
            Pattern::Empty => AST::NotImplemented,
            Pattern::Characters(s) => AST::String(s.to_string()),
            Pattern::Boundary => AST::NotImplemented,
            Pattern::Any => AST::Any,
            Pattern::Set(hash_set) => AST::Set(hash_set.clone()),
            Pattern::Attributes(hash_set) => AST::from(hash_set),
            Pattern::Group(vec) => AST::NotImplemented,
            Pattern::Negate(pattern) => AST::NotImplemented,
            Pattern::Optional(pattern) => AST::Optional(Box::new(AST::from(pattern))),
            Pattern::ZeroOrMore(pattern) => AST::ZeroOrMore(Box::new(AST::from(pattern))),
            Pattern::OneOrMore(pattern) => AST::OneOrMore(Box::new(AST::from(pattern))),
            Pattern::Either(left, right) => {
                AST::Either(Box::new(AST::from(left)), Box::new(AST::from(right)))
            }
        }
    }
}

impl From<&HashSet<Attribute>> for AST {
    fn from(items: &HashSet<Attribute>) -> Self {
        let mut chars = HashSet::new();
        let digits = HashSet::from(['1', '2', '3', '4', '5', '6', '7', '8', '9', '0']);
        let letters = HashSet::from([
            'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q',
            'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
        ]);
        for attr in items {
            match attr {
                Attribute::Space => {
                    chars.insert(' ');
                }
                Attribute::Digit => {
                    chars.extend(&digits);
                }
                Attribute::Letter => chars.extend(&letters),
                Attribute::Uppercase => todo!(),
                Attribute::Lowercase => chars.extend(&letters),
                Attribute::Punctuation => todo!(),
                Attribute::Sign => todo!(),
                Attribute::Seqdelimiter => todo!(),
                Attribute::Seqbeforechars => todo!(),
                Attribute::Seqafterchars => todo!(),
                Attribute::Boundary => todo!(),
                Attribute::UserDefined(_) => todo!(),
            }
        }
        AST::Set(chars)
    }
}

impl AST {
    /// Combine the pre and post patterns with the match characters into one big regexp AST by joining them with concat
    fn from_match_rule(pre: &Patterns, chars: String, post: &Patterns) -> Self {
        AST::Concat(
            Box::new(AST::Concat(
                Box::new(AST::Concat(Box::new(AST::from(pre)), Box::new(AST::Offset))),
                Box::new(AST::String(chars)),
            )),
            Box::new(AST::from(post)),
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
        let ast = AST::from_match_rule(pre, chars, post);
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
        let ast = AST::from(&patterns);
        let nfa = NFA::from(&ast);
        assert_eq!(nfa.find_translations("abc"), vec![translation]);
        assert!(nfa.find_translations("def").is_empty());
    }

    #[test]
    fn find_character_class() {
        let patterns = PatternParser::new("[abc]").pattern().unwrap();
        let translation = Translation::default();
        let ast = AST::from(&patterns);
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
        let ast = AST::from(&patterns);
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
        let ast = AST::from_match_rule(&pre, "foo".into(), &post);
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
