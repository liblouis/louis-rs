use std::ops::Deref;

use crate::parser::{Pattern, Patterns};

use crate::translator::nfa::{AST, NFA};

use super::Translation;

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
            Pattern::Empty => todo!(),
            Pattern::Characters(s) => AST::String(s.to_string()),
            Pattern::Boundary => todo!(),
            Pattern::Any => AST::Any,
            Pattern::Set(hash_set) => AST::Set(hash_set.clone()),
            Pattern::Attributes(hash_set) => todo!(),
            Pattern::Group(vec) => todo!(),
            Pattern::Negate(pattern) => todo!(),
            Pattern::Optional(pattern) => AST::Optional(Box::new(AST::from(pattern))),
            Pattern::ZeroOrMore(pattern) => AST::ZeroOrMore(Box::new(AST::from(pattern))),
            Pattern::OneOrMore(pattern) => AST::OneOrMore(Box::new(AST::from(pattern))),
            Pattern::Either(left, right) => AST::Either(Box::new(AST::from(left)), Box::new(AST::from(right)))
        }
    }
}

impl AST {
    /// Combine the pre and post patterns with the match characters into one big regexp AST by joining the with concat
    fn from_match_rule(pre: &Patterns, chars: String, post: &Patterns) -> Self {
        AST::Concat(Box::new(AST::Concat(Box::new(AST::Concat(Box::new(AST::from(pre)), Box::new(AST::Offset))), Box::new(AST::String(chars)))), Box::new(AST::from(post)))
    }
}

impl NFA {
    fn merge_patterns(&mut self, patterns: &Patterns) {
	match patterns.len() {
	    0 => (),
	    1 => {
		let ast = AST::from(&patterns[0]);
		self.add_fragment(&ast);
	    }
	    _ => {
		let ast = AST::from(&patterns[0]);
		let mut union = self.add_fragment(&ast);
		for pattern in patterns.iter().skip(1) {
		    let ast = AST::from(pattern);
		    let fragment = self.add_fragment(&ast);
		    union = self.add_union(&union, &fragment)
		}
	    }
	}
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

}
