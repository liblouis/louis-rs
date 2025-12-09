//! Store and find context rules using a simple regexp engine ([`nfa`](crate::translator::nfa))

use std::collections::HashSet;

use crate::parser::{Instruction, Quantifier, Test, dots_to_unicode};

use crate::parser::{AnchoredRule, Attribute, CharacterClass, CharacterClasses};
use crate::translator::nfa::{AST, NFA};
use crate::translator::{Translation, TranslationStage};

impl AST {
    fn from_test(test: &Test, ctx: &CharacterClasses) -> Self {
        match test.tests().len() {
            0 => todo!(),
            1 => AST::from_instruction(&test.tests().first().unwrap(), ctx),
            _ => {
                let mut ast = AST::from_instruction(&test.tests().first().unwrap(), ctx);
                for instruction in test.tests().iter().skip(1) {
                    let other = AST::from_instruction(instruction, ctx);
                    ast = AST::Concat(Box::new(ast), Box::new(other));
                }
                ast
            }
        }
    }

    fn from_instruction(instruction: &Instruction, ctx: &CharacterClasses) -> Self {
        match instruction {
            Instruction::Lookback { len } => AST::NotImplemented, // ignore
            Instruction::Variable { var, op, operand } => AST::NotImplemented, // TODO
            Instruction::String { s } => AST::String(s.to_string()),
            Instruction::Dots { dots } => AST::String(dots_to_unicode(dots)),
            Instruction::Attributes { attrs, quantifier } => {
                AST::from_multipass_attributes(attrs, quantifier, ctx)
            }
            Instruction::Class { name, quantifier } => AST::from_class(name, quantifier, ctx),
            Instruction::Negate { test } => AST::NotImplemented,
            Instruction::Replace { tests } => AST::NotImplemented,
        }
    }

    fn from_class(name: &str, quantifier: &Option<Quantifier>, ctx: &CharacterClasses) -> Self {
        let class = CharacterClass::from(name);
        let characters = ctx.get(&class).unwrap_or_default(); // FIXME: should probably fail if we cannot find the class
        if let Some(quantifier) = quantifier {
            match quantifier {
                Quantifier::Number(n) => AST::RepeatExactly(*n, Box::new(AST::Set(characters))),
                Quantifier::Range(min, max) => {
                    AST::RepeatAtLeastAtMost(*min, *max, Box::new(AST::Set(characters)))
                }
                Quantifier::Any => AST::ZeroOrMore(Box::new(AST::Set(characters))),
            }
        } else {
            AST::Set(characters)
        }
    }

    fn from_multipass_attributes(
        attrs: &HashSet<Attribute>,
        quantifier: &Option<Quantifier>,
        ctx: &CharacterClasses,
    ) -> Self {
        let mut characters: HashSet<char> = HashSet::default();
        for attr in attrs {
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
        if let Some(quantifier) = quantifier {
            match quantifier {
                Quantifier::Number(n) => AST::RepeatExactly(*n, Box::new(AST::Set(characters))),
                Quantifier::Range(min, max) => {
                    AST::RepeatAtLeastAtMost(*min, *max, Box::new(AST::Set(characters)))
                }
                Quantifier::Any => AST::ZeroOrMore(Box::new(AST::Set(characters))),
            }
        } else {
            AST::Set(characters)
        }
    }
}

#[derive(Debug)]
pub struct ContextPatterns {
    nfa: NFA,
}

impl ContextPatterns {
    pub fn new() -> Self {
        Self {
            nfa: NFA::default(),
        }
    }

    pub fn insert(
        &mut self,
        test: &Test,
        from: &str,
        to: &str,
        origin: &AnchoredRule,
        ctx: &CharacterClasses,
    ) {
        let translation = Translation::new(from, to, 0, TranslationStage::Main, origin.clone());
        let ast = AST::from_test(test, &ctx);
        self.nfa.merge_accepting_fragment(&ast, translation);
    }

    pub fn find_translations(&self, input: &str) -> Vec<Translation> {
        self.nfa.find_translations(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::RuleParser;
    use crate::parser::test;

    // just create some fake anchored rule for testing purposes
    fn fake_rule() -> AnchoredRule {
        let rule = RuleParser::new("always foo 1").rule().unwrap();
        AnchoredRule::new(rule, None, 0)
    }

    fn context(class: CharacterClass, chars: &[char]) -> CharacterClasses {
        let mut ctx = CharacterClasses::default();
        for ch in chars {
            ctx.insert(class.clone(), *ch);
        }
        ctx
    }

    #[test]
    fn find_string() {
        let tests = test::Parser::new("\"abc\"").tests().unwrap();
        let translation = Translation::default();
        let ctx = CharacterClasses::default();
        let ast = AST::from_test(&tests, &ctx);
        let nfa = NFA::from(&ast);
        assert_eq!(nfa.find_translations("abc"), vec![translation]);
        assert!(nfa.find_translations("def").is_empty());
    }

    #[test]
    fn find_attribute_digit() {
        let tests = test::Parser::new("$d").tests().unwrap();
        let translation = Translation::default();
        let ctx = context(CharacterClass::Digit, &['1', '2', '3']);
        let ast = AST::from_test(&tests, &ctx);
        let nfa = NFA::from(&ast);
        assert_eq!(nfa.find_translations("1"), vec![translation.clone()]);
        assert_eq!(nfa.find_translations("2"), vec![translation.clone()]);
        assert_eq!(nfa.find_translations("3"), vec![translation]);
        assert!(nfa.find_translations("def").is_empty());
    }

    #[test]
    fn find_attribute_uppercase() {
        let tests = test::Parser::new("$U").tests().unwrap();
        let translation = Translation::default();
        let ctx = context(CharacterClass::Uppercase, &['A', 'B', 'C']);
        let ast = AST::from_test(&tests, &ctx);
        let nfa = NFA::from(&ast);
        assert_eq!(nfa.find_translations("A"), vec![translation.clone()]);
        assert_eq!(nfa.find_translations("A"), vec![translation.clone()]);
        assert_eq!(nfa.find_translations("C"), vec![translation]);
        assert!(nfa.find_translations("def").is_empty());
    }

    #[test]
    fn find_attribute_uppercase_punctuation_or_sign() {
        let tests = test::Parser::new("$USp").tests().unwrap();
        let translation = Translation::default();
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
        let ast = AST::from_test(&tests, &ctx);
        let nfa = NFA::from(&ast);
        assert_eq!(nfa.find_translations("%"), vec![translation.clone()]);
        assert_eq!(nfa.find_translations("."), vec![translation.clone()]);
        assert_eq!(nfa.find_translations("A"), vec![translation]);
        assert!(nfa.find_translations("def").is_empty());
    }

    #[test]
    fn find_character_class() {
        let tests = test::Parser::new("%letter").tests().unwrap();
        let translation = Translation::default();
        let ctx = context(CharacterClass::Letter, &['a', 'b', 'c']);
        let ast = AST::from_test(&tests, &ctx);
        let nfa = NFA::from(&ast);
        assert_eq!(nfa.find_translations("a"), vec![translation.clone()]);
        assert_eq!(nfa.find_translations("b"), vec![translation.clone()]);
        assert_eq!(nfa.find_translations("c"), vec![translation]);
        assert!(nfa.find_translations("def").is_empty());
    }

    #[test]
    fn find_character_class_three() {
        let tests = test::Parser::new("%letter3").tests().unwrap();
        let translation = Translation::default();
        let ctx = context(CharacterClass::Letter, &['a', 'b', 'c']);
        let ast = AST::from_test(&tests, &ctx);
        let nfa = NFA::from(&ast);
        assert_eq!(nfa.find_translations("abc"), vec![translation.clone()]);
        assert_eq!(nfa.find_translations("bbb"), vec![translation.clone()]);
        assert_eq!(nfa.find_translations("ccc"), vec![translation]);
        assert!(nfa.find_translations("a").is_empty());
        assert!(nfa.find_translations("aa").is_empty());
        assert!(nfa.find_translations("def").is_empty());
    }
}
