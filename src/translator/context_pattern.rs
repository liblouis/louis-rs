//! Store and find context rules using a simple regexp engine ([`nfa`](crate::translator::nfa))

use std::collections::HashSet;

use crate::parser::{
    Action, ActionInstruction, Quantifier, Test, TestInstruction, dots_to_unicode,
};

use crate::parser::{AnchoredRule, Attribute, CharacterClass, CharacterClasses};
use crate::translator::nfa::{AST, NFA};
use crate::translator::{Translation, TranslationStage};

impl AST {
    fn from_test(test: &Test, ctx: &CharacterClasses) -> Self {
        AST::from_instructions(test.tests(), ctx)
    }

    fn from_instructions(instructions: &Vec<TestInstruction>, ctx: &CharacterClasses) -> Self {
        match instructions.len() {
            0 => todo!(),
            1 => AST::from_instruction(&instructions[0], ctx),
            _ => {
                let mut ast = AST::from_instruction(&instructions[0], ctx);
                for instruction in &instructions[1..] {
                    let other = AST::from_instruction(instruction, ctx);
                    ast = AST::Concat(Box::new(ast), Box::new(other));
                }
                ast
            }
        }
    }

    fn from_instruction(instruction: &TestInstruction, ctx: &CharacterClasses) -> Self {
        match instruction {
            TestInstruction::Lookback { .. } => AST::NotImplemented, // ignore
            TestInstruction::Variable { .. } => AST::NotImplemented, // TODO
            TestInstruction::String { s } => AST::String(s.to_string()),
            TestInstruction::Dots { dots } => AST::String(dots_to_unicode(dots)),
            TestInstruction::Attributes { attrs, quantifier } => {
                AST::from_multipass_attributes(attrs, quantifier, ctx)
            }
            TestInstruction::Class { name, quantifier } => AST::from_class(name, quantifier, ctx),
            TestInstruction::Negate { .. } => AST::NotImplemented,
            TestInstruction::Replace { tests } => {
                AST::Capture(Box::new(AST::from_instructions(tests, ctx)))
            }
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
    use crate::parser::test;

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

    #[test]
    fn find_capture_with_character_class() {
        let tests = test::Parser::new("\"a\"[%digit]\"b\"").tests().unwrap();
        let translation = Translation::new("", "", 3, TranslationStage::Main, None);
        let ctx = context(CharacterClass::Digit, &['1', '2', '3']);
        let ast = AST::from_test(&tests, &ctx);
        let nfa = NFA::from(&ast);
        assert_eq!(
            nfa.find_translations("a1b"),
            [translation
                .clone()
                .with_offset(1)
                .with_capture("1".to_string())]
        );
        assert_eq!(
            nfa.find_translations("a2b"),
            [translation
                .clone()
                .with_offset(1)
                .with_capture("2".to_string())]
        );
        assert_eq!(
            nfa.find_translations("a3b"),
            [translation
                .clone()
                .with_offset(1)
                .with_capture("3".to_string())]
        );
        assert_eq!(nfa.find_translations("bbb"), []);
        assert_eq!(nfa.find_translations("ccc"), []);
        assert_eq!(nfa.find_translations("a"), []);
        assert_eq!(nfa.find_translations("aa"), []);
        assert_eq!(nfa.find_translations("def"), []);
    }

    #[test]
    fn find_capture_with_attribute() {
        let tests = test::Parser::new("\"a\"[$dU]\"b\"").tests().unwrap();
        let translation = Translation::new("", "", 3, TranslationStage::Main, None);
        let mut ctx = context(CharacterClass::Digit, &['1', '2', '3']);
        for c in ['A', 'B', 'C'] {
            ctx.insert(CharacterClass::Uppercase, c);
        }
        let ast = AST::from_test(&tests, &ctx);
        let nfa = NFA::from(&ast);
        assert_eq!(
            nfa.find_translations("a1b"),
            [translation
                .clone()
                .with_offset(1)
                .with_capture("1".to_string())]
        );
        assert_eq!(
            nfa.find_translations("a2b"),
            [translation
                .clone()
                .with_offset(1)
                .with_capture("2".to_string())]
        );
        assert_eq!(
            nfa.find_translations("a3b"),
            [translation
                .clone()
                .with_offset(1)
                .with_capture("3".to_string())]
        );
        assert_eq!(
            nfa.find_translations("aAb"),
            [translation
                .clone()
                .with_offset(1)
                .with_capture("A".to_string())]
        );
        assert_eq!(
            nfa.find_translations("aBb"),
            [translation
                .clone()
                .with_offset(1)
                .with_capture("B".to_string())]
        );
        assert_eq!(
            nfa.find_translations("aCb"),
            [translation
                .clone()
                .with_offset(1)
                .with_capture("C".to_string())]
        );
        assert_eq!(nfa.find_translations("bbb"), []);
        assert_eq!(nfa.find_translations("ccc"), []);
        assert_eq!(nfa.find_translations("a"), []);
        assert_eq!(nfa.find_translations("aa"), []);
        assert_eq!(nfa.find_translations("def"), []);
    }

    #[test]
    fn find_capture_with_attribute_and_quantifier() {
        let tests = test::Parser::new("\"a\"[$dU3]\"b\"").tests().unwrap();
        let translation = Translation::new("", "", 5, TranslationStage::Main, None);
        let mut ctx = context(CharacterClass::Digit, &['1', '2', '3']);
        for c in ['A', 'B', 'C'] {
            ctx.insert(CharacterClass::Uppercase, c);
        }
        let ast = AST::from_test(&tests, &ctx);
        let nfa = NFA::from(&ast);
        assert_eq!(nfa.find_translations("a1b"), []);
        assert_eq!(nfa.find_translations("a22b"), []);
        assert_eq!(nfa.find_translations("a31b"), []);
        assert_eq!(
            nfa.find_translations("a123b"),
            [translation
                .clone()
                .with_offset(1)
                .with_capture("123".to_string())]
        );
        assert_eq!(
            nfa.find_translations("a222b"),
            [translation
                .clone()
                .with_offset(1)
                .with_capture("222".to_string())]
        );
        assert_eq!(
            nfa.find_translations("a321b"),
            [translation
                .clone()
                .with_offset(1)
                .with_capture("321".to_string())]
        );
        assert_eq!(
            nfa.find_translations("aABCb"),
            [translation
                .clone()
                .with_offset(1)
                .with_capture("ABC".to_string())]
        );
        assert_eq!(
            nfa.find_translations("aBBBb"),
            [translation
                .clone()
                .with_offset(1)
                .with_capture("BBB".to_string())]
        );
        assert_eq!(
            nfa.find_translations("aCBAb"),
            [translation
                .clone()
                .with_offset(1)
                .with_capture("CBA".to_string())]
        );
        assert_eq!(nfa.find_translations("bbb"), []);
        assert_eq!(nfa.find_translations("ccc"), []);
        assert_eq!(nfa.find_translations("a"), []);
        assert_eq!(nfa.find_translations("aa"), []);
        assert_eq!(nfa.find_translations("def"), []);
    }
}
