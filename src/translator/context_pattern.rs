//! Store and find context rules using a simple regexp engine ([`nfa`](crate::translator::nfa))

use std::collections::HashSet;

use crate::parser::{
    Action, ActionInstruction, IsLiteral, Quantifier, Test, TestInstruction, dots_to_unicode,
};

use crate::parser::{AnchoredRule, Attribute, CharacterClass, CharacterClasses};
use crate::translator::nfa::{AST, NFA};
use crate::translator::swap::SwapClasses;
use crate::translator::translation::{TranslateTo, TranslationTarget, TranslationTargets};
use crate::translator::{Translation, TranslationError, TranslationStage};

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

impl TranslationTarget {
    fn from_instruction(
        value: ActionInstruction,
        ctx: &SwapClasses,
    ) -> Result<Self, TranslationError> {
        match value {
            ActionInstruction::String { s } => Ok(TranslationTarget::Literal(s)),
            ActionInstruction::Dots { dots } => {
                Ok(TranslationTarget::Literal(dots_to_unicode(&dots)))
            }
            ActionInstruction::SwapClass { name } => {
                let swapper = ctx
                    .get(&name)
                    .ok_or(TranslationError::SwapClassNotDefined(name))?;
                Ok(TranslationTarget::Swap(swapper))
            }
            ActionInstruction::Replace => Ok(TranslationTarget::Capture),
            ActionInstruction::Ignore => Ok(TranslationTarget::Literal("".to_string())),
            // ignore instructions that change the environment
            ActionInstruction::Assignment { .. }
            | ActionInstruction::Increment { .. }
            | ActionInstruction::Decrement { .. } => Ok(TranslationTarget::Literal("".to_string())),
        }
    }
}

impl TranslationTargets {
    fn from_instructions(
        values: &[ActionInstruction],
        ctx: &SwapClasses,
    ) -> Result<Vec<TranslationTarget>, TranslationError> {
        values
            .iter()
            .cloned()
            .map(|instruction| TranslationTarget::from_instruction(instruction, ctx))
            .collect()
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

    /// Create a translation based on `test`, `action`, `origin` and `swap_classes`
    fn translation(
        &self,
        test: &Test,
        action: &Action,
        origin: &AnchoredRule,
        swap_classes: &SwapClasses,
    ) -> Result<Translation, TranslationError> {
        let targets = TranslationTargets::from_instructions(&action.actions(), &swap_classes)?;
        let to = TranslateTo::from_seq(&targets);
        let from = if test.is_literal() {
            test.try_into().unwrap()
        } else {
            "".to_string()
        };
        let translation = if action.is_literal() {
            // if the action is literal then we can just resolve the from with any capture input and
            // it will return a resolved string
            let to = to.resolve("does not matter").to_string();
            Translation::new(
                &from,
                to.as_str(),
                0,
                TranslationStage::Main,
                origin.clone(),
            )
        } else {
            Translation::new(&from, "", 0, TranslationStage::Main, origin.clone()).with_output(&to)
        };
        Ok(translation)
    }

    pub fn insert(
        &mut self,
        test: &Test,
        action: &Action,
        origin: &AnchoredRule,
        character_classes: &CharacterClasses,
        swap_classes: &SwapClasses,
    ) -> Result<(), TranslationError> {
        let translation = self.translation(test, action, origin, swap_classes)?;
        let ast = AST::from_test(test, &character_classes);
        self.nfa.merge_accepting_fragment(&ast, translation);
        Ok(())
    }

    pub fn find_translations(&self, input: &str) -> Vec<Translation> {
        self.nfa.find_translations(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::RuleParser;
    use crate::parser::action;
    use crate::parser::test;

    fn context(class: CharacterClass, chars: &[char]) -> CharacterClasses {
        let mut ctx = CharacterClasses::default();
        for ch in chars {
            ctx.insert(class.clone(), *ch);
        }
        ctx
    }

    /// Create an anchored rule for testing purposes
    fn test_origin(line: &str) -> AnchoredRule {
        let rule = RuleParser::new(line).rule().unwrap();
        AnchoredRule::new(rule, None, 0)
    }

    #[test]
    fn find_string() {
        let tests = test::Parser::new("\"abc\"").tests().unwrap();
        let stage = TranslationStage::Main;
        let ctx = CharacterClasses::default();
        let ast = AST::from_test(&tests, &ctx);
        let nfa = NFA::from(&ast);
        assert_eq!(
            nfa.find_translations("abc"),
            vec![Translation::new("", "", 3, stage, None)]
        );
        assert!(nfa.find_translations("def").is_empty());
    }

    #[test]
    fn find_attribute_digit() {
        let tests = test::Parser::new("$d").tests().unwrap();
        let stage = TranslationStage::Main;
        let ctx = context(CharacterClass::Digit, &['1', '2', '3']);
        let ast = AST::from_test(&tests, &ctx);
        let nfa = NFA::from(&ast);
        assert_eq!(
            nfa.find_translations("1"),
            vec![Translation::new("", "", 1, stage, None)]
        );
        assert_eq!(
            nfa.find_translations("2"),
            vec![Translation::new("", "", 1, stage, None)]
        );
        assert_eq!(
            nfa.find_translations("3"),
            vec![Translation::new("", "", 1, stage, None)]
        );
        assert!(nfa.find_translations("def").is_empty());
    }

    #[test]
    fn find_attribute_uppercase() {
        let tests = test::Parser::new("$U").tests().unwrap();
        let stage = TranslationStage::Main;
        let ctx = context(CharacterClass::Uppercase, &['A', 'B', 'C']);
        let ast = AST::from_test(&tests, &ctx);
        let nfa = NFA::from(&ast);
        assert_eq!(
            nfa.find_translations("A"),
            vec![Translation::new("", "", 1, stage, None)]
        );
        assert_eq!(
            nfa.find_translations("A"),
            vec![Translation::new("", "", 1, stage, None)]
        );
        assert_eq!(
            nfa.find_translations("C"),
            vec![Translation::new("", "", 1, stage, None)]
        );
        assert!(nfa.find_translations("def").is_empty());
    }

    #[test]
    fn find_attribute_uppercase_punctuation_or_sign() {
        let tests = test::Parser::new("$USp").tests().unwrap();
        let mut ctx = CharacterClasses::default();
        let stage = TranslationStage::Main;
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
        assert_eq!(
            nfa.find_translations("%"),
            vec![Translation::new("", "", 1, stage, None)]
        );
        assert_eq!(
            nfa.find_translations("."),
            vec![Translation::new("", "", 1, stage, None)]
        );
        assert_eq!(
            nfa.find_translations("A"),
            vec![Translation::new("", "", 1, stage, None)]
        );
        assert!(nfa.find_translations("def").is_empty());
    }

    #[test]
    fn find_character_class() {
        let tests = test::Parser::new("%letter").tests().unwrap();
        let stage = TranslationStage::Main;
        let ctx = context(CharacterClass::Letter, &['a', 'b', 'c']);
        let ast = AST::from_test(&tests, &ctx);
        let nfa = NFA::from(&ast);
        assert_eq!(
            nfa.find_translations("a"),
            vec![Translation::new("", "", 1, stage, None)]
        );
        assert_eq!(
            nfa.find_translations("b"),
            vec![Translation::new("", "", 1, stage, None)]
        );
        assert_eq!(
            nfa.find_translations("c"),
            vec![Translation::new("", "", 1, stage, None)]
        );
        assert!(nfa.find_translations("def").is_empty());
    }

    #[test]
    fn find_character_class_three() {
        let tests = test::Parser::new("%letter3").tests().unwrap();
        let stage = TranslationStage::Main;
        let ctx = context(CharacterClass::Letter, &['a', 'b', 'c']);
        let ast = AST::from_test(&tests, &ctx);
        let nfa = NFA::from(&ast);
        assert_eq!(
            nfa.find_translations("abc"),
            vec![Translation::new("", "", 3, stage, None)]
        );
        assert_eq!(
            nfa.find_translations("bbb"),
            vec![Translation::new("", "", 3, stage, None)]
        );
        assert_eq!(
            nfa.find_translations("ccc"),
            vec![Translation::new("", "", 3, stage, None)]
        );
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
            [translation.clone().with_offset(1).with_capture("1")]
        );
        assert_eq!(
            nfa.find_translations("a2b"),
            [translation.clone().with_offset(1).with_capture("2")]
        );
        assert_eq!(
            nfa.find_translations("a3b"),
            [translation.clone().with_offset(1).with_capture("3")]
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
            [translation.clone().with_offset(1).with_capture("1")]
        );
        assert_eq!(
            nfa.find_translations("a2b"),
            [translation.clone().with_offset(1).with_capture("2")]
        );
        assert_eq!(
            nfa.find_translations("a3b"),
            [translation.clone().with_offset(1).with_capture("3")]
        );
        assert_eq!(
            nfa.find_translations("aAb"),
            [translation.clone().with_offset(1).with_capture("A")]
        );
        assert_eq!(
            nfa.find_translations("aBb"),
            [translation.clone().with_offset(1).with_capture("B")]
        );
        assert_eq!(
            nfa.find_translations("aCb"),
            [translation.clone().with_offset(1).with_capture("C")]
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
            [translation.clone().with_offset(1).with_capture("123")]
        );
        assert_eq!(
            nfa.find_translations("a222b"),
            [translation.clone().with_offset(1).with_capture("222")]
        );
        assert_eq!(
            nfa.find_translations("a321b"),
            [translation.clone().with_offset(1).with_capture("321")]
        );
        assert_eq!(
            nfa.find_translations("aABCb"),
            [translation.clone().with_offset(1).with_capture("ABC")]
        );
        assert_eq!(
            nfa.find_translations("aBBBb"),
            [translation.clone().with_offset(1).with_capture("BBB")]
        );
        assert_eq!(
            nfa.find_translations("aCBAb"),
            [translation.clone().with_offset(1).with_capture("CBA")]
        );
        assert_eq!(nfa.find_translations("bbb"), []);
        assert_eq!(nfa.find_translations("ccc"), []);
        assert_eq!(nfa.find_translations("a"), []);
        assert_eq!(nfa.find_translations("aa"), []);
        assert_eq!(nfa.find_translations("def"), []);
    }

    // just create some fake anchored rule for testing purposes
    fn origin(line: &str) -> AnchoredRule {
        let rule = RuleParser::new(line).rule().unwrap();
        AnchoredRule::new(rule, None, 0)
    }

    #[test]
    fn context_simple() {
        let tests = test::Parser::new("\"abc\"").tests().unwrap();
        let action = action::Parser::new("\"A_B_C\"").actions().unwrap();
        let character_classes = CharacterClasses::default();
        let swap_classes = SwapClasses::default();
        let origin = origin("context \"abc\" \"A_B_C\"");
        let mut patterns = ContextPatterns::new();
        patterns
            .insert(&tests, &action, &origin, &character_classes, &swap_classes)
            .unwrap();
        let translation = Translation::new("abc", "", 3, TranslationStage::Main, origin.clone())
            .with_output(&TranslateTo::Resolved("A_B_C".to_string()));
        assert_eq!(patterns.find_translations("abc"), [translation]);
        assert!(patterns.find_translations("def").is_empty());
    }

    #[test]
    fn context_capture() {
        let tests = test::Parser::new(r#"["abc"]"#).tests().unwrap();
        let action = action::Parser::new(r#""<"*">""#).actions().unwrap();
        let character_classes = CharacterClasses::default();
        let swap_classes = SwapClasses::default();
        let origin = origin(r#"context "abc" "<"*">""#);
        let mut patterns = ContextPatterns::new();
        patterns
            .insert(&tests, &action, &origin, &character_classes, &swap_classes)
            .unwrap();
        let translation = Translation::new("abc", "", 3, TranslationStage::Main, origin.clone())
            .with_output(&TranslateTo::Resolved("<abc>".to_string()));
        assert_eq!(patterns.find_translations("abc"), [translation]);
        assert!(patterns.find_translations("def").is_empty());
    }

    #[test]
    fn context_capture_advanced() {
        let tests = test::Parser::new(r#"$p3["abc"]$p3"#).tests().unwrap();
        let action = action::Parser::new(r#""<"*">""#).actions().unwrap();
        let character_classes =
            CharacterClasses::new(&[(CharacterClass::Punctuation, &['{', '}'])]);
        let swap_classes = SwapClasses::default();
        let origin = origin(r#"context $p3"abc"$p3 "<"*">""#);
        let mut patterns = ContextPatterns::new();
        patterns
            .insert(&tests, &action, &origin, &character_classes, &swap_classes)
            .unwrap();
        let translation = Translation::new("abc", "", 9, TranslationStage::Main, origin.clone())
            .with_offset(3)
            .with_output(&TranslateTo::Resolved("<abc>".to_string()));
        assert_eq!(patterns.find_translations("{{{abc}}}"), [translation]);
        assert!(patterns.find_translations("def").is_empty());
    }

    #[test]
    fn context_swap() {
        let tests = test::Parser::new(r#"$p3["abc"]$p3"#).tests().unwrap();
        let action = action::Parser::new(r#""<"%foo">""#).actions().unwrap();
        let character_classes =
            CharacterClasses::new(&[(CharacterClass::Punctuation, &['{', '}'])]);
        let swap_classes = SwapClasses::new(&[("foo", &[('a', "A"), ('b', "B"), ('c', "C")])]);
        let origin = origin(r#"context $p3"abc"$p3 "<"%foo">""#);
        let mut patterns = ContextPatterns::new();
        patterns
            .insert(&tests, &action, &origin, &character_classes, &swap_classes)
            .unwrap();
        let translation = Translation::new("abc", "", 9, TranslationStage::Main, origin.clone())
            .with_offset(3)
            .with_output(&TranslateTo::Resolved("<ABC>".to_string()));
        assert_eq!(patterns.find_translations("{{{abc}}}"), [translation]);
        assert!(patterns.find_translations("def").is_empty());
    }
}
