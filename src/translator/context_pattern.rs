//! Store and find context rules using a simple regexp engine ([`Regexp`](crate::translator::regexp))

use std::collections::HashSet;

use crate::parser::multipass::test::Operator;
use crate::parser::{Action, ActionInstruction, HasPrecedence, Quantifier, Test, TestInstruction};

use crate::parser::{AnchoredRule, Attribute, CharacterClass, CharacterClasses};
use crate::translator::effect::{Effect, Environment};
use crate::translator::regexp::{CompiledRegexp, Regexp};
use crate::translator::swap::SwapClasses;
use crate::translator::table::TableContext;
use crate::translator::translation::{
    Translation, TranslationTarget, TranslationTargets, UnresolvedTranslation,
};
use crate::translator::{ResolvedTranslation, TranslationError, TranslationStage};

impl Regexp {
    fn from_test(test: &Test, ctx: &CharacterClasses) -> Self {
        let ast = Regexp::from_instructions(test.tests(), ctx);
        if test.at_end() {
            Regexp::Concat(Box::new(ast), Box::new(Regexp::EndAnchor))
        } else {
            ast
        }
    }

    fn from_instructions(instructions: &[TestInstruction], ctx: &CharacterClasses) -> Self {
        match instructions.len() {
            0 => Regexp::Empty,
            1 => Regexp::from_instruction(&instructions[0], ctx),
            _ => {
                let mut ast = Regexp::from_instruction(&instructions[0], ctx);
                for instruction in &instructions[1..] {
                    let other = Regexp::from_instruction(instruction, ctx);
                    ast = Regexp::Concat(Box::new(ast), Box::new(other));
                }
                ast
            }
        }
    }

    fn from_instruction(instruction: &TestInstruction, ctx: &CharacterClasses) -> Self {
        match instruction {
            TestInstruction::Lookback { .. } => Regexp::NotImplemented, // ignore
            TestInstruction::Variable { var, op, operand } => match op {
                Operator::Eq => Regexp::VariableEqual(*var, *operand),
                // FIXME: operands other than equal do not seem to exist in the wild (at least not
                // in the tables that are shipped with liblouis), so maybe that feature should be
                // removed
                _ => todo!(),
            },
            TestInstruction::String { s } => Regexp::String(s.to_string()),
            TestInstruction::Dots { dots } => Regexp::String(dots.to_string()),
            TestInstruction::Attributes { attrs, quantifier } => {
                Regexp::from_multipass_attributes(attrs, quantifier, ctx)
            }
            TestInstruction::Class { name, quantifier } => {
                Regexp::from_class(name, quantifier, ctx)
            }
            TestInstruction::Negate { test } => Regexp::from_instruction(test, ctx).negate(),
            TestInstruction::Replace { tests } => {
                Regexp::Capture(Box::new(Regexp::from_instructions(tests, ctx)))
            }
        }
    }

    fn from_class(name: &str, quantifier: &Option<Quantifier>, ctx: &CharacterClasses) -> Self {
        let class = CharacterClass::from(name);
        let characters = ctx.get(&class).unwrap_or_default(); // FIXME: should probably fail if we cannot find the class
        if let Some(quantifier) = quantifier {
            match quantifier {
                Quantifier::Number(n) => {
                    Regexp::RepeatExactly(*n, Box::new(Regexp::CharacterClass(characters)))
                }
                Quantifier::Range(min, max) => Regexp::RepeatAtLeastAtMost(
                    *min,
                    *max,
                    Box::new(Regexp::CharacterClass(characters)),
                ),
                Quantifier::OneOrMore => {
                    Regexp::OneOrMore(Box::new(Regexp::CharacterClass(characters)))
                }
            }
        } else {
            Regexp::CharacterClass(characters)
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
                    if let Some(chars) = ctx.get(class) {
                        characters.extend(chars);
                    }
                }
                Attribute::Boundary => (),   // TODO
                Attribute::ByOrder(_) => (), // TODO
                Attribute::Any => (),        // TODO
            }
        }
        if let Some(quantifier) = quantifier {
            match quantifier {
                Quantifier::Number(n) => {
                    Regexp::RepeatExactly(*n, Box::new(Regexp::CharacterClass(characters)))
                }
                Quantifier::Range(min, max) => Regexp::RepeatAtLeastAtMost(
                    *min,
                    *max,
                    Box::new(Regexp::CharacterClass(characters)),
                ),
                Quantifier::OneOrMore => {
                    Regexp::OneOrMore(Box::new(Regexp::CharacterClass(characters)))
                }
            }
        } else {
            Regexp::CharacterClass(characters)
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
            ActionInstruction::Dots { dots } => Ok(TranslationTarget::Literal(dots.to_string())),
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

/// A sequence of [`Effect`]
#[derive(Debug, Clone)]
pub struct Effects(Vec<Effect>);

impl Effects {
    fn from_instructions(values: &[ActionInstruction]) -> Vec<Effect> {
        values
            .iter()
            .cloned()
            .flat_map(Effects::from_instruction)
            .collect()
    }

    fn from_instruction(value: ActionInstruction) -> Option<Effect> {
        if let ActionInstruction::Assignment { variable, value } = value {
            Some(Effect::new(variable, value))
        } else {
            None
        }
    }
}

/// A compiled context regexp paired with whether it is anchored to the beginning of the whole
/// input (liblouis' `` ` `` test anchor). Unlike the end anchor (`~`), the beginning anchor
/// cannot be expressed inside the regexp itself: the regexp only ever sees the remaining suffix
/// of the input, so it has no way to tell whether that suffix starts at position 0 of the
/// original string. Callers of [`ContextPatterns::find`] must supply that externally.
///
/// This is deliberately a simpler shape than `match_pattern.rs`'s `AnchoredMatchRegexp` (one
/// regexp gated by a bool, not two full compiled variants): `` ` ``/`~` are test-level anchors
/// that appear at most once, at the very start/end of the whole test, so "does this rule apply
/// at all" is a single yes/no question — skipping the whole compiled regexp is enough. `match`'s
/// `^` is an *attribute*, usable anywhere a character-class attribute is, including mixed with
/// other attributes in the same set (`%[_~^]`) — the difference isn't "does the rule apply" but
/// "what does this one sub-expression mean here", which has to be baked into the compiled
/// instructions themselves, hence two full variants there instead of a gate on one.
#[derive(Debug)]
struct AnchoredContextRegexp {
    requires_start: bool,
    regexp: CompiledRegexp,
}

#[derive(Debug)]
pub struct ContextPatternsBuilder {
    regexps: Vec<AnchoredContextRegexp>,
}

impl ContextPatternsBuilder {
    pub fn new() -> Self {
        Self {
            regexps: Vec::new(),
        }
    }

    /// Create a translation based on `test`, `action`, `origin` and `swap_classes`
    fn translation(
        &self,
        action: &Action,
        origin: &AnchoredRule,
        stage: TranslationStage,
        swap_classes: &SwapClasses,
    ) -> Result<UnresolvedTranslation, TranslationError> {
        let targets = TranslationTargets::from_instructions(&action.actions(), swap_classes)?;
        let effects = Effects::from_instructions(&action.actions());
        Ok(UnresolvedTranslation::new(
            &targets,
            origin.precedence(),
            stage,
            &effects,
            origin.clone(),
        ))
    }

    pub fn insert(
        &mut self,
        test: &Test,
        action: &Action,
        origin: &AnchoredRule,
        stage: TranslationStage,
        ctx: &TableContext,
    ) -> Result<(), TranslationError> {
        let translation =
            Translation::Unresolved(self.translation(action, origin, stage, ctx.swap_classes())?);
        let test = test.clone().add_implicit_replace();
        // for the multipass stages where we translate from braille to braille the character classes
        // need to be the dots_classes
        let character_classes = match stage {
            TranslationStage::Pre | TranslationStage::Main => ctx.character_classes(),
            TranslationStage::Post1 | TranslationStage::Post2 | TranslationStage::Post3 => {
                ctx.dots_classes()
            }
        };
        let regexp = Regexp::from_test(&test, character_classes).compile_with_payload(translation);
        self.regexps.push(AnchoredContextRegexp {
            requires_start: test.at_beginning(),
            regexp,
        });
        Ok(())
    }

    pub fn build(self) -> ContextPatterns {
        ContextPatterns {
            regexps: self.regexps,
        }
    }
}

#[derive(Debug)]
pub struct ContextPatterns {
    regexps: Vec<AnchoredContextRegexp>,
}

impl ContextPatterns {
    /// `at_start` tells whether `input` begins at position 0 of the whole string being
    /// translated — needed to honor liblouis' `` ` `` ("beginning of input") test anchor.
    pub fn find(&self, input: &str, env: &Environment, at_start: bool) -> Vec<ResolvedTranslation> {
        self.regexps
            .iter()
            .filter(|r| at_start || !r.requires_start)
            .flat_map(|r| r.regexp.find(input, env))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Precedence;
    use crate::parser::RuleParser;
    use crate::parser::multipass::action;
    use crate::parser::multipass::test;

    fn context(class: CharacterClass, chars: &[char]) -> CharacterClasses {
        let mut ctx = CharacterClasses::default();
        for ch in chars {
            ctx.insert(class.clone(), *ch);
        }
        ctx
    }

    #[test]
    fn find_string() {
        let env = Environment::new();
        let tests = test::Parser::new("\"abc\"").tests().unwrap();
        let stage = TranslationStage::Main;
        let ctx = CharacterClasses::default();
        let re = Regexp::from_test(&tests, &ctx).compile();
        assert_eq!(
            re.find("abc", &env).unwrap(),
            ResolvedTranslation::new("", "", 3, stage, None)
        );
        assert_eq!(re.find("def", &env), None);
    }

    #[test]
    fn find_attribute_digit() {
        let env = Environment::new();
        let tests = test::Parser::new("$d").tests().unwrap();
        let stage = TranslationStage::Main;
        let ctx = context(CharacterClass::Digit, &['1', '2', '3']);
        let translation = Translation::Unresolved(UnresolvedTranslation::new(
            &[TranslationTarget::Literal("".to_string())],
            Precedence::Default,
            stage,
            &[],
            None,
        ));
        let regexp = Regexp::from_test(&tests, &ctx);
        let re = regexp.compile_with_payload(translation);
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
        let tests = test::Parser::new("$U").tests().unwrap();
        let stage = TranslationStage::Main;
        let ctx = context(CharacterClass::Uppercase, &['A', 'B', 'C']);
        let translation = Translation::Unresolved(UnresolvedTranslation::new(
            &[TranslationTarget::Literal("".to_string())],
            Precedence::Default,
            stage,
            &[],
            None,
        ));
        let regexp = Regexp::from_test(&tests, &ctx);
        let re = regexp.compile_with_payload(translation);
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
        let translation = Translation::Unresolved(UnresolvedTranslation::new(
            &[TranslationTarget::Literal("".to_string())],
            Precedence::Default,
            stage,
            &[],
            None,
        ));
        let regexp = Regexp::from_test(&tests, &ctx);
        let re = regexp.compile_with_payload(translation);
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
        let tests = test::Parser::new("%letter").tests().unwrap();
        let stage = TranslationStage::Main;
        let ctx = context(CharacterClass::Letter, &['a', 'b', 'c']);
        let translation = Translation::Unresolved(UnresolvedTranslation::new(
            &[TranslationTarget::Literal("".to_string())],
            Precedence::Default,
            stage,
            &[],
            None,
        ));
        let regexp = Regexp::from_test(&tests, &ctx);
        let re = regexp.compile_with_payload(translation);
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
    fn find_character_class_three() {
        let env = Environment::new();
        let tests = test::Parser::new("%letter3").tests().unwrap();
        let stage = TranslationStage::Main;
        let ctx = context(CharacterClass::Letter, &['a', 'b', 'c']);
        let translation = Translation::Unresolved(UnresolvedTranslation::new(
            &[TranslationTarget::Literal("".to_string())],
            Precedence::Default,
            stage,
            &[],
            None,
        ));
        let regexp = Regexp::from_test(&tests, &ctx);
        let re = regexp.compile_with_payload(translation);
        assert_eq!(
            re.find("abc", &env).unwrap(),
            ResolvedTranslation::new("", "", 3, stage, None)
        );
        assert_eq!(
            re.find("bbb", &env).unwrap(),
            ResolvedTranslation::new("", "", 3, stage, None)
        );
        assert_eq!(
            re.find("ccc", &env).unwrap(),
            ResolvedTranslation::new("", "", 3, stage, None)
        );
        assert_eq!(re.find("a", &env), None);
        assert_eq!(re.find("aa", &env), None);
        assert_eq!(re.find("def", &env), None);
    }

    #[test]
    fn find_character_class_any() {
        let env = Environment::new();
        let tests = test::Parser::new("%letter.").tests().unwrap();
        let stage = TranslationStage::Main;
        let ctx = context(CharacterClass::Letter, &['a', 'b', 'c']);
        let translation = Translation::Unresolved(UnresolvedTranslation::new(
            &[TranslationTarget::Literal("".to_string())],
            Precedence::Default,
            stage,
            &[],
            None,
        ));
        let regexp = Regexp::from_test(&tests, &ctx);
        let re = regexp.compile_with_payload(translation);
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
        assert_eq!(re.find("y", &env), None);
        assert_eq!(
            re.find("bbb", &env).unwrap(),
            ResolvedTranslation::new("", "", 3, stage, None)
        );
        assert_eq!(
            re.find("ccc", &env).unwrap(),
            ResolvedTranslation::new("", "", 3, stage, None)
        );
        assert_eq!(re.find("def", &env), None);
    }

    #[test]
    fn find_capture_with_character_class() {
        let env = Environment::new();
        let tests = test::Parser::new("\"a\"[%digit]\"b\"").tests().unwrap();
        let ctx = context(CharacterClass::Digit, &['1', '2', '3']);
        let translation = Translation::Unresolved(UnresolvedTranslation::new(
            &[TranslationTarget::Literal("".to_string())],
            Precedence::Default,
            TranslationStage::Main,
            &[],
            None,
        ));
        let regexp = Regexp::from_test(&tests, &ctx);
        let re = regexp.compile_with_payload(translation);
        assert_eq!(
            re.find("a1b", &env).unwrap(),
            ResolvedTranslation::new("1", "", 3, TranslationStage::Main, None).with_offset(1)
        );
        assert_eq!(
            re.find("a2b", &env).unwrap(),
            ResolvedTranslation::new("2", "", 3, TranslationStage::Main, None).with_offset(1)
        );
        assert_eq!(
            re.find("a3b", &env).unwrap(),
            ResolvedTranslation::new("3", "", 3, TranslationStage::Main, None).with_offset(1)
        );
        assert_eq!(re.find("bbb", &env), None);
        assert_eq!(re.find("ccc", &env), None);
        assert_eq!(re.find("a", &env), None);
        assert_eq!(re.find("aa", &env), None);
        assert_eq!(re.find("def", &env), None);
    }

    #[test]
    fn find_capture_with_attribute() {
        let env = Environment::new();
        let tests = test::Parser::new("\"a\"[$dU]\"b\"").tests().unwrap();
        let mut ctx = context(CharacterClass::Digit, &['1', '2', '3']);
        for c in ['A', 'B', 'C'] {
            ctx.insert(CharacterClass::Uppercase, c);
        }
        let translation = Translation::Unresolved(UnresolvedTranslation::new(
            &[TranslationTarget::Literal("".to_string())],
            Precedence::Default,
            TranslationStage::Main,
            &[],
            None,
        ));
        let regexp = Regexp::from_test(&tests, &ctx);
        let re = regexp.compile_with_payload(translation);
        assert_eq!(
            re.find("a1b", &env).unwrap(),
            ResolvedTranslation::new("1", "", 3, TranslationStage::Main, None).with_offset(1)
        );
        assert_eq!(
            re.find("a2b", &env).unwrap(),
            ResolvedTranslation::new("2", "", 3, TranslationStage::Main, None).with_offset(1)
        );
        assert_eq!(
            re.find("a3b", &env).unwrap(),
            ResolvedTranslation::new("3", "", 3, TranslationStage::Main, None).with_offset(1)
        );
        assert_eq!(
            re.find("aAb", &env).unwrap(),
            ResolvedTranslation::new("A", "", 3, TranslationStage::Main, None).with_offset(1)
        );
        assert_eq!(
            re.find("aBb", &env).unwrap(),
            ResolvedTranslation::new("B", "", 3, TranslationStage::Main, None).with_offset(1)
        );
        assert_eq!(
            re.find("aCb", &env).unwrap(),
            ResolvedTranslation::new("C", "", 3, TranslationStage::Main, None).with_offset(1)
        );
        assert_eq!(re.find("bbb", &env), None);
        assert_eq!(re.find("ccc", &env), None);
        assert_eq!(re.find("a", &env), None);
        assert_eq!(re.find("aa", &env), None);
        assert_eq!(re.find("def", &env), None);
    }

    #[test]
    fn find_capture_with_attribute_and_quantifier() {
        let env = Environment::new();
        let tests = test::Parser::new("\"a\"[$dU3]\"b\"").tests().unwrap();
        let mut ctx = context(CharacterClass::Digit, &['1', '2', '3']);
        for c in ['A', 'B', 'C'] {
            ctx.insert(CharacterClass::Uppercase, c);
        }
        let translation = Translation::Unresolved(UnresolvedTranslation::new(
            &[TranslationTarget::Literal("".to_string())],
            Precedence::Default,
            TranslationStage::Main,
            &[],
            None,
        ));
        let regexp = Regexp::from_test(&tests, &ctx);
        let re = regexp.compile_with_payload(translation);
        assert_eq!(re.find("a1b", &env), None);
        assert_eq!(re.find("a22b", &env), None);
        assert_eq!(re.find("a31b", &env), None);
        assert_eq!(
            re.find("a123b", &env).unwrap(),
            ResolvedTranslation::new("123", "", 5, TranslationStage::Main, None).with_offset(1)
        );
        assert_eq!(
            re.find("a222b", &env).unwrap(),
            ResolvedTranslation::new("222", "", 5, TranslationStage::Main, None).with_offset(1)
        );
        assert_eq!(
            re.find("a321b", &env).unwrap(),
            ResolvedTranslation::new("321", "", 5, TranslationStage::Main, None).with_offset(1)
        );
        assert_eq!(
            re.find("aABCb", &env).unwrap(),
            ResolvedTranslation::new("ABC", "", 5, TranslationStage::Main, None).with_offset(1)
        );
        assert_eq!(
            re.find("aBBBb", &env).unwrap(),
            ResolvedTranslation::new("BBB", "", 5, TranslationStage::Main, None).with_offset(1)
        );
        assert_eq!(
            re.find("aCBAb", &env).unwrap(),
            ResolvedTranslation::new("CBA", "", 5, TranslationStage::Main, None).with_offset(1)
        );
        assert_eq!(re.find("bbb", &env), None);
        assert_eq!(re.find("ccc", &env), None);
        assert_eq!(re.find("a", &env), None);
        assert_eq!(re.find("aa", &env), None);
        assert_eq!(re.find("def", &env), None);
    }

    // just create some fake anchored rule for testing purposes
    fn origin(line: &str) -> AnchoredRule {
        let rule = RuleParser::new(line).rule().unwrap();
        AnchoredRule::new(rule, None, 0)
    }

    #[test]
    fn context_simple() {
        let env = Environment::new();
        let tests = test::Parser::new("\"abc\"").tests().unwrap();
        let action = action::Parser::new("\"A_B_C\"").actions().unwrap();
        let origin = origin("context \"abc\" \"A_B_C\"");
        let stage = TranslationStage::Main;
        let mut builder = ContextPatternsBuilder::new();
        builder
            .insert(&tests, &action, &origin, stage, &TableContext::default())
            .unwrap();
        let translation =
            ResolvedTranslation::new("abc", "A_B_C", 3, TranslationStage::Main, origin.clone());
        let patterns = builder.build();
        assert_eq!(patterns.find("abc", &env, true), [translation]);
        assert_eq!(patterns.find("def", &env, true), []);
    }

    #[test]
    fn context_capture() {
        let env = Environment::new();
        let tests = test::Parser::new(r#"["abc"]"#).tests().unwrap();
        let action = action::Parser::new(r#""<"*">""#).actions().unwrap();
        let origin = origin(r#"context "abc" "<"*">""#);
        let stage = TranslationStage::Main;
        let mut builder = ContextPatternsBuilder::new();
        builder
            .insert(&tests, &action, &origin, stage, &TableContext::default())
            .unwrap();
        let translation =
            ResolvedTranslation::new("abc", "<abc>", 3, TranslationStage::Main, origin.clone());
        let patterns = builder.build();
        assert_eq!(patterns.find("abc", &env, true), [translation]);
        assert!(patterns.find("def", &env, true).is_empty());
    }

    #[test]
    fn context_at_beginning_anchor() {
        let env = Environment::new();
        let tests = test::Parser::new(r#"`"a""#).tests().unwrap();
        let action = action::Parser::new(r#""X""#).actions().unwrap();
        let origin = origin(r#"context `"a" "X""#);
        let stage = TranslationStage::Main;
        let mut builder = ContextPatternsBuilder::new();
        builder
            .insert(&tests, &action, &origin, stage, &TableContext::default())
            .unwrap();
        let translation =
            ResolvedTranslation::new("a", "X", 1, TranslationStage::Main, origin.clone());
        let patterns = builder.build();
        // matches when `input` truly starts at position 0 of the whole string being translated
        assert_eq!(patterns.find("a", &env, true), [translation]);
        // does not match when we are mid-string, even though the substring looks identical
        assert!(patterns.find("a", &env, false).is_empty());
    }

    #[test]
    fn context_at_end_anchor() {
        let env = Environment::new();
        let tests = test::Parser::new(r#""a"~"#).tests().unwrap();
        let action = action::Parser::new(r#""X""#).actions().unwrap();
        let origin = origin(r#"context "a"~ "X""#);
        let stage = TranslationStage::Main;
        let mut builder = ContextPatternsBuilder::new();
        builder
            .insert(&tests, &action, &origin, stage, &TableContext::default())
            .unwrap();
        let translation =
            ResolvedTranslation::new("a", "X", 1, TranslationStage::Main, origin.clone());
        let patterns = builder.build();
        // matches when "a" is exactly the whole remaining input
        assert_eq!(patterns.find("a", &env, true), [translation]);
        // does not match when there is trailing input after "a"
        assert!(patterns.find("ab", &env, true).is_empty());
    }

    #[test]
    fn context_beginning_and_end_anchor_regression() {
        // Regression test for da-dk-g26.ctb's `["-"]~ @36-36: a hyphen that is both anchored to
        // the beginning and the end of input (i.e. the whole input is exactly "-") must not match
        // when there is trailing text after the hyphen, e.g. "-0". Before this fix, the `~`
        // anchor was silently ignored, so this rule matched "-0" and doubled the hyphen cell.
        let env = Environment::new();
        let tests = test::Parser::new(r#"`["-"]~"#).tests().unwrap();
        let action = action::Parser::new("*").actions().unwrap();
        let origin = origin(r#"context `["-"]~ *"#);
        let stage = TranslationStage::Main;
        let mut builder = ContextPatternsBuilder::new();
        builder
            .insert(&tests, &action, &origin, stage, &TableContext::default())
            .unwrap();
        let translation =
            ResolvedTranslation::new("-", "-", 1, TranslationStage::Main, origin.clone());
        let patterns = builder.build();
        assert_eq!(patterns.find("-", &env, true), [translation]);
        assert!(patterns.find("-0", &env, true).is_empty());
        assert!(patterns.find("-", &env, false).is_empty());
    }

    #[test]
    fn context_capture_with_class() {
        let env = Environment::new();
        let tests = test::Parser::new("@135[%accent]@13456").tests().unwrap();
        let action = action::Parser::new("*@136").actions().unwrap();
        let origin = origin("pass2 @135[%accent]@13456 *@136");
        let stage = TranslationStage::Post1;
        let ctx = TableContext::new(
            CharacterClasses::new(&[(CharacterClass::UserDefined("accent".to_string()), &['΄'])]),
            CharacterClasses::new(&[(CharacterClass::UserDefined("accent".to_string()), &['⠐'])]),
            SwapClasses::default(),
        );
        let mut builder = ContextPatternsBuilder::new();
        builder
            .insert(&tests, &action, &origin, stage, &ctx)
            .unwrap();
        let translation =
            ResolvedTranslation::new("⠐", "⠐⠥", 3, TranslationStage::Post1, origin.clone())
                .with_offset(1);
        let patterns = builder.build();
        assert_eq!(patterns.find("⠕⠐⠽", &env, true), [translation]);
        assert!(patterns.find("⠕", &env, true).is_empty());
    }

    #[test]
    fn context_capture_advanced() {
        let env = Environment::new();
        let tests = test::Parser::new(r#"$p3["abc"]$p3"#).tests().unwrap();
        let action = action::Parser::new(r#""<"*">""#).actions().unwrap();
        let context = TableContext::new(
            CharacterClasses::new(&[(CharacterClass::Punctuation, &['{', '}'])]),
            CharacterClasses::default(),
            SwapClasses::default(),
        );
        let origin = origin(r#"context $p3"abc"$p3 "<"*">""#);
        let stage = TranslationStage::Main;
        let mut builder = ContextPatternsBuilder::new();
        builder
            .insert(&tests, &action, &origin, stage, &context)
            .unwrap();
        let translation =
            ResolvedTranslation::new("abc", "<abc>", 9, TranslationStage::Main, origin.clone())
                .with_offset(3);
        let patterns = builder.build();
        assert_eq!(patterns.find("{{{abc}}}", &env, true), [translation]);
        assert!(patterns.find("def", &env, true).is_empty());
    }

    #[test]
    fn context_swap() {
        let env = Environment::new();
        let tests = test::Parser::new(r#"$p3["abc"]$p3"#).tests().unwrap();
        let action = action::Parser::new(r#""<"%foo">""#).actions().unwrap();
        let context = TableContext::new(
            CharacterClasses::new(&[(CharacterClass::Punctuation, &['{', '}'])]),
            CharacterClasses::default(),
            SwapClasses::new(&[("foo", &[('a', "A"), ('b', "B"), ('c', "C")])]),
        );
        let origin = origin(r#"context $p3"abc"$p3 "<"%foo">""#);
        let stage = TranslationStage::Main;
        let mut builder = ContextPatternsBuilder::new();
        builder
            .insert(&tests, &action, &origin, stage, &context)
            .unwrap();
        let translation =
            ResolvedTranslation::new("abc", "<ABC>", 9, TranslationStage::Main, origin.clone())
                .with_offset(3);
        let patterns = builder.build();
        assert_eq!(patterns.find("{{{abc}}}", &env, true), [translation]);
        assert!(patterns.find("def", &env, true).is_empty());
    }
}
