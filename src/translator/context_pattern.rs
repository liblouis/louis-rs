//! Store and find context rules using a simple regexp engine ([`Regexp`](crate::translator::regexp))

use std::collections::HashSet;

use crate::parser::{Action, ActionInstruction, Quantifier, Test, TestInstruction};

use crate::parser::multipass::test::Operator;
use crate::parser::{AnchoredRule, Attribute, CharacterClass, CharacterClasses};
use crate::translator::effect::{Effect, Environment};
use crate::translator::regexp::{Comparison, CompiledRegexp, Regexp};
use crate::translator::swap::SwapClasses;
use crate::translator::table::TableContext;
use crate::translator::translation::{
    Translation, TranslationTarget, TranslationTargets, UnresolvedTranslation,
};
use crate::translator::{
    CharacterDefinition, CompileError, CompileErrorKind, ResolvedTranslation, TranslationStage,
};

impl Regexp {
    /// The body and the look-ahead, which is matched but never consumed, see [`Test`]. The
    /// look-behind is a program of its own, see [`ContextRegexp`].
    fn from_test(test: &Test, whole_match: bool, ctx: &CharacterClasses) -> Self {
        let body = Regexp::from_instructions(test.body(), whole_match, ctx);
        let body = if whole_match {
            Regexp::Consume(Box::new(body))
        } else {
            body
        };
        let mut regexp = body;
        if !test.lookahead().is_empty() {
            let lookahead = Regexp::from_instructions(test.lookahead(), whole_match, ctx);
            regexp = Regexp::Concat(Box::new(regexp), Box::new(lookahead));
        }
        regexp
    }

    fn from_instructions(
        instructions: &[TestInstruction],
        whole_match: bool,
        ctx: &CharacterClasses,
    ) -> Self {
        match instructions.len() {
            0 => Regexp::Empty,
            1 => Regexp::from_instruction(&instructions[0], whole_match, ctx),
            _ => {
                let mut ast = Regexp::from_instruction(&instructions[0], whole_match, ctx);
                for instruction in &instructions[1..] {
                    let other = Regexp::from_instruction(instruction, whole_match, ctx);
                    ast = Regexp::Concat(Box::new(ast), Box::new(other));
                }
                ast
            }
        }
    }

    fn from_instruction(
        instruction: &TestInstruction,
        whole_match: bool,
        ctx: &CharacterClasses,
    ) -> Self {
        match instruction {
            TestInstruction::Variable { var, op, operand } => {
                Regexp::VariableCompare(*var, (*op).into(), *operand)
            }
            TestInstruction::String { s } => Regexp::String(s.to_string()),
            TestInstruction::Dots { dots } => Regexp::String(dots.to_string()),
            TestInstruction::Attributes { attrs, quantifier } => {
                Regexp::from_multipass_attributes(attrs, quantifier, ctx)
            }
            TestInstruction::Class { name, quantifier } => {
                Regexp::from_class(name, quantifier, ctx)
            }
            TestInstruction::Negate { test } => {
                Regexp::from_instruction(test, whole_match, ctx).negate()
            }
            TestInstruction::Replace { tests } => {
                let regexp =
                    Regexp::Capture(Box::new(Regexp::from_instructions(tests, whole_match, ctx)));
                if whole_match {
                    regexp
                } else {
                    Regexp::Consume(Box::new(regexp))
                }
            }
            TestInstruction::AtBeginning => Regexp::StartAnchor,
            TestInstruction::AtEnd => Regexp::EndAnchor,
        }
    }

    fn quantified(self, quantifier: &Option<Quantifier>) -> Self {
        match quantifier {
            None => self,
            Some(Quantifier::Number(n)) => Regexp::RepeatExactly(*n, Box::new(self)),
            Some(Quantifier::Range(min, max)) => {
                Regexp::RepeatAtLeastAtMost(*min, *max, Box::new(self))
            }
            Some(Quantifier::OneOrMore) => Regexp::OneOrMore(Box::new(self)),
        }
    }

    fn from_class(name: &str, quantifier: &Option<Quantifier>, ctx: &CharacterClasses) -> Self {
        let class = CharacterClass::from(name);
        let characters = ctx.get(&class).unwrap_or_default(); // FIXME: should probably fail if we cannot find the class
        Regexp::CharacterClass(characters).quantified(quantifier)
    }

    fn from_multipass_attributes(
        attrs: &HashSet<Attribute>,
        quantifier: &Option<Quantifier>,
        ctx: &CharacterClasses,
    ) -> Self {
        // liblouis compiles `a` to the full attribute mask, so it matches any character no
        // matter what else is in the list -- an alternative with everything else is pointless
        if attrs.contains(&Attribute::Any) {
            return Regexp::Any.quantified(quantifier);
        }
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
                Attribute::Any => unreachable!("handled above"),
            }
        }
        Regexp::CharacterClass(characters).quantified(quantifier)
    }
}

impl TranslationTarget {
    fn from_instruction(
        value: ActionInstruction,
        ctx: &SwapClasses,
        capture_target: &TranslationTarget,
    ) -> Result<Self, CompileErrorKind> {
        match value {
            ActionInstruction::String { s } => Ok(TranslationTarget::Literal(s)),
            ActionInstruction::Dots { dots } => Ok(TranslationTarget::Literal(dots.to_string())),
            ActionInstruction::SwapClass { name } => {
                let swapper = ctx
                    .get(&name)
                    .ok_or(CompileErrorKind::SwapClassNotDefined(name))?;
                Ok(TranslationTarget::Swap(swapper))
            }
            ActionInstruction::Replace => Ok(capture_target.clone()),
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
        capture_target: &TranslationTarget,
    ) -> Result<Vec<TranslationTarget>, CompileErrorKind> {
        values
            .iter()
            .cloned()
            .map(|instruction| {
                TranslationTarget::from_instruction(instruction, ctx, capture_target)
            })
            .collect()
    }
}

impl From<Operator> for Comparison {
    fn from(operator: Operator) -> Self {
        match operator {
            Operator::Eq => Comparison::Eq,
            Operator::Lt => Comparison::Lt,
            Operator::Gt => Comparison::Gt,
            Operator::LtEq => Comparison::LtEq,
            Operator::GtEq => Comparison::GtEq,
        }
    }
}

/// The effects of the instructions that change variables
fn effects(instructions: &[ActionInstruction]) -> Vec<Effect> {
    instructions
        .iter()
        .filter_map(|instruction| match *instruction {
            ActionInstruction::Assignment { variable, value } => Some(Effect::new(variable, value)),
            ActionInstruction::Increment { variable } => Some(Effect::increment(variable)),
            ActionInstruction::Decrement { variable } => Some(Effect::decrement(variable)),
            _ => None,
        })
        .collect()
}

#[derive(Debug)]
pub struct ContextPatternsBuilder {
    regexps: Vec<ContextRegexp>,
}

/// A rule's test as two programs, like a `match` rule's: the look-behind reversed, to run
/// over the reversed text before the cursor (`None` when there is none), and the body
/// followed by the look-ahead, to run from the cursor.
#[derive(Debug)]
struct ContextRegexp {
    lookbehind: Option<CompiledRegexp>,
    main: CompiledRegexp,
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
        capture_target: &TranslationTarget,
    ) -> Result<UnresolvedTranslation, CompileError> {
        let targets =
            TranslationTargets::from_instructions(&action.actions(), swap_classes, capture_target)
                .map_err(|e| e.at(origin))?;
        let effects = effects(&action.actions());
        Ok(UnresolvedTranslation::new(
            &targets,
            stage,
            &effects,
            origin.clone(),
        ))
    }

    /// Add a `context`, `correct` or pass rule: compile its `test` into a regular expression
    /// that carries the translation made from its `action`.
    ///
    /// `capture_target` is what the action's `*` turns into, the characters between `[` and
    /// `]`: as they are in the passes, translated by the character definitions in a forward
    /// `context` rule.
    pub fn insert(
        &mut self,
        test: &Test,
        action: &Action,
        origin: &AnchoredRule,
        stage: TranslationStage,
        ctx: &TableContext,
        capture_target: &TranslationTarget,
    ) -> Result<(), CompileError> {
        let translation = Translation::Unresolved(self.translation(
            action,
            origin,
            stage,
            ctx.swap_classes(),
            capture_target,
        )?);
        let test = test.clone().add_implicit_replace();
        // for the multipass stages where we translate from braille to braille the character classes
        // need to be the dots_classes
        let character_classes = match stage {
            TranslationStage::Pre | TranslationStage::Main => ctx.character_classes(),
            TranslationStage::Post1 | TranslationStage::Post2 | TranslationStage::Post3 => {
                ctx.dots_classes()
            }
            // a display table is a plain character mapping, it has no context rules
            TranslationStage::Display => unreachable!(),
        };
        let main = Regexp::from_test(&test, action.consumes_whole_match(), character_classes)
            .compile_with_payload(translation);
        let lookbehind = (!test.lookbehind().is_empty()).then(|| {
            Regexp::from_instructions(test.lookbehind(), false, character_classes)
                .reverse()
                .compile()
        });
        self.regexps.push(ContextRegexp { lookbehind, main });
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
    regexps: Vec<ContextRegexp>,
}

impl ContextPatterns {
    /// The rules that match at the cursor: their body and look-ahead match `input`, the text
    /// from the cursor on, and their look-behind matches `preceding`, the text before the
    /// cursor, which must be reversed so that it reads backwards from the cursor.
    ///
    /// `at_start` tells whether `input` begins at position 0 of the whole string being
    /// translated — needed to honor liblouis' `` ` `` ("beginning of input") test anchor,
    /// which can appear anywhere in a compiled pattern (see [`Regexp::StartAnchor`]).
    ///
    /// `fallback_definitions` are the table's character definitions, which the action's `*`
    /// uses in a forward `context` rule.
    pub fn find(
        &self,
        input: &str,
        preceding: &str,
        env: &Environment,
        at_start: bool,
        fallback_definitions: &CharacterDefinition,
    ) -> Vec<ResolvedTranslation> {
        self.regexps
            .iter()
            .filter_map(|r| {
                let found = r
                    .main
                    .find_anchored(input, env, at_start, fallback_definitions)?;
                match &r.lookbehind {
                    // the weight counts the look-behind too, as in a single forward run
                    Some(lookbehind) => lookbehind
                        .find_anchored(preceding, env, false, fallback_definitions)
                        .map(|l| {
                            let weight = found.weight() + l.weight();
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
        let re = Regexp::from_test(&tests, true, &ctx).compile();
        assert_eq!(
            re.find("abc", &env).unwrap(),
            ResolvedTranslation::new("", "", 3, stage, None).with_length(3)
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
            stage,
            &[],
            None,
        ));
        let regexp = Regexp::from_test(&tests, true, &ctx);
        let re = regexp.compile_with_payload(translation);
        assert_eq!(
            re.find("1", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None).with_length(1)
        );
        assert_eq!(
            re.find("2", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None).with_length(1)
        );
        assert_eq!(
            re.find("3", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None).with_length(1)
        );
        assert_eq!(re.find("def", &env), None);
    }

    /// `$a` is liblouis' "any character": it matches whatever is there, even with no
    /// character classes defined at all
    #[test]
    fn find_attribute_any() {
        let env = Environment::new();
        let tests = test::Parser::new("$a").tests().unwrap();
        let stage = TranslationStage::Main;
        let ctx = CharacterClasses::default();
        let re = Regexp::from_test(&tests, true, &ctx).compile();
        assert_eq!(
            re.find("x", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None).with_length(1)
        );
        assert_eq!(re.find("", &env), None);
    }

    #[test]
    fn find_attribute_uppercase() {
        let env = Environment::new();
        let tests = test::Parser::new("$U").tests().unwrap();
        let stage = TranslationStage::Main;
        let ctx = context(CharacterClass::Uppercase, &['A', 'B', 'C']);
        let translation = Translation::Unresolved(UnresolvedTranslation::new(
            &[TranslationTarget::Literal("".to_string())],
            stage,
            &[],
            None,
        ));
        let regexp = Regexp::from_test(&tests, true, &ctx);
        let re = regexp.compile_with_payload(translation);
        assert_eq!(
            re.find("A", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None).with_length(1)
        );
        assert_eq!(
            re.find("A", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None).with_length(1)
        );
        assert_eq!(
            re.find("C", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None).with_length(1)
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
            stage,
            &[],
            None,
        ));
        let regexp = Regexp::from_test(&tests, true, &ctx);
        let re = regexp.compile_with_payload(translation);
        assert_eq!(
            re.find("%", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None).with_length(1)
        );
        assert_eq!(
            re.find(".", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None).with_length(1)
        );
        assert_eq!(
            re.find("A", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None).with_length(1)
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
            stage,
            &[],
            None,
        ));
        let regexp = Regexp::from_test(&tests, true, &ctx);
        let re = regexp.compile_with_payload(translation);
        assert_eq!(
            re.find("a", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None).with_length(1)
        );
        assert_eq!(
            re.find("b", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None).with_length(1)
        );
        assert_eq!(
            re.find("c", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None).with_length(1)
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
            stage,
            &[],
            None,
        ));
        let regexp = Regexp::from_test(&tests, true, &ctx);
        let re = regexp.compile_with_payload(translation);
        assert_eq!(
            re.find("abc", &env).unwrap(),
            ResolvedTranslation::new("", "", 3, stage, None).with_length(3)
        );
        assert_eq!(
            re.find("bbb", &env).unwrap(),
            ResolvedTranslation::new("", "", 3, stage, None).with_length(3)
        );
        assert_eq!(
            re.find("ccc", &env).unwrap(),
            ResolvedTranslation::new("", "", 3, stage, None).with_length(3)
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
            stage,
            &[],
            None,
        ));
        let regexp = Regexp::from_test(&tests, true, &ctx);
        let re = regexp.compile_with_payload(translation);
        assert_eq!(
            re.find("a", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None).with_length(1)
        );
        assert_eq!(
            re.find("b", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None).with_length(1)
        );
        assert_eq!(
            re.find("c", &env).unwrap(),
            ResolvedTranslation::new("", "", 1, stage, None).with_length(1)
        );
        assert_eq!(re.find("y", &env), None);
        assert_eq!(
            re.find("bbb", &env).unwrap(),
            ResolvedTranslation::new("", "", 3, stage, None).with_length(3)
        );
        assert_eq!(
            re.find("ccc", &env).unwrap(),
            ResolvedTranslation::new("", "", 3, stage, None).with_length(3)
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
            TranslationStage::Main,
            &[],
            None,
        ));
        let regexp = Regexp::from_test(&tests, true, &ctx);
        let re = regexp.compile_with_payload(translation);
        assert_eq!(
            re.find("a1b", &env).unwrap(),
            ResolvedTranslation::new("1", "", 3, TranslationStage::Main, None).with_length(3)
        );
        assert_eq!(
            re.find("a2b", &env).unwrap(),
            ResolvedTranslation::new("2", "", 3, TranslationStage::Main, None).with_length(3)
        );
        assert_eq!(
            re.find("a3b", &env).unwrap(),
            ResolvedTranslation::new("3", "", 3, TranslationStage::Main, None).with_length(3)
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
            TranslationStage::Main,
            &[],
            None,
        ));
        let regexp = Regexp::from_test(&tests, true, &ctx);
        let re = regexp.compile_with_payload(translation);
        assert_eq!(
            re.find("a1b", &env).unwrap(),
            ResolvedTranslation::new("1", "", 3, TranslationStage::Main, None).with_length(3)
        );
        assert_eq!(
            re.find("a2b", &env).unwrap(),
            ResolvedTranslation::new("2", "", 3, TranslationStage::Main, None).with_length(3)
        );
        assert_eq!(
            re.find("a3b", &env).unwrap(),
            ResolvedTranslation::new("3", "", 3, TranslationStage::Main, None).with_length(3)
        );
        assert_eq!(
            re.find("aAb", &env).unwrap(),
            ResolvedTranslation::new("A", "", 3, TranslationStage::Main, None).with_length(3)
        );
        assert_eq!(
            re.find("aBb", &env).unwrap(),
            ResolvedTranslation::new("B", "", 3, TranslationStage::Main, None).with_length(3)
        );
        assert_eq!(
            re.find("aCb", &env).unwrap(),
            ResolvedTranslation::new("C", "", 3, TranslationStage::Main, None).with_length(3)
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
            TranslationStage::Main,
            &[],
            None,
        ));
        let regexp = Regexp::from_test(&tests, true, &ctx);
        let re = regexp.compile_with_payload(translation);
        assert_eq!(re.find("a1b", &env), None);
        assert_eq!(re.find("a22b", &env), None);
        assert_eq!(re.find("a31b", &env), None);
        assert_eq!(
            re.find("a123b", &env).unwrap(),
            ResolvedTranslation::new("123", "", 5, TranslationStage::Main, None).with_length(5)
        );
        assert_eq!(
            re.find("a222b", &env).unwrap(),
            ResolvedTranslation::new("222", "", 5, TranslationStage::Main, None).with_length(5)
        );
        assert_eq!(
            re.find("a321b", &env).unwrap(),
            ResolvedTranslation::new("321", "", 5, TranslationStage::Main, None).with_length(5)
        );
        assert_eq!(
            re.find("aABCb", &env).unwrap(),
            ResolvedTranslation::new("ABC", "", 5, TranslationStage::Main, None).with_length(5)
        );
        assert_eq!(
            re.find("aBBBb", &env).unwrap(),
            ResolvedTranslation::new("BBB", "", 5, TranslationStage::Main, None).with_length(5)
        );
        assert_eq!(
            re.find("aCBAb", &env).unwrap(),
            ResolvedTranslation::new("CBA", "", 5, TranslationStage::Main, None).with_length(5)
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
            .insert(
                &tests,
                &action,
                &origin,
                stage,
                &TableContext::default(),
                &TranslationTarget::Capture,
            )
            .unwrap();
        let translation =
            ResolvedTranslation::new("abc", "A_B_C", 3, TranslationStage::Main, origin.clone());
        let patterns = builder.build();
        assert_eq!(
            patterns.find("abc", "", &env, true, &CharacterDefinition::default()),
            [translation]
        );
        assert_eq!(
            patterns.find("def", "", &env, true, &CharacterDefinition::default()),
            []
        );
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
            .insert(
                &tests,
                &action,
                &origin,
                stage,
                &TableContext::default(),
                &TranslationTarget::Capture,
            )
            .unwrap();
        let translation =
            ResolvedTranslation::new("abc", "<abc>", 3, TranslationStage::Main, origin.clone());
        let patterns = builder.build();
        assert_eq!(
            patterns.find("abc", "", &env, true, &CharacterDefinition::default()),
            [translation]
        );
        assert!(
            patterns
                .find("def", "", &env, true, &CharacterDefinition::default())
                .is_empty()
        );
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
            .insert(
                &tests,
                &action,
                &origin,
                stage,
                &TableContext::default(),
                &TranslationTarget::Capture,
            )
            .unwrap();
        let translation =
            ResolvedTranslation::new("a", "X", 1, TranslationStage::Main, origin.clone());
        let patterns = builder.build();
        // matches when `input` truly starts at position 0 of the whole string being translated
        assert_eq!(
            patterns.find("a", "", &env, true, &CharacterDefinition::default()),
            [translation]
        );
        // does not match when we are mid-string, even though the substring looks identical
        assert!(
            patterns
                .find("a", "", &env, false, &CharacterDefinition::default())
                .is_empty()
        );
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
            .insert(
                &tests,
                &action,
                &origin,
                stage,
                &TableContext::default(),
                &TranslationTarget::Capture,
            )
            .unwrap();
        let translation =
            ResolvedTranslation::new("a", "X", 1, TranslationStage::Main, origin.clone());
        let patterns = builder.build();
        // matches when "a" is exactly the whole remaining input
        assert_eq!(
            patterns.find("a", "", &env, true, &CharacterDefinition::default()),
            [translation]
        );
        // does not match when there is trailing input after "a"
        assert!(
            patterns
                .find("ab", "", &env, true, &CharacterDefinition::default())
                .is_empty()
        );
    }

    #[test]
    fn context_at_beginning_anchor_after_other_test() {
        // liblouis/liblouis#2071: `quotation-marks.uti:26` places `` ` `` after `#1=0` rather
        // than first. The manual says it must be first, but the compiler doesn't enforce that,
        // and both `` ` `` and `#1=0` are zero-width test-phase conditions, so this must behave
        // identically to `context_at_beginning_anchor` above.
        let mut env = Environment::new();
        env.apply(&Effect::new(1, 0));
        let tests = test::Parser::new(r#"#1=0`"a""#).tests().unwrap();
        let action = action::Parser::new(r#""X""#).actions().unwrap();
        let origin = origin(r#"context #1=0`"a" "X""#);
        let stage = TranslationStage::Main;
        let mut builder = ContextPatternsBuilder::new();
        builder
            .insert(
                &tests,
                &action,
                &origin,
                stage,
                &TableContext::default(),
                &TranslationTarget::Capture,
            )
            .unwrap();
        let translation =
            ResolvedTranslation::new("a", "X", 1, TranslationStage::Main, origin.clone());
        let patterns = builder.build();
        assert_eq!(
            patterns.find("a", "", &env, true, &CharacterDefinition::default()),
            [translation]
        );
        assert!(
            patterns
                .find("a", "", &env, false, &CharacterDefinition::default())
                .is_empty()
        );
    }

    #[test]
    fn context_negated_beginning_and_end_anchor() {
        let env = Environment::new();
        let tests = test::Parser::new(r#"!`"a""#).tests().unwrap();
        let action = action::Parser::new(r#""X""#).actions().unwrap();
        let origin = origin(r#"context !`"a" "X""#);
        let stage = TranslationStage::Main;
        let mut builder = ContextPatternsBuilder::new();
        builder
            .insert(
                &tests,
                &action,
                &origin,
                stage,
                &TableContext::default(),
                &TranslationTarget::Capture,
            )
            .unwrap();
        let translation =
            ResolvedTranslation::new("a", "X", 1, TranslationStage::Main, origin.clone());
        let patterns = builder.build();
        // "not at the beginning" -- opposite of `context_at_beginning_anchor`
        assert!(
            patterns
                .find("a", "", &env, true, &CharacterDefinition::default())
                .is_empty()
        );
        assert_eq!(
            patterns.find("a", "", &env, false, &CharacterDefinition::default()),
            [translation]
        );
    }

    #[test]
    fn context_lookbehind_is_not_consumed() {
        // `_1$d[...]`: the parser makes `$d` a look-behind, checked backwards from the
        // cursor, so the rule is found at the bracket and only the bracket is consumed
        let env = Environment::new();
        let tests = test::Parser::new(r#"_1$d["b"]"#).tests().unwrap();
        let action = action::Parser::new(r#""X""#).actions().unwrap();
        let origin = origin(r#"context _1$d["b"] "X""#);
        let stage = TranslationStage::Main;
        let ctx = TableContext::new(
            CharacterClasses::new(&[(CharacterClass::Digit, &['a'])]),
            CharacterClasses::default(),
            SwapClasses::default(),
        );
        let mut builder = ContextPatternsBuilder::new();
        builder
            .insert(
                &tests,
                &action,
                &origin,
                stage,
                &ctx,
                &TranslationTarget::Capture,
            )
            .unwrap();
        let patterns = builder.build();
        // "a" is a digit here, so the rule fires on "b", the look-behind checked backwards
        // from it: `length()` is 1, just "b", while `weight()`, used only to rank competing
        // candidates, counts the digit too
        let translation = ResolvedTranslation::new("b", "X", 2, stage, origin.clone());
        assert_eq!(
            patterns.find("b", "a", &env, false, &CharacterDefinition::default()),
            [translation]
        );
        // does not fire when the preceding character isn't a digit
        assert!(
            patterns
                .find("b", "b", &env, false, &CharacterDefinition::default())
                .is_empty()
        );
        // nor at the start of the input, with nothing before it
        assert!(
            patterns
                .find("b", "", &env, true, &CharacterDefinition::default())
                .is_empty()
        );
    }

    #[test]
    fn context_lookahead_is_not_consumed() {
        // `[...]!$d_`: the parser makes `!$d` a look-ahead, checked after the bracket and
        // left for the next position to translate on its own.
        let env = Environment::new();
        let tests = test::Parser::new(r#"["b"]!$d_"#).tests().unwrap();
        let action = action::Parser::new(r#""X""#).actions().unwrap();
        let origin = origin(r#"context ["b"]!$d_ "X""#);
        let stage = TranslationStage::Main;
        let ctx = TableContext::new(
            CharacterClasses::new(&[(CharacterClass::Digit, &['1'])]),
            CharacterClasses::default(),
            SwapClasses::default(),
        );
        let mut builder = ContextPatternsBuilder::new();
        builder
            .insert(
                &tests,
                &action,
                &origin,
                stage,
                &ctx,
                &TranslationTarget::Capture,
            )
            .unwrap();
        let patterns = builder.build();
        // matches, and only consumes "b" (`length()` 1), when followed by a non-digit; `weight()`
        // is 2, reflecting the whole "b"+"z" span that was checked
        let translation = ResolvedTranslation::new("b", "X", 2, stage, origin.clone());
        assert_eq!(
            patterns.find("bz", "", &env, true, &CharacterDefinition::default()),
            [translation]
        );
        // does not fire when followed by a digit
        assert!(
            patterns
                .find("b1", "", &env, true, &CharacterDefinition::default())
                .is_empty()
        );
    }

    #[test]
    fn context_lookbehind_and_lookahead_without_brackets_or_with_star() {
        // th-g2.ctb: `"ใคร"!%ToneAndVowels_` has no brackets and `"เ"[...]"ีย"!"์"_ *...`
        // copies with `*`, so both consume the whole body, but not the look-ahead or
        // look-behind
        let env = Environment::new();
        let ctx = TableContext::new(
            CharacterClasses::new(&[(CharacterClass::Digit, &['1'])]),
            CharacterClasses::default(),
            SwapClasses::default(),
        );
        let find_after = |test: &str, action: &str, preceding: &str, input: &str| {
            let origin = origin(&format!("context {test} {action}"));
            let tests = test::Parser::new(test).tests().unwrap();
            let action = action::Parser::new(action).actions().unwrap();
            let mut builder = ContextPatternsBuilder::new();
            builder
                .insert(
                    &tests,
                    &action,
                    &origin,
                    TranslationStage::Main,
                    &ctx,
                    &TranslationTarget::Capture,
                )
                .unwrap();
            builder
                .build()
                .find(
                    input,
                    preceding,
                    &env,
                    preceding.is_empty(),
                    &CharacterDefinition::default(),
                )
                .iter()
                .map(|t| (t.output(), t.length()))
                .collect::<Vec<_>>()
        };
        let find = |test: &str, action: &str, input: &str| find_after(test, action, "", input);
        assert_eq!(find(r#""b"!$d_"#, r#""X""#, "bz"), [("X".into(), 1)]);
        assert_eq!(find(r#""b"!$d_"#, r#""X""#, "b1"), []);
        assert_eq!(
            find(r#""a"["b"]!$d_"#, r#"*"Y""#, "abz"),
            [("bY".into(), 2)]
        );
        // a look-back spanning more than the last instruction
        assert_eq!(find(r#""b"!$d"c"_2"#, r#""X""#, "bzc"), [("X".into(), 1)]);
        // one that ends inside a string splits it
        assert_eq!(find(r#""b""cd"_1"#, r#""X""#, "bcd"), [("X".into(), 2)]);
        // a look-behind with `*`, as da-dk-g26.ctb's `_$D[@6-46-6] *`, consumes only the
        // bracket
        assert_eq!(
            find_after(r#"_$d["b"]"#, r#"*"Y""#, "1", "b"),
            [("bY".into(), 1)]
        );
        assert_eq!(find_after(r#"_$d["b"]"#, r#"*"Y""#, "z", "b"), []);
        // and at the start of the input there is nothing to look behind at
        assert_eq!(find(r#"_$d["b"]"#, r#"*"Y""#, "b"), []);
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
            .insert(
                &tests,
                &action,
                &origin,
                stage,
                &TableContext::default(),
                &TranslationTarget::Capture,
            )
            .unwrap();
        let translation =
            ResolvedTranslation::new("-", "-", 1, TranslationStage::Main, origin.clone());
        let patterns = builder.build();
        assert_eq!(
            patterns.find("-", "", &env, true, &CharacterDefinition::default()),
            [translation]
        );
        assert!(
            patterns
                .find("-0", "", &env, true, &CharacterDefinition::default())
                .is_empty()
        );
        assert!(
            patterns
                .find("-", "", &env, false, &CharacterDefinition::default())
                .is_empty()
        );
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
            .insert(
                &tests,
                &action,
                &origin,
                stage,
                &ctx,
                &TranslationTarget::Capture,
            )
            .unwrap();
        let translation =
            // `*@136` copies the bracket and advances to `endMatch`, so all three cells go
            ResolvedTranslation::new("⠐", "⠐⠥", 3, TranslationStage::Post1, origin.clone())
	    .with_length(3);
        let patterns = builder.build();
        assert_eq!(
            patterns.find("⠕⠐⠽", "", &env, true, &CharacterDefinition::default()),
            [translation]
        );
        assert!(
            patterns
                .find("⠕", "", &env, true, &CharacterDefinition::default())
                .is_empty()
        );
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
            .insert(
                &tests,
                &action,
                &origin,
                stage,
                &context,
                &TranslationTarget::Capture,
            )
            .unwrap();
        let translation =
            // `"<"*">"` copies the bracket and advances to `endMatch`, eating both `$p3`s
            ResolvedTranslation::new("abc", "<abc>", 9, TranslationStage::Main, origin.clone())
                .with_length(9);
        let patterns = builder.build();
        assert_eq!(
            patterns.find("{{{abc}}}", "", &env, true, &CharacterDefinition::default()),
            [translation]
        );
        assert!(
            patterns
                .find("def", "", &env, true, &CharacterDefinition::default())
                .is_empty()
        );
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
            .insert(
                &tests,
                &action,
                &origin,
                stage,
                &context,
                &TranslationTarget::Capture,
            )
            .unwrap();
        let translation =
            ResolvedTranslation::new("abc", "<ABC>", 9, TranslationStage::Main, origin.clone())
                .with_offset(3)
                .with_length(3);
        let patterns = builder.build();
        assert_eq!(
            patterns.find("{{{abc}}}", "", &env, true, &CharacterDefinition::default()),
            [translation]
        );
        assert!(
            patterns
                .find("def", "", &env, true, &CharacterDefinition::default())
                .is_empty()
        );
    }
}
